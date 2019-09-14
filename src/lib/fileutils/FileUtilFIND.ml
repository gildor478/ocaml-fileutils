(******************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                    *)
(*                                                                            *)
(*  Copyright (C) 2003-2014, Sylvain Le Gall                                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version, with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         *)
(*  COPYING for more details.                                                 *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             *)
(******************************************************************************)

open FileUtilTypes
open FilePath
open FileUtilMisc
open FileUtilTEST
open FileUtilSTAT
open FileUtilREADLINK


let find ?(follow=Skip) ?match_compile tst fln exec user_acc =

  let user_test = compile_filter ?match_compile tst in

  let skip_action =
    match follow with
      | Skip | AskFollow _ | Follow -> ignore
      | SkipInform f -> f
  in

  let should_skip fln already_followed =
    match follow with
      | Skip | SkipInform _ -> true
      | AskFollow f ->
          if not already_followed then
            f fln
          else
            true
      | Follow ->
          if already_followed then
            raise (RecursiveLink fln)
          else
            false
  in

  let already_read = ref SetFilename.empty in

  let rec find_aux acc fln =
    let st_opt =
      try
        Some (stat fln)
      with FileDoesntExist _ ->
        None
    in
    let stL_opt =
      match st_opt with
        | Some st when st.is_link ->
            begin
              try
                Some (stat ~dereference:true fln)
              with FileDoesntExist _ ->
                None
            end
        | _ ->
            st_opt
    in
    let acc =
      if user_test ?st_opt ?stL_opt fln then
        exec acc fln
      else
        acc
    in
      match st_opt with
        | Some st ->
            if st.kind = Symlink then begin
              follow_symlink stL_opt acc fln
            end else if st.kind = Dir then begin
              enter_dir acc fln
            end else begin
              acc
            end
        | None -> acc

  and enter_dir acc drn =
    Array.fold_left
      (fun acc rfln ->
         if is_parent rfln || is_current rfln then
           acc
         else
           find_aux acc (concat drn rfln))
      acc
      (Sys.readdir drn)

  and follow_symlink stL_opt acc fln =
      match stL_opt with
        | Some stL when stL.kind = Dir ->
            let cur_link = readlink fln in
            let already_followed =
              try
                already_read := prevent_recursion !already_read cur_link;
                false
              with RecursiveLink _ ->
                true
            in
              if should_skip fln already_followed then begin
                skip_action fln;
                acc
              end else begin
                enter_dir acc fln
              end
        | _ ->
            acc
  in
    find_aux user_acc (reduce fln)
