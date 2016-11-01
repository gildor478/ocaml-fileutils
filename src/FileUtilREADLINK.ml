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
open FileUtilPWD
open FileUtilTEST


let readlink fln =
  let all_upper_dir fln =
    let rec all_upper_dir_aux lst fln =
      let dir = dirname fln in
        match lst with
        | prev_dir :: _ when prev_dir = dir -> lst
        | _ -> all_upper_dir_aux (dir :: lst) dir
    in
    all_upper_dir_aux [fln] fln
  in
  let ctst =
    let st_opt, stL_opt = None, None in
    compile_filter ?st_opt ?stL_opt Is_link
  in
  let rec readlink_aux already_read fln =
    let newly_read = prevent_recursion already_read fln in
    let dirs = all_upper_dir fln in
    try
        let src_link = List.find ctst (List.rev dirs) in
        let dst_link = Unix.readlink src_link in
        let real_link =
          if is_relative dst_link then
            reduce (concat (dirname src_link) dst_link)
          else
            reduce dst_link
        in
        readlink_aux newly_read (reparent src_link real_link fln)
      with Not_found ->
        fln
  in
  readlink_aux SetFilename.empty (make_absolute (pwd ()) fln)


