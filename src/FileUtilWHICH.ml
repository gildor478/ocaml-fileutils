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
open FileUtilTEST


let which ?(path) fln =
  let real_path =
    match path with
      | None ->
          path_of_string
            (try
               Sys.getenv "PATH"
             with Not_found ->
               "")
      | Some x ->
        x
  in
  let exec_test = test (And(Is_exec, Is_file)) in
  let which_path =
    match Sys.os_type with
    | "Win32" ->
      begin
        let real_ext =
          List.map
            (fun dot_ext ->
               (* Remove leading "." if it exists *)
               if (String.length dot_ext) >= 1 && dot_ext.[0] = '.' then
                 String.sub dot_ext 1 ((String.length dot_ext) - 1)
               else
                 dot_ext)
            (* Extract possible extension from PATHEXT *)
            (path_of_string
               (try
                  Sys.getenv "PATHEXT"
                with Not_found ->
                  ""))
        in
        let to_filename dirname ext = add_extension (concat dirname fln) ext in
        let ctst dirname ext = exec_test (to_filename dirname ext) in
        List.fold_left
          (fun found dirname ->
             if found = None then begin
               try
                 let ext = List.find (ctst dirname) real_ext in
                 Some (to_filename dirname ext)
               with Not_found ->
                 None
             end else
               found)
          None
          real_path
      end
    | _ ->
      begin
        let to_filename dirname = concat dirname fln in
        try
          Some
            (to_filename
               (List.find
                  (fun dirname ->
                     exec_test (to_filename dirname)) real_path))
        with Not_found ->
          None
      end
  in
  match which_path with
  | Some fn -> fn
  | None -> raise Not_found
