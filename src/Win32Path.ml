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

open FilePath_type

include CommonPath


let rec dir_writer lst =
  match lst with
    Root s :: tl -> (s^":\\")^(dir_writer tl)
  | [ CurrentDir Short ] -> ""
  | lst ->
    let dir_writer_aux cmp =
      match cmp with
      (* We should raise an exception here *)
        Root s -> s
      | ParentDir -> ".."
      | CurrentDir _ -> "."
      | Component s -> s
    in
    String.concat "\\" (List.map dir_writer_aux lst)


let dir_reader str =
  let fn_part_of_string =
    function
    | ".." -> ParentDir
    | "."  -> CurrentDir Long
    | str  -> Component str
  in
  let fn_part_split str =
    let lst =
      List.flatten
        (List.map
           (StringExt.split ~map:fn_part_of_string '\\')
           (StringExt.split ~map:(fun s -> s) '/' str))
    in
    match lst with
    (* TODO: we don't make the difference between c:a and c:\a *)
    | Component "" :: tl -> tl
    | lst -> lst
  in
  try
    let drive_letter, str = StringExt.break_at_first ':' str in
    Root drive_letter :: (fn_part_split str)
  with Not_found ->
    fn_part_split str

let fast_is_current fn =
  if String.length fn = 0 || fn = "." then
    true
  else if fn.[0] <> '.' then
    false
  else
    raise CannotHandleFast

let fast_is_parent fn =
  if fn = ".." then
    true
  else if String.length fn < 2 || fn.[0] <> '.' || fn.[1] <> '.' then
    false
  else
    raise CannotHandleFast

let path_writer lst = String.concat ";" lst


let path_reader str = StringExt.split ~map:(fun s -> s) ';' str
