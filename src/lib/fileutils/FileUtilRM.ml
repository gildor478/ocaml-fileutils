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
open FileUtilLS

exception RmError of string

type rm_error =
  [ `DirNotEmpty of filename
  | `Exc of exn
  | `NoRecurse of filename ]


let rm
      ?(error=fun str _ -> raise (RmError str))
      ?(force=Force)
      ?(recurse=false)
      fln_lst =
  let handle_error, handle_exception =
    handle_error_gen "rm" error
      (function
         | `DirNotEmpty fn ->
             Printf.sprintf "Directory %s not empty." fn
         | `NoRecurse fn ->
             Printf.sprintf
               "Cannot delete directory %s when recurse is not set."
               fn
         | #exc -> "")
  in
  let test_dir = test (And(Is_dir, Not(Is_link))) in
  let rmdir fn =
    try
      Unix.rmdir fn
    with
      | Unix.Unix_error(Unix.ENOTEMPTY, _, _) ->
          handle_error ~fatal:true (`DirNotEmpty fn)
      | e ->
          handle_exception ~fatal:true e
  in
  let rec rm_aux lst =
    List.iter
      (fun fn ->
         let exists =
           try
             let _st: Unix.LargeFile.stats = Unix.LargeFile.lstat fn in
             true
           with Unix.Unix_error(Unix.ENOENT, _, _) ->
             false
         in
         if exists && (doit force fn) then begin
           if test_dir fn then begin
             if recurse then begin
               rm_aux (ls fn);
               rmdir fn
             end else
               handle_error ~fatal:true (`NoRecurse fn)
           end else
             Unix.unlink fn
         end)
      lst
  in
  rm_aux fln_lst
