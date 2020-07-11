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
open FileUtilUMASK
open FileUtilCHMOD

exception MkdirError of string

type mkdir_error =
  [ `DirnameAlreadyUsed of filename
  | `Exc of exn
  | `MissingComponentPath of filename
  | `MkdirChmod of filename * Unix.file_perm * string * exc ]


let mkdir
      ?(error=(fun str _ -> raise (MkdirError str)))
      ?(parent=false)
      ?mode dn =
  let handle_error, handle_exception =
    handle_error_gen "mkdir" error
      (function
         | `DirnameAlreadyUsed fn ->
             Printf.sprintf "Directory %s already exists and is a file." fn
         | `MissingComponentPath fn ->
             Printf.sprintf
               "Unable to create directory %s, an upper directory is missing."
               fn
         | `MkdirChmod (dn, mode, str, _) ->
             Printf.sprintf
               "Recursive error in 'mkdir %s' in 'chmod %04o %s': %s"
               dn mode dn str
         | #exc -> "")
  in
  let mode_apply =
    FileUtilMode.apply ~is_dir:true ~umask:(umask (`Octal (fun i -> i)))
  in
  let mode_self =
    match mode with
      | Some (`Octal m) -> m
      | Some (`Symbolic t) -> mode_apply 0o777 t
      | None -> umask_apply 0o0777
  in
  let mode_parent =
    umask
      (`Symbolic
         (fun t ->
            mode_apply 0 (t @ [`User (`Add (`List [`Write; `Exec]))])))
  in
  let rec mkdir_simple mode dn =
    if test_exists dn then begin
      if test (Not Is_dir) dn then
        handle_error ~fatal:true (`DirnameAlreadyUsed dn);
    end else begin
      if parent then begin
        mkdir_simple mode_parent (dirname dn)
      end;
      (* Make sure that the directory has not been created as a side effect
       * of creating the parent.
       *)
      if not (test_exists dn) then begin
        try
          Unix.mkdir dn mode;
          chmod
            ~error:(fun str e ->
                      handle_error ~fatal:true
                        (`MkdirChmod (dn, mode, str, e)))
            (`Octal mode) [dn]
        with Unix.Unix_error(Unix.ENOENT, _, _)
          | Unix.Unix_error(Unix.ENOTDIR, _, _) ->
              handle_error ~fatal:true (`MissingComponentPath dn)
          | e -> handle_exception ~fatal:true e
      end
    end
  in
    mkdir_simple mode_self dn (* (FilePath.reduce dn) *)
