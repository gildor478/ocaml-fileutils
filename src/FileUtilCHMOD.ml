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
open FileUtilMisc
open FileUtilPermission
open FileUtilSTAT
open FileUtilLS
open FileUtilUMASK

exception ChmodError of string

type chmod_error = [`Exc of exn]


let chmod
      ?(error=fun str _ -> raise (ChmodError str))
      ?(recurse=false)
      mode lst =
  let _, handle_exception =
    handle_error_gen "chmod" error (function #exc -> "")
  in
  let rec chmod_one fn =
    let st = stat fn in
      if st.kind = Dir && recurse then begin
        List.iter chmod_one (ls fn)
      end;
      if not st.is_link then begin
        let int_perm =
          match mode with
            | `Octal i -> i
            | `Symbolic t ->
                FileUtilMode.apply
                  ~is_dir:(st.kind = Dir)
                  ~umask:(umask (`Octal (fun i -> i)))
                  (int_of_permission st.permission) t
        in
          if int_perm <> int_of_permission st.permission then
            try
              Unix.chmod fn int_perm
            with e ->
              handle_exception ~fatal:true e
      end
  in
    List.iter chmod_one lst
