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

module SetFilename = Set.Make (struct
  type t = filename
  let compare = FilePath.compare
end)


let doit force fln =
  match force with
    Force -> true
  | Ask ask -> ask fln


let prevent_recursion fln_set fln =
  (* TODO: use a set of dev/inode *)
  if SetFilename.mem fln fln_set then
    raise (RecursiveLink fln)
  else
    SetFilename.add fln fln_set


let solve_dirname dirname =
  (* We have an ambiguity concerning "" and "." *)
  if is_current dirname then
    current_dir
  else
    reduce dirname


type exc = [ `Exc of exn ]


let handle_error_gen nm error custom =
  let handle_error ~fatal e =
    let str =
      match e with
        | `Exc (Unix.Unix_error(err, nm, arg)) ->
            Printf.sprintf "%s: %s (%s, %S)" nm (Unix.error_message err) nm arg
        | `Exc exc ->
            Printf.sprintf "%s: %s" nm (Printexc.to_string exc)
        | e -> custom e
    in
      if fatal then begin
        try
          error str e;
          raise (Fatal str)
        with exc ->
          raise exc
      end else begin
        error str e
      end
  in
  let handle_exception ~fatal exc =
      handle_error ~fatal (`Exc exc)
  in
    handle_error, handle_exception
