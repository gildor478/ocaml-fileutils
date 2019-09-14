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

open FileUtilMisc

exception UmaskError of string

type umask_error = [ `Exc of exn | `NoStickyBit of int ]


let umask
      ?(error=(fun str _ -> raise (UmaskError str)))
      ?mode out =
  let handle_error, handle_exception =
    handle_error_gen "umask" error
      (function
       | `NoStickyBit i ->
           Printf.sprintf "Cannot set sticky bit in umask 0o%04o" i
       | #exc -> "")
  in
  let complement i = 0o0777 land (lnot i) in
  let try_umask i =
    if Sys.os_type = "Win32" then 0 else
    try
      Unix.umask i
    with e ->
      handle_exception ~fatal:true e;
      raise e
  in
  let get () =
    let cmask = try_umask 0o777 in
    let _mask: int = try_umask cmask in
    cmask
  in
  let set i =
    let eff_i = i land 0o777 in
    let _i: int =
      if i <> eff_i then
        handle_error ~fatal:true (`NoStickyBit i);
      try_umask eff_i
    in
    eff_i
  in
  let v =
    match mode with
    | Some (`Symbolic s) ->
        let v = get () in
          set
            (complement
               (FileUtilMode.apply ~is_dir:false ~umask:0 (complement v) s))
    | Some (`Octal i) -> set i
    | None -> get ()
  in
  match out with
    | `Symbolic f -> f (FileUtilMode.of_int (0o0777 land (lnot v)))
    | `Octal f -> f v


let umask_apply m = m land (lnot (umask (`Octal (fun i -> i))))
