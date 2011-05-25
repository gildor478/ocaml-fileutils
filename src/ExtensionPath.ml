(********************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                      *)
(*                                                                              *)
(*  Copyright (C) 2003-2011, Sylvain Le Gall                                    *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Manipulate path extension 
  *)

open FilePath_type;;

let get fn =
  let start_pos =
    (String.rindex fn '.') + 1
  in
  let fn_len =
    String.length fn
  in
    if start_pos = fn_len then
      ""
    else
      String.sub fn start_pos (fn_len - start_pos)
;;

let check fn ext =
  try
    (get fn) = ext
  with Not_found ->
    false
;;

let chop fn =
  try
    let end_pos = 
      String.rindex fn '.'
    in
      if end_pos = 0 then
        ""
      else
        String.sub fn 0 end_pos
  with Not_found ->
    fn
;;

let add fn ext =
  fn ^ "." ^ ext
;;

let replace fn ext =
  add (chop fn) ext
;;

