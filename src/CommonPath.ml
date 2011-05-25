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

(** A fast operation cannot be done, will
    continue by trying more complex processing
  *)

module StringExt = FileStringExt;;

exception CannotHandleFast;;

let fast_concat _ _ =
  raise CannotHandleFast
;;

let fast_basename _ =
  raise CannotHandleFast
;;

let fast_dirname _ = 
  raise CannotHandleFast
;;

let fast_is_relative _ = 
  raise CannotHandleFast
;;

let fast_is_current _ =
  raise CannotHandleFast
;;

let fast_is_parent _ = 
  raise CannotHandleFast
;;
