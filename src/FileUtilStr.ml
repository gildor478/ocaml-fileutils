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

(** FileUtil with Str regexp match test
    @author Sylvain Le Gall
  *)

(** Compile [FileUtil.Match] expression using [Str.regexp]
  *)
let match_compile str =
  let regex = 
    Str.regexp str
  in
    fun fn -> Str.string_match regex fn 0
;;

(** See {!FileUtil.test}
  *)
let test =
  FileUtil.test ~match_compile:match_compile
;;

(** See {!FileUtil.find}
  *)
let find =
  FileUtil.find ~match_compile:match_compile
;;
