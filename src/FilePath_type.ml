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

type current_dir_type = 
	  Short 
	| Long
;;

type filename_part =
	  Root of string
	| ParentDir 
	| CurrentDir of current_dir_type
	| Component of string
;;

type filename = string
;;

type extension = string
;;

(* Utility function to parse filename *)

let begin_string str lst =
	(str,lst)
;;

let add_string str1 (str2,lst) = 
	(str1 ^ str2,lst)
;;

let end_string (str,lst) =
	(Component str) :: lst
;;

(* Definition of the caracteristic length of a path *)
let path_length = 80
;;
