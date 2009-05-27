(********************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                      *)
(*                                                                              *)
(*  Copyright (C) 2003-2009, Sylvain Le Gall                                    *)
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

open FilePath_type;;

include CommonPath;;

let rec dir_writer lst = 
	match lst with 
	  Root s :: tl ->
	  	"/"^(dir_writer tl)
	| [ CurrentDir Short ] ->
		""
	| lst ->
		let rec dir_writer_aux cmp =
			match cmp with
			  Root _ -> ""
			| ParentDir -> ".."
			| CurrentDir _ -> "."
			| Component s -> s
		in
		String.concat "/" ( List.map dir_writer_aux lst )
;;

let dir_reader str = 
  UnixPath_parser.main_filename 
    UnixPath_lexer.token_filename
    (Lexing.from_string str)
;;

let path_writer lst = 
	String.concat ":" lst
;;

let path_reader     = UnixPath_parser.main_path_variable 
	UnixPath_lexer.token_path_variable
;;

