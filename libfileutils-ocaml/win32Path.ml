(**************************************************************************)
(*   Ocaml-fileutils                                                      *)
(*                                                                        *)
(*   Copyright (C) 2003, 2004 Sylvain Le Gall <sylvain@le-gall.net>       *)
(*                                                                        *)
(*   This program is free software; you can redistribute it and/or        *)
(*   modify it under the terms of the GNU Library General Public          *)
(*   License as published by the Free Software Foundation; either         *)
(*   version 2 of the License, or any later version ; with the OCaml      *)
(*   static compilation exception.                                        *)
(*                                                                        *)
(*   This program is distributed in the hope that it will be useful,      *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 *)
(*   See the LICENCE file for more details.                               *)
(*                                                                        *)
(*   You should have received a copy of the GNU General Public License    *)
(*   along with this program; if not, write to the Free Software          *)
(*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA             *)
(*   02111-1307  USA                                                      *)
(*                                                                        *)
(*   Contact: sylvain@le-gall.net                                         *)
(*                                                                        *)
(**************************************************************************)

open FilePath_type;;

let rec dir_writer lst = 
	match lst with 
	  Root s :: tl ->
	  	(s^":/")^(dir_writer tl)
	| [ CurrentDir Short ] ->
		""
	| lst ->
		let dir_writer_aux cmp =
			match cmp with
			  (* We should raise an exception here *)
			  Root s -> s
			| ParentDir -> ".."
			| CurrentDir _ -> "."
			| Component s -> s
		in
		String.concat "\\" (List.map dir_writer_aux lst)
;;

let dir_reader  = Win32Path_parser.main_filename 
	Win32Path_lexer.token_filename
;;

let path_writer lst = String.concat ";" lst
;;

let path_reader     = Win32Path_parser.main_path_variable 
	Win32Path_lexer.token_path_variable
;;

