/**************************************************************************/
/*   Ocaml-fileutils                                                      */
/*                                                                        */
/*   Copyright (C) 2003, 2004 Sylvain Le Gall <sylvain@le-gall.net>       */
/*                                                                        */
/*   This program is free software; you can redistribute it and/or        */
/*   modify it under the terms of the GNU Library General Public          */
/*   License as published by the Free Software Foundation; either         */
/*   version 2 of the License, or any later version ; with the OCaml      */
/*   static compilation exception.                                        */
/*                                                                        */
/*   This program is distributed in the hope that it will be useful,      */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of       */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 */
/*   See the LICENCE file for more details.                               */
/*                                                                        */
/*   You should have received a copy of the GNU General Public License    */
/*   along with this program; if not, write to the Free Software          */
/*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA             */
/*   02111-1307  USA                                                      */
/*                                                                        */
/*   Contact: sylvain@le-gall.net                                         */
/*                                                                        */
/**************************************************************************/

%{

(* Warning : this construction destroy implicitely // construction *)

open FilePath_type;;

%}

%token SLASH
%token DOUBLE_DOT
%token DOT
%token <string> IDENT
%token EOF
%start main_filename
%type <FilePath_type.filename_part list> main_filename
%start main_path_variable
%type <FilePath_type.filename list> main_path_variable

%%
filename_part_separator:
  SLASH normal_filename_part { $2 }
| EOF                        { [] }
;

end_simple_filename_part:
  IDENT end_simple_filename_part       { add_string $1 $2 }
| DOT end_simple_filename_part         { add_string "." $2 }
| DOUBLE_DOT end_simple_filename_part  { add_string ".." $2 }
| filename_part_separator              { begin_string "" $1 }
;

middle_simple_filename_part:
  IDENT end_simple_filename_part       { add_string $1 $2 }
| DOT end_simple_filename_part         { add_string "." $2 }
| DOUBLE_DOT end_simple_filename_part  { add_string ".." $2 }
;

begin_simple_filename_part:
  IDENT end_simple_filename_part         { add_string $1 $2}
| DOT middle_simple_filename_part        { add_string "." $2}
| DOUBLE_DOT middle_simple_filename_part { add_string ".." $2}
;

normal_filename_part:
  DOUBLE_DOT filename_part_separator { ParentDir :: $2 }
| DOT filename_part_separator        { CurrentDir :: $2 }
| filename_part_separator            { (Component "") :: $1 }
| begin_simple_filename_part         { end_string $1 }
;

no_slash_begin_filename_part:
  DOUBLE_DOT filename_part_separator { ParentDir :: $2 }
| DOT filename_part_separator        { CurrentDir :: $2 }
| begin_simple_filename_part         { end_string $1 }
;

main_filename:
  SLASH normal_filename_part   { (Root "") :: $2 }
| no_slash_begin_filename_part { $1 }
;

main_path_variable:
  IDENT main_path_variable { $1 :: $2 }
| EOF                      { [] }
;

