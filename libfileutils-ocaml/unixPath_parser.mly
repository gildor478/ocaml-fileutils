%{

(* Warning : this construction destroy implicitely // construction *)

open SysPath_type;;

let begin_string str lst =
	(str,lst)
;;

let add_string str1 (str2,lst) = 
	(str1 ^ str2,lst)
;;

let end_string (str,lst) =
	(Component str) :: lst
;;

%}

%token SLASH
%token DOUBLE_DOT
%token DOT
%token <string> IDENT
%token EOF
%start main_filename
%type <SysPath_type.filename_part list> main_filename
%start main_path_variable
%type <string list> main_path_variable
%start main_extension
%type <string * string> main_extension

%%
filename_part_separator:
  SLASH normal_filename_part { $2 }
| SLASH EOF                  { [Component ""] }
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
| begin_simple_filename_part         { end_string $1 }
;

main_filename:
  SLASH normal_filename_part { (Root "") :: $2 }
| normal_filename_part       { $1 }
;

main_path_variable:
  IDENT main_path_variable { $1 :: $2 }
| EOF                      { [] }
;

main_extension:
  IDENT DOT IDENT EOF  { ($1,$3) }
| IDENT DOT EOF        { ($1,"") }
| IDENT main_extension { let (m,ext) = $2 in ( $1^m,ext) }
| DOT main_extension   { let (m,ext) = $2 in ("."^m,ext) }
;
