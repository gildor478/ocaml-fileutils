%{

(* Warning : this construction destroy implicitely // construction *)

open SysPath_type;;

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

