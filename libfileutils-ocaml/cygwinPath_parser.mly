%{

open SysPath_type;;

%}

%token ROOT_SEPARATOR
%token DOUBLE_DOT
%token DOT
%token <string> IDENT
%token EOF
%token SEPARATOR
%start main_filename 
%type <SysPath_type.filename_part list> main_filename
%start main_path_variable
%type <SysPath_type.filename list> main_path_variable

%%

filename_part_separator:
  SEPARATOR normal_filename_part { $2 }
| EOF                            { [] }
;

end_simple_filename_part:
  IDENT end_simple_filename_part      { add_string $1 $2 }
| DOT end_simple_filename_part        { add_string "." $2 }
| DOUBLE_DOT end_simple_filename_part { add_string ".." $2 }
| filename_part_separator             { begin_string "" $1 }
;

middle_simple_filename_part:
  IDENT end_simple_filename_part      { add_string $1 $2 }
| DOT end_simple_filename_part        { add_string "." $2 }
| DOUBLE_DOT end_simple_filename_part { add_string ".." $2 }
;

begin_simple_filename_part:
  IDENT end_simple_filename_part         { add_string $1 $2 }
| DOT middle_simple_filename_part        { add_string "." $2 }
| DOUBLE_DOT middle_simple_filename_part { add_string ".." $2 }
;

normal_filename_part:
  DOUBLE_DOT filename_part_separator { ParentDir :: $2 }
| DOT filename_part_separator        { CurrentDir :: $2 }
| filename_part_separator            { (Component "") :: $1 }
| begin_simple_filename_part         { end_string $1 }
;

main_filename:
  IDENT ROOT_SEPARATOR normal_filename_part { (Root $1) :: $3 }
| normal_filename_part                      { $1 }
;

main_path_variable:
  IDENT main_path_variable { $1 :: $2 }
| EOF                      { [] }
;
