%{

open SysPath_type;;

%}

%token SEPARATOR
%token EOF
%token DOT
%token <string> IDENT
%start main_filename
%type <SysPath_type.filename_part list> main_filename
%start main_path_variable
%type <SysPath_type.filename list> main_path_variable

%%
no_separator:
  IDENT no_separator { add_string $1 $2 }
| EOF                { begin_string "" [] }
;
end_normal_filename:
  IDENT end_normal_filename       { add_string $1 $2 }
| SEPARATOR begin_normal_filename { begin_string "" $2 }
| EOF                             { begin_string "" [] }
;
begin_normal_filename:
  IDENT end_normal_filename       { end_string(add_string $1 $2) }
| SEPARATOR begin_normal_filename { ParentDir :: $2 }
| EOF                             { [] }
;
main_filename:
  IDENT SEPARATOR begin_normal_filename { (Root $1) :: $3 }
| SEPARATOR begin_normal_filename       { CurrentDir :: $2 }
| IDENT no_separator                    { end_string(add_string $1 $2) }
;
main_path_variable:
  IDENT main_path_variable { $1 :: $2 }
| EOF                      { [] }
;
