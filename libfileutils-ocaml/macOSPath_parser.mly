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
%type <string list> main_path_variable
%start main_extension
%type <string * string> main_extension

%%
no_separator:
  IDENT no_separator { add_string $1 $2 }
| EOF                { begin_string "" [] }
;
end_normal_filename:
  IDENT end_normal_filename       { add_string $1 $2 }
| SEPARATOR begin_normal_filename { begin_string "" $1 }
;
begin_normal_filename:
  IDENT end_normal_filename       { add_string $1 $2 }
| SEPARATOR begin_normal_filename { ParentDir :: $1 }
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
main_extension:
  IDENT DOT IDENT EOF   { ($1,$3) }
| IDENT DOT EOF         { ($1,"") }
| IDENT main_extension  { let (m,ext) = $2 in ( $1^m,ext) }
| DOT main_extension    { let (m,ext) = $2 in ("."^m,ext) }
;
