%{

open SysPath_type;;

%}

%token EOF
%token DOT
%token <string> IDENT
%start main_extension
%type <string * string> main_extension

%%
main_extension:
  IDENT DOT IDENT EOF   { ($1,$3) }
| IDENT DOT EOF         { ($1,"") }
| IDENT main_extension  { let (m,ext) = $2 in ( $1^m,ext) }
| DOT main_extension    { let (m,ext) = $2 in ("."^m,ext) }
;
