%{

open SysPath_type;;

%}

%token ROOT_SEPARATOR
%token DOUBLE_DOT
%token DOT
%token <string> IDENT
%token EOF
%start main_filename 
%type <SysPath_type.filename_part list> main_filename
%start main_path_variable
%type <string list> main_path_variable

%%

main_filename:
  IDENT ROOT_SEPARATOR normal_filename_begin


