
open SysPath_type;;

let filename_of_filename_part cmp =
	match cmp with
	  Root _ -> ""
	| ParentDir -> ".."
	| CurrentDir -> "."
	| Component s -> s
;;

let dir_separator  = "/"
;;

let dir_spec   = UnixPath_parser.main_filename UnixPath_lexer.token_filename
;;

let path_separator = ":" 
;;

let path_spec      = UnixPath_parser.main_path_variable UnixPath_lexer.token_path_variable
;;

