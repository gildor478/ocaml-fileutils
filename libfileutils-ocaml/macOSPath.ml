
open SysPath_type;;

(* Using this function is really not relevant, since MacPath are really
   context dependent *)
let filename_of_filename_part cmp =
	match cmp with
	  Root s -> s
	| ParentDir -> ""
	| CurrentDir -> ""
	| Component s -> s
;;

let dir_separator  = ":"
;;

let dir_spec       = MacOSPath_parser.main_filename MacOSPath_lexer.token_filename
;;

let path_separator = ";" 
;;

let path_spec      = MacOSPath_parser.main_path_variable MacOSPath_lexer.token_path_variable
;;
