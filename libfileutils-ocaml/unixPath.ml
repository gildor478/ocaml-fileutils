
open SysPath_type;;

let filename_of_filename_part cmp =
	match cmp with
	  Root _ -> ""
	| ParentDir -> ".."
	| CurrentDir -> "."
	| Component s -> s
;;

let dir_writer lst = 
	let dir_sep = "/"
	in
	let rec dir_writer_aux cmp =
		match cmp with
		  Root _ -> ""
		| ParentDir -> ".."
		| CurrentDir -> "."
		| Component s -> s
	in
	String.concat dir_sep ( List.map dir_writer_aux lst )
;;

let dir_reader  = UnixPath_parser.main_filename 
	UnixPath_lexer.token_filename
;;

let path_writer lst = 
	String.concat ":" lst
;;

let path_reader     = UnixPath_parser.main_path_variable 
	UnixPath_lexer.token_path_variable
;;

