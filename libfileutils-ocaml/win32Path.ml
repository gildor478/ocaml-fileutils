
open SysPath_type;;

let rec dir_writer lst = 
	match lst with 
	  Root s :: tl ->
	  	(s^":/")^(dir_writer tl)
	| lst ->
		let dir_writer_aux cmp =
			match cmp with
			  (* We should raise an exception here *)
			  Root s -> s
			| ParentDir -> ".."
			| CurrentDir -> "."
			| Component s -> s
		in
		String.concat "\\" (List.map dir_writer_aux lst)
;;

let dir_reader  = Win32Path_parser.main_filename 
	Win32Path_lexer.token_filename
;;

let path_writer lst = String.concat ":" lst
;;

let path_reader     = Win32Path_parser.main_path_variable 
	Win32Path_lexer.token_path_variable
;;

