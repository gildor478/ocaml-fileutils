
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
		String.concat "/" (List.map dir_writer_aux lst)
;;

let dir_reader     = CygwinPath_parser.main_filename 
	CygwinPath_lexer.token_filename
;;

let path_writer lst = String.concat ";" lst
;;

let path_reader    = CygwinPath_parser.main_path_variable 
	CygwinPath_lexer.token_path_variable
;;

