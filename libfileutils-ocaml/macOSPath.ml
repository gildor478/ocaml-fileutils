
open SysPath_type;;

let rec dir_writer lst =
	let buffer = Buffer.create path_length
	in
	let rec dir_writer_aux lst =
		match lst with
		  Root s :: tl ->
			Buffer.add_string buffer s;
			Buffer.add_char   buffer ':';
			dir_writer_aux tl
		| CurrentDir :: tl 
		| ParentDir  :: tl ->
			Buffer.add_char   buffer ':';
			dir_writer_aux tl
		| (Component "") :: tl ->
			dir_writer_aux tl
		| (Component s) :: [] ->
			Buffer.add_string buffer s;
			dir_writer_aux []
		| (Component s) :: tl ->
			Buffer.add_string buffer s;
			Buffer.add_char   buffer ':';
			dir_writer_aux tl
		| [] ->
			Buffer.contents buffer
	in
	match lst with
	  ParentDir :: _ -> 
	  	dir_writer_aux ( CurrentDir :: lst )
	| _ -> 
		dir_writer_aux lst
;;

let dir_reader      = MacOSPath_parser.main_filename 
	MacOSPath_lexer.token_filename
;;

let path_writer lst = 
	String.concat ";" lst
;;

let path_reader     = MacOSPath_parser.main_path_variable 
	MacOSPath_lexer.token_path_variable
;;
