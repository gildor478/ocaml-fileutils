
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
