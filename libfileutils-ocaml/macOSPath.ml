
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

let implode lst =
	String.concat ":" (List.map 
		filename_of_filename_part
		lst )
;;

let explode str =
	let lexbuf = Lexing.from_string str
	in
	MacOSPath_parser.main_filename
		MacOSPath_lexer.token_filename
		lexbuf
;;

let make_path_variable lst =
	String.concat ";" lst
;;

let read_path_variable str =
	let lexbuf = Lexing.from_string str
	in
	MacOSPath_parser.main_path_variable
		MacOSPath_lexer.token_path_variable
		lexbuf
;;

