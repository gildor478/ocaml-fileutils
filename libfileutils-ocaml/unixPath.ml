
open SysPath_type;;

let filename_of_filename_part cmp =
	match cmp with
	  Root _ -> ""
	| ParentDir -> ".."
	| CurrentDir -> "."
	| Component s -> s
;;

let implode lst =
	String.concat "/" (List.map 
		filename_of_filename_part 
		lst)
;;

let explode str =
	let lexbuf = Lexing.from_string str
	in
	UnixPath_parser.main_filename
		UnixPath_lexer.token_filename
		lexbuf 
;;

let make_path_variable lst =
	String.concat ":" lst
;;

let read_path_variable str =
	let lexbuf = Lexing.from_string str
	in
	UnixPath_parser.main_path_variable
		UnixPath_lexer.token_path_variable
		lexbuf
;;

let split_basename_extension fln_part = 
	match fln_part with
	  Component str ->
		let lexbuf = Lexing.from_string str
		in
		let (base,ext ) = UnixPath_parser.main_extension
			UnixPath_lexer.token_extension
			lexbuf
		in
		(Component base, ext)
	| ParentDir 
	| CurrentDir 
	| Root _ ->
		( fln_part, "" )
;;
