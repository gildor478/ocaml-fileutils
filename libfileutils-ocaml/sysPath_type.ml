
type filename_part =
	Root of string
	| ParentDir 
	| CurrentDir
	| Component of string

type filename = string

type extension = string

(* Utility function to parse filename *)

let begin_string str lst =
	(str,lst)
;;

let add_string str1 (str2,lst) = 
	(str1 ^ str2,lst)
;;

let end_string (str,lst) =
	(Component str) :: lst
;;

(* Definition of the caracteristic length of a path *)
let path_length = 80
;;
