(** You cannot pass a base path which is relative *)
exception Base_path_relative
(** One of the path you have passed is relative and cannot be reduce *)
exception Path_relative_unreducable

(** All operation as defined in module Filename *)
val current_dir_name : string
val parent_dir_name : string
val concat : string -> string -> string
val is_relative : string -> bool
val is_implicit : string -> bool
val check_suffix : string -> string -> bool
val chop_suffix : string -> string -> string
val chop_extension : string -> string
val basename : string -> string
val dirname : string -> string
val temp_file : string -> string -> string
val open_temp_file : ?mode:Pervasives.open_flag list -> 
	string -> string -> string * Pervasives.out_channel
val quote : string -> string


(** Take a list of path component and return the string 
corresponding to this path *)
val implode : string list -> string

(** Take a string corresponding to a path a return the component 
of this path *)
val explode : string -> string list

(** Remove all path component which are relative inside the path
For example : /a/../b -> /b/. Path must not be relative *)
val reduce : string -> string

(** Create an absolute path from a path relative to the base path *)
val make_absolute : string -> string -> string

(** Create a path which is relative to the base path *)
val make_relative : string -> string -> string

(** Create a directory listing of the dir *)
val list_dir : string -> string list

(** Pattern you can use to filter you directory listing *)
type test_file =
    Is_file
  | Is_dir
  | Is_link
  | And of test_file * test_file
  | Or of test_file * test_file
  | Not of test_file

(** Apply a filtering pattern to a directory listing *)
val filter_dir : test_file -> string list -> string list

(** Test the existence of the file... *)
val test : test_file -> string -> bool

(** Create an environnement PATH like string from different path *)
val make_path : string list -> string

(** Return the different component of an environnement PATH like string *)
val explode_path : string -> string list
