
type filename  = string
type extension = string
type filename_part = SysPath_type.filename_part
(*	  SysPath_type.CurrentDir
	| SysPath_type.ParentDir 
	| SysPath_type.Root of string
	| SysPath_type.Component of string
*)
(** You cannot pass a base path which is relative *)
exception SysPathBasePathRelative
(** One of the path you have passed is relative and cannot be reduce *)
exception SysPathRelativeUnreducable
(** We do not have recognized any OS, please contact upstream *)
exception SysPathUnrecognizedOS of string

(** All operation as defined in module Filename *)
val concat          : filename -> filename_part -> string

val is_relative     : filename -> bool
val is_implicit     : filename -> bool

val chop_extension  : filename -> filename
val check_extension : filename -> extension -> bool

val basename        : filename -> filename_part
val dirname         : filename -> filename
val quote           : filename -> string

val filename_of_filename_part : filename_part -> filename
val filename_part_of_filename : filename -> filename_part

val current_dir : filename_part
val parent_dir  : filename_part

(** Take a list of path component and return the string 
corresponding to this path *)
val implode : filename_part list -> filename

(** Take a string corresponding to a path a return the component 
of this path *)
val explode : filename -> filename_part list

(** Remove all path component which are relative inside the path
For example : /a/../b -> /b/. Path must not be relative *)
val reduce : filename -> filename

(** Create an absolute path from a path relative to the base path *)
val make_absolute : filename -> filename -> filename

(** Create a path which is relative to the base path *)
val make_relative : filename -> filename -> filename

(** Create an environnement PATH like string from different path *)
val make_path_variable : filename list -> string

(** Return the different component of an environnement PATH like string *)
val read_path_variable : string -> filename list

(** Do the same as implode but transform each sting in the list in filename_part *)
val implode_string : string list -> filename 
