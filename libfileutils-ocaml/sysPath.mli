
type filename  = string
type extension = string
type filename_part = SysPath_type.filename_part

(** You cannot pass a base path which is relative *)
exception SysPathBasePathRelative
(** One of the path you have passed is relative and cannot be reduce *)
exception SysPathRelativeUnreducable
(** We do not have recognized any OS, please contact upstream *)
exception SysPathUnrecognizedOS of string
(** The path use was empty *)
exception SysPathEmpty

(** All operation as defined in module Filename *)
val concat          : filename -> filename_part -> string

val is_relative     : filename -> bool
val is_implicit     : filename -> bool

(** Manipulate the extension *)

(** Remove extension *)
val chop_extension  : filename -> filename

(** Extract the extension *)
val get_extension   : filename -> extension 

(** Check the extension *)
val check_extension : filename -> extension -> bool

(** Splitting path *)

(** Extract the filename of a complete path *)
val basename        : filename -> filename_part

(** Extract the directory name of a complete path *)
val dirname         : filename -> filename

(** Move to the upper directory *)
val up_dir          : filename -> filename 

(** Remove all path component which are relative inside the path
For example : /a/../b -> /b/. Path must not be relative *)
val reduce : filename -> filename

(** Create an absolute path from a path relative to the base path *)
val make_absolute : filename -> filename -> filename

(** Create a path which is relative to the base path *)
val make_relative : filename -> filename -> filename

(** Transformation of PATH like variable *)

(** Create an environnement PATH like string from different path *)
val make_path_variable : filename list -> string

(** Return the different component of an environnement PATH like string *)
val read_path_variable : string -> filename list

(** Those function are provided for the ease of developper, however
    it is not recomended to use it : IT IS DANGEROUS. Most of path
    are context depedent and cannot be interpreted as simple 
    component without the rest of the string. Use it at your own 
    risk *)
val filename_of_filename_part : filename_part -> filename
val filename_part_of_filename : filename -> filename_part

val current_dir : filename_part
val parent_dir  : filename_part
val root        : string -> filename_part
val component   : string -> filename_part

(** Take a list of path component and return the string 
corresponding to this path *)
val implode : filename_part list -> filename

(** Take a string corresponding to a path a return the component 
of this path *)
val explode : filename -> filename_part list

(** Do the same as implode but transform each sting in the list in filename_part *)
val implode_string : string list -> filename 
