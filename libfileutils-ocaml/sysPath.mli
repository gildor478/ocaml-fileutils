(** Module to manipulate abstract filename ( doesn't need to be real path ).*)

(** {1 Types and exceptions } *)

(** You cannot pass a base filename which is relative *)
exception SysPathBaseFilenameRelative

(** One of the filename passed is relative and cannot be reduce *)
exception SysPathRelativeUnreducable

(** We do not have recognized any OS, please contact upstream *)
exception SysPathUnrecognizedOS of string

(** An expansion of filename_part_of_filename generate more than one component *)
exception SysPathFilenameMultiple

(** The filename use was empty *)
exception SysPathEmpty

(** The last component of the filename does not support extension ( Root, ParentDir... ) *)
exception SysPathNoExtension

(** The filename used is invalid *)
exception SysPathInvalidFilename

(** Abstract filename type *)
type filename = SysPath_type.filename

and filename_part = SysPath_type.filename_part

and extension = SysPath_type.extension

(** {1 Filename specification and utilities } *)

(** Signature needed to create a new        
    filesystem... See the source code for   
    more information                        *)
module type OS_SPECIFICATION =
  sig
    val dir_writer : filename_part list -> string
    val dir_reader : Lexing.lexbuf -> filename_part list
    val path_writer : filename list -> string
    val path_reader : Lexing.lexbuf -> filename list
  end
  
(** Generic operation on filename *)
module type PATH_SPECIFICATION =
  sig


(** {2 Manipulating/Splitting path } *) 


(** Extract only the file name of a complete filename *)
    val basename : filename -> filename
    
(** Extract the directory name of a complete filename *)
    val dirname : filename -> filename

(** Move to the upper directory *)
    val up_dir : filename -> filename

(** Append a filename to the filename *)
    val concat : filename -> filename -> filename

(** Make a filename from the different filename given *)
    val make_filename : filename list -> filename

(** Return the shortest filename which is equal to the filename given *)
    val reduce : filename -> filename

(** Create an absolute filename from a filename relative to an absolute base filename *)
    val make_absolute : filename -> filename -> filename

(** Create a filename which is relative to the base filename *)
    val make_relative : filename -> filename -> filename

(** Reparent a filename : reparent fln_src fln_dst fln, return the same filename as fln 
    but the root is no more fln_src but fln_dst *)
    val reparent : filename -> filename -> filename -> filename

(** Identity : for testing the stability of implode/explode *)
    val identity : filename -> filename

(** Test if the filename is a valid one *)
    val is_valid : filename -> bool

(** Check if the filename is relative to a dir or not ( ie beginning with Component, ParentDir, CurrentDir ) *)
    val is_relative : filename -> bool

(** Check if the filename is an absolute one or not ( ie beginning by a Root reference )*)
    val is_implicit : filename -> bool


(** {2 Manipulate the extension }*)


(** Remove extension *)
    val chop_extension : filename -> filename

(** Extract the extension *)
    val get_extension : filename -> extension

(** Check the extension *)
    val check_extension : filename -> extension -> bool

(** Add an extension *)
    val add_extension : filename -> extension -> filename


(** {2 Transformation of PATH like variable }*)


(** Create an environnement PATH like string from different filename *)
    val make_path_variable : filename list -> string

(** Return the different filename of an environnement PATH like string *)
    val read_path_variable : string -> filename list


(** {2 Dangerous functions }*)


(** Those function are provided for the ease of developper, however
    it is not recommended to use it : {e IT IS DANGEROUS}. Most of filename
    are context depedent and cannot be interpreted as simple 
    component without the rest of the filename. Use it at your own 
    risk *)

    val current_dir : filename
    val parent_dir : filename
    val root : string -> filename
    val component : string -> filename

(** Take a list of filename component and return the string 
corresponding to this filename *)
    val implode : filename_part list -> filename

(** Take a string corresponding to a filename a return the component 
of this filename *)
    val explode : filename -> filename_part list
  end

(** Meta structure to generate a module for 
    a new filesystem specification. See the 
    source code to know how to do it        *)
module type META_PATH_SPECIFICATION =
  functor (OsOperation : OS_SPECIFICATION) -> PATH_SPECIFICATION
  
module GenericPath : META_PATH_SPECIFICATION

(** {1 Instanciation of specific OS filename }*)

(** Some instanciation of the above structure.   
    You can use it to manipulate filename        
    However, this function is mainly for testing 
    purpose. Better use the current binding of   
    this function for your OS                    *)
  
module UnixPath : PATH_SPECIFICATION
module MacOSPath : PATH_SPECIFICATION
module Win32Path : PATH_SPECIFICATION
module CygwinPath : PATH_SPECIFICATION

(** {1 Operation for the current OS }*)

(** All those function are binding to your current     
    running OS. See module signature of {!SysPath.PATH_SPECIFICATION}
    to know how they work                              *)

val basename : filename -> filename
val dirname : filename -> filename
val up_dir : filename -> filename
val concat : filename -> filename -> filename
val reduce : filename -> filename
val make_absolute : filename -> filename -> filename
val make_relative : filename -> filename -> filename
val reparent : filename -> filename -> filename -> filename
val identity : filename -> filename
val is_valid : filename -> bool
val is_relative : filename -> bool
val is_implicit : filename -> bool
val chop_extension : filename -> filename
val get_extension : filename -> extension
val check_extension : filename -> extension -> bool
val add_extension : filename -> extension -> filename
val make_path_variable : filename list -> string
val read_path_variable : string -> filename list
val current_dir : filename
val parent_dir : filename
val root : string -> filename
val component : string -> filename
val implode : filename_part list -> filename
val explode : filename -> filename_part list
val make_filename : filename list -> filename
