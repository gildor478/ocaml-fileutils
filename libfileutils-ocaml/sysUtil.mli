

(** Pattern you can use to test file *)
type test_file =
    Is_dev_block
  | Is_dev_char
  | Is_dir
  | Exists
  | Is_file
  | Is_set_group_ID
  | Has_sticky_bit
  | Is_link
  | Is_pipe
  | Is_readable
  | Is_writeable
  | Size_not_null
  | Is_socket
  | Has_set_user_ID
  | Is_exec
  | Is_owned_by_user_ID
  | Is_owned_by_group_ID
  | Is_newer_than of string * string
  | Is_older_than of string * string
  | Has_same_device_and_inode of string * string
  | And of test_file * test_file
  | Or of test_file * test_file
  | Not of test_file
  | Match of string
  | True
  | False

val list_dir : string -> string list

(** Apply a filtering pattern to a filename *)
val filter : test_file -> string list -> string list

(** Test the existence of the file... *)
val test : test_file -> string -> bool

(** Try to find the executable in the PATH. Use environement variable
PATH if none is provided *)
val which : ?path:string list -> string -> string

exception MkdirMissingComponentPath;;
exception MkdirDirnameAlreadyUsed;;
(** Create the directory which name is provided. Turn parent to true
if you also want to create every topdir of the path. Use mode to 
provide some specific right ( default 755 ). *)
val mkdir : ?parent:bool -> ?mode:int -> string -> unit

(** Modify the time stamp of the given filename. Turn create to false
if you don't want to create the file *)
val touch : ?create:bool -> string -> unit

(** Descend the directory tree starting from the given filename and using
the test provided to find what is looking for. You cannot match current_dir
and parent_dir.*)
val find : test_file -> string -> string list

(** Remove the filename provided. Turn recurse to true in order to 
completely delete a directory *)
exception RmDirNotEmpty;;
val rm : ?recurse:bool -> string -> unit
