(** Module to manipulate real file *)

(** {1 Types and exception }*)

exception MkdirMissingComponentPath
exception MkdirDirnameAlreadyUsed
exception CpCannotCopyDirToDir
exception CpCannotCopyDirToFile
exception CpCannotCopy
exception CpNoSourceFile
exception RmDirNotEmpty
exception MvNoSourceFile

(** The base type of the module *)
type filename = SysPath.DefaultPath.filename

(** {1 Testing file} *)

(** For certain command, you should need to ask the user wether
    or not he does want to do some action. Provide the function 
    to Ask or Force the action *)
type interactive =
	  Force
	(** Do it anyway *)
	| Ask of (filename -> bool)
	(** Promp the user *)

(** Pattern you can use to test file *)
type test_file =
	Is_dev_block
	(** FILE exists and is block special *)
	| Is_dev_char
	(** FILE exists and is character special *)
	| Is_dir
	(** FILE exists and is a directory *)
	| Exists
	(** FILE exists *)
	| Is_file
	(** FILE exists and is a regular file *)
	| Is_set_group_ID
	(** FILE exists and is set-group-ID *)
	| Has_sticky_bit
	(** FILE exists and has its sticky bit set *)
	| Is_link
	(** FILE exists and is a symbolic link *)
	| Is_pipe
	(** FILE exists and is a named pipe *)
	| Is_readable
	(** FILE exists and is readable *)
	| Is_writeable
	(** FILE exists and is writeable *)
	| Size_not_null
	(** FILE exists and has a size greater than zero *)
	| Is_socket
	(** FILE exists and is a socket *)
	| Has_set_user_ID
	(** FILE exists and its set-user-ID bit is set *)
	| Is_exec
	(** FILE exists and is executable *)
	| Is_owned_by_user_ID
	(** FILE exists and is owned by the effective user ID *)
	| Is_owned_by_group_ID
	(** FILE exists and is owned by the effective group ID *)
	| Is_newer_than of filename * filename
	(** FILE1 is newer (modification date) than FILE2 *)
	| Is_older_than of filename * filename
	(** FILE1 is older than FILE2 *)
	| Has_same_device_and_inode of filename * filename
	(** FILE1 and FILE2 have the same device and inode numbers *)
	| And of test_file * test_file
	(** Result of TEST1 and TEST2 *)
	| Or of test_file * test_file
	(** Result of TEST1 or TEST2 *)
	| Not of test_file
	(** Result of not TEST *)
	| Match of string
	(** Match Str regex *)
	| True
	(** Always true *)
	| False
	(** Always false *)
	| Has_extension of string
	(** Check extension *)
	| Is_parent_dir 
	(** Is it the parent dir *)
	| Is_current_dir
	(** Is it the current dir *)

(** {1 Common operation on file }*)

(** List the content of a directory *)
val list_dir : filename -> filename list

(** Apply a filtering pattern to a filename *)
val filter : test_file -> filename list -> filename list

(** Test the existence of the file... *)
val test : test_file -> filename -> bool

(** Try to find the executable in the PATH. Use environement variable
PATH if none is provided *)
val which : ?path:filename list -> filename -> filename

(** Create the directory which name is provided. Turn parent to true
if you also want to create every topdir of the path. Use mode to 
provide some specific right ( default 755 ). *)
val mkdir : ?parent:bool -> ?mode:int -> filename -> unit

(** Modify the time stamp of the given filename. Turn create to false
if you don't want to create the file *)
val touch : ?create:bool -> filename -> unit

(** Descend the directory tree starting from the given filename and using
the test provided to find what is looking for. You cannot match current_dir
and parent_dir.*)
val find : test_file -> filename -> filename list

(** Remove the filename provided. Turn recurse to true in order to 
completely delete a directory *)
val rm : ?force:interactive -> ?recurse:bool -> filename -> unit

(** Copy the hierarchy of files/directory to another destination *)
val cp : ?force:interactive -> ?recurse:bool -> filename -> filename -> unit

(** Move files/directory to another destination *)
val mv : ?force:interactive -> filename -> filename -> unit
