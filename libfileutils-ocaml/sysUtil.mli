(** The base type of the module *)

type filename = string


(** For certain command, you should need to ask the user wether
    or not he does want to do some action. Provide the function 
    to Ask or Force the action *)
type interactive =
	(** Do it anyway *)
	  Force
	(** Promp the user *)
	| Ask of (filename -> bool)

(** Pattern you can use to test file *)
type test_file =
	(** FILE exists and is block special *)
	Is_dev_block
	(** FILE exists and is character special *)
	| Is_dev_char
	(** FILE exists and is a directory *)
	| Is_dir
	(** FILE exists *)
	| Exists
	(** FILE exists and is a regular file *)
	| Is_file
	(** FILE exists and is set-group-ID *)
	| Is_set_group_ID
	(** FILE exists and has its sticky bit set *)
	| Has_sticky_bit
	(** FILE exists and is a symbolic link *)
	| Is_link
	(** FILE exists and is a named pipe *)
	| Is_pipe
	(** FILE exists and is readable *)
	| Is_readable
	(** FILE exists and is writeable *)
	| Is_writeable
	(** FILE exists and has a size greater than zero *)
	| Size_not_null
	(** FILE exists and is a socket *)
	| Is_socket
	(** FILE exists and its set-user-ID bit is set *)
	| Has_set_user_ID
	(** FILE exists and is executable *)
	| Is_exec
	(** FILE exists and is owned by the effective user ID *)
	| Is_owned_by_user_ID
	(** FILE exists and is owned by the effective group ID *)
	| Is_owned_by_group_ID
	(** FILE1 is newer (modification date) than FILE2 *)
	| Is_newer_than of filename * filename
	(** FILE1 is older than FILE2 *)
	| Is_older_than of filename * filename
	(** FILE1 and FILE2 have the same device and inode numbers *)
	| Has_same_device_and_inode of filename * filename
	(** Result of TEST1 and TEST2 *)
	| And of test_file * test_file
	(** Result of TEST1 or TEST2 *)
	| Or of test_file * test_file
	(** Result of not TEST *)
	| Not of test_file
	(** Match Str regex *)
	| Match of string
	(** Always true *)
	| True
	(** Always false *)
	| False

val list_dir : filename -> filename list

(** Apply a filtering pattern to a filename *)
val filter : test_file -> filename list -> filename list

(** Test the existence of the file... *)
val test : test_file -> filename -> bool

(** Try to find the executable in the PATH. Use environement variable
PATH if none is provided *)
val which : ?path:filename list -> filename -> filename

exception MkdirMissingComponentPath;;
exception MkdirDirnameAlreadyUsed;;
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
exception RmDirNotEmpty;;
val rm : ?force:interactive -> ?recurse:bool -> filename -> unit

(** Copy the hierarchy of files/directory to another destination *)
exception CpCannotCopy;;
val cp : ?force:interactive -> ?recurse:bool -> filename -> filename -> unit

(** Move files/directory to another destination *)
val mv : ?force:interactive -> filename -> filename -> unit

