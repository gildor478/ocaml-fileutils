
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
