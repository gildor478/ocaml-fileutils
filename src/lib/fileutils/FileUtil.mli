(******************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                    *)
(*                                                                            *)
(*  Copyright (C) 2003-2014, Sylvain Le Gall                                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version, with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         *)
(*  COPYING for more details.                                                 *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             *)
(******************************************************************************)

(** POSIX utilities for files and directories.

    A module to provide the core POSIX utilities to manipulate files and
    directories. All functions try to mimic common POSIX utilities but are
    written in pure OCaml.

    @author Sylvain Le Gall
  *)

open FilePath


(*********************************************************************)
(**

  {2 Types and exceptions }

  *)

exception FileDoesntExist of filename
exception RecursiveLink of filename

(** Generic error handling functions. Whenever such a function is available it
    helps report the error and allows to raise an exception. The [string]
    provided is the human readable version of ['a]. In most cases ['a] is a
    polymorphic variant.
  *)
type 'a error_handler = string -> 'a -> unit

(** Exception raised when after an [error_handler] the execution cannot
    continue. The rest of the workflow logic cannot handle the default case and
    the whole operation can be in the middle of transformation.
  *)
exception Fatal of string

(** Policy concerning links which are directories. *)
type action_link =
  | Follow
    (** We consider link as simple directory (it is dangerous) *)
  | Skip
    (** Just skip it *)
  | SkipInform of (filename -> unit)
    (** Skip and execute an action *)
  | AskFollow of (filename -> bool)
    (** Ask and wait for input, false means skip *)

(** For certain command, you should need to ask the user wether
    or not he wants to act.
  *)
type interactive =
  | Force (** Do it anyway *)
  | Ask of (filename -> bool) (** Promp the user *)


(*********************************************************************)
(**

   {2 Permission }

  *)

(** Base permission. This is the permission corresponding to one user or group.
  *)
type base_permission =
  {
    sticky: bool;
    exec: bool;
    write: bool;
    read: bool;
  }

(** Full permission. All the base permissions of a file.
  *)
type permission =
  {
    user: base_permission;
    group: base_permission;
    other: base_permission;
  }

(** Translate POSIX integer permission. *)
val permission_of_int: int -> permission

(** Return the POSIX integer permission *)
val int_of_permission: permission -> int

(** Permission symbolic mode. *)
module Mode:
sig
  type who = [`User | `Group | `Other | `All]
  type wholist = [ who | `List of who list ]
  type permcopy = [`User | `Group | `Other]
  type perm = [ `Read | `Write | `Exec | `ExecX | `Sticky | `StickyO ]
  type permlist = [ perm | `List of perm list ]
  type actionarg = [ permlist | permcopy ]
  type action = [ `Set of actionarg | `Add of actionarg | `Remove of actionarg]
  type actionlist = [ action | `List of action list ]
  type clause = [ `User of actionlist | `Group of actionlist
                | `Other of actionlist | `All of actionlist
                | `None of actionlist ]

  (** Typical symbolic mode:
   - g+r -> [`Group (`Add `Read)]
   - u=rw,g+rw,o-rwx ->
     [`User (`Set (`List [`Read; `Write]));
      `Group (`Add (`List [`Read; `Write]));
      `Other (`Remove (`List [`Read; `Write; `Exec]))]
   *)
  type t = clause list

  val to_string: t -> string
  val apply: is_dir:bool -> umask:int -> Unix.file_perm -> t -> Unix.file_perm
end

(*********************************************************************)
(**

   {2 Size operation}

  *)

(** File size
  *)
type size =
    TB of int64 (** Tera bytes *)
  | GB of int64 (** Giga bytes *)
  | MB of int64 (** Mega bytes *)
  | KB of int64 (** Kilo bytes *)
  | B  of int64 (** Bytes *)

(** Convert size to bytes. *)
val byte_of_size: size -> int64

(** Add two sizes. *)
val size_add: size -> size -> size

(** Compare two sizes, using the classical compare function. If fuzzy is set to
    true, the comparison is done on the most significant size unit of both
    value.
  *)
val size_compare: ?fuzzy:bool -> size -> size -> int

(** Convert a value to a string representation. If fuzzy is set to true, only
    consider the most significant unit
  *)
val string_of_size: ?fuzzy:bool -> size -> string

(*********************************************************************)
(**

   {2 stat }

  *)

(** Kind of file. This set is a combination of all POSIX file, some of them
    doesn't exist at all on certain file system or OS.
  *)
type kind =
    Dir
  | File
  | Dev_char
  | Dev_block
  | Fifo
  | Socket
  | Symlink (** @since 0.4.6 *)


(** Information about a file. This type is derived from Unix.stat
  *)
type stat =
  {
    kind: kind;
    is_link: bool;
    permission: permission;
    size: size;
    owner: int;
    group_owner: int;
    access_time: float;
    modification_time: float;
    creation_time: float;
    device: int;
    inode: int;
  }


(** [stat fln] Return information about the file (like Unix.stat)
    Non POSIX command.
  *)
val stat: ?dereference:bool -> filename -> stat

(*********************************************************************)
(**

  {2 umask }

  *)

exception UmaskError of string

(** Possible umask errors. *)
type umask_error = [ `Exc of exn | `NoStickyBit of int ]

(** Get or set the file mode creation mask.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/umask.html}POSIX documentation}.
  *)
val umask:
  ?error:(umask_error error_handler) ->
  ?mode:[< `Octal of int | `Symbolic of Mode.t ] ->
  [< `Octal of int -> 'a | `Symbolic of Mode.t -> 'a] ->
  'a

(** Apply umask to a given file permission.
  *)
val umask_apply: int -> int

(*********************************************************************)
(**

  {2 test }

  *)

(** Pattern you can use to test file. If the file doesn't exist the result is
    always false.
  *)
type test_file =
  | Is_dev_block                 (** FILE is block special *)
  | Is_dev_char                  (** FILE is character special *)
  | Is_dir                       (** FILE is a directory *)
  | Exists                       (** FILE exists *)
  | Is_file                      (** FILE is a regular file *)
  | Is_set_group_ID              (** FILE is set-group-ID *)
  | Has_sticky_bit               (** FILE has its sticky bit set *)
  | Is_link                      (** FILE is a symbolic link *)
  | Is_pipe                      (** FILE is a named pipe *)
  | Is_readable                  (** FILE is readable *)
  | Is_writeable                 (** FILE is writeable *)
  | Size_not_null                (** FILE has a size greater than zero *)
  | Size_bigger_than of size     (** FILE has a size greater than given size *)
  | Size_smaller_than of size    (** FILE has a size smaller than given size *)
  | Size_equal_to of size        (** FILE has the same size as given size *)
  | Size_fuzzy_equal_to of size  (** FILE has approximatively the same size as
                                     given size *)
  | Is_socket                    (** FILE is a socket *)
  | Has_set_user_ID              (** FILE its set-user-ID bit is set *)
  | Is_exec                      (** FILE is executable *)
  | Is_owned_by_user_ID          (** FILE is owned by the effective user ID *)
  | Is_owned_by_group_ID         (** FILE is owned by the effective group ID *)
  | Is_newer_than of filename    (** FILE1 is newer (modification date) than
                                     FILE2 *)
  | Is_older_than of filename    (** FILE1 is older than FILE2 *)
  | Is_newer_than_date of float  (** FILE is newer than given date *)
  | Is_older_than_date of float  (** FILE is older than given date *)
  | And of test_file * test_file (** Result of TEST1 and TEST2 *)
  | Or of test_file * test_file  (** Result of TEST1 or TEST2 *)
  | Not of test_file             (** Result of not TEST *)
  | Match of string              (** Compilable match (Str or PCRE or ...) *)
  | True                         (** Always true *)
  | False                        (** Always false *)
  | Has_extension of extension   (** Check extension *)
  | Has_no_extension             (** Check absence of extension *)
  | Is_parent_dir                (** Basename is the parent dir *)
  | Is_current_dir               (** Basename is the current dir *)
  | Basename_is of filename      (** Check the basename *)
  | Dirname_is of filename       (** Check the dirname *)
  | Custom of (filename -> bool) (** Custom operation on filename *)


(** Test a file.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/test.html}POSIX documentation}.
  *)
val test:
  ?match_compile:(filename -> filename -> bool) ->
  test_file -> filename -> bool

(*********************************************************************)
(**

  {2 chmod }

  *)

exception ChmodError of string

(** Possible chmod errors. *)
type chmod_error = [`Exc of exn]

(** Change permissions of files.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/chmod.html}POSIX documentation}.
  *)
val chmod:
  ?error:(chmod_error error_handler) ->
  ?recurse:bool ->
  [< `Octal of Unix.file_perm | `Symbolic of Mode.t ] ->
  filename list -> unit

(*********************************************************************)
(**

  {2 mkdir }

  *)

exception MkdirError of string

(** Possible mkdir errors. *)
type mkdir_error =
  [ `DirnameAlreadyUsed of filename
  | `Exc of exn
  | `MissingComponentPath of filename
  | `MkdirChmod of filename * Unix.file_perm * string * chmod_error ]

(** Create the directory which name is provided. Set [~parent] to true
    if you also want to create every directory of the path. Use mode to
    provide some specific right.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/mkdir.html}POSIX documentation}.
  *)
val mkdir:
  ?error:(mkdir_error error_handler) ->
  ?parent:bool ->
  ?mode:[< `Octal of Unix.file_perm | `Symbolic of Mode.t ] ->
  filename -> unit

(*********************************************************************)
(**

    {2 rm }

  *)

exception RmError of string

(** Possible rm errors. *)
type rm_error =
  [ `DirNotEmpty of filename
  | `Exc of exn
  | `NoRecurse of filename ]

(** Remove the filename provided. Set [~recurse] to true in order to
    completely delete a directory.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/rm.html}POSIX documentation}.
  *)
val rm:
  ?error:(rm_error error_handler) ->
  ?force:interactive -> ?recurse:bool -> filename list -> unit

(*********************************************************************)
(**

    {2 cp }

  *)

exception CpError of string

(** Possible cp errors. *)
type cp_error =
  [ `CannotChmodDstDir of filename * exn
  | `CannotCopyDir of filename
  | `CannotCopyFilesToFile of filename list * filename
  | `CannotCreateDir of filename * exn
  | `CannotListSrcDir of filename * exn
  | `CannotOpenDstFile of filename * exn
  | `CannotOpenSrcFile of filename * exn
  | `CannotRemoveDstFile of filename * exn
  | `DstDirNotDir of filename
  | `ErrorRead of filename * exn
  | `ErrorWrite of filename * exn
  | `Exc of exn
  | `NoSourceFile of filename
  | `PartialWrite of filename * int * int
  | `SameFile of filename * filename
  | `UnhandledType of filename * kind ]

(** Copy the hierarchy of files/directory to another destination.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/cp.html}POSIX documentation}.
  *)
val cp:
  ?follow:action_link ->
  ?force:interactive ->
  ?recurse:bool ->
  ?preserve:bool ->
  ?error:(cp_error error_handler) ->
  filename list -> filename -> unit

(*********************************************************************)
(**

    {2 mv }

  *)

exception MvError of string

(** Possible mv errors. *)
type mv_error =
  [ `Exc of exn
  | `MvCp of filename * filename * string * cp_error
  | `MvRm of  filename * string * rm_error
  | `NoSourceFile ]

(** Move files/directories to another destination.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/mv.html}POSIX documentation}.
  *)
val mv:
  ?error:(mv_error error_handler) ->
  ?force:interactive -> filename -> filename -> unit


(*********************************************************************)
(**

   {2 touch }

  *)

(** Time for file *)
type touch_time_t =
  | Touch_now                   (** Use Unix.gettimeofday *)
  | Touch_file_time of filename (** Get mtime of file *)
  | Touch_timestamp of float    (** Use GMT timestamp *)


(** Modify the timestamp of the given filename.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/touch.html}POSIX documentation}.
    If atime and mtime are not specified, they are both considered true. If only
    atime or mtime is sepcified, the other is false.
    @param atime  modify access time.
    @param mtime  modify modification time.
    @param create if file doesn't exist, create it, default true
    @param time   what time to set, default Touch_now
  *)
val touch:
  ?atime:bool ->
  ?mtime:bool ->
  ?create:bool -> ?time:touch_time_t -> filename -> unit

(*********************************************************************)
(**

   {2 ls }

  *)

(** Apply a filtering pattern to a filename.
  *)
val filter: test_file -> filename list -> filename list

(** List the content of a directory.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/ls.html}POSIX documentation}.
  *)
val ls: filename -> filename list

(*********************************************************************)
(**

  {2 Misc operations }

  *)

(** Return the current dir.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/pwd.html}POSIX documentation}.
  *)
val pwd: unit -> filename

(** Resolve to the real filename removing symlink.
    Non POSIX command.
  *)
val readlink: filename -> filename

(** Try to find the executable in the PATH. Use environement variable
    PATH if none is provided.
    Non POSIX command.
  *)
val which:
  ?path:filename list -> filename -> filename

(** [cmp skip1 fln1 skip2 fln2] Compare files [fln1] and [fln2] starting at pos
    [skip1] [skip2] and returning the first octect where a difference occurs.
    Returns [Some -1] if one of the file is not readable or if it is not a
    file.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/cmp.html}POSIX documentation}.
  *)
val cmp:
  ?skip1:int ->
  filename -> ?skip2:int -> filename -> int option

(** [du fln_lst] Return the amount of space of all the file
    which are subdir of fln_lst. Also return details for each
    file scanned.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/du.html}POSIX documentation}.
  *)
val du: filename list -> size * (filename * size) list

(** [find ~follow:fol tst fln exec accu] Descend the directory tree starting
    from the given filename and using the test provided. You cannot match
    [current_dir] and [parent_dir]. For every file found, the action [exec] is
    done, using the [accu] to start. For a simple file listing, you can use
    [find True "." (fun x y -> y :: x) []]
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/find.html}POSIX documentation}.
  *)
val find:
  ?follow:action_link ->
  ?match_compile:(filename -> filename -> bool) ->
  test_file ->
  filename -> ('a -> filename -> 'a) -> 'a -> 'a

(** For future release:
- [val pathchk: filename -> boolean * string], check whether file names are
  valid or portable
- [val setfacl: filename -> permission -> unit], set file access control
  lists (UNIX + extended attribute)
- [val getfacl: filename -> permission], get file access control lists

ACL related function will be handled through a plugin system to handle at
runtime which attribute can be read/write (i.e. Win32 ACL, NFS acl, Linux ACL --
or none).
*)
