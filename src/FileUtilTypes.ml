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

open FilePath

exception FileDoesntExist of filename
exception RecursiveLink of filename
exception Fatal of string

(** See FileUtil.mli *)
type action_link =
  | Follow
  | Skip
  | SkipInform of (filename -> unit)
  | AskFollow of (filename -> bool)


(** See FileUtil.mli *)
type interactive =
    Force
  | Ask of (filename -> bool)


(** See FileUtil.mli *)
type size =
    TB of int64
  | GB of int64
  | MB of int64
  | KB of int64
  | B  of int64


(** See FileUtil.mli *)
type kind =
    Dir
  | File
  | Dev_char
  | Dev_block
  | Fifo
  | Socket
  | Symlink


(** See FileUtil.mli *)
type base_permission =
  {
    sticky: bool;
    exec: bool;
    write: bool;
    read: bool;
  }


(** See FileUtil.mli *)
type permission =
  {
    user: base_permission;
    group: base_permission;
    other: base_permission;
  }


(** See FileUtil.mli *)
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


(** See FileUtil.mli *)
type test_file =
  | Is_dev_block
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
  | Size_bigger_than of size
  | Size_smaller_than of size
  | Size_equal_to of size
  | Size_fuzzy_equal_to of size
  | Is_socket
  | Has_set_user_ID
  | Is_exec
  | Is_owned_by_user_ID
  | Is_owned_by_group_ID
  | Is_newer_than of filename
  | Is_older_than of filename
  | Is_newer_than_date of float
  | Is_older_than_date of float
  | And of test_file * test_file
  | Or of test_file * test_file
  | Not of test_file
  | Match of string
  | True
  | False
  | Has_extension of extension
  | Has_no_extension
  | Is_parent_dir
  | Is_current_dir
  | Basename_is of filename
  | Dirname_is of filename
  | Custom of (filename -> bool)


(** See FileUtil.mli *)
type touch_time_t =
  | Touch_now
  | Touch_file_time of filename
  | Touch_timestamp of float

