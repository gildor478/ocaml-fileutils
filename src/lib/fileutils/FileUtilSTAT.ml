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

open FileUtilTypes
open FileUtilPermission


let stat ?(dereference=false) fln =
  let kind_of_stat ustat =
    match ustat.Unix.LargeFile.st_kind with
      | Unix.S_REG -> File
      | Unix.S_DIR -> Dir
      | Unix.S_CHR -> Dev_char
      | Unix.S_BLK -> Dev_block
      | Unix.S_FIFO -> Fifo
      | Unix.S_SOCK -> Socket
      | Unix.S_LNK -> Symlink
  in
  try
    let ustat = Unix.LargeFile.lstat fln in
    let is_link = (kind_of_stat ustat = Symlink) in
    let ustat =
      if is_link && dereference then
        Unix.LargeFile.stat fln
      else
        ustat
    in
    {
      kind              = kind_of_stat ustat;
      is_link           = is_link;
      permission        = permission_of_int ustat.Unix.LargeFile.st_perm;
      size              = B ustat.Unix.LargeFile.st_size;
      owner             = ustat.Unix.LargeFile.st_uid;
      group_owner       = ustat.Unix.LargeFile.st_gid;
      access_time       = ustat.Unix.LargeFile.st_atime;
      modification_time = ustat.Unix.LargeFile.st_mtime;
      creation_time     = ustat.Unix.LargeFile.st_ctime;
      device            = ustat.Unix.LargeFile.st_dev;
      inode             = ustat.Unix.LargeFile.st_ino;
    }
  with Unix.Unix_error(Unix.ENOENT, _, _) ->
    raise (FileDoesntExist fln)

