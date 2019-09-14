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
open FilePath
open FileUtilMisc
open FileUtilPermission
open FileUtilTOUCH
open FileUtilRM
open FileUtilSTAT
open FileUtilUMASK
open FileUtilMKDIR
open FileUtilCHMOD
open FileUtilTEST

exception CpError of string
exception CpSkip

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


let same_file st1 st2 =
  st1.device = st2.device && st1.inode = st2.inode

let[@ warning "-27"] cp
    ?(follow=Skip)
    ?(force=Force)
    ?(recurse=false)
    ?(preserve=false)
    ?(error=(fun str _ -> raise (CpError str)))
    fln_src_lst
    fln_dst =

  let herror, _ =
    let spf fmt = Printf.sprintf fmt in
    let exs () e =
      match e with
      | Unix.Unix_error(err, _, _) -> Unix.error_message err
      | e -> Printexc.to_string e
    in
    handle_error_gen "cp" error
      (function
       | `CannotRemoveDstFile(fn_dst, e) ->
           spf "Cannot remove destination file '%s': %a." fn_dst exs e
       | `CannotOpenDstFile(fn_dst, e) ->
           spf "Cannot open destination file '%s': %a." fn_dst exs e
       | `CannotOpenSrcFile(fn_src, e) ->
           spf "Cannot open source file '%s': %a." fn_src exs e
       | `ErrorRead(fn_src, e) ->
           spf "Error reading file '%s': %a." fn_src exs e
       | `ErrorWrite(fn_dst, e) ->
           spf "Error writing file '%s': %a." fn_dst exs e
       | `PartialWrite(fn_dst, read, written) ->
           spf
             "Partial write to file '%s': %d read, %d written."
             fn_dst
             read
             written
       | `CannotCopyDir fn_src ->
           spf "Cannot copy directory '%s' recursively." fn_src
       | `DstDirNotDir fn_dst ->
           spf "Destination '%s' is not a directory." fn_dst
       | `CannotCreateDir(fn_dst, e) ->
           spf "Cannot create directory '%s': %a." fn_dst exs e
       | `CannotListSrcDir(fn_src, e) ->
           spf "Cannot list directory '%s': %a." fn_src exs e
       | `CannotChmodDstDir(fn_dst, e) ->
           spf "'Cannot chmod directory %s': %a." fn_dst exs e
       | `NoSourceFile fn_src ->
           spf "Source file '%s' doesn't exist." fn_src
       | `SameFile(fn_src, fn_dst) ->
           spf "'%s' and '%s' are the same file." fn_src fn_dst
       | `UnhandledType(fn_src, _) ->
           spf "Cannot handle the type of kind for file '%s'." fn_src
       | `CannotCopyFilesToFile(_, fn_dst) ->
           spf "Cannot copy a list of files to another file '%s'." fn_dst
       | #exc -> "")
  in
  let handle_error e =
    herror ~fatal:false e;
    raise CpSkip
  in
  let handle_exception f a h =
    try
      f a
    with e ->
      herror ~fatal:false (h e);
      raise CpSkip
  in

  let copy_time_props st_src fln_dst =
    if preserve then begin
      touch
        ~time:(Touch_timestamp st_src.modification_time)
        ~mtime:true
        ~create:false
        fln_dst;
      touch
        ~time:(Touch_timestamp st_src.access_time)
        ~atime:true
        ~create:false
        fln_dst;
    end
  in

  let buffer = Bytes.make 1024 ' ' in

  let cp_file st_src dst_exists fn_src fn_dst =
    let mode = int_of_permission st_src.permission in
    (* POSIX conditions: *)
    (* 3a *)
    let fd_dst =
      (* 3ai *)
      if dst_exists && doit force fn_dst then begin
        try
          (* 3aii *)
          Unix.openfile fn_dst [Unix.O_WRONLY; Unix.O_TRUNC] mode
        with _ ->
          (* 3aii *)
          handle_exception
            (fun lst -> rm lst) [fn_dst]
            (fun e -> `CannotRemoveDstFile(fn_dst, e));
          handle_exception
            (Unix.openfile fn_dst [Unix.O_WRONLY; Unix.O_CREAT]) mode
            (fun e -> `CannotOpenDstFile(fn_dst, e))
      end else if not dst_exists then begin
        handle_exception
          (Unix.openfile fn_dst [Unix.O_WRONLY; Unix.O_CREAT]) mode
          (fun e -> `CannotOpenDstFile(fn_dst, e))
      end else begin
        raise CpSkip
      end
    in
    let read = ref 0 in
      try
        let fd_src =
          handle_exception
            (Unix.openfile fn_src [Unix.O_RDONLY]) 0o600
            (fun e -> `CannotOpenSrcFile(fn_src, e))
        in
          try
            while (read :=
                   handle_exception
                     (Unix.read fd_src buffer 0) (Bytes.length buffer)
                     (fun e -> `ErrorRead(fn_src, e));
                   !read <> 0) do
              let written =
                handle_exception
                  (Unix.write fd_dst buffer 0) !read
                  (fun e -> `ErrorWrite(fn_dst, e))
              in
                if written != !read then
                  handle_error (`PartialWrite(fn_src, !read, written))
            done;
            Unix.close fd_src;
            Unix.close fd_dst;
            copy_time_props st_src fn_dst
          with e ->
            Unix.close fd_src;
            raise e
      with e ->
        Unix.close fd_dst;
        raise e
  in

  let cp_symlink fn_src fn_dst =
    (* No Unix.lutimes to set time of the symlink. *)
    Unix.symlink (Unix.readlink fn_src) fn_dst
  in

  let rec cp_dir st_src dst_exists fn_src fn_dst =
    (* 2a *)
    if not recurse then begin
      handle_error (`CannotCopyDir fn_src)
    (* 2d, 2c *)
    end else if dst_exists && (stat fn_dst).kind <> Dir then begin
      handle_error (`DstDirNotDir fn_dst)
    end else begin
      (* 2e *)
      let dst_created =
        if not dst_exists then begin
          let mode =
            let src_mode = int_of_permission st_src.permission in
            let dst_mode =
              if preserve then src_mode else umask_apply src_mode
            in
              `Octal (dst_mode lor 0o0700)
          in
            handle_exception
              (fun fn -> mkdir ~mode fn) fn_dst
              (fun e -> `CannotCreateDir(fn_dst, e));
            true
        end else begin
          false
        end
      in
        (* 2f *)
        Array.iter
          (fun bn ->
             if not (is_current bn || is_parent bn) then
               cp_one (concat fn_src bn) (concat fn_dst bn))
          (handle_exception
             Sys.readdir fn_src
             (fun e -> `CannotListSrcDir(fn_src, e)));
        (* 2g *)
        if dst_created then begin
          let mode =
            let src_mode = int_of_permission st_src.permission in
              `Octal (if preserve then src_mode else umask_apply src_mode)
          in
            handle_exception
              (chmod mode) [fn_dst]
              (fun e -> `CannotChmodDstDir(fn_dst, e));
            copy_time_props st_src fn_dst
        end
    end

  and cp_one fn_src fn_dst =
    let st_src, st_src_deref =
      (* Check existence of source files. *)
      if test_exists fn_src then begin
        let st = stat fn_src in
        if st.kind = Symlink && not recurse then begin
          st, stat ~dereference:true fn_src
        end else begin
          st, st
        end
      end else begin
        handle_error (`NoSourceFile fn_src)
      end
    in

    let same_file, dst_exists =
      (* Test if fn_dst exists and if it is the same file as fn_src. *)
      try
        same_file st_src (stat fn_dst), true
      with FileDoesntExist _ ->
        false, false
    in

      if same_file then begin
        handle_error (`SameFile(fn_src, fn_dst))
      end;
      try
        match st_src.kind with
          | Dir -> cp_dir st_src dst_exists fn_src fn_dst
          | File -> cp_file st_src dst_exists fn_src fn_dst
          | Symlink ->
            if st_src_deref.kind = Dir || recurse then
              cp_symlink fn_src fn_dst
            else
              cp_file st_src_deref dst_exists fn_src fn_dst
          | Fifo | Dev_char | Dev_block | Socket ->
              handle_error (`UnhandledType(fn_src, st_src.kind))
      with CpSkip ->
        ()
  in
    if test Is_dir fln_dst then
      List.iter
        (fun fn_src ->
           cp_one fn_src (concat fln_dst (basename fn_src)))
        fln_src_lst
    else if List.length fln_src_lst <= 1 then
      List.iter
        (fun fn_src -> cp_one fn_src fln_dst)
        fln_src_lst
    else
      handle_error (`CannotCopyFilesToFile(fln_src_lst, fln_dst))
