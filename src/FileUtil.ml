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
    directories. All function nearly match common POSIX utilities but in
    rewritten OCaml.

    @author Sylvain Le Gall
  *)

open FilePath

(** {2 Types and exceptions }*)

exception SizeInvalid
exception FileDoesntExist of filename
exception RecursiveLink of filename
exception RmDirNotEmpty of filename
exception RmDirNoRecurse of filename
exception MkdirMissingComponentPath of filename
exception MkdirDirnameAlreadyUsed of filename
exception CpError of string
exception MvNoSourceFile

(** Policy concerning links which are directories *)
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
    Force
  (** Do it anyway *)
  | Ask of (filename -> bool)
  (** Promp the user *)

(** File size
  *)
type size =
    TB of int64 (** Tera bytes *)
  | GB of int64 (** Giga bytes *)
  | MB of int64 (** Mega bytes *)
  | KB of int64 (** Kilo bytes *)
  | B  of int64 (** Bytes *)

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


(** Time for file *)
type touch_time_t =
  | Touch_now                   (** Use Unix.gettimeofday *)
  | Touch_file_time of filename (** Get mtime of file *)
  | Touch_timestamp of float    (** Use GMT timestamp *)


(** {2 Classical permission } *)

(** Translate POSIX integer permission.
  *)
let permission_of_int pr =
  let perm_match oct =
    (pr land oct) <> 0
  in
  {
    user =
      {
        sticky = perm_match 0o4000;
        exec   = perm_match 0o0100;
        write  = perm_match 0o0200;
        read   = perm_match 0o0400;
      };
    group =
      {
        sticky = perm_match 0o2000;
        exec   = perm_match 0o0010;
        write  = perm_match 0o0020;
        read   = perm_match 0o0040;
      };
    other =
      {
        sticky = perm_match 0o1000;
        exec   = perm_match 0o0001;
        write  = perm_match 0o0002;
        read   = perm_match 0o0004;
      };
  }

(** Return the POSIX integer permission *)
let int_of_permission pr =
  let permission_int = [
    (pr.user.sticky,  0o4000);
    (pr.user.exec,    0o0100);
    (pr.user.write,   0o0200);
    (pr.user.read,    0o0400);
    (pr.group.sticky, 0o2000);
    (pr.group.exec,   0o0010);
    (pr.group.write,  0o0020);
    (pr.group.read,   0o0040);
    (pr.other.sticky, 0o1000);
    (pr.other.exec,   0o0001);
    (pr.other.write,  0o0002);
    (pr.other.read,   0o0004)
  ]
  in
  List.fold_left (fun full_perm (b, perm) ->
    if b then
      perm lor full_perm
    else
      full_perm)
    0o0000 permission_int

(** {2 Size operation} *)

(** Convert size to bytes
  *)
let byte_of_size sz =
  let rec mul_1024 n i =
    if n > 0 then
      mul_1024
        (n - 1)
        (Int64.mul 1024L i)
    else
      i
  in
    match sz with
      | B i  -> i
      | KB i -> mul_1024 1 i
      | MB i -> mul_1024 2 i
      | GB i -> mul_1024 3 i
      | TB i -> mul_1024 4 i


(** Add two size
  *)
let size_add sz1 sz2 =
  B (Int64.add (byte_of_size sz1) (byte_of_size sz2))


(** Compare two size, using the classical compare function. If fuzzy is set to
    true, the comparison is done on the most significant size unit of both
    value.
  *)
let size_compare ?(fuzzy=false) sz1 sz2 =
  let by1 =
    byte_of_size sz1
  in
  let by2 =
    byte_of_size sz2
  in
    if fuzzy then begin
      let rec fuzzy_comp n1 n2 =
        if n1 = n2 then
          0
        else begin
          let up_unit_n1 =
            Int64.div n1 1024L
          in
          let up_unit_n2 =
            Int64.div n2 1024L
          in
            if up_unit_n1 <> 0L && up_unit_n2 <> 0L then
              fuzzy_comp up_unit_n1 up_unit_n2
            else
              Int64.compare n1 n2
        end
      in
        fuzzy_comp by1 by2
    end else
      Int64.compare by1 by2


(** Convert a value to a string representation. If fuzzy is set to true, only
    consider the most significant unit
  *)
let string_of_size ?(fuzzy=false) sz =
  let szstr i unt (cur_i, cur_unt, tl) =
    let tl =
      (cur_i, cur_unt) :: tl
    in
      i, unt, tl
  in

  let rec decomp_continue fup i unt acc =
    if i = 0L then
      szstr i unt acc
    else begin
      (** Continue with upper unit *)
      let r =
        Int64.rem i 1024L
      in
      let q =
        Int64.div i 1024L
      in
        decomp_start (szstr r unt acc) (fup q)
    end

  and decomp_start acc sz =
    (* Decompose size for current unit and try
     * to use upper unit
     *)
    match sz with
      | TB i ->
          szstr i "TB" acc
      | GB i ->
          decomp_continue (fun n -> TB n) i "GB" acc
      | MB i ->
          decomp_continue (fun n -> GB n) i "MB" acc
      | KB i ->
          decomp_continue (fun n -> MB n) i "KB" acc
      | B i ->
          decomp_continue (fun n -> KB n) i "B" acc
  in

  (* Only accumulate significant unit in tail *)
  let only_significant_unit (cur_i, cur_unt, lst) =
    let significant_lst =
      List.filter
        (fun (i, _) -> i <> 0L)
        ((cur_i, cur_unt) :: lst)
    in
      match significant_lst with
        | [] -> cur_i, cur_unt, []
        | (cur_i, cur_unt) :: tl -> (cur_i, cur_unt, tl)
  in

  let main_i, main_unt, rem_lst =
    only_significant_unit (decomp_start (0L, "B", []) sz)
  in

    if fuzzy then begin
      let _, rem =
        List.fold_left
          (fun (div, acc) (i, _unt) ->
             let acc =
               acc +. ((Int64.to_float i) /. div)
             in
               div *. 1024.0,
               acc)
          (1024.0, 0.0)
          rem_lst
      in
        Printf.sprintf "%.2f %s"
          ((Int64.to_float main_i) +. rem)
          main_unt
    end else begin
      String.concat
        " "
        (List.map
           (fun (i, unt) -> Printf.sprintf "%Ld %s" i unt)
           ((main_i, main_unt) :: rem_lst))
    end


(** {2 Operations on files and directories} *)

(**/**)
module SetFilename = Set.Make (struct
  type t = filename
  let compare = FilePath.compare
end)


let doit force fln =
  match force with
    Force -> true
  | Ask ask -> ask fln


let prevent_recursion fln_set fln =
  (* TODO: use a set of dev/inode *)
  if SetFilename.mem fln fln_set then
    raise (RecursiveLink fln)
  else
    SetFilename.add fln fln_set


let solve_dirname dirname =
  (* We have an ambiguity concerning "" and "." *)
  if is_current dirname then
    current_dir
  else
    reduce dirname


(**/**)

(** [stat fln] Return information about the file (like Unix.stat)
    Non POSIX command.
  *)
let stat ?(dereference=false) (fln: filename) =
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


(**/**)
let get_umask () =
  let cmask = Unix.umask 0o777 in
  let _mask: int = Unix.umask cmask in
    cmask


let apply_umask m =
    m land (lnot (get_umask ()))
(**/**)

(*
type mode =
    [`Octal of int;
     `Symbolic

 *)
(*
let umask ?mode out =
  match out with
    |
 *)


(** List the content of a directory
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/ls.html}POSIX documentation}.
  *)
let ls dirname =
  let array_dir = Sys.readdir (solve_dirname dirname) in
  let list_dir = Array.to_list array_dir in
    List.map
      (fun x -> concat dirname x)
      list_dir


(** Change permissions of files.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/chmod.html}POSIX documentation}.
  *)
let chmod ?(recurse=false) mode lst =
  let rec chmod_one fn =
    let st = stat fn in
      if st.kind = Dir && recurse then begin
        List.iter chmod_one (ls fn)
      end;
      if not st.is_link then begin
        let int_perm =
          match mode with
            | `Octal i -> i
            | `Symbolic t ->
                FileUtilMode.apply
                  ~is_dir:(st.kind = Dir)
                  ~umask:(get_umask ())
                  (int_of_permission st.permission) t
        in
          if int_perm <> int_of_permission st.permission then
            Unix.chmod fn int_perm
      end
  in
    List.iter chmod_one lst

(**/**)


let compile_filter ?(match_compile=(fun s fn -> s = fn)) flt =
  let cflt =
    let rec cc =
      function
        | True                   -> `Val true
        | False                  -> `Val false
        | Is_dev_block           -> `Stat (`Kind Dev_block)
        | Is_dev_char            -> `Stat (`Kind Dev_char)
        | Is_dir                 -> `Stat (`Kind Dir)
        | Is_file                -> `Stat (`Kind File)
        | Is_socket              -> `Stat (`Kind Socket)
        | Is_pipe                -> `Stat (`Kind Fifo)
        | Is_link                -> `Is_link
        | Is_set_group_ID        -> `Stat `Is_set_group_ID
        | Has_sticky_bit         -> `Stat `Has_sticky_bit
        | Has_set_user_ID        -> `Stat `Has_set_user_ID
        | Is_readable            -> `Stat `Is_readable
        | Is_writeable           -> `Stat `Is_writeable
        | Is_exec                -> `Stat `Is_exec
        | Size_not_null          -> `Stat (`Size (`Bigger, B 0L))
        | Size_bigger_than sz    -> `Stat (`Size (`Bigger, sz))
        | Size_smaller_than sz   -> `Stat (`Size (`Smaller, sz))
        | Size_equal_to sz       -> `Stat (`Size (`Equal, sz))
        | Size_fuzzy_equal_to sz -> `Stat (`Size (`FuzzyEqual, sz))
        | Is_owned_by_user_ID ->
            `Stat (`Is_owned_by_user_ID (Unix.geteuid ()))
        | Is_owned_by_group_ID ->
            `Stat (`Is_owned_by_group_ID (Unix.getegid ()))
        | Exists                 -> `Stat `Exists
        | Is_newer_than fn1      -> `Stat (`Newer (stat fn1).modification_time)
        | Is_older_than fn1      -> `Stat (`Older (stat fn1).modification_time)
        | Is_newer_than_date(dt) -> `Stat (`Newer dt)
        | Is_older_than_date(dt) -> `Stat (`Older dt)
        | Has_extension ext      -> `Has_extension ext
        | Has_no_extension       -> `Has_no_extension
        | Is_current_dir         -> `Is_current_dir
        | Is_parent_dir          -> `Is_parent_dir
        | Basename_is s          -> `Basename_is s
        | Dirname_is s           -> `Dirname_is s
        | Custom f               -> `Custom f
        | Match str              -> `Custom (match_compile str)
        | And(flt1, flt2) ->
            begin
              match cc flt1, cc flt2 with
                | `Val true, cflt | cflt, `Val true -> cflt
                | `Val false, cflt | cflt,  `Val false -> `Val false
                | cflt1, cflt2 -> `And (cflt1, cflt2)
            end
        | Or(flt1, flt2) ->
            begin
              match cc flt1, cc flt2 with
                | `Val true, _ | _, `Val true -> `Val true
                | `Val false, cflt | cflt,  `Val false -> cflt
                | cflt1, cflt2 -> `Or (cflt1, cflt2)
            end
        | Not flt ->
            begin
              match cc flt with
                | `Val b -> `Val (not b)
                | cflt -> `Not cflt
            end
    in
      cc flt
  in
  let need_statL, need_stat =
    let rec dfs =
      function
        | `Val _ | `Has_extension _ | `Has_no_extension | `Is_current_dir
        | `Is_parent_dir | `Basename_is _ | `Dirname_is _
        | `Custom _ ->
            false, false
        | `Stat _ ->
            true, false
        | `Is_link ->
            false, true
        | `And (cflt1, cflt2) | `Or (cflt1, cflt2) ->
            let need_stat1, need_statL1 = dfs cflt1 in
            let need_stat2, need_statL2 = dfs cflt2 in
              need_stat1 || need_stat2, need_statL1 || need_statL2
        | `Not cflt ->
            dfs cflt
    in
      dfs cflt
  in
    (* Compiled function to return. *)
    fun ?st_opt ?stL_opt fn ->
      let st_opt =
        if need_stat && st_opt = None then begin
          try
            match stL_opt with
              | Some st when not st.is_link -> stL_opt
              | _ -> Some (stat fn)
          with FileDoesntExist _ ->
            None
        end else
          st_opt
      in
      let stL_opt =
        if need_statL && stL_opt = None then begin
          try
            match st_opt with
              | Some st when not st.is_link -> st_opt
              | _ -> Some (stat ~dereference:true fn)
          with FileDoesntExist _ ->
            None
        end else
          stL_opt
      in
      let rec eval =
        function
          | `Val b -> b
          | `Has_extension ext ->
              begin
                try
                  check_extension fn ext
                with FilePath.NoExtension _ ->
                  false
              end
          | `Has_no_extension ->
              begin
                try
                  let _str: filename = chop_extension fn in
                    false
                with FilePath.NoExtension _ ->
                  true
              end
          | `Is_current_dir -> is_current (basename fn)
          | `Is_parent_dir -> is_parent (basename fn)
          | `Basename_is bn -> (FilePath.compare (basename fn) bn) = 0
          | `Dirname_is dn -> (FilePath.compare (dirname fn) dn) = 0
          | `Custom f -> f fn
          | `Stat e ->
              begin
                match stL_opt, e with
                  | Some stL, `Exists -> true
                  | Some stL, `Kind knd -> stL.kind = knd
                  | Some stL, `Is_set_group_ID -> stL.permission.group.sticky
                  | Some stL, `Has_sticky_bit -> stL.permission.other.sticky
                  | Some stL, `Has_set_user_ID -> stL.permission.user.sticky
                  | Some stL, `Size (cmp, sz) ->
                      begin
                        let diff = size_compare stL.size sz in
                          match cmp with
                            | `Bigger -> diff > 0
                            | `Smaller -> diff < 0
                            | `Equal -> diff = 0
                            | `FuzzyEqual ->
                                (size_compare ~fuzzy:true stL.size sz) = 0
                      end
                  | Some stL, `Is_owned_by_user_ID uid -> uid = stL.owner
                  | Some stL, `Is_owned_by_group_ID gid -> gid = stL.group_owner
                  | Some stL, `Is_readable ->
                      let perm = stL.permission in
                        perm.user.read || perm.group.read || perm.other.read
                  | Some stL, `Is_writeable ->
                      let perm = stL.permission in
                        perm.user.write || perm.group.write || perm.other.write
                  | Some stL, `Is_exec ->
                      let perm = stL.permission in
                        perm.user.exec || perm.group.exec || perm.other.exec
                  | Some stL, `Newer dt -> stL.modification_time > dt
                  | Some stL, `Older dt -> stL.modification_time < dt
                  | None, _ -> false
              end
          | `Is_link ->
              begin
                match st_opt with
                  | Some st -> st.is_link
                  | None -> false
              end
          | `And (cflt1, cflt2) ->
              (eval cflt1) && (eval cflt2)
          | `Or (cflt1, cflt2) ->
              (eval cflt1) || (eval cflt2)
          | `Not cflt ->
              not (eval cflt)
      in
        eval cflt


let all_upper_dir fln =
  let rec all_upper_dir_aux lst fln =
    let dir = dirname fln in
    match lst with
      prev_dir :: tl when prev_dir = dir ->
      lst
    | _ ->
	all_upper_dir_aux (dir :: lst) dir
    in
      all_upper_dir_aux [fln] fln


(**/**)

(** Test a file.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/test.html}POSIX documentation}.
  *)
let test ?match_compile tst =
  let ctst =
    compile_filter ?match_compile tst
  in
    fun fln -> ctst (solve_dirname fln)

(**/**)
let test_exists =
  test (Or(Exists, Is_link))
(**/**)

(** Return the currend dir
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/pwd.html}POSIX documentation}.
  *)
let pwd () =
  reduce (Sys.getcwd ())


(** Resolve to the real filename removing symlink
    Non POSIX command.
  *)
let readlink fln =
  let ctst = compile_filter Is_link in
  let rec readlink_aux already_read fln =
    let newly_read = prevent_recursion already_read fln in
    let dirs = all_upper_dir fln in
      try
        let src_link = List.find ctst (List.rev dirs) in
        let dst_link = Unix.readlink src_link in
        let real_link =
          if is_relative dst_link then
            reduce (concat (dirname src_link) dst_link)
          else
            reduce dst_link
        in
          readlink_aux newly_read (reparent src_link real_link fln)
      with Not_found ->
        fln
  in
    readlink_aux SetFilename.empty (make_absolute (pwd ()) fln)


(** Apply a filtering pattern to a filename
  *)
let filter flt lst =
  List.filter (test flt) lst


(** Try to find the executable in the PATH. Use environement variable
    PATH if none is provided
    Non POSIX command.
  *)
let which ?(path) fln =
  let real_path =
    match path with
      | None ->
          path_of_string
            (try
               Sys.getenv "PATH"
             with Not_found ->
               "")
      | Some x ->
        x
  in

  let exec_test =
    test (And(Is_exec, Is_file))
  in

  let which_path =
    match Sys.os_type with
      | "Win32" ->
          (
            let real_ext =
              List.map
                (fun dot_ext ->
                   (* Remove leading "." if it exists *)
                   if (String.length dot_ext) >= 1 && dot_ext.[0] = '.' then
                     String.sub dot_ext 1 ((String.length dot_ext) - 1)
                   else
                     dot_ext)
                (* Extract possible extension from PATHEXT *)
                (path_of_string
                   (try
                      Sys.getenv "PATHEXT"
                    with Not_found ->
                      ""))
            in

            let to_filename dirname ext =
              add_extension (concat dirname fln) ext
            in

            let ctst dirname ext =
              exec_test (to_filename dirname ext)
            in

              List.fold_left
                (fun found dirname ->
                   if found = None then begin
                     try
                       let ext =
                         List.find (ctst dirname) real_ext
                       in
                         Some (to_filename dirname ext)
                     with Not_found ->
                       None
                   end else
                     found)
                None
                real_path
          )
      | _ ->
          (
            let to_filename dirname =
              concat dirname fln
            in

            try
              Some
                (to_filename
                   (List.find
                      (fun dirname ->
                         exec_test (to_filename dirname)) real_path))
            with Not_found ->
              None
          )
  in
    match which_path with
      | Some fn -> fn
      | None -> raise Not_found


(** Create the directory which name is provided. Turn parent to true
    if you also want to create every topdir of the path. Use mode to
    provide some specific right (default 755).
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/mkdir.html}POSIX documentation}.
  *)
let mkdir ?(parent=false) ?mode fln =
  let mode =
    match mode with
      | Some m -> m
      | None -> apply_umask 0o0777
  in
  let mkdir_simple fln =
    if test_exists fln then begin
      if test (Not Is_dir) fln then
        raise (MkdirDirnameAlreadyUsed fln)
    end else begin
      try
        Unix.mkdir fln mode
      with Unix.Unix_error(Unix.ENOENT, _, _)
        | Unix.Unix_error(Unix.ENOTDIR, _, _) ->
        raise (MkdirMissingComponentPath fln)
    end
  in
  let directories =
    if parent then
      all_upper_dir fln
    else
      [fln]
  in
  List.iter mkdir_simple directories


(** Modify the timestamp of the given filename.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/touch.html}POSIX documentation}.
    If atime and mtime are not specified, they are both considered true. If only
    atime or mtime is sepcified, the other is false.
    @param atime  modify access time.
    @param mtime  modify modification time.
    @param create if file doesn't exist, create it, default true
    @param time   what time to set, default Touch_now
  *)
let touch ?atime ?mtime ?(create=true) ?(time=Touch_now) fln =

  let atime, mtime =
    match atime, mtime with
      | None, None -> true, true
      | Some b, None -> b, false
      | None, Some b -> false, b
      | Some b1, Some b2 -> b1, b2
  in

  let set_time () =
    let fatime, fmtime =
      match time with
        | Touch_now ->
            0.0, 0.0
        | Touch_timestamp time_ref ->
            time_ref, time_ref
        | Touch_file_time fln_ref ->
            let st = stat fln_ref in
              st.access_time, st.modification_time
    in
    let fatime, fmtime =
      if not (atime && mtime) then begin
        let st = stat fln in
          (if atime then fatime else st.access_time),
          (if mtime then fmtime else st.modification_time)
      end else begin
        fatime, fmtime
      end
    in
      Unix.utimes fln fatime fmtime
  in
    (* Create file if required *)
    if test_exists fln then begin
      set_time ()
    end else if create then begin
      close_out (open_out fln);
      set_time ()
    end


(** [find ~follow:fol tst fln exec accu] Descend the directory tree starting
    from the given filename and using the test provided. You cannot match
    [current_dir] and [parent_dir]. For every file found, the action [exec] is
    done, using the [accu] to start. For a simple file listing, you can use
    [find True "." (fun x y -> y :: x) []]
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/find.html}POSIX documentation}.
  *)
let find ?(follow=Skip) ?match_compile tst fln exec user_acc =

  let user_test = compile_filter ?match_compile tst in

  let skip_action =
    match follow with
      | Skip | AskFollow _ | Follow -> ignore
      | SkipInform f -> f
  in

  let should_skip fln already_followed =
    match follow with
      | Skip | SkipInform _ -> true
      | AskFollow f ->
          if not already_followed then
            f fln
          else
            true
      | Follow ->
          if already_followed then
            raise (RecursiveLink fln)
          else
            false
  in

  let already_read = ref SetFilename.empty in

  let rec find_aux acc fln =
    let st_opt =
      try
        Some (stat fln)
      with FileDoesntExist _ ->
        None
    in
    let stL_opt =
      match st_opt with
        | Some st when st.is_link ->
            begin
              try
                Some (stat ~dereference:true fln)
              with FileDoesntExist _ ->
                None
            end
        | _ ->
            st_opt
    in
    let acc =
      if user_test ?st_opt ?stL_opt fln then
        exec acc fln
      else
        acc
    in
      match st_opt with
        | Some st ->
            if st.kind = Symlink then begin
              follow_symlink stL_opt acc fln
            end else if st.kind = Dir then begin
              enter_dir acc fln
            end else begin
              acc
            end
        | None -> acc

  and enter_dir acc drn =
    Array.fold_left
      (fun acc rfln ->
         if is_parent rfln || is_current rfln then
           acc
         else
           find_aux acc (concat drn rfln))
      acc
      (Sys.readdir drn)

  and follow_symlink stL_opt acc fln =
      match stL_opt with
        | Some stL when stL.kind = Dir ->
            let cur_link = readlink fln in
            let already_followed =
              try
                already_read := prevent_recursion !already_read cur_link;
                false
              with RecursiveLink _ ->
                true
            in
              if should_skip fln already_followed then begin
                skip_action fln;
                acc
              end else begin
                enter_dir acc fln
              end
        | _ ->
            acc
  in
    find_aux user_acc (reduce fln)


(** Remove the filename provided. Turn recurse to true in order to
    completely delete a directory
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/rm.html}POSIX documentation}.
  *)
let rm ?(force=Force) ?(recurse=false) fln_lst =
  let test_dir =
    test (And(Is_dir, Not(Is_link)))
  in

  let rmdir fn =
    try
      Unix.rmdir fn
    with Unix.Unix_error(Unix.ENOTEMPTY, _, _) ->
      raise (RmDirNotEmpty fn)
  in

  let rec rm_aux lst =
    List.iter
      (fun fn ->
         let exists =
           try
             let _st: Unix.stats = Unix.lstat fn in
               true
           with Unix.Unix_error(Unix.ENOENT, _, _) ->
             false
         in
         if exists && (doit force fn) then begin
           if test_dir fn then begin
             if recurse then begin
               rm_aux (ls fn);
               rmdir fn
             end else
               raise (RmDirNoRecurse fn)
           end else
             Unix.unlink fn
         end)
      lst
  in

    rm_aux fln_lst


(**/**)
exception CpSkip


let same_file st1 st2 =
  st1.device = st2.device && st1.inode = st2.inode
(**/**)

(** Copy the hierarchy of files/directory to another destination
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/cp.html}POSIX documentation}.
  *)
let cp ?(follow=Skip)
       ?(force=Force)
       ?(recurse=false)
       ?(preserve=false)
       ?(error=(fun str _ -> raise (CpError str)))
       fln_src_lst fln_dst =

  let handle_error e =
    let exs () e =
      match e with
        | Unix.Unix_error(err, _, _) -> Unix.error_message err
        | e -> Printexc.to_string e
    in
    let spf fmt = Printf.sprintf fmt in
    let str =
      match e with
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
        | `CannotCopyFilesToFile(fn_src_lst, fn_dst) ->
            spf "Cannot copy a list of files to another file '%s'." fn_dst
    in
      error str e;
      raise CpSkip
  in

  let handle_exception f a h =
    try
      f a
    with e ->
      handle_error (h e)
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

  let buffer = String.make 1024 ' ' in

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
            rm [fn_dst]
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
                     (Unix.read fd_src buffer 0) (String.length buffer)
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
              if preserve then src_mode else apply_umask src_mode
            in
              dst_mode lor 0o0700
          in
            handle_exception
              (mkdir ~mode) fn_dst
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
              if preserve then src_mode else apply_umask src_mode
          in
            handle_exception
              (chmod (`Octal mode)) [fn_dst]
              (fun e -> `CannotChmodDstDir(fn_dst, e));
            copy_time_props st_src fn_dst
        end
    end

  and cp_one fn_src fn_dst =
    let st_src =
      (* Check existence of source files. *)
      if test_exists fn_src then
        stat fn_src
      else
        handle_error (`NoSourceFile fn_src)
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
          | Symlink -> cp_symlink fn_src fn_dst
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


(** Move files/directories to another destination
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/mv.html}POSIX documentation}.
  *)
let rec mv ?(force=Force) fln_src fln_dst =
  let fln_src_abs =  make_absolute (pwd ()) fln_src in
  let fln_dst_abs =  make_absolute (pwd ()) fln_dst in
  if compare fln_src_abs fln_dst_abs <> 0 then begin
    if test_exists fln_dst_abs && doit force fln_dst then begin
        rm [fln_dst_abs];
        mv fln_src_abs fln_dst_abs
    end else if test Is_dir fln_dst_abs then begin
      mv ~force
        fln_src_abs
        (make_absolute
           fln_dst_abs
           (basename fln_src_abs))
    end else if test_exists fln_src_abs then begin
      try
        Sys.rename fln_src_abs fln_dst_abs
      with Sys_error _ ->
        cp ~force ~recurse:true [fln_src_abs] fln_dst_abs;
        rm ~force ~recurse:true [fln_src_abs]
    end else
      raise MvNoSourceFile
  end


(** [cmp skip1 fln1 skip2 fln2] Compare files [fln1] and [fln2] starting at pos
    [skip1] [skip2] and returning the first octect where a difference occurs.
    Returns [Some -1] if one of the file is not readable or if it is not a
    file.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/cmp.html}POSIX documentation}.
  *)
let cmp ?(skip1 = 0) fln1 ?(skip2 = 0) fln2 =
  if (reduce fln1) = (reduce fln2) then
    None
  else if (test (And(Is_readable, Is_file)) fln1)
      && (test (And(Is_readable, Is_file)) fln2) then begin
    let fd1 = open_in_bin fln1 in
    let fd2 = open_in_bin fln2 in
    let clean_fd () =
      let () = try close_in fd1 with _ -> () in
      let () = try close_in fd2 with _ -> () in
        ()
    in

    let test_empty st =
      try
        Stream.empty st;
        true
      with Stream.Failure ->
        false
    in

    let _ = seek_in fd1 skip1 in
    let _ = seek_in fd2 skip2 in
    let stream1 = Stream.of_channel fd1 in
    let stream2 = Stream.of_channel fd2 in
    try
      begin
        while ((Stream.next stream1) = (Stream.next stream2)) do
          ()
        done;
        clean_fd ();
        Some (Stream.count stream1)
      end
    with
      | Stream.Failure ->
          begin
            match ((test_empty stream1), (test_empty stream2)) with
                true, true  ->
                  None
              | true, false
              | false, true
              (* Don't know how this case could be... *)
              | false, false ->
                  clean_fd ();
                  Some (Stream.count stream1)
          end
      | e ->
          clean_fd ();
          raise e
  end else
    Some (-1)


(** [du fln_lst] Return the amount of space of all the file
    which are subdir of fln_lst. Also return details for each
    file scanned.
    See {{:http://pubs.opengroup.org/onlinepubs/007904875/utilities/du.html}POSIX documentation}.
  *)
let du fln_lst =
  let du_aux (sz, lst) fln =
    let st = stat fln in
      (size_add sz st.size, (fln, st.size) :: lst)
  in
    List.fold_left
      (fun (accu: size * (filename * size) list) fln ->
         find True fln du_aux accu)
      (B 0L, [])
    fln_lst


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
