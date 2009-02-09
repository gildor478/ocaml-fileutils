(**************************************************************************)
(*   Ocaml-fileutils                                                      *)
(*                                                                        *)
(*   Copyright (C) 2003, 2004 Sylvain Le Gall <sylvain@le-gall.net>       *)
(*                                                                        *)
(*   This program is free software; you can redistribute it and/or        *)
(*   modify it under the terms of the GNU Library General Public          *)
(*   License as published by the Free Software Foundation; either         *)
(*   version 2 of the License, or any later version ; with the OCaml      *)
(*   static compilation exception.                                        *)
(*                                                                        *)
(*   This program is distributed in the hope that it will be useful,      *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 *)
(*   See the LICENCE file for more details.                               *)
(*                                                                        *)
(*   You should have received a copy of the GNU General Public License    *)
(*   along with this program; if not, write to the Free Software          *)
(*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA             *)
(*   02111-1307  USA                                                      *)
(*                                                                        *)
(*   Contact: sylvain@le-gall.net                                         *)
(*                                                                        *)
(**************************************************************************)

(** POSIX utils for files and directories.
    
    A module to provide the core POSIX utilities to manipulate files and
    directorys. All function nearly match common POSIX utilities but try to be
    more portable.
    
    @author Sylvain Le Gall
  *)

open FilePath

(** {2 Types and exceptions }*)

exception SizeInvalid;;
exception FileDoesntExist
exception RecursiveLink of filename
exception RmDirNotEmpty of filename
exception MkdirMissingComponentPath of filename
exception MkdirDirnameAlreadyUsed of filename
exception CpCannotCopy of filename
exception CpNoSourceFile of filename
exception CpCannotCopyFilesToFile of (filename list) * filename 
exception CpCannotCopyDir of filename
exception MvNoSourceFile

(** The policy concerning the links which are directory *)
type action_link =
    Follow
    (** We consider links as simple directory (it is dangerous) *)
  | Skip 
  (** Just skip it *)
  | SkipInform of (filename -> unit)
  (** Skip and execute an action *)
  | AskFollow of (filename -> bool)
  (** Ask and wait for input : true means follow and false means
      skip *)
      
(** For certain command, you should need to ask the user wether
    or not he does want to do some action. Provide the function 
    to Ask or Force the action *)
type interactive =
    Force
  (** Do it anyway *)
  | Ask of (filename -> bool)
  (** Promp the user *)

type size = 
    TB of float
    (** Terra bytes *)
  | GB of float
    (** Giga bytes *)
  | MB of float
    (** Mega bytes *)
  | KB of float
    (** Kilo bytes *)
  | B  of float
    (** Bytes *)
    
(** Kind of file. This set is a combination of all POSIX file, some of them
    doesn't exist at all on certain file system *)
type kind = 
    Dir  
  | File 
  | Dev_char
  | Dev_block
  | Link
  | Fifo
  | Socket
  
(** Base permission. This is the base type for one set of permission *)
type base_permission = 
  {
    sticky : bool;
    exec   : bool;
    write  : bool;
    read   : bool;
  }

(** Permission. All the base permission of a file *)
type permission =
  {
    user  : base_permission;
    group : base_permission;
    other : base_permission;
  }
  
(** Information about a file. This type is derived from Unix.stat *)
type stat =
  {
    kind              : kind;
    is_link           : bool;
    permission        : permission;
    size              : size;
    owner             : int;
    group_owner       : int;
    access_time       : float;
    modification_time : float;
    creation_time     : float;
  }

(** Pattern you can use to test file *)
type test_file =
| Is_dev_block                 (** FILE exists and is block special *)
| Is_dev_char                  (** FILE exists and is character special *)
| Is_dir                       (** FILE exists and is a directory *)
| Exists                       (** FILE exists *)
| Is_file                      (** FILE exists and is a regular file *)
| Is_set_group_ID              (** FILE exists and is set-group-ID *)
| Has_sticky_bit               (** FILE exists and has its sticky bit set *)
| Is_link                      (** FILE exists and is a symbolic link *)
| Is_pipe                      (** FILE exists and is a named pipe *)
| Is_readable                  (** FILE exists and is readable *)
| Is_writeable                 (** FILE exists and is writeable *)
| Size_not_null                (** FILE exists and has a size greater than zero *)
| Size_bigger_than of size     (** FILE exists and has a size greater than given size *)
| Size_smaller_than of size    (** FILE exists and has a size smaller than given size *)
| Size_equal_to of size        (** FILE exists and has the same size as given size *)
| Size_fuzzy_equal_to of size  (** FILE exists and has approximatively the same size as given size *)
| Is_socket                    (** FILE exists and is a socket *)
| Has_set_user_ID              (** FILE exists and its set-user-ID bit is set *)
| Is_exec                      (** FILE exists and is executable *)
| Is_owned_by_user_ID          (** FILE exists and is owned by the effective user ID *)
| Is_owned_by_group_ID         (** FILE exists and is owned by the effective group ID *)
| Is_newer_than of filename    (** FILE1 is newer (modification date) than FILE2 *)
| Is_older_than of filename    (** FILE1 is older than FILE2 *)
| Is_newer_than_date of float  (** FILE is newer than given date *)
| Is_older_than_date of float  (** FILE is older than given date *)
| And of test_file * test_file (** Result of TEST1 and TEST2 *)
| Or of test_file * test_file  (** Result of TEST1 or TEST2 *)
| Not of test_file             (** Result of not TEST *)
| Match of string              (** Match regexp *)
| True                         (** Always true *)
| False                        (** Always false *)
| Has_extension of extension   (** Check extension *)
| Has_no_extension             (** Check absence of extension *)
| Is_parent_dir                (** Is it the parent dir *)
| Is_current_dir               (** Is it the current dir *)
| Basename_is of filename      (** Check the basename *)
| Dirname_is of filename       (** Check the dirname *)
| Custom of (filename -> bool) (** Custom operation on filename *)


(** {2 Classical permission } *)

(** Understand the POSIX permission integer norm *)
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

(** Return the POSIX integer permission associated to the permission *)
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
  List.fold_left (fun full_perm (b,perm) -> 
    if b then 
      perm lor full_perm 
    else 
      full_perm) 
    0o0000 permission_int
  
(** {2 Size operation} *)

(** Convert to the upper unit a size *)
let size_convert_down sz =
  match sz with
    TB f -> GB (f *. 1024.0)
  | GB f -> MB (f *. 1024.0)
  | MB f -> KB (f *. 1024.0)
  | KB f -> B  (f *. 1024.0)
  | B  f -> B f

(** Convert to the smaller unit a size *)
let size_convert_up sz = 
  match sz with 
    TB f -> TB f
  | GB f -> TB (f /. 1024.0)
  | MB f -> GB (f /. 1024.0)
  | KB f -> MB (f /. 1024.0)
  | B  f -> KB (f /. 1024.0)

(** Compare two units of size : classification of size is 
  [ To, Go, Mo, Ko, O ], with To being the bigger unit *)
let size_compare_unit sz1 sz2 =
  let value_unit sz = 
    match sz with
      TB _ -> 4
    | GB _ -> 3
    | MB _ -> 2
    | KB _ -> 1
    |  B _ -> 0
  in
  (value_unit sz1) - (value_unit sz2)
 
(** size_to_same_unit sz1 sz2 : convert sz2 to the unit of sz1 *)
let size_to_same_unit sz1 sz2 =
  let rec size_to_same_unit_aux sz = 
    if (size_compare_unit sz1 sz) < 0 then
      size_to_same_unit_aux (size_convert_down sz)
    else if (size_compare_unit sz1 sz) > 0 then
      size_to_same_unit_aux (size_convert_up sz)
    else
      sz
  in
  size_to_same_unit_aux sz2
      
(** Convert a size to To *)
let size_to_To sz = size_to_same_unit (TB 0.0) sz

(** Convert a size to Go *)
let size_to_Go sz = size_to_same_unit (GB 0.0) sz

(** Convert a size to Mo *)
let size_to_Mo sz = size_to_same_unit (MB 0.0) sz

(** Convert a size to Ko *)
let size_to_Ko sz = size_to_same_unit (KB 0.0) sz

(** Convert a size to O*)
let size_to_O  sz = size_to_same_unit (B  0.0) sz

(** Apply an operation to a size : the two size are converted
     to the same unit and the function is applied to their value 
  *)
let size_apply_operation f sz1 sz2 = 
  let sz2p = size_to_same_unit sz1 sz2
  in
  match sz1,sz2p with
    TB f1, TB f2 -> TB (f f1 f2)
  | GB f1, GB f2 -> GB (f f1 f2)
  | MB f1, MB f2 -> MB (f f1 f2)
  | KB f1, KB f2 -> KB (f f1 f2)
  | B  f1, B  f2 -> B  (f f1 f2)
  |     _ ,    _ -> raise SizeInvalid

(** Compare two size, using the classical compare function. The two size
    are converted to the same unit before. If fuzzy is set to true, the 
    comparison is done on the floor value of the two size. 
  *)
let size_compare ?(fuzzy=false) sz1 sz2 = 
  let sz2p = size_to_same_unit sz1 sz2
  in
  match sz1,sz2p with
    TB f1, TB f2
  | GB f1, GB f2 
  | MB f1, MB f2 
  | KB f1, KB f2 
  | B  f1, B  f2 -> 
      if fuzzy then 
        Pervasives.compare (floor f1) (floor f2)
      else
        Pervasives.compare f1 f2
  |     _ ,    _ -> raise SizeInvalid

(** size_add sz1 sz2 : add sz1 to sz2, result is in the unit of sz1 *)
let size_add sz1 sz2 = size_apply_operation (+.) sz1 sz2

(** size_sub sz1 sz2 : substract sz1 to sz2, result is in the unit of sz1 *)
let size_sub sz1 sz2 = size_apply_operation (-.) sz1 sz2

(** Convert a value to a string representation. If fuzzy is set to true, only
* consider the most significant unit *)
let string_of_size ?(fuzzy=false) sz = 
  let buffer = Buffer.create 16
  in
  let append_unit unt vl =
    begin
      if Buffer.length buffer = 0 then
        ()
      else
        Buffer.add_char buffer ' '
    end;
    Printf.bprintf buffer "%d %s" (truncate vl) unt;
    vl -. (float_of_int (truncate vl))
  in
  let rec string_of_size_aux sz =
    if fuzzy && (Buffer.length buffer > 0) then
      Buffer.contents buffer
    else
      begin
        match sz with
          TB f when f > 1.0 -> 
            string_of_size_aux (TB (append_unit "TB" f))
        | GB f when f > 1.0 ->
            string_of_size_aux (GB (append_unit "GB" f))
        | MB f when f > 1.0 ->
            string_of_size_aux (MB (append_unit "MB" f))
        | KB f when f > 1.0 ->
            string_of_size_aux (KB (append_unit "KB" f))
        | B  f when f > 1.0 ->
            string_of_size_aux (B  (append_unit "B"  f))
        | B  f ->
            Buffer.contents buffer
        | sz ->
            string_of_size_aux (size_convert_down sz)
      end
  in
  string_of_size_aux sz
;;
        
(** {2 Operations on files and directories} *)

(**/**)
module SetFilename = Set.Make (struct
  type t = filename
  let compare = FilePath.compare
end)
;;

let doit force fln = 
  match force with
    Force -> true
  | Ask ask -> ask fln
;;

let prevent_recursion fln_set fln = 
  if SetFilename.mem fln fln_set then
    raise (RecursiveLink fln)
  else
    SetFilename.add fln fln_set
;;

let solve_dirname dirname =
  (* We have an ambiguity concerning "" and "." *)
  if is_current dirname then
    current_dir
  else
    dirname
  
(**/**)
  
(** stat fln : Returns information about the file (like Unix.stat) *)
let stat (filename: filename): stat =
  try
    let stats = Unix.stat filename
    in
    let kind = 
      match stats.Unix.st_kind with
	Unix.S_REG -> File 
      | Unix.S_DIR -> Dir 
      | Unix.S_CHR -> Dev_char 
      | Unix.S_BLK -> Dev_block
      | Unix.S_LNK -> Link
      | Unix.S_FIFO -> Fifo 
      | Unix.S_SOCK -> Socket
    in
    let is_link = 
      let stats = Unix.lstat filename 
      in
      stats.Unix.st_kind = Unix.S_LNK
    in
    {
      kind              = kind;
      is_link           = is_link;
      permission        = permission_of_int stats.Unix.st_perm;
      size              = B (float_of_int stats.Unix.st_size);
      owner             = stats.Unix.st_uid;
      group_owner       = stats.Unix.st_gid;
      access_time       = stats.Unix.st_atime;
      modification_time = stats.Unix.st_mtime;
      creation_time     = stats.Unix.st_ctime;
    }
  with Unix.Unix_error(_) ->
    raise FileDoesntExist 
;;

(**/**)
let rec compile_filter flt =
  let wrapper f fln =
    try 
      let stats = stat fln
      in
      f stats
    with FileDoesntExist ->
      false
  in
  let res_filter =
    match flt with
      Is_dev_block    -> wrapper (fun st -> st.kind = Dev_block)
    | Is_dev_char     -> wrapper (fun st -> st.kind = Dev_char)
    | Is_dir          -> wrapper (fun st -> st.kind = Dir)
    | Is_file         -> wrapper (fun st -> st.kind = File)
    | Is_socket       -> wrapper (fun st -> st.kind = Socket)
    | Is_pipe         -> wrapper (fun st -> st.kind = Fifo)
    | Is_link         -> wrapper (fun st -> st.is_link)
    | Exists          -> wrapper (fun st -> true) 
    | Is_set_group_ID -> wrapper (fun st -> st.permission.group.sticky)
    | Has_sticky_bit  -> wrapper (fun st -> st.permission.other.sticky)
    | Has_set_user_ID -> wrapper (fun st -> st.permission.user.sticky)
    | Is_readable     -> wrapper (
      fun st -> st.permission.user.read  || st.permission.group.read  || st.permission.other.read
     )
    | Is_writeable    -> wrapper (
      fun st -> st.permission.user.write || st.permission.group.write || st.permission.other.write
     )
    | Is_exec         -> wrapper (
      fun st -> st.permission.user.exec  || st.permission.group.exec  || st.permission.other.exec
     )
    | Size_not_null          -> wrapper (
      fun st -> (size_compare st.size (B 0.0)) > 0
     )
    | Size_bigger_than sz    -> wrapper (
      fun st -> (size_compare st.size sz) > 0
     )
    | Size_smaller_than sz   -> wrapper (
      fun st -> (size_compare st.size sz) < 0
     )
    | Size_equal_to sz       -> wrapper (
      fun st -> (size_compare st.size sz) = 0
     )
    | Size_fuzzy_equal_to sz -> wrapper (
      fun st -> (size_compare ~fuzzy:true st.size sz) = 0 
     )
    | True            -> fun x -> true
    | False           -> fun x -> false
    | Is_owned_by_user_ID  -> wrapper (
      fun st -> Unix.geteuid () = st.owner
     )
    | Is_owned_by_group_ID -> wrapper (
      fun st -> Unix.getegid () = st.group_owner
     )
    | Is_newer_than(f1)    -> 
	begin
	  try 
	    let st1 = stat f1
	    in
	    wrapper (fun st2 -> st1.modification_time > st2.modification_time)
	  with FileDoesntExist ->
	    fun x -> false
	end
    | Is_older_than(f1)    -> 
	begin
	  try 
	    let st1 = stat f1
	    in
	    wrapper (fun st2 -> st1.modification_time < st2.modification_time)
	  with FileDoesntExist ->
	    fun x -> false
	end
    | Is_newer_than_date(dt) -> wrapper (fun st -> st.modification_time > dt)
    | Is_older_than_date(dt) -> wrapper (fun st -> st.modification_time < dt)
    | And(flt1,flt2) ->
      let cflt1 = (compile_filter flt1)
      in
      let cflt2 = (compile_filter flt2)
      in
      fun x -> (cflt1 x) && (cflt2 x)
    | Or(flt1,flt2) ->
      let cflt1 = (compile_filter flt1)
      in
      let cflt2 = (compile_filter flt2)
      in
      fun x -> (cflt1 x) || (cflt2 x)
    | Not(flt1) ->
      let cflt1 = (compile_filter flt1)
      in
      fun x -> not (cflt1 x)
    | Match(r) ->
        (* TODO: regexp compile *)
        failwith "Not implemented"
    | Has_extension(ext) ->
      begin
	fun x -> 
	  try 
	    check_extension x ext
	  with FilePath.NoExtension ->
	    false
      end
    | Has_no_extension ->
      begin
	fun x -> 
	  try
	    let _ = chop_extension x 
	    in 
	    false
	  with FilePath.NoExtension ->
	    true
      end
    | Is_current_dir ->
      fun x -> (is_current (basename x))
    | Is_parent_dir ->
      fun x -> (is_parent  (basename x))
    | Basename_is s ->
      let rs = reduce s
      in
      fun x -> (reduce (basename x)) = rs
    | Dirname_is s ->
      let rs = reduce s
      in
      fun x -> (reduce (dirname x)) = rs
    | Custom f ->
	f
  in
  fun x -> res_filter x 
;;

let all_upper_dir fln = 
  let rec all_upper_dir_aux lst fln = 
    let dir = dirname fln
    in
    match lst with
      prev_dir :: tl when prev_dir = dir ->
      lst
    | _ ->
	all_upper_dir_aux (dir :: lst) dir
    in
      all_upper_dir_aux [fln] fln
;;

(**/**)

(** Test the existence of the file... *)
let test tst =
  let ctst = compile_filter tst
  in
  fun fln -> ctst (solve_dirname fln)
;;

(** Return the currend dir *) 
let pwd () = 
  reduce (Sys.getcwd ())
;;

(** Return the real filename of a filename which could have link *) 
let readlink fln =
  let ctst = compile_filter Is_link
  in
  let rec readlink_aux already_read fln = 
    let newly_read = prevent_recursion already_read fln
    in
    let dirs = all_upper_dir fln
    in
    try 
      let src_link = List.find ctst (List.rev dirs)
      in
      let dst_link = Unix.readlink src_link 
      in
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
;;

(** List the content of a directory *)
let ls dirname =
  let real_dirname = 
  	solve_dirname dirname
  in
  let array_dir = Sys.readdir real_dirname
  in
  let list_dir  = Array.to_list array_dir
  in
	List.map (fun x -> 
    concat dirname x
   )  list_dir
;;

(** Apply a filtering pattern to a filename *)
let filter flt lst =
  List.filter (test flt) lst
;;

(** Try to find the executable in the PATH. Use environement variable
PATH if none is provided *)
let which ?(path) fln =
  let real_path =
    match path with
      None ->
      path_of_string (Sys.getenv "PATH")
    | Some x ->
      x
  in
  let ctst x = 
    test (And(Is_exec,Not(Is_dir))) 
      (concat x fln)
  in
  let which_path =
    List.find ctst real_path
  in
  concat which_path fln
;;

(** Create the directory which name is provided. Turn parent to true
if you also want to create every topdir of the path. Use mode to 
provide some specific right (default 755). *)
let mkdir ?(parent=false) ?(mode=0o0755) fln =
  let mkdir_simple fln =
    if test Exists fln then
      if test Is_dir fln then
        ()
      else
        raise (MkdirDirnameAlreadyUsed fln)
    else
      try 
        Unix.mkdir fln mode 
      with Unix.Unix_error(Unix.ENOENT,_,_) | Unix.Unix_error(Unix.ENOTDIR,_,_) ->
        raise (MkdirMissingComponentPath fln)
  in
  let directories = 
    if parent then 
      all_upper_dir fln
    else
      [fln]
  in
  List.iter mkdir_simple directories
;;

(** Modify the time stamp of the given filename. Turn create to false
if you don't want to create the file *)
(* TODO: check that it doesn't set the filesize to 0 *)
let touch ?(create=true) fln =
  if (test (And(Exists,Is_file)) fln) || create then
    close_out (open_out_bin fln)
  else 
    ()
;;

(** find ~follow:fol tst fln exec accu : Descend the directory tree starting 
from the given filename and using the test provided to find what is looking 
for. You cannot match current_dir and parent_dir. For every file found, 
the action exec is done, using the accu to start. For a simple file
listing, you can use find True "." (fun x y -> x :: y) [] *)
let find ?(follow = Skip) tst fln exec accu =
  let ctest = compile_filter (And(tst,Not(Or(Is_parent_dir,Is_current_dir))))
  in
  let cdir  = compile_filter (And(Is_dir,Not(Or(Is_parent_dir,Is_current_dir))))
  in
  let clink = fun fln ->
    if test Is_link fln then
      match follow with
        Follow -> true
      | Skip   -> false
      | SkipInform f -> f fln; false
      | AskFollow f -> f fln
    else
      true    
  in
  let rec find_dir (already_read,accu) fln =
    let newly_read = prevent_recursion already_read (make_absolute (pwd ()) (readlink fln))
    in
    let dir_content = ls fln
    in
    let new_accu = List.fold_left exec accu (List.filter ctest dir_content)
    in
    let directories = List.filter clink (List.filter cdir dir_content)
    in
    if directories = [] then
      (newly_read,new_accu)
    else
      List.fold_left find_dir (newly_read,new_accu) directories
  in
  let find_simple (already_read,accu) fln =
    let new_accu = 
      if ctest fln then
        exec accu fln
      else
        accu
    in
    if test Is_dir fln then
      find_dir (already_read,new_accu) fln
    else
      (already_read,new_accu)
  in
  snd(find_simple (SetFilename.empty,accu) fln)
;;

(** Remove the filename provided. Turn recurse to true in order to 
completely delete a directory *)
let rm ?(force=Force) ?(recurse=false) fln_lst =
  let cfile = (And(Custom (doit force),Or(Not(Is_dir),Is_link)))
  in
  let cdir  = (And(Custom (doit force),Is_dir))
  in
  let rmdir () fln =
    try 
        Unix.rmdir fln
    with Unix.Unix_error(Unix.ENOTEMPTY,_,_) ->
      raise (RmDirNotEmpty fln)
  in
  let rmfile () fln =
      Unix.unlink fln
  in
  let rmfull fln = 
    find cfile fln rmfile ();
    let set_dir = 
      find cdir fln 
      (fun set fln -> SetFilename.add fln set) SetFilename.empty
    in
    List.iter (rmdir ()) (SetFilename.elements set_dir)
  in
  if recurse then
    List.iter rmfull fln_lst 
  else
    begin
    List.iter (rmfile ()) (filter cfile fln_lst);
    List.iter (rmdir  ()) (filter cdir fln_lst)
    end
;;

(** Copy the hierarchy of files/directory to another destination *)
let cp ?(follow=Skip) ?(force=Force) ?(recurse=false) fln_src_lst fln_dst = 
  let cpfile fln_src fln_dst =
    let cpfile () = 
      let buffer_len = 1024
      in
      let buffer = String.make buffer_len ' '
      in
      let read_len = ref 0
      in
      let ch_in = open_in_bin fln_src
      in
      let ch_out = open_out_bin fln_dst
      in
      while (read_len := input ch_in buffer 0 buffer_len; !read_len <> 0) do
        output ch_out buffer 0 !read_len
      done;
      close_in ch_in;
      close_out ch_out
    in
    let st = stat fln_src
    in
    match st.kind with
      File -> 
       cpfile ()
    | Dir ->
      mkdir fln_dst
    (* We do not accept to copy this kind of files *)
    (* It is too POSIX specific, should not be     *)
    (* implemented on other platform               *)
    | Link 
    | Fifo 
    | Dev_char 
    | Dev_block
    | Socket ->
      raise (CpCannotCopy fln_src)
  in
  let cpfull dir_src dir_dst fln = 
    find (And(Custom(doit force), Is_dir)) fln (
      fun () fln_src -> cpfile fln_src (reparent dir_src dir_dst fln)
     ) ();
    find (And(Custom(doit force), Not(Is_dir))) fln (
      fun () fln_src -> cpfile fln_src (reparent dir_src dir_dst fln)
     ) ()
  in
  (* Test sur l'existence des fichiers source et création des noms de fichiers
  * absolu *)
  let real_fln_src_lst = 
    List.map (
      fun x -> 
        if test (Not(Exists)) x then 
          raise (CpNoSourceFile x) 
        else if test Is_dir x && not recurse then
          raise (CpCannotCopyDir x)
        else
          make_absolute (pwd ()) x
     ) 
    fln_src_lst
  in
  let real_fln_dst =
    make_absolute (pwd ()) fln_dst
  in
  if test Is_dir real_fln_dst then
    List.iter (fun x -> cpfull (dirname x) real_fln_dst x) real_fln_src_lst
  else 
    (
      if (List.length real_fln_src_lst) = 1 then
        let real_fln_src = List.nth real_fln_src_lst 0
        in
        cpfull real_fln_src real_fln_dst real_fln_src 
        (* Off course, reparent will replace the common prefix 
        * of 3rd arg and 1st arg by 2nd arg, which give 
        * fln_src -> fln_dst *)
      else
        raise (CpCannotCopyFilesToFile (real_fln_src_lst, real_fln_dst))
   )
;;

(** Move files/directory to another destination *)
let rec mv ?(force=Force) fln_src fln_dst =
  let fln_src_abs =  make_absolute (pwd ()) fln_src
  in
  let fln_dst_abs =  make_absolute (pwd ()) fln_dst
  in
  if compare fln_src_abs fln_dst_abs <> 0 then
    if test Exists fln_dst_abs then
      if doit force fln_dst then
      (
        rm [fln_dst_abs];
        mv fln_src_abs fln_dst_abs
     )
      else
        ()
    else if test Is_dir fln_dst_abs then
      mv ~force:force 
        fln_src_abs
        (make_absolute fln_dst_abs (basename fln_src_abs))
    else if test Exists fln_src_abs then
      Sys.rename fln_src_abs fln_src_abs
    else
      raise MvNoSourceFile
  else
    ()
;;

(** cmp skip1 fln1 skip2 fln2 : Compare files fln1 fln2 starting at pos skip1 
* skip2 and returning the first octect where a difference occurs. Returns 
* (Some -1) if one of the file is not readable or if it is not a file. *) 
let cmp ?(skip1 = 0) fln1 ?(skip2 = 0) fln2 =
  if (reduce fln1) = (reduce fln2) then
    None
  else if (test (And(Is_readable,Is_file)) fln1) && (test (And(Is_readable,Is_file)) fln2) then
    let fd1 = open_in_bin fln1
    in
    let fd2 = open_in_bin fln2
    in
    let _ = seek_in fd1 skip1
    in
    let _ = seek_in fd2 skip2
    in
    let stream1 = Stream.of_channel fd1
    in
    let stream2 = Stream.of_channel fd2
    in
    try 
      while ((Stream.next stream1) = (Stream.next stream2)) 
      do () done;
      Some (Stream.count stream1)
    with Stream.Failure ->
      let test_empty st = 
        try 
          Stream.empty st;
          true
        with Stream.Failure ->
          false
    in
    match ((test_empty stream1),(test_empty stream2)) with
      true, true   -> None
    | true, false 
    | false, true 
    (* Don't know how this case could be... *)
    | false, false -> Some (Stream.count stream1)
  else
    (Some (-1))
;;

(** du fln_lst : Returns the amount of space of all the file 
* which are subdir of fln_lst. Also returns details for each 
* file scanned *)
let du fln_lst = 
  let du_aux (sz, lst) fln = 
    let st = stat fln
    in
    (size_add sz st.size, (fln, st.size) :: lst)
  in
  List.fold_left 
  (fun (accu : size * (filename * size) list) fln -> find True fln du_aux accu) 
  (B 0.0, [])
  fln_lst  
;;

(** For future release : 
   - pathchk : filename -> boolean * string 
  *)

(** {2 Deprecated } *)

(* TODO: replace this 
(** Implementation using regexp *)
module StrUtil : FILE_UTILS = GenericUtil(struct 
  type t = Str.regexp
  let  compile = Str.regexp
  let  test    = fun r x -> Str.string_match r x 0
end)
;;
*)
