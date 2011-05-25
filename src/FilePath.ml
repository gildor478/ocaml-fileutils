(********************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                      *)
(*                                                                              *)
(*  Copyright (C) 2003-2011, Sylvain Le Gall                                    *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

open FilePath_type;;

exception BaseFilenameRelative of filename;;
exception UnrecognizedOS of string;;
exception EmptyFilename;;
exception NoExtension of filename;;
exception InvalidFilename of filename;;

module type OS_SPECIFICATION =
sig
  val dir_writer: (filename_part list) -> filename 
  val dir_reader: filename -> (filename_part list)
  val path_writer: (filename list) -> string
  val path_reader: string -> (filename list)
  val fast_concat: filename -> filename -> filename 
  val fast_basename: filename -> filename
  val fast_dirname: filename -> filename 
  val fast_is_relative: filename -> bool
  val fast_is_current: filename -> bool
  val fast_is_parent: filename -> bool
end
;;

module type PATH_SPECIFICATION =
sig
  type filename  
  type extension 

  val string_of_filename : filename -> string
  val filename_of_string : string -> filename
  val extension_of_string: string -> extension
  val string_of_extension: extension -> string
  val make_filename: string list -> filename 
  val is_subdir: filename -> filename -> bool
  val is_updir: filename -> filename -> bool
  val compare: filename -> filename -> int
  val basename: filename -> filename
  val dirname: filename -> filename
  val concat: filename -> filename -> filename
  val reduce: ?no_symlink:bool -> filename -> filename
  val make_absolute: filename -> filename -> filename
  val make_relative: filename -> filename -> filename
  val reparent: filename -> filename -> filename -> filename 
  val identity: filename -> filename
  val is_valid: filename -> bool
  val is_relative: filename -> bool
  val is_current: filename -> bool
  val is_parent: filename -> bool
  val chop_extension: filename -> filename
  val get_extension: filename -> extension 
  val check_extension: filename -> extension -> bool
  val add_extension: filename -> extension -> filename
  val replace_extension: filename -> extension -> filename
  val string_of_path: filename list -> string
  val path_of_string: string -> filename list
  val current_dir: filename
  val parent_dir: filename
end
;;

module type PATH_STRING_SPECIFICATION =
sig
  module Abstract: PATH_SPECIFICATION 

  include PATH_SPECIFICATION with 
    type filename = string and 
    type extension = string
end
;;

(* Convert an OS_SPECIFICATION to PATH_SPECIFICATION *)
module GenericPath  = 
functor ( OsOperation : OS_SPECIFICATION ) ->
struct
  type filename = FilePath_type.filename_part list

  type extension = FilePath_type.extension

  (* Filename_from_string *)

  let filename_of_string str = 
    try 
      OsOperation.dir_reader str
    with Parsing.Parse_error ->
      raise (InvalidFilename str)

  (* String_from_filename *)

  let string_of_filename path = 
    OsOperation.dir_writer path

  (* Reduce *)

  let reduce ?(no_symlink=false) path =
    (* TODO: not tail recursive ! *)
    let rec reduce_aux lst = 
      match lst with 
        | ParentDir :: tl when no_symlink ->
            begin
              match reduce_aux tl with
                | Root s :: tl ->
                    Root s :: tl 
                | ParentDir :: tl ->
                    ParentDir :: ParentDir :: tl
                | [] ->
                    ParentDir :: tl 
                | _ :: tl ->
                    tl                  
            end
        | ParentDir :: tl ->
            ParentDir :: (reduce_aux tl)
        | CurrentDir _ :: tl 
        | Component "" :: tl ->
            (reduce_aux tl)
        | Component s :: tl ->
            Component s :: (reduce_aux tl)
        | Root s :: tl ->
            Root s :: (reduce_aux tl)
        | [] ->
            []
    in
    let rev_path = List.rev path in
    match reduce_aux rev_path with
      | [] when no_symlink = false->
	(* assert ( List.for_all ( function | Component "" | CurrentDir _ -> true | _ -> false ) rev_path ) *)
	(try
	   (* use last CurrentDir _ *)
	   [ List.find ( function | CurrentDir _ -> true | _ -> false ) rev_path ]
	 with
	   | Not_found -> [] ) (* Only Component "" *) 
      |l -> List.rev l



  (* Compare, subdir, updir *)

  type filename_relation = SubDir | UpDir | Equal | NoRelation of int

  let relation_of_filename path1 path2 =
    let rec relation_of_filename_aux path1 path2 =
      match (path1,path2) with
        ([], []) ->
        Equal
      | (hd1 :: tl1, hd2 :: tl2) ->
        if hd1 = hd2 then
          relation_of_filename_aux tl1 tl2
        else
        begin
          NoRelation (String.compare 
              (string_of_filename [hd1]) 
              (string_of_filename [hd2])
            )
        end
      | (subdir, []) ->
        SubDir
      | ([], updir) ->
        UpDir
    in
    relation_of_filename_aux path1 path2
    
  let is_subdir path1 path2 =
    match relation_of_filename path1 path2 with
      SubDir ->
      true
    | _ ->
      false

  let is_updir path1 path2 =
    match relation_of_filename path1 path2 with
      UpDir ->
      true
    | _ ->
      false


  let compare path1 path2 =
    match relation_of_filename path1 path2 with
      SubDir ->
      -1
    | UpDir ->
      1
    | Equal ->
      0
    | NoRelation i ->
      i 

  (* Concat *)

  let concat lst_path1 lst_path2 =
    reduce 
      (match lst_path2 with
         | CurrentDir Short :: tl_path2 ->
             lst_path1 @ tl_path2
         | _ ->
             lst_path1 @ lst_path2)


  (* Is_relative *)

  let is_relative lst_path =
    match lst_path with
     (Root _) :: _ -> false
    | _            -> true

  
  (* Is_valid *)
  
  let is_valid path = 
    (* As we are manipulating abstract filename, 
       and that it has been parsed, we are
       sure that all is correct *)
    true

  let is_current path = 
    match path with
      [ (CurrentDir _) ] -> true
    | _ -> false

  let is_parent path =
    match path with
      [ ParentDir ] -> true
    | _ -> false

  (* Basename *)

  let basename path = 
    match List.rev path with  
      hd :: tl ->
      [hd]
    | [] ->
      raise EmptyFilename

  (* Dirname *)

  let dirname path = 
    match List.rev path with
      hd :: tl ->
      List.rev tl
    | [] ->
      raise EmptyFilename

  (* Extension manipulation *)

  let wrap_extension f path =
    match basename path with 
      | [Component fn] ->
          f fn
      | _ ->
          raise (NoExtension (string_of_filename path))

  let check_extension path ext = 
    wrap_extension
      (fun fn -> ExtensionPath.check fn ext)
      path

  let get_extension path = 
    wrap_extension
      (fun fn -> ExtensionPath.get fn)
      path

  let chop_extension path =
    wrap_extension 
      (fun fn -> 
         concat
           (dirname path)
           [Component (ExtensionPath.chop fn)])
      path

  let add_extension path ext =
    wrap_extension 
      (fun fn -> 
         concat
           (dirname path)
           [Component (ExtensionPath.add fn ext)])
      path

  let replace_extension path ext =
    wrap_extension 
      (fun fn -> 
         concat 
           (dirname path)
           [Component (ExtensionPath.replace fn ext)])
      path

  let extension_of_string x = x

  let string_of_extension x = x 
    
  (* Make_asbolute *)

  let make_absolute path_base path_path =    
    reduce 
      (if is_relative path_base then
         raise (BaseFilenameRelative (string_of_filename path_base))
       else if is_relative path_path then
         path_base @ path_path
       else
         path_path)

  (* Make_relative *)

  let make_relative path_base path_path =
    let rec make_relative_aux lst_base lst_path =
      match  (lst_base, lst_path) with
      x :: tl_base, a :: tl_path when x = a ->
        make_relative_aux tl_base tl_path
      | _, _ ->
        let back_to_base = List.rev_map 
          (fun x -> ParentDir)
          lst_base
        in
          back_to_base @ lst_path
    in
      reduce 
        (if is_relative path_base then
           raise (BaseFilenameRelative (string_of_filename path_base))
         else if is_relative path_path then
           path_path
         else
           make_relative_aux path_base path_path)

  (* Make_filename *)

  let make_filename lst_path =
    reduce (List.flatten (List.map filename_of_string lst_path))
    
  (* Reparent *)

  let reparent path_src path_dst path =
    let path_relative =
      make_relative path_src path
    in
      make_absolute path_dst path_relative

  (* Identity *)
  
  let identity path = path
  
  (* Manipulate path like variable *)

  let string_of_path lst = 
    OsOperation.path_writer (List.map string_of_filename lst)

  let path_of_string str = 
    List.map 
      filename_of_string 
      (OsOperation.path_reader str)

  (* Generic filename component *)

  let current_dir = [ CurrentDir Long ]

  let parent_dir = [ ParentDir ]
end 
;;

(* Convert an OS_SPECIFICATION to PATH_STRING_SPECIFICATION *)
module GenericStringPath =
functor (OsOperation : OS_SPECIFICATION) ->
struct

  module Abstract = GenericPath(OsOperation)

  type filename  = string
  type extension = string

  let string_of_filename path = 
    path

  let filename_of_string path = 
    path

  let string_of_extension ext = 
    ext 

  let extension_of_string str =
    str

  let f2s = Abstract.string_of_filename

  let s2f = Abstract.filename_of_string

  let e2s = Abstract.string_of_extension

  let s2e = Abstract.extension_of_string

  let is_subdir path1 path2 = 
    Abstract.is_subdir (s2f path1) (s2f path2)

  let is_updir path1 path2 =
    Abstract.is_updir  (s2f path1) (s2f path2)

  let compare path1 path2 =
    Abstract.compare   (s2f path1) (s2f path2)

  let basename path =
    try 
      OsOperation.fast_basename path
    with CommonPath.CannotHandleFast ->
      f2s (Abstract.basename (s2f path))

  let dirname path = 
    try
      OsOperation.fast_dirname path
    with CommonPath.CannotHandleFast ->
      f2s (Abstract.dirname  (s2f path))

  let concat path1 path2 = 
    try
      OsOperation.fast_concat path1 path2
    with CommonPath.CannotHandleFast ->
      f2s (Abstract.concat (s2f path1) (s2f path2))
    
  let make_filename path_lst =
    f2s (Abstract.make_filename path_lst)

  let reduce ?no_symlink path =
    f2s (Abstract.reduce ?no_symlink (s2f path))

  let make_absolute base_path path =
    f2s (Abstract.make_absolute (s2f base_path) (s2f path))

  let make_relative base_path path =
    f2s (Abstract.make_relative (s2f base_path) (s2f path))

  let reparent path_src path_dst path =
    f2s (Abstract.reparent (s2f path_src)  (s2f path_dst) (s2f path))

  let identity path =
    f2s (Abstract.identity (s2f path))

  let is_valid path =
    try
      Abstract.is_valid (s2f path)
    with InvalidFilename _ ->
      false

  let is_relative path = 
    try 
      OsOperation.fast_is_relative path
    with CommonPath.CannotHandleFast ->
      Abstract.is_relative (s2f path)

  let is_current path =
    try
      OsOperation.fast_is_current path
    with CommonPath.CannotHandleFast ->
      Abstract.is_current (s2f path)

  let is_parent path =
    try
      OsOperation.fast_is_parent path
    with CommonPath.CannotHandleFast ->
      Abstract.is_parent (s2f path)

  let wrap_extension f path =
    let bfn =
      OsOperation.fast_basename path
    in
      if OsOperation.fast_is_parent bfn ||
         OsOperation.fast_is_current bfn ||
         not (OsOperation.fast_is_relative bfn) then
        raise (NoExtension path)
      else
        (f bfn)

  let chop_extension path =
    try
      wrap_extension 
        (fun fn -> 
           OsOperation.fast_concat 
             (OsOperation.fast_dirname path)
             (ExtensionPath.chop fn))
        path
    with CommonPath.CannotHandleFast ->
      f2s (Abstract.chop_extension (s2f path))

  let get_extension path =
    try
      wrap_extension 
        (fun fn -> ExtensionPath.get fn)
        path
    with CommonPath.CannotHandleFast ->
      e2s (Abstract.get_extension (s2f path))

  let check_extension path ext =
    try
      wrap_extension
        (fun fn -> ExtensionPath.check fn ext)
        path
    with CommonPath.CannotHandleFast ->
      Abstract.check_extension (s2f path) (s2e ext)

  let add_extension path ext =
    try
      wrap_extension
        (fun fn -> 
           OsOperation.fast_concat 
             (OsOperation.fast_dirname path)
             (ExtensionPath.add fn ext))
        path
    with CommonPath.CannotHandleFast ->
      f2s (Abstract.add_extension (s2f path) (s2e ext))

  let replace_extension path ext =
    try
      wrap_extension
        (fun fn -> 
           OsOperation.fast_concat 
             (OsOperation.fast_dirname path)
             (ExtensionPath.replace fn ext))
        path
    with CommonPath.CannotHandleFast ->
      f2s (Abstract.replace_extension (s2f path) (s2e ext))

  let string_of_path path_lst =
    Abstract.string_of_path (List.map s2f path_lst)

  let path_of_string str =
    List.map f2s (Abstract.path_of_string str)

  let current_dir =
    f2s (Abstract.current_dir)

  let parent_dir =
    f2s (Abstract.parent_dir)
end
;;

module DefaultPath = GenericStringPath(struct

  let os_depend unix macos win32 =
    match Sys.os_type with
      "Unix"
    | "Cygwin" -> unix
    | "MacOS"  -> macos
    | "Win32"  -> win32
    | s        -> raise (UnrecognizedOS s)
    
  let dir_writer  = 
    os_depend 
      UnixPath.dir_writer
      MacOSPath.dir_writer  
      Win32Path.dir_writer  

  let dir_reader  = 
    os_depend 
      UnixPath.dir_reader
      MacOSPath.dir_reader  
      Win32Path.dir_reader  

  let path_writer = 
    os_depend 
      UnixPath.path_writer
      MacOSPath.path_writer 
      Win32Path.path_writer 

  let path_reader = 
    os_depend 
      UnixPath.path_reader
      MacOSPath.path_reader 
      Win32Path.path_reader 

  let fast_concat = 
    os_depend 
      UnixPath.fast_concat
      MacOSPath.fast_concat 
      Win32Path.fast_concat 

  let fast_basename = 
    os_depend 
      UnixPath.fast_basename
      MacOSPath.fast_basename 
      Win32Path.fast_basename 

  let fast_dirname = 
    os_depend 
      UnixPath.fast_dirname
      MacOSPath.fast_dirname 
      Win32Path.fast_dirname 

  let fast_is_relative =
    os_depend 
      UnixPath.fast_is_relative
      MacOSPath.fast_is_relative 
      Win32Path.fast_is_relative 

  let fast_is_current = 
    os_depend 
      UnixPath.fast_is_current
      MacOSPath.fast_is_current 
      Win32Path.fast_is_current 

  let fast_is_parent = 
    os_depend 
      UnixPath.fast_is_parent
      MacOSPath.fast_is_parent 
      Win32Path.fast_is_parent 

end)
;;

module UnixPath =  GenericStringPath(UnixPath);;

module MacOSPath = GenericStringPath(MacOSPath);;

module Win32Path = GenericStringPath(Win32Path);;

module CygwinPath = UnixPath;;

include DefaultPath;;
