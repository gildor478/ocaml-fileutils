open SysPath_type;;

(** Module to manipulate abstract filename ( doesn't need to be real path ).*)

(** {1 Exceptions } *)

(** You cannot pass a base filename which is relative *)
exception SysPathBaseFilenameRelative;;

(** One of the filename passed is relative and cannot be reduce *)
exception SysPathRelativeUnreducable;;

(** We do not have recognized any OS, please contact upstream *)
exception SysPathUnrecognizedOS of string;;

(** An expansion of filename_part_of_filename generate more than one component *)
exception SysPathFilenameMultiple;;

(** The filename use was empty *)
exception SysPathEmpty;;

(** The last component of the filename does not support extension ( Root, ParentDir... ) *)
exception SysPathNoExtension;;

(** The filename used is invalid *)
exception SysPathInvalidFilename;;

(** {1 Filename utility specification} *)

(** Signature needed to create a new        
    filesystem... See the source code for   
    more information                        
  *)
module type OS_SPECIFICATION =
sig
	val dir_writer                : (filename_part list) -> string
	val dir_reader                : Lexing.lexbuf -> (filename_part list)
	val path_writer               : (filename list) -> string
	val path_reader               : Lexing.lexbuf -> (filename list)
end
;;

(** Generic operation on filename *)
module type PATH_SPECIFICATION =
sig

type filename  
type extension

(** {2 Converting from/to string } *)

(** Create a filename from a string *)
val string_of_filename : filename -> string

(** Create a string from a filename *)
val filename_of_string : string -> filename

(** Make a filename from set of strings *)
val make_filename      : string list -> filename 

(** {2 Ordering filename } *)

(** is_subdir fl1 fl2 : Is fl2 a sub directory of fl1 *)
val is_subdir            : filename -> filename -> bool

(** is_updir fl1 fl2 : Is fl1 a sub directory of fl2 *)
val is_updir             : filename -> filename -> bool

(** Give an order between the two filename. The classification is done by sub directory relation 
    ( fl1 < subdir(fl1) ) and lexicographical order of each part of the reduce filename when fl1 and fl2
    has no hierarchical relation 
  *)
val compare              : filename -> filename -> int

(** {2 Manipulating/Splitting path } *) 

(** Extract only the file name of a filename *)
val basename        : filename -> filename

(** Extract the directory name of a filename *)
val dirname         : filename -> filename

(** Append a filename to the filename *)
val concat          : filename -> filename -> filename

(** Return the shortest filename which is equal to the filename given *)
val reduce          : filename -> filename

(** Create an absolute filename from a filename relative and an absolute base filename *)
val make_absolute   : filename -> filename -> filename

(** Create a filename which is relative to the base filename *)
val make_relative   : filename -> filename -> filename

(** Reparent a filename : reparent fln_src fln_dst fln, return the same filename as fln 
    but the root is no more fln_src but fln_dst *)
val reparent        : filename -> filename -> filename -> filename 

(** Identity : for testing the stability of implode/explode *)
val identity        : filename -> filename

(** Test if the filename is a valid one *)
val is_valid        : filename -> bool

(** Check if the filename is relative to a dir or not ( ie beginning with Component, ParentDir, CurrentDir ) *)
val is_relative     : filename -> bool

(** Check if the filename is an absolute one or not ( ie beginning by a Root reference )*)
val is_implicit     : filename -> bool

(** Check if the filename is a reference to current directory ( ie beginning with CurrentDir ) *)
val is_current      : filename -> bool

(** Check if the filename is a reference to the parent directory ( ie beginning with ParentDir ) *)
val is_parent       : filename -> bool


(** {2 Manipulate the extension }*)

(** Remove extension *)
val chop_extension      : filename -> filename

(** Extract the extension *)
val get_extension       : filename -> extension 

(** Check the extension *)
val check_extension     : filename -> extension -> bool

(** Add an extension *)
val add_extension       : filename -> extension -> filename

(** Create an extension from a string *)
val extension_of_string : string -> extension

(** Create a string from an extension *)
val string_of_extension : extension -> string


(** {2 Transformation of PATH like variable }*)

(** Create an environnement PATH like string from different filename *)
val string_of_path : filename list -> string

(** Return the different filename of an environnement PATH like string *)
val path_of_string : string -> filename list
end
;;

(** Generic operation on filename, the type filename being a string *)
module type PATH_STRING_SPECIFICATION =
(PATH_SPECIFICATION with type filename = string)
;;

(** Meta structure to generate a module for 
    a new filesystem specification. See the 
    source code to know how to do it        *)
module type META_PATH_SPECIFICATION =
functor ( OsOperation : OS_SPECIFICATION ) -> 
PATH_SPECIFICATION
;;

(** Meta structure to define a string-proxy doing
    the conversion between an abstract type to a
    string type *)
module type META_PATH_STRING_SPECIFICATION =
functor ( PathOperation : PATH_SPECIFICATION ) ->
PATH_STRING_SPECIFICATION
;;

(** Implementation of the META_PATH_SPECIFICATION *)
module GenericPath : META_PATH_SPECIFICATION = 
functor ( OsOperation : OS_SPECIFICATION ) ->
struct
	type filename = SysPath_type.filename_part list

	type extension = SysPath_type.extension

	(* Debug function *)

	let debug_print_component lst=
		let debug_print_one_component elem =
			match elem with
			  Root s      -> "Root : "^s
			| Component s -> "Component : "^s
			| ParentDir   -> "ParenDir"
			| CurrentDir  -> "CurrentDir"
		in
		List.iter print_string (List.map (fun x -> (debug_print_one_component x)^" ;") lst);
		print_newline ()

	(* Filename_from_string *)

	let filename_of_string str = 
		try 
			let lexbuf = Lexing.from_string str
			in
			OsOperation.dir_reader lexbuf
		with Parsing.Parse_error ->
			raise SysPathInvalidFilename

	(* String_from_filename *)

	let string_of_filename path = 
		OsOperation.dir_writer path

	(* Reduce *)

	let reduce path =
		let rec reduce_aux lst = 
			match lst with 
			  ParentDir :: tl ->
			  	begin
				match reduce_aux tl with
			  	  Root s :: tl ->
				  	Root s :: tl 
				| ParentDir :: tl ->
					ParentDir :: ParentDir :: tl
				| [] ->
					ParentDir :: tl 
				| _ :: tl ->
					tl
				end
			| CurrentDir :: tl 
			| Component "" :: tl ->
				(reduce_aux tl)
			| Component s :: tl ->
				Component s :: (reduce_aux tl)
			| Root s :: tl ->
				Root s :: (reduce_aux tl)
			| [] ->
				[]
		in
		List.rev (reduce_aux (List.rev path))


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
		relation_of_filename_aux (reduce path1) (reduce path2)
		
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
		lst_path1 @ lst_path2	


	(* Is_relative *)

	let is_relative lst_path =
		match lst_path with
		 (Root _) :: _ -> false
		| _            -> true

	
	(* Is_implicit *)
	
	let is_implicit lst_path  = 
		match lst_path with
		  ParentDir :: _ 
		| CurrentDir :: _ 
		| Component _ :: _ -> true
		| _                -> false

	(* Is_valid *)
	
	let is_valid path = 
		(* As we are manipulating abstract filename, 
		   and that it has been parsed, we are
		   sure that all is correct *)
		true

	let is_current path = 
		match path with
		  [ CurrentDir ] -> true
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
			raise SysPathEmpty

	(* Dirname *)

	let dirname path = 
		match List.rev path with
		  hd :: tl ->
			List.rev tl
		| [] ->
			raise SysPathEmpty

	(* Extension manipulation *)

	let split_extension path = 
		match basename path with
		  (Component str) :: []->
			let lexbuf = Lexing.from_string str
			in
			let (base,ext) = GenericPath_parser.main_extension
				GenericPath_lexer.token_extension
				lexbuf
			in
			((dirname path) @ [Component base], ext)
		| _ ->
			raise SysPathNoExtension

	let check_extension path ext = 
		let (real_path, real_ext) = split_extension path
		in
		ext = real_ext 

	let get_extension path = 
		let (real_path, real_ext) = split_extension path
		in
		real_ext

	let chop_extension  path =
		let (real_path, real_ext) = split_extension path
		in
		real_path

	let add_extension path ext =
		match List.rev path with
		  Component str :: tl ->
		  	List.rev ( Component (str^"."^ext) :: tl )
		| _ ->
			raise SysPathNoExtension


	let extension_of_string x = x

	let string_of_extension x = x 
		
	(* Make_asbolute *)

	let make_absolute path_base path_path =
		if is_relative path_base then
			raise SysPathBaseFilenameRelative
		else if is_relative path_path then
			reduce (path_base @ path_path)
		else
			reduce (path_path)

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
		if is_relative path_base then
			raise SysPathBaseFilenameRelative
		else if is_relative path_path then
			reduce path_path
		else
			make_relative_aux (reduce path_base) (reduce path_path)


	(* Make_filename *)

	let make_filename lst_path =
		List.flatten (List.map filename_of_string lst_path)
		
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
		try
			let lexbuf = Lexing.from_string str
			in
			List.map filename_of_string (OsOperation.path_reader lexbuf)
		with Parsing.Parse_error ->
			raise SysPathInvalidFilename
end 
;;

(** Implementation of the META_PATH_STRING_SPECIFICATION *)
module GenericStringPath : META_PATH_STRING_SPECIFICATION =
functor ( PathOperation : PATH_SPECIFICATION ) ->
struct

	type filename  = string
	type extension = PathOperation.extension

	let string_of_filename path = 
		path

	let filename_of_string path = 
		path

	let f2s = PathOperation.string_of_filename

	let s2f = PathOperation.filename_of_string

	let is_subdir path1 path2 = 
		PathOperation.is_subdir (s2f path1) (s2f path2)

	let is_updir path1 path2 =
		PathOperation.is_updir  (s2f path1) (s2f path2)

	let compare path1 path2 =
		PathOperation.compare   (s2f path1) (s2f path2)

	let basename path =
		f2s (PathOperation.basename (s2f path))

	let dirname path = 
		f2s (PathOperation.dirname  (s2f path))

	let concat path1 path2 = 
		f2s (PathOperation.concat (s2f path1) (s2f path2))
		
	let make_filename path_lst =
		f2s (PathOperation.make_filename path_lst)

	let reduce path =
		f2s (PathOperation.reduce (s2f path))

	let make_absolute base_path path =
		f2s (PathOperation.make_absolute (s2f base_path) (s2f path))

	let make_relative base_path path =
		f2s (PathOperation.make_relative (s2f base_path) (s2f path))

	let reparent path_src path_dst path =
		f2s (PathOperation.reparent (s2f path_src)  (s2f path_dst) (s2f path))

	let identity path =
		f2s (PathOperation.identity (s2f path))

	let is_valid path =
		try
			PathOperation.is_valid (s2f path)
		with SysPathInvalidFilename ->
			false

	let is_relative path  = 
		PathOperation.is_relative ( s2f path )

	let is_implicit path =
		PathOperation.is_implicit ( s2f path )

	let is_current path =
		PathOperation.is_current ( s2f path )

	let is_parent path =
		PathOperation.is_parent ( s2f path )

	let chop_extension path =
		f2s (PathOperation.chop_extension (s2f path))

	let get_extension path =
		PathOperation.get_extension (s2f path)

	let check_extension path ext =
		PathOperation.check_extension (s2f path) ext

	let add_extension path ext =
		f2s (PathOperation.add_extension (s2f path) ext)

	let string_of_extension ext = 
		PathOperation.string_of_extension ext 

	let extension_of_string str =
		PathOperation.extension_of_string str

	let string_of_path path_lst =
		PathOperation.string_of_path (List.map s2f path_lst)

	let path_of_string str =
		List.map f2s (PathOperation.path_of_string str)
end
;;

(** {1 Instanciation of specific OS filename }*)

(** Some instanciation of the above structure. You can use it to manipulate filename        
    However, this function is mainly for testing purpose. You should consider using 
    DefaultPath  which is the binding for the current running OS. If you need to 
    manipulate frequently filename, for storing them in a Set, doing frequent operation... 
    You should consider using an abstract type. Otherwise, you should consider using
    the normal one, which enables you to simply manipulate string
    ( type filename = string, so all operation can be made using string 
  *)

module AbstractDefaultPath : PATH_SPECIFICATION = GenericPath(struct

	let os_depend unix macos win32 cygwin =
		match Sys.os_type with
		  "Unix"   -> unix
		| "MacOS"  -> macos
		| "Win32"  -> win32
		| "Cygwin" -> cygwin
		| s        -> raise (SysPathUnrecognizedOS s)
		
	let dir_writer  = os_depend UnixPath.dir_writer  MacOSPath.dir_writer  Win32Path.dir_writer  CygwinPath.dir_writer
	let dir_reader  = os_depend UnixPath.dir_reader  MacOSPath.dir_reader  Win32Path.dir_reader  CygwinPath.dir_reader
	let path_writer = os_depend UnixPath.path_writer MacOSPath.path_writer Win32Path.path_writer CygwinPath.path_writer
	let path_reader = os_depend UnixPath.path_reader MacOSPath.path_reader Win32Path.path_reader CygwinPath.path_reader
end)
;;

module DefaultPath : PATH_STRING_SPECIFICATION = GenericStringPath(AbstractDefaultPath)
;;

module AbstractUnixPath : PATH_SPECIFICATION =  GenericPath(struct
	let dir_writer                = UnixPath.dir_writer
	let dir_reader                = UnixPath.dir_reader
	let path_writer               = UnixPath.path_writer
	let path_reader               = UnixPath.path_reader
end)
;;
		
module UnixPath : PATH_STRING_SPECIFICATION = GenericStringPath(AbstractUnixPath)
;;

module AbstractMacOSPath : PATH_SPECIFICATION = GenericPath(struct
	let dir_writer                = MacOSPath.dir_writer
	let dir_reader                = MacOSPath.dir_reader
	let path_writer               = MacOSPath.path_writer
	let path_reader               = MacOSPath.path_reader
end)
;;

module MacOSPath : PATH_STRING_SPECIFICATION = GenericStringPath(AbstractMacOSPath)
;;

module AbstractWin32Path : PATH_SPECIFICATION = GenericPath(struct 
	let dir_writer                = Win32Path.dir_writer
	let dir_reader                = Win32Path.dir_reader
	let path_writer               = Win32Path.path_writer
	let path_reader               = Win32Path.path_reader
end)
;;

module Win32Path : PATH_STRING_SPECIFICATION = GenericStringPath(AbstractWin32Path)
;;

module AbstractCygwinPath : PATH_SPECIFICATION = GenericPath(struct
	let dir_writer                = CygwinPath.dir_writer
	let dir_reader                = CygwinPath.dir_reader
	let path_writer               = CygwinPath.path_writer
	let path_reader               = CygwinPath.path_reader
end)
;;

module CygwinPath : PATH_STRING_SPECIFICATION = GenericStringPath(AbstractCygwinPath)
;;
