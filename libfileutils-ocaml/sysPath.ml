open SysPath_type;;

exception SysPathBaseFilenameRelative;;
exception SysPathRelativeUnreducable;;
exception SysPathUnrecognizedOS of string;;
exception SysPathFilenameMultiple;;
exception SysPathEmpty;;
exception SysPathNoExtension;;
exception SysPathInvalidFilename;;

module type OS_SPECIFICATION =
sig
	val dir_writer                : (filename_part list) -> string
	val dir_reader                : Lexing.lexbuf -> (filename_part list)
	val path_writer               : (filename list) -> string
	val path_reader               : Lexing.lexbuf -> (filename list)
end
;;

module type PATH_SPECIFICATION =
sig

type filename
type extension

val string_from_filename : filename -> string
val filename_from_string : string -> filename

val is_subdir            : filename -> filename -> bool
val is_updir             : filename -> filename -> bool
val compare              : filename -> filename -> int

val basename        : filename -> filename
val dirname         : filename -> filename
val concat          : filename -> filename -> filename
val make_filename   : filename list -> filename 
val reduce          : filename -> filename
val make_absolute   : filename -> filename -> filename
val make_relative   : filename -> filename -> filename
val reparent        : filename -> filename -> filename -> filename 
val identity        : filename -> filename
val is_valid        : filename -> bool
val is_relative     : filename -> bool
val is_implicit     : filename -> bool

val chop_extension  : filename -> filename
val get_extension   : filename -> extension 
val check_extension : filename -> extension -> bool
val add_extension   : filename -> extension -> filename

val string_from_path : filename list -> string
val path_from_string : string -> filename list
end
;;

module type META_PATH_SPECIFICATION =
functor ( OsOperation : OS_SPECIFICATION ) -> 
PATH_SPECIFICATION
;;

module GenericPath : META_PATH_SPECIFICATION = 
functor ( OsOperation : OS_SPECIFICATION ) ->
struct
	type filename = SysPath_type.filename

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

	let filename_from_string str = 
		try 
			let lexbuf = Lexing.from_string str
			in
			OsOperation.dir_reader lexbuf
		with Parsing.Parse_error ->
			raise SysPathInvalidFilename

	(* String_from_filename *)

	let string_from_filename path = 
		OsOperation.dir_writer path


	(* Compare, subdir, updir *)

	type filename_relation = SubDir | UpDir | Equal | NoRelation of int

	let relation_of_filename path1 path2 =
		let rec relation_of_filename_aux path1 path2 =
			match (path1,path2) with
			  (hd1 :: tl1, hd2 :: tl2) ->
				if hd1 = hd2 then
					relation_of_filename_aux tl1 tl2
				else
					NoRelation (String.compare hd1 hd2)
			| (subdir, []) ->
				SubDir
			| ([], updir) ->
				UpDir
			| ([], []) ->
				Equal
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


	(* Concat *)

	let concat lst_path1 lst_path2 =
		lst_path1 @ lst_path2	


	(* Is_relative *)

	let is_relative_list lst_path =
		match lst_path with
		 (Root _) :: _ -> false
		| _            -> true

	
	(* Is_implicit *)
	
	let is_implicit_list lst_path  = 
		match lst_path with
		  ParentDir :: _ 
		| CurrentDir :: _ 
		| Component _ :: _ -> true
		| _                -> false

	(* Is_valid *)
	
	let is_valid path = 
		(* As we are manipulating abstract filename, and that it has been parsed, we are
		   sure that all is correct *)
		true

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
			((dirname path) @ [base], ext)
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
		path^"."^ext

	(* Reduce *)

	let reduce path =
		let rec reduce_aux lst = 
			match lst with 
			  ParentDir :: tl ->
			  	begin
				match reduce_list_aux tl with
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
				(reduce_list_aux tl)
			| Component s :: tl ->
				Component s :: (reduce_list_aux tl)
			| Root s :: tl ->
				Root s :: (reduce_list_aux tl)
			| [] ->
				[]
		in
		List.rev (reduce_aux (List.rev path))
		
	(* Make_asbolute *)

	let make_absolute path_base path_path =
		if is_relative_list path_base then
			raise SysPathBaseFilenameRelative
		else if is_relative_list path_path then
			reduce (path_base @ path_path)
		else
			reduce (path_path)

	(* Make_relative *)

	let make_relative path_base path_path =
		let rec make_relative_aux lst_base lst_path =
			match  (lst_base, lst_path) with
			x :: tl_base, a :: tl_path when x = a ->
				make_relative_list_aux tl_base tl_path
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
		List.flatten lst_path
		
	(* Reparent *)

	let reparent path_src path_dst path =
		let path_relative =
			make_relative_list path_src path
		in
		make_absolute path_dst path_relative

	(* Identity *)
	
	let identity path = path
	
	(* Manipulate path like variable *)

	let string_from_path lst = 
		OsOperation.path_writer lst

	let path_from_string str = 
		try
			let lexbuf = Lexing.from_string str
			in
			OsOperation.path_reader lexbuf
		with Parsing.Parse_error ->
			raise SysPathInvalidFilename
end 
;;

module type META_PATH_STRING_SPECIFICATION =
functor ( PathOperation : PATH_SPECIFICATION ) ->
PATH_SPECIFICATION
;;


module GenericStringPath : META_PATH_STRING_SPECIFICATION =
functor ( PathOperation : PATH_SPECIFICATION ) ->
struct

	type filename = string
	type extension = PathOperation.extension

	let string_from_filename path = 
		path

	let filename_from_string path = 
		path

	let f2s = PathOperation.filename_of_string

	let s2f = PathOperation.string_of_filename

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
		f2s (PathOperation.concat   (s2f path1) (s2f path2))
		
	let make_filename path_lst =
		f2s (PathOperation.make_filename (List.map s2f path_lst))

	let reduce path =
		f2S (PathOperation.reduce (s2f path))

	let make_absolute base_path path =
		f2s (PathOperation.make_absolute (s2f base_path) (s2f path))

	let make_relative base_path path =
		f2s (PathOperation.make_relative (s2f base_path) (s2f path))

	let reparent path_src path_dst path =
		f2s (PathOperation.reparent      (s2f path_src)  (s2f path_dst) (s2f path))

	let identity path =
		f2s (PathOperation.identity      (s2f path))

	let is_valid path =
		try
			f2s (PathOperation.identity      (s2f path))
		with SysPathInvalidFilename ->
			false

	let is_relative path  = 
		PathOperation.is_relative ( s2f path )

	let is_implicit path =
		PathOperation.is_implicit ( s2f path )

	let chop_extension path =
		f2s (PathOperation.chop_extension (s2f path))

	let get_extension path =
		PathOperation.get_extension (s2f path)

	let check_extension path ext =
		PathOperation.check_extension (s2f path) ext

	let add_extension path ext =
		f2s (PathOperation.add_extension (s2f path) ext)

	let string_from_path path_lst =
		PathOperation.string_from_path (List.map s2f path_lst)

	let path_from_string str =
		List.map f2s (PathOperation.path_from_string str)
end
;;
	

		
module UnixPath : PATH_SPECIFICATION = GenericPath(struct
	let dir_writer                = UnixPath.dir_writer
	let dir_reader                = UnixPath.dir_reader
	let path_writer               = UnixPath.path_writer
	let path_reader               = UnixPath.path_reader
end)
;;

module MacOSPath : PATH_SPECIFICATION = GenericPath(struct
	let dir_writer                = MacOSPath.dir_writer
	let dir_reader                = MacOSPath.dir_reader
	let path_writer               = MacOSPath.path_writer
	let path_reader               = MacOSPath.path_reader
end)
;;

module Win32Path : PATH_SPECIFICATION = GenericPath(struct 
	let dir_writer                = Win32Path.dir_writer
	let dir_reader                = Win32Path.dir_reader
	let path_writer               = Win32Path.path_writer
	let path_reader               = Win32Path.path_reader
end)
;;

module CygwinPath : PATH_SPECIFICATION = GenericPath(struct
	let dir_writer                = CygwinPath.dir_writer
	let dir_reader                = CygwinPath.dir_reader
	let path_writer               = CygwinPath.path_writer
	let path_reader               = CygwinPath.path_reader
end)
;;

let
(
 basename,       dirname,            up_dir, 
 concat,         reduce,             make_absolute, 
 make_relative,  reparent,           identity,           
 is_valid,       is_relative,        is_implicit,
 chop_extension, get_extension,      check_extension, 
 add_extension,  make_path_variable, read_path_variable, 
 current_dir,    parent_dir,         root, 
 component,      implode,            explode,
 make_filename
)
=
	match Sys.os_type with
	  "Unix" ->
(
 UnixPath.basename,       UnixPath.dirname,            UnixPath.up_dir, 
 UnixPath.concat,         UnixPath.reduce,             UnixPath.make_absolute, 
 UnixPath.make_relative,  UnixPath.reparent,           UnixPath.identity,           
 UnixPath.is_valid,       UnixPath.is_relative,        UnixPath.is_implicit,
 UnixPath.chop_extension, UnixPath.get_extension,      UnixPath.check_extension, 
 UnixPath.add_extension,  UnixPath.make_path_variable, UnixPath.read_path_variable, 
 UnixPath.current_dir,    UnixPath.parent_dir,         UnixPath.root, 
 UnixPath.component,      UnixPath.implode,            UnixPath.explode,
 UnixPath.make_filename
)
	| "MacOS" ->
(
 MacOSPath.basename,       MacOSPath.dirname,            MacOSPath.up_dir, 
 MacOSPath.concat,         MacOSPath.reduce,             MacOSPath.make_absolute, 
 MacOSPath.make_relative,  MacOSPath.reparent,           MacOSPath.identity,           
 MacOSPath.is_valid,       MacOSPath.is_relative,        MacOSPath.is_implicit,
 MacOSPath.chop_extension, MacOSPath.get_extension,      MacOSPath.check_extension, 
 MacOSPath.add_extension,  MacOSPath.make_path_variable, MacOSPath.read_path_variable, 
 MacOSPath.current_dir,    MacOSPath.parent_dir,         MacOSPath.root, 
 MacOSPath.component,      MacOSPath.implode,            MacOSPath.explode,
 MacOSPath.make_filename
)
	| "Win32" ->
(
 Win32Path.basename,       Win32Path.dirname,            Win32Path.up_dir, 
 Win32Path.concat,         Win32Path.reduce,             Win32Path.make_absolute, 
 Win32Path.make_relative,  Win32Path.reparent,           Win32Path.identity,           
 Win32Path.is_valid,       Win32Path.is_relative,        Win32Path.is_implicit,
 Win32Path.chop_extension, Win32Path.get_extension,      Win32Path.check_extension, 
 Win32Path.add_extension,  Win32Path.make_path_variable, Win32Path.read_path_variable, 
 Win32Path.current_dir,    Win32Path.parent_dir,         Win32Path.root, 
 Win32Path.component,      Win32Path.implode,            Win32Path.explode,
 Win32Path.make_filename
)
	| "Cygwin" ->
(
 CygwinPath.basename,       CygwinPath.dirname,            CygwinPath.up_dir, 
 CygwinPath.concat,         CygwinPath.reduce,             CygwinPath.make_absolute, 
 CygwinPath.make_relative,  CygwinPath.reparent,           CygwinPath.identity,           
 CygwinPath.is_valid,       CygwinPath.is_relative,        CygwinPath.is_implicit,
 CygwinPath.chop_extension, CygwinPath.get_extension,      CygwinPath.check_extension, 
 CygwinPath.add_extension,  CygwinPath.make_path_variable, CygwinPath.read_path_variable, 
 CygwinPath.current_dir,    CygwinPath.parent_dir,         CygwinPath.root, 
 CygwinPath.component,      CygwinPath.implode,            CygwinPath.explode,
 CygwinPath.make_filename
)
	| s ->
		raise (SysPathUnrecognizedOS s)
;;
