open SysPath_type;;

(** You cannot pass a base path which is relative *)
exception SysPathBasePathRelative;;
(** One of the path you have passed is relative and cannot be reduce *)
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
(** The path used is invalid *)
exception SysPathInvalidPath;;

type filename = SysPath_type.filename
;;

type filename_part = SysPath_type.filename_part
;;

type extension = SysPath_type.extension
;;

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
(********************************)
(** Manipulating/Splitting path *)
(********************************)

(** Extract the filename of a complete path *)
val basename        : filename -> filename

(** Extract the directory name of a complete path *)
val dirname         : filename -> filename

(** Move to the upper directory *)
val up_dir          : filename -> filename 

(** Append a filename_part to the filename *)
val concat      : filename -> filename -> filename

(** Remove all path component which are relative inside the path
For example : /a/../b -> /b/. Path must not be relative *)
val reduce : filename -> filename

(** Create an absolute path from a path relative to the base path *)
val make_absolute : filename -> filename -> filename

(** Create a path which is relative to the base path *)
val make_relative : filename -> filename -> filename

(** Reparent a path : reparent fln_src fln_dst fln, return the same filename as fln 
    but the root is no more fln_src but fln_dst *)
val reparent : filename -> filename -> filename -> filename 

(** Identity : for testing the stability of implode/explode *)
val identity : filename -> filename

(** Test if the filename is a valid one *)
val is_valid : filename -> bool

(** Check if the path is relative to a dir or not ( ie beginning with Component, ParentDir, CurrentDir ) *)
val is_relative : filename -> bool

(** Check if the path is an absolute one or not ( ie beginning by a Root reference )*)
val is_implicit : filename -> bool

(*****************************)
(** Manipulate the extension *)
(*****************************)

(** Remove extension *)
val chop_extension  : filename -> filename

(** Extract the extension *)
val get_extension   : filename -> extension 

(** Check the extension *)
val check_extension : filename -> extension -> bool

(** Add an extension *)
val add_extension   : filename -> extension -> filename

(*****************************************)
(** Transformation of PATH like variable *)
(*****************************************)

(** Create an environnement PATH like string from different path *)
val make_path_variable : filename list -> string

(** Return the different component of an environnement PATH like string *)
val read_path_variable : string -> filename list

(***********************)
(* Dangerous functions *)
(***********************)

(** Those function are provided for the ease of developper, however
    it is not recomended to use it : IT IS DANGEROUS. Most of path
    are context depedent and cannot be interpreted as simple 
    component without the rest of the string. Use it at your own 
    risk *)

val current_dir : filename
val parent_dir  : filename
val root        : string -> filename
val component   : string -> filename

(** Take a list of path component and return the string 
corresponding to this path *)
val implode : filename_part list -> filename

(** Take a string corresponding to a path a return the component 
of this path *)
val explode : filename -> filename_part list   
end
;;

module type META_PATH_SPECIFICATION =
functor ( OsOperation : OS_SPECIFICATION ) -> 
PATH_SPECIFICATION
;;

module GenericPath : META_PATH_SPECIFICATION = 
functor ( OsOperation : OS_SPECIFICATION ) ->
struct
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

	(* Explode *)

	let explode str = 
		try 
			let lexbuf = Lexing.from_string str
			in
			OsOperation.dir_reader lexbuf
		with Parsing.Parse_error ->
			raise SysPathInvalidFilename

	(* Implode *)

	let implode lst = 
		OsOperation.dir_writer lst 

	(* Concat *)

	let concat_list lst_fln1 lst_fln2 =
		implode (lst_fln1 @ lst_fln2)
	
	let concat fln1 fln2 = 
		concat_list  (explode fln1) (explode fln2)

	(* Is_relative *)

	let is_relative_list lst_fln =
		match lst_fln with
		 (Root _) :: _ -> false
		| _            -> true

	let is_relative fln  = 
		is_relative_list ( explode fln )
	
	(* Is_implicit *)
	
	let is_implicit_list lst_fln  = 
		match lst_fln with
		  ParentDir :: _ 
		| CurrentDir :: _ 
		| Component _ :: _ -> true
		| _                -> false

	let is_implicit fln =
		is_implicit_list ( explode fln )

	
	(* Is_valid *)
	
	let is_valid fln =
		try
			let _ = explode fln
			in
			true
		with SysPathInvalidFilename ->
			false

	(* Basename *)

	let basename fln = 
		match List.rev ( explode fln ) with	
		  hd :: tl ->
			implode ( [hd] )
		| [] ->
			raise SysPathEmpty

	(* Dirname *)

	let dirname fln = 
		match List.rev ( explode fln ) with
		  hd :: tl ->
			implode (List.rev tl)
		| [] ->
			raise SysPathEmpty

	(* Extension manipulation *)

	let split_extension fln = 
		match explode (basename fln) with
		  (Component str) :: []->
			let lexbuf = Lexing.from_string str
			in
			let (base,ext) = GenericPath_parser.main_extension
				GenericPath_lexer.token_extension
				lexbuf
			in
			(concat (dirname fln) base, ext)
		| _ ->
			raise SysPathNoExtension

	let check_extension fln ext = 
		let (real_fln, real_ext) = split_extension fln
		in
		ext = real_ext 

	let get_extension fln = 
		let (real_fln, real_ext) = split_extension fln
		in
		real_ext

	let chop_extension  fln =
		let (real_fln, real_ext) = split_extension fln
		in
		real_fln

	let add_extension fln ext =
		fln^"."^ext

	(* Reduce *)

	let rec reduce_list path_lst =
		let stack_dir = Stack.create ()
		in
		let to_list () =
			let tmp_arr = Array.make (Stack.length stack_dir) CurrentDir 
			in
			for i = (Stack.length stack_dir) - 1 downto 0 do 
				Array.set tmp_arr i (Stack.pop stack_dir) 
			done;
			Array.to_list tmp_arr
		in
		let walk_path itm =
			match itm with
			  ParentDir    -> 
			  	begin
				let last_cmp = Stack.pop stack_dir
				in
				match last_cmp with
				  Root s -> Stack.push (Root s) stack_dir
				| _ -> ()
				end
			| CurrentDir   -> ()
			| Component "" -> ()
			| Component _ 
			| Root _       -> Stack.push itm stack_dir
		in
		if is_relative_list path_lst then
			raise SysPathRelativeUnreducable
		else
			let lst = 
				List.iter walk_path path_lst;
				to_list ()
			in
			lst

	let reduce path =
		implode (reduce_list (explode path))

	(* Make_asbolute *)

	let make_absolute_list lst_base lst_path =
		if is_relative_list lst_base then
			raise SysPathBasePathRelative
		else if is_relative_list lst_path then
			reduce_list (lst_base @ lst_path)
		else
			reduce_list (lst_path)


	let make_absolute base_path path =
		implode (make_absolute_list (explode base_path) (explode path))

	(* Make_relative *)

	let make_relative_list lst_base lst_path =
		let rec make_relative_list_aux lst_base lst_path =
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
		if is_relative_list lst_base then
			raise SysPathBasePathRelative
		else if is_relative_list lst_path then
			(reduce_list lst_path)
		else
			make_relative_list_aux (reduce_list lst_base) (reduce_list lst_path)


	let make_relative base_path path =
		implode (make_relative_list (explode base_path) (explode path))

	(* Reparent *)

	let reparent_list lst_src lst_dst lst_fln =
		let lst_relative =
			make_relative_list lst_src lst_fln
		in
		make_absolute_list lst_dst lst_relative

	let reparent fln_src fln_dst fln =
		implode (reparent_list (explode fln_src) (explode fln_dst) (explode fln))

	(* Identity *)
	
	let identity fln =
		implode (explode fln)

	
	(* Dangerous functions *)

	let parent_dir  = implode [ParentDir]

	let current_dir = implode [CurrentDir]

	let root s      = implode [Root s]

	let component s = implode [Component s]

	let up_dir fln  = reduce ( concat fln parent_dir )

	(* Manipulate path like variable *)

	let make_path_variable lst = 
		OsOperation.path_writer lst

	let read_path_variable str = 
		try
			let lexbuf = Lexing.from_string str
			in
			OsOperation.path_reader lexbuf
		with Parsing.Parse_error ->
			raise SysPathInvalidPath
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
 component,      implode,            explode
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
 UnixPath.component,      UnixPath.implode,            UnixPath.explode
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
 MacOSPath.component,      MacOSPath.implode,            MacOSPath.explode
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
 Win32Path.component,      Win32Path.implode,            Win32Path.explode
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
 CygwinPath.component,      CygwinPath.implode,            CygwinPath.explode
)
	| s ->
		raise (SysPathUnrecognizedOS s)
;;
