open SysPath_type;;

exception SysPathBasePathRelative;;
exception SysPathRelativeUnreducable;;
exception SysPathUnrecognizedOS of string;;
exception SysPathFilenameMultiple;;
exception SysPathEmpty;;

type filename = SysPath_type.filename
;;

type filename_part = SysPath_type.filename_part
;;

type extension = SysPath_type.extension
;;

module GenericPath = 
functor ( OsOperation ) ->
struct
	let filename_of_filename_part x =
		OsOperation.filename_of_filename_part

	let filename_part_of_filename x =
		match explode x with
		  [ y ] -> y
		| [] -> raise SysPathEmpty
		| _  -> raise SysPathFilenameMultiple


	let explode str = 
		let (path_lexer,path_parser) = OsOperation.explode_spec
		in
		let lexbuf = Lexing.from_string str
		in
		path_parser path_lexer lexbuf

	let implode lst = 
		String.concat OsOperation.dir_separator 
		( List.map filename_of_filename_part lst )

	let concat fln fln_part = 
		(* We use a lot of time to concatenate because of the @ 
		   we shoudl try to avoid this kind of behavior *)
		implode  ((explode fln) @ [fln_part])

	let is_relative fln  = 
		match explode fln with
		 (Root _) :: _ -> false
		| _            -> true

	let is_implicit fln  = 
		match explode fln with
		  ParentDir :: _ 
		| CurrentDir :: _ -> true
		| _               -> false

	let basename fln = 
		try
			List.nth lst ((List.length (explode fln))-1) 
		with (Failure "nth") ->
			raise SysPathEmpty

	let dirname fln = 
		match List.rev ( explode fln ) with
		  hd :: tl ->
			implode (List.rev tl)
		| [] ->
			raise SysPathEmpty

	let split_extension fln = 
		match basename fln with
		  Component str ->
			let lexbuf = Lexing.from_string str
			in
			let (base,ext) = GenericPath_parser.main_extension
				GenericPath_lexer.token_extension
				lexbuf
			in
			(Component base, ext)
		| ParentDir 
		| CurrentDir 
		| Root _ ->
			( fln_part, "" )

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

	let check_base_path path =
		if is_relative path then
			raise SysPathBasePathRelative
		else
			()

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
			  ParentDir    -> ignore ( Stack.pop stack_dir )
			| CurrentDir   -> ()
			| Component "" -> ()
			| Component _ 
			| Root _       -> Stack.push itm stack_dir
		in
		let lst = 
			List.iter walk_path path_lst;
			to_list ()
		in
		lst

	let reduce path =
		if is_relative path then
			raise SysPathRelativeUnreducable
		else
			implode (reduce_list (explode path))

	let make_absolute_list lst_base lst_path =
		reduce_list (lst_base @ lst_path)

	let make_absolute base_path path =
		if is_relative path then
			begin
			let list_absolute =
				check_base_path base_path;
				make_absolute_list 
					(reduce_list (explode base_path))
					(explode path)
			in
			implode list_absolute
			end
		else
			path

	let rec make_relative_list lst_base lst_path =
		match  (lst_base, lst_path) with
		x :: tl_base, a :: tl_path when x = a ->
			make_relative_list tl_base tl_path
		| _, _ ->
			let back_to_base = List.rev_map 
				(fun x -> ParentDir)
				lst_base
			in
			back_to_base @ lst_path

	let make_relative base_path path =
		if is_relative path then
			path
		else
			begin
			let list_relative =
				check_base_path base_path;
				make_relative_list 
					(reduce_list (explode base_path))
					(reduce_list (explode path))
			in
			implode list_relative
			end

	let parent_dir  = ParentDir

	let current_dir = CurrentDir

	let root s      = Root s

	let component s = Component s

	let up_dir fln  = reduce ( concat fln ParentDir )

	let make_path_variable lst = 
		String.concat OsOperation.path_separator lst

	let read_path_variable str = 
		let (path_parser,path_lexer)= OsOperation.path_spec
		in
		let lexbuf = Lexing.from_string str
		in
		path_parser path_lexer lexbuf
end
;;


module UnixPath = GenericPath(struct
	let filename_of_filename_part = UnixPath.filename_of_filename_part
	let dir_separator             = UnixPath.dir_separator
	let dir_spec                  = UnixPath.dir_spec
	let path_separator            = UnixPath.path_separator
	let path_spec                 = UnixPath.path_spec
end)
;;

module MacOSPath = GenericPath(struct
	let filename_of_filename_part = MacOSPath.filename_of_filename_part
	let dir_separator             = MacOSPath.dir_separator
	let dir_spec                  = MacOSPath.dir_spec
	let path_separator            = MacOSPath.path_separator
	let path_spec                 = MacOSPath.path_spec
end)
;;

module Win32Path = GenericPath(struct 
	let filename_of_filename_part = Win32Path.filename_of_filename_part
	let dir_separator             = Win32Path.dir_separator
	let dir_spec                  = Win32Path.dir_spec
	let path_separator            = Win32Path.path_separator
	let path_spec                 = Win32Path.path_spec
end)
;;

module MingwPath = GenericPath(struct
	let filename_of_filename_part = MingwPath.filename_of_filename_part
	let dir_separator             = MingwPath.dir_separator
	let dir_spec                  = MingwPath.dir_spec
	let path_separator            = MingwPath.path_separator
	let path_spec                 = MingwPath.path_spec
end)
;;

let (
 implode,
 explode,
 make_path_variable,
 read_path_variable,
 split_basename_extension,
 filename_of_filename_part
 ) =
	match Sys.os_type with
	  "Unix" ->
	  	(
		 UnixPath.implode,  
		 UnixPath.explode,  
		 UnixPath.make_path_variable,
		 UnixPath.read_path_variable,
		 UnixPath.split_basename_extension,
		 UnixPath.filename_of_filename_part
		)
	| "Win32" ->
	| "Cygwin" ->
	| "MacOS" ->
	| s ->
		raise (SysPathUnrecognizedOS s)
;;
