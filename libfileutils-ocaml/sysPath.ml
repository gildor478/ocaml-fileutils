(** A module to manipulate path and filename *)
(** It doesn't need doesn't need the existence 
of any file and doesn't have any border effect *)

open SysPath_type;;

exception SysPathBasePathRelative;;
exception SysPathRelativeUnreducable;;
exception SysPathUnrecognizedOS of string;;
exception SysPathFilenameMultiple;;

type filename = SysPath_type.filename
;;

type filename_part = SysPath_type.filename_part
;;

type extension = SysPath_type.extension
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
(*	| "Win32" ->
	  	(Win32Path.implode, Win32Path.explode, Win32Path.make_path, Win32Path.explode_path)
	| "Cygwin" ->
	  	(CygwinPath.implode,CygwinPath.explode,CygwinPath.make_path,CygwinPath.explode_path)
	| "MacOS" ->
	  	(MacOSPath.implode, MacOSPath.explode, MacOSPath.make_path, MacOSPath.explode_path)*)
	| s ->
		raise (SysPathUnrecognizedOS s)
;;

let concat fln fln_part = 
	(* We use a lot of time to concatenate because of the @ 
	   we shoudl try to avoid this kind of behavior *)
	implode  ((explode fln) @ [fln_part])
;;

let is_relative fln  = 
	match explode fln with
	 (Root _) :: _ ->
	 	false
	| _ ->
		true
;;

let is_implicit fln  = 
	match explode fln with
	  ParentDir :: _ 
	| CurrentDir :: _ ->
		true
	| _ ->
		false
;;

let basename fln = 
	let lst = explode fln 
	in
	let nth = (List.length lst) - 1
	in
	if nth < 0 then
		Component ""
	else
		List.nth lst ((List.length lst)-1) 
;;

let dirname fln = 
	match List.rev ( explode fln ) with
	  hd :: tl ->
	  	implode (List.rev tl)
	| [] ->
		filename_of_filename_part CurrentDir	
;;

let check_extension fln ext = 
	let (real_fln, real_ext) = split_basename_extension (basename fln)
	in
	ext = real_ext 
;;

let chop_extension fln = 
	let (real_fln, real_ext) = split_basename_extension (basename fln)
	in
	real_ext
;;

let quote fln = 
	(* A corriger *)
	Filename.quote fln
;;

let filename_part_of_filename x =
	match explode x with
	  [ y ] -> y
	| [] -> Component ""
	| _  -> raise SysPathFilenameMultiple
;;

let check_base_path path =
	if is_relative path then
		raise SysPathBasePathRelative
	else
		()
;; 

let rec reduce_list path_lst =
	let stack_dir = Stack.create ()
	in
	let safe_push itm =
		Stack.push itm stack_dir
	in
	let safe_pop () =
		ignore ( Stack.pop stack_dir )
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
		  ParentDir    -> safe_pop ()
		| CurrentDir   -> ()
		| Component "" -> ()
		| Component _ 
		| Root _       -> safe_push itm
	in
	let lst = 
		List.iter walk_path path_lst;
		to_list ()
	in
	lst
;;

let reduce path =
	if is_relative path then
		raise SysPathRelativeUnreducable
	else
		implode (reduce_list (explode path))
;;

let make_absolute_list lst_base lst_path =
	reduce_list (lst_base @ lst_path)
;;

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
;;

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
;;

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
;;

let implode_string lst =
	implode ( List.map filename_part_of_filename lst )
;;

let parent_dir = ParentDir
;;

let current_dir = CurrentDir
;;
