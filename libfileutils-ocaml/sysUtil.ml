(** A module to provide the core system utilities to 
manipulate file and directory. All function nearly match
common unix utilities ( but try to be more portable )*)

open SysPath;;

type fs_type = 
	Dir  
	| File 
	| Dev_char
	| Dev_block
	| Link
	| Fifo
	| Socket
;;	

exception File_doesnt_exist ;;
exception RmDirNotEmpty;;
exception MkdirMissingComponentPath;;
exception MkdirDirnameAlreadyUsed;;

let stat_type filename =
	try
		let stats = Unix.stat filename
		in
		match stats.Unix.st_kind with
		 Unix.S_REG -> File 
		| Unix.S_DIR -> Dir 
		| Unix.S_CHR -> Dev_char 
		| Unix.S_BLK -> Dev_block
		| Unix.S_LNK -> Link
		| Unix.S_FIFO -> Fifo 
		| Unix.S_SOCK -> Socket
	with Unix.Unix_error(_) ->
		raise File_doesnt_exist 
;;

let stat_type_match tp filename = stat_type filename = tp
;;

let stat_right filename =
	try
		let stats = Unix.stat filename
		in
		stats.Unix.st_perm 
	with Unix.Unix_error(_) ->
		raise File_doesnt_exist
;;

let stat_right_match right filename = 
	(stat_right filename land right) <> 0
;;

let right_sticky       = 0o1000
;;

let right_sticky_group = 0o2000
;;

let right_sticky_user  = 0o4000
;;

let right_exec         = 0o0111
;;

let right_write        = 0o0222
;;

let right_read         = 0o0444
;;

let stat_size filename =
	try
		let stats = Unix.stat filename
		in
		stats.Unix.st_size
	with Unix.Unix_error(_) ->
		raise File_doesnt_exist
;;

let stat_ugid filename =
	try
		let stats = Unix.stat filename
		in
		(stats.Unix.st_uid,stats.Unix.st_gid)
	with Unix.Unix_error(_) ->
		raise File_doesnt_exist
;;

let stat_mtime filename =
	try
		let stats = Unix.stat filename
		in
		stats.Unix.st_mtime
	with Unix.Unix_error(_) ->
		raise File_doesnt_exist
;;

let stat_dev filename =
	try
		let stats = Unix.stat filename
		in
		(stats.Unix.st_dev,stats.Unix.st_rdev)
	with Unix.Unix_error(_) ->
		raise File_doesnt_exist
;;

let stat_inode filename =
	try
		let stats = Unix.stat filename
		in
		stats.Unix.st_ino
	with Unix.Unix_error(_) ->
		raise File_doesnt_exist
;;

type test_file =
	(** FILE exists and is block special *)
	Is_dev_block
	(** FILE exists and is character special *)
	| Is_dev_char
	(** FILE exists and is a directory *)
	| Is_dir
	(** FILE exists *)
	| Exists
	(** FILE exists and is a regular file *)
	| Is_file
	(** FILE exists and is set-group-ID *)
	| Is_set_group_ID
	(** FILE exists and has its sticky bit set *)
	| Has_sticky_bit
	(** FILE exists and is a symbolic link *)
	| Is_link
	(** FILE exists and is a named pipe *)
	| Is_pipe
	(** FILE exists and is readable *)
	| Is_readable
	(** FILE exists and is writeable *)
	| Is_writeable
	(** FILE exists and has a size greater than zero *)
	| Size_not_null
	(** FILE exists and is a socket *)
	| Is_socket
	(** FILE exists and its set-user-ID bit is set *)
	| Has_set_user_ID
	(** FILE exists and is executable *)
	| Is_exec
	(** FILE exists and is owned by the effective user ID *)
	| Is_owned_by_user_ID
	(** FILE exists and is owned by the effective group ID *)
	| Is_owned_by_group_ID
	(** FILE1 is newer (modification date) than FILE2 *)
	| Is_newer_than of string * string
	(** FILE1 is older than FILE2 *)
	| Is_older_than of string * string
	(** FILE1 and FILE2 have the same device and inode numbers *)
	| Has_same_device_and_inode of string * string
	(** Result of TEST1 and TEST2 *)
	| And of test_file * test_file
	(** Result of TEST1 or TEST2 *)
	| Or of test_file * test_file
	(** Result of not TEST *)
	| Not of test_file
	(** Match Str regex *)
	| Match of string
	(** Always true *)
	| True
	(** Always false *)
	| False
;;

(*let test_file_of_string x =
	match x with 
	"-b" -> Is_dev_block
	| "-c" -> Is_dev_char
	| "-d" -> Is_dir
	| "-e" -> Exists
	| "-f" -> Is_file
	| "-g" -> Is_set_group_ID
	| "-k" -> Has_sticky_bit
	| "-L" -> Is_link
	| "-p" -> Is_pipe
	| "-r" -> Is_readable
	| "-s" -> Size_not_null
	| "-S" -> Is_socket
	| "-u" -> Has_set_user_ID
	| "-w" -> Is_writeable
	| "-x" -> Is_exec
	| "-O" -> Is_owned_by_user_ID
	| "-G" -> Is_owned_by_group_ID
	| "-nt" -> Is_newer_than
	| "-ot" -> Is_older_than
	| "-ef" -> Has_same_device_and_inode
*)

let rec compile_filter flt =
	let res_filter =
		match flt with
		  Is_dev_block    -> stat_type_match Dev_block
		| Is_dev_char     -> stat_type_match Dev_char
		| Is_dir          -> stat_type_match Dir
		| Is_file         -> stat_type_match File
		| Is_set_group_ID -> stat_right_match right_sticky_group
		| Has_sticky_bit  -> stat_right_match right_sticky
		| Is_link         -> stat_type_match Link
		| Is_pipe         -> stat_type_match Fifo
		| Is_readable     -> stat_right_match right_read
		| Is_writeable    -> stat_right_match right_write
		| Size_not_null   -> fun x -> (stat_size x) > 0
		| Is_socket       -> stat_type_match Socket
		| Has_set_user_ID -> stat_right_match right_sticky_user
		| Is_exec         -> stat_right_match right_exec
		| True            -> fun x -> true
		| False           -> fun x -> false
		| Is_owned_by_user_ID  -> fun x -> Unix.geteuid () = fst (stat_ugid x)
		| Is_owned_by_group_ID -> fun x -> Unix.getegid () = snd (stat_ugid x)
		| Is_newer_than(f1,f2) -> fun x -> (stat_mtime f1) < (stat_mtime f2)
		| Is_older_than(f1,f2) -> fun x -> (stat_mtime f1) > (stat_mtime f2)
		| Has_same_device_and_inode(f1,f2) -> 
			fun x -> (stat_dev f1,stat_inode f1) = (stat_dev f2, stat_inode f2)
		| Exists	  -> fun x -> let _ = stat_right x in  true
		| And(flt1,flt2) ->
			begin
			fun x -> 
				let cflt1 = (compile_filter flt1)
				in
				let cflt2 = (compile_filter flt2)
				in
				(cflt1 x) && (cflt2 x)
			end
		| Or(flt1,flt2) ->
			begin
			fun x -> 
				let cflt1 = (compile_filter flt1)
				in
				let cflt2 = (compile_filter flt2)
				in
				(cflt1 x) || (cflt2 x)
			end
		| Not(flt1) ->
			begin
			fun x -> 
				let cflt1 = (compile_filter flt1)
				in
				not (cflt1 x)
			end	
		| Match(r) ->
			begin
			let reg = Str.regexp r
			in
			fun x -> Str.string_match reg x 0
			end
	in
	fun x -> ( try res_filter x with File_doesnt_exist -> false )
;;

let list_dir dirname =
	let hdir = Unix.opendir dirname
	in
	let rec list_dir_aux lst =
		try
			let filename = Unix.readdir hdir
			in
			let complete_path = 
				concat dirname filename
			in
			list_dir_aux (complete_path :: lst)
		with End_of_file ->
			Unix.closedir hdir;
			lst
	in
	list_dir_aux []
;;	


let filter flt lst =
	let cflt = compile_filter flt
	in
	List.filter cflt lst
;;

let test tst fln =
	let ctst = compile_filter tst
	in
	ctst fln 
;;	

let which ?(path) fln =
	let real_path =
		match path with
		  None ->
			explode_path (Sys.getenv "PATH")
		| Some x ->
			x
	in
	let ctst x = 
		test (And(Is_exec,Not(Is_dir))) (concat x fln)
	in
	let which_path =
		List.find ctst real_path
	in
	concat which_path fln
;;

let mkdir ?(parent=false) ?mode fln =
	let real_mode = 
		match mode with
		  Some x -> x
		| None -> 0o0755
	in
	let mkdir_simple fln =
		if test Exists fln then
			if test Is_dir fln then
				()
			else
				raise MkdirDirnameAlreadyUsed 
		else
			try 
				Unix.mkdir fln real_mode 
			with Unix.Unix_error(Unix.ENOENT,_,_) | Unix.Unix_error(Unix.ENOTDIR,_,_) ->
				raise MkdirMissingComponentPath
	in
	if parent then
		let exploded_path = explode fln
		in
		let _ = List.fold_left (fun x y -> 
			match x with 
			  Some s -> let next = concat s y in
			  	mkdir_simple next; Some next
			| None ->
				mkdir_simple y; Some y
			) None exploded_path
		in
		()
	else
		mkdir_simple fln
;;		

let touch ?(create=true) fln =
	if (test (And(Exists,Is_file)) fln) || create then
		close_out (open_out fln)
	else 
		()
;;

let find tst fln =
	let ctest = compile_filter (And(tst,Not(Or(Match("\\.\\."),Match("\\.")))))
	in
	let cdir  = compile_filter (And(Is_dir,Not(Or(Match("\\.\\."),Match("\\.")))))
	in
	let rec find_simple fln =
		let dir_content = list_dir fln
		in
		List.fold_left 
			(fun x y -> List.rev_append (find_simple y) x)
			(List.filter ctest dir_content)
			(List.filter cdir dir_content)
	in
	if test Is_dir fln then
		find_simple fln
	else if ctest fln then
		[fln]
	else
		[]
;;

let rm ?(recurse=false) fln =
	let rm_simple fln =
		if test Is_dir fln then
			try 
				Unix.rmdir fln
			with Unix.Unix_error(Unix.ENOTEMPTY,_,_) ->
				raise RmDirNotEmpty
		else
			Unix.unlink fln
	in
	if recurse then
		List.iter rm_simple (find True fln)
	else
		rm_simple fln
;;
