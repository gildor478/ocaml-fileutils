(** A module to provide the core system utilities to 
manipulate file and directory. All function nearly match
common unix utilities ( but try to be more portable )*)

open Path;;

type fs_type = 
	Dir  
	| File 
	| Dev_char
	| Dev_block
	| Link
	| Fifo
	| Socket
	| Unknown
;;	

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
		Unknown 
;;

let stat_type_match tp filename = stat_type filename = tp
;;

let stat_right filename =
	try
		let stats = Unix.stat filename
		in
		stats.Unix.st_perm with
	with Unix.Unix_error(_) ->
		Unknown 
;;

let stat_right_match right filename = (stat_right filename land right) <> 0
;;

let right_sticky       = 0o1000
;;

let right_sticky_group = 0o2000
;;

let right_sticky_user  = 0o4000
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
	(** FILE exists and has a size greater than zero *)
	| Size_not_null
	(** FILE exists and is a socket *)
	| Is_socket
	(** FILE exists and its set-user-ID bit is set *)
	| Has_set_user_ID
	(** FILE exists and is writable *)
	| Is_writeable
	(** FILE exists and is executable *)
	| Is_exec
	(** FILE exists and is owned by the effective user ID *)
	| Is_owned_by_user_ID
	(** FILE exists and is owned by the effective group ID *)
	| Is_owned_by_group_ID
	(** FILE1 is newer (modification date) than FILE2 *)
	| Is_newer_than string * string
	(** FILE1 is older than FILE2 *)
	| Is_older_than string * string
	(** FILE1 and FILE2 have the same device and inode numbers *)
	| Has_same_device_and_inode string * string
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
	match flt with
	  Is_dev_block    -> stat_type_match Dev_block
	| Is_dev_char     -> stat_type_match Dev_char
	| Is_dir          -> stat_type_match Dir
	| Exists	  -> fun x -> not(stat_match Unknown x)
	| Is_file         -> stat_type_match File
	| Is_set_group_ID -> stat_right_match right_sticky_group
	| Has_sticky_bit  -> stat_right_match right_sticky
	| Is_link         -> stat_type_match Link
	| Is_pipe         -> stat_type_match Fifo
	| Is_readable     -> acces_match access_read
	| Size_not_null
	| Is_socket       -> stat_match Socket
	| Has_set_user_ID -> stat_right_match right_sticky_user
	| Is_writeable    -> access_match access_write
	| Is_exec         -> 
	| Is_owned_by_user_ID
	| Is_owned_by_group_ID
	| Is_newer_than string * string
	| Is_older_than string * string
	| Has_same_device_and_inode string * string
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
	| True ->
		fun x -> true
	| False ->
		fun x -> false
			
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


let filter_dir flt lst =
	let cflt = compile_filter flt
	in
	List.filter cflt lst
;;

let test tst fln =
	let ctst = compile_filter tst
	in
	ctst fln 
;;	


