(** A module to manipulate path and filename *)
(** It doesn't need doesn't need the existence 
of any file and doesn't have any border effect *)

exception Base_path_relative;;
exception Path_relative_unreducable;;

let current_dir_name = Filename.current_dir_name;;
let parent_dir_name  = Filename.parent_dir_name;;
let concat           = Filename.concat;;
let is_relative      = Filename.is_relative;;
let is_implicit      = Filename.is_implicit;;
let check_suffix     = Filename.check_suffix;;
let chop_suffix      = Filename.chop_suffix;;
let chop_extension   = Filename.chop_extension;;
let basename         = Filename.basename;;
let dirname          = Filename.dirname;;
let temp_file        = Filename.temp_file;;
let open_temp_file   = Filename.open_temp_file;;
let quote            = Filename.quote;;

let check_base_path path =
	if is_relative path then
		raise Base_path_relative
	else
		()
;; 

let implode path =
	String.concat "/" path
;;

let rec explode path =
	Str.split_delim (Str.regexp "/") path
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
		let tmp_arr = Array.make (Stack.length stack_dir) ""
		in
		for i = (Stack.length stack_dir) - 1 downto 0 do 
			Array.set tmp_arr i (Stack.pop stack_dir) 
		done;
		Array.to_list tmp_arr
	in
	let walk_path itm =
		if itm = parent_dir_name then
			safe_pop ()
		else if itm = current_dir_name then
			()
		else if itm = "" then
			()
		else
			safe_push itm
	in
	let lst = List.iter walk_path path_lst;
		to_list ()
	in
	match path_lst with
	"" :: _ ->
		"" :: lst
	| _ ->
		lst
;;

let reduce path =
	if is_relative path then
		raise Path_relative_unreducable
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
			(fun x -> parent_dir_name)
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

let make_path lst =
	String.concat ":" lst
;;

let explode_path s =	
	Str.split (Str.regexp ":") s 
;;
