open Fort;;
open SysPath;;
open SysUtil;;

let dir_test = "test-util"
in
let expect_equal_string = expect_equal ~printer:(fun x -> x)
in
let expect_equal_list_string = expect_equal ~printer:(fun lst -> 
	List.fold_left (fun x y -> x^";"^y ) "" lst)
in
let test_implode test imp exp = 
	expect_pass ~desc:("Implode " ^ test)
	~body:(fun () ->
		expect_equal_string imp (implode exp)
	)
in
let test_explode test exp imp =
	expect_pass ~desc:("Explode " ^ test)
	~body:(fun () ->
		expect_equal_list_string exp (explode imp)
	)
in
let test_reduce test exp =
	expect_pass ~desc:("Reduce " ^ test)
	~body:( fun () ->
		expect_equal_string "/a/b/c" (reduce exp)
	)
in
let test_make_path test exp =
	expect_pass ~desc:("Make Path " ^ test)
	~body:( fun () ->
		expect_equal_string "/a:b:/c/d" (make_path exp)
	)
in
let test_explode_path test imp =
	expect_pass ~desc:("Explode Path " ^ test)
	~body:( fun () ->
		expect_equal_list_string ["/a";"b";"/c/d"] (explode_path imp)
	)
in
let test_make_absolute test base rela res =
	expect_pass ~desc:("Make absolute " ^ test)
	~body:( fun () ->
		expect_equal_string res (make_absolute base rela)
	)
in
let test_make_relative test base abs res =
	expect_pass ~desc:("Make absolute " ^ test)
	~body:( fun () ->
		expect_equal_string res (make_relative base abs)
	)
in


test_implode "absolute"		"/a/b/c" 	["" ; "a" ; "b" ; "c"];
test_implode "implicit v1"		"a/b/c"  	["a"; "b"; "c"];
test_implode "implicit v2"	"./a/b/c" 	["."; "a"; "b"; "c"];
test_implode "implicit v3"	"../a/b/c"	[".."; "a"; "b"; "c"];
test_implode "int implicit v1"	"a/b/./c"	["a"; "b"; "."; "c" ];
test_implode "int implicit v2"	"a/b/../c"	["a"; "b"; ".."; "c" ];
test_implode "int implicit v3"	"a/b/.././c"	["a"; "b"; ".."; "."; "c" ];
test_implode "int implicit v4"	"a/b/./../c"	["a"; "b"; "."; ".."; "c" ];
test_implode "int implicit v5"	"a/../b/./c"	["a"; ".."; "b"; "."; "c" ];
test_implode "int implicit v6"	"a/../../b/./c" ["a"; ".."; ".."; "b"; "."; "c" ];
test_implode "keep last /"	"/a/b/c/"	[ ""; "a"; "b"; "c"; ""];
test_reduce "identity" 			"/a/b/c";
test_reduce "remove trailer" 		"/a/b/c/";
test_reduce "remove last .." 		"/a/b/c/d/..";
test_reduce "remove last ." 		"/a/b/c/.";
test_reduce "remove inside .." 		"/a/d/../b/c";
test_reduce "remove inside ." 		"/a/./b/c";
test_reduce "remove last . and .." 	"/a/b/c/d/./..";
test_reduce "remove last .. and ." 	"/a/b/c/d/../.";
test_reduce "remove following . and .." "/a/b/d/./../c";
test_reduce "remove following .. and ." "/a/b/d/.././c";
test_reduce "remove multiple .." 	"/a/b/../d/../b/c";
test_reduce "remove multiple ." 	"/a/./././b/./c";
test_reduce "remove multiple . and .." 	"/a/../a/./b/../c/../b/./c";

test_make_path "identity"	["/a";"b";"/c/d"];
test_explode_path "identity"	"/a:b:/c/d";

test_make_absolute "identity" "/a/b/c" "." "/a/b/c";
test_make_absolute "simple v1" "/a/b/c" "./d" "/a/b/c/d";
test_make_absolute "simple v2" "/a/b/c" "../d" "/a/b/d";


test_make_relative "identity" "/a/b/c" "/a/b/c" "";
test_make_relative "simple v1" "/a/b/c" "/a/b/d" "../d";

Fort.mkdir dir_test 0o755;
expect_true ~msg:("Test Is_dir "^dir_test) 
	(test Is_dir dir_test);
expect_true ~msg:("Test Not(Is_dir) "^dir_test) 
	(not (test (Not Is_dir) dir_test));
expect_true ~msg:("Test Is_dev_block "^dir_test) 
	(not (test Is_dev_block dir_test));
expect_true ~msg:("Test Is_dev_char "^dir_test) 
	(not (test Is_dev_char dir_test));
expect_true ~msg:("Test Exists "^dir_test) 
	(not (test Exists dir_test));
expect_true ~msg:("Test Is_file "^dir_test) 
	(not (test Is_file dir_test));
expect_true ~msg:("Test Is_set_group_ID "^dir_test) 
	(not (test Is_set_group_ID dir_test));
expect_true ~msg:("Test Has_sticky_bit "^dir_test) 
	(not (test Has_sticky_bit dir_test));
expect_true ~msg:("Test Is_link "^dir_test) 
	(not (test Is_link dir_test));
expect_true ~msg:("Test Is_pipe "^dir_test) 
	(not (test Is_pipe dir_test));
expect_true ~msg:("Test Is_readable "^dir_test) 
	(test Is_readable dir_test);
expect_true ~msg:("Test Is_writeable "^dir_test) 
	(test Is_writeable dir_test);
expect_true ~msg:("Test Size_not_null "^dir_test) 
	(not (test Size_not_null dir_test));
expect_true ~msg:("Test Is_socket "^dir_test) 
	(not (test Is_socket dir_test));
expect_true ~msg:("Test Has_set_user_ID "^dir_test) 
	(not (test Has_set_user_ID dir_test));
expect_true ~msg:("Test Is_exec "^dir_test) 
	(test Is_exec dir_test);
expect_true ~msg:("Test Is_owned_by_user_ID "^dir_test) 
	(not (test Is_owned_by_user_ID dir_test));
expect_true ~msg:("Test Is_owned_by_group_ID "^dir_test) 
	(not (test Is_owned_by_group_ID dir_test));
expect_true ~msg:("Test Is_newer_than(test.ml,test.ml) "^dir_test) 
	(not (test (Is_newer_than("test.ml","test.ml")) dir_test));
expect_true ~msg:("Test Is_older_than(test.ml,test.ml) "^dir_test) 
	(not (test (Is_older_than("test.ml","test.ml")) dir_test));
expect_true ~msg:("Test Has_same_device_and_inode(test.ml,test.ml)"^dir_test) 
	(test (Has_same_device_and_inode("test.ml","test.ml")) dir_test);
(*expect_true ~msg:("Test And of test_file * test_file"^dir_test) 
	(not (test And of test_file * test_file dir_test));*)
(*expect_true ~msg:("Test Or of test_file * test_file"^dir_test) 
	(not (test Or of test_file * test_file dir_test));*)
(*expect_true ~msg:("Test Match of string"^dir_test) 
	(not (test Match of string dir_test));*)
expect_true ~msg:("Test True"^dir_test) 
	(test True dir_test);
expect_true ~msg:("Test False"^dir_test) 
	(not (test False dir_test));

();;
