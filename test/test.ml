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
let expect_equal_bool = expect_equal ~printer:(string_of_bool)
in
(* Test to be performed *)
let test_implode (test,imp,exp) = 
	expect_pass ~desc:("Implode " ^ test)
	~body:(fun () ->
		expect_equal_string imp (implode exp)
	)
in
let test_explode (test,exp,imp) =
	expect_pass ~desc:("Explode " ^ test)
	~body:(fun () ->
		expect_equal_list_string exp (explode imp)
	)
in
let test_reduce (test,exp) =
	expect_pass ~desc:("Reduce " ^ test)
	~body:( fun () ->
		expect_equal_string "/a/b/c" (reduce exp)
	)
in
let test_make_path (test,exp) =
	expect_pass ~desc:("Make Path " ^ test)
	~body:( fun () ->
		expect_equal_string "/a:b:/c/d" (make_path exp)
	)
in
let test_explode_path (test,imp) =
	expect_pass ~desc:("Explode Path " ^ test)
	~body:( fun () ->
		expect_equal_list_string ["/a";"b";"/c/d"] (explode_path imp)
	)
in
let test_make_absolute (test,base,rela,res) =
	expect_pass ~desc:("Make absolute " ^ test)
	~body:( fun () ->
		expect_equal_string res (make_absolute base rela)
	)
in
let test_make_relative (test,base,abs,res) =
	expect_pass ~desc:("Make absolute " ^ test)
	~body:( fun () ->
		expect_equal_string res (make_relative base abs)
	)
in
let test_test (stest,expr,file,res) =
	expect_pass ~desc:("Test "^stest^" on "^file) 
	~body:( fun () ->
		expect_equal_bool res (test expr file)
	)
in

(****************)
(* SysPath test *)
(****************)

(* Implode path *)
List.iter test_implode 
[
 ("absolute",            "/a/b/c",        ["" ; "a" ; "b" ; "c"]           );
 ("implicit v1",         "a/b/c",         ["a"; "b"; "c"]                  );
 ("implicit v2",         "./a/b/c",       ["."; "a"; "b"; "c"]             );
 ("implicit v3",         "../a/b/c",      [".."; "a"; "b"; "c"]            );
 ("int implicit v1",     "a/b/./c",       ["a"; "b"; "."; "c" ]            );
 ("int implicit v2",     "a/b/../c",      ["a"; "b"; ".."; "c" ]           );
 ("int implicit v3",     "a/b/.././c",    ["a"; "b"; ".."; "."; "c" ]      );
 ("int implicit v4",     "a/b/./../c",    ["a"; "b"; "."; ".."; "c" ]      );
 ("int implicit v5",     "a/../b/./c",    ["a"; ".."; "b"; "."; "c" ]      );
 ("int implicit v6",     "a/../../b/./c", ["a"; ".."; ".."; "b"; "."; "c" ]);
 ("keep last /",         "/a/b/c/",       [ ""; "a"; "b"; "c"; ""]         )
];

(* Reduce path *)
List.iter test_reduce
[
 ("identity",                    "/a/b/c"          );
 ("remove trailer",              "/a/b/c/"         );
 ("remove last ..",              "/a/b/c/d/.."     );
 ("remove last .",               "/a/b/c/."        );
 ("remove inside ..",            "/a/d/../b/c"     );
 ("remove inside .",             "/a/./b/c"        );
 ("remove last . and ..",        "/a/b/c/d/./.."   );
 ("remove last .. and .",        "/a/b/c/d/../."   );
 ("remove following . and ..",   "/a/b/d/./../c"   );
 ("remove following .. and .",   "/a/b/d/.././c"   );
 ("remove multiple ..",          "/a/b/../d/../b/c");
 ("remove multiple .",           "/a/./././b/./c"  );
 ("remove multiple . and ..",    "/a/../a/./b/../c/../b/./c")
];

(* Create path *)
List.iter test_make_path
[
 ("identity", ["/a";"b";"/c/d"])
];

(* Explode path *)
List.iter test_explode_path
[
 ("identity", "/a:b:/c/d")
];

(* Convert to absolute *)
List.iter test_make_absolute
[
 ("identity",  "/a/b/c", ".",    "/a/b/c");
 ("simple v1", "/a/b/c", "./d",  "/a/b/c/d");
 ("simple v2", "/a/b/c", "../d", "/a/b/d")
];

(* Convert to relative *)
List.iter test_make_relative 
[
 ("identity",  "/a/b/c", "/a/b/c", "");
 ("simple v1", "/a/b/c", "/a/b/d", "../d")
];

Fort.mkdir dir_test 0o755;
List.iter test_test
[
 ("True",                         True,                dir_test,true );
 ("False",                        False,               dir_test,false);
 ("Is_dir",                       Is_dir,              dir_test,true );
 ("Not Is_dir",                   (Not Is_dir),        dir_test,false);
 ("Is_dev_block",                 Is_dev_block,        dir_test,false);
 ("Is_dev_char",                  Is_dev_char,         dir_test,false);
 ("Exists",                       Exists,              dir_test,true );
 ("Is_file",                      Is_file,             dir_test,false);
 ("Is_set_group_ID",              Is_set_group_ID,     dir_test,false);
 ("Has_sticky_bit",               Has_sticky_bit,      dir_test,false);
 ("Is_link",                      Is_link,             dir_test,false);
 ("Is_pipe",                      Is_pipe,             dir_test,false);
 ("Is_readable",                  Is_readable,         dir_test,true );
 ("Is_writeable",                 Is_writeable,        dir_test,true );
 ("Size_not_null",                Size_not_null,       dir_test,true );
 ("Is_socket",                    Is_socket,           dir_test,false);
 ("Has_set_user_ID",              Has_set_user_ID,     dir_test,false);
 ("Is_exec",                      Is_exec,             dir_test,true );
 ("And of test_file * test_file", And(True,False),     dir_test,false);
 ("Or of test_file * test_file",  Or(True,False),      dir_test,true );
 ("Match",                        Match(dir_test),     dir_test,true );
 ("Is_owned_by_user_ID",          Is_owned_by_user_ID, dir_test,true );
 ("Is_owned_by_group_ID",         Is_owned_by_group_ID,dir_test,true );
 ("Is_newer_than",                (Is_newer_than("test.ml","test.ml")),            dir_test,false);
 ("Is_older_than",                (Is_older_than("test.ml","test.ml")),            dir_test,false);
 ("Has_same_device_and_inode",    (Has_same_device_and_inode("test.ml","test.ml")),dir_test,true )
];

Fort.test ~desc:"Touch in not existing subdir"
~body:( fun () ->
	try 
		SysUtil.touch (implode [dir_test;"doesntexist";"essai0"]);
		Pass
	with _ ->
		XFail
);

expect_pass ~desc:"Touch in existing dir v1"
~body:( fun () ->
	touch (implode [dir_test;"essai0"]);
	expect_true (test Exists (implode [dir_test;"essai0"]))
);

expect_pass ~desc:"Touch in existing dir with no create"
~body:( fun () ->
	touch ~create:false (implode [dir_test;"essai2"]);
	expect_true (not (test Exists (implode [dir_test;"essai2"])))
);

expect_pass ~desc:"Touch in existing dir v2"
~body:( fun () ->
	touch (implode [dir_test;"essai1"]);
	expect_true (test Exists (implode [dir_test;"essai1"]))
);

(* Too fast 
expect_pass ~desc:"Touch precedence"
~body:( fun () ->
	expect_true (test (Is_newer_than(concat dir_test "essai0",concat dir_test "essai1")) dir_test )
);
*)

expect_pass ~desc:"Mkdir simple v1"
~body:( fun () ->
	mkdir (implode [dir_test;"essai2"]);
	expect_true (test Is_dir (concat dir_test "essai2"))
);

expect_pass ~desc:"Mkdir simple && mode 700"
~body:( fun () ->
	mkdir ~mode:0o0700 (concat dir_test "essai3");
	expect_true (test Is_dir (concat dir_test "essai3"))
);

Fort.test ~desc:"Mkdir recurse v1"
~body:(fun () ->
	try
		mkdir (implode [dir_test; "essai4"; "essai5"]);
		Pass
	with MkdirMissingComponentPath ->
		XFail
);

Fort.test ~desc:"Mkdir && already exist v1"
~body:(fun () ->
	try
		mkdir (concat dir_test "essai0");
		Pass
	with MkdirDirnameAlreadyUsed ->
		XFail
);

expect_pass ~desc:"Mkdir recurse v2"
~body:(fun () ->
	mkdir ~parent:true (implode [dir_test; "essai4"; "essai5"]);
	expect_true (test Is_dir (implode [dir_test; "essai4"; "essai5"]))
);
		
	
();;
