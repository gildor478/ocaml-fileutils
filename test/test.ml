open Fort;;
open SysPath;;
open SysUtil;;

exception CannotContinueTest;;

module Test = 
functor ( OsPath : PATH_SPECIFICATION ) ->
struct
	let os_string = ref ""
	
	let expect_equal_string = expect_equal ~printer:(fun x -> x)
	
	let expect_equal_list_string = expect_equal ~printer:(fun lst -> 
		List.fold_left (fun x y -> x^";"^y ) "" lst)
		
	let expect_equal_bool = expect_equal ~printer:(string_of_bool)


	let reduce (test,exp,res) =
		expect_pass ~desc:((!os_string)^" : reduce " ^ test)
		~body:( fun () ->
			expect_equal_string res (OsPath.reduce exp)
		)
		
	let make_path (test,exp,res) =
		expect_pass ~desc:((!os_string)^" : make Path " ^ test)
		~body:( fun () ->
			expect_equal_string res (OsPath.make_path_variable exp)
		)
		
	let make_absolute (test,base,rela,res) =
		expect_pass ~desc:((!os_string)^" : make absolute " ^ test)
		~body:( fun () ->
			expect_equal_string res (OsPath.make_absolute base rela)
		)
		
	let make_relative (test,base,abs,res) =
		expect_pass ~desc:((!os_string)^" : make relative " ^ test)
		~body:( fun () ->
			expect_equal_string res (OsPath.make_relative base abs)
		)
		
	let valid (test,exp) =
		expect_pass ~desc:((!os_string)^" : valid "^test )
		~body:( fun () ->
			expect_true (OsPath.is_valid exp)
		)
		
	let identity (test,exp) = 
		expect_pass ~desc:((!os_string)^" : identity "^test)
		~body:( fun () ->
			expect_equal_string exp (OsPath.identity exp)
		)
end
;;

module TestUnix  = Test(UnixPath)
;;
TestUnix.os_string := "Unix"
;;
module TestMacOS = Test(MacOSPath)
;;
TestMacOS.os_string := "MacOS"
;;
module TestWin32 = Test(Win32Path)
;;
TestWin32.os_string := "Win32"
;;
(*module TestCygwin = Test(CygWin)
;;
TestCygwin.os_string := "Cygwin"
;;*)

(*********************)
(* Unix SysPath test *)
(*********************)

(* Reduce path *)
List.iter TestUnix.reduce
[
 ("identity",                    "/a/b/c",                   "/a/b/c");
 ("remove trailer",              "/a/b/c/",                  "/a/b/c");
 ("remove last ..",              "/a/b/c/d/..",              "/a/b/c");
 ("remove last .",               "/a/b/c/.",                 "/a/b/c");
 ("remove inside ..",            "/a/d/../b/c",              "/a/b/c");
 ("remove inside .",             "/a/./b/c",                 "/a/b/c");
 ("remove last . and ..",        "/a/b/c/d/./..",            "/a/b/c");
 ("remove last .. and .",        "/a/b/c/d/../.",            "/a/b/c");
 ("remove following . and ..",   "/a/b/d/./../c",            "/a/b/c");
 ("remove following .. and .",   "/a/b/d/.././c",            "/a/b/c");
 ("remove multiple ..",          "/a/b/../d/../b/c",         "/a/b/c");
 ("remove multiple .",           "/a/./././b/./c",           "/a/b/c");
 ("remove multiple . and ..",    "/a/../a/./b/../c/../b/./c","/a/b/c")
];

(* Create path *)
List.iter TestUnix.make_path
[
 ("identity", ["/a";"b";"/c/d"], "/a:b:/c/d")
];

(* Convert to absolute *)
List.iter TestUnix.make_absolute
[
 ("identity",  "/a/b/c", ".",    "/a/b/c");
 ("simple v1", "/a/b/c", "./d",  "/a/b/c/d");
 ("simple v2", "/a/b/c", "../d", "/a/b/d")
];

(* Convert to relative *)
List.iter TestUnix.make_relative 
[
 ("identity",  "/a/b/c", "/a/b/c", "");
 ("simple v1", "/a/b/c", "/a/b/d", "../d")
];

(**********************)
(* Win32 SysPath test *)
(**********************)

let test_path = 
[
 ("Root",   "c:/");
 ("Simple v1", "c:/a/b");
 ("Simple v2", "c:/a/b/c/");
 ("Simple ..", "c:/a/../b/c");
 ("Multiple ..", "c:/a/../b/../c")
]
in
(* Is_valid *)
List.iter TestWin32.valid test_path
;

(* Identity *)
List.iter TestWin32.identity test_path
;

(* Reduce path *)
List.iter TestWin32.reduce
[
 ("identity",                    "c:/a/b/c",                   "c:/a/b/c");
 ("remove trailer",              "c:/a/b/c/",                  "c:/a/b/c");
 ("remove last ..",              "c:/a/b/c/d/..",              "c:/a/b/c");
 ("remove last .",               "c:/a/b/c/.",                 "c:/a/b/c");
 ("remove inside ..",            "c:/a/d/../b/c",              "c:/a/b/c");
 ("remove inside .",             "c:/a/./b/c",                 "c:/a/b/c");
 ("remove last . and ..",        "c:/a/b/c/d/./..",            "c:/a/b/c");
 ("remove last .. and .",        "c:/a/b/c/d/../.",            "c:/a/b/c");
 ("remove following . and ..",   "c:/a/b/d/./../c",            "c:/a/b/c");
 ("remove following .. and .",   "c:/a/b/d/.././c",            "c:/a/b/c");
 ("remove multiple ..",          "c:/a/b/../d/../b/c",         "c:/a/b/c");
 ("remove multiple .",           "c:/a/./././b/./c",           "c:/a/b/c");
 ("remove multiple . and ..",    "c:/a/../a/./b/../c/../b/./c","c:/a/b/c")
];

(* Create path *)
List.iter TestWin32.make_path
[
 ("identity", ["c:/a";"b";"c:/c\\d"], "c:/a:b:c:/c\\d")
];

(* Convert to absolute *)
List.iter TestWin32.make_absolute
[
 ("identity",  "c:/a\\b\\c", ".",     "c:/a\\b\\c");
 ("simple v1", "c:/a\\b\\c", ".\\d",  "c:/a\\b\\c\\d");
 ("simple v2", "c:/a\\b\\c", "..\\d", "c:/a\\b\\d")
];

(* Convert to relative *)
List.iter TestWin32.make_relative 
[
 ("identity",  "c:/a\\b\\c", "c:/a\\b\\c", "");
 ("simple v1", "c:/a\\b\\c", "c:/a\\b\\d", "..\\d")
];

(**********************)
(* MacOS SysPath test *)
(**********************)

(* Is_valid *)
List.iter TestMacOS.valid
[
 ("Root", "a:");
];

(* Identity *)
List.iter TestMacOS.identity
[
 ("Root", "a:");
];

(* Reduce path *)
List.iter TestMacOS.reduce
[
 ("identity",           "root:a:b:c",      "root:a:b:c");
 ("remove trailer",     "root:a:b:c:",     "root:a:b:c");
 ("remove last ..",     "root:a:b:c:d::",  "root:a:b:c");
 ("remove inside ..",   "root:a:d::b:c",   "root:a:b:c");
 ("remove last ..",     "root:a:b:c:d::",  "root:a:b:c");
 ("remove following ..","root:a:b:d::c",   "root:a:b:c");
 ("remove multiple ..", "root:a:b::d::b:c","root:a:b:c");
];

(* Create path *)
List.iter TestMacOS.make_path
[
 ("identity", [":a";"b";":c:d"],":a;b;:c:d")
];

(* Convert to absolute *)
List.iter TestMacOS.make_absolute
[
 ("identity",  "root:a:b:c", ":",   "root:a:b:c");
 ("simple v1", "root:a:b:c", ":d",  "root:a:b:c:d");
 ("simple v2", "root:a:b:c", "::d", "root:a:b:d")
];

(* Convert to relative *)
List.iter TestMacOS.make_relative 
[
 ("identity",  "root:a:b:c", "root:a:b:c", "");
 ("simple v1", "root:a:b:c", "root:a:b:d", "::d")
];

(*
(***********************)
(* Cygwin SysPath test *)
(***********************)

(* Reduce path *)
List.iter TestCygwin.reduce
[
 ("identity",                    "/a/b/c",                   "/a/b/c");
 ("remove trailer",              "/a/b/c/",                  "/a/b/c");
 ("remove last ..",              "/a/b/c/d/..",              "/a/b/c");
 ("remove last .",               "/a/b/c/.",                 "/a/b/c");
 ("remove inside ..",            "/a/d/../b/c",              "/a/b/c");
 ("remove inside .",             "/a/./b/c",                 "/a/b/c");
 ("remove last . and ..",        "/a/b/c/d/./..",            "/a/b/c");
 ("remove last .. and .",        "/a/b/c/d/../.",            "/a/b/c");
 ("remove following . and ..",   "/a/b/d/./../c",            "/a/b/c");
 ("remove following .. and .",   "/a/b/d/.././c",            "/a/b/c");
 ("remove multiple ..",          "/a/b/../d/../b/c",         "/a/b/c");
 ("remove multiple .",           "/a/./././b/./c",           "/a/b/c");
 ("remove multiple . and ..",    "/a/../a/./b/../c/../b/./c","/a/b/c")
];

(* Create path *)
List.iter TestCygwin.make_path
[
 ("identity", ["/a";"b";"/c/d"])
];

(* Convert to absolute *)
List.iter TestCygwin.make_absolute
[
 ("identity",  "/a/b/c", ".",    "/a/b/c");
 ("simple v1", "/a/b/c", "./d",  "/a/b/c/d");
 ("simple v2", "/a/b/c", "../d", "/a/b/d")
];

(* Convert to relative *)
List.iter TestCygwin.make_relative 
[
 ("identity",  "/a/b/c", "/a/b/c", "");
 ("simple v1", "/a/b/c", "/a/b/d", "../d")
];
*)
(****************)
(* SysUtil test *)
(****************)

(* Test to be performed *)

let dir_test = "test-util"
in
let test_test (stest,expr,file,res) =
	expect_pass ~desc:("Test "^stest^" on "^file) 
	~body:( fun () ->
		expect_true (res = (test expr file))
	)
in
let ask_user file = 
	let answer =
		print_string ("Delete file : "^file^" (y/n) ? ");
		read_line ()
	in
	answer = "y"
in


(*
if !error_syspath then
  raise CannotContinueTest
else
  begin
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
  		SysUtil.touch (implode_string [dir_test;"doesntexist";"essai0"]);
  		Pass
  	with _ ->
  		XFail
  );
  
  expect_pass ~desc:"Touch in existing dir v1"
  ~body:( fun () ->
  	touch (implode_string [dir_test;"essai0"]);
  	expect_true (test Exists (implode_string [dir_test;"essai0"]))
  );
  
  expect_pass ~desc:"Touch in existing dir with no create"
  ~body:( fun () ->
  	touch ~create:false (implode_string [dir_test;"essai2"]);
  	expect_true (not (test Exists (implode_string [dir_test;"essai2"])))
  );
  
  expect_pass ~desc:"Touch in existing dir v2"
  ~body:( fun () ->
  	touch (implode_string [dir_test;"essai1"]);
  	expect_true (test Exists (implode_string [dir_test;"essai1"]))
  );
  
  (* Too fast 
  expect_pass ~desc:"Touch precedence"
  ~body:( fun () ->
  	expect_true (test (Is_newer_than(concat dir_test "essai0",concat dir_test "essai1")) dir_test )
  );
  *)
  
  expect_pass ~desc:"Mkdir simple v1"
  ~body:( fun () ->
  	mkdir (implode_string [dir_test;"essai2"]);
  	expect_true (test Is_dir (implode_string [ dir_test ; "essai2" ]))
  );
  
  expect_pass ~desc:"Mkdir simple && mode 700"
  ~body:( fun () ->
  	mkdir ~mode:0o0700 (implode_string [ dir_test ; "essai3" ]);
  	expect_true (test Is_dir (implode_string [ dir_test ; "essai3" ]))
  );
  
  Fort.test ~desc:"Mkdir recurse v1"
  ~body:(fun () ->
  	try
  		mkdir (implode_string [dir_test; "essai4"; "essai5"]);
  		Pass
  	with MkdirMissingComponentPath ->
  		XFail
  );
  
  Fort.test ~desc:"Mkdir && already exist v1"
  ~body:(fun () ->
  	try
  		mkdir (implode_string [dir_test; "essai0"]);
  		Pass
  	with MkdirDirnameAlreadyUsed ->
  		XFail
  );
  
  expect_pass ~desc:"Mkdir recurse v2"
  ~body:(fun () ->
  	mkdir ~parent:true (implode_string [dir_test; "essai4"; "essai5"]);
  	expect_true (test Is_dir (implode_string [dir_test; "essai4"; "essai5"]))
  );
  
  expect_pass ~desc:"Find v1"
  ~body:(fun () ->
  	let lst = find True dir_test 
  	in
  	expect_equal_list_string 		
  		(
  			(implode_string [ dir_test ; "essai4" ; "essai5" ]) :: 
  			(List.map (fun x -> implode_string [ dir_test ; x ] ) [
  				"essai4";
  				"essai3";
  				"essai2";
  				"essai1";
  				"essai0"
  			])
  		)
  		lst 
  
  );
  
  expect_pass ~desc:"Find v2"
  ~body:(fun () ->
  	let lst = find Is_dir dir_test
  	in
  	expect_equal_list_string 
  		(
  		(implode_string [ dir_test ; "essai4" ; "essai5" ]) ::
  			(List.map (fun x -> implode_string [ dir_test ; x ]) [
  				"essai4";
  				"essai3";
  				"essai2"
  			])
  		)
  		lst
  );
  			
  expect_pass ~desc:"Find v3"
  ~body:(fun () ->
  	let lst = find Is_file dir_test
  	in
  	expect_equal_list_string 
  		(List.map (fun x -> implode_string [ dir_test ; x ]) [
  			"essai1";
  			"essai0"
  		])
  		lst
  );
  
  expect_pass ~desc:"Cp v1"
  ~body:(fun () ->
  	cp (implode_string [dir_test ; "essai0"]) (implode_string [ dir_test ; "essai6" ]);
  	expect_true (test (Exists) (implode_string [ dir_test ; "essai6" ]))
  );
  
  expect_pass ~desc:"Cp v2"
  ~body:(fun () ->
  	cp (implode_string [dir_test ; "essai0"]) (implode_string [ dir_test ; "essai4" ]);
  	expect_true (test (Exists) (implode_string [ dir_test ; "essai4" ; "essai0" ]))
  );
  
  expect_pass ~desc:"Rm v1"
  ~body:(fun () ->
  	rm ~force:(Ask ask_user) (implode_string [dir_test  ; "essai2"]);
  	expect_true (test (Not Exists) (implode_string [ dir_test ; "essai2" ]))
  );
  
  Fort.test ~desc:"Rm v2"
  ~body:(fun () ->
  	try 
  		rm ~force:(Ask ask_user) (implode_string [ dir_test ; "essai4" ]);
  		Pass
  	with RmDirNotEmpty ->
  		XFail
  );
  
  expect_pass ~desc:"Rm v3"
  ~body:(fun () ->
  	rm ~force:(Ask ask_user) ~recurse:true dir_test;
  	expect_true (test (Not Exists) dir_test)
  )
  end
;*)

();;
