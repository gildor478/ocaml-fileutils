(**************************************************************************)
(*   Ocaml-fileutils                                                      *)
(*                                                                        *)
(*   Copyright (C) 2003, 2004 Sylvain Le Gall <sylvain@le-gall.net>       *)
(*                                                                        *)
(*   This program is free software; you can redistribute it and/or        *)
(*   modify it under the terms of the GNU Library General Public          *)
(*   License as published by the Free Software Foundation; either         *)
(*   version 2 of the License, or any later version ; with the OCaml      *)
(*   static compilation exception.                                        *)
(*                                                                        *)
(*   This program is distributed in the hope that it will be useful,      *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 *)
(*   See the LICENCE file for more details.                               *)
(*                                                                        *)
(*   You should have received a copy of the GNU General Public License    *)
(*   along with this program; if not, write to the Free Software          *)
(*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA             *)
(*   02111-1307  USA                                                      *)
(*                                                                        *)
(*   Contact: sylvain@le-gall.net                                         *)
(*                                                                        *)
(**************************************************************************)

open Fort;;
open FilePath;;
open DefaultPath;;
open FileUtil;;
open StrUtil;;


exception CannotContinueTest;;

let expect_equal_string s1 s2 = 
	expect_equal ~printer:(fun x -> x) s1 s2
;;	

let expect_equal_list_string lst1 lst2= 
	expect_equal ~printer:(fun lst -> String.concat ";" lst) lst1 lst2
;;	

let expect_equal_bool b1 b2= 
	expect_equal ~printer:(string_of_bool) b1 b2
;;

module Test = 
functor ( OsPath : PATH_STRING_SPECIFICATION ) ->
struct
	let os_string = ref ""
	
	let reduce (exp,res) =
		expect_pass ~desc:((!os_string)^" : reduce \"" ^ exp ^ "\"")
		~body:( fun () ->
			expect_equal_string res (OsPath.reduce exp)
		)
		
	let make_path (exp,res) =
		expect_pass ~desc:((!os_string)^" : make_path [ " ^ 
			(String.concat " ; " exp) ^ " ]" )
		~body:( fun () ->
			expect_equal_string res (OsPath.string_of_path exp)
		)
		
	let make_absolute (base,rela,res) =
		expect_pass ~desc:((!os_string)^" : make_absolute \"" ^ base ^ "\" \"" ^ rela ^ "\"" )
		~body:( fun () ->
			expect_equal_string res (OsPath.make_absolute base rela)
		)
		
	let make_relative (base,abs,res) =
		expect_pass ~desc:((!os_string)^" : make_relative \"" ^ base ^ "\" \"" ^ abs ^ "\"" )
		~body:( fun () ->
			expect_equal_string res (OsPath.make_relative base abs)
		)
		
	let valid (exp) =
		expect_pass ~desc:((!os_string)^" : is_valid \"" ^ exp ^ "\"" )
		~body:( fun () ->
			expect_true (OsPath.is_valid exp)
		)
		
	let identity (exp) = 
		expect_pass ~desc:((!os_string)^" : identity \""^exp^ "\"")
		~body:( fun () ->
			expect_equal_string exp (OsPath.identity exp)
		)

	let extension (filename,basename,extension) = 
		expect_pass ~desc:((!os_string)^" : chop_extension \""^filename^"\"")
		~body:( fun () ->
			expect_equal_string (OsPath.chop_extension filename) basename
		);
		expect_pass ~desc:((!os_string)^" : get_extension \""^filename^"\"")
		~body:( fun () ->
			expect_equal_string 
			(OsPath.string_of_extension (OsPath.get_extension filename))
			extension
		);
		expect_pass ~desc:((!os_string)^" : check_extension \""^filename^"\"")
		~body:( fun () ->
			expect_true (OsPath.check_extension filename 
			(OsPath.extension_of_string extension))
		);
		expect_pass ~desc:((!os_string)^" : check_extension ( false ) \""^filename^"\"")
		~body:( fun () ->
			expect_true (not (OsPath.check_extension filename 
			(OsPath.extension_of_string "dummy")))
		)
end
;;

module TestFilename = 
struct
	let is_relative (exp) =
		expect_pass ~desc:("Test default : is_relative "^exp)
		~body:( fun () ->
			expect_true ((is_relative exp) = (Filename.is_relative exp))
		)

	let is_implicit (exp) =
		expect_pass ~desc:("Test default : is_implicit "^exp)
		~body:( fun () ->
			expect_true ((is_implicit exp) = (Filename.is_implicit exp))
		)
	
	let concat (exp1,exp2) =
		expect_pass ~desc:("Test default : concat "^exp1^" "^exp2)
		~body:( fun () ->
			expect_true ((concat exp1 exp2) = (Filename.concat exp1 exp2))
		)

	let basename (exp) =
		expect_pass ~desc:("Test default : basename "^exp)
		~body:( fun () ->
			expect_true ((basename exp) = (Filename.basename exp))
		)

	let dirname (exp) =
		expect_pass ~desc:("Test default : dirname "^exp)
		~body:( fun () ->
			expect_true ((dirname exp) = (Filename.dirname exp))
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
module TestCygwin = Test(CygwinPath)
;;
TestCygwin.os_string := "Cygwin"
;;

(*********************)
(* Unix FilePath test*)
(*********************)
let test_path = 
[
 ("/");
 ("/a/b");
 ("/a/b/c/");
 ("/a/../b/c");
 ("/a/../b/../c");
 ("a/b/c/");
 ("../a/b");
 ("");
 (".");
 ("./");
 ("..");
 ("../")
]
in
(* Is_valid *)
List.iter TestUnix.valid test_path
;

(* Identity *)
List.iter TestUnix.identity test_path
;

(* Reduce path *)
List.iter TestUnix.reduce
[
 ("/a/b/c",                   "/a/b/c");
 ("/a/b/c/",                  "/a/b/c");
 ("/a/b/c/d/..",              "/a/b/c");
 ("/a/b/c/.",                 "/a/b/c");
 ("/a/d/../b/c",              "/a/b/c");
 ("/a/./b/c",                 "/a/b/c");
 ("/a/b/c/d/./..",            "/a/b/c");
 ("/a/b/c/d/../.",            "/a/b/c");
 ("/a/b/d/./../c",            "/a/b/c");
 ("/a/b/d/.././c",            "/a/b/c");
 ("/a/b/../d/../b/c",         "/a/b/c");
 ("/a/./././b/./c",           "/a/b/c");
 ("/a/../a/./b/../c/../b/./c","/a/b/c");
 ("/a/../..",                 "/");
 ("./d/../a/b/c",             "a/b/c");
 ("a/b/c/../../../",          "");
 ("",                         "");
 (".",                        "");
 ("./",                       "");
 ("..",                       "..");
 ("../",                      "..");
];

(* Create path *)
List.iter TestUnix.make_path
[
 (["/a";"b";"/c/d"], "/a:b:/c/d");
 ([],                "");
];

(* Convert to absolute *)
List.iter TestUnix.make_absolute
[
 ("/a/b/c", ".",    "/a/b/c");
 ("/a/b/c", "./d",  "/a/b/c/d");
 ("/a/b/c", "../d", "/a/b/d");
 ("/a/b/c", "",     "/a/b/c");
 ("/a/b/c", ".",    "/a/b/c");
 ("/a/b/c", "./",   "/a/b/c");
 ("/a/b/c", "..",   "/a/b");
 ("/a/b/c", "../",  "/a/b")
];

(* Convert to relative *)
List.iter TestUnix.make_relative 
[
 ("/a/b/c", "/a/b/c", "");
 ("/a/b/c", "/a/b/d", "../d")
];


(* Check extension *)
List.iter TestUnix.extension
[
 ("/a/b/c.d",   "/a/b/c",   "d");
 ("/a/b.c/d.e", "/a/b.c/d", "e");
 ("a.",         "a",        "");
];

(**********************)
(* Win32 FilePath test*)
(**********************)

let test_path = 
[
 ("c:/");
 ("c:/a\\b");
 ("c:/a\\b\\c\\");
 ("c:/a\\..\\b\\c");
 ("c:/a\\..\\b\\..\\c");
 ("a\\b\\c\\");
 ("..\\a\\b");
 ("");
 (".");
 (".\\");
 ("..");
 ("..\\")
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
 ("c:/a\\b\\c",                           "c:/a\\b\\c");
 ("c:/a\\b\\c\\",                         "c:/a\\b\\c");
 ("c:/a\\b\\c\\d\\..",                    "c:/a\\b\\c");
 ("c:/a\\b\\c\\.",                        "c:/a\\b\\c");
 ("c:/a\\d\\..\\b\\c",                    "c:/a\\b\\c");
 ("c:/a\\.\\b\\c",                        "c:/a\\b\\c");
 ("c:/a\\b\\c\\d\\.\\..",                 "c:/a\\b\\c");
 ("c:/a\\b\\c\\d\\..\\.",                 "c:/a\\b\\c");
 ("c:/a\\b\\d\\.\\..\\c",                 "c:/a\\b\\c");
 ("c:/a\\b\\d\\..\\.\\c",                 "c:/a\\b\\c");
 ("c:/a\\b\\..\\d\\..\\b\\c",             "c:/a\\b\\c");
 ("c:/a\\.\\.\\.\\b\\.\\c",               "c:/a\\b\\c");
 ("c:/a\\..\\a\\.\\b\\..\\c\\..\\b\\.\\c","c:/a\\b\\c");
 ("a\\..\\b",                             "b");
 ("",                                     "");
 (".",                                    "");
 (".\\",                                  "");
 ("..",                                   "..");
 ("..\\",                                 "..");
];

(* Create path *)
List.iter TestWin32.make_path
[
 (["c:/a";"b";"c:/c\\d"], "c:/a;b;c:/c\\d");
 ([],                     "");
];

(* Convert to absolute *)
List.iter TestWin32.make_absolute
[
 ("c:/a\\b\\c", ".",     "c:/a\\b\\c");
 ("c:/a\\b\\c", ".\\d",  "c:/a\\b\\c\\d");
 ("c:/a\\b\\c", "..\\d", "c:/a\\b\\d");
 ("c:/a\\b\\c", "",      "c:/a\\b\\c");
 ("c:/a\\b\\c", ".",     "c:/a\\b\\c");
 ("c:/a\\b\\c", ".\\",   "c:/a\\b\\c");
 ("c:/a\\b\\c", "..",    "c:/a\\b");
 ("c:/a\\b\\c", "..\\",  "c:/a\\b");
];

(* Convert to relative *)
List.iter TestWin32.make_relative 
[
 ("c:/a\\b\\c", "c:/a\\b\\c", "");
 ("c:/a\\b\\c", "c:/a\\b\\d", "..\\d")
];

(* Check extension *)
List.iter TestWin32.extension
[
 ("c:/a\\b\\c.d",   "c:/a\\b\\c",   "d");
 ("c:/a\\b.c\\d.e", "c:/a\\b.c\\d", "e");
 ("a.",         "a",        "");
];


(**********************)
(* MacOS FilePath test*)
(**********************)

let test_path = 
[
 ("a:");
 ("a:::");
 (":a:b:c");
 ("");
 (":");
 ("::");
]
in

(* Is_valid *)
List.iter TestMacOS.valid test_path;

(* Identity *)
List.iter TestMacOS.identity test_path;

(* Reduce path *)
List.iter TestMacOS.reduce
[
 ("root:a:b:c",      "root:a:b:c");
 ("root:a:b:c:",     "root:a:b:c");
 ("root:a:b:c:d::",  "root:a:b:c");
 ("root:a:d::b:c",   "root:a:b:c");
 ("root:a:b:c:d::",  "root:a:b:c");
 ("root:a:b:d::c",   "root:a:b:c");
 ("root:a:b::d::b:c","root:a:b:c");
 ("",                "");
 (":",               "");
 ("::",              "::");
];

(* Create path *)
List.iter TestMacOS.make_path
[
 ([":a";"b";":c:d"],":a;b;:c:d");
 ([],               "");
];

(* Convert to absolute *)
List.iter TestMacOS.make_absolute
[
 ("root:a:b:c", ":",   "root:a:b:c");
 ("root:a:b:c", ":d",  "root:a:b:c:d");
 ("root:a:b:c", "::d", "root:a:b:d");
 ("root:a:b:c", "",    "root:a:b:c");
 ("root:a:b:c", ":",   "root:a:b:c");
 ("root:a:b:c", "::",  "root:a:b");
];

(* Convert to relative *)
List.iter TestMacOS.make_relative 
[
 ("root:a:b:c", "root:a:b:c", "");
 ("root:a:b:c", "root:a:b:d", "::d")
];

(* Check extension *)
List.iter TestWin32.extension
[
 ("root:a:b:c.d",   "root:a:b:c",   "d");
 ("root:a:b.c:d.e", "root:a:b.c:d", "e");
 ("a.",         "a",        "");
];

(***********************)
(* Cygwin FilePath test*)
(***********************)

(* Reduce path *)
List.iter TestCygwin.reduce
[
 ("c:/a/b/c",                   "c:/a/b/c");
 ("c:/a/b/c/",                  "c:/a/b/c");
 ("c:/a/b/c/d/..",              "c:/a/b/c");
 ("c:/a/b/c/.",                 "c:/a/b/c");
 ("c:/a/d/../b/c",              "c:/a/b/c");
 ("c:/a/./b/c",                 "c:/a/b/c");
 ("c:/a/b/c/d/./..",            "c:/a/b/c");
 ("c:/a/b/c/d/../.",            "c:/a/b/c");
 ("c:/a/b/d/./../c",            "c:/a/b/c");
 ("c:/a/b/d/.././c",            "c:/a/b/c");
 ("c:/a/b/../d/../b/c",         "c:/a/b/c");
 ("c:/a/./././b/./c",           "c:/a/b/c");
 ("c:/a/../a/./b/../c/../b/./c","c:/a/b/c")
];

(* Create path *)
List.iter TestCygwin.make_path
[
 (["c:/a";"c:b";"c:/c/d"], "c:/a;c:b;c:/c/d")
];

(* Convert to absolute *)
List.iter TestCygwin.make_absolute
[
 ("c:/a/b/c", ".",    "c:/a/b/c");
 ("c:/a/b/c", "./d",  "c:/a/b/c/d");
 ("c:/a/b/c", "../d", "c:/a/b/d")
];

(* Convert to relative *)
List.iter TestCygwin.make_relative 
[
 ("c:/a/b/c", "c:/a/b/c", "");
 ("c:/a/b/c", "c:/a/b/d", "../d")
];

(* Check extension *)
List.iter TestWin32.extension
[
 ("c:/a/b/c.d",   "c:/a/b/c",   "d");
 ("c:a/b.c/d.e",  "c:a/b.c/d",  "e");
 ("a.",           "a",        "");
];

(****************)
(* FileUtil test*)
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

let answer = 
  print_string ("Continue test (y/n) ?"^
  " [ You should answer no if some test before has failed ]");
  read_line ()
in
  if  answer <> "y" then
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
   ("Is_newer_than",                (Is_newer_than("test.ml")),dir_test,true);
   ("Is_older_than",                (Is_older_than("test.ml")),dir_test,false);
  ];
  
  Fort.test ~desc:"Touch in not existing subdir"
  ~body:( fun () ->
  	try 
  		FileUtil.StrUtil.touch (make_filename [dir_test;"doesntexist";"essai0"]);
  		Pass
  	with _ ->
  		XFail
  );
  
  expect_pass ~desc:"Touch in existing dir v1"
  ~body:( fun () ->
  	touch (make_filename [dir_test;"essai0"]);
  	expect_true (test Exists (make_filename [dir_test;"essai0"]))
  );
  
  expect_pass ~desc:"Touch in existing dir with no create"
  ~body:( fun () ->
  	touch ~create:false (make_filename [dir_test;"essai2"]);
  	expect_true (not (test Exists (make_filename [dir_test;"essai2"])))
  );
  
  expect_pass ~desc:"Touch in existing dir v2"
  ~body:( fun () ->
  	touch (make_filename [dir_test;"essai1"]);
  	expect_true (test Exists (make_filename [dir_test;"essai1"]))
  );
  
  (* Too fast 
  expect_pass ~desc:"Touch precedence"
  ~body:( fun () ->
  	expect_true (test (Is_newer_than(concat dir_test "essai0",concat dir_test "essai1")) dir_test )
  );*)
  
  expect_pass ~desc:"Mkdir simple v1"
  ~body:( fun () ->
  	mkdir (make_filename [dir_test;"essai2"]);
  	expect_true (test Is_dir (make_filename [ dir_test ; "essai2" ]))
  );
  
  expect_pass ~desc:"Mkdir simple && mode 700"
  ~body:( fun () ->
  	mkdir ~mode:0o0700 (make_filename [ dir_test ; "essai3" ]);
  	expect_true (test Is_dir (make_filename [ dir_test ; "essai3" ]))
  );
  
  Fort.test ~desc:"Mkdir recurse v1"
  ~body:(fun () ->
  	try
  		mkdir (make_filename [dir_test; "essai4"; "essai5"]);
  		Pass
  	with MkdirMissingComponentPath _ ->
  		XFail
  );
  
  Fort.test ~desc:"Mkdir && already exist v1"
  ~body:(fun () ->
  	try
  		mkdir (make_filename [dir_test; "essai0"]);
  		Pass
  	with MkdirDirnameAlreadyUsed _ ->
  		XFail
  );
  
  expect_pass ~desc:"Mkdir recurse v2"
  ~body:(fun () ->
  	mkdir ~parent:true (make_filename [dir_test; "essai4"; "essai5"]);
  	expect_true (test Is_dir (make_filename [dir_test; "essai4"; "essai5"]))
  );
  
  expect_pass ~desc:"Find v1"
  ~body:(fun () ->
  	let lst = find True dir_test ( fun lst fln -> fln :: lst ) [] 
  	in
  	expect_equal_list_string 		
  		(
  			(make_filename [ dir_test ; "essai4" ; "essai5" ]) :: 
  			(List.map (fun x -> make_filename [ dir_test ; x ] ) [
  				"essai0";
  				"essai1";
  				"essai2";
  				"essai3";
  				"essai4"
  			])
  		)
  		lst 
  
  );
  
  expect_pass ~desc:"Find v2"
  ~body:(fun () ->
  	let lst = find Is_dir dir_test ( fun lst fln -> fln :: lst ) []
  	in
  	expect_equal_list_string 
  		(
  		(make_filename [ dir_test ; "essai4" ; "essai5" ]) ::
  			(List.map (fun x -> make_filename [ dir_test ; x ]) [
  				"essai2";
  				"essai3";
  				"essai4";
  			])
  		)
  		lst
  );
  			
  expect_pass ~desc:"Find v3"
  ~body:(fun () ->
  	let lst = find Is_file dir_test ( fun lst fln -> fln :: lst ) [] 
  	in
  	expect_equal_list_string 
  		(List.map (fun x -> make_filename [ dir_test ; x ]) [
  			"essai0";
  			"essai1"
  		])
  		lst
  );
  
  expect_pass ~desc:"Cp v1"
  ~body:(fun () ->
  	cp [(make_filename [dir_test ; "essai0"])] (make_filename [ dir_test ; "essai6" ]);
  	expect_true (test (Exists) (make_filename [ dir_test ; "essai6" ]))
  );
  
  expect_pass ~desc:"Cp v2"
  ~body:(fun () ->
  	cp [(make_filename [dir_test ; "essai0"])] (make_filename [ dir_test ; "essai4" ]);
  	expect_true (test (Exists) (make_filename [ dir_test ; "essai4" ; "essai0" ]))
  );
  
  expect_pass ~desc:"Rm v1"
  ~body:(fun () ->
  	rm ~force:(Ask ask_user) [(make_filename [dir_test  ; "essai2"])];
  	expect_true (test (Not Exists) (make_filename [ dir_test ; "essai2" ]))
  );
  
  Fort.test ~desc:"Rm v2"
  ~body:(fun () ->
  	try 
  		rm ~force:(Ask ask_user) [(make_filename [ dir_test ; "essai4" ])];
  		Pass
  	with RmDirNotEmpty _ ->
  		XFail
  );
  
  expect_pass ~desc:"Rm v3"
  ~body:(fun () ->
  	rm ~force:(Ask ask_user) ~recurse:true [dir_test];
  	expect_true (test (Not Exists) dir_test)
  )
  end
;

();;
