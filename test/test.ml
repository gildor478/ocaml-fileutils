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

open OUnit;;
open FilePath;;
open DefaultPath;;
open FileUtil;;
open StrUtil;;

module SetFilename = Set.Make (struct
    type t = FilePath.DefaultPath.filename
    let compare = FilePath.DefaultPath.compare
end)
;;

let assert_equal_string ~msg = 
  assert_equal ~printer:(fun x -> x) ~msg:msg
;;  

let expect_equal_list_string lst1 lst2= 
  assert_equal ~printer:(fun lst -> String.concat ";" lst) lst1 lst2
;;  

let expect_equal_bool b1 b2= 
  assert_equal ~printer:(string_of_bool) b1 b2
;;

module Test = 
functor ( OsPath : PATH_STRING_SPECIFICATION ) ->
struct
  let os_string = ref ""
  
  let test_label s value = (!os_string)^" : "^s^" \""^value^"\""

  let test_label_list s lst = test_label s ("[ "^(String.concat ";" lst)^"]")

  let test_label_pair s (a,b) = test_label s (a^"\" \""^b)

  let test_name s = (s)
  
  let reduce (exp,res) =
    (test_name "reduce") >:: ( fun () ->
      assert_equal_string ~msg:(test_label "reduce" exp)
      res (OsPath.reduce exp)
    )
    
  let make_path (exp,res) =
    (test_name "make_path") >:: ( fun () ->
      assert_equal_string ~msg:(test_label_list "make_path" exp)
      res (OsPath.string_of_path exp)
    )
    
  let make_absolute (base,rela,res) =
    (test_name "make_absolute") >:: ( fun () ->
      assert_equal_string ~msg:(test_label_pair "make_absolute" (base,rela))
      res (OsPath.make_absolute base rela)
    )
    
  let make_relative (base,abs,res) =
    (test_name "make_relative") >:: ( fun () ->
      assert_equal_string ~msg:(test_label_pair "make_relative" (base,abs))
      res (OsPath.make_relative base abs)
    )
    
  let valid (exp) =
    (test_name "valid") >:: ( fun () ->
      assert_bool (test_label "is_valid" exp)
      (OsPath.is_valid exp)
    )
    
  let identity (exp) = 
    (test_name "identity") >:: ( fun () ->
      assert_equal_string ~msg:(test_label "identity" exp)
      exp (OsPath.identity exp)
    )

  let extension (filename,basename,extension) = 
    (test_name "extension") >:: ( fun () ->
      assert_equal_string ~msg:(test_label "chop_extension" filename)
      (OsPath.chop_extension filename) basename;
      
      assert_equal_string ~msg:(test_label "get_extension" filename)
      (OsPath.string_of_extension (OsPath.get_extension filename)) extension;
      
      assert_bool (test_label "check_extension" filename)
      (OsPath.check_extension filename (OsPath.extension_of_string extension));
      
      assert_bool (test_label "check_extension ( false ) " filename)
      (not (OsPath.check_extension filename (OsPath.extension_of_string "dummy")))
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
let test_unix = 
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
  "Unix FilePath" >::: (
    (* Is_valid *)
    (
      List.map TestUnix.valid test_path
    )
    
    (* Identity *)
    @ (
      List.map TestUnix.identity test_path
    )

    (* Reduce path *)
    @ (
      List.map TestUnix.reduce
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
      ]
    )

    (* Create path *)
    @ (
      List.map TestUnix.make_path
      [
       (["/a";"b";"/c/d"], "/a:b:/c/d");
       ([],                "");
      ]
    )

    (* Convert to absolute *)
    @ ( 
      List.map TestUnix.make_absolute
      [
       ("/a/b/c", ".",    "/a/b/c");
       ("/a/b/c", "./d",  "/a/b/c/d");
       ("/a/b/c", "../d", "/a/b/d");
       ("/a/b/c", "",     "/a/b/c");
       ("/a/b/c", ".",    "/a/b/c");
       ("/a/b/c", "./",   "/a/b/c");
       ("/a/b/c", "..",   "/a/b");
       ("/a/b/c", "../",  "/a/b")
      ]
    )

    (* Convert to relative *)
    @ (
      List.map TestUnix.make_relative 
      [
       ("/a/b/c", "/a/b/c", "");
       ("/a/b/c", "/a/b/d", "../d")
      ]
    )

    (* Check extension *)
    @ (
      List.map TestUnix.extension
      [
       ("/a/b/c.d",   "/a/b/c",   "d");
       ("/a/b.c/d.e", "/a/b.c/d", "e");
       ("a.",         "a",        "");
      ]
    )
  )
in

(**********************)
(* Win32 FilePath test*)
(**********************)

let test_win32 = 
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
  "Win32 FilePath" >:::
    (
      (* Is_valid *)
      (
        List.map TestWin32.valid test_path
      )

      (* Identity *)
      @ (
        List.map TestWin32.identity test_path
      )

      (* Reduce path *)
      @ (
        List.map TestWin32.reduce
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
        ]
      )

      (* Create path *)
      @ ( 
        List.map TestWin32.make_path
        [
         (["c:/a";"b";"c:/c\\d"], "c:/a;b;c:/c\\d");
         ([],                     "");
        ]
      )

      (* Convert to absolute *)
      @ (
        List.map TestWin32.make_absolute
        [
         ("c:/a\\b\\c", ".",     "c:/a\\b\\c");
         ("c:/a\\b\\c", ".\\d",  "c:/a\\b\\c\\d");
         ("c:/a\\b\\c", "..\\d", "c:/a\\b\\d");
         ("c:/a\\b\\c", "",      "c:/a\\b\\c");
         ("c:/a\\b\\c", ".",     "c:/a\\b\\c");
         ("c:/a\\b\\c", ".\\",   "c:/a\\b\\c");
         ("c:/a\\b\\c", "..",    "c:/a\\b");
         ("c:/a\\b\\c", "..\\",  "c:/a\\b");
        ]
      )

      (* Convert to relative *)
      @ (
        List.map TestWin32.make_relative 
        [
         ("c:/a\\b\\c", "c:/a\\b\\c", "");
         ("c:/a\\b\\c", "c:/a\\b\\d", "..\\d")
        ]
      )

      (* Check extension *)
      @ (
        List.map TestWin32.extension
        [
         ("c:/a\\b\\c.d",   "c:/a\\b\\c",   "d");
         ("c:/a\\b.c\\d.e", "c:/a\\b.c\\d", "e");
         ("a.",         "a",        "");
        ]
      )
    )
in


(**********************)
(* MacOS FilePath test*)
(**********************)

let test_macos = 
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
  "MacOS FilePath" >:::
    (
      (* Is_valid *)
      (
        List.map TestMacOS.valid test_path
      )

      (* Identity *)
      @ ( 
        List.map TestMacOS.identity test_path
      )

      (* Reduce path *)
      @ (
        List.map TestMacOS.reduce
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
        ]
      )

      (* Create path *)
      @ (
        List.map TestMacOS.make_path
        [
         ([":a";"b";":c:d"],":a;b;:c:d");
         ([],               "");
        ]
      )

      (* Convert to absolute *)
      @ (
        List.map TestMacOS.make_absolute
        [
         ("root:a:b:c", ":",   "root:a:b:c");
         ("root:a:b:c", ":d",  "root:a:b:c:d");
         ("root:a:b:c", "::d", "root:a:b:d");
         ("root:a:b:c", "",    "root:a:b:c");
         ("root:a:b:c", ":",   "root:a:b:c");
         ("root:a:b:c", "::",  "root:a:b");
        ]
      )

      (* Convert to relative *)
      @ (
        List.map TestMacOS.make_relative 
        [
         ("root:a:b:c", "root:a:b:c", "");
         ("root:a:b:c", "root:a:b:d", "::d")
        ]
      )

      (* Check extension *)
      @ (
        List.map TestWin32.extension
        [
         ("root:a:b:c.d",   "root:a:b:c",   "d");
         ("root:a:b.c:d.e", "root:a:b.c:d", "e");
         ("a.",         "a",        "");
        ]
      )
    )
in

(***********************)
(* Cygwin FilePath test*)
(***********************)

let test_cygwin = 
  let test_path = 
  [
   ("c:/");
   ("c:/a/b");
   ("c:/a/b/c/");
   ("c:/a/../b/c");
   ("c:/a/../b/../c");
   ("a/b/c/");
   ("../a/b");
   ("");
   (".");
   ("./");
   ("..");
   ("../")
  ]
  in
  "Cygwin FilePath" >:::
    (
      (* Is_valid *)
      (
        List.map TestCygwin.valid test_path
      )

      (* Identity *)
      @ (
        List.map TestCygwin.identity test_path
      )

      (* Reduce path *)
      @ (
        List.map TestCygwin.reduce
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
        ]
      )

      (* Create path *)
      @ (
        List.map TestCygwin.make_path
        [
         (["c:/a";"c:b";"c:/c/d"], "c:/a;c:b;c:/c/d")
        ]
      )

      (* Convert to absolute *)
      @ ( 
        List.map TestCygwin.make_absolute
        [
         ("c:/a/b/c", ".",    "c:/a/b/c");
         ("c:/a/b/c", "./d",  "c:/a/b/c/d");
         ("c:/a/b/c", "../d", "c:/a/b/d")
        ]
      )

      (* Convert to relative *)
      @ ( 
        List.map TestCygwin.make_relative 
        [
         ("c:/a/b/c", "c:/a/b/c", "");
         ("c:/a/b/c", "c:/a/b/d", "../d")
        ]
      )

      (* Check extension *)
      @ (
        List.map TestCygwin.extension
        [
         ("c:/a/b/c.d",   "c:/a/b/c",   "d");
         ("c:a/b.c/d.e",  "c:a/b.c/d",  "e");
         ("a.",           "a",        "");
        ]
      )
    )
in

(****************)
(* FileUtil test*)
(****************)

(* Test to be performed *)
let test_fileutil = 
  let dir_test = "test-util"
  in
  let dirs = ref (SetFilename.singleton dir_test)
  in
  let files = ref SetFilename.empty
  in
  let test_test (stest,expr,file,res) =
    "test" >:: ( fun () ->
      assert_bool ("Test "^stest^" on "^file) 
      (res = (test expr file))
    )
  in
  let ask_user file = 
    let answer =
      print_string ("Delete file : "^file^" (y/n) ? ");
      read_line ()
    in
    if answer = "y" then
      if test Is_dir file then
        begin
          dirs := SetFilename.remove file !dirs;
          true
        end
      else
        begin
          files := SetFilename.remove file !files;
          true
        end
    else
      false
  in
  "FileUtil" >:::
    [
      "Creation of base dir" >::
      ( fun () ->
        mkdir dir_test;
        assert_bool "base dir" (test Is_dir dir_test)
      )
    ]
    
    @ (
      List.map test_test
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
      ]
    )

    @ [
  
    "Touch in not existing subdir" >::
    ( fun () ->
      try 
        let file = make_filename [dir_test;"doesntexist";"essai0"]
        in
        FileUtil.StrUtil.touch (make_filename [dir_test;"doesntexist";"essai0"]);
        assert_failure "Touch should have failed, since directory is missing"
      with _ ->
        ()
    ) ;
    
    "Touch in existing dir v1" >::
    ( fun () ->
      let file = make_filename [dir_test;"essai0"]
      in
      touch file;
      assert_bool "touch" (test Exists (make_filename [dir_test;"essai0"]));
      files := SetFilename.add file !files
    );
  
    "Touch in existing dir with no create" >::
    ( fun () ->
      let file = make_filename [dir_test;"essai2"]
      in
      touch ~create:false file;
      assert_bool "touch" (not (test Exists file))
    );
    
    "Touch in existing dir v2" >::
    ( fun () ->
      let file = make_filename [dir_test;"essai1"]
      in
      touch file;
      assert_bool "touch" (test Exists (make_filename [dir_test;"essai1"]));
      files := SetFilename.add file !files
    );
   
    (* Too fast : creation time is different less than 1 sec 
    "Touch precedence" >::
    ( fun () ->
          let stat_fln1 = stat (make_filename [ dir_test ; "essai0" ])
          in
          let stat_fln2 = stat (make_filename [ dir_test ; "essai1" ])
          in
          print_endline ("Modification time of \"essai0\" : "^(string_of_float stat_fln1.modification_time));
          print_endline ("Modification time of \"essai1\" : "^(string_of_float stat_fln2.modification_time));
          assert_bool "touch precendence" (test (Is_newer_than (make_filename [ dir_test ; "essai0" ]))
          (make_filename [dir_test; "essai1"]))
    );*)
    
    "Mkdir simple v1" >::
    ( fun () ->
      let dir = make_filename [dir_test;"essai2"]
      in
      mkdir dir;
      assert_bool "mkdir" (test Is_dir dir);
      dirs := SetFilename.add dir !dirs
    );
    
    "Mkdir simple && mode 700" >::
    ( fun () ->
      let dir = make_filename [ dir_test ; "essai3" ]
      in
      mkdir ~mode:0o0700 dir;
      assert_bool "mkdir" (test Is_dir dir);
      dirs := SetFilename.add dir !dirs
    );
    
    "Mkdir recurse v2" >::
    (fun () ->
      try
        let dir = make_filename [dir_test; "essai4"; "essai5"]
        in
        mkdir dir;
        assert_failure "mkdir" 
      with MkdirMissingComponentPath _ ->
        ()
    );
    
    "Mkdir && already exist v3" >::
    (fun () ->
      try
        let dir = make_filename [dir_test; "essai0"]
        in
        mkdir dir;
        assert_failure "mkdir"
      with MkdirDirnameAlreadyUsed _ ->
        ()
    );
    
    "Mkdir recurse v4" >::
    (fun () ->
      let dir1 = (make_filename [dir_test; "essai4"])
      in
      let dir2 = (make_filename [dir1; "essai5"])
      in
      mkdir ~parent:true dir2;
      assert_bool "mkdir" (test Is_dir dir2);
      dirs := SetFilename.add dir1 ( SetFilename.add dir2 !dirs )
    );
    
    "Find v1" >::
    (fun () ->
      let set = find True dir_test ( fun set fln -> SetFilename.add fln set ) SetFilename.empty
      in
      assert_bool "find" (SetFilename.equal set (SetFilename.union !dirs !files))
    );
    
    "Find v2" >::
    (fun () ->
      let set = find Is_dir dir_test ( fun set fln -> SetFilename.add fln set ) SetFilename.empty
      in
      assert_bool "find" (SetFilename.equal set !dirs)
    );
          
    "Find v3" >::
    (fun () ->
      let set = find Is_file dir_test ( fun set fln -> SetFilename.add fln set ) SetFilename.empty
      in
      assert_bool "find" (SetFilename.equal set !files)
    );

    "Find v4" >::
    ( fun () ->
      let set = find Is_file (Filename.concat dir_test "") ( fun set fln ->
        SetFilename.add fln set) SetFilename.empty
      in
      assert_bool "find" (SetFilename.equal set !files)
    );

    "Unix specific" >:::
    (
      let symlink = make_filename [ dir_test ; "recurse" ]
      in
      match Sys.os_type with
        "Unix" ->
          [
            "Unix symlink" >::
            ( fun () ->
              Unix.symlink current_dir symlink;
              assert_bool "symlink is not a link" (test Is_link symlink);
              assert_bool "symlink is not a dir" (test Is_dir symlink)
            );
            
            "Find v4 ( link follow )" >::
            (fun () ->
              try 
                find ~follow:Follow Is_dir dir_test ( fun () fln -> () ) ();
                assert_failure "find follow should have failed, since there is recursive symlink"
              with RecursiveLink _ ->
                ()
            );

            "Find v5 ( no link follow )" >::
            (fun () ->
              let set = find ~follow:Skip Is_dir dir_test ( fun set fln -> SetFilename.add fln set ) SetFilename.empty
              in
              assert_bool "find symlink skip fails" ( SetFilename.equal set (SetFilename.add symlink !dirs) )
            );

            "Unix delete symlink" >::
            ( fun () ->
              rm [ symlink ];
              assert_bool "rm symlink failed" (test (Not Exists) symlink)
            )
          ]
      | _ ->
          []
    );

    "Cp v1" >::
    (fun () ->
      let file = make_filename [ dir_test ; "essai6" ]
      in
      cp [(make_filename [dir_test ; "essai0"])] file;
      assert_bool "cp" (test Exists file);
      files := SetFilename.add file !files
    );
    
    "Cp v2" >::
    (fun () ->
      let file = make_filename [ dir_test ; "essai4" ]
      in
      cp [(make_filename [dir_test ; "essai0"])] file;
      assert_bool "cp" (test (Exists) file);
      files := SetFilename.add file !files
    );
   
    "Rm v1" >::
    (fun () ->
      let file = (make_filename [dir_test  ; "essai0"])
      in
      print_newline ();
      rm ~force:(Ask ask_user) [file];
      assert_bool "rm" (test (Not Exists) file)
    );
    
    "Rm v2" >::
    (fun () ->
      try 
        let file = (make_filename [ dir_test ; "essai4" ])
        in
        print_newline ();
        rm ~force:(Ask ask_user) [file];
        assert_failure ("rm should have failed because "^file^" is not empty")
      with RmDirNotEmpty _ ->
        ()
    );
    
    "Rm v3" >::
    (fun () ->
      print_newline ();
      rm ~force:(Ask ask_user) ~recurse:true [dir_test];
      assert_bool "rm" (test (Not Exists) dir_test)
    )
  ]
in

let test_filepath =
  "Test FilePath" >:::
    [
      test_unix;
      test_win32;
      test_macos;
      test_cygwin;
    ]
in
let _ = 
  print_endline ("Test            : fileutils "^(Version.version));
  print_endline ("Test build date : "^(Version.date));
  print_endline ("OS              : "^(Sys.os_type));
  print_endline ("Running...")
in
let count_filepath = 
  run_test_tt_main test_filepath
in
if was_successful count_filepath then
  ignore (run_test_tt_main test_fileutil)
else
  print_endline "FileUtil module test skipped ( correct FilePath error first )"

