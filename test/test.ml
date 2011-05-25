(********************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                      *)
(*                                                                              *)
(*  Copyright (C) 2003-2011, Sylvain Le Gall                                    *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

open OUnit;;
open FilePath;;
open FileUtil;;

module SetFilename = Set.Make (struct
    type t = FilePath.DefaultPath.filename
    let compare = FilePath.DefaultPath.compare
end)
;;

let verbose = 
  ref false
;;

let dbug_print f =
  if !verbose then
    prerr_endline (f ())
;;

let assert_equal_string ~msg = 
  assert_equal ~printer:(fun x -> x) ~msg:msg
;;  

(** Check that two set of file are equal *)
let assert_equal_set_filename st_ref st =
  let to_file_list_string lst =
    String.concat ", " 
      (List.map 
         (Printf.sprintf "%S")
         lst)
  in
  let msg_difference = 
    let file_not_expected =
      SetFilename.elements (SetFilename.diff st st_ref)
    in
    let file_not_found =
      SetFilename.elements (SetFilename.diff st_ref st)
    in
      match file_not_expected, file_not_found with 
        | [], [] ->
            "No difference"
        | _, [] ->
            Printf.sprintf 
              "Files %s are not expected in result" 
              (to_file_list_string file_not_expected)
        | [], _ ->
            Printf.sprintf 
              "Files %s are not found in result" 
              (to_file_list_string file_not_found)
        | _, _ ->
            Printf.sprintf
              "File %s are not found and %s are not expected in result"
              (to_file_list_string file_not_found)
              (to_file_list_string file_not_expected)
  in
    assert_equal 
      ~cmp:SetFilename.equal 
      ~msg:msg_difference
      ~printer:(fun st -> to_file_list_string (SetFilename.elements st))
      st_ref
      st
;;

(** Ensure that we are dealing with generated file (and not random
    file on the filesystem
  *)
module SafeFS =
struct
  let magic =
    Random.self_init ();
    Random.bits ()

  let file_mark fn =
    let chn =
      open_out_bin fn
    in
      output_binary_int chn magic;
      close_out chn

  let file_check fn =
    try
      let chn =
        open_in_bin fn
      in
      let magic =
        input_binary_int chn
      in
        close_in chn;
        magic = magic
    with _ ->
      false

  let dir_marker fn =
    Filename.concat fn "_mark"

  let is_special_file fn =
    (Filename.basename fn) = "_mark"

  let dir_mark fn =
    file_mark (dir_marker fn)

  let dir_check fn =
    file_check (dir_marker fn)

  let dir_unmark fn =
    if dir_check fn then
      Sys.remove (dir_marker fn)
    
  let assert_removable fn =
    if Sys.file_exists fn then
      (
        if Sys.is_directory fn then
          (
            assert_bool
              (Printf.sprintf "%S directory cannot be removed" fn)
              (dir_check fn)
          )
        else
          (
            assert_bool
              (Printf.sprintf "%S file cannot be removed" fn)
              (file_check fn)
          )
      )
    else
      (
        assert_failure 
          (Printf.sprintf "%S doesn't exist" fn)
      )
end
;;

module Test = 
functor (OsPath : PATH_STRING_SPECIFICATION) ->
struct
  let os_string = ref ""
  
  let test_label s value = (!os_string)^" : "^s^" \""^value^"\""

  let test_label_list s lst = test_label s ("["^(String.concat ";" lst)^"]")

  let test_label_pair s (a,b) = test_label s (a^"\" \""^b)

  let test_name s = (s)
  
  let reduce (exp,res) =
    (test_name "reduce") >:: (fun () ->
      assert_equal_string ~msg:(test_label "reduce" exp)
      res (OsPath.reduce ~no_symlink:true exp)
    )
    
  let make_path (exp,res) =
    (test_name "make_path") >:: (fun () ->
      assert_equal_string ~msg:(test_label_list "make_path" exp)
      res (OsPath.string_of_path exp)
    )
    
  let make_absolute (base,rela,res) =
    (test_name "make_absolute") >:: (fun () ->
      assert_equal_string ~msg:(test_label_pair "make_absolute" (base,rela))
      res (OsPath.reduce ~no_symlink:true (OsPath.make_absolute base rela))
    )
    
  let make_relative (base,abs,res) =
    (test_name "make_relative") >:: (fun () ->
      assert_equal_string ~msg:(test_label_pair "make_relative" (base,abs))
      res (OsPath.make_relative base abs)
    )
    
  let valid (exp) =
    (test_name "valid") >:: (fun () ->
      assert_bool (test_label "is_valid" exp)
      (OsPath.is_valid exp)
    )
    
  let identity (exp) = 
    (test_name "identity") >:: (fun () ->
      assert_equal_string ~msg:(test_label "identity" exp)
      exp (OsPath.identity exp)
    )

  let extension (filename,basename,extension) = 
    (test_name "extension") >:: (fun () ->
      assert_equal_string ~msg:(test_label "chop_extension" filename)
      (OsPath.chop_extension filename) basename;
      
      assert_equal_string ~msg:(test_label "get_extension" filename)
      (OsPath.string_of_extension (OsPath.get_extension filename)) extension;
      
      assert_bool (test_label "check_extension" filename)
      (OsPath.check_extension filename (OsPath.extension_of_string extension));
      
      assert_bool (test_label "check_extension (false) " filename)
      (not (OsPath.check_extension filename (OsPath.extension_of_string "dummy")))
    )

  let is_relative (filename, res) =
    (test_name "is_relative") >::
     (fun () ->
        assert_equal 
          res
          (OsPath.is_relative filename))
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

(** Static test *)
let _ = 
  assert(UnixPath.get_extension "test.txt" = "txt");
  assert(MacOSPath.get_extension "test.txt" = "txt");
  assert(Win32Path.get_extension "test.txt" = "txt");
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
   ("c:\\");
   ("c:\\a\\b");
   ("c:\\a\\b\\c\\");
   ("c:\\a\\..\\b\\c");
   ("c:\\a\\..\\b\\..\\c");
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
         ("c:\\a\\b\\c",                           "c:\\a\\b\\c");
         ("c:\\a\\b\\c\\",                         "c:\\a\\b\\c");
         ("c:\\a\\b\\c\\d\\..",                    "c:\\a\\b\\c");
         ("c:\\a\\b\\c\\.",                        "c:\\a\\b\\c");
         ("c:\\a\\d\\..\\b\\c",                    "c:\\a\\b\\c");
         ("c:\\a\\.\\b\\c",                        "c:\\a\\b\\c");
         ("c:\\a\\b\\c\\d\\.\\..",                 "c:\\a\\b\\c");
         ("c:\\a\\b\\c\\d\\..\\.",                 "c:\\a\\b\\c");
         ("c:\\a\\b\\d\\.\\..\\c",                 "c:\\a\\b\\c");
         ("c:\\a\\b\\d\\..\\.\\c",                 "c:\\a\\b\\c");
         ("c:\\a\\b\\..\\d\\..\\b\\c",             "c:\\a\\b\\c");
         ("c:\\a\\.\\.\\.\\b\\.\\c",               "c:\\a\\b\\c");
         ("c:\\a\\..\\a\\.\\b\\..\\c\\..\\b\\.\\c","c:\\a\\b\\c");
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
         (["c:/a";"b";"c:/c\\d"], "c:\\a;b;c:\\c\\d");
         ([],                     "");
        ]
      )

      (* Convert to absolute *)
      @ (
        List.map TestWin32.make_absolute
        [
         ("c:\\a\\b\\c", ".",     "c:\\a\\b\\c");
         ("c:\\a\\b\\c", ".\\d",  "c:\\a\\b\\c\\d");
         ("c:\\a\\b\\c", "..\\d", "c:\\a\\b\\d");
         ("c:\\a\\b\\c", "",      "c:\\a\\b\\c");
         ("c:\\a\\b\\c", ".",     "c:\\a\\b\\c");
         ("c:\\a\\b\\c", ".\\",   "c:\\a\\b\\c");
         ("c:\\a\\b\\c", "..",    "c:\\a\\b");
         ("c:\\a\\b\\c", "..\\",  "c:\\a\\b");
        ]
      )

      (* Convert to relative *)
      @ (
        List.map TestWin32.make_relative 
        [
         ("c:\\a\\b\\c", "c:/a\\b\\c", "");
         ("c:\\a\\b\\c", "c:/a\\b\\d", "..\\d")
        ]
      )

      (* Check extension *)
      @ (
        List.map TestWin32.extension
        [
         ("c:\\a\\b\\c.d",   "c:\\a\\b\\c",   "d");
         ("c:\\a\\b.c\\d.e", "c:\\a\\b.c\\d", "e");
         ("a.",         "a",        "");
        ]
      )

      @ (
        List.map TestWin32.is_relative
          [
            "c:/a",  false;
            "c:\\a", false;
            "./a",   true;
            ".\\a",  true;
            "../a",  true;
            "..\\a",  true;
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
        List.map TestMacOS.extension
        [
         ("root:a:b:c.d",   "root:a:b:c",   "d");
         ("root:a:b.c:d.e", "root:a:b.c:d", "e");
         ("a.",         "a",        "");
        ]
      )
    )
in

(*****************)
(* FileUtil test *)
(*****************)

(* Test to be performed *)
let test_fileutil = 
  let dirs = 
    ref SetFilename.empty 
  in
  let files = 
    ref SetFilename.empty
  in

  let add_fn fn = 
    if Sys.is_directory fn then
      (
        SafeFS.dir_mark fn;
        files := SetFilename.add (SafeFS.dir_marker fn) !files;
        dirs := SetFilename.add fn !dirs
      )
    else
      (
        SafeFS.file_mark fn;
        files := SetFilename.add fn !files;
      )
  in

  let auto_ask_user fn = 
    if not (SafeFS.is_special_file fn) then
      (
        if Sys.file_exists fn then
          (
            SafeFS.assert_removable fn;
            if Sys.is_directory fn then
              (
                dbug_print
                 (fun () -> "Removing directory '"^fn^"'");
                SafeFS.dir_unmark fn;
                dirs := SetFilename.remove fn !dirs
              )
            else
              (
                dbug_print
                 (fun () -> "Removing file '"^fn^"'");
                files := SetFilename.remove fn !files
              )
          )
        else
          (
            dbug_print
              (fun () -> "Allow to remove not existing file '"^fn^"'");
          );
        true
      )
    else
      (
        dbug_print 
          (fun () -> "Skipping special file '"^fn^"'");
        false
      )
  in

  let dir_test = 
    let fn =
      Filename.temp_file "fileutil-" ""
    in
      Sys.remove fn;
      Unix.mkdir fn 0o700;
      at_exit
        (fun () ->
           if Sys.file_exists fn then
             try
               Unix.rmdir fn
             with _ ->
               ());
      add_fn fn;
      fn
  in

  let dir_otherfs = 
    pwd ()
  in

  let file_test =
    let fn =
      make_filename [dir_test; "essai99"]
    in
      touch fn;
      at_exit
        (fun () ->
           try
             if Sys.file_exists fn then
               Sys.remove fn
           with _ ->
             ());
      add_fn fn;
      fn
  in

  "FileUtil" >:::
    [
      "Creation of base dir" >::
      (fun () ->
        assert_bool "base dir" (test Is_dir dir_test);
        assert_bool "file test" (test Is_file file_test)
      )
    ]
    
    @ (
      let test_test (stest,expr,file,res, cond) =
        "test" >:: 
        (fun () ->
           cond ();
           assert_bool 
             ("Test "^stest^" on "^file) 
             (res = (test expr file))
        )
      in
      let all () =
        ()
      in
      let not_win32 () =
        skip_if (Sys.os_type = "Win32") "Test not available on win32"
      in
      List.map test_test
      [
       "True",            True,            dir_test,  true,  all;
       "False",           False,           dir_test,  false, all;
       "Is_dir",          Is_dir,          dir_test,  true,  all;
       "Not Is_dir",      (Not Is_dir),    dir_test,  false, all;
       "Is_dev_block",    Is_dev_block,    dir_test,  false, all;
       "Is_dev_char",     Is_dev_char,     dir_test,  false, all;
       "Exists",          Exists,          dir_test,  true,  all;
       "Is_file",         Is_file,         dir_test,  false, all;
       "Is_set_group_ID", Is_set_group_ID, dir_test,  false, all;
       "Has_sticky_bit",  Has_sticky_bit,  dir_test,  false, all;
       "Is_link",         Is_link,         dir_test,  false, all;
       "Is_pipe",         Is_pipe,         dir_test,  false, all;
       "Is_readable",     Is_readable,     dir_test,  true,  all;
       "Is_writeable",    Is_writeable,    dir_test,  true,  all;
       "Size_not_null",   Size_not_null,   file_test, true,  all;
       "Is_socket",       Is_socket,       dir_test,  false, all;
       "Has_set_user_ID", Has_set_user_ID, dir_test,  false, all;
       "Is_exec",         Is_exec,         dir_test,  true,  all;
       "Match",           Match(dir_test), dir_test,  true,  all;

       "And of test_file * test_file", And(True,False), dir_test, false, all;
       "Or of test_file * test_file", Or(True,False), dir_test, true, all;
       "Is_owned_by_user_ID", Is_owned_by_user_ID, dir_test, true, not_win32;
       "Is_owned_by_group_ID", Is_owned_by_group_ID,dir_test,true, not_win32;
       "Is_newer_than", (Is_newer_than dir_test), dir_test, false, all;
       "Is_older_than", (Is_older_than dir_test), dir_test, false, all;
      ]
    )

    @ [

    "Test with FileUtilStr.Match" >::
    (fun () ->
       assert_bool 
         "FileUtilStr.Match = true"
         (FileUtilStr.test (Match ".*fileutil-") dir_test);
       assert_bool
         "FileUtilStr.Match = false"
         (not (FileUtilStr.test (Match "fileutil") dir_test))
    );

    "Touch in not existing subdir" >::
    (fun () ->
      try 
        let file = 
          make_filename [dir_test;"doesntexist";"essai0"]
        in
        touch file;
        assert_failure "Touch should have failed, since directory is missing"
      with _ ->
        ()
    ) ;
    
    "Touch in existing dir v1" >::
    (fun () ->
      let file = 
        make_filename [dir_test;"essai0"]
      in
      touch file;
      Unix.sleep 1;
      assert_bool "touch" (test Exists (make_filename [dir_test;"essai0"]));
      add_fn file
    );
  
    "Touch in existing dir with no create" >::
    (fun () ->
      let file = make_filename [dir_test;"essai2"]
      in
      touch ~create:false file;
      assert_bool "touch" (not (test Exists file))
    );
    
    "Touch in existing dir v2" >::
    (fun () ->
      let file = make_filename [dir_test;"essai1"]
      in
      touch file;
      assert_bool "touch" (test Exists (make_filename [dir_test;"essai1"]));
      add_fn file
    );
   
    "Touch precedence" >::
    (fun () ->
      let time =
        Unix.gettimeofday ()
      in
      let fn1 = 
        make_filename [dir_test ; "essai1"]
      in
      let fn2 =
        make_filename [dir_test ; "essai0"]
      in
        touch ~time:(Touch_timestamp time) fn1;
        touch ~time:(Touch_timestamp (time +. 1.0)) fn2;
        assert_bool 
          "touch precedence 1" 
          (test 
             (Is_newer_than fn1)
             fn2);
        assert_bool 
          "touch precedence 2" 
          (test 
             (Is_older_than fn2)
             fn1)
    );
    
    "Mkdir simple v1" >::
    (fun () ->
      let dir = make_filename [dir_test;"essai2"]
      in
      mkdir dir;
      assert_bool "mkdir" (test Is_dir dir);
      add_fn dir
    );
    
    "Mkdir simple && mode 700" >::
    (fun () ->
      let dir = make_filename [dir_test ; "essai3"]
      in
      mkdir ~mode:0o0700 dir;
      assert_bool "mkdir" (test Is_dir dir);
      add_fn dir
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
      add_fn dir1; 
      add_fn dir2
    );

    "Find v0" >::
    (bracket 
       (fun () ->
          let pwd = pwd () in
            Sys.chdir dir_test;
            pwd)
       (fun pwd ->
          let find_acc dir = 
            find True "." (fun acc x -> reduce x :: acc) []
          in
          let lst_dot = 
            find_acc "."
          in
          let lst_empty = 
            find_acc ""
          in
            assert_bool
              "find '.' is empty"
              (lst_dot <> []);
            assert_bool
              "find '' is empty"
              (lst_empty <> []);
            assert_bool
              "find '.' <> find ''"
              (lst_dot = lst_empty))
       (fun pwd ->
          Sys.chdir pwd));
    
    "Find v1" >::
    (fun () ->
      let set = find True dir_test (fun set fln -> SetFilename.add fln set) SetFilename.empty
      in
      assert_equal_set_filename 
        (SetFilename.union !dirs !files)
        set 
    );
    
    "Find v2" >::
    (fun () ->
      let set = find Is_dir dir_test (fun set fln -> SetFilename.add fln set) SetFilename.empty
      in
      assert_equal_set_filename
        !dirs
        set 
    );
          
    "Find v3" >::
    (fun () ->
      let set = find Is_file dir_test (fun set fln -> SetFilename.add fln set) SetFilename.empty
      in
      assert_equal_set_filename 
        !files
        set 
    );

    "Find v4" >::
    (fun () ->
      let set = find Is_file (Filename.concat dir_test "") (fun set fln ->
        SetFilename.add fln set) SetFilename.empty
      in
      assert_equal_set_filename 
        !files
        set 
    );

    "Unix specific" >:::
    (
      let symlink = make_filename [dir_test ; "recurse"]
      in
      match Sys.os_type with
        "Unix" ->
          [
            "Unix symlink" >::
            (fun () ->
              Unix.symlink current_dir symlink;
              assert_bool "symlink is not a link" (test Is_link symlink);
              assert_bool "symlink is not a dir" (test Is_dir symlink)
            );
            
            "Find v4 (link follow)" >::
            (fun () ->
              try 
                find ~follow:Follow Is_dir dir_test (fun () fln -> ()) ();
                assert_failure "find follow should have failed, since there is recursive symlink"
              with RecursiveLink _ ->
                ()
            );

            "Find v5 (no link follow)" >::
            (fun () ->
              let set = find ~follow:Skip Is_dir dir_test (fun set fln -> SetFilename.add fln set) SetFilename.empty
              in
              assert_bool "find symlink skip fails" (SetFilename.equal set !dirs)
            );

            "Unix delete symlink" >::
            (fun () ->
              rm [symlink];
              assert_bool "rm symlink failed" (test (Not Exists) symlink)
            )
          ]
      | _ ->
          []
    );

    "Cp v1" >::
    (fun () ->
      let file = make_filename [dir_test ; "essai6"]
      in
      cp [(make_filename [dir_test ; "essai0"])] file;
      assert_bool "cp" (test Exists file);
      add_fn file
    );
    
    "Cp v2" >::
    (fun () ->
      let file = make_filename [dir_test ; "essai4"]
      in
      cp [(make_filename [dir_test ; "essai0"])] file;
      assert_bool "cp" (test (Exists) file);
      add_fn file
    );

    "Cp with space" >::
    (fun () ->
       let dirspace = make_filename [dir_test; "essai 7"]
       in
       let file = make_filename [dirspace; "essai0"]
       in
         mkdir dirspace;
         cp [(make_filename [dir_test ; "essai0"])] file;
         assert_bool "cp" (test (Exists) file);
         add_fn dirspace;
         add_fn file
    );

    "Mv simple" >::
    (fun () ->
       let file0 = make_filename [dir_test; "essai0"]
       in
       let file1 = make_filename [dir_test; "essai10"]
       in
       let file2 = make_filename [dir_test; "essai9"]
       in
         cp [file0] file1;
         mv file1 file2;
         cp [file0] file1;
         mv file1 file2;
         add_fn file2;
         assert_bool "mv" (test Exists file2);
    );

    "Mv otherfs" >::
    (fun () ->
       let file_test = make_filename [dir_test; "essai12"]
       in
       let file = make_filename [dir_otherfs; "essai11"]
       in
         touch file_test;
         SafeFS.file_mark file_test;
         mv file_test file;
         assert_bool "mv" (test Exists file);
         rm ~force:(Ask auto_ask_user) [file]
    );
   
    "Rm simple" >::
    (fun () ->
      let file = (make_filename [dir_test  ; "essai0"])
      in
      rm ~force:(Ask auto_ask_user) [file];
      assert_bool "rm" (test (Not Exists) file)
    );
    
    "Rm no recurse" >::
    (fun () ->
      let file = (make_filename [dir_test; "essai4"])
      in
        try 
          rm ~force:(Ask auto_ask_user) [file];
          assert_failure ("rm should have failed because "^file^" is a directory")
        with RmDirNoRecurse _ ->
          (* Need to mark again essai4 *)
          add_fn file
    );

    "Rm ask duplicate" >::
    (fun () ->
       let dir =
         make_filename [dir_test; "ask-duplicate"]
       in
       let fn =
         make_filename [dir; "toto.txt"]
       in
       let set_asked = 
         ref SetFilename.empty
       in
       let set_duplicated =
         ref SetFilename.empty
       in
       let ask_register fn =
         if SetFilename.mem fn !set_asked then
           set_duplicated := SetFilename.add fn !set_duplicated;
         set_asked := SetFilename.add fn !set_asked;
         auto_ask_user fn
       in
         mkdir dir;
         touch fn;
         add_fn dir;
         add_fn fn;
         rm ~force:(Ask ask_register) ~recurse:true [dir];
         assert_equal 
           ~msg:"duplicate file asked when removing"
           SetFilename.empty
           !set_duplicated
    );
    
    "Rm final" >::
    (fun () ->
      rm ~force:(Ask auto_ask_user) ~recurse:true [dir_test];
      assert_bool "rm" (test (Not Exists) dir_test)
    );

    "Which ocamlc" >::
    (fun () ->
       try 
         let _str : string = 
           which "ocamlc"
         in
           ()
       with Not_found ->
         assert_failure "Cannot find ocamlc"
    );


    "Size" >:::
    [

      "string_of_size" >:::
      (
        let i64_unit = 
          1025L
        in
        let i64_unit2 =
          Int64.succ (Int64.mul 1024L 1024L)
        in
        let test_of_vector fuzzy (str, sz) =
          TestCase 
            (fun () -> 
               assert_equal 
                 ~printer:(fun s -> s)
                 str
                 (string_of_size ~fuzzy:fuzzy sz))
        in

          [ 
            "exact" >:::
            (
              List.map
                (test_of_vector false)
                [
                  "0 TB", TB 0L;
                  "0 GB", GB 0L;
                  "0 MB", MB 0L;
                  "0 KB", KB 0L;
                  "0 B",  B  0L;
                  "1 TB", TB 1L;
                  "1 GB", GB 1L;
                  "1 MB", MB 1L;
                  "1 KB", KB 1L;
                  "1 B",  B  1L;
                  "1025 TB",   TB i64_unit;
                  "1 TB 1 GB", GB i64_unit;
                  "1 GB 1 MB", MB i64_unit;
                  "1 MB 1 KB", KB i64_unit;
                  "1 KB 1 B",  B  i64_unit;
                  "1024 TB 1 GB", GB i64_unit2;
                  "1 TB 1 MB",    MB i64_unit2;
                  "1 GB 1 KB",    KB i64_unit2;
                  "1 MB 1 B",     B  i64_unit2;
                  "97 MB 728 KB 349 B", B 102457693L;
                ]
            );

            "fuzzy" >:::
            (
              List.map
                (test_of_vector true)
                [
                  "0.00 TB", TB 0L;
                  "0.00 GB", GB 0L;
                  "0.00 MB", MB 0L;
                  "0.00 KB", KB 0L;
                  "0.00 B",  B  0L;
                  "1.00 TB", TB 1L;
                  "1.00 GB", GB 1L;
                  "1.00 MB", MB 1L;
                  "1.00 KB", KB 1L;
                  "1.00 B",  B  1L;
                  "1025.00 TB", TB i64_unit;
                  "1.00 TB",    GB i64_unit;
                  "1.00 GB",    MB i64_unit;
                  "1.00 MB",    KB i64_unit;
                  "1.00 KB",    B  i64_unit;
                  "1024.00 TB", GB i64_unit2;
                  "1.00 TB",    MB i64_unit2;
                  "1.00 GB",    KB i64_unit2;
                  "1.00 MB",    B  i64_unit2;
                  "97.71 MB", B 102457693L;
                ]
            );
          ]
      );

      "size_add" >:::
      (
        let test_of_vector (str, szs) = 
          TestCase 
            (fun () ->
               assert_equal 
                 ~printer:(fun s -> s)
                 str
                 (string_of_size
                    (List.fold_left size_add (B 0L) szs)))
        in
          List.map 
            test_of_vector
            [ 
              "1 TB 10 MB 12 KB", [TB 1L; KB 12L; MB 10L];
              "2 MB 976 KB",      [KB 2000L; MB 1L]                                
            ]
      );

      "size_compare" >:::
      (
        let test_of_vector (sz1, sz2, res) = 
          TestCase 
            (fun () ->
               let cmp =
                 size_compare sz1 sz2
               in
               let norm i =
                 if i < 0 then 
                   -1
                 else if i > 0 then
                   1
                 else
                   0
               in
                 assert_equal 
                   ~printer:string_of_int
                   (norm res)
                   cmp
            )
        in
          List.map 
            test_of_vector
            [ 
              TB 1L, TB 1L, 0;
              GB 1L, GB 1L, 0;
              MB 1L, MB 1L, 0;
              KB 1L, KB 1L, 0;
               B 1L,  B 1L, 0;
              TB 1L,  B 1L, 1;
              GB 1L,  B 1L, 1;
              MB 1L,  B 1L, 1;
              KB 1L,  B 1L, 1;
               B 2L,  B 1L, 1;
            ]
      );
    ];
  ]
in

let tests =
  "ocaml-fileutils" >:::
  [
    "FilePath" >:::
    [
      test_unix;
      test_win32;
      test_macos;
    ];

    test_fileutil;
  ]
in
  ignore (run_test_tt_main tests)

