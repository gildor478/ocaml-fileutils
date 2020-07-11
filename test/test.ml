(******************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                    *)
(*                                                                            *)
(*  Copyright (C) 2003-2014, Sylvain Le Gall                                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version, with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         *)
(*  COPYING for more details.                                                 *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             *)
(******************************************************************************)

open OUnit2
open FilePath
open FileUtil

exception ExpectedException

let test_umask = 0o0022


let umask_mutex = OUnitShared.Mutex.create OUnitShared.ScopeProcess


let bracket_umask umask =
  bracket
    (fun test_ctxt ->
       OUnitShared.Mutex.lock test_ctxt.OUnitTest.shared umask_mutex;
       Unix.umask umask)
    (fun umask test_ctxt ->
       let _i: int = Unix.umask umask in
         OUnitShared.Mutex.unlock test_ctxt.OUnitTest.shared umask_mutex)


let with_bracket_umask test_ctxt umask f =
  OUnitBracket.with_bracket test_ctxt (bracket_umask umask) f


module SetFilename = Set.Make (struct
    type t = FilePath.DefaultPath.filename
    let compare = FilePath.DefaultPath.compare
end)


let assert_equal_string ~msg =
  assert_equal ~printer:(fun x -> x) ~msg:msg


module DiffSetFilename =
  OUnitDiff.SetMake
    (struct
       type t = string
       let compare = FilePath.DefaultPath.compare
       let pp_printer = Format.pp_print_string
       let pp_print_sep = OUnitDiff.pp_comma_separator
     end)

(** Check that two set of file are equal *)
let assert_equal_set_filename ?msg st_ref st =
  DiffSetFilename.assert_equal ?msg
    (DiffSetFilename.of_list (SetFilename.elements st_ref))
    (DiffSetFilename.of_list (SetFilename.elements st))


let assert_perm fn exp =
  assert_equal
    ~msg:(Printf.sprintf "permission of '%s'" fn)
    ~printer:(Printf.sprintf "0o%04o")
    exp (Unix.lstat fn).Unix.st_perm


let assert_error msg e f =
  assert_raises
    ~msg
    ExpectedException
    (fun () ->
       f (fun _ err -> if e err then raise ExpectedException))

(** Ensure that we are dealing with generated file (and not random
    file on the filesystem).
  *)
module SafeFS =
struct
  module S =
    Set.Make
      (struct
         type t = int * int
         let compare = Stdlib.compare
       end)

  type t =
      {
        mutable files: SetFilename.t;
        mutable dirs: SetFilename.t;
        mutable markers: S.t;
      }

  let default () =
    {
      files = SetFilename.empty;
      dirs = SetFilename.empty;
      markers = S.empty;
    }

  let marker fn =
    let st = Unix.lstat fn in
      (st.Unix.st_dev, st.Unix.st_ino)

  let mark t fn =
    t.markers <- S.add (marker fn) t.markers

  let touch t fn =
    if Sys.file_exists fn then begin
      failwith (Printf.sprintf "File %S already exists." fn)
    end else begin
      let chn = open_out fn in
        close_out chn;
        mark t fn;
        t.files <- SetFilename.add fn t.files
    end

  let mkdir t dn =
    if Sys.file_exists dn then begin
      failwith (Printf.sprintf "Directory %S already exists." dn)
    end else begin
      Unix.mkdir dn 0o755;
      mark t dn;
      t.dirs <- SetFilename.add dn t.dirs
    end

  let auto_ask_user t =
    Ask (fun fn -> S.mem (marker fn) t.markers)

  let create dn dirs files =
    let t = default () in
      mark t dn;
      t.dirs <- SetFilename.add dn t.dirs;
      List.iter (fun fn -> mkdir t (Filename.concat dn fn)) dirs;
      List.iter (fun fn -> touch t (Filename.concat dn fn)) files;
      t
end

module Test =
functor (OsPath: PATH_STRING_SPECIFICATION) ->
struct
  let os_string = ref ""

  let test_label s value = (!os_string)^" : "^s^" \""^value^"\""

  let test_label_list s lst = test_label s ("["^(String.concat ";" lst)^"]")

  let test_label_pair s (a, b) = test_label s (a^"\" \""^b)

  let test_name s = (s)

  let reduce (exp, res) =
    (test_name "reduce") >::
     (fun _ ->
        assert_equal_string
          ~msg:(test_label "reduce" exp)
          res (OsPath.reduce ~no_symlink:true exp))

  let make_path (exp, res) =
    (test_name "make_path") >::
     (fun _ ->
        assert_equal_string ~msg:(test_label_list "make_path" exp)
          res (OsPath.string_of_path exp))

  let make_absolute (base, rela, res) =
    (test_name "make_absolute") >::
     (fun _ ->
        assert_equal_string ~msg:(test_label_pair "make_absolute" (base, rela))
          res (OsPath.reduce ~no_symlink:true (OsPath.make_absolute base rela)))

  let make_relative (base, abs, res) =
    (test_name "make_relative") >::
     (fun _ ->
        assert_equal_string ~msg:(test_label_pair "make_relative" (base, abs))
          res (OsPath.make_relative base abs))

  let valid exp =
    (test_name "valid") >::
     (fun _ ->
        assert_bool (test_label "is_valid" exp)
          (OsPath.is_valid exp))

  let identity exp =
    (test_name "identity") >::
     (fun _ ->
        assert_equal_string ~msg:(test_label "identity" exp)
          exp (OsPath.identity exp))

  let extension (filename, basename, extension) =
    (test_name "extension") >::
     (fun _ ->
        assert_equal_string ~msg:(test_label "chop_extension" filename)
          (OsPath.chop_extension filename) basename;

        assert_equal_string ~msg:(test_label "get_extension" filename)
          (OsPath.string_of_extension (OsPath.get_extension filename))
          extension;

        assert_bool (test_label "check_extension" filename)
          (OsPath.check_extension filename
             (OsPath.extension_of_string extension));

        assert_bool (test_label "check_extension (false) " filename)
          (not (OsPath.check_extension filename
                  (OsPath.extension_of_string "dummy"))))

  let is_relative (filename, res) =
    (test_name "is_relative") >::
     (fun _ ->
        assert_equal
          res
          (OsPath.is_relative filename))
end


module TestUnix  = Test(UnixPath)
module TestWin32 = Test(Win32Path)
let () =
  TestUnix.os_string := "Unix";
  TestWin32.os_string := "Win32"


(** Static test *)
let _ =
  assert(UnixPath.get_extension "test.txt" = "txt");
  assert(Win32Path.get_extension "test.txt" = "txt")

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
       ("/a/b/c",                    "/a/b/c");
       ("/a/b/c/",                   "/a/b/c");
       ("/a/b/c/d/..",               "/a/b/c");
       ("/a/b/c/.",                  "/a/b/c");
       ("/a/d/../b/c",               "/a/b/c");
       ("/a/./b/c",                  "/a/b/c");
       ("/a/b/c/d/./..",             "/a/b/c");
       ("/a/b/c/d/../.",             "/a/b/c");
       ("/a/b/d/./../c",             "/a/b/c");
       ("/a/b/d/.././c",             "/a/b/c");
       ("/a/b/../d/../b/c",          "/a/b/c");
       ("/a/./././b/./c",            "/a/b/c");
       ("/a/../a/./b/../c/../b/./c", "/a/b/c");
       ("/a/../..",                  "/");
       ("./d/../a/b/c",              "a/b/c");
       ("a/b/c/../../../",           "");
       ("",                          "");
       (".",                         "");
       ("./",                        "");
       ("..",                        "..");
       ("../",                       "..");
      ]
    )

    (* Create path *)
    @ (
      List.map TestUnix.make_path
      [
       (["/a"; "b"; "/c/d"], "/a:b:/c/d");
       ([],                  "");
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
      (List.map TestWin32.valid test_path)

      (* Identity *)
      @ (List.map TestWin32.identity test_path)

      (* Reduce path *)
      @ (List.map TestWin32.reduce
           [("c:\\a\\b\\c",                            "c:\\a\\b\\c");
            ("c:\\a\\b\\c\\",                          "c:\\a\\b\\c");
            ("c:\\a\\b\\c\\d\\..",                     "c:\\a\\b\\c");
            ("c:\\a\\b\\c\\.",                         "c:\\a\\b\\c");
            ("c:\\a\\d\\..\\b\\c",                     "c:\\a\\b\\c");
            ("c:\\a\\.\\b\\c",                         "c:\\a\\b\\c");
            ("c:\\a\\b\\c\\d\\.\\..",                  "c:\\a\\b\\c");
            ("c:\\a\\b\\c\\d\\..\\.",                  "c:\\a\\b\\c");
            ("c:\\a\\b\\d\\.\\..\\c",                  "c:\\a\\b\\c");
            ("c:\\a\\b\\d\\..\\.\\c",                  "c:\\a\\b\\c");
            ("c:\\a\\b\\..\\d\\..\\b\\c",              "c:\\a\\b\\c");
            ("c:\\a\\.\\.\\.\\b\\.\\c",                "c:\\a\\b\\c");
            ("c:\\a\\..\\a\\.\\b\\..\\c\\..\\b\\.\\c", "c:\\a\\b\\c");
            ("a\\..\\b",                               "b");
            ("",                                       "");
            (".",                                      "");
            (".\\",                                    "");
            ("..",                                     "..");
            ("..\\",                                   "..")])

      (* Create path *)
      @ (List.map TestWin32.make_path
           [(["c:/a"; "b"; "c:/c\\d"], "c:\\a;b;c:\\c\\d");
            ([],                       "")])

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

(*****************)
(* FileUtil test *)
(*****************)

(* Test to be performed *)
let test_fileutil =
  "FileUtil" >:::
    ["Test" >::
     (fun test_ctxt ->
        let tmp_dir = bracket_tmpdir test_ctxt in
        let file_test =
          let fn, chn = bracket_tmpfile test_ctxt in
            output_string chn "foo";
            close_out chn;
            fn
        in
        let non_fatal_test file (stest, expr, res) =
          non_fatal test_ctxt
            (fun _ ->
               assert_bool
                 ("Test "^stest^" on "^file)
                 (res = (test expr file)))
        in
          non_fatal_test file_test ("Size_not_null",   Size_not_null, true);
          List.iter
            (non_fatal_test tmp_dir)
            [
              "True",            True,            true;
              "False",           False,           false;
              "Is_dir",          Is_dir,          true;
              "Not Is_dir",      (Not Is_dir),    false;
              "Is_dev_block",    Is_dev_block,    false;
              "Is_dev_char",     Is_dev_char,     false;
              "Exists",          Exists,          true;
              "Is_file",         Is_file,         false;
              "Is_set_group_ID", Is_set_group_ID, false;
              "Has_sticky_bit",  Has_sticky_bit,  false;
              "Is_link",         Is_link,         false;
              "Is_pipe",         Is_pipe,         false;
              "Is_readable",     Is_readable,     true;
              "Is_writeable",    Is_writeable,    true;
              "Is_socket",       Is_socket,       false;
              "Has_set_user_ID", Has_set_user_ID, false;
              "Is_exec",         Is_exec,         true;
              "Match",           Match(tmp_dir), true;

              "And of test_file * test_file", And(True, False), false;
              "Or of test_file * test_file", Or(True, False), true;
              "Is_newer_than", (Is_newer_than tmp_dir), false;
              "Is_older_than", (Is_older_than tmp_dir), false;
            ];
          if Sys.os_type <> "Win32" then begin
            List.iter
              (non_fatal_test tmp_dir)
              [
                "Is_owned_by_user_ID", Is_owned_by_user_ID, true;
                "Is_owned_by_group_ID", Is_owned_by_group_ID, true;
              ]
          end);

    "Test with FileUtilStr.Match" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir ~prefix:"fileutil-foobar" test_ctxt in
         assert_bool
           "FileUtilStr.Match = true"
           (FileUtilStr.test (Match ".*fileutil-") tmp_dir);
         assert_bool
           "FileUtilStr.Match = false"
           (not (FileUtilStr.test (Match "fileutil") tmp_dir)));

    "Mode" >:::
    [
      "to_string" >::
      (fun _ ->
         List.iter
           (fun (str, mode) ->
              assert_equal
                ~printer:(fun s -> s)
                str
                (FileUtil.Mode.to_string mode))
           [
             "u+r", [`User (`Add `Read)];
             "u+rw", [`User (`Add (`List [`Read; `Write]))];
             "+rw,u=rw,g=rwx",
             [
               `None (`Add (`List [`Read; `Write]));
               `User (`Set (`List [`Read; `Write]));
               `Group (`Set (`List [`Read; `Write; `Exec]));
             ];
           ]);

      "apply" >::
      (fun _ ->
         List.iter
           (fun (is_dir, umask, i, m, e) ->
              assert_equal
                ~msg:(Printf.sprintf
                        "0o%04o + %s" i (FileUtil.Mode.to_string m))
                ~printer:(Printf.sprintf "0o%04o")
                e (FileUtil.Mode.apply ~is_dir ~umask i m))
           [
             false, 0o022, 0o0600,
             [`Group (`Add `Read)], 0o0640;

             false, 0o022, 0o0600,
             [`Group (`Add (`List [`Read; `Write]))], 0o0660;

             false, 0o022, 0o0600,
             [`Other (`Add (`List [`Read; `Write]))], 0o0606;

             false, 0o022, 0o0600,
             [`User (`Set (`List [`Read; `Write; `Exec]))], 0o0700;

             false, 0o022, 0o0600,
             [`User (`Set (`List [`Read; `Write; `Exec]))], 0o0700;

             false, 0o022, 0o0600,
             [`None (`Add (`List [`Read; `Write; `Exec]))], 0o0755;

             false, 0o022, 0o0600,
             [`Group (`Add `ExecX)], 0o0600;

             false, 0o022, 0o0700,
             [`Group (`Add `ExecX)], 0o0710;

             true, 0o022, 0o0600,
             [`Group (`Add `ExecX)], 0o0610;

             false, 0o022, 0o0600,
             [`Group (`Set `User)], 0o0660;

             false, 0o022, 0o0600,
             [`Group (`Add `StickyO)], 0o0600;

             false, 0o022, 0o0600,
             [`Group (`Add `Sticky)], 0o2600;

             false, 0o022, 0o0600,
             [`Other (`Add `StickyO)], 0o1600;

             false, 0o022, 0o0600,
             [`Other (`Add `Sticky)], 0o0600;
           ]
      )
    ];

    "Touch in not existing subdir" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       try
         let file = make_filename [tmp_dir; "doesntexist"; "essai0"] in
         touch file;
         assert_failure
           "Touch should have failed, since intermediate directory is missing"
       with _ ->
         ());

    "Touch in existing dir v1" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let file = make_filename [tmp_dir; "essai0"] in
       touch file;
       assert_bool "touch" (test Exists file);
    );

    "Touch in existing dir with no create" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let file = make_filename [tmp_dir; "essai2"] in
         touch ~create:false file;
         assert_bool "touch" (not (test Exists file)));

    "Touch in existing dir v2" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let file = make_filename [tmp_dir; "essai1"] in
         touch file;
         assert_bool "touch" (test Exists file));

    "Touch precedence" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let time = Unix.gettimeofday () in
       let fn1 = make_filename [tmp_dir; "essai1"] in
       let fn2 = make_filename [tmp_dir; "essai0"] in
         touch ~time:(Touch_timestamp time) fn1;
         touch ~time:(Touch_timestamp (time +. 1.0)) fn2;
         assert_bool "touch precedence 1"
           (test (Is_newer_than fn1) fn2);
         assert_bool
           "touch precedence 2"
           (test (Is_older_than fn2) fn1));

    "Mkdir simple v1" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir = make_filename [tmp_dir; "essai2"] in
         mkdir dir;
         assert_bool "mkdir" (test Is_dir dir));

    "Mkdir simple && mode 700" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir = make_filename [tmp_dir; "essai3"] in
         mkdir ~mode:(`Octal 0o0700) dir;
         assert_bool "mkdir" (test Is_dir dir));

    "Mkdir recurse v2" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir = make_filename [tmp_dir; "essai4"; "essai5"] in
         assert_error
           "missing component path"
           (function
              | `MissingComponentPath _ -> true
              | _ -> false)
           (fun error -> mkdir ~error dir));

    "Mkdir && already exist v3" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir = make_filename [tmp_dir; "essai0"] in
         touch dir;
         assert_error
           "dirname already used"
           (function
              | `DirnameAlreadyUsed _ -> true
              | _ -> false)
           (fun error -> mkdir ~error dir));

    "Mkdir recurse v4" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir1 = (make_filename [tmp_dir; "essai4"]) in
       let dir2 = (make_filename [dir1; "essai5"]) in
         mkdir ~parent:true dir2;
         assert_bool "mkdir" (test Is_dir dir2);
         if Sys.os_type <> "Win32" then begin
           assert_perm dir1 0o0755;
           assert_perm dir2 0o0755;
         end;
         rm ~recurse:true [dir1];
         assert_bool "no dir" (not (test Exists dir2));

         mkdir
           ~parent:true
           ~mode:(`Symbolic [`Group (`Add `Write); `Other (`Set (`List []))])
           dir2;
         assert_bool "mkdir" (test Is_dir dir2);
         if Sys.os_type <> "Win32" then begin
           assert_perm dir1 0o0755;
           assert_perm dir2 0o0770;
         end;
         rm ~recurse:true [dir1];
         assert_bool "no dir" (not (test Exists dir2));

         mkdir
           ~parent:true
           ~mode:(`Octal 0o0770)
           dir2;
         assert_bool "mkdir" (test Is_dir dir2);
         if Sys.os_type	<> "Win32" then begin
           assert_perm dir1 0o0755;
           assert_perm dir2 0o0770;
         end;
         rm ~recurse:true [dir1];
         assert_bool "no dir" (not (test Exists dir2)));

    "Mkdir with trailing slash" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir =
         Filename.concat tmp_dir (Filename.concat "nonexistent-dir" "")
       in
       mkdir ~parent:true dir;
       assert_bool "mkdir" (test Is_dir dir));

    "Find v0" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       with_bracket_chdir test_ctxt tmp_dir
         (fun _ ->
            let find_acc _ =
              find True "." (fun acc x -> reduce x :: acc) []
            in
            let lst_dot =
              find_acc "."
            in
            let lst_empty =
              find_acc ""
            in
              assert_bool "find '.' is empty" (lst_dot <> []);
              assert_bool "find '' is empty" (lst_empty <> []);
              assert_bool "find '.' <> find ''" (lst_dot = lst_empty)));

    "Find v1" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let sfs =
         SafeFS.create tmp_dir
           ["essai_dir"]
           ["essai_file"]
       in
       let set =
         find True tmp_dir (fun set fln -> SetFilename.add fln set)
           SetFilename.empty
       in
         assert_equal_set_filename
           (SetFilename.union sfs.SafeFS.dirs sfs.SafeFS.files)
           set);

    "Find v2" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let sfs =
         SafeFS.create tmp_dir
           ["essai_dir"]
           ["essai_file"]
       in
       let set =
         find Is_dir tmp_dir (fun set fln -> SetFilename.add fln set)
           SetFilename.empty
       in
         assert_equal_set_filename sfs.SafeFS.dirs set);

    "Find v3" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let sfs =
         SafeFS.create tmp_dir
           ["essai_dir"]
           ["essai_file"]
       in
       let set =
         find Is_file tmp_dir (fun set fln -> SetFilename.add fln set)
           SetFilename.empty
       in
         assert_equal_set_filename sfs.SafeFS.files set);

    "Find v4" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let sfs =
         SafeFS.create tmp_dir
           ["essai_dir"]
           ["essai_file"]
       in
       let set =
         find Is_file (Filename.concat tmp_dir "")
           (fun set fln -> SetFilename.add fln set)
           SetFilename.empty
      in
         assert_equal_set_filename sfs.SafeFS.files set);

    "Unix specific" >:::
    (
      let mk_symlink test_ctxt =
        let () =
          skip_if (Sys.os_type <> "Unix") "Symlink only works on Unix."
        in
        let tmp_dir = bracket_tmpdir test_ctxt in
        let symlink = make_filename [tmp_dir; "recurse"] in
        let sfs =
          SafeFS.create tmp_dir
            ["essai_dir"]
            ["essai_file"]
        in
          Unix.symlink current_dir symlink;
          SafeFS.mark sfs symlink;
          tmp_dir, symlink, sfs
      in
      let mk_filelink test_ctxt =
        let () =
          skip_if (Sys.os_type <> "Unix") "Symlink only works on Unix."
        in
        let tmp_dir = bracket_tmpdir test_ctxt in
        let symlink = make_filename [tmp_dir; "recurse"] in
        let source  = make_filename [tmp_dir; "essai_file"] in
        let sfs =
          SafeFS.create tmp_dir
            []
            ["essai_file"]
        in
        Unix.symlink source symlink;
        SafeFS.mark sfs symlink;
        tmp_dir, symlink, sfs
      in
      let mk_deadlink test_ctxt =
        let () =
          skip_if (Sys.os_type <> "Unix") "Symlink only works on Unix."
        in
        let tmp_dir = bracket_tmpdir test_ctxt in
        let dir = make_filename [tmp_dir; "dir1"] in
        let symlink = make_filename [dir; "dead"] in
          mkdir dir;
          Unix.symlink "non_existing.txt" symlink;
          tmp_dir, symlink, dir
      in
        [
          "Unix symlink" >::
          (fun test_ctxt ->
             let _, symlink, _ = mk_symlink test_ctxt in
               assert_bool "symlink is not a link" (test Is_link symlink);
               assert_bool "symlink is not a dir" (test Is_dir symlink));

          "Find v4 (link follow)" >::
          (fun test_ctxt ->
             let tmp_dir, _, _ = mk_symlink test_ctxt in
               try
                 find ~follow:Follow Is_dir tmp_dir (fun () _ -> ()) ();
                 assert_failure
                   "find follow should have failed, since there is \
                    recursive symlink"
               with RecursiveLink _ ->
                 ());

          "Find v5 (no link follow)" >::
          (fun test_ctxt ->
             let tmp_dir, fn, sfs = mk_symlink test_ctxt in
             let set =
               find ~follow:Skip Is_dir tmp_dir
                 (fun set fln -> SetFilename.add fln set)
                 SetFilename.empty
             in
               assert_bool "find symlink skip fails"
                 (SetFilename.equal set
                    (SetFilename.add fn sfs.SafeFS.dirs)));

          "Unix delete symlink" >::
          (fun test_ctxt ->
             let _, symlink, _ = mk_symlink test_ctxt in
               rm [symlink];
                 try
                   let _st: Unix.stats = Unix.lstat symlink in
                     assert_failure "rm symlink failed"
                 with Unix.Unix_error(Unix.ENOENT, _, _) ->
                   ());
          "Dead link + stat" >::
          (fun test_ctxt ->
             let _, symlink, _ = mk_deadlink test_ctxt in
             let st = stat symlink in
               assert_bool "is marked as a link" st.is_link;
               assert_equal ~msg:"is a link" Symlink st.kind;
               assert_raises
                 ~msg:"cannot dereference"
                 (FileDoesntExist symlink)
                 (fun () -> stat ~dereference:true symlink));

          "Dead link + test" >::
          (fun test_ctxt ->
             let _, symlink, _ = mk_deadlink test_ctxt in
               assert_bool "dead link exists"
                 (test Is_link symlink));

          "Dead symlink + rm" >::
          (fun test_ctxt ->
             let _, _, dir = mk_deadlink test_ctxt in
               rm ~recurse:true [dir]);

          "Dead symlink + cp -r" >::
          (fun test_ctxt ->
             let tmp_dir, _, dir1 = mk_deadlink test_ctxt in
             let dir2 = make_filename [tmp_dir; "dir2"] in
               cp ~recurse:true [dir1] dir2;
               try
                 (* test Is_link *)
                 let _st: Unix.stats =
                   Unix.lstat (make_filename [dir2; "dead"])
                 in
                 ()
               with Unix.Unix_error(Unix.ENOENT, _, _) ->
                 assert_failure "dead link not copied.");

          "Dead symlink + cp -r v2" >::
          (fun test_ctxt ->
             let tmp_dir, symlink, _ = mk_deadlink test_ctxt in
             let dir2 = make_filename [tmp_dir; "dir2"] in
               cp ~recurse:true [symlink] dir2;
               try
                 (* test Is_link *)
                 let _st: Unix.stats = Unix.lstat dir2 in
                 ()
               with Unix.Unix_error(Unix.ENOENT, _, _) ->
                 assert_failure "dead link not copied.");

          "Dead symlink + cp" >::
          (fun test_ctxt ->
             let tmp_dir, symlink, _ = mk_deadlink test_ctxt in
             let dir2 = make_filename [tmp_dir; "dir2"] in
               try
                 cp [symlink] dir2;
                 assert_failure "dead link should not copied."
               with FileDoesntExist _ ->
                 ());

          "Live filelink + cp" >::
          (fun test_ctxt ->
             let tmp_dir, symlink, _ = mk_filelink test_ctxt in
             let dest = make_filename [tmp_dir; "dest"] in
             cp [symlink] dest;
             assert_bool "regular" (not(test Is_link dest)));

          "Readlink" >::
          (fun test_ctxt ->
             let tmp_dir, fn, _ = mk_symlink test_ctxt in
             let real_tmp_dir = readlink tmp_dir in
               assert_bool
                 "tmp_dir resolve to non-empty directory"
                 (real_tmp_dir <> "");
               assert_equal
                 ~printer:(Printf.sprintf "%S")
                 real_tmp_dir (readlink fn));
        ]
    );

    "Chmod" >::
    (fun test_ctxt ->
       let () = skip_if (Sys.os_type <> "Unix") "Chmod only works on Unix." in
       let fn, chn = bracket_tmpfile test_ctxt in
       let () = close_out chn in

       let iter_chmod =
         List.iter
           (fun (ini, mode, exp) ->
              Unix.chmod fn ini;
              chmod mode [fn];
              assert_perm fn exp)
       in

       let () =
           iter_chmod
             [
               0o0000, `Symbolic [`User (`Add `Exec)],     0o0100;
               0o0100, `Symbolic [`User (`Remove `Exec)],  0o0000;
               0o0000, `Symbolic [`Group (`Add `Exec)],    0o0010;
               0o0010, `Symbolic [`Group (`Remove `Exec)], 0o0000;
               0o0000, `Symbolic [`Other (`Add `Exec)],    0o0001;
               0o0001, `Symbolic [`Other (`Remove `Exec)], 0o0000;
               0o0000, `Symbolic [`All (`Add `Exec)],      0o0111;
               0o0111, `Symbolic [`All (`Remove `Exec)],   0o0000;
               0o0000, `Symbolic [`User (`Add `ExecX)],    0o0000;
               0o0010, `Symbolic [`User (`Add `ExecX)],    0o0110;
               0o0001, `Symbolic [`User (`Add `ExecX)],    0o0101;
             ];
         iter_chmod
           [
               0o0200, `Symbolic [`User (`Add `Write)],     0o0200;
               0o0000, `Symbolic [`User (`Add `Write)],     0o0200;
               0o0200, `Symbolic [`User (`Remove `Write)],  0o0000;
               0o0000, `Symbolic [`Group (`Add `Write)],    0o0020;
               0o0020, `Symbolic [`Group (`Remove `Write)], 0o0000;
               0o0000, `Symbolic [`Other (`Add `Write)],    0o0002;
               0o0002, `Symbolic [`Other (`Remove `Write)], 0o0000;
               0o0000, `Symbolic [`All (`Add `Write)],      0o0222;
               0o0222, `Symbolic [`All (`Remove `Write)],   0o0000;
               0o0000, `Symbolic [`User (`Add `Read)],      0o0400;
               0o0400, `Symbolic [`User (`Remove `Read)],   0o0000;
               0o0000, `Symbolic [`Group (`Add `Read)],     0o0040;
               0o0040, `Symbolic [`Group (`Remove `Read)],  0o0000;
               0o0000, `Symbolic [`Other (`Add `Read)],     0o0004;
               0o0004, `Symbolic [`Other (`Remove `Read)],  0o0000;
               0o0000, `Symbolic [`All (`Add `Read)],       0o0444;
               0o0444, `Symbolic [`All (`Remove `Read)],    0o0000;
               0o0000, `Octal 0o644,                        0o0644;
               0o0100,
               (* u=r,g=u,u+w *)
               `Symbolic [`User (`Set `Read);
                          `Group (`Set `User);
                          `User (`Add `Write)],
               0o640;
           ]
       in
       let tmp_dir = bracket_tmpdir test_ctxt in
       let fn =  make_filename [tmp_dir; "essai6"] in
         touch fn;
         Unix.chmod fn 0o0000;
         chmod ~recurse:true (`Symbolic [`User (`Add `Read)]) [tmp_dir];
         assert_perm fn 0o0400);


    "Cp v1" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let file = make_filename [tmp_dir; "essai6"] in
       let fn0 = make_filename [tmp_dir; "essai0"] in
         touch fn0;
         cp [fn0] file;
         assert_bool "cp" (test Exists file));

    "Cp v2" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let file = make_filename [tmp_dir; "essai4"] in
       let fn0 = make_filename [tmp_dir; "essai0"] in
         touch fn0;
         cp [fn0] file;
         assert_bool "cp" (test Exists file));

    "Cp with space" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dirspace = make_filename [tmp_dir; "essai 7"] in
       let file = make_filename [dirspace; "essai0"] in
       let fn0 = make_filename [tmp_dir; "essai0"] in
         touch fn0;
         mkdir dirspace;
         cp [fn0] file;
         assert_bool "cp" (test Exists file));

    "Cp dir to dir" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir1 = make_filename [tmp_dir; "dir1"] in
       let dir2 = make_filename [tmp_dir; "dir2"] in
         mkdir dir1;
         touch (make_filename [dir1; "file.txt"]);
         cp ~recurse:true [dir1] dir2;
         assert_bool "cp" (test Exists (make_filename [dir2; "file.txt"]));
         cp ~recurse:true [dir1] dir2;
         assert_bool "cp dir" (test Is_dir (make_filename [dir2; "dir1"])));

    "Cp ACL" >::
    (fun test_ctxt ->
       let () = skip_if (Sys.os_type <> "Unix") "Unix file permissions only work on Unix." in
       let tmp_dir = bracket_tmpdir test_ctxt in
       let fn1 = make_filename [tmp_dir; "foo1.txt"] in
       let fn2 = make_filename [tmp_dir; "foo2.txt"] in
       let fn3 = make_filename [tmp_dir; "foo3.txt"] in
       begin
         touch fn1;
         Unix.chmod fn1 0o444;
         assert_perm fn1 0o444;
         cp [fn1] fn2;
         assert_perm fn2 0o444;
         Unix.chmod fn1 0o555;
         assert_perm fn1 0o555;
         cp [fn1] fn3;
         assert_perm fn3 0o555
      end);

    "Cp preserve" >::
    (fun test_ctxt ->
       let () = skip_if (Sys.os_type = "Win32") "Directory atime/mtime modification is broken on Windows." in
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir1 = make_filename [tmp_dir; "dir1"] in
       let fn1 = make_filename [dir1; "fn1.txt"] in
       let dir2 = make_filename [tmp_dir; "dir2"] in
       let fn2 = make_filename [dir2; "fn1.txt"] in
       let assert_equal_time ?msg exp got =
         assert_equal ?msg ~printer:string_of_float exp got
       in
         mkdir dir1;
         touch ~time:(Touch_timestamp 1.0) ~mtime:true fn1;
         touch ~time:(Touch_timestamp 2.0) ~atime:true fn1;
         touch ~time:(Touch_timestamp 3.0) ~mtime:true dir1;
         touch ~time:(Touch_timestamp 4.0) ~atime:true dir1;
         assert_equal_time ~msg:"fn1 mtime"  1.0 (stat fn1).modification_time;
         assert_equal_time ~msg:"fn1 atime"  2.0 (stat fn1).access_time;
         assert_equal_time ~msg:"dir1 mtime" 3.0 (stat dir1).modification_time;
         assert_equal_time ~msg:"dir1 atime" 4.0 (stat dir1).access_time;
         cp ~recurse:true ~preserve:true [dir1] dir2;
         assert_equal_time ~msg:"fn2 mtime"  1.0 (stat fn2).modification_time;
         assert_equal_time ~msg:"fn2 atime"  2.0 (stat fn2).access_time;
         assert_equal_time ~msg:"dir2 mtime" 3.0 (stat dir2).modification_time;
         assert_equal_time ~msg:"dir2 atime" 4.0 (stat dir2).access_time);

    "Cp POSIX" >::
    (fun test_ctxt ->
       let tmp_dir1 = bracket_tmpdir test_ctxt in
       let tmp_dir2 = bracket_tmpdir test_ctxt in
         touch (concat tmp_dir1 "foo.txt");
         with_bracket_chdir test_ctxt tmp_dir1
           (fun _ ->
              cp ~recurse:true [current_dir] tmp_dir2);
         assert_bool "file" (test Is_file (concat tmp_dir2 "foo.txt")));

    "Mv simple" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let file0 = make_filename [tmp_dir; "essai0"] in
       let file1 = make_filename [tmp_dir; "essai10"] in
       let file2 = make_filename [tmp_dir; "essai9"] in
         touch file0;
         cp [file0] file1;
         mv file1 file2;
         cp [file0] file1;
         mv file1 file2;
         assert_bool "mv" (test Exists file2));

    "Mv otherfs" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let file_test = make_filename [tmp_dir; "essai12"] in
       let sfs = SafeFS.create tmp_dir [] ["essai12"] in
       let file =
         let fn = Filename.temp_file ~temp_dir:(pwd ()) "otherfs" ".txt" in
           Sys.remove fn;
           bracket ignore
             (fun () _ ->
                rm ~force:(SafeFS.auto_ask_user sfs) [fn])
             test_ctxt;
           fn
       in
         mv file_test file;
         SafeFS.mark sfs file;
         assert_bool "mv" (test Exists file));

    "Rm simple" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let file = (make_filename [tmp_dir; "essai0"]) in
       let sfs = SafeFS.create tmp_dir [] ["essai0"] in
         rm ~force:(SafeFS.auto_ask_user sfs) [file];
         assert_bool "rm" (test (Not Exists) file));

    "Rm no recurse" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir = (make_filename [tmp_dir; "essai4"]) in
       let sfs = SafeFS.create tmp_dir ["essai4"] ["essai0"] in
         mkdir dir;
         assert_error
           "rm should have failed trying to delete a directory"
           (function
              | `NoRecurse _ -> true
              | _ -> false)
           (fun error -> rm ~error ~force:(SafeFS.auto_ask_user sfs) [dir]));

    "Rm ask duplicate" >::
    (fun test_ctxt ->
       let tmp_dir = bracket_tmpdir test_ctxt in
       let dir = make_filename [tmp_dir; "ask-duplicate"] in
       let sfs =
         SafeFS.create tmp_dir
           ["ask-duplicate"]
           [make_filename ["ask-duplicate"; "toto.txt"]]
       in
       let set_asked = ref SetFilename.empty in
       let set_duplicated = ref SetFilename.empty in
       let ask_register fn =
         if SetFilename.mem fn !set_asked then
           set_duplicated := SetFilename.add fn !set_duplicated;
         set_asked := SetFilename.add fn !set_asked;
         match SafeFS.auto_ask_user sfs with
           | Ask f -> f fn
           | _ -> false
       in
         rm ~force:(Ask ask_register) ~recurse:true [dir];
         assert_equal
           ~msg:"duplicate file asked when removing"
           SetFilename.empty
           !set_duplicated);

    "Which ocamlc" >::
    (fun _ ->
       try
         let _str: string = which "ocamlc" in
           ()
       with Not_found ->
         assert_failure "Cannot find ocamlc");

    "Umask" >::
    (fun test_ctxt ->
       let () = skip_if (Sys.os_type <> "Unix") "Umask only works on Unix." in
       assert_equal
         ~printer:(Printf.sprintf "0o%04o")
         test_umask
         (umask (`Octal (fun i -> i)));
       assert_equal
         ~printer:FileUtil.Mode.to_string
         [`User (`Set (`List [`Read; `Write; `Exec]));
          `Group (`Set (`List [`Read; `Exec]));
          `Other (`Set (`List [`Read; `Exec]))]
         (umask (`Symbolic (fun s -> s)));
       List.iter
         (fun (i, e) ->
            assert_equal
              ~printer:(Printf.sprintf "0o%04o")
              e (umask_apply i))
         [
           0o777, 0o755;
           0o1777, 0o1755
         ];
       with_bracket_umask test_ctxt test_umask
         (fun _ _ ->
            umask ~mode:(`Octal 0o0222) (`Octal ignore);
            assert_equal
              ~printer:(Printf.sprintf "0o%04o")
              0o0222 (umask (`Octal (fun i -> i))));
       with_bracket_umask test_ctxt test_umask
         (fun _ _ ->
            assert_raises
              (UmaskError("Cannot set sticky bit in umask 0o1222"))
              (fun () ->
                 umask ~mode:(`Octal 0o1222) (`Octal ignore)));
       List.iter
         (fun (s, e) ->
            with_bracket_umask test_ctxt test_umask
              (fun msk _ ->
                 assert_equal
                   ~msg:(Printf.sprintf
                           "0o%04o + %s -> 0o%04o"
                           msk (FileUtil.Mode.to_string s) e)
                   ~printer:(Printf.sprintf "0o%04o")
                   e (umask ~mode:(`Symbolic s) (`Octal (fun i -> i)))))
         [
           [`None (`Add `Read)], 0o0022;
           [`None (`Add (`List [`Read; `Write]))], 0o0000;
           [`All (`Remove `Read)], 0o0466;
           [`Group (`Set (`List [`Read; `Write; `Exec]))], 0o0002;
         ];
       ()
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
          test_case
            (fun _ ->
               assert_equal
                 ~printer:(fun s -> s)
                 str
                 (string_of_size ~fuzzy:fuzzy sz))
        in

          [
            "exact" >:::
            (List.map
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
               ]);

            "fuzzy" >:::
            (List.map
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
               ]);
          ]);

      "size_add" >:::
      (let test_of_vector (str, szs) =
         test_case
           (fun _ ->
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
           ]);

      "size_compare" >:::
      (
        let test_of_vector (sz1, sz2, res) =
          test_case
            (fun _ ->
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
                   cmp)
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
            ]);
    ];
  ]


let () =
  let _i: int = if Sys.os_type = "Win32" then 0 else Unix.umask test_umask in
  run_test_tt_main
    ("ocaml-fileutils" >:::
     [
       "FilePath" >:::
       [
         test_unix;
         test_win32;
       ];

       test_fileutil;
     ])
