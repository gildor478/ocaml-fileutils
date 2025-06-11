(* Fileutils provides two modules:
     - FilePath is used for platform-agnostic file path manipulation, providing
       more functionality than the Standard Library Filename module
     - FileUtil implements some of the POSIX file manipulation programs such
       as cp and rm, but implemented in pure, portable OCaml.
*)

(* Some examples from the FilePath module for file path manipultation. *)
let filepath () =
  Printf.printf "Current dir %S\n"
    FilePath.current_dir;
  Printf.printf "Parent dir %S\n"
    FilePath.parent_dir;
  Printf.printf "Make a filename from parts: %S\n"
    (FilePath.make_filename ["one"; "two"; "three"]);
  Printf.printf "Simplify a filename: %S\n"
    (FilePath.reduce (FilePath.make_filename [FilePath.current_dir; "one"]));
  Printf.printf "Replace an extension: %S\n"
    (FilePath.replace_extension "foo.jpg" "jpeg");
  Printf.printf "Making a PATH string: %S\n"
    (FilePath.string_of_path ["one"; FilePath.make_filename ["two"; "three"]]);
  let structure = [FilePath.parent_dir; "one"; "two"; "three"] in
    Printf.printf "Unix: %S\nWindows: %S\nCygwin: %S\nDefault: %S = FilePath.: %S\n"
      (FilePath.UnixPath.make_filename structure)
      (FilePath.Win32Path.make_filename structure)
      (FilePath.CygwinPath.make_filename structure)
      (FilePath.DefaultPath.make_filename structure)
      (FilePath.make_filename structure)

(* The main FileUtil module, for POSIX-style filesystem manipulation. *)
let fileutil () =
  (* Make a directory for our example. *)
  begin try
    FileUtil.mkdir ~parent:true (FilePath.make_filename ["mkdir"; "example"])
  with
    e -> Printf.printf "mkdir error: %S\n" (Printexc.to_string e); raise e
  end;
  (* Copy a couple of files into it with *)
  begin try
    FileUtil.cp
      ["dune-workspace"; "dune-project"] 
      (FilePath.make_filename ["mkdir"; "example"])
  with
    e -> Printf.printf "cp error: %S\n" (Printexc.to_string e); raise e
  end;
  (* List directory contents *)
  begin try
    List.iter
      (fun x -> Printf.printf "%s\n" (FilePath.basename x))
      (FileUtil.ls (FilePath.make_filename ["mkdir"; "example"]))
  with
    e -> Printf.printf "ls error: %S\n" (Printexc.to_string e); raise e
  end;
  (* Compare the two files *)
  begin match
    FileUtil.cmp
      (FilePath.make_filename ["mkdir"; "example"; "dune-workspace"])
      (FilePath.make_filename ["mkdir"; "example"; "dune-project"])
  with
    | None -> Printf.printf "No difference\n"
    | Some -1 -> Printf.printf "Could not complete comparison operation\n"
    | Some p -> Printf.printf "Difference at position %i\n" p
  end;
  (* Delete everything to clean up *)
  begin try
    FileUtil.rm ~recurse:true ["mkdir"]
  with
    e -> Printf.printf "rmdir error: %S\n" (Printexc.to_string e); raise e
  end

let () =
  match Sys.argv with
  | [|_; "filepath"|] -> filepath ()
  | [|_; "fileutil"|] -> fileutil ()
  | _ -> Printf.eprintf "fileutils example: unknown command line\n"
