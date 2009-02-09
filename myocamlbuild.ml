
    open Ocamlbuild_plugin

let dispatch_ocamlfind = 
  (* These functions are not really officially exported *)
  let run_and_read = 
    Ocamlbuild_pack.My_unix.run_and_read
  in

  let blank_sep_strings = 
    Ocamlbuild_pack.Lexers.blank_sep_strings
  in

  (* This lists all supported packages *)
  let find_packages () =
    blank_sep_strings &
      Lexing.from_string &
      run_and_read "ocamlfind list | cut -d' ' -f1"
  in

  (* This is supposed to list available syntaxes, but I don't know how to do it.
   *)
  let find_syntaxes () = 
    ["camlp4o"; "camlp4r"]
  in

  (* ocamlfind command *)
  let ocamlfind x = 
    S[A"ocamlfind"; x]
  in

    function
      | Before_options ->
          (* override default commands by ocamlfind ones *)
          Options.ocamlc   := ocamlfind & A"ocamlc";
          Options.ocamlopt := ocamlfind & A"ocamlopt";
          Options.ocamldep := ocamlfind & A"ocamldep";
          Options.ocamldoc := ocamlfind & A"ocamldoc"
                                
      | After_rules ->
          (* When one link an OCaml library/binary/package, one should use 
           * -linkpkg
           *)
          flag ["ocaml"; "link"] & A"-linkpkg";
                
          (* For each ocamlfind package one inject the -package option when 
           * compiling, computing dependencies, generating documentation and 
           * linking.
           *)
           List.iter 
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
            end 
            (find_packages ());
          (* Like -package but for extensions syntax. Morover -syntax is useless 
           * when linking. 
           *)
           List.iter 
            begin fun syntax ->
              flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
              flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
              flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
            end 
            (find_syntaxes ());

          (* The default "thread" tag is not compatible with ocamlfind. Indeed, the
           * default rules add the "threads.cma" or "threads.cmxa" options when
           * using this tag. When using the "-linkpkg" option with ocamlfind, this
           * module will then be added twice on the command line.  To solve this,
           * one approach is to add the "-thread" option when using the "threads"
           * package using the previous plugin. 
           *)
           flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
           flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);

      | _ -> ()
;;

let _ = 
  dispatch 
    begin
      function
        | After_rules as e ->
            dispatch_ocamlfind e;

            ocaml_lib "src/libuname-ocaml/uname";

            flag 
              ["link"; "library"; "ocaml"; "byte"; "use_libuname"]
              (S[A"-dllib"; A"-luname"; A"-cclib"; A"-luname"]);

            flag 
              ["link"; "library"; "ocaml"; "native"; "use_libuname"]
              (S[A"-cclib"; A"-luname"]);

            dep 
              ["link"; "ocaml"; "use_libuname"] 
              ["src/libuname-ocaml/libuname.a"];

        | e ->
            dispatch_ocamlfind e
    end
;;

