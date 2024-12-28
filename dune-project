(lang dune 2.9)
(name fileutils)

(explicit_js_mode)

(generate_opam_files)

(source (github gildor478/ocaml-fileutils))
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(authors "Sylvain Le Gall")
(maintainers "Sylvain Le Gall <sylvain+ocaml@le-gall.net>")
(documentation "https://gildor478.github.io/ocaml-fileutils/")

(package
  (name fileutils)
  (synopsis "XDG basedir location for data/cache/configuration files")
  (description
"\| This library provides an API to perform POSIX like operations on files like:
"\|
"\| - mv
"\| - cp
"\| - rm
"\| - mkdir
"\| - touch
"\| - which...
"\|
"\| It also provides a module to manipulate abstract filenames:
"\|
"\| - classification
"\| - make_relative: made a filename relative to another
"\| - make_absolute
 )
  (depends
    base-unix
    (ounit2 (and (>= 2.0.0) :with-test))
    (ocaml (>= 4.14))))
