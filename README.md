Fileutils - OCaml API to manipulate real files (POSIX like) and filenames
=========================================================================

[![Travis status][travis-img]][travis]
[![AppVeyor status][appveyor-img]][appveyor]

Features of the project:

* pure OCaml
* file functions inspired from GNU fileutils (aiming to be POSIX compatible)
    * cp: copy files and directories
    * mv: rename files and directories
    * rm: remove files and directories
    * test: check file types and compare values
    * find: find files that match certain criteria
    * mkdir: create directory and its parents
    * ls: list content of a directory
    * touch: change file timestamps
    * which: locate a command
    * readlink: resolve symlink
    * du: compute disk usage
    * stat: abstract of Unix.stat
    * cmp: compare files
    * chmod: change permissions of a file
* filename functions support Win32/Unix/MacOS and Cygwin filenames:
    * Compare: is_subdir, is_updir, compare
    * Transform: make_absolute, make_relative, reduce
    * Extension: chop_extension, check_extension

[travis]:         https://travis-ci.org/gildor478/ocaml-fileutils
[travis-img]:     https://travis-ci.org/gildor478/ocaml-fileutils.svg?branch=master
[appveyor]:       https://ci.appveyor.com/project/gildor478/ocaml-fileutils/branch/master
[appveyor-img]:   https://ci.appveyor.com/api/projects/status/pddhb2c22rc8wtd3/branch/master?svg=true
[opam]:           https://opam.ocaml.org

Installation
------------

The recommended way to install fileutils is via the [opam package manager][opam]:

```sh
$ opam install fileutils
```

Documentation
-------------

API documentation is
[available online](https://gildor478.github.io/ocaml-fileutils).
