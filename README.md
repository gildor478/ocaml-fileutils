Fileutils - OCaml API to manipulate real files (POSIX like) and filenames
=========================================================================

This library provides an API to perform POSIX like operations on files like:

- mv
- cp
- rm
- mkdir
- touch
- which...

It also providesa module to manipulate abstract filenames:

- classification
- make_relative: made a filename relative to another
- make_absolute

[![Travis status][travis-img]][travis]
[![AppVeyor status][appveyor-img]][appveyor]

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
