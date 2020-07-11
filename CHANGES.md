## v0.6.3 - 2020-07-11

## Fixed

- Fix mkdir when trying to create directory with ~parent:true and a directory
  ending with "/". For example:
```
mkdir ~parent:true "non-existent/"
```
(Closes: #14)

## v0.6.2 - 2020-03-26

### Fixed

- Improve documentation:
  - hide modules for the implementation.
  - clarify usage of chop_extension/add_extension.
- Move fileutils and fileutils.str in their own directories.

## v0.6.1 - 2019-09-12

### Fixed

- Migrate the build system to dune (thanks to zapashcanon).
- Migrate CHANGELOG to CHANGES for dune-release.

## v0.6.0 - 2019-08-25

### Fixed

- Migrate CHANGELOG to [Keep a Changelog] format.
- umask returns 0 on Windows, this is consistent with
  [Perl, Python and Ruby
  behavior](https://github.com/gildor478/ocaml-fileutils/pull/6#issuecomment-509062371).
  Thanks to @dmbaturin.

### Removed

- Support for MacOS 9, since OCaml doesn't support it since 3.09 (Closes: #8).

## v0.5.3 - 2017-11-12

* Minor release:
	- Use bytes rather than string to be compatible with OCaml 4.06.0.
	(Closes: #5)

## v0.5.2 - 2017-05-23

* Minor release:
	- Test file existence with Unix.LargeFile.lstat in FileUtilRM.
	(Closes: OF#1749)

## v0.5.1 - 2016-11-02

* Minor release:
	- Fix non POSIX behavior of cp with links when "recurse:false".
	(Closes: OF#1649)

## v0.5.0 - 2015-07-10

* Major release to account all the API changes:
* Rebuild the exception/reporting framework:
	- Remove exceptions in favor of a single exception per command and a
		polymorphic variant tag.
	- Use a reporting function that can be passed as  a parameter
		[?error:'a error_handler] to most of the functions.
* Reimplement functions to be more POSIX compliant implementation (Closes: OF#761):
	(functions: cp, umask, chmod, mkdir, rm, mv, touch)
* Make sure dead symlinks are handled properly (Closes: OF#712, OF#711):
	- derefenced when needed (functions: test)
	- offer the choice when possible (function: stat)
* Implement symbolic mode that may have contextual meaning.
* Improve documentation (add links to POSIX doc, reorganize content in section).
* Split FileUtil.ml into multiple files.
* Implement chmod (Closes: OF#416).
* [cp] now propagate timestamp when invoked with [~preserve] (Closes: OF#709).
* Upgrade OUnit to OUnit2.
* Fix typo in cp (Closes: OF#816, OF#1317).

## v0.4.5 - 2013-06-03

* Fix fd leaking cmp (Closes: OF#1012).
* Fix test suite for BSD system.

## v0.4.4 - 2012-06-12

* Regenerate with oasis 0.3.0~rc6

## v0.4.3 - 2011-05-26

* OASIS enabled

## v0.4.2 - 2010-09-06

* Apply patch from Rüdiger Schmitt, fix handling for '.' in find and ls
	(Close: OF#418, OF#736)

## v0.4.1 - 2010-09-01

* Apply patch from S. Glondu to use the right find function in FileUtilStr
	(Closes: OF#731)
* Fix some typo in documentation
* Apply patch from Debian to use a byte plugin for ocamlbuild

## v0.4.0 - 2009-09-09

* Simplify interface, avoid nested module when possible:
	* Add filename information to all exception
	* FileUtil:
		* size is now a 64bits integer, functions are restricted to 4 most useful
			operations
		* Str match is now separated into another module (FileUtilStr, package
			fileutils-str)
		* All operations are now directly in FileUtil and not in FileUtil.StrUtil
	* FilePath:
		* Remove is_implicit, use is_relative as replacement
		* All functions of FilePath.DefaultPath are now directly accessible in
			FilePath
		* Default operation on string, use sub-module Abstract for abstract
			operations
		 * FilePath.reduce don't reduce ".." except if asked to (i.e. no symlink)
		 * CygwinPath related function use directly UnixPath
* Make documentation more clear
* Introduce fast operation for string filename: when possible to operate
	directly on string use it
* Drop parser/lexer for path: this is complicated and not efficient. Prefer
	simple string manipulation which is more efficient
* Replace build system by ocamlbuild, ocamlfind, a simple Makefile,
	ocaml-autoconf macros and configure
* Adapt compilation and test to Windows
* Simplify rm and avoid asking question twice (Closes: #FS79)
* Use Unix.LargeFile to handle huge file (Closes: FS#77)
* Simplify size operation. Now all operation is done on Int64 (Closes: FS#76)
* Implement FileUtilStr that allow Str.regexp match outside the core
	FileUtil module (Closes: FS#13)
* Add a wildcard on .a and .lib to allow installation on Windows
	(Closes: FS#84)
* Update license header (Closes: FS#8, FS#55)
* Accept "/" as separator for Win32 (Closes: FS#78, FS#83, FS#68)
* For win32, use PATHEXT to locate executable with "which" (Closes: FS#73)
* Don't suppose ".." can be reduced and test it (Closes: FS#10)
* Fix "mv" and allow to copy data between filesystem (Closes: FS#6, FS#7)
* Optimize FileUtil.find speed, now only 2x slower than UNIX find (was 40x slower before)
	(Closes: FS#65)

## v0.3.0

* Change the version to 0.3 (lot of changes for a minor version)
* Update webpages
* Correct a bug that prevent sr\@Ltn to be parsed (which comes from the
	lexer of UnixPath, there is [^'.''/''\\']* which can produce empty token)
* Correct a bug that prevent to parse the initial current dir (ie produce nothing
	when use find "." or find "/a/")

## v0.2.2

* Changes the version to 0.2.2 in TopMakefile.in (closes: FS#33)
* Stop removing Makefile in distclean target (closes: FS#31)
* Change --enable-docdir --enable-builddir to --withXX (closes: FS#32)
* Configure now test that ocamlfind is not detected and that we want to
	use ocamlfind (closes: FS#34)
* Correct error concerning parsing of "" as a current dir (closes: FS#40)
* Correct error concerning the test Has_extension (closes: #41)
* Use a new CurrenDir of (Long|Short) to denote the difference between "" and "."
* Implement readlink
* Implement pwd (closes: FS#39)
* Implement cmp (closes: FS#37, FS#38)
* Implement new test: Has_no_extension | Basename_is | Dirname_is
* Implement an anti recursion system (experimental, need to be tested) :
	* Use a type action_link: Follow, Skip, SkipInform, AskFollow
	* Maitain a set of visited directories
* Implement new test: Is_older_than_date, Is_newer_than_date, Size_bigger_than,
	Size_smaller_than, Size_equal_to, Size_fuzzy_equal_to, Custom
* Rewrite the test: Is_older_than, Is_newer_than, now takes only one args
* Implement type size and operation coming along (add, sub, convert, compare,
	string_of_size).
* Implement type permission / base_permission and operation coming along (
	permission_of_int, int_of_permission).
* Implement type kind (Dir, File...).
* Implement function stat
* Rewrite find, in order to be able to execute codes foreach filename. Very useful
	for rewriting other functions (rm, cp, mv)
* Use list argument in place of single filename for rm, cp
* Fix a bug that prevent ls to be able to list ""
* Reworked unitary tests: include test for symlink and anti recursion
* Unitary tests change from Fort to OUnit test suite

## v0.2.1

* Minor bug fixes to correct website aspect

## v0.2.0

* Use module/functor to abstract a lot of operation.
* Generate a decent ocamldoc documentation
* Abstract regexp matching using functor
* Separate the sysPath modules in two: Abstract and not. Abstract
	permits to parse once and for all the filename, and then operate
	on it. It allows to handle fast all operation. Concrete module
	are only proxy that do the conversion to/from the Abstract
	implementation.
* Introduce relation (updir, subdir, compare) to allow manipulating
	filename in classical structure (Set, Map...)
* Rename sysPath, sysUtil to filePath, fileUtil since it appears that it is
	more consistent regarding the name of the library (i was not convinced, that
	sysPath represents anything).

## v0.1.1 - 2004-01-30

* Fix some weird comportement with reduce (especially
	when trying to reduce filename which try to .. a root)
	and add the possibility to reduce relative filename
* Rework on the way everything is made :
	* Support 4 different scheme of filename (Unix (the
		native way), MacOS, Win32, Cygwin)
	* Each scheme use a parser/lexer to decompose his
		filename and a .ml to handle the whole discriminant
		element of a specific scheme (ie the way filename are
		decomposed and the way path like variable are decomposed)
	* All the operation are defined relatively to the
		discriminant operation in a functorized module
	* Each scheme produces a module from his discriminant element
		and from the generic operation. These modules are defined in
		SysPath.{UnixPath|Win32Path|CygwinPath|MacOSPath}.
	* Depending on the current environement one of the module above
		is the default binding for all the operation.
* Add SysUtil which try to create some portable file operation :
	mv, cp, touch, mkdir, test, find. This module abandon any non
	cross platform operation and will never support it (ie links for
	example, won't be supported).
* This release is an alpha release. 0.2 will be the stable one.


## BTS references

* FS#XX: Flyspray BTS (pre-2008)
* OF#XX: OCaml Forge BTS (pre-2019)
