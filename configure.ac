dnl ********************************************************************************
dnl *  ocaml-fileutils: files and filenames common operations                      *
dnl *                                                                              *
dnl *  Copyright (C) 2003-2009, Sylvain Le Gall                                    *
dnl *                                                                              *
dnl *  This library is free software; you can redistribute it and/or modify it     *
dnl *  under the terms of the GNU Lesser General Public License as published by    *
dnl *  the Free Software Foundation; either version 2.1 of the License, or (at     *
dnl *  your option) any later version, with the OCaml static compilation           *
dnl *  exception.                                                                  *
dnl *                                                                              *
dnl *  This library is distributed in the hope that it will be useful, but         *
dnl *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *
dnl *  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *
dnl *  details.                                                                    *
dnl *                                                                              *
dnl *  You should have received a copy of the GNU Lesser General Public License    *
dnl *  along with this library; if not, write to the Free Software Foundation,     *
dnl *  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *
dnl ********************************************************************************

AC_INIT(ocaml-fileutils, 0.4.0)

AC_CONFIG_SRCDIR([src/FileUtil.ml])

AC_PROG_INSTALL

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install ocamlc])
fi

if test "$OCAMLBUILD" = "no"; then
  AC_MSG_ERROR([You must install ocamlbuild])
fi

if test "$OCAMLDOC" = "no"; then
  AC_MSG_ERROR([You must install ocamldoc])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install ocamlfind (findlib)])
fi

AC_PROG_OCAMLLEX
if test "$OCAMLLEX" = "no"; then
  AC_MSG_ERROR([You must install ocamllex])
fi

AC_PROG_OCAMLYACC
if test "$OCAMLYACC" = "no"; then
  AC_MSG_ERROR([You must install ocamlyacc])
fi

if test "$OCAMLBEST" = "opt"; then
  ocamlbuild_best_library=cmxa
  ocamlbuild_best_program=native
else
  ocamlbuild_best_library=cma
  ocamlbuild_best_program=byte
fi
AC_SUBST(ocamlbuild_best_library)
AC_SUBST(ocamlbuild_best_program)

AC_CHECK_OCAML_PKG([oUnit])
BUILD_TEST="yes"
if test "$OCAML_PKG_oUnit" = "no"; then
  AC_MSG_WARN(["You must install oUnit library to enable test"])
  BUILD_TEST="no"
fi
AC_SUBST(BUILD_TEST)

AC_CONFIG_FILES([TopMakefile])
AC_OUTPUT
