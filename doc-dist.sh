#!/bin/bash
##############################################################################
#  ocaml-fileutils: files and filenames common operations                    #
#                                                                            #
#  Copyright (C) 2003-2014, Sylvain Le Gall                                  #
#                                                                            #
#  This library is free software; you can redistribute it and/or modify it   #
#  under the terms of the GNU Lesser General Public License as published by  #
#  the Free Software Foundation; either version 2.1 of the License, or (at   #
#  your option) any later version, with the OCaml static compilation         #
#  exception.                                                                #
#                                                                            #
#  This library is distributed in the hope that it will be useful, but       #
#  WITHOUT ANY WARRANTY; without even the implied warranty of                #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         #
#  COPYING for more details.                                                 #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this library; if not, write to the Free Software Foundation,   #
#  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             #
##############################################################################

. admin-gallu-common || exit 1

set -e

arg_string_set version --default "dev" \
  "Version of OUnit."

arg_parse arg_anon_fail "$@"

CURDIR=$(pwd)

TOPDIR="ocaml-fileutils-doc-$version"
get_tmpdir TEMPDIR
mkdir -p "$TEMPDIR/$TOPDIR/api-fileutils"
cp -R _build/src/api-fileutils.docdir/* "$TEMPDIR/$TOPDIR/api-fileutils"

tar czf "$CURDIR/dist/$TOPDIR.tar.gz" -C $TEMPDIR $TOPDIR
