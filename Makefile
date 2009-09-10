################################################################################
#  ocaml-fileutils: files and filenames common operations                      #
#                                                                              #
#  Copyright (C) 2003-2009, Sylvain Le Gall                                    #
#                                                                              #
#  This library is free software; you can redistribute it and/or modify it     #
#  under the terms of the GNU Lesser General Public License as published by    #
#  the Free Software Foundation; either version 2.1 of the License, or (at     #
#  your option) any later version, with the OCaml static compilation           #
#  exception.                                                                  #
#                                                                              #
#  This library is distributed in the hope that it will be useful, but         #
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  #
#  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          #
#  details.                                                                    #
#                                                                              #
#  You should have received a copy of the GNU Lesser General Public License    #
#  along with this library; if not, write to the Free Software Foundation,     #
#  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               #
################################################################################

-include TopMakefile

BUILDDIR=./_build/src
SRCDIR=./src
OCAMLBUILDFLAGS+=-classic-display -no-log

all:

_build/myocamlbuild: myocamlbuild.ml
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -just-plugin
	-cp $@.exe $@

myocamlbuild: _build/myocamlbuild

all: myocamlbuild
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) \
	  $(SRCDIR)/fileutils.cma \
	  $(SRCDIR)/fileutils-str.cma \
	  $(SRCDIR)/fileutils.$(ocamlbuild_best_library) \
	  $(SRCDIR)/fileutils-str.$(ocamlbuild_best_library) \
	  $(SRCDIR)/fileutils.docdir/index.html 

clean:
	$(if $(OCAMLBUILD),-$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -clean)

distclean: clean
	-$(RM) -r "autom4te.cache"
	-$(RM) "config.cache"
	-$(RM) "config.log"
	-$(RM) "config.status"
	-$(RM) "TopMakefile"

install: all
	$(INSTALL) -d $(htmldir)/api
	$(INSTALL_DATA) -t $(htmldir)/api \
	  $(wildcard $(BUILDDIR)/fileutils.docdir/*)
	$(OCAMLFIND) install \
	  -patch-version $(PACKAGE_VERSION) \
	  fileutils \
	  "$(SRCDIR)/META" \
	  "$(BUILDDIR)/fileutils.cma" \
	  "$(BUILDDIR)/fileutils-str.cma" \
	  "$(BUILDDIR)/FileUtil.cmi" \
	  "$(BUILDDIR)/FileUtil.ml" \
	  "$(BUILDDIR)/FileUtilStr.cmi" \
	  "$(BUILDDIR)/FileUtilStr.ml" \
	  "$(BUILDDIR)/FilePath.cmi" \
	  "$(BUILDDIR)/FilePath.mli" \
	  $(wildcard $(BUILDDIR)/fileutils.cmxa) \
	  $(wildcard $(BUILDDIR)/fileutils.a) \
	  $(wildcard $(BUILDDIR)/fileutils.lib) \
	  $(wildcard $(BUILDDIR)/fileutils-str.cmxa) \
	  $(wildcard $(BUILDDIR)/fileutils-str.a) \
	  $(wildcard $(BUILDDIR)/fileutils-str.lib) \
	  $(wildcard $(BUILDDIR)/*.cmx) 

uninstall:
	-$(RM) -r $(htmldir)/api
	-$(OCAMLFIND) remove fileutils

DISTDIR=$(PACKAGE_TARNAME)-$(PACKAGE_VERSION)
TARBALL=$(DISTDIR).tar.gz
SVN_TRUNK=$(shell LC_ALL=en_US svn info | sed -n -e 's/^URL: \(.*\)$$/\1/p')
SVN_TAG=$(dir $(SVN_TRUNK))/tags/$(PACKAGE_VERSION)
dist:
	svn export . "$(DISTDIR)"
	cp -L "m4/ocaml.m4" "$(DISTDIR)/m4/ocaml.m4"
	cd "$(DISTDIR)" && ./autogen.sh
	$(RM) -r "$(DISTDIR)/autom4te.cache"
	tar czf "$(TARBALL)" "$(DISTDIR)"
	$(RM) -r "$(DISTDIR)"
	gpg -s -a -b "$(TARBALL)"
	# Probing tags
	if ! svn ls $(SVN_TAG); then echo svn copy . $(SVN_TAG); fi

test: all
ifeq ($(BUILD_TEST),yes)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) \
	  test/test.$(ocamlbuild_best_program) \
	  test/BenchFind.$(ocamlbuild_best_program)
	cd "_build/test" && ./test.$(ocamlbuild_best_program) $(TESTFLAGS)
else
	echo "Test cannot be built" >&2 && exit 1
endif

headache:
	find ./ -name .svn -prune -false -o -name _build -prune -false -o -type f \
	  | xargs headache -h .header -c .headache.config

bench-find: all
	_build/test/BenchFind.native

.PHONY: all clean distclean install uninstall dist test myocamlbuild headache bench-find
