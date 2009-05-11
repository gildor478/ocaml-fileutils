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

BUILDDIR=$(CURDIR)/_build/libfileutils-ocaml
OCAMLBUILDFLAGS+=-classic-display -no-log

test:

_build/myocamlbuild: myocamlbuild.ml
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -just-plugin
	-cp $@.exe $@

myocamlbuild: _build/myocamlbuild

all: myocamlbuild
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) fileutils.otarget 

clean:
	$(if $(OCAMLBUILD),-$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -clean)

distclean: clean
	-$(RM) -r "$(CURDIR)/autom4te.cache"
	-$(RM) "$(CURDIR)/config.cache"
	-$(RM) "$(CURDIR)/config.log"
	-$(RM) "$(CURDIR)/config.status"
	-$(RM) "$(CURDIR)/fileutils.itarget"
	-$(RM) "$(CURDIR)/TopMakefile"
	-$(RM) "$(CURDIR)/libfileutils-ocaml/META"

install: all
	$(INSTALL) -d $(htmldir)/api
	$(INSTALL_DATA) -t $(htmldir)/api \
	  $(wildcard $(BUILDDIR)/fileutils.docdir/*)
	$(OCAMLFIND) install \
	  fileutils \
	  "$(CURDIR)/libfileutils-ocaml/META" \
	  "$(BUILDDIR)/fileutils.cma" \
	  "$(BUILDDIR)/fileutils-str.cma" \
	  "$(BUILDDIR)/fileUtil.cmi" \
	  "$(BUILDDIR)/fileUtil.ml" \
	  "$(BUILDDIR)/fileUtilStr.cmi" \
	  "$(BUILDDIR)/fileUtilStr.ml" \
	  "$(BUILDDIR)/filePath.cmi" \
	  "$(BUILDDIR)/filePath.mli" \
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
dist:
	svn export "$(CURDIR)" "$(CURDIR)/$(DISTDIR)"
	# TODO: fix this ocaml.m4 copy
	cp -L "$(CURDIR)/m4/ocaml.m4" "$(CURDIR)/$(DISTDIR)/m4/ocaml.m4"
	cd "$(CURDIR)/$(DISTDIR)" && ./autogen.sh
	tar czf "$(DISTDIR).tar.gz" "$(DISTDIR)"
	$(RM) -r "$(CURDIR)/$(DISTDIR)"
	gpg -s -a -b "$(DISTDIR).tar.gz"
	echo Don't forget to tag version $(PACKAGE_VERSION)

test: all
	cd "$(CURDIR)/_build/test" && ./test.$(ocamlbuild_best_program)

headache:
	find ./ -name .svn -prune -false -o -name _build -prune -false -o -type f | xargs headache -h .header -c .headache.config


.PHONY: all clean distclean install uninstall dist test myocamlbuild headache
