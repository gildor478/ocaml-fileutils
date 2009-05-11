##########################################################################
#   Ocaml-fileutils                                                      #
#                                                                        #
#   Copyright (C) 2003, 2004 Sylvain Le Gall <sylvain@le-gall.net>       #
#                                                                        #
#   This program is free software; you can redistribute it and/or        #
#   modify it under the terms of the GNU Library General Public          #
#   License as published by the Free Software Foundation; either         #
#   version 2 of the License, or any later version ; with the OCaml      #
#   static compilation exception.                                        #
#                                                                        #
#   This program is distributed in the hope that it will be useful,      #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of       #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 #
#   See the LICENCE file for more details.                               #
#                                                                        #
#   You should have received a copy of the GNU General Public License    #
#   along with this program; if not, write to the Free Software          #
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA             #
#   02111-1307  USA                                                      #
#                                                                        #
#   Contact: sylvain@le-gall.net                                         #
#                                                                        #
##########################################################################

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


.PHONY: all clean distclean install uninstall dist test myocamlbuild
