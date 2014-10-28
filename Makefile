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

defaultl: test

# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

# Precommit target
#  Check style of code.
PRECOMMIT_ARGS= \
	    --exclude myocamlbuild.ml \
	    --exclude setup.ml \
	    --exclude README.txt \
	    --exclude INSTALL.txt \
	    --exclude Makefile \
	    --exclude configure \
	    --exclude _tags

precommit:
	-@if command -v OCamlPrecommit > /dev/null; then \
	  OCamlPrecommit $(PRECOMMIT_ARGS); \
	else \
	  echo "Skipping precommit checks.";\
	fi

precommit-full:
	OCamlPrecommit --full $(PRECOMMIT_ARGS)

test: precommit

.PHONY: precommit

# Headache target
#  Fix license header of file.

headache:
	find ./ \
		-name _darcs -prune -false -o \
		-name .git -prune -false -o \
		-name _build -prune -false -o \
		-type f \
		| xargs headache -h _header -c _headache.config

.PHONY: headache

doc-dev-dist: doc fix-perms
	./doc-dist.sh --version dev

.PHONY: doc-dev-dist

# Deploy target
#  Deploy/release the software.

deploy:
	./doc-dist.sh --version $(shell oasis query version)
	admin-gallu-deploy --verbose \
		--forge_upload --forge_group ocaml-fileutils --forge_user gildor-admin \
	  --forge_extra_file "dist/ocaml-fileutils-doc-$(shell oasis query version).tar.gz"
	admin-gallu-oasis-increment \
	  --setup_run --setup_args "-setup-update dynamic" --use_vcs

.PHONY: deploy

fix-perms:
	chmod +x doc-dist.sh

.PHONY: fix-perms

website-clean:
	cd website && $(MAKE) clean

clean: website-clean

website-distclean:
	cd website && $(MAKE) distclean

distclean: website-distclean

.PHONY: website-distclean website-clean
