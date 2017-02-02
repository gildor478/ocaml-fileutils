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

INKSCAPE=inkscape
COMPOSITE=composite
PANDOC=pandoc
TAR=tar
CURL=curl
FAB=fab
LINKCHECKER=linkchecker

# Name of the product.
NAME = ocaml-fileutils
API_NAME = fileutils

# Determine if we can scrape host.
ONLINE := $(shell (ping -c 1 forge.ocamlcore.org > /dev/null 2>&1 && echo true) || echo false)

# HTML page to scrape for data.
FORGE_PAGE = "http://forge.ocamlcore.org/frs/?group_id=128"

# Dev documetation link.
DEV_DOC_URL = "file:///$(shell pwd)/dist/ocaml-fileutils-doc-dev.tar.gz"

default: all

include Makefile.scrape
include website-tools/Makefile.common

GENERATED_HTML=$(patsubst mkd/%.mkd,html/%.html,$(wildcard mkd/*.mkd))
GENERATED_HTML+= html/index.html
GENERATED_IMG=html/logo.png

all: $(GENERATED_HTML) $(GENERATED_IMG) extract-api-documentation

.PHONY: all

clean:: distclean
	-$(RM) $(GENERATED_HTML) $(GENERATED_IMG) mkd/documentation.mkd mkd/index.mkd
	-$(RM) Makefile.scrape

distclean::

.PHONY: clean

html/%.png: images/%.svg
	inkscape $(INKSCAPEFLAGS) -e $@ $< > /dev/null

#
# Checkout external modules.
#
# Get program from gildor478/website-tools to build everything.
#

GIT_WEBSITE_TOOLS=https://github.com/gildor478/website-tools.git
checkout-website-tools:
	if ! test -d website-tools ; then git clone $(GIT_WEBSITE_TOOLS); fi
	cd website-tools && git pull

website-tools/Makefile.common: checkout-website-tools

.PHONY: checkout-website-tools

test: all
