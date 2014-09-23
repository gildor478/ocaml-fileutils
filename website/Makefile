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
DEV_DOC_URL = file://$(shell readlink -f "../dist/ocaml-fileutils-doc-dev.tar.gz")

default: all

include Makefile.scrape
include website-tools/Makefile.common

GENERATED_HTML=$(patsubst mkd/%.mkd,html/%.html,$(wildcard mkd/*.mkd))
GENERATED_HTML+=html/documentation.html html/index.html
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
