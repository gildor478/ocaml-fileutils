SGML_CATALOG_FILES=/etc/sgml/catalog
export SGML_CATALOG_FILES

PROC    = xsltproc --nonet --catalogs
WEBSITE = /usr/share/xml/docbook/custom/website/2.5.0/xsl

all: ocaml-fileutils.html

%.html: autolayout.xml
	$(PROC)       $(WEBSITE)/chunk-tabular.xsl $<

autolayout.xml: layout.xml
	$(PROC) -o $@ $(WEBSITE)/autolayout.xsl $<

clean:
	-$(RM) *.html autolayout.xml
