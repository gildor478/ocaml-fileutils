REQUIRES = fileutils 
PREDICATES = byte

test:
	$(RM) -r test-util
	fort -I +fort `$(OCAMLFIND) query -i-format $(REQUIRES) -predicates $(PREDICATES)` \
	`$(OCAMLFIND) query -a-format $(REQUIRES) -predicates $(PREDICATES)` test.ml

include ../TopMakefile
