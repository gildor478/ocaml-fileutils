REQUIRES = fileutils  
PREDICATES = byte 

include ../TopMakefile

test:
	fort -I +fort `$(OCAMLFIND) query -i-format $(REQUIRES) -predicates $(PREDICATES)` \
	`$(OCAMLFIND) query -a-format $(REQUIRES) -predicates $(PREDICATES)` test.ml
