all:
	cd libfileutils-ocaml && $(MAKE) all

install:
	cd libfileutils-ocaml && $(MAKE) install-lib

uninstall:
	cd libfileutils-ocaml && $(MAKE) uninstall-lib

clean:
	cd libfileutils-ocaml && $(MAKE) clean

distclean:
	rm -f libfileutils-ocaml/*.cm[iox] libfileutils-ocaml/*.opt 
	rm -f libfileutils-ocaml/*.byte libfileutils-ocaml/*.cmxa 
	rm -f libfileutils-ocaml/*.cma
	rm -f libfileutils-ocaml/*.so libfileutils-ocaml/*.a
	rm -f test/*.cm[oix] test/*.opt test/*.byte
	rm -f config.*
	rm -f TopMakefile
	rm -rf .libs
