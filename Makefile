all:
	cd libfileutils-ocaml && $(MAKE) all

install:
	cd libfileutils-ocaml && $(MAKE) install-lib

uninstall:
	cd libfileutils-ocaml && $(MAKE) uninstall-lib

clean:
	cd libfileutils-ocaml && $(MAKE) clean
