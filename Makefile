all:
	cd libfileutils-ocaml && make all

install:
	cd libfileutils-ocaml && make install-lib

uninstall:
	cd libfileutils-ocaml && make uninstall-lib

clean:
	cd libfileutils-ocaml && make clean
	rm -f TopMakefile config.*
	rm -rf .libs
