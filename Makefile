all:
	cd libfileutils-ocaml && make all

install:
	cd libfileutils-ocaml && make install-lib

clean:
	cd libfileutils-ocaml && make clean
	rm -f TopMakefile config.*
	rm -rf .libs
