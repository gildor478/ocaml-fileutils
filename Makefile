all: mkdir-temp
	cd libfileutils-ocaml && $(MAKE) all

install:
	cd libfileutils-ocaml && $(MAKE) install

uninstall:
	cd libfileutils-ocaml && $(MAKE) uninstall

clean: mkdir-temp-clean
	cd libfileutils-ocaml && $(MAKE) clean


mkdir-temp: mkdir-temp-stamp
mkdir-temp-stamp: 
	mkdir $(TEMPBUILD)
	mkdir $(TEMPBUILDLIB)
	mkdir $(TEMPBUILDEXE)
	mkdir $(TEMPBUILDDOC)
	touch mkdir-temp-stamp
mkdir-temp-clean:
	$(RM) -r $(TEMPBUILDDOC)
	$(RM) -r $(TEMPBUILDEXE)
	$(RM) -r $(TEMPBUILDLIB)
	$(RM) -r $(TEMPBUILD)
	$(RM) mkdir-temp-stamp
	
include TopMakefile
