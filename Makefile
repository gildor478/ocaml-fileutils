##########################################################################
#   Ocaml-fileutils                                                      #
#                                                                        #
#   Copyright (C) 2003, 2004 Sylvain Le Gall <sylvain@le-gall.net>       #
#                                                                        #
#   This program is free software; you can redistribute it and/or        #
#   modify it under the terms of the GNU Library General Public          #
#   License as published by the Free Software Foundation; either         #
#   version 2 of the License, or any later version ; with the OCaml      #
#   static compilation exception.                                        #
#                                                                        #
#   This program is distributed in the hope that it will be useful,      #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of       #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 #
#   See the LICENCE file for more details.                               #
#                                                                        #
#   You should have received a copy of the GNU General Public License    #
#   along with this program; if not, write to the Free Software          #
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA             #
#   02111-1307  USA                                                      #
#                                                                        #
#   Contact: sylvain@le-gall.net                                         #
#                                                                        #
##########################################################################

all: mkdir-temp
	cd libfileutils-ocaml && $(MAKE) all

install:
	cd libfileutils-ocaml && $(MAKE) install

uninstall:
	cd libfileutils-ocaml && $(MAKE) uninstall

clean:: mkdir-temp-clean
	cd libfileutils-ocaml && $(MAKE) clean
	cd test               && $(MAKE) clean
	cd website            && $(MAKE) clean

doc: mkdir-temp
	cd libfileutils-ocaml && $(MAKE) doc

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

.PHONY: all install uninstall clean doc 
