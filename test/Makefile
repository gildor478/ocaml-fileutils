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

REQUIRES = fileutils 
PREDICATES = byte

test:
	$(RM) -r test-util
	fort -I +fort -I $(TEMPBUILDLIB)/fileutils fileutils.cma test.ml

include ../TopMakefile
