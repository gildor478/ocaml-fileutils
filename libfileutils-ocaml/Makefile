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

NAME = fileutils
LIBRARY = true
CMO = \
	filePath_type.cmo \
	unixPath_parser.cmo \
	unixPath_lexer.cmo \
	unixPath.cmo \
	macOSPath_parser.cmo \
	macOSPath_lexer.cmo \
	macOSPath.cmo \
	win32Path_parser.cmo \
	win32Path_lexer.cmo \
	win32Path.cmo \
	cygwinPath_parser.cmo \
	cygwinPath_lexer.cmo \
	cygwinPath.cmo \
	genericPath_parser.cmo \
	genericPath_lexer.cmo \
	filePath.cmo \
	fileUtil.cmo 
REQUIRES = unix str 
PREDICATES = 
INSTALLIB = \
	fileutils.a \
	fileutils.cmxa \
	fileutils.cma \
	filePath.cmi \
	fileUtil.cmi \
	META
ODOC = \
	filePath.ml \
	fileUtil.ml

include ../TopMakefile

$(NAME).odoc: $(CMO)

unixPath_parser.cmo: unixPath_parser.cmi
unixPath_parser.cmx: unixPath_parser.cmi

macOSPath_parser.cmo: macOSPath_parser.cmi
macOSPath_parser.cmx: macOSPath_parser.cmi

win32Path_parser.cmo: win32Path_parser.cmi
win32Path_parser.cmx: win32Path_parser.cmi

cygwinPath_parser.cmo: cygwinPath_parser.cmi
cygwinPath_parser.cmx: cygwinPath_parser.cmi

genericPath_parser.cmo: genericPath_parser.cmi
genericPath_parser.cmx: genericPath_parser.cmi

clean::
	$(RM) unixPath_parser.ml 
	$(RM) unixPath_parser.mli 
	$(RM) unixPath_parser.output
	$(RM) unixPath_lexer.ml
	$(RM) macOSPath_parser.ml 
	$(RM) macOSPath_parser.mli 
	$(RM) macOSPath_parser.output
	$(RM) macOSPath_lexer.ml
	$(RM) win32Path_parser.ml 
	$(RM) win32Path_parser.mli 
	$(RM) win32Path_parser.output
	$(RM) win32Path_lexer.ml
	$(RM) cygwinPath_parser.ml 
	$(RM) cygwinPath_parser.mli 
	$(RM) cygwinPath_parser.output
	$(RM) cygwinPath_lexer.ml
	$(RM) genericPath_parser.ml 
	$(RM) genericPath_parser.mli 
	$(RM) genericPath_parser.output
	$(RM) genericPath_lexer.ml
