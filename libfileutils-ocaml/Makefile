NAME = fileutils
LIBRARY = true
CMO = \
	sysPath_type.cmo \
	unixPath_parser.cmo \
	unixPath_lexer.cmo \
	unixPath.cmo \
	macOSPath_parser.cmo \
	macOSPath_lexer.cmo \
	macOSPath.cmo \
	win32Path_parser.cmo \
	win32Path_lexer.cmo \
	win32Path.cmo \
	genericPath_parser.cmo \
	genericPath_lexer.cmo \
	sysPath.cmo \
	sysUtil.cmo 
REQUIRES = unix str 
PREDICATES = 
INSTALLIB = \
	fileutils.a \
	fileutils.cmxa \
	fileutils.cma \
	sysPath_type.cmi \
	sysPath.cmi \
	sysUtil.cmi \
	META

include ../TopMakefile

unixPath_parser.cmo: unixPath_parser.cmi
unixPath_parser.cmx: unixPath_parser.cmi

macOSPath_parser.cmo: macOSPath_parser.cmi
macOSPath_parser.cmx: macOSPath_parser.cmi

win32Path_parser.cmo: win32Path_parser.cmi
win32Path_parser.cmx: win32Path_parser.cmi

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
	$(RM) genericPath_parser.ml 
	$(RM) genericPath_parser.mli 
	$(RM) genericPath_parser.output
	$(RM) genericPath_lexer.ml
	

