{

open MacOSPath_parser;;

}

rule
token_filename = parse
  ':'	         { SEPARATOR }
| "\\:" as cmp   { (IDENT cmp) }
| [^':']* as cmp { (IDENT cmp) } 
| eof            { EOF }
and
token_path_variable = parse
 ';'             { token_path_variable lexbuf }
| [^';']* as cmp { (IDENT cmp) }
| eof            { EOF }
and
token_extension = parse
 '.'              { DOT }
| [^'.']* as cmp  { (IDENT cmp) }
| eof             { EOF }

