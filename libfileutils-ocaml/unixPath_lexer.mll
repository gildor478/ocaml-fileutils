{

open UnixPath_parser;;

}

rule 
token_filename = parse
  '/'                  { SLASH }
| ".."                 { DOUBLE_DOT }
| "."                  { DOT }
| "\\/" as cmp         
| "\\." as cmp
| "\\.\\." as cmp      
| [^'.''/''\\']* as cmp{ (IDENT cmp) }
| eof                  { EOF }
and 
token_path_variable = parse
 ':'                   { token_path_variable lexbuf }
| [^':']* as cmp       { (IDENT cmp) }
| eof                  { EOF }
