{

open GenericPath_parser;;

}

rule
token_extension = parse
 '.'              { DOT }
| [^'.']* as cmp  { (IDENT cmp) }
| eof             { EOF }
