{
    open Xn_cfg_parser
	let unquote x = String.sub x 1 (String.length x - 2)
}
rule token = parse
	  ['a'-'z']['_''0'-'9''a'-'z']* as x { IDENT x }
	| ['0'-'9']['0'-'9''a'-'f''x']* as x { INT (int_of_string x) }
    | '\''([^'\'''\n']|'.')*'\'' as x { STRING (unquote x) }
    | '"'([^'"''\n']|'.')*'"' as x { STRING (unquote x) }
	| ','                  { COMMA }
	| '['                  { LBRACKET }
	| ']'                  { RBRACKET }
	| '='                  { EQ }
	| ';'                  { SEMICOLON }
	| '\n'
	| '#'[^'\n']*'\n'      { NEWLINE }
	| [' ' '\t' ]          { token lexbuf }
	| eof                  { EOF }
