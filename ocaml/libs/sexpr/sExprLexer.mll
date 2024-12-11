{
  open SExprParser
}

rule token = parse
  | [' ' '\t' '\r' '\n']+ | ';' [^ '\n']* '\n'     { token lexbuf }
  | '"'  (([^ '"' '\\']  | ('\\' _))* as s) '"'    { STRING s }
  | '\'' (([^ '\'' '\\'] | ('\\' _))* as s) '\''   { STRING s }
  | [^ '"' ' ' '\t' '\n' '(' ')']+ as s { SYMBOL s }
  | '('                                 { OPEN }
  | ')'                                 { CLOSE }
  | _                                   { assert false }
