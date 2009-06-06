{
  open SExprParser
  let line = ref 1
}

rule token = parse
  | [' ' '\t' '\r']                     { token lexbuf }
  | ';' [^ '\n']* '\n'                  { incr line; token lexbuf }
  | '\n'                                { incr line; token lexbuf }
  | "<<" ([^ '<']+ as tag1) '<' ([^ '<']* as s) '<' ([^ '<']+ as tag2) '<'
      { if tag1=tag2 then WEIRD(tag1, s) else invalid_arg "Weird tag" }
  | '"'  (([^ '"' '\\']  | ('\\' _))* as s) '"'    { STRING s }
  | '\'' (([^ '\'' '\\'] | ('\\' _))* as s) '\''   { STRING s }
  | [^ '"' ' ' '\t' '\n' '(' ')']+ as s { SYMBOL s }
  | '('                                 { OPEN }
  | ')'                                 { CLOSE }
  | _                                   { assert false }
