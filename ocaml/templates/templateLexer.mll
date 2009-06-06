{
  open TemplateParser
  exception Parse_failure_in_funny_bracket of char
  exception Parse_failure of char
  exception Eof
}

rule command = parse
  | [ ^ '%' '>' ] + as e  { if e = "end" then END else BEGIN e }
  | _ as e { raise (Parse_failure_in_funny_bracket e) }

and token = parse
  | "<%"                                { command lexbuf }
  | "%>"                                { token lexbuf }

  | (( '<' [^'%'] ) | [ ^ '<' '%' ] | ([ ^ '<' ] '%') )+ as e { STRING e }
  | eof                                 { EOF }
  | _ as e { raise (Parse_failure e) } 
 
