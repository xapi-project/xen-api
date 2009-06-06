{
open Db_filter_parse
let string_chars s = String.sub s 1 (String.length s - 2)
}

rule lexer = parse
    [' ' '\t' '\n']  {lexer lexbuf}
  | '='              {EQ}
  | "true"           {TRUE}
  | "false"          {FALSE}
  | '('              {LPAREN}
  | ')'              {RPAREN}
  | "field"          {FIELD}
  | "and"            {LAND}
  | "or"             {LOR}
  | "not"            {LNOT}
  | '"' [^ '"']* '"' {IDENT (string_chars (Lexing.lexeme lexbuf))}
  | eof              {EOF}
