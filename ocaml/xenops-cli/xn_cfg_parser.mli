type token =
  | IDENT of (string)
  | STRING of (string)
  | INT of (int)
  | COMMA
  | LBRACKET
  | RBRACKET
  | EQ
  | SEMICOLON
  | NEWLINE
  | EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Xn_cfg_types.config
