%{
open Xn_cfg_types
%}
%token <string> IDENT STRING
%token <int> INT
%token COMMA LBRACKET RBRACKET EQ SEMICOLON NEWLINE EOF
%start file             /* the entry point */
%type <Xn_cfg_types.config> file
%%
file: EOF              { [] }
 |    NEWLINE file     { $2 }
 |    setting file     { $1 :: $2 }
;

setting: IDENT EQ value endstmt { $1, $3 }
;

endstmt: NEWLINE { () }
 |       SEMICOLON { () }
;

value: atom { $1 }
 |     LBRACKET nlok valuelist RBRACKET { List $3 }
;

atom: STRING { String $1 }
 |    INT { Int $1 }
;

valuelist: /* empty */ { [] }
 |         values { $1 }
 |         values COMMA nlok { $1 }
;

values: atom nlok { [ $1 ] }
 |      values COMMA nlok atom nlok { $4 :: $1 }
;

nlok: /* empty */ { () }
 | nlok NEWLINE { () }
;
