
%{


%}


%token CREATE TABLE INSERT INTO VALUES COMMIT BEGIN TRANSACTION 
%token LPAREN RPAREN SEMICOLON COMMA
%token <string> ID
%token <string> STRING1
%token <string> STRING2
%start statement
%type <Sqlsyntax.t> statement
%%

field :
  ID ID {($1,$2)}
;
fieldlist :
  | field {[$1]}
  | fieldlist COMMA field {$3::$1}
;
value :
  STRING1 {$1}

valuelist :
  | value {[$1]}
  | valuelist COMMA value {$3::$1}

statement : 
  | CREATE TABLE ID LPAREN fieldlist RPAREN SEMICOLON {Sqlsyntax.make_create $3 (List.rev $5)}
  | INSERT INTO STRING2 VALUES LPAREN valuelist RPAREN SEMICOLON {Sqlsyntax.Insert {Sqlsyntax.table=$3; Sqlsyntax.values=(List.rev $6)}}
  | BEGIN TRANSACTION SEMICOLON {Sqlsyntax.Begin}
  | COMMIT SEMICOLON {Sqlsyntax.Commit} 
;


%%
