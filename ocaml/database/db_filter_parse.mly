
%{
 open Db_filter_types;;
%}
  
%token EQ
%token TRUE
%token FALSE
%token FIELD
%token LAND
%token LOR
%token LNOT
%token LPAREN
%token RPAREN
%token EOF
%token <string> IDENT

%right LNOT
%left LAND LOR 


%start exprstr
%type <Db_filter_types.expr> expr
%type <Db_filter_types._val> value
%type <Db_filter_types.expr> exprstr

%%

value :
  IDENT { Literal $1 }
| FIELD IDENT { Field $2 }

expr :
  value EQ value { Eq($1,$3) }
| expr LAND expr { And($1,$3) }
| expr LOR expr { Or($1,$3) }
| LNOT expr { Not($2) }
| TRUE { True }
| FALSE { False }
| LPAREN expr RPAREN { $2 }

exprstr :
  expr EOF { $1 }
