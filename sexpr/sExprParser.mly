%token <string> SYMBOL STRING
%token <string * string> WEIRD
%token OPEN CLOSE

%start expr
%type <SExpr.t> expr

%%

expr_list: { [] }
| expr expr_list { $1 :: $2 };

expr:
| OPEN expr_list CLOSE { SExpr.Node $2 }
| SYMBOL               { SExpr.Symbol $1 }
| STRING               { SExpr.mkstring $1 }
| WEIRD                { (fun (tag, s) -> SExpr.WeirdString(tag, s)) $1 };
