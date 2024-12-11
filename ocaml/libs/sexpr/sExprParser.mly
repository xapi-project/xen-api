%token <string> SYMBOL STRING
%token OPEN CLOSE

%start<SExpr.t> expr

%%

expr:
| OPEN es = list(expr) CLOSE { SExpr.Node es }
| s = SYMBOL { SExpr.Symbol s }
| s = STRING { SExpr.mkstring s }
