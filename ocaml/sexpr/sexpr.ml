open Threadext

let lock = Mutex.create ()

let of_string s =
	Mutex.execute lock
		(fun () -> SExprParser.expr SExprLexer.token (Lexing.from_string s))

let string_of = SExpr.string_of
