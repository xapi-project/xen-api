let lexer = Lexing.from_channel stdin

let _ = match Sys.argv with
  | [|_; "-nofmt"|] ->
      let start_time = Sys.time() in
      let sexpr = SExprParser.expr SExprLexer.token lexer in
      let parse_time = Sys.time() in
      let s = SExpr.string_of sexpr in
      let print_time = Sys.time() in
      Printf.fprintf stderr
	"Parse time: %f\nPrint time: %f\n%!"
	(parse_time -. start_time) (print_time -. parse_time);
      print_endline s
  | _ ->
      let sexpr = SExprParser.expr SExprLexer.token lexer in
      let ff = Format.formatter_of_out_channel stdout in
      SExpr.output_fmt ff sexpr;
      Format.fprintf ff "@."
