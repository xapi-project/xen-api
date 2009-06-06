open TemplateParser

let _ =
  print_endline "Type an expression on stdin:";
  let lexbuf = Lexing.from_channel stdin in

(*
  while true do
    let t = TemplateLexer.token lexbuf in
    let pp_token = function
      | BEGIN x -> Printf.sprintf "BEGIN [%s]" x
      | STRING x -> Printf.sprintf "STRING [%s]" x 
      | END -> "END" in
    print_endline (pp_token t);
    flush stdout
  done
*)

  let ts = TemplateParser.all TemplateLexer.token (Lexing.from_channel stdin) in
  let ff = Format.formatter_of_out_channel stdout in
  List.iter (Template.output_fmt ff) ts;
  Format.fprintf ff "@."


