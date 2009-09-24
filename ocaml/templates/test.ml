(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
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


