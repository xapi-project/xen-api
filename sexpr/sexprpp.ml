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
