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
(* Support for simple expression language for filtering database rows. *)

(* Language is stratified into values (field names or literals) and
   boolean expressions. *)

open Db_filter_types

let string_of_val = function
  | Field x -> "Field " ^ x
  | Literal x -> "Literal " ^ x

let rec string_of_expr =
  let binexpr name a b = Printf.sprintf "%s (%s, %s)" name (string_of_expr a) (string_of_expr b) in
  let binval name a b = Printf.sprintf "%s (%s, %s)" name (string_of_val a) (string_of_val b) in
  function
  | True -> "True"
  | False -> "False"
  | Not x -> Printf.sprintf "Not ( %s )" (string_of_expr x)
  | And(a, b) -> binexpr "And" a b
  | Or(a, b) -> binexpr "Or" a b
  | Eq(a, b) -> binval "Eq" a b

exception XML_unmarshall_error
let val_of_xml xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [a; s] ->
    begin
      match (XMLRPC.From.string a) with
        "field" -> Field (XMLRPC.From.string s)
      | "literal" -> Literal (XMLRPC.From.string s)
      | _ -> raise XML_unmarshall_error
    end
  | _ -> raise XML_unmarshall_error

let rec expr_of_xml xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [x] ->
    begin
      match (XMLRPC.From.string x) with
        "true" -> True
      | "false" -> False
      | _ -> raise XML_unmarshall_error
    end
  | [x;y] ->
    begin
      match (XMLRPC.From.string x) with
        "not" -> Not (expr_of_xml y)
      | _ -> raise XML_unmarshall_error
    end
  | [x;y;z] ->
    begin
      match (XMLRPC.From.string x) with
        "and" -> And(expr_of_xml y, expr_of_xml z)
      | "or" -> Or(expr_of_xml y, expr_of_xml z)
      | "eq" -> Eq(val_of_xml y, val_of_xml z)
      | _ -> raise XML_unmarshall_error
    end
  | _ -> raise XML_unmarshall_error

let xml_of_val v =
  match v with
    Field s -> XMLRPC.To.array [XMLRPC.To.string "field"; XMLRPC.To.string s]
  | Literal s -> XMLRPC.To.array [XMLRPC.To.string "literal"; XMLRPC.To.string s]

let rec xml_of_expr e =
  match e with
    True -> XMLRPC.To.array [XMLRPC.To.string "true"]
  | False -> XMLRPC.To.array [XMLRPC.To.string "false"]
  | Not e -> XMLRPC.To.array [XMLRPC.To.string "not";xml_of_expr e]
  | And(a,b) -> XMLRPC.To.array [XMLRPC.To.string "and"; xml_of_expr a; xml_of_expr b]
  | Or(a,b) -> XMLRPC.To.array [XMLRPC.To.string "or"; xml_of_expr a; xml_of_expr b]
  | Eq(a,b) -> XMLRPC.To.array [XMLRPC.To.string "eq"; xml_of_val a; xml_of_val b]


(** Evaluate a predicate over a database row represented by a function
    'lookup_val' which knows how to return the contents of fields. *)
let eval_expr (lookup_val: _val -> string) =
  let compare f _a _b = f (lookup_val _a) (lookup_val _b) in
  let rec f = function
    | True -> true
    | Not x -> not (f x)
    | And (a, b) -> f a && (f b)
    | Eq (_a, _b) -> compare (=) _a _b
    | False -> false
    | Or (a, b) -> f a || (f b)
  in f

exception Expression_error of (string * exn)

(* A simple parser for the expression language: *)
let expr_of_string x = try
    Db_filter_parse.exprstr Db_filter_lex.lexer
      (Lexing.from_string x)
  with e -> raise (Expression_error (x, e))




