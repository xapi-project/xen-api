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
(** Translate from the XMLRPC encoded response into a JSON response *)

open Xml
open Stdext.Xstringext

let escape_string s =
  String.escaped ~rules:[('\n',"\\n");('"',"\\\"");('\\',"\\\\")] s

let rec xmlrpc_to_json xml =
  let float_to_string f =
    match classify_float f with
    | FP_normal | FP_subnormal -> Printf.sprintf "%0.0f" f
    | FP_nan -> "NaN"
    | FP_infinite -> if f>0.0 then "Infinity" else "-Infinity"
    | FP_zero -> "0.0"
  in
  match xml with
  | Element("i4",_,[PCData i]) -> i
  | Element("boolean",_,[PCData b]) -> b
  | Element("string",_,[PCData s]) -> "\""^(escape_string s)^"\""
  | PCData s -> "\""^(escape_string s)^"\""
  | Element("double",_,[PCData d]) -> float_to_string (float_of_string d)
  | Element("dateTime.iso8601",_,[PCData d]) -> "\""^d^"\""
  | Element("base64",_,[PCData b]) -> "\""^b^"\""
  | Element("struct",_,list) ->
    let map_func elts =
      match elts with
      | Element("member",_,[Element("name",_,[PCData n]);Element("value",_,[v])]) ->
        "\""^n^"\":"^(xmlrpc_to_json v)
      | Element("member",_,[Element("name",_,[PCData n]);Element("value",_,[])]) ->
        "\""^n^"\":\"\""
      | Element(n,_,_) -> ( Printf.fprintf stderr "%s" ("Bad XMLRPC (expecting member, got "^n^")"); "<bad!>")
      | _ -> failwith "Bad XMLRPC"
    in
    let elts = List.map map_func list in
    "{"^(String.concat "," elts)^"}"
  | Element("array",_,[Element("data",_,values)]) ->
    let values = List.map (function
        |Element("value",_,[v]) -> xmlrpc_to_json v
        | Element("value",_,[]) -> "\"\""
        | _ -> failwith "Bad XMLRPC"
      ) values in
    "["^(String.concat "," values)^"]"
  | Element(n,_,_) -> (Printf.fprintf stderr "%s" ("Bad XMLRPC (expecting something, got "^n^")"); "<bad2 (got "^n^">")





