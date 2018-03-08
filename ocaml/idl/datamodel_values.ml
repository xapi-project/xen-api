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
open Datamodel_types

(* For values that can be easily represented as strings, return the string *)
exception Map_key_that_cannot_be_represented_as_string
let to_string v =
  match v with
    VString s -> s
  | VInt i -> Int64.to_string i
  | VFloat f -> string_of_float f
  | VEnum e -> e
  | _ -> raise Map_key_that_cannot_be_represented_as_string

let rec to_rpc v =
  match v with
    VString s -> Rpc.String s
  | VInt i -> Rpc.Int i
  | VFloat f -> Rpc.Float f
  | VBool b -> Rpc.Bool b
  | VDateTime d -> Rpc.String (Date.to_string d)
  | VEnum e -> Rpc.String e
  | VMap vvl -> Rpc.Dict (List.map (fun (v1,v2)-> to_string v1, to_rpc v2) vvl)
  | VSet vl -> Rpc.Enum (List.map (fun v->to_rpc v) vl)
  | VRef r -> Rpc.String r
  | VCustom (_,_) -> failwith "Can't RPC up a custom value"

open Printf

let to_ocaml_string v =
  let rec aux = function
    | Rpc.Null -> "Rpc.Null"
    | Rpc.String s -> sprintf "Rpc.String \"%s\"" s
    | Rpc.Int i -> sprintf "Rpc.Int %LdL" i
    | Rpc.Int32 i -> sprintf "Rpc.Int32 %ldl" i
    | Rpc.Float f -> sprintf "Rpc.Float %f" f
    | Rpc.Bool b -> sprintf "Rpc.Bool %b" b
    | Rpc.Dict d -> sprintf "Rpc.Dict [%s]" (String.concat ";" (List.map (fun (n,v) -> sprintf "(\"%s\",%s)" n (aux v)) d))
    | Rpc.Enum l -> sprintf "Rpc.Enum [%s]" (String.concat ";" (List.map aux l))
    | Rpc.DateTime t -> sprintf "Rpc.DateTime %s" t in
  match v with
  | VCustom (x,_) -> x
  | _ -> aux (to_rpc v)

let rec to_db v =
  let open Schema.Value in
  match v with
    VString s -> String s
  | VInt i -> String (Int64.to_string i)
  | VFloat f -> String (string_of_float f)
  | VBool true -> String "true"
  | VBool false -> String "false"
  | VDateTime d -> String (Date.to_string d)
  | VEnum e -> String e
  | VMap vvl ->
    Pairs(List.map (fun (k, v) -> to_string k, to_string v) vvl)
  | VSet vl ->
    Set(List.map to_string vl)
  | VRef r -> String r
  | VCustom (x,y) -> to_db y

(* Generate suitable "empty" database value of specified type *)
let gen_empty_db_val t =
  let open Schema in
  match t with
  | String -> Value.String ""
  | Int -> Value.String "0"
  | Float -> Value.String (string_of_float 0.0)
  | Bool -> Value.String "false"
  | DateTime -> Value.String (Date.to_string Date.never)
  | Enum (_,(enum_value,_)::_) -> Value.String enum_value
  | Enum (_, []) -> assert false
  | Set _ -> Value.Set []
  | Map _ -> Value.Pairs []
  | Ref _ -> Value.String null_ref
  | Record _ -> Value.String ""
