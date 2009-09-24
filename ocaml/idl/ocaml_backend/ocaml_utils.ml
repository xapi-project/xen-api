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
open Stringext
open Datamodel_types

let keywords = [ "mod" ]

(** Escape enum names to make them readable polymorphic variant type
    constructors. *)
let constructor_of string =
  let remove_non_alphanum = function
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' as c -> String.make 1 c
    | _ -> "" in
  let string = if List.mem string keywords then "_" ^ string else string in
  let list = match String.explode string with
      '0'..'9' :: _ as list -> "`_" :: List.map remove_non_alphanum list    
    | list -> "`" :: List.map remove_non_alphanum list in
  String.concat "" list

let ocaml_of_record_name x = 
  String.uncapitalize x

let ocaml_of_record_field x y = 
  ocaml_of_record_name x ^ "_" ^ (String.concat "_" y)

(** Convert an IDL enum into a polymorhic variant. *)
let ocaml_of_enum list =
  "[ "^String.concat " | " (List.map constructor_of list)^" ]"

(** Convert an IDL type to a function name; we need to generate functions to
    marshal/unmarshal from XML for each unique IDL type *)
let rec alias_of_ty = function
  | String -> "string"
  | Int -> "int64"
  | Float -> "float"
  | Bool -> "bool"
  | DateTime -> "datetime"
  | Set ty -> alias_of_ty ty^"_set"
  | Enum(name, _) -> String.uncapitalize (String.sub name 0 1) ^ (String.sub name 1 (String.length name - 1))
  | Map(k, v) -> alias_of_ty k^"_to_"^alias_of_ty v^"_map"
  | Ref x -> "ref_"^x
  | Record x -> ocaml_of_record_name x ^ "_t"

(** Convert an IDL type into a string containing OCaml code representing the
    type. *)
let rec ocaml_of_ty = function
  | String -> "string"
  | Int -> "int64"
  | Float -> "float"
  | Bool -> "bool"
  | DateTime -> "Date.iso8601"
  | Set (Record x) -> alias_of_ty (Record x) ^ " list"
  | Set x -> ocaml_of_ty x ^ " list"
  | Enum(name, cs) -> ocaml_of_enum (List.map fst cs)
  | Map(l, r) -> "("^alias_of_ty l^" * "^alias_of_ty r^") list"
(*  | Ref "session" -> "Uuid.cookie" *)
  | Ref ty -> "[`"^ty^"] Ref.t"
  | Record x -> failwith "ocaml_of_ty got a record"


let keywords = [ "type"; "and"; "class" ]
let escape x = String.uncapitalize (if List.mem x keywords then "_" ^ x else x)

(** Take a field name as a list (including namespaces) and return a flat name *)
let ocaml_of_id x = String.concat "_" x

(** Take an object name and return the corresponding ocaml name *)
let ocaml_of_obj_name x = 
  if x = "" 
  then failwith "Empty object name"
  else (match x.[0] with
	| 'A'..'Z' | 'a'..'z' -> String.capitalize x
	| _ -> "M_" ^ x)


