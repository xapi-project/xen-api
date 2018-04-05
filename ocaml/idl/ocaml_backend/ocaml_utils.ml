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
open Printf

let keywords = [ "mod"; "type"; "class"; "ref"; "and" ]

let escape x =
  if List.mem x keywords then
    "_" ^ x
  else match x.[0] with
    | 'a' .. 'z' | '_' -> x
    | _                -> "_" ^ x

(** Escape enum names to make them readable polymorphic variant type
    constructors. *)
let constructor_of string =
  let remove_non_alphanum = function
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' as c -> String.make 1 c
    | _ -> "" in
  let string = if List.mem string keywords then "_" ^ string else string in
  let list = match Xapi_stdext_std.Xstringext.String.explode string with
      '0'..'9' :: _ as list -> "`_" :: List.map remove_non_alphanum list
    | list -> "`" :: List.map remove_non_alphanum list in
  String.concat "" list

(* generates: vM *)
let ocaml_of_record_name x =
  escape (String.uncapitalize_ascii x)

(* generates: _VM *)
let ocaml_of_record_name_rpc x =
  escape x

(* generates: _VM_foo *)
let ocaml_of_record_field_rpc x =
  escape (String.concat "_" x)

(* generates: vM_foo *)
let ocaml_of_record_field = function
  | []     -> failwith "ocaml_of_record_field"
  | h :: t -> ocaml_of_record_field_rpc (String.uncapitalize_ascii h :: t)

let ocaml_of_module_name x =
  String.capitalize_ascii x

(** Convert an IDL enum into a polymorhic variant. *)
let ocaml_of_enum list =
  "[ "^String.concat " | " (List.map constructor_of list)^" ]"

(** Convert an IDL type to a function name; we need to generate functions to
    marshal/unmarshal from XML for each unique IDL type *)
let rec alias_of_ty ?(prefix="") = function
  | String                        -> "string"
  | Int                           -> "int64"
  | Float                         -> "float"
  | Bool                          -> "bool"
  | DateTime                      -> "datetime"
  | Set ty                        -> sprintf "%s_set" (alias_of_ty ty)
  | Enum(name, _)                 -> String.uncapitalize_ascii name
  | Map(k, v)                     -> sprintf "%s_to_%s_map" (alias_of_ty k) (alias_of_ty v)
  | Ref x                         -> sprintf "ref_%s" x
  | Record x                      -> sprintf "%s_t" (ocaml_of_record_name x)
  | Option x                      -> sprintf "%s_option" (alias_of_ty x)

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
  | Option x -> ocaml_of_ty x ^ " option"
  | Record x -> failwith "ocaml_of_ty got a record"

(** Take an object name and return the corresponding ocaml name *)
let ocaml_of_obj_name x =
  if x = ""
  then failwith "Empty object name"
  else (match x.[0] with
      | 'A'..'Z' | 'a'..'z' -> String.capitalize_ascii x
      | _ -> "M_" ^ x)


