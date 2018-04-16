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

type 'a t =
  | Real of string
    (* ref to an object in the database *)
  | Dummy of string * string
    (* ref to an object that is not in the database, with its name *)
  | Other of string
    (* ref used for other purposes (it doesn't have one of the official prefixes) *)
  | Null
    (* ref to nothing at all *)

let ref_prefix = "OpaqueRef:"
let dummy_prefix = "DummyRef:"
let dummy_sep = "|"
let ref_null = ref_prefix ^ "NULL"

let make () =
  let uuid = Uuidm.v `V4 |> Uuidm.to_string in
  Real uuid

let null = Null

(* a dummy reference is a reference of an object which is not in database *)
let make_dummy name =
  let uuid = Uuidm.v `V4 |> Uuidm.to_string in
  Dummy (uuid, name)

let is_dummy = function
  | Dummy _ -> true
  | _ -> false

let string_of = function
  | Real uuid -> ref_prefix ^ uuid
  | Dummy (uuid, name) -> dummy_prefix ^ dummy_sep ^ uuid ^ dummy_sep ^ name
  | Other x -> x
  | Null -> ref_null

let of_string x =
  if x = ref_null then
    Null
  else
    match Astring.String.cut ~sep:ref_prefix x with
    | Some ("", uuid) -> Real uuid
    | _ ->
      match Astring.String.cuts ~sep:dummy_sep x with
      | prefix::uuid::name when prefix = dummy_prefix -> Dummy (uuid, String.concat dummy_sep name)
      | _ -> Other x

let name_of_dummy = function
  | Real x | Other x -> failwith (
      Printf.sprintf "Ref.name_of_dummy: %s is not a dummy reference" x)
  | Null -> failwith "Ref.name_of_dummy: NULL is not a dummy reference"
  | Dummy (_, name) -> name

(* we do not show the name when we pretty print the dummy reference *)
let really_pretty_and_small x =
  let small_uuid s =
    try
      let r = Bytes.create 12 in
      for i = 0 to 7 do Bytes.set r (i) s.[i]; done;
      for i = 0 to 3 do Bytes.set r (i + 8)  s.[8 + 1 + i]; done;
      Bytes.unsafe_to_string r
    with _ ->
      s
  in
  match x with
  | Dummy (uuid, _) -> "D:" ^ (small_uuid uuid)
  | Real uuid -> "R:" ^ (small_uuid uuid)
  | Other x -> "O:" ^ (Astring.String.with_range ~len:12 x)
  | Null -> "NULL"
