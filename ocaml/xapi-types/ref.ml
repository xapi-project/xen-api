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

(** Internally, a reference is simply a string. *)
type 'a t = string

let ref_prefix = "OpaqueRef:"

let make () =
  let uuid = Uuidm.v `V4 |> Uuidm.to_string in
  ref_prefix ^ uuid

let null = ref_prefix ^ "NULL"

let string_of x = x

let of_string x = x

(* very ugly hack to be able to distinguish dummy and real tasks. Moreover, dummy tasks have their name *)
(* embedded into the reference .... *)

(* a dummy reference is a reference of an object which is not in database *)
let dummy_sep = "|"
let dummy_prefix = "DummyRef:"

let make_dummy task_name =
  let uuid = Uuidm.v `V4 |> Uuidm.to_string in
  dummy_prefix ^ dummy_sep ^ uuid ^ dummy_sep ^ task_name

let is_dummy x =
  Astring.String.is_prefix ~affix:dummy_prefix x

let cut_in_three ~sep str =
  let (>>=) opt f =
    match opt with
    | Some value -> f value
    | None -> None
  in
  let open Astring in
  String.cut ~sep str
  >>= fun (head, tail) ->
  String.cut ~sep tail
  >>= fun (head', tail) ->
  Some (head, head', tail)

let name_of_dummy x =
  match cut_in_three ~sep:dummy_sep x with
  | Some(_, _, name) -> name
  | None -> failwith (
      Printf.sprintf "Ref.name_of_dummy: %s is not a valid dummy reference" x)

(* we do not show the name when we pretty print the dummy reference *)
let pretty_string_of_dummy x =
  match cut_in_three ~sep:dummy_sep x with
  | Some (_, uuid, _) -> dummy_prefix ^ uuid
  | None -> failwith (
      Printf.sprintf "Ref.pretty_string_of_dummy: %s is not a valid dummy reference" x)

let really_pretty_and_small x =
  let s, prelen, c =
    if is_dummy x then
      (pretty_string_of_dummy x, String.length dummy_prefix, 'D')
    else
      (string_of x, String.length ref_prefix, 'R')
  in
  try
    let r = Bytes.create 14 in
    Bytes.set r 0 c; Bytes.set r 1 ':';
    for i = 0 to 7 do Bytes.set r (i + 2) s.[prelen + i]; done;
    for i = 0 to 3 do Bytes.set r (i + 10)  s.[prelen + 8 + 1 + i]; done;
    Bytes.unsafe_to_string r
  with _ ->
    s
