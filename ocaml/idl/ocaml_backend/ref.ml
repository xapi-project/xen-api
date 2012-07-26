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

let wrap f () =
  let uuid = Uuid.string_of_uuid (f ()) in
  ref_prefix ^ uuid
let secure = wrap Uuid.secure
let insecure = wrap Uuid.insecure

let null = ref_prefix ^ "NULL"

let string_of x = x

let of_string x = x

(* very ugly hack to be able to distinguish dummy and real tasks. Moreover, dummy tasks have their name *)
(* embedded into the reference .... *)

(* a dummy reference is a reference of an object which is not in database *)
let dummy_sep = '|'
let dummy_prefix = "DummyRef:"
  
open Stringext

let make_dummy task_name =
  let uuid = Uuid.string_of_uuid (Uuid.insecure ()) in
  dummy_prefix ^ String.of_char dummy_sep ^ uuid ^ String.of_char dummy_sep ^ task_name 

let is_dummy x =
  String.startswith dummy_prefix x
  
let name_of_dummy x = 
  match String.split ~limit:3 dummy_sep x with
    | [_;_;name] -> name
    | l -> failwith (Printf.sprintf "Ref.name_of_dummy: %s is not a valid dummy reference (%i)" x (List.length l))

(* we do not show the name when we pretty print the dummy reference *)
let pretty_string_of_dummy x =
  match String.split ~limit:3 dummy_sep x with
    | [_;uuid;_] -> dummy_prefix ^ uuid
    | l -> failwith (Printf.sprintf "Ref.pretty_string_of_dummy: %s is not a valid dummy reference (%i)" x (List.length l))

let really_pretty_and_small x =
  let s, prelen, c =
    if is_dummy x then
      (pretty_string_of_dummy x, String.length dummy_prefix, 'D')
    else
      (string_of x, String.length ref_prefix, 'R')
    in
  try
    let r = String.create 14 in
    r.[0] <- c; r.[1] <- ':';
    for i = 0 to 7 do r.[i + 2] <- s.[prelen + i]; done;
    for i = 0 to 3 do r.[i + 10] <- s.[prelen + 8 + 1 + i]; done;
    r
  with _ ->
    s
