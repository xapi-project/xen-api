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

(** Types used to store events: *****************************************************************)
type op = API.event_operation
let rpc_of_op = API.rpc_of_event_operation
let op_of_rpc = API.event_operation_of_rpc

type event = {
  id: string;
  ts: string;
  ty: string;
  op: op;
  reference: string;
  snapshot: Rpc.t option;
} [@@deriving rpc]

let ev_struct_remap = [
  "id","id";
  "ts","timestamp";
  "ty","class";
  "op","operation";
  "reference","ref";
  "snapshot","snapshot"
]

let remap map str =
  match str with
  | Rpc.Dict d ->
    Rpc.Dict (List.map (fun (k,v) -> (List.assoc k map, v)) d)
  | _ -> str

let rpc_of_event ev =
  remap ev_struct_remap (rpc_of_event ev)

let event_of_rpc rpc =
  event_of_rpc (remap (List.map (fun (k,v) -> (v,k)) ev_struct_remap) rpc)

type events = event list [@@deriving rpc]

type token = string [@@deriving rpc]

type event_from = {
  events: event list;
  valid_ref_counts: (string * int32) list;
  token: token;
} [@@deriving rpc]

let rec rpc_of_event_from e =
  Rpc.Dict
    [ ("events",
       (Rpc.Enum (List.map rpc_of_event e.events)));
      ("valid_ref_counts",
       (let dict =
          List.map
            (fun (key, count) ->
               (key, (Rpc.Int32 count)))
            e.valid_ref_counts
        in Rpc.Dict dict));
      ("token", (rpc_of_token e.token)) ]

(** Return result of an events.from call *)

open Printf

let string_of_op = function `add -> "add" | `_mod -> "mod" | `del -> "del"
let op_of_string x = match String.lowercase_ascii x with
  | "add" -> `add | "mod" -> `_mod | "del" -> `del
  | x -> failwith (sprintf "Unknown operation type: %s" x)

let string_of_event ev = sprintf "%s %s %s %s %s" ev.id ev.ty (string_of_op ev.op) ev.reference
    (if ev.snapshot = None then "(no snapshot)" else "OK")

let parse_event_from rpc = rpc |> Xmlrpc.to_string |> Xmlrpc.of_string |> event_from_of_rpc
(** This function should be used, instead of
    {!event_from_of_rpc}, to parse the value returned by {!Xapi_event.from} in
    unit tests. *)


