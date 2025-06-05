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
    id: string [@key "id"]
  ; ts: string [@key "timestamp"]
  ; ty: string [@key "class"]
  ; op: op [@key "operation"]
  ; reference: string [@key "ref"]
  ; snapshot: Rpc.t option [@key "snapshot"]
}
[@@deriving rpc]

type events = event list [@@deriving rpc]

type token = string [@@deriving rpc]

type event_from = {
    events: event list
  ; valid_ref_counts: (string * int32) list
  ; token: token
}
[@@deriving rpc]

let rec rpc_of_event_from e =
  Rpc.Dict
    [
      ("events", Rpc.Enum (List.map rpc_of_event e.events))
    ; ( "valid_ref_counts"
      , let dict =
          List.map
            (fun (key, count) -> (key, Rpc.Int32 count))
            e.valid_ref_counts
        in
        Rpc.Dict dict
      )
    ; ("token", rpc_of_token e.token)
    ]

(* xmlrpc and jsonrpc would map Int32 to Int, but int32_of_rpc can't actually parse
    an Int32 back as an int32... this is a bug in ocaml-rpc that should be fixed.
    meanwhile work it around by mapping Rpc.Int32 to Rpc.Int upon receiving the message
   (it is only Rpc.Int32 for backward compat with non-XAPI Xmlrpc clients)
*)

let rec fixup_int32 = function
  | Rpc.Dict dict ->
      Rpc.Dict (List.map fixup_kv dict)
  | Rpc.Int32 i ->
      Rpc.Int (Int64.of_int32 i)
  | rpc ->
      rpc

and fixup_kv (k, v) = (k, fixup_int32 v)

let event_from_of_rpc rpc = rpc |> fixup_int32 |> event_from_of_rpc

(** Return result of an events.from call *)

open Printf

let string_of_op = function `add -> "add" | `_mod -> "mod" | `del -> "del"

let op_of_string x =
  match String.lowercase_ascii x with
  | "add" ->
      `add
  | "mod" ->
      `_mod
  | "del" ->
      `del
  | x ->
      failwith (sprintf "Unknown operation type: %s" x)

let string_of_event ev =
  sprintf "%s %s %s %s %s" ev.id ev.ty (string_of_op ev.op) ev.reference
    (if ev.snapshot = None then "(no snapshot)" else "OK")

(** This function should be used, instead of
    {!event_from_of_rpc}, to parse the value returned by {!Xapi_event.from} in
    unit tests. *)
let parse_event_from rpc =
  rpc |> Xmlrpc.to_string |> Xmlrpc.of_string |> event_from_of_rpc
