(*
 * Copyright (C) Citrix Systems Inc.
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

(* Mock RPC for testing *)

let rpc __context call =
  match call.Rpc.name, call.Rpc.params with
  | "event.from", [_session_id_rpc;classes_rpc;token_rpc;timeout_rpc] ->
    let open API in
    let classes = string_set_of_rpc classes_rpc in
    let token = string_of_rpc token_rpc in
    let timeout = float_of_rpc timeout_rpc in
    let contents = Xapi_event.from ~__context ~classes ~token ~timeout in
    Rpc.{success=true; contents = contents |> Xmlrpc.to_string |> Xmlrpc.of_string}
  | "VM.update_allowed_operations", [session_id_rpc;self_rpc] ->
    let open API in
    let _session_id = ref_session_of_rpc session_id_rpc in
    let self = ref_VM_of_rpc self_rpc in
    Xapi_vm_lifecycle.update_allowed_operations ~__context ~self;
    Rpc.{success=true; contents=Rpc.String ""}
  | "VM.atomic_set_resident_on", [session_id_rpc;vm_rpc;host_rpc]->
    let open API in
    let _session_id = ref_session_of_rpc session_id_rpc in
    let vm = ref_VM_of_rpc vm_rpc in
    let host = ref_host_of_rpc host_rpc in
    Db.VM.set_resident_on ~__context ~self:vm ~value:host;
    Rpc.{success=true; contents=Rpc.String ""}
  | _ -> failwith "Unexpected RPC"
