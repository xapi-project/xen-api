(*
 * Copyright (C) 2026 Vates.
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

type event =
  | Started of {host: API.ref_host}
  | Resumed of {host: API.ref_host}
  | Paused
  | Unpaused
  | Suspended
  | Shutdown of {forced: bool}
  | Rebooted of {forced: bool}
  | Migrated of {source: API.ref_host; destination: API.ref_host}
  | Snapshotted of {snapshot_name: string}
  | Checkpointed of {snapshot_name: string}
  | Snapshot_reverted of {snapshot: API.ref_VM}
  | Cloned of {clone: API.ref_VM}

let message = function
  | Started _ ->
      Api_messages.vm_started
  | Resumed _ ->
      Api_messages.vm_resumed
  | Paused ->
      Api_messages.vm_paused
  | Unpaused ->
      Api_messages.vm_unpaused
  | Suspended ->
      Api_messages.vm_suspended
  | Shutdown _ ->
      Api_messages.vm_shutdown
  | Rebooted _ ->
      Api_messages.vm_rebooted
  | Migrated _ ->
      Api_messages.vm_migrated
  | Snapshotted _ ->
      Api_messages.vm_snapshotted
  | Checkpointed _ ->
      Api_messages.vm_checkpointed
  | Snapshot_reverted _ ->
      Api_messages.vm_snapshot_reverted
  | Cloned _ ->
      Api_messages.vm_cloned

(* Whether the event gives the VM a new domain *)
let creates_domain = function
  | Started _ | Resumed _ | Rebooted _ | Migrated _ ->
      true
  | Paused
  | Unpaused
  | Suspended
  | Shutdown _
  | Snapshotted _
  | Checkpointed _
  | Snapshot_reverted _
  | Cloned _ ->
      false

let body ~__context ~vm event =
  let host_name self = Db.Host.get_name_label ~__context ~self in
  let host_uuid self = Db.Host.get_uuid ~__context ~self in
  let what =
    match event with
    | Started {host} ->
        Printf.sprintf "started on host: %s (uuid: %s)" (host_name host)
          (host_uuid host)
    | Resumed {host} ->
        Printf.sprintf "resumed on host: %s (uuid: %s)" (host_name host)
          (host_uuid host)
    | Paused ->
        "paused"
    | Unpaused ->
        "unpaused"
    | Suspended ->
        "suspended"
    | Shutdown {forced= false} ->
        "shutdown"
    | Shutdown {forced= true} ->
        "shutdown forcibly"
    | Rebooted {forced} ->
        let host = Db.VM.get_resident_on ~__context ~self:vm in
        Printf.sprintf "rebooted %s, host: %s (uuid: %s)"
          ( if forced then
              "forcibly"
            else
              "cleanly"
          )
          (host_name host) (host_uuid host)
    | Migrated {source; destination} ->
        Printf.sprintf
          "migrated from host '%s' (uuid: %s) to host '%s' (uuid: %s)"
          (host_name source) (host_uuid source) (host_name destination)
          (host_uuid destination)
    | Snapshotted {snapshot_name} ->
        Printf.sprintf "snapshotted, snapshot name: %s" snapshot_name
    | Checkpointed {snapshot_name} ->
        Printf.sprintf "checkpointed, snapshot name: %s" snapshot_name
    | Snapshot_reverted {snapshot} ->
        Printf.sprintf "reverted to snapshot: %s (uuid %s)"
          (Db.VM.get_name_label ~__context ~self:snapshot)
          (Db.VM.get_uuid ~__context ~self:snapshot)
    | Cloned {clone} ->
        Printf.sprintf "cloned (new uuid: %s)"
          (Db.VM.get_uuid ~__context ~self:clone)
  in
  let domid =
    if creates_domain event then
      Printf.sprintf ", domid: %Ld" (Db.VM.get_domid ~__context ~self:vm)
    else
      ""
  in
  Printf.sprintf "VM '%s' (uuid: %s) %s%s"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    what domid

let create ~__context ~vm event =
  let body = body ~__context ~vm event in
  let name, priority = message event in
  try
    ignore
      (Xapi_message.create ~__context ~name ~priority ~cls:`VM
         ~obj_uuid:(Db.VM.get_uuid ~__context ~self:vm)
         ~body
      )
  with _ -> ()
