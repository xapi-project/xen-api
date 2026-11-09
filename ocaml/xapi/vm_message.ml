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

(* Bodies of the messages recording the lifecycle of a VM *)

let snapshot_body ~__context ~vm ~new_name =
  Printf.sprintf "VM '%s' (uuid: %s) snapshotted, snapshot name: %s"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    new_name

let checkpoint_body ~__context ~vm ~new_name =
  Printf.sprintf "VM '%s' (uuid: %s) checkpointed, snapshot name: %s"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    new_name

let start_body ~__context ~vm ~host =
  Printf.sprintf "VM '%s' (uuid: %s) started on host: %s (uuid: %s)"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    (Db.Host.get_name_label ~__context ~self:host)
    (Db.Host.get_uuid ~__context ~self:host)

let start_on_body ~__context ~vm ~host =
  Printf.sprintf "VM '%s' (uuid: %s) started on host: %s (uuid: %s)"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    (Db.Host.get_name_label ~__context ~self:host)
    (Db.Host.get_uuid ~__context ~self:host)

let pause_body ~__context ~vm =
  Printf.sprintf "VM '%s' (uuid: %s) paused"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)

let unpause_body ~__context ~vm =
  Printf.sprintf "VM '%s' (uuid: %s) unpaused"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)

let clean_shutdown_body ~__context ~vm =
  Printf.sprintf "VM '%s' (uuid: %s) shutdown"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)

let shutdown_body ~__context ~vm =
  Printf.sprintf "VM '%s' (uuid: %s) shutdown"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)

let clean_reboot_body ~__context ~vm =
  Printf.sprintf "VM '%s' (uuid: %s) rebooted cleanly"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)

let hard_shutdown_body ~__context ~vm =
  Printf.sprintf "VM '%s' (uuid: %s) shutdown forcibly"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)

let hard_reboot_body ~__context ~vm =
  Printf.sprintf "VM '%s' (uuid: %s) rebooted forcibly"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)

let suspend_body ~__context ~vm =
  Printf.sprintf "VM '%s' (uuid: %s) suspended"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)

let revert_body ~__context ~vm ~snapshot =
  Printf.sprintf "VM '%s' (uuid: %s) reverted to snapshot: %s (uuid %s)"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    (Db.VM.get_name_label ~__context ~self:snapshot)
    (Db.VM.get_uuid ~__context ~self:snapshot)

let csvm_body ~__context ~vm ~result =
  Printf.sprintf "VM '%s' (uuid: %s) cloned (new uuid: %s)"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:result)

let resume_body ~__context ~vm ~host =
  Printf.sprintf "VM '%s' (uuid: %s) resumed on host: %s (uuid: %s)"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    (Db.Host.get_name_label ~__context ~self:host)
    (Db.Host.get_uuid ~__context ~self:host)

let resume_on_body ~__context ~vm ~host =
  Printf.sprintf "VM '%s' (uuid: %s) resumed on host: %s (uuid: %s)"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    (Db.Host.get_name_label ~__context ~self:host)
    (Db.Host.get_uuid ~__context ~self:host)

let pool_migrate_body ~__context ~vm ~source_host ~host =
  Printf.sprintf
    "VM '%s' (uuid: %s) migrated from host '%s' (uuid: %s) to host '%s' (uuid: \
     %s)"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    (Db.Host.get_name_label ~__context ~self:source_host)
    (Db.Host.get_uuid ~__context ~self:source_host)
    (Db.Host.get_name_label ~__context ~self:host)
    (Db.Host.get_uuid ~__context ~self:host)

let migrate_send_body ~__context ~vm ~source_host ~host =
  Printf.sprintf
    "VM '%s' (uuid: %s) migrated from host '%s' (uuid: %s) to host '%s' (uuid: \
     %s)"
    (Db.VM.get_name_label ~__context ~self:vm)
    (Db.VM.get_uuid ~__context ~self:vm)
    (Db.Host.get_name_label ~__context ~self:source_host)
    (Db.Host.get_uuid ~__context ~self:source_host)
    (Db.Host.get_name_label ~__context ~self:host)
    (Db.Host.get_uuid ~__context ~self:host)
