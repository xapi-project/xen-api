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

let make_vm ~__context ~name_label ~uuid =
  let self = Test_common.make_vm ~__context ~name_label () in
  Db.VM.set_uuid ~__context ~self ~value:uuid ;
  self

let test_bodies () =
  let __context = Test_common.make_test_database () in
  let vm = make_vm ~__context ~name_label:"vm" ~uuid:"vm-uuid" in
  Db.VM.set_domid ~__context ~self:vm ~value:12L ;
  let other = make_vm ~__context ~name_label:"other" ~uuid:"other-uuid" in
  let host =
    Test_common.make_host ~__context ~name_label:"h" ~uuid:"h-uuid" ()
  in
  Db.VM.set_resident_on ~__context ~self:vm ~value:host ;
  let source_host =
    Test_common.make_host ~__context ~name_label:"src" ~uuid:"src-uuid" ()
  in
  let check name expected actual =
    Alcotest.(check string) name expected actual
  in
  let open Vm_message in
  check "snapshot" "VM 'vm' (uuid: vm-uuid) snapshotted, snapshot name: snap"
    (body ~__context ~vm (Snapshotted {snapshot_name= "snap"})) ;
  check "checkpoint" "VM 'vm' (uuid: vm-uuid) checkpointed, snapshot name: snap"
    (body ~__context ~vm (Checkpointed {snapshot_name= "snap"})) ;
  check "start"
    "VM 'vm' (uuid: vm-uuid) started on host: h (uuid: h-uuid), domid: 12"
    (body ~__context ~vm (Started {host})) ;
  check "start_on"
    "VM 'vm' (uuid: vm-uuid) started on host: h (uuid: h-uuid), domid: 12"
    (body ~__context ~vm (Started {host})) ;
  check "pause" "VM 'vm' (uuid: vm-uuid) paused" (body ~__context ~vm Paused) ;
  check "unpause" "VM 'vm' (uuid: vm-uuid) unpaused"
    (body ~__context ~vm Unpaused) ;
  check "clean_shutdown" "VM 'vm' (uuid: vm-uuid) shutdown"
    (body ~__context ~vm (Shutdown {forced= false})) ;
  check "shutdown" "VM 'vm' (uuid: vm-uuid) shutdown"
    (body ~__context ~vm (Shutdown {forced= false})) ;
  check "clean_reboot"
    "VM 'vm' (uuid: vm-uuid) rebooted cleanly, host: h (uuid: h-uuid), domid: \
     12"
    (body ~__context ~vm (Rebooted {forced= false})) ;
  check "hard_shutdown" "VM 'vm' (uuid: vm-uuid) shutdown forcibly"
    (body ~__context ~vm (Shutdown {forced= true})) ;
  check "hard_reboot"
    "VM 'vm' (uuid: vm-uuid) rebooted forcibly, host: h (uuid: h-uuid), domid: \
     12"
    (body ~__context ~vm (Rebooted {forced= true})) ;
  check "suspend" "VM 'vm' (uuid: vm-uuid) suspended"
    (body ~__context ~vm Suspended) ;
  check "revert"
    "VM 'vm' (uuid: vm-uuid) reverted to snapshot: other (uuid other-uuid)"
    (body ~__context ~vm (Snapshot_reverted {snapshot= other})) ;
  check "csvm" "VM 'vm' (uuid: vm-uuid) cloned (new uuid: other-uuid)"
    (body ~__context ~vm (Cloned {clone= other})) ;
  check "resume"
    "VM 'vm' (uuid: vm-uuid) resumed on host: h (uuid: h-uuid), domid: 12"
    (body ~__context ~vm (Resumed {host})) ;
  check "resume_on"
    "VM 'vm' (uuid: vm-uuid) resumed on host: h (uuid: h-uuid), domid: 12"
    (body ~__context ~vm (Resumed {host})) ;
  check "pool_migrate"
    "VM 'vm' (uuid: vm-uuid) migrated from host 'src' (uuid: src-uuid) to host \
     'h' (uuid: h-uuid), domid: 12"
    (body ~__context ~vm (Migrated {source= source_host; destination= host})) ;
  check "migrate_send"
    "VM 'vm' (uuid: vm-uuid) migrated from host 'src' (uuid: src-uuid) to host \
     'h' (uuid: h-uuid), domid: 12"
    (body ~__context ~vm (Migrated {source= source_host; destination= host}))

let test = [("Message bodies", `Quick, test_bodies)]

let tests = [("Test_vm_message", test)]
