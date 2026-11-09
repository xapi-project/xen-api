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
  let other = make_vm ~__context ~name_label:"other" ~uuid:"other-uuid" in
  let host =
    Test_common.make_host ~__context ~name_label:"h" ~uuid:"h-uuid" ()
  in
  let source_host =
    Test_common.make_host ~__context ~name_label:"src" ~uuid:"src-uuid" ()
  in
  let check name expected actual =
    Alcotest.(check string) name expected actual
  in
  let open Vm_message in
  check "snapshot" "VM 'vm' (uuid: vm-uuid) snapshotted, snapshot name: snap"
    (snapshot_body ~__context ~vm ~new_name:"snap") ;
  check "checkpoint" "VM 'vm' (uuid: vm-uuid) checkpointed, snapshot name: snap"
    (checkpoint_body ~__context ~vm ~new_name:"snap") ;
  check "start" "VM 'vm' (uuid: vm-uuid) started on host: h (uuid: h-uuid)"
    (start_body ~__context ~vm ~host) ;
  check "start_on" "VM 'vm' (uuid: vm-uuid) started on host: h (uuid: h-uuid)"
    (start_on_body ~__context ~vm ~host) ;
  check "pause" "VM 'vm' (uuid: vm-uuid) paused" (pause_body ~__context ~vm) ;
  check "unpause" "VM 'vm' (uuid: vm-uuid) unpaused"
    (unpause_body ~__context ~vm) ;
  check "clean_shutdown" "VM 'vm' (uuid: vm-uuid) shutdown"
    (clean_shutdown_body ~__context ~vm) ;
  check "shutdown" "VM 'vm' (uuid: vm-uuid) shutdown"
    (shutdown_body ~__context ~vm) ;
  check "clean_reboot" "VM 'vm' (uuid: vm-uuid) rebooted cleanly"
    (clean_reboot_body ~__context ~vm) ;
  check "hard_shutdown" "VM 'vm' (uuid: vm-uuid) shutdown forcibly"
    (hard_shutdown_body ~__context ~vm) ;
  check "hard_reboot" "VM 'vm' (uuid: vm-uuid) rebooted forcibly"
    (hard_reboot_body ~__context ~vm) ;
  check "suspend" "VM 'vm' (uuid: vm-uuid) suspended"
    (suspend_body ~__context ~vm) ;
  check "revert"
    "VM 'vm' (uuid: vm-uuid) reverted to snapshot: other (uuid other-uuid)"
    (revert_body ~__context ~vm ~snapshot:other) ;
  check "csvm" "VM 'vm' (uuid: vm-uuid) cloned (new uuid: other-uuid)"
    (csvm_body ~__context ~vm ~result:other) ;
  check "resume" "VM 'vm' (uuid: vm-uuid) resumed on host: h (uuid: h-uuid)"
    (resume_body ~__context ~vm ~host) ;
  check "resume_on" "VM 'vm' (uuid: vm-uuid) resumed on host: h (uuid: h-uuid)"
    (resume_on_body ~__context ~vm ~host) ;
  check "pool_migrate"
    "VM 'vm' (uuid: vm-uuid) migrated from host 'src' (uuid: src-uuid) to host \
     'h' (uuid: h-uuid)"
    (pool_migrate_body ~__context ~vm ~source_host ~host) ;
  check "migrate_send"
    "VM 'vm' (uuid: vm-uuid) migrated from host 'src' (uuid: src-uuid) to host \
     'h' (uuid: h-uuid)"
    (migrate_send_body ~__context ~vm ~source_host ~host)

let test = [("Message bodies", `Quick, test_bodies)]

let tests = [("Test_vm_message", test)]
