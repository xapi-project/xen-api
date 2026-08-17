(*
 * Copyright (c) Cloud Software Group
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

(* Unit tests for [Storage_smapiv3_migrate.get_snapshot_tree]: build small
   VM/VDI/VBD topologies in a mock database and assert on the resulting forest.
   *)

module T = Test_common
module Date = Clock.Date
module SM = Storage_smapiv3_migrate

let dbg = "test_storage_smapiv3_migrate"

let active_uuid = "active-disk-uuid"

let active_vdi = Storage_interface.Vdi.of_string active_uuid

(* Nodes record the *VDI*'s snapshot_time but siblings sort by the *VM*'s, so
   each snapshot's VDI time is its VM time plus this offset. *)
let vdi_time_offset = 1000.

let rec count_nodes nodes =
  List.fold_left
    (fun acc node -> acc + 1 + count_nodes node.SM.children)
    0 nodes

let check_time msg expected_unix node =
  Alcotest.(check (float 0.5))
    msg expected_unix
    (Date.to_unix_time node.SM.snapshot_time)

let make_live_vm_with_disk ~__context =
  let vm = T.make_vm ~__context ~name_label:"live" () in
  let vdi = T.make_vdi ~__context ~uuid:active_uuid () in
  ignore (T.make_vbd ~__context ~vM:vm ~vDI:vdi ~_type:`Disk ()) ;
  (vm, vdi)

let add_snapshot ~__context ~live_vm ~disk ~parent ~time ~label =
  let snap_vm = T.make_vm ~__context ~name_label:label () in
  Db.VM.set_is_a_snapshot ~__context ~self:snap_vm ~value:true ;
  Db.VM.set_snapshot_of ~__context ~self:snap_vm ~value:live_vm ;
  Db.VM.set_snapshot_time ~__context ~self:snap_vm
    ~value:(Date.of_unix_time time) ;
  Db.VM.set_parent ~__context ~self:snap_vm ~value:parent ;
  let snap_vdi = T.make_vdi ~__context () in
  Db.VDI.set_is_a_snapshot ~__context ~self:snap_vdi ~value:true ;
  Db.VDI.set_snapshot_of ~__context ~self:snap_vdi ~value:disk ;
  Db.VDI.set_snapshot_time ~__context ~self:snap_vdi
    ~value:(Date.of_unix_time (time +. vdi_time_offset)) ;
  ignore (T.make_vbd ~__context ~vM:snap_vm ~vDI:snap_vdi ~_type:`Disk ()) ;
  (snap_vm, Db.VDI.get_uuid ~__context ~self:snap_vdi)

let test_empty () =
  let __context = T.make_test_database () in
  let _vm, _vdi = make_live_vm_with_disk ~__context in
  let tree = SM.get_snapshot_tree ~dbg ~vdi:active_vdi in
  Alcotest.(check int) "no snapshots yields an empty forest" 0 (List.length tree)

let test_linear_chain () =
  let __context = T.make_test_database () in
  let vm, disk = make_live_vm_with_disk ~__context in
  let snap1, u1 =
    add_snapshot ~__context ~live_vm:vm ~disk ~parent:Ref.null ~time:1.
      ~label:"snap1"
  in
  let snap2, u2 =
    add_snapshot ~__context ~live_vm:vm ~disk ~parent:snap1 ~time:2.
      ~label:"snap2"
  in
  Db.VM.set_parent ~__context ~self:vm ~value:snap2 ;
  let tree = SM.get_snapshot_tree ~dbg ~vdi:active_vdi in
  Alcotest.(check int) "two nodes in a linear chain" 2 (count_nodes tree) ;
  match tree with
  | [root] -> (
      Alcotest.(check string) "root is the oldest snapshot" u1 root.SM.vdi_uuid ;
      Alcotest.(check bool)
        "root is on the active path" true root.SM.on_active_path ;
      check_time "root records the VDI snapshot time" (1. +. vdi_time_offset)
        root ;
      match root.SM.children with
      | [child] ->
          Alcotest.(check string)
            "child is the second snapshot" u2 child.SM.vdi_uuid ;
          Alcotest.(check bool)
            "child is on the active path" true child.SM.on_active_path ;
          check_time "child records the VDI snapshot time"
            (2. +. vdi_time_offset) child ;
          Alcotest.(check int)
            "leaf has no children" 0
            (List.length child.SM.children)
      | _ ->
          Alcotest.fail "expected exactly one child under the root"
    )
  | _ ->
      Alcotest.fail "expected exactly one root"

(* Revert-and-branch: snap1 <- snap2, then revert to snap1 and take snap3, with
   the live VM under snap3. snap3 is added BEFORE snap2 so the time-sort has to
   reorder the children rather than echo insertion order. *)
let test_branch_after_revert () =
  let __context = T.make_test_database () in
  let vm, disk = make_live_vm_with_disk ~__context in
  let snap1, u1 =
    add_snapshot ~__context ~live_vm:vm ~disk ~parent:Ref.null ~time:1.
      ~label:"snap1"
  in
  let snap3, u3 =
    add_snapshot ~__context ~live_vm:vm ~disk ~parent:snap1 ~time:3.
      ~label:"snap3"
  in
  let _snap2, u2 =
    add_snapshot ~__context ~live_vm:vm ~disk ~parent:snap1 ~time:2.
      ~label:"snap2 (reverted branch)"
  in
  Db.VM.set_parent ~__context ~self:vm ~value:snap3 ;
  let tree = SM.get_snapshot_tree ~dbg ~vdi:active_vdi in
  Alcotest.(check int) "three nodes across the branch" 3 (count_nodes tree) ;
  match tree with
  | [root] -> (
      Alcotest.(check string) "root is snap1" u1 root.SM.vdi_uuid ;
      Alcotest.(check bool)
        "root is on the active path" true root.SM.on_active_path ;
      check_time "root records the VDI snapshot time" (1. +. vdi_time_offset)
        root ;
      match root.SM.children with
      | [c1; c2] ->
          Alcotest.(check string)
            "first child is the reverted snap2" u2 c1.SM.vdi_uuid ;
          Alcotest.(check bool)
            "reverted snap2 is off the active path" false c1.SM.on_active_path ;
          check_time "snap2 records the VDI snapshot time"
            (2. +. vdi_time_offset) c1 ;
          Alcotest.(check string) "second child is snap3" u3 c2.SM.vdi_uuid ;
          Alcotest.(check bool)
            "snap3 is on the active path" true c2.SM.on_active_path ;
          check_time "snap3 records the VDI snapshot time"
            (3. +. vdi_time_offset) c2
      | _ ->
          Alcotest.fail "expected exactly two children under the root"
    )
  | _ ->
      Alcotest.fail "expected exactly one root"

let test =
  [
    ("empty snapshot tree", `Quick, test_empty)
  ; ("linear snapshot chain", `Quick, test_linear_chain)
  ; ("branch after revert", `Quick, test_branch_after_revert)
  ]
