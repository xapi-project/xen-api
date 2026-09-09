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

let mac1 = "00:00:00:00:00:01"

let mac2 = "00:00:00:00:00:02"

let check_network_map =
  Alcotest.(
    check
      (slist
         (pair (Alcotest_comparators.ref ()) (Alcotest_comparators.ref ()))
         compare
      )
  )

let test_infer_vif_map_empty () =
  let __context = Test_common.make_test_database () in
  check_network_map "Asserted by test_infer_vif_map_empty" []
    (Xapi_vm_migrate.infer_vif_map ~__context [] [])

let test_infer_vif_map () =
  let __context = Test_common.make_test_database () in
  let vm_vif1 = Test_common.make_vif ~__context ~mAC:mac1 () in
  let vm_vif2 = Test_common.make_vif ~__context ~mAC:mac2 () in
  let snap_vif1 = Test_common.make_vif ~__context ~mAC:mac1 () in
  let snap_vif2 = Test_common.make_vif ~__context ~mAC:mac2 () in
  (* In reality this network won't be in the local database, but for our
     	 * purposes it is a meaningless UUID so it's OK for it to be in the local
     	 * database. *)
  let network1 = Test_common.make_network ~__context () in
  check_network_map
    "test_infer_vif_map: check that a map with a single VIF -> network pair is \
     unchanged"
    (Xapi_vm_migrate.infer_vif_map ~__context [vm_vif1] [(vm_vif1, network1)])
    [(vm_vif1, network1)] ;
  Alcotest.check_raises "test_infer_vif_map: check that a missing VIF is caught"
    Api_errors.(Server_error (vif_not_in_map, [Ref.string_of vm_vif2]))
    (fun () ->
      ignore
        (Xapi_vm_migrate.infer_vif_map ~__context [vm_vif1; vm_vif2]
           [(vm_vif1, network1)]
        )
    ) ;
  let inferred_map =
    Xapi_vm_migrate.infer_vif_map ~__context [vm_vif1; snap_vif1]
      [(vm_vif1, network1)]
  in
  Alcotest.(check (Alcotest_comparators.ref ()))
    "test_infer_vif_map: check that a snapshot VIF is mapped implicitly"
    (List.assoc snap_vif1 inferred_map)
    network1 ;
  Alcotest.check_raises
    "Check that an orphaned, unmapped snapshot VIF is caught."
    Api_errors.(Server_error (vif_not_in_map, [Ref.string_of snap_vif2]))
    (fun () ->
      ignore
        (Xapi_vm_migrate.infer_vif_map ~__context
           [vm_vif1; snap_vif1; snap_vif2]
           [(vm_vif1, network1)]
        )
    )

let check_lineage_parent =
  Alcotest.(check (option (Alcotest_comparators.ref ())))

let make_disk ~__context ~vm =
  let vDI = Test_common.make_vdi ~__context () in
  ignore (Test_common.make_vbd ~__context ~vM:vm ~vDI ~_type:`Disk ()) ;
  vDI

(* A snapshot of [live_vm] taken from [parent], holding one snapshot VDI per
   disk, linked the way VM.snapshot links them. *)
let make_snapshot ~__context ~live_vm ~parent ~disks =
  let snapshot = Test_common.make_vm ~__context ~name_label:"snapshot" () in
  Db.VM.set_is_a_snapshot ~__context ~self:snapshot ~value:true ;
  Db.VM.set_snapshot_of ~__context ~self:snapshot ~value:live_vm ;
  Db.VM.set_parent ~__context ~self:snapshot ~value:parent ;
  let snapshot_disks =
    List.map
      (fun disk ->
        let vDI = make_disk ~__context ~vm:snapshot in
        Db.VDI.set_is_a_snapshot ~__context ~self:vDI ~value:true ;
        Db.VDI.set_snapshot_of ~__context ~self:vDI ~value:disk ;
        vDI
      )
      disks
  in
  (snapshot, snapshot_disks)

(* [make_snapshot] for a VM whose only disk is [disk]. *)
let make_snapshot_of_disk ~__context ~live_vm ~parent ~disk =
  match make_snapshot ~__context ~live_vm ~parent ~disks:[disk] with
  | snapshot, [snapshot_disk] ->
      (snapshot, snapshot_disk)
  | _ ->
      Alcotest.fail "expected one snapshot VDI per disk"

let test_lineage_parent_of_no_snapshots () =
  let __context = Test_common.make_test_database () in
  let vm = Test_common.make_vm ~__context () in
  let disk = make_disk ~__context ~vm in
  check_lineage_parent "a disk that has never been snapshotted starts a chain"
    None
    (Xapi_vm_migrate.lineage_parent_of ~__context ~vm ~vdi:disk)

let test_lineage_parent_of_linear_chain () =
  let __context = Test_common.make_test_database () in
  let vm = Test_common.make_vm ~__context () in
  let disk = make_disk ~__context ~vm in
  let snap1, snap1_disk =
    make_snapshot_of_disk ~__context ~live_vm:vm ~parent:Ref.null ~disk
  in
  let snap2, snap2_disk =
    make_snapshot_of_disk ~__context ~live_vm:vm ~parent:snap1 ~disk
  in
  Db.VM.set_parent ~__context ~self:vm ~value:snap2 ;
  check_lineage_parent "the oldest snapshot starts the chain" None
    (Xapi_vm_migrate.lineage_parent_of ~__context ~vm:snap1 ~vdi:snap1_disk) ;
  check_lineage_parent "the newest snapshot follows the oldest" (Some snap1_disk)
    (Xapi_vm_migrate.lineage_parent_of ~__context ~vm:snap2 ~vdi:snap2_disk) ;
  check_lineage_parent "the disk in use follows the newest snapshot"
    (Some snap2_disk)
    (Xapi_vm_migrate.lineage_parent_of ~__context ~vm ~vdi:disk)

let test_lineage_parent_of_branch_after_revert () =
  (* snap1 <- snap2, then a revert to snap1 and snap3 taken from there, which
     leaves snap2 on a branch of its own. *)
  let __context = Test_common.make_test_database () in
  let vm = Test_common.make_vm ~__context () in
  let disk = make_disk ~__context ~vm in
  let snap1, snap1_disk =
    make_snapshot_of_disk ~__context ~live_vm:vm ~parent:Ref.null ~disk
  in
  let snap2, snap2_disk =
    make_snapshot_of_disk ~__context ~live_vm:vm ~parent:snap1 ~disk
  in
  let snap3, snap3_disk =
    make_snapshot_of_disk ~__context ~live_vm:vm ~parent:snap1 ~disk
  in
  Db.VM.set_parent ~__context ~self:vm ~value:snap3 ;
  check_lineage_parent "the abandoned branch follows the revert point"
    (Some snap1_disk)
    (Xapi_vm_migrate.lineage_parent_of ~__context ~vm:snap2 ~vdi:snap2_disk) ;
  check_lineage_parent "the branch taken after the revert follows it too"
    (Some snap1_disk)
    (Xapi_vm_migrate.lineage_parent_of ~__context ~vm:snap3 ~vdi:snap3_disk) ;
  check_lineage_parent
    "the disk in use skips the branch it was reverted away from"
    (Some snap3_disk)
    (Xapi_vm_migrate.lineage_parent_of ~__context ~vm ~vdi:disk)

let test_lineage_parent_of_several_disks () =
  (* Both disks are snapshotted by the same VM, and each follows its own. *)
  let __context = Test_common.make_test_database () in
  let vm = Test_common.make_vm ~__context () in
  let disk1 = make_disk ~__context ~vm in
  let disk2 = make_disk ~__context ~vm in
  let snap, snap_disks =
    make_snapshot ~__context ~live_vm:vm ~parent:Ref.null ~disks:[disk1; disk2]
  in
  Db.VM.set_parent ~__context ~self:vm ~value:snap ;
  match snap_disks with
  | [snap_disk1; snap_disk2] ->
      check_lineage_parent "the first disk follows its own snapshot"
        (Some snap_disk1)
        (Xapi_vm_migrate.lineage_parent_of ~__context ~vm ~vdi:disk1) ;
      check_lineage_parent "the second disk follows its own snapshot"
        (Some snap_disk2)
        (Xapi_vm_migrate.lineage_parent_of ~__context ~vm ~vdi:disk2)
  | _ ->
      Alcotest.fail "expected one snapshot VDI per disk"

let test =
  [
    ("test_infer_vif_map_empty", `Quick, test_infer_vif_map_empty)
  ; ("test_infer_vif_map", `Quick, test_infer_vif_map)
  ; ( "test_lineage_parent_of_no_snapshots"
    , `Quick
    , test_lineage_parent_of_no_snapshots
    )
  ; ( "test_lineage_parent_of_linear_chain"
    , `Quick
    , test_lineage_parent_of_linear_chain
    )
  ; ( "test_lineage_parent_of_branch_after_revert"
    , `Quick
    , test_lineage_parent_of_branch_after_revert
    )
  ; ( "test_lineage_parent_of_several_disks"
    , `Quick
    , test_lineage_parent_of_several_disks
    )
  ]
