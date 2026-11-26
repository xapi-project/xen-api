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

type sriov_vgpu = {
    vm: API.ref_VM
  ; vgpu: API.ref_VGPU
  ; held: API.ref_PCI
  ; reserved: API.ref_PCI
}

let pf_address = "0000:41:00.0"

let reserved_address = "0000:41:00.4"

let set_xapi_version ~__context host version =
  [(Xapi_globs._platform_version, "3.4.0"); (Xapi_globs._xapi_version, version)]
  |> List.iter (fun (key, value) ->
      Db.Host.remove_from_software_version ~__context ~self:host ~key ;
      Db.Host.add_to_software_version ~__context ~self:host ~key ~value
  )

(* The coordinator (localhost) on version 26.24.0 and another host on
   [other_version] *)
let make_pool other_version =
  let __context = Test_common.make_test_database () in
  let coordinator = Helpers.get_localhost ~__context in
  let other = Test_common.make_host ~__context () in
  set_xapi_version ~__context coordinator "26.24.0" ;
  set_xapi_version ~__context other other_version ;
  (__context, coordinator, other)

(* A VM with an SR-IOV vGPU, assigned a VF on [dest]. If [running], the VM
   runs on [source] and holds a VF there. *)
let make_sriov_vgpu ~__context ?(running = true) ~source ~dest () =
  let pci host pci_id = Test_common.make_pci ~__context ~host ~pci_id () in
  let pf = pci dest pf_address in
  let held = pci source "0000:81:00.5" in
  let reserved = pci dest reserved_address in
  let pgpu = Test_common.make_pgpu ~__context ~pCI:pf ~host:dest () in
  let power_state =
    if running then
      `Running
    else
      `Halted
  in
  let vm = Test_common.make_vm ~__context ~power_state () in
  let _type =
    Test_common.make_vgpu_type ~__context ~implementation:`nvidia_sriov ()
  in
  let vgpu =
    Test_common.make_vgpu ~__context ~vM:vm ~_type
      ~scheduled_to_be_resident_on:pgpu ()
  in
  if running then (
    Db.VM.set_resident_on ~__context ~self:vm ~value:source ;
    Db.VGPU.set_PCI ~__context ~self:vgpu ~value:held ;
    Db.PCI.add_attached_VMs ~__context ~self:held ~value:vm
  ) ;
  Db.PCI.set_scheduled_to_be_attached_to ~__context ~self:reserved ~value:vm ;
  Vgpuops.assign_virtual_function ~__context dest vm
    (Vgpuops.vgpu_of_ref ~__context vgpu)
    reserved ;
  {vm; vgpu; held; reserved}

let check_ref = Alcotest.(check (Alcotest_comparators.ref ()))

let check_vgpu ~__context ~pci ~scheduled v =
  check_ref "VGPU.PCI" pci (Db.VGPU.get_PCI ~__context ~self:v.vgpu) ;
  check_ref "VGPU.scheduled_to_be_assigned_to" scheduled
    (Db.VGPU.get_scheduled_to_be_assigned_to ~__context ~self:v.vgpu)

let check_vgpu_map ~__context v =
  Xapi_vm_migrate.infer_vgpu_map ~__context v.vm
  |> List.map (fun (_, address) ->
      Xenops_interface.Pci.string_of_address address
  )
  |> Alcotest.(check (list string))
       "vGPU map has the destination PF and VF"
       [pf_address; reserved_address]

let test_migration_keeps_held_vf () =
  let __context, coordinator, other = make_pool "26.24.0" in
  let v = make_sriov_vgpu ~__context ~source:other ~dest:coordinator () in
  check_vgpu ~__context ~pci:v.held ~scheduled:v.reserved v ;
  check_vgpu_map ~__context v ;
  Xapi_vm_migrate.take_reserved_vfs ~__context ~vm:v.vm ;
  check_vgpu ~__context ~pci:v.reserved ~scheduled:Ref.null v

let test_migration_from_older_host () =
  let __context, coordinator, other = make_pool "26.23.0" in
  let v = make_sriov_vgpu ~__context ~source:other ~dest:coordinator () in
  check_vgpu ~__context ~pci:v.reserved ~scheduled:v.reserved v ;
  check_vgpu_map ~__context v

let test_migration_to_older_host () =
  let __context, coordinator, other = make_pool "26.23.0" in
  let v = make_sriov_vgpu ~__context ~source:coordinator ~dest:other () in
  check_vgpu ~__context ~pci:v.reserved ~scheduled:v.reserved v

let test_start_assigns_vf () =
  let __context, coordinator, other = make_pool "26.24.0" in
  let v =
    make_sriov_vgpu ~__context ~running:false ~source:other ~dest:coordinator ()
  in
  check_vgpu ~__context ~pci:v.reserved ~scheduled:Ref.null v

let test_take_reserved_vfs_of_other_host () =
  let __context, coordinator, other = make_pool "26.24.0" in
  let v = make_sriov_vgpu ~__context ~source:coordinator ~dest:other () in
  Xapi_vm_migrate.take_reserved_vfs ~__context ~vm:v.vm ;
  check_vgpu ~__context ~pci:v.held ~scheduled:v.reserved v

let test =
  [
    ("test_infer_vif_map_empty", `Quick, test_infer_vif_map_empty)
  ; ("test_infer_vif_map", `Quick, test_infer_vif_map)
  ; ("test_migration_keeps_held_vf", `Quick, test_migration_keeps_held_vf)
  ; ("test_migration_from_older_host", `Quick, test_migration_from_older_host)
  ; ("test_migration_to_older_host", `Quick, test_migration_to_older_host)
  ; ("test_start_assigns_vf", `Quick, test_start_assigns_vf)
  ; ( "test_take_reserved_vfs_of_other_host"
    , `Quick
    , test_take_reserved_vfs_of_other_host
    )
  ]
