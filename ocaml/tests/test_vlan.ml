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

open Test_common

let test_pool_introduce () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let network = make_network ~__context () in
  let tagged_PIF = make_pif ~__context ~network ~host () in
  let tag = 3201L in
  let vlan_network = make_network ~__context ~bridge:"xapi0" () in
  let untagged_PIF = make_pif ~__context ~network:vlan_network ~host ~vLAN:tag () in
  let vlan = Xapi_vlan.pool_introduce ~__context
    ~tagged_PIF ~untagged_PIF ~tag ~other_config:[]
  in
  Alcotest.check (Alcotest_comparators.ref ()) "VLAN master of untagged PIF" vlan (Db.PIF.get_VLAN_master_of ~__context ~self:untagged_PIF);
  Alcotest.check (Alcotest_comparators.ref ()) "VLAN's tagged PIF" tagged_PIF (Db.VLAN.get_tagged_PIF ~__context ~self:vlan);
  Alcotest.check (Alcotest_comparators.ref ()) "VLAN's untagged PIF" untagged_PIF (Db.VLAN.get_untagged_PIF ~__context ~self:vlan);
  Alcotest.(check int64) "VLAN's tag" tag (Db.VLAN.get_tag ~__context ~self:vlan)

let test_create_internal () =
  let __context = make_test_database () in
  let tag = 3201L in
  let device = "eth0" in
  let host = make_host ~__context () in
  let network = make_network ~__context ~bridge:"xapi0" () in
  let tagged_PIF = make_pif ~__context ~network ~host () in
  let vlan, untagged_PIF = Xapi_vlan.create_internal ~__context
    ~host ~tagged_PIF ~tag ~network ~device
  in
  Alcotest.check (Alcotest_comparators.ref ()) "VLAN master of untagged PIF" vlan (Db.PIF.get_VLAN_master_of ~__context ~self:untagged_PIF);
  Alcotest.check (Alcotest_comparators.ref ()) "VLAN's tagged PIF" tagged_PIF (Db.VLAN.get_tagged_PIF ~__context ~self:vlan);
  Alcotest.check (Alcotest_comparators.ref ()) "VLAN's untagged PIF" untagged_PIF (Db.VLAN.get_untagged_PIF ~__context ~self:vlan);
  Alcotest.(check int64) "VLAN's tag" tag (Db.VLAN.get_tag ~__context ~self:vlan);
  Alcotest.check (Alcotest_comparators.ref ()) "untagged PIF's network" network (Db.PIF.get_network ~__context ~self:untagged_PIF);
  Alcotest.(check string) "untagged PIF's device" device (Db.PIF.get_device ~__context ~self:untagged_PIF);
  Alcotest.check (Alcotest_comparators.ref ()) "untagged PIF's host" host (Db.PIF.get_host ~__context ~self:untagged_PIF);
  Alcotest.(check int64) "untagged PIF's VLAN" tag (Db.PIF.get_VLAN ~__context ~self:untagged_PIF)

let test_create_unmanaged_pif () =
  let __context = make_test_database () in
  let tag = 3201L in
  let host = make_host ~__context () in
  let network = make_network ~__context () in
  let vlan_network = make_network ~__context ~bridge:"xapi0" () in
  let tagged_PIF = make_pif ~__context ~network ~host ~managed:false () in
  assert_raises_api_error
    Api_errors.pif_unmanaged
    ~args:[Ref.string_of tagged_PIF]
    (fun () -> Xapi_vlan.create ~__context ~tagged_PIF ~tag ~network:vlan_network)

let test_create_network_already_connected () =
  let __context = make_test_database () in
  let tag = 3201L in
  let host = make_host ~__context () in
  let network = make_network ~__context () in
  let tagged_PIF = make_pif ~__context ~network ~host () in
  assert_raises_api_error
    Api_errors.network_already_connected
    ~args:[Ref.string_of host; Ref.string_of tagged_PIF]
    (fun () -> Xapi_vlan.create ~__context ~tagged_PIF ~tag ~network:network)

let test_create_pif_is_bond_slave () =
  let __context = make_test_database () in
  let tag = 3201L in
  let host = make_host ~__context () in
  let tagged_PIF =
    let members = mknlist 2 (create_physical_pif ~__context ~host) in
    let _ = create_bond_pif ~__context ~host ~members () in
    List.hd members
  in
  let vlan_network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.cannot_add_vlan_to_bond_slave
    ~args:[Ref.string_of tagged_PIF]
    (fun () -> Xapi_vlan.create ~__context ~tagged_PIF ~tag ~network:vlan_network)

let test_create_pif_is_vlan_master () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let vlan_network2 = make_network ~__context ~bridge:"xapi02" () in
  let untagged_PIF =
    let physical_PIF = create_physical_pif ~__context ~host () in
    create_vlan_pif ~__context ~host ~vlan:1L ~pif:physical_PIF ()
  in
  assert_raises_api_error
    Api_errors.pif_is_vlan
    ~args:[Ref.string_of untagged_PIF]
    (fun () ->
       let tag = 3201L in
       Xapi_vlan.create ~__context ~tagged_PIF:untagged_PIF ~tag ~network:vlan_network2)

let test_create_pif_is_vlan_master_on_sriov () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let vlan_network2 = make_network ~__context ~bridge:"xapi02" () in
  let untagged_PIF =
    let physical_PIF = create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF () in
    create_vlan_pif ~__context ~host ~vlan:1L ~pif:sriov_logical_PIF ()
  in
  assert_raises_api_error
    Api_errors.pif_is_vlan
    ~args:[Ref.string_of untagged_PIF]
    (fun () ->
       let tag = 3201L in
       Xapi_vlan.create ~__context ~tagged_PIF:untagged_PIF ~tag ~network:vlan_network2)

let test_create_invalid_tag () =
  let __context = make_test_database () in
  let tag = -1L in
  let host = make_host ~__context () in
  let network = make_network ~__context () in
  let vlan_network = make_network ~__context ~bridge:"xapi0" () in
  let tagged_PIF = make_pif ~__context ~network ~host () in
  assert_raises_api_error
    Api_errors.vlan_tag_invalid
    ~args:[Int64.to_string tag]
    (fun () -> Xapi_vlan.create ~__context ~tagged_PIF ~tag ~network:vlan_network);
  let new_tag = 4095L in
  assert_raises_api_error
    Api_errors.vlan_tag_invalid
    ~args:[Int64.to_string new_tag]
    (fun () -> Xapi_vlan.create ~__context ~tagged_PIF ~tag:new_tag ~network:vlan_network)

let test_create_vlan_already_exists () =
  let __context = make_test_database () in
  let tag = 3201L in
  let device = "eth0" in
  let host = make_host ~__context () in
  let network = make_network ~__context () in
  let vlan_network = make_network ~__context ~bridge:"xapi0" () in
  let tagged_PIF = make_pif ~__context ~network ~host () in
  let _ = Xapi_vlan.create_internal ~__context
    ~host ~tagged_PIF ~tag ~network:vlan_network ~device in
  let new_vlan_network = make_network ~__context ~bridge:"xapi1" () in
  assert_raises_api_error
    Api_errors.pif_vlan_exists
    ~args:[device]
    (fun () -> Xapi_vlan.create ~__context ~tagged_PIF ~tag ~network:new_vlan_network)

let test_create_pif_is_tunnel_access () =
  let __context = make_test_database () in
  let tag = 3201L in
  let host = make_host ~__context () in
  let vlan_network = make_network ~__context ~bridge:"xapi1" () in
  let tagged_PIF =
    let transport_PIF = create_physical_pif ~__context ~host () in
    create_tunnel_pif ~__context ~host ~pif:transport_PIF ()
  in
  assert_raises_api_error
    Api_errors.is_tunnel_access_pif
    ~args:[Ref.string_of tagged_PIF]
    (fun () -> Xapi_vlan.create ~__context ~tagged_PIF ~tag ~network:vlan_network)

let test_gc_vlan () =
  let __context = make_test_database () in
  let tag = 3201L in
  let host = make_host ~__context () in
  let network = make_network ~__context () in
  let tagged_PIF = make_pif ~__context ~network ~host () in
  let vlan_network = make_network ~__context ~bridge:"xapi0" () in
  let untagged_PIF = make_pif ~__context ~network:vlan_network ~host ~vLAN:tag () in
  let vlan = Xapi_vlan.pool_introduce ~__context
    ~tagged_PIF ~untagged_PIF ~tag ~other_config:[]
  in
  Alcotest.check (Alcotest_comparators.ref ()) "VLAN master of untagged PIF" vlan (Db.PIF.get_VLAN_master_of ~__context ~self:untagged_PIF);
  Db.PIF.set_host ~__context ~self:untagged_PIF ~value:Ref.null;
  Db.PIF.set_network ~__context ~self:untagged_PIF ~value:Ref.null;
  Db_gc_util.gc_PIFs ~__context;
  Alcotest.(check bool) "not valid ref" false (Db.is_valid_ref __context vlan)

let test_create_sriov_vlan_into_non_sriov_vlan_network () =
  let __context = make_test_database () in
  let vlan_network =
    let host = make_host ~__context () in
    let physical_PIF = create_physical_pif ~__context ~host () in
    let vlan_pif = create_vlan_pif ~__context ~host ~vlan:1L ~pif:physical_PIF () in
    Db.PIF.get_network ~__context ~self:vlan_pif
  in
  let tagged_PIF =
    let host = make_host ~__context () in
    let physical_PIF = create_physical_pif ~__context ~host () in
    create_sriov_pif ~__context ~pif:physical_PIF ()
  in
  assert_raises_api_error
    Api_errors.network_incompatible_with_vlan_on_sriov
    ~args:[Ref.string_of vlan_network]
    (fun () ->
       let tag = 3201L in
       Xapi_vlan.create ~__context ~tagged_PIF ~tag ~network:vlan_network)

let test_create_non_sriov_vlan_into_sriov_vlan_network () =
  let __context = make_test_database () in
  let vlan_network =
    let host = make_host ~__context () in
    let physical_PIF = create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF () in
    let vlan_pif = create_vlan_pif ~__context ~host ~vlan:1L ~pif:sriov_logical_PIF () in
    Db.PIF.get_network ~__context ~self:vlan_pif
  in
  let tagged_PIF =
    let host = make_host ~__context () in
    create_physical_pif ~__context ~host ()
  in
  assert_raises_api_error
    Api_errors.network_incompatible_with_vlan_on_bridge
    ~args:[Ref.string_of vlan_network]
    (fun () ->
       let tag = 3201L in
       Xapi_vlan.create ~__context ~tagged_PIF ~tag ~network:vlan_network)

let test_create_sriov_vlan_into_sriov_vlan_network_with_different_type_pci_device () =
  let __context = make_test_database () in
  let vlan_network =
    let host = make_host ~__context () in
    let physical_PIF = create_physical_pif ~__context ~host () in
    let pci = make_pci ~__context ~vendor_id:"101" ~device_id:"2" () in
    Db.PIF.set_PCI ~__context ~self:physical_PIF ~value:pci;
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF () in
    let vlan_pif = create_vlan_pif ~__context ~host ~vlan:1L ~pif:sriov_logical_PIF () in
    Db.PIF.get_network ~__context ~self:vlan_pif
  in
  let tagged_PIF =
    let host = make_host ~__context () in
    let physical_PIF = create_physical_pif ~__context ~host () in
    let pci = make_pci ~__context ~vendor_id:"101" ~device_id:"3" () in
    Db.PIF.set_PCI ~__context ~self:physical_PIF ~value:pci;
    create_sriov_pif ~__context ~pif:physical_PIF ()
  in
  assert_raises_api_error
    Api_errors.network_has_incompatible_vlan_on_sriov_pifs
    ~args:[Ref.string_of tagged_PIF; Ref.string_of vlan_network]
    (fun () ->
       let tag = 3201L in
       Xapi_vlan.create ~__context ~tagged_PIF ~tag ~network:vlan_network)

let test =
  [
    "test_pool_introduce", `Quick, test_pool_introduce;
    "test_create_internal", `Quick, test_create_internal;
    "test_create_unmanaged_pif", `Quick, test_create_unmanaged_pif;
    "test_create_network_already_connected", `Quick, test_create_network_already_connected;
    "test_create_pif_is_bond_slave", `Quick, test_create_pif_is_bond_slave;
    "test_create_pif_is_vlan_master", `Quick, test_create_pif_is_vlan_master;
    "test_create_invalid_tag", `Quick, test_create_invalid_tag;
    "test_create_vlan_already_exists", `Quick, test_create_vlan_already_exists;
    "test_create_pif_is_tunnel_access", `Quick, test_create_pif_is_tunnel_access;
    "test_create_pif_is_vlan_master_on_sriov", `Quick, test_create_pif_is_vlan_master_on_sriov;
    "test_gc_vlan", `Quick, test_gc_vlan;
    "test_create_sriov_vlan_into_non_sriov_vlan_network", `Quick, test_create_sriov_vlan_into_non_sriov_vlan_network;
    "test_create_non_sriov_vlan_into_sriov_vlan_network", `Quick, test_create_non_sriov_vlan_into_sriov_vlan_network;
    "test_create_sriov_vlan_into_sriov_vlan_network_with_different_type_pci_device", `Quick, test_create_sriov_vlan_into_sriov_vlan_network_with_different_type_pci_device;
  ]
