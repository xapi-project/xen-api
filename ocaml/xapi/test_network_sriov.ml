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

open OUnit
open Test_common

let test_create_internal () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let physical_PIF = create_physical_pif ~__context ~host () in
  let physical_rec = Db.PIF.get_record ~__context ~self:physical_PIF in
  let network = make_network ~__context ~bridge:"xapi0" () in
  let sriov, logical_PIF = Xapi_network_sriov.create_internal ~__context ~physical_PIF ~physical_rec ~network in
  assert_equal sriov (List.hd (Db.PIF.get_sriov_physical_PIF_of ~__context ~self:physical_PIF));
  assert_equal sriov (List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:logical_PIF));
  assert_equal physical_PIF (Db.Network_sriov.get_physical_PIF ~__context ~self:sriov);
  assert_equal logical_PIF (Db.Network_sriov.get_logical_PIF ~__context ~self:sriov);
  assert_equal network (Db.PIF.get_network ~__context ~self:logical_PIF);
  assert_equal host (Db.PIF.get_host ~__context ~self:logical_PIF)

let test_create_on_unmanaged_pif () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let physical_PIF = create_physical_pif ~__context ~host ~managed:false () in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.pif_unmanaged
    ~args:[Ref.string_of physical_PIF]
    (fun () -> Xapi_network_sriov.create ~__context ~pif:physical_PIF ~network)

let test_create_network_already_connected () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let network = make_network ~__context () in
  let physical_PIF = create_physical_pif ~__context ~host ~network () in
  Db.PIF.set_capabilities ~__context ~self:physical_PIF ~value:["sriov"];
  assert_raises_api_error
    Api_errors.network_already_connected
    ~args:[Ref.string_of host; Ref.string_of physical_PIF]
    (fun () -> Xapi_network_sriov.create ~__context ~pif:physical_PIF ~network)

let test_create_on_bond_master () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let pif =
    let members = mknlist 2 (create_physical_pif ~__context ~host) in
    create_bond_pif ~__context ~host ~members ()
  in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.pif_is_not_physical
    ~args:[Ref.string_of pif]
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network)

let test_create_on_tunnel_access () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let pif =
    let transport_PIF = create_physical_pif ~__context ~host () in
    create_tunnel_pif ~__context ~host ~pif:transport_PIF ()
  in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.pif_is_not_physical
    ~args:[Ref.string_of pif]
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network)

let test_create_on_sriov_logical () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let pif =
    let physical_PIF = create_physical_pif ~__context ~host () in
    create_sriov_pif ~__context ~pif:physical_PIF ()
  in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.pif_is_not_physical
    ~args:[Ref.string_of pif]
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network)

let test_create_on_vlan () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let pif =
    let physical_PIF = create_physical_pif ~__context ~host () in
    create_vlan_pif ~__context ~host ~vlan:1L ~pif:physical_PIF ()
  in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.pif_is_not_physical
    ~args:[Ref.string_of pif]
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network)

let test_create_on_vlan_on_sriov_logical () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let pif =
    let physical_PIF = create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF () in
    create_vlan_pif ~__context ~host ~vlan:1L ~pif:sriov_logical_PIF ()
  in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.pif_is_not_physical
    ~args:[Ref.string_of pif]
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network)

let test_create_on_pif_already_enabled_sriov () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let pif =
    let physical_PIF = create_physical_pif ~__context ~host () in
    let _ = create_sriov_pif ~__context ~pif:physical_PIF () in
    physical_PIF
  in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.network_sriov_already_enabled
    ~args:[Ref.string_of pif]
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network)

let test_create_on_pif_not_have_sriov_capability () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let pif = create_physical_pif ~__context ~host () in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.pif_is_not_sriov_capable
    ~args:[Ref.string_of pif]
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network)

let test_create_on_network_not_compatible_sriov () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let network = make_network ~__context ~bridge:"xapi0" () in
  let _ =
    (* attach non sriov PIF to the network *)
    let host = make_host ~__context () in
    create_physical_pif ~__context ~host ~network ()
  in
  let pif = create_physical_pif ~__context ~host () in
  Db.PIF.set_capabilities ~__context ~self:pif ~value:["sriov"];
  assert_raises_api_error
    Api_errors.network_is_not_sriov_compatible
    ~args:[Ref.string_of network]
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network)

let test_create_sriov_with_different_pci_type_into_one_network () =
  let __context = make_test_database () in
  let network = make_network ~__context ~bridge:"xapi0" () in
  let _ =
    (* attach sriov PIF to the network *)
    let host = make_host ~__context () in
    let physical_PIF =
      let pif = create_physical_pif ~__context ~host ~network () in
      Db.PIF.set_capabilities ~__context ~self:pif ~value:["sriov"];
      let pci = make_pci ~__context ~vendor_id:"101" ~device_id:"2" () in
      Db.PIF.set_PCI ~__context ~self:pif ~value:pci;
      pif
    in
    create_sriov_pif ~__context ~pif:physical_PIF ~network ()
  in
  let pif =
    let host = make_host ~__context () in
    let pif = create_physical_pif ~__context ~host () in
    Db.PIF.set_capabilities ~__context ~self:pif ~value:["sriov"];
    let pci = make_pci ~__context ~vendor_id:"99" ~device_id:"1" () in
    Db.PIF.set_PCI ~__context ~self:pif ~value:pci;
    pif
  in
  Db.PIF.set_capabilities ~__context ~self:pif ~value:["sriov"];
  assert_raises_api_error
    Api_errors.network_has_incompatible_sriov_pifs
    ~args:[Ref.string_of pif; Ref.string_of network]
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network)

let test_require_operation_on_pci_device_not_attached_not_need_reboot () =
  let __context = make_test_database () in
  let sriov_logical_PIF =
    let host = make_host ~__context () in
    let network = make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = create_physical_pif ~__context ~host ~network () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:false;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
    sriov_logical_PIF
  in
  assert_equal false (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_sysfs () =
  (* Need operate pci device when Network_sriov.configuration_mode = `sysfs *)
  let __context = make_test_database () in
  let sriov_logical_PIF =
    let host = make_host ~__context () in
    let network = make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = create_physical_pif ~__context ~host ~network () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:true;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`sysfs;
    sriov_logical_PIF
  in
  assert_equal true (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_unknown () =
  (* Need not operate pci device when Network_sriov.configuration_mode = `Unknown *)
  let __context = make_test_database () in
  let sriov_logical_PIF =
    let host = make_host ~__context () in
    let network = make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = create_physical_pif ~__context ~host ~network () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:true;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`unknown;
    sriov_logical_PIF
  in
  assert_equal false (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~self:sriov_logical_PIF)

let create_physical_pif_with_driver ~__context ~host ~network ?(driver_name="") () =
  let physical_PIF = create_physical_pif ~__context ~host ~network () in
  let pci = make_pci ~__context ~vendor_id:"99" ~device_id:"1" ~driver_name () in
  Db.PIF.set_PCI ~__context ~self:physical_PIF ~value:pci;
  physical_PIF

(* Network_sriov.configuration_mode = `modprobe *)
let test_require_operation_on_pci_device_modprobe_0 () =
  (* No other sriov has same driver with me.
     I am currently attached.
     So operate the device.*)
  let __context = make_test_database () in
  let sriov_logical_PIF =
    let host = make_host ~__context () in
    let network = make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = create_physical_pif_with_driver ~__context ~host ~network () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:true;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`modprobe;
    sriov_logical_PIF
  in
  assert_equal true (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_modprobe_1 () =
  (* No other sriov has same driver with me.
     I am not currently attached but will enable after reboot
     So operate the device.*)
  let __context = make_test_database () in
  let sriov_logical_PIF =
    let host = make_host ~__context () in
    let network = make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = create_physical_pif_with_driver ~__context ~host ~network () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:false;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:true;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`modprobe;
    sriov_logical_PIF
  in
  assert_equal true (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~self:sriov_logical_PIF)

let create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached ~requires_reboot =
  let physical_PIF = create_physical_pif_with_driver ~__context ~host ~network ~driver_name () in
  let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF ~network () in
  Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:currently_attached;
  let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
  Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:requires_reboot;
  Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`modprobe;
  sriov_logical_PIF

let test_require_operation_on_pci_device_modprobe_2 () =
  (* There are 1 other sriov has same driver name with me.
     I am currently attached but the other one is not currently attached and do not require reboot.
     So operate the device. *)
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let driver_name = "mock_driver" in
  let _ =
    let network = make_network ~__context ~bridge:"xapi0" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:false
  in
  let sriov_logical_PIF =
    let network = make_network ~__context ~bridge:"xapi1" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:true ~requires_reboot:false
  in
  assert_equal true (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_modprobe_3 () =
  (* There are 1 other sriov has same driver name with me.
     I am currently attached and the other one is currently attached.
     So do NOT operate the device. *)
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let driver_name = "mock_driver" in
  let _ =
    let network = make_network ~__context ~bridge:"xapi0" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:true ~requires_reboot:false
  in
  let sriov_logical_PIF =
    let network = make_network ~__context ~bridge:"xapi1" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:true ~requires_reboot:false
  in
  assert_equal false (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_modprobe_4 () =
  (* There are 1 other sriov has same driver name with me.
     I am NOT currently attached but require reboot. The other one is NOT currently attached and NOT require reboot.
     So operate the device.*)
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let driver_name = "mock_driver" in
  let _ =
    let network = make_network ~__context ~bridge:"xapi0" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:false
  in
  let sriov_logical_PIF =
    let network = make_network ~__context ~bridge:"xapi1" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:true
  in
  assert_equal true (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_modprobe_5 () =
  (* There are 1 other sriov has same driver name with me.
     I am NOT currently attached and require reboot. The other one is NOT currently attached but require reboot.
     So do NOT operate the device. *)
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let driver_name = "mock_driver" in
  let _ =
    let network = make_network ~__context ~bridge:"xapi0" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:true
  in
  let sriov_logical_PIF =
    let network = make_network ~__context ~bridge:"xapi1" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:true
  in
  assert_equal false (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~self:sriov_logical_PIF)


let test =
  "test_network_sriov" >:::
  [
    "test_create_internal" >:: test_create_internal;
    "test_create_on_unmanaged_pif" >:: test_create_on_unmanaged_pif;
    "test_create_network_already_connected" >:: test_create_network_already_connected;
    "test_create_on_bond_master" >:: test_create_on_bond_master;
    "test_create_on_tunnel_access" >:: test_create_on_tunnel_access;
    "test_create_on_sriov_logical" >:: test_create_on_sriov_logical;
    "test_create_on_vlan" >:: test_create_on_vlan;
    "test_create_on_vlan_on_sriov_logical" >:: test_create_on_vlan_on_sriov_logical;
    "test_create_on_pif_already_enabled_sriov" >:: test_create_on_pif_already_enabled_sriov;
    "test_create_on_pif_not_have_sriov_capability" >:: test_create_on_pif_not_have_sriov_capability;
    "test_create_on_network_not_compatible_sriov" >:: test_create_on_network_not_compatible_sriov;
    "test_create_sriov_with_different_pci_type_into_one_network" >:: test_create_sriov_with_different_pci_type_into_one_network;
    "test_require_operation_on_pci_device_not_attached_not_need_reboot" >:: test_require_operation_on_pci_device_not_attached_not_need_reboot;
    "test_require_operation_on_pci_device_sysfs" >:: test_require_operation_on_pci_device_sysfs;
    "test_require_operation_on_pci_device_unknown" >:: test_require_operation_on_pci_device_unknown;
    "test_require_operation_on_pci_device_modprobe_0" >:: test_require_operation_on_pci_device_modprobe_0;
    "test_require_operation_on_pci_device_modprobe_1" >:: test_require_operation_on_pci_device_modprobe_1;
    "test_require_operation_on_pci_device_modprobe_2" >:: test_require_operation_on_pci_device_modprobe_2;
    "test_require_operation_on_pci_device_modprobe_3" >:: test_require_operation_on_pci_device_modprobe_3;
    "test_require_operation_on_pci_device_modprobe_4" >:: test_require_operation_on_pci_device_modprobe_4;
    "test_require_operation_on_pci_device_modprobe_5" >:: test_require_operation_on_pci_device_modprobe_5;
  ]
