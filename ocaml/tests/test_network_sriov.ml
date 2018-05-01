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

module T = Test_common

let test_create_internal () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let physical_PIF = T.create_physical_pif ~__context ~host () in
  let physical_rec = Db.PIF.get_record ~__context ~self:physical_PIF in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  let sriov, logical_PIF = Xapi_network_sriov.create_internal ~__context ~physical_PIF ~physical_rec ~network in
  Alcotest.check Alcotest_comparators.(ref ())
    "test_create_internal sriov_ physical_PIF"
    sriov
    (List.hd (Db.PIF.get_sriov_physical_PIF_of ~__context ~self:physical_PIF));
  Alcotest.check Alcotest_comparators.(ref ())
    "test_create_internal sriov_logical_PIF"
    sriov
    (List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:logical_PIF));
  Alcotest.check Alcotest_comparators.(ref ())
    "test_create_internal physical_PIF"
    physical_PIF
    (Db.Network_sriov.get_physical_PIF ~__context ~self:sriov);
  Alcotest.check Alcotest_comparators.(ref ())
    "test_create_internal logical_PIF"
    logical_PIF
    (Db.Network_sriov.get_logical_PIF ~__context ~self:sriov);
  Alcotest.check Alcotest_comparators.(ref ())
    "test_create_internal network"
    network
    (Db.PIF.get_network ~__context ~self:logical_PIF);
  Alcotest.check Alcotest_comparators.(ref ())
    "test_create_internal host"
    host
    (Db.PIF.get_host ~__context ~self:logical_PIF)

let test_create_on_unmanaged_pif () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let physical_PIF = T.create_physical_pif ~__context ~host ~managed:false () in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_unmanaged_pif"
    Api_errors.(Server_error (pif_unmanaged, [Ref.string_of physical_PIF]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif:physical_PIF ~network |> ignore)

let test_create_network_already_connected () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let physical_PIF = T.create_physical_pif ~__context ~host ~network () in
  Db.PIF.set_capabilities ~__context ~self:physical_PIF ~value:["sriov"];
  Alcotest.check_raises
    "test_create_network_already_connected"
    Api_errors.(Server_error (network_already_connected, [Ref.string_of host; Ref.string_of physical_PIF]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif:physical_PIF ~network |> ignore)

let test_create_on_bond_master () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let pif =
    let members = T.mknlist 2 (T.create_physical_pif ~__context ~host) in
    T.create_bond_pif ~__context ~host ~members ()
  in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_bond_master"
    Api_errors.(Server_error (pif_is_not_physical, [Ref.string_of pif]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network |> ignore)

let test_create_on_tunnel_access () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let pif =
    let transport_PIF = T.create_physical_pif ~__context ~host () in
    T.create_tunnel_pif ~__context ~host ~pif:transport_PIF ()
  in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_tunnel_access"
    Api_errors.(Server_error (pif_is_not_physical, [Ref.string_of pif]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network |> ignore)

let test_create_on_sriov_logical () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let pif =
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    T.create_sriov_pif ~__context ~pif:physical_PIF ()
  in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_sriov_logical"
    Api_errors.(Server_error (pif_is_not_physical, [Ref.string_of pif]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network |> ignore)

let test_create_on_vlan () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let pif =
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    T.create_vlan_pif ~__context ~host ~vlan:1L ~pif:physical_PIF ()
  in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_vlan"
    Api_errors.(Server_error (pif_is_not_physical, [Ref.string_of pif]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network |> ignore)

let test_create_on_vlan_on_sriov_logical () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let pif =
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF () in
    T.create_vlan_pif ~__context ~host ~vlan:1L ~pif:sriov_logical_PIF ()
  in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_vlan_on_sriov_logical"
    Api_errors.(Server_error (pif_is_not_physical, [Ref.string_of pif]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network |> ignore)

let test_create_on_pif_already_enabled_sriov () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let pif =
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    let _ = T.create_sriov_pif ~__context ~pif:physical_PIF () in
    physical_PIF
  in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_pif_already_enabled_sriov"
    Api_errors.(Server_error (network_sriov_already_enabled, [Ref.string_of pif]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network |> ignore)

let test_create_on_pif_not_have_sriov_capability () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let pif = T.create_physical_pif ~__context ~host () in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_pif_not_have_sriov_capability"
    Api_errors.(Server_error (pif_is_not_sriov_capable, [Ref.string_of pif]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network |> ignore)

let test_create_on_network_not_compatible_sriov () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  let _ =
    (* attach non sriov PIF to the network *)
    let host = T.make_host ~__context () in
    T.create_physical_pif ~__context ~host ~network ()
  in
  let pif = T.create_physical_pif ~__context ~host () in
  Db.PIF.set_capabilities ~__context ~self:pif ~value:["sriov"];
  Alcotest.check_raises
    "test_create_on_network_not_compatible_sriov"
    Api_errors.(Server_error (network_incompatible_with_sriov, [Ref.string_of network]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network |> ignore)

let test_create_sriov_with_different_pci_type_into_one_network () =
  let __context = T.make_test_database () in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  let _ =
    (* attach sriov PIF to the network *)
    let host = T.make_host ~__context () in
    let physical_PIF =
      let pif = T.create_physical_pif ~__context ~host ~network () in
      Db.PIF.set_capabilities ~__context ~self:pif ~value:["sriov"];
      let pci = T.make_pci ~__context ~vendor_id:"101" ~device_id:"2" () in
      Db.PIF.set_PCI ~__context ~self:pif ~value:pci;
      pif
    in
    T.create_sriov_pif ~__context ~pif:physical_PIF ~network ()
  in
  let pif =
    let host = T.make_host ~__context () in
    let pif = T.create_physical_pif ~__context ~host () in
    Db.PIF.set_capabilities ~__context ~self:pif ~value:["sriov"];
    let pci = T.make_pci ~__context ~vendor_id:"99" ~device_id:"1" () in
    Db.PIF.set_PCI ~__context ~self:pif ~value:pci;
    pif
  in
  Db.PIF.set_capabilities ~__context ~self:pif ~value:["sriov"];
  Alcotest.check_raises
    "test_create_sriov_with_different_pci_type_into_one_network"
    Api_errors.(Server_error (network_has_incompatible_sriov_pifs, [Ref.string_of pif; Ref.string_of network]))
    (fun () -> Xapi_network_sriov.create ~__context ~pif ~network |> ignore)

let test_require_operation_on_pci_device_not_attached_not_need_reboot () =
  let __context = T.make_test_database () in
  let sriov_logical_PIF, sriov=
    let host = T.make_host ~__context () in
    let network = T.make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = T.create_physical_pif ~__context ~host ~network () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:false;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
    sriov_logical_PIF, sriov
  in
  Alcotest.(check bool)
    "test_require_operation_on_pci_device_not_attached_not_need_reboot"
    false
    (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~sriov ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_sysfs () =
  (* Need operate pci device when Network_sriov.configuration_mode = `sysfs *)
  let __context = T.make_test_database () in
  let sriov_logical_PIF, sriov =
    let host = T.make_host ~__context () in
    let network = T.make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = T.create_physical_pif ~__context ~host ~network () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:true;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`sysfs;
    sriov_logical_PIF, sriov
  in
  Alcotest.(check bool)
    "test_require_operation_on_pci_device_sysfs"
    true
    (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~sriov ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_unknown () =
  (* Need not operate pci device when Network_sriov.configuration_mode = `Unknown *)
  let __context = T.make_test_database () in
  let sriov_logical_PIF, sriov =
    let host = T.make_host ~__context () in
    let network = T.make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = T.create_physical_pif ~__context ~host ~network () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:true;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`unknown;
    sriov_logical_PIF, sriov
  in
  Alcotest.(check bool)
    "test_require_operation_on_pci_device_unknown"
    false
    (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~sriov ~self:sriov_logical_PIF)

let create_physical_pif_with_driver ~__context ~host ~network ?(driver_name="") () =
  let physical_PIF = T.create_physical_pif ~__context ~host ~network () in
  let pci = T.make_pci ~__context ~vendor_id:"99" ~device_id:"1" ~driver_name () in
  Db.PIF.set_PCI ~__context ~self:physical_PIF ~value:pci;
  physical_PIF

(* Network_sriov.configuration_mode = `modprobe *)
let test_require_operation_on_pci_device_modprobe_0 () =
  (* No other sriov has same driver with me.
     I am currently attached.
     So operate the device.*)
  let __context = T.make_test_database () in
  let sriov_logical_PIF, sriov =
    let host = T.make_host ~__context () in
    let network = T.make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = create_physical_pif_with_driver ~__context ~host ~network () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:true;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`modprobe;
    sriov_logical_PIF, sriov
  in
  Alcotest.(check bool)
    "test_require_operation_on_pci_device_modprobe_0"
    true
    (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~sriov ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_modprobe_1 () =
  (* No other sriov has same driver with me.
     I am not currently attached but will enable after reboot
     So operate the device.*)
  let __context = T.make_test_database () in
  let sriov_logical_PIF, sriov =
    let host = T.make_host ~__context () in
    let network = T.make_network ~__context ~bridge:"xapi0" () in
    let physical_PIF = create_physical_pif_with_driver ~__context ~host ~network () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF ~network () in
    Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:false;
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:true;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`modprobe;
    sriov_logical_PIF, sriov
  in
  Alcotest.(check bool)
    "test_require_operation_on_pci_device_modprobe_1"
    true
    (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~sriov ~self:sriov_logical_PIF)

let create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached ~requires_reboot =
  let physical_PIF = create_physical_pif_with_driver ~__context ~host ~network ~driver_name () in
  let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF ~network () in
  Db.PIF.set_currently_attached ~__context ~self:sriov_logical_PIF ~value:currently_attached;
  let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_PIF) in
  Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:requires_reboot;
  Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:`modprobe;
  sriov_logical_PIF, sriov

let test_require_operation_on_pci_device_modprobe_2 () =
  (* There are 1 other sriov has same driver name with me.
     I am currently attached but the other one is not currently attached and do not require reboot.
     So operate the device. *)
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let driver_name = "mock_driver" in
  let _ =
    let network = T.make_network ~__context ~bridge:"xapi0" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:false
  in
  let sriov_logical_PIF, sriov =
    let network = T.make_network ~__context ~bridge:"xapi1" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:true ~requires_reboot:false
  in
  Alcotest.(check bool)
    "test_require_operation_on_pci_device_modprobe_2"
    true
    (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~sriov ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_modprobe_3 () =
  (* There are 1 other sriov has same driver name with me.
     I am currently attached and the other one is currently attached.
     So do NOT operate the device. *)
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let driver_name = "mock_driver" in
  let _ =
    let network = T.make_network ~__context ~bridge:"xapi0" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:true ~requires_reboot:false
  in
  let sriov_logical_PIF, sriov =
    let network = T.make_network ~__context ~bridge:"xapi1" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:true ~requires_reboot:false
  in
  Alcotest.(check bool)
    "test_require_operation_on_pci_device_modprobe_3"
    false
    (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~sriov ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_modprobe_4 () =
  (* There are 1 other sriov has same driver name with me.
     I am NOT currently attached but require reboot. The other one is NOT currently attached and NOT require reboot.
     So operate the device.*)
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let driver_name = "mock_driver" in
  let _ =
    let network = T.make_network ~__context ~bridge:"xapi0" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:false
  in
  let sriov_logical_PIF, sriov =
    let network = T.make_network ~__context ~bridge:"xapi1" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:true
  in
  Alcotest.(check bool)
    "test_require_operation_on_pci_device_modprobe_4"
    true
    (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~sriov ~self:sriov_logical_PIF)

let test_require_operation_on_pci_device_modprobe_5 () =
  (* There are 1 other sriov has same driver name with me.
     I am NOT currently attached and require reboot. The other one is NOT currently attached but require reboot.
     So do NOT operate the device. *)
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let driver_name = "mock_driver" in
  let _ =
    let network = T.make_network ~__context ~bridge:"xapi0" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:true
  in
  let sriov_logical_PIF, sriov =
    let network = T.make_network ~__context ~bridge:"xapi1" () in
    create_modprobe_sriov_logical_pif_with_driver ~__context ~host ~network ~driver_name ~currently_attached:false ~requires_reboot:true
  in
  Alcotest.(check bool)
    "test_require_operation_on_pci_device_modprobe_5"
    false
    (Xapi_network_sriov_helpers.require_operation_on_pci_device ~__context ~sriov ~self:sriov_logical_PIF)


let test =
  [ "test_create_internal", `Quick, test_create_internal
  ; "test_create_on_unmanaged_pif", `Quick, test_create_on_unmanaged_pif
  ; "test_create_network_already_connected", `Quick, test_create_network_already_connected
  ; "test_create_on_bond_master", `Quick, test_create_on_bond_master
  ; "test_create_on_tunnel_access", `Quick, test_create_on_tunnel_access
  ; "test_create_on_sriov_logical", `Quick, test_create_on_sriov_logical
  ; "test_create_on_vlan", `Quick, test_create_on_vlan
  ; "test_create_on_vlan_on_sriov_logical", `Quick, test_create_on_vlan_on_sriov_logical
  ; "test_create_on_pif_already_enabled_sriov", `Quick, test_create_on_pif_already_enabled_sriov
  ; "test_create_on_pif_not_have_sriov_capability", `Quick, test_create_on_pif_not_have_sriov_capability
  ; "test_create_on_network_not_compatible_sriov", `Quick, test_create_on_network_not_compatible_sriov
  ; "test_create_sriov_with_different_pci_type_into_one_network", `Quick, test_create_sriov_with_different_pci_type_into_one_network
  ; "test_require_operation_on_pci_device_not_attached_not_need_reboot", `Quick, test_require_operation_on_pci_device_not_attached_not_need_reboot
  ; "test_require_operation_on_pci_device_sysfs", `Quick, test_require_operation_on_pci_device_sysfs
  ; "test_require_operation_on_pci_device_unknown", `Quick, test_require_operation_on_pci_device_unknown
  ; "test_require_operation_on_pci_device_modprobe_0", `Quick, test_require_operation_on_pci_device_modprobe_0
  ; "test_require_operation_on_pci_device_modprobe_1", `Quick, test_require_operation_on_pci_device_modprobe_1
  ; "test_require_operation_on_pci_device_modprobe_2", `Quick, test_require_operation_on_pci_device_modprobe_2
  ; "test_require_operation_on_pci_device_modprobe_3", `Quick, test_require_operation_on_pci_device_modprobe_3
  ; "test_require_operation_on_pci_device_modprobe_4", `Quick, test_require_operation_on_pci_device_modprobe_4
  ; "test_require_operation_on_pci_device_modprobe_5", `Quick, test_require_operation_on_pci_device_modprobe_5
  ]
