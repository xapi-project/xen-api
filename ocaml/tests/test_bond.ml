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

let gen_members = T.mknlist 2

let test_create_on_unmanaged_pif () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let create_member = (T.create_physical_pif ~__context ~host ~managed:false) in
  let network = T.make_network ~__context () in
  let members = gen_members create_member in
  Alcotest.check_raises
    "test_create_on_unmanaged_pif"
    Api_errors.(Server_error (pif_unmanaged, [Ref.string_of (List.hd members)]))
    (fun () -> (Xapi_bond.create ~__context ~network ~members ~mAC:"ff:ff:ff:ff:ff:ff" ~mode:`activebackup ~properties:[]) |> ignore )

let test_create_network_already_connected () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let connected_pif = T.create_physical_pif ~__context ~network ~host () in
  let create_member = T.create_physical_pif ~__context ~host in
  let members = gen_members create_member in
  Alcotest.check_raises
    "test_create_network_already_connected"
    Api_errors.(Server_error (network_already_connected, [Ref.string_of host; Ref.string_of connected_pif]))
    (fun () -> Xapi_bond.create ~__context ~network ~members ~mAC:"ff:ff:ff:ff:ff:ff" ~mode:`activebackup ~properties:[] |> ignore)

let test_create_member_is_bond_slave () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let create_member () =
    let members = gen_members (T.create_physical_pif ~__context ~host) in
    let _ = T.create_bond_pif ~__context ~host ~members () in
    List.hd members
  in
  let members = gen_members create_member in
  Alcotest.check_raises
    "test_create_member_is_bond_slave"
    Api_errors.(Server_error (pif_already_bonded, [Ref.string_of (List.hd members)]))
    (fun () -> Xapi_bond.create ~__context ~network ~members ~mAC:"ff:ff:ff:ff:ff:ff" ~mode:`activebackup ~properties:[] |> ignore)

let test_create_member_is_vlan_master_on_physical () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let create_member () =
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    T.create_vlan_pif ~__context ~host ~pif:physical_PIF ~vlan:1L ()
  in
  let members = gen_members create_member in
  Alcotest.check_raises
    "test_create_member_is_vlan_master_on_physical"
    Api_errors.(Server_error (pif_vlan_exists, [Db.PIF.get_device_name ~__context ~self:(List.hd members)]))
    (fun () -> Xapi_bond.create ~__context ~network ~members ~mAC:"ff:ff:ff:ff:ff:ff" ~mode:`activebackup ~properties:[] |> ignore)

let test_create_member_is_vlan_master_on_sriov () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let create_member () =
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF () in
    T.create_vlan_pif ~__context ~host ~pif:sriov_logical_PIF ~vlan:1L ()
  in
  let members = gen_members create_member in
  Alcotest.check_raises
    "test_create_member_is_vlan_master_on_sriov"
    Api_errors.(Server_error (pif_vlan_exists, [Db.PIF.get_device_name ~__context ~self:(List.hd members)]))
    (fun () -> Xapi_bond.create ~__context ~network ~members ~mAC:"ff:ff:ff:ff:ff:ff" ~mode:`activebackup ~properties:[] |> ignore)

let test_create_member_is_sriov_logical () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let create_member () =
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    T.create_sriov_pif ~__context ~pif:physical_PIF ()
  in
  let members = gen_members create_member in
  Alcotest.check_raises
    "test_create_member_is_sriov_logical"
    Api_errors.(Server_error (pif_is_sriov_logical, [Ref.string_of (List.hd members)]))
    (fun () -> Xapi_bond.create ~__context ~network ~members ~mAC:"ff:ff:ff:ff:ff:ff" ~mode:`activebackup ~properties:[] |> ignore)

let test_create_member_is_tunnel_access () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let create_member () =
    let transport_PIF = T.create_physical_pif ~__context ~host () in
    T.create_tunnel_pif ~__context ~host ~pif:transport_PIF ()
  in
  let members = gen_members create_member in
  Alcotest.check_raises
    "test_create_member_is_tunnel_access"
    Api_errors.(Server_error (is_tunnel_access_pif, [Ref.string_of (List.hd members)]))
    (fun () -> Xapi_bond.create ~__context ~network ~members ~mAC:"ff:ff:ff:ff:ff:ff" ~mode:`activebackup ~properties:[] |> ignore)

let test_create_bond_into_sriov_network () =
  let __context = T.make_test_database () in
  let sriov_network =
    let host = T.make_host ~__context () in
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF () in
    Db.PIF.get_network ~__context ~self:sriov_logical_PIF
  in
  let members =
    let host = T.make_host ~__context () in
    let create_member = T.create_physical_pif ~__context ~host in
    gen_members create_member
  in
  Alcotest.check_raises
    "test_create_bond_into_sriov_network"
    Api_errors.(Server_error (network_incompatible_with_bond, [Ref.string_of sriov_network]))
    (fun () -> Xapi_bond.create ~__context ~network:sriov_network ~members ~mAC:"ff:ff:ff:ff:ff:ff" ~mode:`activebackup ~properties:[] |> ignore)

let test_create_bond_into_sriov_vlan_network () =
  let __context = T.make_test_database () in
  let sriov_vlan_network =
    let host = T.make_host ~__context () in
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF () in
    let vlan_pif = T.create_vlan_pif ~__context ~host ~vlan:1L ~pif:sriov_logical_PIF () in
    Db.PIF.get_network ~__context ~self:vlan_pif
  in
  let members =
    let host = T.make_host ~__context () in
    let create_member = T.create_physical_pif ~__context ~host in
    gen_members create_member
  in
  Alcotest.check_raises
    "test_create_bond_into_sriov_vlan_network"
    Api_errors.(Server_error (network_incompatible_with_bond, [Ref.string_of sriov_vlan_network]))
    (fun () -> Xapi_bond.create ~__context ~network:sriov_vlan_network ~members ~mAC:"ff:ff:ff:ff:ff:ff" ~mode:`activebackup ~properties:[] |> ignore)

let test =
  [ "test_create_on_unmanaged_pif", `Quick, test_create_on_unmanaged_pif
  ; "test_create_network_already_connected", `Quick, test_create_network_already_connected
  ; "test_create_member_is_bond_slave", `Quick, test_create_member_is_bond_slave
  ; "test_create_member_is_vlan_master_on_physical", `Quick, test_create_member_is_vlan_master_on_physical
  ; "test_create_member_is_vlan_master_on_sriov", `Quick, test_create_member_is_vlan_master_on_sriov
  ; "test_create_member_is_sriov_logical", `Quick, test_create_member_is_sriov_logical
  ; "test_create_member_is_tunnel_access", `Quick, test_create_member_is_tunnel_access
  ; "test_create_bond_into_sriov_network", `Quick, test_create_bond_into_sriov_network
  ; "test_create_bond_into_sriov_vlan_network", `Quick, test_create_bond_into_sriov_vlan_network
  ]
