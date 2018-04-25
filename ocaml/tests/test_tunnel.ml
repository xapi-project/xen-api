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
  let network = T.make_network ~__context () in
  let transport_PIF = T.make_pif ~__context ~network ~host () in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  let tunnel, access_PIF = Xapi_tunnel.create_internal ~__context ~transport_PIF ~network ~host in

  Alcotest.check (Alcotest_comparators.ref ())
    "get tunnel access PIF"
    tunnel
    (List.hd (Db.PIF.get_tunnel_access_PIF_of ~__context ~self:access_PIF));
  Alcotest.check (Alcotest_comparators.ref ())
    "get tunnel transport PIF"
    tunnel
    (List.hd (Db.PIF.get_tunnel_transport_PIF_of ~__context ~self:transport_PIF));
  Alcotest.check (Alcotest_comparators.ref ())
    "get transport PIF"
    transport_PIF
    (Db.Tunnel.get_transport_PIF ~__context ~self:tunnel);
  Alcotest.check (Alcotest_comparators.ref ())
    "get access PIF"
    access_PIF
    (Db.Tunnel.get_access_PIF ~__context ~self:tunnel);
  Alcotest.check (Alcotest_comparators.ref ())
    "get network"
    network
    (Db.PIF.get_network ~__context ~self:access_PIF);
  Alcotest.check (Alcotest_comparators.ref ())
    "get host"
    host
    (Db.PIF.get_host ~__context ~self:access_PIF)

let test_create_on_unmanaged_pif () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let transport_PIF = T.create_physical_pif ~__context ~host ~managed:false () in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_unmanaged_pif"
    Api_errors.(Server_error (pif_unmanaged, [Ref.string_of transport_PIF]))
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF ~network |> ignore)

let test_create_network_already_connected () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let network = T.make_network ~__context () in
  let transport_PIF = T.create_physical_pif ~__context ~host ~network ~managed:false () in
  Alcotest.check_raises
    "test_create_network_already_connected"
    Api_errors.(Server_error (network_already_connected, [Ref.string_of host; Ref.string_of transport_PIF]))
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF ~network |> ignore)

let test_create_on_bond_slave () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let transport_PIF =
    let members = T.mknlist 2 (T.create_physical_pif ~__context ~host) in
    let _ = T.create_bond_pif ~__context ~host ~members () in
    List.hd members
  in
  let network = T.make_network ~__context ~bridge:"xapi0" () in
  Alcotest.check_raises
    "test_create_on_bond_slave"
    Api_errors.(Server_error (cannot_add_tunnel_to_bond_slave, [Ref.string_of transport_PIF]))
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF ~network |> ignore)

let test_create_on_tunnel_access () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let transport_PIF = T.create_physical_pif ~__context ~host () in
  let access_PIF = T.create_tunnel_pif ~__context ~host ~pif:transport_PIF () in
  let network = T.make_network ~__context ~bridge:"xapi1" () in
  Alcotest.check_raises
    "test_create_on_tunnel_access"
    Api_errors.(Server_error (is_tunnel_access_pif, [Ref.string_of access_PIF]))
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF:access_PIF ~network |> ignore)

let test_create_on_sriov_logical () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let physical_PIF = T.create_physical_pif ~__context ~host () in
  let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF () in
  let network = T.make_network ~__context ~bridge:"xapi01" () in
  Alcotest.check_raises
    "test_create_on_sriov_logical"
    Api_errors.(Server_error (cannot_add_tunnel_to_sriov_logical, [Ref.string_of sriov_logical_PIF]))
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF:sriov_logical_PIF ~network |> ignore)

let test_create_on_vlan_on_sriov_logical () =
  let __context = T.make_test_database () in
  let host = T.make_host ~__context () in
  let physical_PIF = T.create_physical_pif ~__context ~host () in
  let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF () in
  let transport_PIF = T.create_vlan_pif ~__context ~host ~pif:sriov_logical_PIF ~vlan:1L () in
  let network = T.make_network ~__context ~bridge:"xapi01" () in
  Alcotest.check_raises
    "test_create_on_vlan_on_sriov_logical"
    Api_errors.(Server_error (cannot_add_tunnel_to_vlan_on_sriov_logical, [Ref.string_of transport_PIF]))
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF ~network |> ignore)

let test_create_tunnel_into_sriov_network () =
  let __context = T.make_test_database () in
  let sriov_network =
    let host = T.make_host ~__context () in
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF () in
    Db.PIF.get_network ~__context ~self:sriov_logical_PIF
  in
  let pif =
    let host = T.make_host ~__context () in
    T.create_physical_pif ~__context ~host ()
  in
  Alcotest.check_raises
    "test_create_tunnel_into_sriov_network"
    Api_errors.(Server_error (network_incompatible_with_tunnel, [Ref.string_of sriov_network]))
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF:pif ~network:sriov_network |> ignore)

let test_create_tunnel_into_sriov_vlan_network () =
  let __context = T.make_test_database () in
  let sriov_vlan_network =
    let host = T.make_host ~__context () in
    let physical_PIF = T.create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = T.create_sriov_pif ~__context ~pif:physical_PIF () in
    let vlan_pif = T.create_vlan_pif ~__context ~host ~vlan:1L ~pif:sriov_logical_PIF () in
    Db.PIF.get_network ~__context ~self:vlan_pif
  in
  let pif =
    let host = T.make_host ~__context () in
    T.create_physical_pif ~__context ~host ()
  in
  Alcotest.check_raises
    "test_create_tunnel_into_sriov_vlan_network"
    Api_errors.(Server_error (network_incompatible_with_tunnel, [Ref.string_of sriov_vlan_network]))
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF:pif ~network:sriov_vlan_network |> ignore)

let test =
  [ "test_create_internal", `Quick, test_create_internal
  ; "test_create_on_unmanaged_pif", `Quick, test_create_on_unmanaged_pif
  ; "test_create_network_already_connected", `Quick, test_create_network_already_connected
  ; "test_create_on_bond_slave", `Quick, test_create_on_bond_slave
  ; "test_create_on_tunnel_access", `Quick, test_create_on_tunnel_access
  ; "test_create_on_sriov_logical", `Quick, test_create_on_sriov_logical
  ; "test_create_on_vlan_on_sriov_logical", `Quick, test_create_on_vlan_on_sriov_logical
  ; "test_create_tunnel_into_sriov_network", `Quick, test_create_tunnel_into_sriov_network
  ; "test_create_tunnel_into_sriov_vlan_network", `Quick, test_create_tunnel_into_sriov_vlan_network
  ]
