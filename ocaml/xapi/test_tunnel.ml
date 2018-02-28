
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
  let network = make_network ~__context () in
  let transport_PIF = make_pif ~__context ~network ~host () in
  let network = make_network ~__context ~bridge:"xapi0" () in
  let tunnel, access_PIF = Xapi_tunnel.create_internal ~__context ~transport_PIF ~network ~host in
  assert_equal tunnel (List.hd (Db.PIF.get_tunnel_access_PIF_of ~__context ~self:access_PIF));
  assert_equal tunnel (List.hd (Db.PIF.get_tunnel_transport_PIF_of ~__context ~self:transport_PIF));
  assert_equal transport_PIF (Db.Tunnel.get_transport_PIF ~__context ~self:tunnel);
  assert_equal access_PIF (Db.Tunnel.get_access_PIF ~__context ~self:tunnel);
  assert_equal network (Db.PIF.get_network ~__context ~self:access_PIF);
  assert_equal host (Db.PIF.get_host ~__context ~self:access_PIF)

let test_create_on_unmanaged_pif () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let transport_PIF = create_physical_pif ~__context ~host ~managed:false () in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.pif_unmanaged
    ~args:[Ref.string_of transport_PIF]
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF ~network)

let test_create_network_already_connected () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let network = make_network ~__context () in
  let transport_PIF = create_physical_pif ~__context ~host ~network ~managed:false () in
  assert_raises_api_error
    Api_errors.network_already_connected
    ~args:[Ref.string_of host; Ref.string_of transport_PIF]
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF ~network)

let test_create_on_bond_slave () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let transport_PIF =
    let members = mknlist 2 (create_physical_pif ~__context ~host) in
    let _ = create_bond_pif ~__context ~host ~members () in
    List.hd members
  in
  let network = make_network ~__context ~bridge:"xapi0" () in
  assert_raises_api_error
    Api_errors.cannot_add_tunnel_to_bond_slave
    ~args:[Ref.string_of transport_PIF]
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF ~network)

let test_create_on_tunnel_access () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let transport_PIF = create_physical_pif ~__context ~host () in
  let access_PIF = create_tunnel_pif ~__context ~host ~pif:transport_PIF () in
  let network = make_network ~__context ~bridge:"xapi1" () in
  assert_raises_api_error
    Api_errors.is_tunnel_access_pif
    ~args:[Ref.string_of access_PIF]
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF:access_PIF ~network)

let test_create_on_sriov_logical () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let physical_PIF = create_physical_pif ~__context ~host () in
  let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF () in
  let network = make_network ~__context ~bridge:"xapi01" () in
  assert_raises_api_error
    Api_errors.cannot_add_tunnel_to_sriov_logical
    ~args:[Ref.string_of sriov_logical_PIF]
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF:sriov_logical_PIF ~network)

let test_create_on_vlan_on_sriov_logical () =
  let __context = make_test_database () in
  let host = make_host ~__context () in
  let physical_PIF = create_physical_pif ~__context ~host () in
  let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF () in
  let transport_PIF = create_vlan_pif ~__context ~host ~pif:sriov_logical_PIF ~vlan:1L () in
  let network = make_network ~__context ~bridge:"xapi01" () in
  assert_raises_api_error
    Api_errors.cannot_add_tunnel_to_vlan_on_sriov_logical
    ~args:[Ref.string_of transport_PIF]
    (fun () -> Xapi_tunnel.create ~__context ~transport_PIF ~network)

let test_create_tunnel_into_sriov_network () =
  let __context = make_test_database () in
  let sriov_network =
    let host = make_host ~__context () in
    let physical_PIF = create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF () in
    Db.PIF.get_network ~__context ~self:sriov_logical_PIF
  in
  let pif =
    let host = make_host ~__context () in
    create_physical_pif ~__context ~host ()
  in
  assert_raises_api_error
    Api_errors.network_incompatible_with_tunnel
    ~args:[Ref.string_of sriov_network]
    (fun () ->
       Xapi_tunnel.create ~__context ~transport_PIF:pif ~network:sriov_network)

let test_create_tunnel_into_sriov_vlan_network () =
  let __context = make_test_database () in
  let sriov_vlan_network =
    let host = make_host ~__context () in
    let physical_PIF = create_physical_pif ~__context ~host () in
    let sriov_logical_PIF = create_sriov_pif ~__context ~pif:physical_PIF () in
    let vlan_pif = create_vlan_pif ~__context ~host ~vlan:1L ~pif:sriov_logical_PIF () in
    Db.PIF.get_network ~__context ~self:vlan_pif
  in
  let pif =
    let host = make_host ~__context () in
    create_physical_pif ~__context ~host ()
  in
  assert_raises_api_error
    Api_errors.network_incompatible_with_tunnel
    ~args:[Ref.string_of sriov_vlan_network]
    (fun () ->
       Xapi_tunnel.create ~__context ~transport_PIF:pif ~network:sriov_vlan_network)

let test =
  "test_tunnel" >:::
  [
    "test_create_internal" >:: test_create_internal;
    "test_create_on_unmanaged_pif" >:: test_create_on_unmanaged_pif;
    "test_create_network_already_connected" >:: test_create_network_already_connected;
    "test_create_on_bond_slave" >:: test_create_on_bond_slave;
    "test_create_on_tunnel_access" >:: test_create_on_tunnel_access;
    "test_create_on_sriov_logical" >:: test_create_on_sriov_logical;
    "test_create_on_vlan_on_sriov_logical" >:: test_create_on_vlan_on_sriov_logical;
    "test_create_tunnel_into_sriov_network" >:: test_create_tunnel_into_sriov_network;
    "test_create_tunnel_into_sriov_vlan_network" >:: test_create_tunnel_into_sriov_vlan_network;
  ]
