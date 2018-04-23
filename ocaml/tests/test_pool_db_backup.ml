(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

let test_prepare_restore () =

  let make_context mac1 mac2 host_uuid dom0_uuid =
    let __context = T.make_test_database () in
    let master = List.hd (Db.Host.get_all ~__context) in
    Db.Host.set_uuid ~__context ~self:master ~value:host_uuid;
    List.iter
      (fun self -> Db.VM.set_uuid ~__context ~self ~value:dom0_uuid)
      (Db.Host.get_resident_VMs ~__context ~self:master);
    let slave = T.make_host ~__context ~name_label:"slave" () in
    let management_net = T.make_network ~__context ~name_label:"management network" () in
    let (_: API.ref_PIF) = T.make_pif ~__context ~network:management_net ~device:"eth0" ~host:master ~management:true ~mAC:mac1 () in
    let (_: API.ref_PIF) = T.make_pif ~__context ~network:management_net ~device:"eth0" ~host:slave ~management:true ~mAC:mac2 () in
    __context in

  let my_installation_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid in
  let my_control_uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
  let old_context = make_context "1" "2" my_installation_uuid my_control_uuid in
  let new_context = make_context "a" "b" "host-uuid" "dom0-uuid" in
  Pool_db_backup.prepare_database_for_restore ~old_context ~new_context;
  let all_hosts = Db.Host.get_all ~__context:new_context in
  (* new_context should have exactly 1 host: the master *)
  Alcotest.(check int)"test_prepare_restore: should only be 1 host" (List.length all_hosts) 1;

  let master = List.hd all_hosts in
  (* new_context master host should have PIF with MAC "a" *)
  let pif = List.hd (Db.Host.get_PIFs ~__context:new_context ~self:master) in
  let mac = Db.PIF.get_MAC ~__context:new_context ~self:pif in
  Alcotest.(check string) "test_prepare_restore: PIF should have MAC a" mac "a";

  (* new_context should have correct master host uuid *)
  let host_uuid = Db.Host.get_uuid ~__context:new_context ~self:master in
  Alcotest.(check string) "test_prepare_restore: master uuid wrong" host_uuid my_installation_uuid;

  (* new_context should have correct master dom0 uuid *)
  let dom0 = List.hd (Db.Host.get_resident_VMs ~__context:new_context ~self:master) in
  let dom0_uuid = Db.VM.get_uuid ~__context:new_context ~self:dom0 in
  Alcotest.(check string) "test_prepare_restore: master dom0 uuid wrong" dom0_uuid my_control_uuid

let test =
  [ "test_prepare_restore", `Quick, test_prepare_restore
  ]
