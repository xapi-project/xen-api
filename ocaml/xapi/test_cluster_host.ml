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

open Xapi_cluster_host

let create_cluster ~__context pool_auto_join =
  let cluster_ref = Ref.make () in
  let cluster_uuid = Uuidm.to_string (Uuidm.create `V4) in
  let network = Test_common.make_network ~__context () in
  Db.Cluster.create ~__context ~ref:cluster_ref ~uuid:cluster_uuid ~network ~cluster_token:"token"
    ~cluster_stack:"corosync" ~allowed_operations:[] ~current_operations:[] ~pool_auto_join ~cluster_config:[]
    ~other_config:[];
  cluster_ref

let test_dbsync_join () =
  let __context = Test_common.make_test_database () in
  let cluster = create_cluster ~__context true in
  let localhost = Helpers.get_localhost ~__context in
  let result = sync_required ~__context ~host:localhost in
  OUnit.assert_equal result (Some (cluster))

let test_dbsync_nojoin () =
  let __context = Test_common.make_test_database () in
  let _cluster = create_cluster ~__context false in
  let localhost = Helpers.get_localhost ~__context in
  let result = sync_required ~__context ~host:localhost in
  OUnit.assert_equal result None

open OUnit

let pif_plug_rpc __context call =
  match call.Rpc.name, call.Rpc.params with
  | "PIF.plug", [session_id_rpc;self_rpc] ->
    let open API in
    let _session_id = ref_session_of_rpc session_id_rpc in
    let self = ref_PIF_of_rpc self_rpc in
    Db.PIF.set_currently_attached ~__context ~self ~value:true;
    Rpc.{success=true; contents = Rpc.String ""}
  | _ -> failwith "Unexpected RPC"


let test_prereq () =
  let __context = Test_common.make_test_database () in
  let exn = "we_havent_decided_on_the_exception_yet" in
  let cluster = create_cluster ~__context true in
  let network = Db.Cluster.get_network ~__context ~self:cluster in
  let localhost = Helpers.get_localhost ~__context in
  let pifref = Test_common.make_pif ~__context ~network ~host:localhost () in
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  assert_raises
    (Failure exn)
    (fun () ->
      try
        Xapi_cluster_host.check_pif_prerequisites pif
      with _ ->
        failwith exn);
  (* Put in IPv4 info *)
  Db.PIF.set_IP ~__context ~self:pifref ~value:"1.1.1.1";
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  assert_raises
    (Failure exn)
    (fun () ->
      try
        Xapi_cluster_host.check_pif_prerequisites pif
      with _ ->
        failwith exn);
  Db.PIF.set_currently_attached ~__context ~self:pifref ~value:true;
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  assert_raises
    (Failure exn)
    (fun () ->
      try
        Xapi_cluster_host.check_pif_prerequisites pif
      with _ ->
        failwith exn);
  Db.PIF.set_disallow_unplug ~__context ~self:pifref ~value:true;
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  assert_equal (Xapi_cluster_host.check_pif_prerequisites pif) ()


let test_fix_prereq () =
  let __context = Test_common.make_test_database () in
  Context.set_test_rpc __context (pif_plug_rpc __context);
  let exn = "we_havent_decided_on_the_exception_yet" in
  let cluster = create_cluster ~__context true in
  let network = Db.Cluster.get_network ~__context ~self:cluster in
  let localhost = Helpers.get_localhost ~__context in
  let pifref = Test_common.make_pif ~__context ~network ~host:localhost () in
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  assert_raises
    (Failure exn)
    (fun () ->
      try
        Xapi_cluster_host.fix_pif_prerequisites __context pif
      with _ ->
        failwith exn);
  Db.PIF.set_IP ~__context ~self:pifref ~value:"1.1.1.1";
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  Xapi_cluster_host.fix_pif_prerequisites ~__context pif;
  let pif = Xapi_clustering.pif_of_host ~__context network localhost in
  assert_equal (Xapi_cluster_host.check_pif_prerequisites pif) ()
let test =
  "test_cluster_host" >:::
  [
    "test_dbsync_join" >:: test_dbsync_join;
    "test_dbsync_nojoin" >:: test_dbsync_nojoin;
    "test_prerequisites" >:: test_prereq;
    "test_fix_prerequisites" >:: test_fix_prereq;
  ]
