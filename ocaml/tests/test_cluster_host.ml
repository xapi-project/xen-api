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
  Db.Cluster.create ~__context ~ref:cluster_ref ~uuid:cluster_uuid ~cluster_token:"token"
    ~cluster_stack:Constants.default_smapiv3_cluster_stack
    ~token_timeout:Constants.default_token_timeout_s
    ~token_timeout_coefficient:Constants.default_token_timeout_coefficient_s ~allowed_operations:[]
    ~current_operations:[] ~pool_auto_join ~cluster_config:[] ~other_config:[] ~pending_forget:[];
  cluster_ref

let check_cluster_option =
  Alcotest.(check (option (Alcotest_comparators.ref ()) ))

let test_dbsync_join () =
  let __context = Test_common.make_test_database () in
  let cluster = create_cluster ~__context true in
  let localhost = Helpers.get_localhost ~__context in
  let result = sync_required ~__context ~host:localhost in
  check_cluster_option "test_dbsync_join" (Some (cluster)) result

let test_dbsync_nojoin () =
  let __context = Test_common.make_test_database () in
  let _cluster = create_cluster ~__context false in
  let localhost = Helpers.get_localhost ~__context in
  let result = sync_required ~__context ~host:localhost in
  check_cluster_option "test_dbsync_nojoin" None result

let pif_plug_rpc __context call =
  match call.Rpc.name, call.Rpc.params with
  | "PIF.plug", [session_id_rpc;self_rpc] ->
    let open API in
    let _session_id = ref_session_of_rpc session_id_rpc in
    let self = ref_PIF_of_rpc self_rpc in
    Db.PIF.set_currently_attached ~__context ~self ~value:true;
    Rpc.{success=true; contents = Rpc.String ""}
  | "Cluster_host.create", [session_id_rpc;cluster_rpc;host_rpc;pif_rpc] ->
    let open API in
    let _session_id = ref_session_of_rpc session_id_rpc in
    let cluster = ref_Cluster_of_rpc cluster_rpc in
    let host = ref_host_of_rpc host_rpc in
    let pIF = ref_PIF_of_rpc pif_rpc in
    ignore(Test_common.make_cluster_host ~__context ~cluster ~host ~pIF ());
    Rpc.{success=true; contents = Rpc.String ""}
  | _ -> failwith "Unexpected RPC"


let test_fix_prereq () =
  let __context = Test_common.make_test_database () in
  Context.set_test_rpc __context (pif_plug_rpc __context);
  let network = Test_common.make_network ~__context () in
  let host = Helpers.get_localhost ~__context in
  let pifref = Test_common.make_pif ~__context ~network ~host () in
  Alcotest.check_raises
    "Should fail when checking PIF prequisites"
    Api_errors.(Server_error (pif_has_no_network_configuration, [ Ref.string_of pifref ]))
    (fun () -> Xapi_cluster_host.fix_pif_prerequisites __context pifref);
  Db.PIF.set_IP ~__context ~self:pifref ~value:"1.1.1.1";
  Xapi_cluster_host.fix_pif_prerequisites ~__context pifref;
  let pif = Xapi_clustering.pif_of_host ~__context network host in
  Alcotest.(check unit)
    "PIF prerequisites have now been fixed"
    () (Xapi_clustering.assert_pif_prerequisites pif)

let test_create_as_necessary () =
  let __context = Test_common.make_test_database () in
  Context.set_test_rpc __context (pif_plug_rpc __context);
  let cluster = create_cluster ~__context true in
  let localhost = Helpers.get_localhost ~__context in
  let network = Test_common.make_network ~__context () in
  let pifref = Test_common.make_pif ~__context ~network ~host:localhost () in
  Db.PIF.set_IP ~__context ~self:pifref ~value:"1.1.1.1";
  let _pif = Xapi_clustering.pif_of_host ~__context network localhost in
  let result = sync_required ~__context ~host:localhost in
  check_cluster_option "sync_required without an existing cluster_host" (Some cluster) result;
  Alcotest.check_raises
    "create_as_necessary should fail if autojoin is set and the pool master has no cluster_host"
    Api_errors.(Server_error (internal_error,
      [ Printf.sprintf "No cluster_host exists on master" ]))
    (fun () -> Xapi_cluster_host.create_as_necessary ~__context ~host:localhost);
  let _ = Test_common.make_cluster_host ~__context ~pIF:(fst _pif) ~host:(Helpers.get_master ~__context) ~cluster () in
  Xapi_cluster_host.create_as_necessary ~__context ~host:localhost;
  let result = sync_required ~__context ~host:localhost in
  check_cluster_option "sync_required with an existing cluster_host" None result;
  let host = Test_common.make_host ~__context () in
  let result = sync_required ~__context ~host in
  check_cluster_option
    "sync_required with an existing cluster_host on master but not given host"
    (Some cluster) result

(* CA-275728 *)
let test_destroy_forbidden_when_sr_attached () =
  let __context = Test_common.make_test_database () in
  let cluster_stack = "mock_cluster_stack" in
  let host = Helpers.get_localhost ~__context in
  let cluster_host =
    let (_, cluster_host) = Test_common.make_cluster_and_cluster_host ~__context ~cluster_stack () in
    Db.Cluster_host.set_host ~__context ~self:cluster_host ~value:host;
    cluster_host
  in
  let sR =
    let sr_type = "mock_sr_type" in
    let _sm : _ API.Ref.t = Test_common.make_sm ~__context ~_type:sr_type ~required_cluster_stack:[cluster_stack] () in
    Test_common.make_sr ~__context ~_type:sr_type ()
  in
  let _pbd : _ API.Ref.t = Test_common.make_pbd ~__context ~host ~sR ~currently_attached:true () in
  Alcotest.check_raises
    ("Should raise cluster_stack_in_use: [ " ^ cluster_stack ^ " ] ")
    Api_errors.(Server_error (cluster_stack_in_use, [ cluster_stack ]))
    (fun () -> Xapi_cluster_host.destroy ~__context ~self:cluster_host)

type declare_dead_args = {
  dead_members: Cluster_interface.address list;
  dbg: string
} [@@deriving rpcty]

let test_clusterd_rpc ~__context call =
  match call.Rpc.name, call.Rpc.params with
  | "declare-dead", [args] ->
    let args = Rpcmarshal.unmarshal declare_dead_args.Rpc.Types.ty args |> Rresult.R.get_ok in
    let all = Db.Cluster_host.get_all ~__context in
    let ndead = List.length args.dead_members in
    let nall = List.length all in
    Printf.printf "dead_members: %d, all: %d\n" ndead nall;
    if ndead = nall - 1 then
      Rpc.{success = true; contents = Rpc.rpc_of_unit () }
    else
      let err = Cluster_interface.InternalError "Remaining hosts are not all alive" in
      (* in the test we must declare N-1 as dead before it succeeds *)
      Rpc.failure (Rpcmarshal.marshal Cluster_interface.error.Rpc.Types.ty err)
  | name, params ->
    failwith (Printf.sprintf "Unexpected RPC: %s(%s)" name (String.concat " " (List.map Rpc.to_string params)))

let test_rpc ~__context call =
  match call.Rpc.name, call.Rpc.params with
  | "Cluster_host.forget", [_session; self] ->
    let open API in
    Xapi_cluster_host.forget ~__context ~self:(ref_Cluster_host_of_rpc self);
    Rpc.{success = true; contents = Rpc.String "" }
  | "host.apply_guest_agent_config", _ ->
    Rpc.{success = true; contents = Rpc.rpc_of_unit () }
  | name, params ->
    failwith (Printf.sprintf "Unexpected RPC: %s(%s)" name (String.concat " " (List.map Rpc.to_string params)))

let make ~__context extra_hosts =
  Context.set_test_rpc __context (test_rpc ~__context);
  Context.set_test_clusterd_rpc __context (test_clusterd_rpc ~__context);
  Test_common.make_cluster_and_hosts ~__context extra_hosts


let test_forget () =
  let __context = Test_common.make_test_database () in
  let host2 = Test_common.make_host ~__context () in
  let cluster, original_cluster_hosts = make ~__context [host2] in

  Xapi_host.destroy ~__context ~self:host2;
  let pending = Db.Cluster.get_pending_forget ~__context ~self:cluster in
  Alcotest.(check (list string)) "no pending forgets"
              [] pending;

  Db_gc_util.gc_Cluster_hosts ~__context;
  let cluster_hosts = Db.Cluster.get_cluster_hosts ~__context ~self:cluster in
  Alcotest.(check (list Alcotest_comparators.(ref ())) "surviving cluster host"
              [List.hd original_cluster_hosts] cluster_hosts)


let test_forget2 () =
  let __context = Test_common.make_test_database () in
  let host2 = Test_common.make_host ~__context () in
  let host3 = Test_common.make_host __context () in
  let cluster, original_cluster_hosts = make ~__context [host2; host3] in

  Xapi_host.destroy ~__context ~self:host3;

  let pending = Db.Cluster.get_pending_forget ~__context ~self:cluster in
  Alcotest.(check (list string) "1 pending forgets"
              ["192.0.2.3"] pending);

  Xapi_host.destroy ~__context ~self:host2;

  Db_gc_util.gc_Cluster_hosts ~__context;
  let cluster_hosts = Db.Cluster.get_cluster_hosts ~__context ~self:cluster in
  Alcotest.(check (list Alcotest_comparators.(ref ())) "surviving cluster host"
              [List.hd original_cluster_hosts] cluster_hosts);

  let pending = Db.Cluster.get_pending_forget ~__context ~self:cluster in
  Alcotest.(check (list string) "no pending forgets"
              [] pending)


let test =
  [ "test_dbsync_join", `Quick, test_dbsync_join
  ; "test_dbsync_nojoin", `Quick, test_dbsync_nojoin
  ; "test_fix_prerequisites", `Quick, test_fix_prereq
  ; "test_create_as_necessary", `Quick, test_create_as_necessary
  ; "test_destroy_forbidden_when_sr_attached", `Quick, test_destroy_forbidden_when_sr_attached
  ; "test_forget", `Quick, test_forget
  ; "test_forget2", `Quick, test_forget2
  ]
