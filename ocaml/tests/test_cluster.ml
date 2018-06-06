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

open Xapi_cluster

(** NOTE: This mock rpc is also used by tests in test_clustering *)

let test_clusterd_rpc ~__context call =
  let token = "test_token" in
  match call.Rpc.name, call.Rpc.params with
  | "create", _ ->
    Rpc.{success = true; contents = Rpc.String token }
  | ("enable" | "disable" | "destroy" | "leave"), _ ->
    Rpc.{success = true; contents = Rpc.Null }
  | name, params ->
    failwith (Printf.sprintf "Unexpected RPC: %s(%s)" name (String.concat " " (List.map Rpc.to_string params)))

let test_rpc ~__context call =
  match call.Rpc.name, call.Rpc.params with
  | "Cluster_host.destroy", [self] ->
    let open API in
    Xapi_cluster_host.destroy ~__context ~self:(ref_Cluster_host_of_rpc self);
    Rpc.{success = true; contents = Rpc.String "" }
  | "Cluster.destroy", [_session; self] ->
    let open API in
    Xapi_cluster.destroy ~__context ~self:(ref_Cluster_of_rpc self);
    Rpc.{success = true; contents = Rpc.String "" }
  | name, params ->
    failwith (Printf.sprintf "Unexpected RPC: %s(%s)" name (String.concat " " (List.map Rpc.to_string params)))

let create_cluster ~__context ?(cluster_stack=Constants.default_smapiv3_cluster_stack) ?(test_clusterd_rpc=test_clusterd_rpc) () =
  Context.set_test_rpc __context (test_rpc ~__context);
  Context.set_test_clusterd_rpc __context (test_clusterd_rpc ~__context);
  let network = Test_common.make_network ~__context () in
  let localhost = Helpers.get_localhost ~__context in
  let pIF = Test_common.make_pif ~__context ~network ~host:localhost () in
  Db.PIF.set_IP ~__context ~self:pIF ~value:"192.0.2.1";
  Db.PIF.set_currently_attached ~__context ~self:pIF ~value:true;
  Db.PIF.set_disallow_unplug ~__context ~self:pIF ~value:true;
  Xapi_cluster.create ~__context ~pIF ~cluster_stack ~pool_auto_join:true ~token_timeout:1. ~token_timeout_coefficient:1.

let test_create_destroy_status () =
  let __context = Test_common.make_test_database () in
  let cluster = create_cluster ~__context () in
  pool_destroy ~__context ~self:cluster

let test_enable () =
  let __context = Test_common.make_test_database () in
  let cluster = create_cluster ~__context () in

  (* simulate xapi getting restarted *)
  begin match Xapi_clustering.find_cluster_host ~__context ~host:Helpers.(get_localhost ~__context) with
    | Some self -> Xapi_cluster_host.enable ~__context ~self
    | None -> Alcotest.fail "Couldn't find freshly-created cluster_host"
  end;
  pool_destroy ~__context ~self:cluster

let test_invalid_cluster_stack () =
  let __context = Test_common.make_test_database () in
  let cluster_stack = "invalid_cluster_stack" in
  Alcotest.check_raises
    "Cluster.create should fail upon receiving an invalid cluster stack"
    Api_errors.(Server_error (invalid_cluster_stack, [ cluster_stack ]))
    (fun () -> create_cluster ~__context ~cluster_stack () |> ignore)

let test_create_cleanup () =
  let __context = Test_common.make_test_database () in
  let test_failed_clusterd_rpc ~__context call =
    match call.Rpc.name, call.Rpc.params with
    | "create", _ ->
       Rpc.{ success = false
           ; contents = Rpcmarshal.marshal
                          (Cluster_interface.error.Rpc.Types.ty)
                          (Cluster_interface.InternalError "Cluster.create failed")
           }
    | _, _ ->
     Rpc.{success = true; contents = Rpc.Null }
  in
  try
    create_cluster ~__context ~test_clusterd_rpc:test_failed_clusterd_rpc () |> ignore;
    Alcotest.fail "Cluster.create should have failed"
  with
  | e ->
    print_endline (ExnHelper.string_of_exn e);
    Alcotest.(check (slist (Alcotest_comparators.ref ()) compare))
      "Cluster refs should be destroyed"
      [] (Db.Cluster.get_all ~__context);
    Alcotest.(check (slist (Alcotest_comparators.ref ()) compare))
      "Cluster_host refs should be destroyed"
    [] (Db.Cluster_host.get_all ~__context)

let test =
  [ "test_create_destroy_service_status", `Quick, test_create_destroy_status
  ; "test_enable", `Quick, test_enable
  ; "test_invalid_cluster_stack", `Quick, test_invalid_cluster_stack
  ; "test_create_cleanup", `Quick, test_create_cleanup
  ]
