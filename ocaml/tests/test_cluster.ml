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

let test_clusterd_rpc ~__context call =
  let token = "test_token" in
  match call.Rpc.name, call.Rpc.params with
  | "create", _ ->
     Rpc.{success = true; contents = Rpc.String token }
  | ("enable" | "disable" | "destroy"), _ ->
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

let create_cluster ~__context =
  Context.set_test_rpc __context (test_rpc ~__context);
  Context.set_test_clusterd_rpc __context (test_clusterd_rpc ~__context);
  let network = Test_common.make_network ~__context () in
  let localhost = Helpers.get_localhost ~__context in
  let pifref = Test_common.make_pif ~__context ~network ~host:localhost () in
  Db.PIF.set_IP ~__context ~self:pifref ~value:"192.0.2.1";
  Db.PIF.set_currently_attached ~__context ~self:pifref ~value:true;
  Db.PIF.set_disallow_unplug ~__context ~self:pifref ~value:true;
  Xapi_cluster.create ~__context ~network ~cluster_stack:"corosync" ~pool_auto_join:true ~token_timeout:1. ~token_timeout_coefficient:1.

let test_create_destroy_status () =
  let __context = Test_common.make_test_database () in
  let cluster = create_cluster ~__context in
  pool_destroy ~__context ~self:cluster

let test_enable () =
  let __context = Test_common.make_test_database () in
  let cluster = create_cluster ~__context in
  (* simulate xapi getting restarted *)

  Create_storage.maybe_reenable_cluster_host __context;
  pool_destroy ~__context ~self:cluster

let test =
  [ "test_create_destroy_service_status", `Quick, test_create_destroy_status
  ; "test_enable", `Quick, test_enable
  ]
