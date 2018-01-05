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

open Xapi_clustering

module D=Debug.Make(struct let name="xapi_cluster" end)
open D

(* TODO: update allowed_operations on boot/toolstack-restart *)

let validate_params ~token_timeout ~token_timeout_coefficient =
  let invalid_value x y = raise (Api_errors.(Server_error (invalid_value, [ x; y ]))) in
  if token_timeout < 1.0 then invalid_value "token_timeout" (string_of_float token_timeout);
  if token_timeout_coefficient < 0.65 then invalid_value "token_timeout_coefficient" (string_of_float token_timeout_coefficient)

let create ~__context ~network ~cluster_stack ~pool_auto_join ~token_timeout ~token_timeout_coefficient =
  Pool_features.assert_enabled ~__context ~f:Features.Corosync;
  (* TODO: take network lock *)
  with_clustering_lock (fun () ->
      let dbg = Context.string_of_task __context in
      validate_params ~token_timeout ~token_timeout_coefficient;
      let cluster_ref = Ref.make () in
      let cluster_host_ref = Ref.make () in
      let cluster_uuid = Uuidm.to_string (Uuidm.create `V4) in
      let cluster_host_uuid = Uuidm.to_string (Uuidm.create `V4) in
      (* For now we assume we have only one pool
         TODO: get master ref explicitly passed in as parameter*)
      let pool = Db.Pool.get_all ~__context |> List.hd in
      let host = Db.Pool.get_master ~__context ~self:pool in

      let pif = pif_of_host ~__context network host in
      assert_pif_prerequisites pif;
      let ip = ip_of_pif pif in

      let token_timeout_ms = Int64.of_float(token_timeout*.1000.0) in
      let token_timeout_coefficient_ms = Int64.of_float(token_timeout_coefficient*.1000.0) in
      let init_config = {
        Cluster_idl.Interface.local_ip = ip;
        token_timeout_ms = Some token_timeout_ms;
        token_coefficient_ms = Some token_timeout_coefficient_ms;
        name = None
      } in

      let result = Cluster_client.LocalClient.create (rpc ~__context) dbg init_config in
      match result with
      | Result.Ok cluster_token ->
        D.debug "Got OK from LocalClient.create";
        Db.Cluster.create ~__context ~ref:cluster_ref ~uuid:cluster_uuid ~network ~cluster_token ~cluster_stack
          ~pool_auto_join ~token_timeout:token_timeout_ms ~token_timeout_coefficient:token_timeout_coefficient_ms ~current_operations:[] ~allowed_operations:[] ~cluster_config:[]
          ~other_config:[];
        Db.Cluster_host.create ~__context ~ref:cluster_host_ref ~uuid:cluster_host_uuid ~cluster:cluster_ref ~host ~enabled:true
          ~current_operations:[] ~allowed_operations:[] ~other_config:[];
        Xapi_cluster_host_helpers.update_allowed_operations ~__context ~self:cluster_host_ref;
        D.debug "Created Cluster: %s and Cluster_host: %s" cluster_uuid cluster_host_uuid;
        cluster_ref
      | Result.Error error -> handle_error error
    )

let destroy ~__context ~self =
  let dbg = Context.string_of_task __context in
  assert_cluster_has_one_node ~__context ~self;
  let cluster_host = Db.Cluster.get_cluster_hosts ~__context ~self |> List.hd in
  assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack ~__context ~self:cluster_host;
  let result = Cluster_client.LocalClient.destroy (rpc ~__context) dbg in
  match result with
  | Result.Ok () ->
    Db.Cluster_host.destroy ~__context ~self:cluster_host;
    Db.Cluster.destroy ~__context ~self;
    Xapi_clustering.Daemon.stop ~__context
  | Result.Error error -> handle_error error

(* helper function; concurrency checks are done in implementation of Cluster.create and Cluster_host.create *)
let pool_create ~__context ~network ~cluster_stack ~token_timeout ~token_timeout_coefficient =
  validate_params ~token_timeout ~token_timeout_coefficient;
  let master = Helpers.get_master ~__context in
  let hosts = Db.Host.get_all ~__context in

  let cluster = Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.Cluster.create ~rpc ~session_id ~network ~cluster_stack:"corosync" ~pool_auto_join:true ~token_timeout ~token_timeout_coefficient)
  in

  List.iter (fun host ->
      if master <> host then
        (* We need to run this code on the slave *)
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            let cluster_host_ref = Client.Client.Cluster_host.create ~rpc ~session_id ~cluster ~host in
            D.debug "Created Cluster_host: %s" (Ref.string_of cluster_host_ref);
          )) hosts;

  cluster

(* Helper function; concurrency checks are done in implementation of Cluster.destroy and Cluster_host.destroy *)
let pool_destroy ~__context ~self =
  (* For now we assume we have only one pool, and that the cluster is the same as the pool.
     This means that the pool master must be a member of this cluster. *)
  let master = Helpers.get_master ~__context in
  let master_cluster_host =
    Xapi_clustering.find_cluster_host ~__context ~host:master
    |> Xapi_stdext_monadic.Opt.unbox
  in
  let slave_cluster_hosts =
    Db.Cluster.get_cluster_hosts ~__context ~self |> List.filter ((<>) master_cluster_host)
  in
  (* First destroy the Cluster_host objects of the slaves *)
  List.iter
    (fun cluster_host ->
       (* We need to run this code on the slave *)
       Helpers.call_api_functions ~__context (fun rpc session_id ->
           Client.Client.Cluster_host.destroy ~rpc ~session_id ~self:cluster_host)
    )
    slave_cluster_hosts;
  (* Then destroy the Cluster_host of the pool master and the Cluster itself *)
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.Cluster.destroy ~rpc ~session_id ~self)

let pool_resync ~__context ~self =
  (* First ensure that the Cluster_hosts that are enabled in xapi's DB are really enabled *)
  Db.Cluster_host.get_all ~__context |> List.iter (fun cluster_host ->
      if Db.Cluster_host.get_enabled ~__context ~self:cluster_host then
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            (* We rely on the fact that Cluster_host.enable unconditionally
               invokes the low-level enable operations and is idempotent. *)
            Client.Client.Cluster_host.enable ~rpc ~session_id ~self:cluster_host)
    );
  (* Then create the missing Cluster_hosts *)
  let pool_auto_join = Db.Cluster.get_pool_auto_join ~__context ~self in
  if pool_auto_join then begin
      Db.Host.get_all ~__context |> List.iter (fun host -> Xapi_cluster_host.create_as_necessary ~__context ~host)
  end
