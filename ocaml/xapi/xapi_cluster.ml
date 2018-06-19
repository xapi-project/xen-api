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
  let invalid_value x y = raise (Api_errors.(Server_error (invalid_value, [ x; string_of_float y ]))) in
  if token_timeout < Constants.minimum_token_timeout_s then
    invalid_value "token_timeout" token_timeout;
  if token_timeout_coefficient < Constants.minimum_token_timeout_coefficient_s then
    invalid_value "token_timeout_coefficient" token_timeout_coefficient

let create ~__context ~pIF ~cluster_stack ~pool_auto_join ~token_timeout ~token_timeout_coefficient =
  assert_cluster_stack_valid ~cluster_stack;

  (* Currently we only support corosync. If we support more cluster stacks, this
   * should be replaced by a general function that checks the given cluster_stack *)
  Pool_features.assert_enabled ~__context ~f:Features.Corosync;
  with_clustering_lock __LOC__(fun () ->
      let dbg = Context.string_of_task __context in
      validate_params ~token_timeout ~token_timeout_coefficient;
      let cluster_ref = Ref.make () in
      let cluster_host_ref = Ref.make () in
      let cluster_uuid = Uuidm.to_string (Uuidm.create `V4) in
      let cluster_host_uuid = Uuidm.to_string (Uuidm.create `V4) in
      (* For now we assume we have only one pool
         TODO: get master ref explicitly passed in as parameter*)
      let host = Helpers.get_master ~__context in

      let pifrec = Db.PIF.get_record ~__context ~self:pIF in
      assert_pif_prerequisites (pIF,pifrec);
      let ip = ip_of_pif (pIF,pifrec) in

      let token_timeout_ms = Int64.of_float(token_timeout*.1000.0) in
      let token_timeout_coefficient_ms = Int64.of_float(token_timeout_coefficient*.1000.0) in
      let init_config = {
        Cluster_interface.local_ip = ip;
        token_timeout_ms = Some token_timeout_ms;
        token_coefficient_ms = Some token_timeout_coefficient_ms;
        name = None
      } in

      Xapi_clustering.Daemon.enable ~__context;
      let result = Cluster_client.LocalClient.create (rpc ~__context) dbg init_config in
      match result with
      | Result.Ok cluster_token ->
        D.debug "Got OK from LocalClient.create";
        Db.Cluster.create ~__context ~ref:cluster_ref ~uuid:cluster_uuid ~cluster_token ~cluster_stack ~pending_forget:[]
          ~pool_auto_join ~token_timeout ~token_timeout_coefficient ~current_operations:[] ~allowed_operations:[] ~cluster_config:[]
          ~other_config:[];
        Db.Cluster_host.create ~__context ~ref:cluster_host_ref ~uuid:cluster_host_uuid ~cluster:cluster_ref ~host ~enabled:true ~pIF
          ~current_operations:[] ~allowed_operations:[] ~other_config:[] ~joined:true;
        Xapi_cluster_host_helpers.update_allowed_operations ~__context ~self:cluster_host_ref;
        D.debug "Created Cluster: %s and Cluster_host: %s" (Ref.string_of cluster_ref) (Ref.string_of cluster_host_ref);
        set_ha_cluster_stack ~__context;
        cluster_ref
      | Result.Error error ->
        D.warn "Error occurred during Cluster.create";
        handle_error error
    )

let destroy ~__context ~self =
  let cluster_hosts = Db.Cluster.get_cluster_hosts ~__context ~self in
  let cluster_host = match cluster_hosts with
    | [] ->
      info "No cluster_hosts found. Proceeding with cluster destruction.";
      None
    | [ cluster_host ] -> Some (cluster_host)
    | _ ->
      let n = List.length cluster_hosts in
      raise Api_errors.(Server_error(cluster_does_not_have_one_node, [string_of_int n]))
  in
  Xapi_stdext_monadic.Opt.iter (fun ch ->
    assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack ~__context ~self:ch;
    Xapi_cluster_host.force_destroy ~__context ~self:ch
  ) cluster_host;
  Db.Cluster.destroy ~__context ~self;
  D.debug "Cluster destroyed successfully";
  set_ha_cluster_stack ~__context;
  Xapi_clustering.Daemon.disable ~__context

(* Get pool master's cluster_host, return network of PIF *)
let get_network = get_network_internal

(** Cluster.pool* functions are convenience wrappers for iterating low-level APIs over a pool.
    Concurrency checks are done in the implementation of these calls *)

let pool_create ~__context ~network ~cluster_stack ~token_timeout ~token_timeout_coefficient =
  validate_params ~token_timeout ~token_timeout_coefficient;
  let master = Helpers.get_master ~__context in
  let slave_hosts = Xapi_pool_helpers.get_slaves_list ~__context in
  let pIF,_ = pif_of_host ~__context network master in
  let cluster = Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.Cluster.create ~rpc ~session_id ~pIF ~cluster_stack
        ~pool_auto_join:true ~token_timeout ~token_timeout_coefficient)
  in

  List.iter (fun host ->
      (* Cluster.create already created cluster_host on master, so we only iterate through slaves *)
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          let pif,_ = pif_of_host ~__context network host in
          let cluster_host_ref = Client.Client.Cluster_host.create ~rpc ~session_id ~cluster ~host ~pif in
          D.debug "Created Cluster_host: %s" (Ref.string_of cluster_host_ref);
        )) slave_hosts;

  cluster

let foreach_cluster_host ~__context ~self ~(fn : rpc:(Rpc.call -> Rpc.response) ->
      session_id:API.ref_session -> self:API.ref_Cluster_host -> unit) ~log =
  let wrapper = if log then log_and_ignore_exn else (fun f -> f ()) in
  List.iter (fun self ->
    Helpers.call_api_functions ~__context
      (fun rpc session_id -> wrapper (fun () -> fn rpc session_id self)))

let pool_destroy_common ~__context ~self ~force =
  (* Prevent new hosts from joining if destroy fails *)
  Db.Cluster.set_pool_auto_join ~__context ~self ~value:false;
  let slave_cluster_hosts =
    let all_hosts = Db.Cluster.get_cluster_hosts ~__context ~self in
    let master = Helpers.get_master ~__context in
    match Xapi_clustering.find_cluster_host ~__context ~host:master with
    | None -> all_hosts
    | Some master_ch ->
      List.filter ((<>) master_ch) all_hosts
  in
  foreach_cluster_host ~__context ~self ~log:force
    ~fn:Client.Client.Cluster_host.destroy
    slave_cluster_hosts

let pool_force_destroy ~__context ~self =
  (* Set pool_autojoin:false and try to destroy slave cluster_hosts *)
  pool_destroy_common ~__context ~self ~force:true;

  (* Note we include the master here, we should attempt to force destroy it *)
  (* Now try to force_destroy, keep track of any errors here *)
  debug "Ignoring exceptions while trying to force destroy cluster hosts.";
  foreach_cluster_host ~__context ~self ~log:true
    ~fn:Client.Client.Cluster_host.force_destroy
    (Db.Cluster.get_cluster_hosts ~__context ~self);

  info "Forgetting any cluster_hosts that couldn't be destroyed.";
  foreach_cluster_host ~__context ~self ~log:true
    ~fn:Client.Client.Cluster_host.forget
    (Db.Cluster_host.get_all ~__context);

  let unforgotten_cluster_hosts = List.filter
      (fun self -> not (Db.Cluster_host.get_joined ~__context ~self))
      (Db.Cluster_host.get_all ~__context)
  in
  info "If forget failed on any remaining cluster_hosts, we now delete them competely";
  foreach_cluster_host ~__context ~self ~log:false
    ~fn:(fun ~rpc ~session_id ~self -> Db.Cluster_host.destroy ~__context ~self)
    unforgotten_cluster_hosts;

  match Db.Cluster_host.get_all ~__context with
  | [] ->
    D.debug "Successfully destroyed all cluster_hosts in pool, now destroying cluster %s"
      (Ref.string_of self);
    Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.Cluster.destroy ~rpc ~session_id ~self);
    debug "Cluster.pool_force_destroy was successful";
  | _ -> raise Api_errors.(Server_error (cluster_force_destroy_failed, [Ref.string_of self]))

let pool_destroy ~__context ~self =
  (* Set pool_autojoin:false and try to destroy slave cluster_hosts *)
  pool_destroy_common ~__context ~self ~force:false;

  (* Then destroy the Cluster_host of the pool master and the Cluster itself *)
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.Cluster.destroy ~rpc ~session_id ~self)

let pool_resync ~__context ~(self : API.ref_Cluster) =
  List.iter
    (fun host -> log_and_ignore_exn (fun () ->
           Xapi_cluster_host.create_as_necessary ~__context ~host;
           Xapi_cluster_host.resync_host ~__context ~host;
           if is_clustering_disabled_on_host ~__context host
           then raise Api_errors.(Server_error (no_compatible_cluster_host, [Ref.string_of host]))
            (* If host.clustering_enabled then resync_host should successfully
               find or create a matching cluster_host which is also enabled *)
        )
    ) (Xapi_pool_helpers.get_master_slaves_list ~__context)
