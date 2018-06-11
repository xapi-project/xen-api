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

module D=Debug.Make(struct let name="xapi_cluster_host" end)
open D

(* TODO: update allowed_operations on boot/toolstack-restart *)

(* We can't fix _all_ of the prerequisites, as we can't automatically
   create an IP address. So what we do here is to at least plug the
   thing in and ensure it has disallow unplug set. *)
let fix_pif_prerequisites ~__context (self : API.ref_PIF) =
  (* The following is to raise an exception if there's no IP. This
     avoids making any changes to the PIF if there's something we
     simply can't fix. *)
  let pif_rec self = Db.PIF.get_record ~__context ~self in
  ip_of_pif (self,pif_rec self) |> ignore;
  if not (pif_rec self).API.pIF_currently_attached then
    Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.PIF.plug ~rpc ~session_id ~self);
  if not (pif_rec self).API.pIF_disallow_unplug then begin
    debug "Setting disallow_unplug on cluster PIF %s" (Ref.string_of self);
    Db.PIF.set_disallow_unplug ~__context ~self ~value:true
  end

let call_api_function_with_alert ~__context ~msg ~cls ~obj_uuid ~body
  ~(api_func : (Rpc.call -> Rpc.response) -> API.ref_session -> unit) =
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        try
          api_func rpc session_id
        with err ->
          Backtrace.is_important err;
          let body = Printf.sprintf "Error: %s\nMessage: %s" ExnHelper.(string_of_exn err) body in
          Xapi_alert.add ~msg ~cls ~obj_uuid ~body;
          raise err
      )

(* Create xapi db object for cluster_host, resync_host calls clusterd *)
let create_internal ~__context ~cluster ~host ~pIF : API.ref_Cluster_host =
  with_clustering_lock __LOC__ (fun () ->
      assert_operation_host_target_is_localhost ~__context ~host;
      assert_pif_attached_to ~host ~pIF ~__context;
      assert_cluster_host_can_be_created ~__context ~host;
      let ref = Ref.make () in
      let uuid = Uuidm.to_string (Uuidm.create `V4) in
      Db.Cluster_host.create ~__context ~ref ~uuid ~cluster ~host ~pIF ~enabled:false
        ~current_operations:[] ~allowed_operations:[] ~other_config:[] ~joined:false;
      ref
  )

(* Helper function atomically enables clusterd and joins the cluster_host *)
let join_internal ~__context ~self =
  with_clustering_lock __LOC__ (fun () ->

      let pIF = Db.Cluster_host.get_PIF ~__context ~self in
      fix_pif_prerequisites ~__context pIF;

      let dbg = Context.string_of_task __context in
      let cluster = Db.Cluster_host.get_cluster ~__context ~self in
      let cluster_token = Db.Cluster.get_cluster_token ~__context ~self:cluster in
      let ip = ip_of_pif (pIF, Db.PIF.get_record ~__context ~self:pIF) in
      let ip_list = Xapi_stdext_std.Listext.List.filter_map (fun self ->
          let p_ref = Db.Cluster_host.get_PIF ~__context ~self in
          let p_rec = Db.PIF.get_record ~__context ~self:p_ref in
          (* parallel join: some hosts may not have an IP yet *)
          try
            let other_ip = ip_of_pif (p_ref,p_rec) in
            if other_ip <> ip then Some other_ip
            else None
          with _ -> None
        ) (Db.Cluster.get_cluster_hosts ~__context ~self:cluster) in
      if ip_list = [] then
        raise Api_errors.(Server_error (no_cluster_hosts_reachable, [Ref.string_of cluster]));

      debug "Enabling clusterd and joining cluster_host %s" (Ref.string_of self);
      Xapi_clustering.Daemon.enable ~__context;
      let result =
        Cluster_client.LocalClient.join (rpc ~__context) dbg cluster_token ip ip_list
      in
      match result with
      | Result.Ok () ->
        debug "Cluster join create was successful for cluster_host %s" (Ref.string_of self);
        Db.Cluster_host.set_joined ~__context ~self ~value:true;
        Db.Cluster_host.set_enabled ~__context ~self ~value:true;
        debug "Cluster_host %s joined and enabled" (Ref.string_of self)
      | Result.Error error ->
        warn "Error occurred when joining cluster_host %s" (Ref.string_of self);
        handle_error error
  )

(* Enable cluster_host in client layer via clusterd *)
let resync_host ~__context ~host =
  match find_cluster_host ~__context ~host with
  | None      -> () (* no clusters exist *)
  | Some self ->    (* cluster_host and cluster exist *)
    let body = Printf.sprintf "Unable to create cluster host on %s."
        (Db.Host.get_name_label ~__context ~self:host) in
    let obj_uuid = Db.Host.get_uuid ~__context ~self:host in

    call_api_function_with_alert ~__context
      ~msg:Api_messages.cluster_host_enable_failed
      ~cls:`Host ~obj_uuid ~body
      ~api_func:(fun rpc session_id ->
          (* If we have just joined, enable will prevent concurrent clustering ops *)
          if not (Db.Cluster_host.get_joined ~__context ~self)
          then join_internal ~__context ~self
          else
            if Db.Cluster_host.get_enabled ~__context ~self then begin
              (* [enable] unconditionally invokes low-level enable operations and is idempotent.
                 RPU reformats partition, losing service status, never re-enables clusterd *)
              debug "Cluster_host %s is enabled, starting up xapi-clusterd" (Ref.string_of self);
              Xapi_clustering.Daemon.enable ~__context;

              (* Note that join_internal and enable both use the clustering lock *)
              Client.Client.Cluster_host.enable rpc session_id self end
          )

(* API call split into separate functions to create in db and enable in client layer *)
let create ~__context ~cluster ~host ~pif =
  let cluster_host : API.ref_Cluster_host = create_internal ~__context ~cluster ~host ~pIF:pif in
  resync_host ~__context ~host;
  cluster_host

let destroy_op ~__context ~self meth =
  with_clustering_lock __LOC__ (fun () ->
      let dbg = Context.string_of_task __context in
      let host = Db.Cluster_host.get_host ~__context ~self in
      assert_operation_host_target_is_localhost ~__context ~host;
      assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack ~__context ~self;
      let result = Cluster_client.LocalClient.destroy (rpc ~__context) dbg in
      match result with
      | Result.Ok () ->
        Db.Cluster_host.destroy ~__context ~self;
        debug "Cluster_host.%s was successful" meth;
        Xapi_clustering.Daemon.disable ~__context
      | Result.Error error ->
        warn "Error occurred during Cluster_host.%s" meth;
        handle_error error)

let force_destroy ~__context ~self =
  destroy_op ~__context ~self "force_destroy"

let destroy ~__context ~self =
  assert_cluster_host_enabled ~__context ~self ~expected:true;
  let cluster = Db.Cluster_host.get_cluster ~__context ~self in
  let () = match Db.Cluster.get_cluster_hosts ~__context ~self:cluster with
    | [ _ ] ->
      raise Api_errors.(Server_error (cluster_host_is_last, [Ref.string_of self]))
    | _ -> ()
  in
  destroy_op ~__context ~self "destroy"

let ip_of_str str = Cluster_interface.IPv4 str

let forget ~__context ~self =
  with_clustering_lock __LOC__ (fun () ->
      let dbg = Context.string_of_task __context in
      let cluster = Db.Cluster_host.get_cluster ~__context ~self in
      let pif = Db.Cluster_host.get_PIF ~__context ~self in
      let ip = Db.PIF.get_IP ~__context ~self:pif in
      let pending = ip :: Db.Cluster.get_pending_forget ~__context ~self:cluster in
      debug "Setting pending forget to %s" (String.concat "," pending);
      Db.Cluster.set_pending_forget ~__context ~self:cluster ~value:pending;

      let pending = List.map ip_of_str pending in
      let result = Cluster_client.LocalClient.declare_dead (rpc ~__context) dbg pending in
      match result with
      | Result.Ok () ->
        debug "Successfully forgot permanently dead hosts, setting pending forget to empty";
        Db.Cluster.set_pending_forget ~__context ~self:cluster ~value:[];
        (* must not disable the daemon here, because we declared another unreachable node dead,
         * not the current one *)
        debug "Cluster_host.forget was successful"
      | Result.Error error ->
        warn "Error encountered when declaring dead cluster_host %s (did you declare all dead hosts yet?)" (Ref.string_of self);
        handle_error error
    )

let enable ~__context ~self =
  with_clustering_lock __LOC__ (fun () ->
      let dbg = Context.string_of_task __context in
      let host = Db.Cluster_host.get_host ~__context ~self in
      assert_operation_host_target_is_localhost ~__context ~host;
      let pifref = Db.Cluster_host.get_PIF ~__context ~self in
      let pifrec = Db.PIF.get_record ~__context ~self:pifref in
      assert_pif_prerequisites (pifref,pifrec);

      let ip = ip_of_pif (pifref,pifrec) in
      let init_config = {
        Cluster_interface.local_ip = ip;
        token_timeout_ms = None;
        token_coefficient_ms = None;
        name = None
      } in (* TODO: Pass these through from CLI *)
      let result = Cluster_client.LocalClient.enable (rpc ~__context) dbg init_config in
      match result with
      | Result.Ok () ->
        Db.Cluster_host.set_enabled ~__context ~self ~value:true;
        debug "Cluster_host.enable was successful for cluster_host: %s" (Ref.string_of self)
      | Result.Error error ->
        warn "Error encountered when enabling cluster_host %s" (Ref.string_of self);
        handle_error error
    )

let disable ~__context ~self =
  with_clustering_lock __LOC__ (fun () ->
      let dbg = Context.string_of_task __context in
      let host = Db.Cluster_host.get_host ~__context ~self in
      assert_operation_host_target_is_localhost ~__context ~host;
      assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack ~__context ~self;

      let result = Cluster_client.LocalClient.disable (rpc ~__context) dbg in
      match result with
      | Result.Ok () ->
        Db.Cluster_host.set_enabled ~__context ~self ~value:false;
        debug "Cluster_host.disable was successful for cluster_host: %s" (Ref.string_of self)
      | Result.Error error ->
        warn "Error encountered when disabling cluster_host %s" (Ref.string_of self);
        handle_error error
    )

let disable_clustering ~__context =
  let host = Helpers.get_localhost ~__context in
  match Xapi_clustering.find_cluster_host ~__context ~host with
  | None -> info "No cluster host found"
  | Some self ->
     info "Disabling cluster_host %s" (Ref.string_of self);
     disable ~__context ~self

let sync_required ~__context ~host =
  let clusters = Db.Cluster.get_all_records ~__context in
  match clusters with
  | [] -> None
  | [cluster_ref, cluster_rec] -> begin
      let expr = Db_filter_types.(And (Eq (Field "host", Literal (Ref.string_of host)),
                                       Eq (Field "cluster", Literal (Ref.string_of cluster_ref)))) in
      let my_cluster_hosts = Db.Cluster_host.get_internal_records_where ~__context ~expr in
      match my_cluster_hosts with
      | [(_ref,_rec)] -> None
      | [] ->
        if cluster_rec.API.cluster_pool_auto_join
        then Some cluster_ref
        else None
      | _ -> raise Api_errors.(Server_error (internal_error, [ "Host cannot be associated with more than one cluster_host"; Ref.string_of host ]))
    end
  | _ -> raise Api_errors.(Server_error (internal_error, ["Cannot have more than one Cluster object per pool currently"]))

(* If cluster found without local cluster_host, create one in db *)
let create_as_necessary ~__context ~host =
  match sync_required ~__context ~host with
  | Some cluster -> (* assume pool autojoin set *)
    let network = get_network_internal ~__context ~self:cluster in
    let pIF,_ = Xapi_clustering.pif_of_host ~__context network host in
    create_internal ~__context ~cluster ~host ~pIF |> ignore
  | None -> ()

