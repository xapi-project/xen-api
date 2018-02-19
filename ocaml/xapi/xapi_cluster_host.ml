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
let fix_pif_prerequisites ~__context (pif_ref,pif_rec) =
  (* The following is to raise an exception if there's no IP. This
     avoids making any changes to the PIF if there's something we
     simply can't fix. *)
  ignore(ip_of_pif (pif_ref,pif_rec));
  if not pif_rec.API.pIF_currently_attached then
    Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.PIF.plug ~rpc ~session_id ~self:pif_ref);
  if not pif_rec.API.pIF_disallow_unplug then begin
    debug "Setting disallow_unplug on cluster PIF";
    Db.PIF.set_disallow_unplug ~__context ~self:pif_ref ~value:true
  end

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
      | _ -> failwith "Internal error: More than one cluster_host object associated with this host"
    end
  | _ -> failwith "Internal error: Cannot have more than one Cluster object per pool currently"

let create_as_necessary ~__context ~host =
  match sync_required ~__context ~host with
  | Some cluster_ref ->
    let network = Db.Cluster.get_network ~__context ~self:cluster_ref in
    let pif = Xapi_clustering.pif_of_host ~__context network host in
    fix_pif_prerequisites ~__context pif;
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        Client.Client.Cluster_host.create rpc session_id cluster_ref host) |> ignore
  | None -> ()

let create ~__context ~cluster ~host =
  (* TODO: take network lock *)
  with_clustering_lock (fun () ->
      assert_operation_host_target_is_localhost ~__context ~host;
      assert_cluster_host_can_be_created ~__context ~host;
      let ref = Ref.make () in
      let dbg = Context.string_of_task __context in
      let uuid = Uuidm.to_string (Uuidm.create `V4) in
      let network = Db.Cluster.get_network ~__context ~self:cluster in
      let cluster_token = Db.Cluster.get_cluster_token ~__context ~self:cluster in
      let pif = pif_of_host ~__context network host in
      assert_pif_prerequisites pif;
      let ip = ip_of_pif pif in
      let ip_list = List.map (fun cluster_host ->
          Db.Cluster_host.get_host ~__context ~self:cluster_host |>
          pif_of_host ~__context network |>
          ip_of_pif
        ) (Db.Cluster.get_cluster_hosts ~__context ~self:cluster) in
      Xapi_clustering.Daemon.enable ~__context;
      let result = Cluster_client.LocalClient.join (rpc ~__context) dbg cluster_token ip ip_list in
      match result with
      | Result.Ok () ->
        Db.Cluster_host.create ~__context ~ref ~uuid ~cluster ~host ~enabled:true
          ~current_operations:[] ~allowed_operations:[] ~other_config:[];
        ref
      | Result.Error error -> handle_error error
    )

let host_destroy ~__context ~cluster_host =
  Xapi_stdext_monadic.Opt.iter (fun ch ->
      Db.Cluster_host.destroy ~__context ~self:ch
  ) cluster_host;
  Xapi_clustering.Daemon.disable ~__context

let force_destroy ~__context ~self =
  let dbg = Context.string_of_task __context in
  let host = Db.Cluster_host.get_host ~__context ~self in
  assert_operation_host_target_is_localhost ~__context ~host;
  assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack ~__context ~self;
  let result = Cluster_client.LocalClient.destroy (rpc ~__context) dbg in
  match result with
  | Result.Ok () ->
      host_destroy ~__context ~cluster_host:(Some self)
  | Result.Error error -> handle_error error

let destroy ~__context ~self =
  let dbg = Context.string_of_task __context in
  let host = Db.Cluster_host.get_host ~__context ~self in
  assert_operation_host_target_is_localhost ~__context ~host;
  assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack ~__context ~self;
  assert_cluster_host_enabled ~__context ~self ~expected:true;
  let result = Cluster_client.LocalClient.leave (rpc ~__context) dbg in
  match result with
  | Result.Ok () ->
      host_destroy ~__context ~cluster_host:(Some self)
  | Result.Error error -> handle_error error

let enable ~__context ~self =
  (* TODO: debug/error/info logging *)
  with_clustering_lock (fun () ->
      let dbg = Context.string_of_task __context in
      let host = Db.Cluster_host.get_host ~__context ~self in
      assert_operation_host_target_is_localhost ~__context ~host;
      let cluster = Db.Cluster_host.get_cluster ~__context ~self in
      let network = Db.Cluster.get_network ~__context ~self:cluster in
      let pif = pif_of_host ~__context network host in
      assert_pif_prerequisites pif;
      let ip = ip_of_pif pif in
      let init_config = {
        Cluster_interface.local_ip = ip;
        token_timeout_ms = None;
        token_coefficient_ms = None;
        name = None
      } in (* TODO: Pass these through from CLI *)
      let result = Cluster_client.LocalClient.enable (rpc ~__context) dbg init_config in
      match result with
      | Result.Ok () ->
        Db.Cluster_host.set_enabled ~__context ~self ~value:true
      | Result.Error error -> handle_error error
    )

let disable ~__context ~self =
  (* TODO: debug/error/info logging *)
  with_clustering_lock (fun () ->
      let dbg = Context.string_of_task __context in
      let host = Db.Cluster_host.get_host ~__context ~self in
      assert_operation_host_target_is_localhost ~__context ~host;
      assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack ~__context ~self;
      let result = Cluster_client.LocalClient.disable (rpc ~__context) dbg in
      match result with
      | Result.Ok () ->
          Db.Cluster_host.set_enabled ~__context ~self ~value:false
      | Result.Error error -> handle_error error
    )
