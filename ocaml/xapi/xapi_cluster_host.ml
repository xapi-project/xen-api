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

open Cluster_interface
open Xapi_clustering

module D=Debug.Make(struct let name="xapi_cluster_host" end)
open D

(* TODO: move anything "generic" to Xapi_cluster_host_helpers, or a new Xapi_clustering file/module *)
(* TODO: update allowed_operations on cluster_host creation *)
(* TODO: update allowed_operations on boot/toolstack-restart *)

let ip_of_host ~__context ~network ~host =
  debug "Looking up PIF for network %s" (Ref.string_of network);
  let pifs = Db.PIF.get_records_where ~__context
      ~expr:Db_filter_types.(And (Eq(Literal (Ref.string_of host),Field "host"),
                                  Eq(Literal (Ref.string_of network),Field "network"))) in
  match pifs with
  | [(ref, record)] ->
    let ip = record.API.pIF_IP in
    if ip = "" then failwith (Printf.sprintf "PIF %s does not have any IP" (Ref.string_of ref));
    if not record.API.pIF_disallow_unplug then failwith (Printf.sprintf "PIF %s allows unplug" (Ref.string_of ref));
    debug "Got IP %s for host %s" ip (Ref.string_of host);
    Cluster_interface.IPv4 ip
  | _ ->
    let msg = Printf.sprintf "No PIF found for host:%s and network:%s" (Ref.string_of host) (Ref.string_of network) in
    debug "%s" msg;
    failwith msg

let handle_error error =
  (* TODO: replace with API errors? *)
  match error with
  | InternalError message -> failwith ("Internal Error: " ^ message)
  | Unix_error message -> failwith ("Unix Error: " ^ message)

let assert_cluster_host_can_be_created ~__context ~host =
  if Db.Cluster_host.get_refs_where ~__context
      ~expr:Db_filter_types.(Eq(Literal (Ref.string_of host),Field "host")) <> [] then
    failwith "Cluster host cannot be created because it already exists"

let create ~__context ~cluster ~host =
  (* TODO: take network lock *)
  with_clustering_lock (fun () ->
      assert_cluster_host_can_be_created ~__context ~host;
      let ref = Ref.make () in
      let uuid = Uuidm.to_string (Uuidm.create `V4) in
      let network = Db.Cluster.get_network ~__context ~self:cluster in
      let cluster_token = Db.Cluster.get_cluster_token ~__context ~self:cluster in
      let ip = ip_of_host ~__context ~network ~host in
      let ip_list = List.map (fun cluster_host ->
          ip_of_host ~__context ~network ~host:(Db.Cluster_host.get_host ~__context ~self:cluster_host)
        ) (Db.Cluster.get_cluster_hosts ~__context ~self:cluster) in
      let result = Cluster_client.LocalClient.join (Cluster_client.rpc (fun () -> "")) cluster_token ip ip_list in
      match result with
      | Result.Ok () ->
        Db.Cluster_host.create ~__context ~ref ~uuid ~cluster ~host ~enabled:false
          ~current_operations:[] ~allowed_operations:[] ~other_config:[];
        ref
      | Result.Error error -> handle_error error
    )

let destroy ~__context ~self =
  Db.Cluster_host.destroy ~__context ~self

let enable ~__context ~self =
  (* TODO: debug/error/info logging *)
  with_clustering_lock (fun () ->
      let host = Db.Cluster_host.get_host ~__context ~self in
      let cluster = Db.Cluster_host.get_cluster ~__context ~self in
      let network = Db.Cluster.get_network ~__context ~self:cluster in
      let ip = ip_of_host ~__context ~network ~host in
      let result = Cluster_client.LocalClient.enable (Cluster_client.rpc (fun () -> "")) ip in
      match result with
      | Result.Ok () ->
        Db.Cluster_host.set_enabled ~__context ~self ~value:true
      | Result.Error error -> handle_error error
    )

let disable ~__context ~self =
  (* TODO: debug/error/info logging *)
  with_clustering_lock (fun () ->
      let cluster = Db.Cluster_host.get_cluster ~__context ~self in
      let cluster_stack = Db.Cluster.get_cluster_stack ~__context ~self:cluster in
      let host = Db.Cluster_host.get_host ~__context ~self in
      let pbds = Db.Host.get_PBDs ~__context ~self:host in
      let srs = List.map (fun pbd -> Db.PBD.get_SR ~__context ~self:pbd) pbds in
      List.iter (fun sr ->
          let sr_type = Db.SR.get_type ~__context ~self:sr in
          let matching_sms = Db.SM.get_records_where ~__context
              ~expr:Db_filter_types.(Eq(Field "type", Literal sr_type)) in
          List.iter (fun (sm_ref, sm_rec) ->
              if List.mem cluster_stack sm_rec.API.sM_required_cluster_stack then
                failwith (Printf.sprintf "Host has attached SR whose SM requires cluster stack %s" cluster_stack) (* TODO: replace with API error *)
            ) matching_sms
        ) srs;
      let result = Cluster_client.LocalClient.disable (Cluster_client.rpc (fun () -> "")) () in
      match result with
      | Result.Ok () ->
        Db.Cluster_host.set_enabled ~__context ~self ~value:false
      | Result.Error error -> handle_error error
    )

let sync_required ~__context =
  let clusters = Db.Cluster.get_all_records ~__context in
  let localhost = Helpers.get_localhost ~__context in
  match clusters with
  | [] -> None
  | [cluster_ref, cluster_rec] -> begin
      let expr = Db_filter_types.(And (Eq (Field "host", Literal (Ref.string_of localhost)),
                                       Eq (Field "cluster", Literal (Ref.string_of cluster_ref)))) in
      let my_cluster_hosts = Db.Cluster_host.get_internal_records_where ~__context ~expr in
      match my_cluster_hosts with
      | [(_ref,_rec)] -> None
      | [] ->
        if cluster_rec.API.cluster_pool_auto_join
        then Some (cluster_ref, localhost)
        else None
      | _ -> failwith "Internal error: More than one cluster_host object associated with this host"
    end
  | _ -> failwith "Internal error: Cannot have more than one Cluster object per pool currently"

let sync_cluster_hosts ~__context =
  match sync_required ~__context with
  | Some (cluster_ref, localhost) ->
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        Client.Client.Cluster_host.create rpc session_id cluster_ref localhost) |> ignore
  | None -> ()
