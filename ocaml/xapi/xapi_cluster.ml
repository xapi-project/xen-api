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

(* TODO: update allowed_operations on cluster creation *)
(* TODO: update allowed_operations on boot/toolstack-restart *)

let create ~__context ~network ~cluster_stack ~pool_auto_join =
  with_clustering_lock (fun () ->
      let cluster_ref = Ref.make () in
      let cluster_host_ref = Ref.make () in
      let cluster_uuid = Uuidm.to_string (Uuidm.create `V4) in
      let cluster_host_uuid = Uuidm.to_string (Uuidm.create `V4) in
      (* For now we assume we have only one pool
         TODO: get master ref explicitely passed in as parameter*)
      let pool = Db.Pool.get_all ~__context |> List.hd in
      let host = Db.Pool.get_master ~__context ~self:pool in

      let ip = pif_of_host ~__context network host |> ip_of_pif in

      let result = Cluster_client.LocalClient.create (Cluster_client.rpc (fun () -> "")) ip in
      match result with
      | Result.Ok cluster_token ->
        D.debug "Got OK from LocalClient.create";
        Db.Cluster.create ~__context ~ref:cluster_ref ~uuid:cluster_uuid ~network ~cluster_token ~cluster_stack
          ~pool_auto_join ~current_operations:[] ~allowed_operations:[] ~cluster_config:[]
          ~other_config:[];
        Db.Cluster_host.create ~__context ~ref:cluster_host_ref ~uuid:cluster_host_uuid ~cluster:cluster_ref ~host ~enabled:true
          ~current_operations:[] ~allowed_operations:[] ~other_config:[];
        D.debug "Created Cluster: %s and Cluster_host: %s" (Ref.string_of cluster_ref) (Ref.string_of cluster_host_ref);
        cluster_ref
      | Result.Error error -> handle_error error
    )

let destroy ~__context ~self =
  (* TODO: take cluster lock ?? *)
  (* TODO: concurrency; update/use allowed/current_operations via message_forwarding ?? (not mentioned in design; should it be?) *)
  (* TODO: debug/error/info logging *)
  (* TODO: call xapi-clusterd.Local.destroy *)
  (* TODO: destroy member records *)
  Db.Cluster.destroy ~__context ~self

(* helper function; concurrency checks are done in implementation of Cluster.create and Cluster_host.create *)
let pool_create ~__context ~pool ~cluster_stack ~network =
  let master = Db.Pool.get_master ~__context ~self:pool in
  let hosts = Db.Host.get_all ~__context in

  let cluster = Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.Cluster.create ~rpc ~session_id ~network ~cluster_stack:"corosync" ~pool_auto_join:true)
  in

  List.iter (fun host ->
      if master <> host then
        (* We need to run this code on the slave *)
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            let cluster_host_ref = Client.Client.Cluster_host.create ~rpc ~session_id ~cluster ~host in
            D.debug "Created Cluster_host: %s" (Ref.string_of cluster_host_ref);
          )) hosts;

  cluster

let pool_resync ~__context ~self =
  let pool_auto_join = Db.Cluster.get_pool_auto_join ~__context ~self in
  if pool_auto_join then begin
    Helpers.call_api_functions ~__context (fun rpc session_id ->
      Db.Host.get_all ~__context |> List.iter (fun host -> Xapi_cluster_host.create_as_necessary ~__context ~host))
  end