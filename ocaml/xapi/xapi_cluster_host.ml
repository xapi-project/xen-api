(*
 * Copyright (C) 2006-2017 Citrix Systems Inc.
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
(* Module that defines API functions for VDI objects
 * @group XenAPI functions
*)

module D=Debug.Make(struct let name="xapi_cluster_host" end)
open D
open Cluster_interface

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
  match error with
  | InternalError message -> failwith ("Internal Error: " ^ message)
  | Unix_error message -> failwith ("Unix Error: " ^ message)

let assert_cluster_host_can_be_created ~__context ~host =
  if Db.Cluster_host.get_refs_where ~__context
      ~expr:Db_filter_types.(Eq(Literal (Ref.string_of host),Field "host")) <> [] then
    failwith "Cluster cannot be created because it already exists"

let create ~__context ~cluster ~host =
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

let destroy ~__context ~self =
  Db.Cluster_host.destroy ~__context ~self

let enable ~__context ~self =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "enable" ]))

let disable ~__context ~self =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "disable" ]))
