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

module D=Debug.Make(struct let name="xapi_clustering" end)
open D

(* Called by Cluster.create/destroy *)
let set_ha_cluster_stack ~__context =
  let self = Helpers.get_pool ~__context in
  let value = Cluster_stack_constraints.choose_cluster_stack ~__context in
  Db.Pool.set_ha_cluster_stack ~__context ~self ~value

(* host-local clustering lock *)
let clustering_lock_m = Mutex.create ()

let with_clustering_lock f =
  debug "Trying to grab host-local clustering lock...";
  Stdext.Threadext.Mutex.execute clustering_lock_m
    (fun () ->
       Stdext.Pervasiveext.finally
         (fun () ->
            debug "Grabbed host-local clustering lock; executing function...";
            f ())
         (fun () -> debug "Function execution finished; returned host-local clustering lock."))

(* Note we have to add type annotations to network/host here because they're only used in the context of
  Db.PIF.get_records_where, and they're just strings there *)
let pif_of_host ~__context (network : API.ref_network) (host : API.ref_host) =
  debug "Looking up PIF for network %s" (Ref.string_of network);
  let pifs = Db.PIF.get_records_where ~__context
      ~expr:Db_filter_types.(And (Eq(Literal (Ref.string_of host),Field "host"),
                                  Eq(Literal (Ref.string_of network),Field "network"))) in
  match pifs with
  | [(ref, record)] ->
    (ref, record)
  | _ ->
    let msg = Printf.sprintf "No PIF found for host:%s and network:%s" (Ref.string_of host) (Ref.string_of network) in
    debug "%s" msg;
    raise Api_errors.(Server_error (internal_error, [ msg ]))

let ip_of_pif (ref,record) =
  let ip = record.API.pIF_IP in
  if ip = "" then raise Api_errors.(Server_error (pif_has_no_network_configuration, [ Ref.string_of ref ]));
  Cluster_interface.IPv4 ip

(** [assert_pif_prerequisites (pif_ref,pif_rec)] raises an exception if any of
    the prerequisites of using a PIF for clustering are unmet. These
    prerequisites are:
    {ul
    {- that the PIF has an IPv4 address}
    {- that the PIF is currently_attached}
    {- that the PIF has disallow_unplug set}
    }*)
let assert_pif_prerequisites pif =
  let (pif_ref, record) = pif in
  let assert_pif_permaplugged (pif_ref,record) =
    if not record.API.pIF_disallow_unplug then
      raise Api_errors.(Server_error (pif_allows_unplug, [ Ref.string_of pif_ref ] ));
    if not record.pIF_currently_attached then
      raise Api_errors.(Server_error (required_pif_is_unplugged, [ Ref.string_of pif_ref ] ))
  in
  assert_pif_permaplugged pif;
  ignore (ip_of_pif pif);
  debug "Got IP %s for PIF %s" record.API.pIF_IP (Ref.string_of pif_ref)

let assert_pif_attached_to ~__context ~host ~pif =
  if not (List.mem pif (Db.Host.get_PIFs ~__context ~self:host)) then
    raise Api_errors.(Server_error (pif_not_attached_to_host, [Ref.string_of pif; Ref.string_of host]))

let handle_error = function
  | InternalError message -> raise Api_errors.(Server_error (internal_error, [ message ]))
  | Unix_error message -> failwith ("Unix Error: " ^ message)

let assert_cluster_host_can_be_created ~__context ~host =
  match Db.Cluster_host.get_refs_where ~__context
      ~expr:Db_filter_types.(Eq(Literal (Ref.string_of host),Field "host")) with
  | [] -> ()
  | _ -> raise Api_errors.(Server_error (internal_error, [ "Cluster host cannot be created because it already exists" ]))

(** One of the cluster stacks returned by
    [get_required_cluster_stacks context sr_sm_type]
    should be configured and running for SRs of type [sr_sm_type] to work. *)
let get_required_cluster_stacks ~__context ~sr_sm_type =
  let sms_matching_sr_type =
    Db.SM.get_records_where ~__context
      ~expr:Db_filter_types.(Eq(Field "type", Literal sr_sm_type))
  in
  sms_matching_sr_type
  |> List.map (fun (_sm_ref, sm_rec) -> sm_rec.API.sM_required_cluster_stack)
  (* We assume that we only have one SM for each SR type, so this is only to satisfy type checking *)
  |> List.flatten

let assert_cluster_stack_valid ~cluster_stack =
  if not (List.mem cluster_stack Constants.supported_smapiv3_cluster_stacks)
  then raise Api_errors.(Server_error (invalid_cluster_stack, [ cluster_stack ]))

let with_clustering_lock_if_needed ~__context ~sr_sm_type f =
  match get_required_cluster_stacks ~__context ~sr_sm_type with
    | [] -> f ()
    | _required_cluster_stacks -> with_clustering_lock f

let with_clustering_lock_if_cluster_exists ~__context f =
  match Db.Cluster.get_all ~__context with
    | [] -> f ()
    | _ -> with_clustering_lock f

let find_cluster_host ~__context ~host =
  match Db.Cluster_host.get_refs_where ~__context
          ~expr:(Db_filter_types.(Eq (Field "host", Literal (Ref.string_of host)))) with
  | [ref] -> Some ref
  | _::_  -> (* should never happen; this indicates a bug *)
    let msg = "Multiple cluster_hosts found for host" in
    error "%s %s" msg (Db.Host.get_uuid ~__context ~self:host);
    raise Api_errors.(Server_error(internal_error, [msg; (Ref.string_of host)]))
  | _ -> None

let get_network_internal ~__context ~self =
  let cluster_host = match find_cluster_host ~__context ~host:Helpers.(get_master ~__context) with
  | Some cluster_host -> cluster_host
  | None ->
    raise Api_errors.(Server_error (internal_error,
    [ Printf.sprintf "No cluster_host master found for cluster %s" (Ref.string_of self) ]))
  in
  let master_pif = Db.Cluster_host.get_PIF ~__context ~self:cluster_host in
  let cluster_network = Db.PIF.get_network ~__context ~self:master_pif in
  if List.exists
    (fun cluster_host ->
      let pif = Db.Cluster_host.get_PIF ~__context ~self:cluster_host in
      let cluster_host_network = Db.PIF.get_network ~__context ~self:pif in
      cluster_host_network <> cluster_network
    ) (Db.Cluster_host.get_all ~__context)
  then
    debug "Not all cluster hosts of cluster %s on same network %s"
      (Ref.string_of self) (Ref.string_of cluster_network);

  cluster_network

let assert_cluster_host_enabled ~__context ~self ~expected =
  let actual = Db.Cluster_host.get_enabled ~__context ~self in
  if actual <> expected then
    match expected with
    | true  -> raise Api_errors.(Server_error(clustering_disabled, [Ref.string_of self]))
    | false -> raise Api_errors.(Server_error(clustering_enabled, [Ref.string_of self]))

let assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type =
  begin match get_required_cluster_stacks ~__context ~sr_sm_type with
    | [] -> ()
    | required_cluster_stacks ->
      (* One of these [required_cluster_stacks] should be configured and running *)
      let cluster_stack_of ~cluster_host =
        let cluster = Db.Cluster_host.get_cluster ~__context ~self:cluster_host in
        Db.Cluster.get_cluster_stack ~__context ~self:cluster
      in
      let error_no_cluster_host_found condition =
        debug "No_cluster_host found%s" condition;
        raise Api_errors.(Server_error (no_compatible_cluster_host, [Ref.string_of host]))
      in
      begin match find_cluster_host ~__context ~host with
        | Some cluster_host when (List.mem (cluster_stack_of ~cluster_host) required_cluster_stacks) ->
          assert_cluster_host_enabled ~__context ~self:cluster_host ~expected:true
        | Some _ -> error_no_cluster_host_found " with matching cluster_stack"
        | None -> error_no_cluster_host_found ""
      end
  end

(* certain cluster_host operations (such as enable, disable) must run on the host on which it is
   operating on in order to work correctly, as they must communicate directly to the local
   xapi-clusterd daemon running on the target host *)
let assert_operation_host_target_is_localhost ~__context ~host =
  if host <> Helpers.get_localhost ~__context then
    raise Api_errors.(Server_error (internal_error, [ "A clustering operation was attempted from the wrong host" ]))

let assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack ~__context ~self =
  let cluster = Db.Cluster_host.get_cluster ~__context ~self in
  let cluster_stack = Db.Cluster.get_cluster_stack ~__context ~self:cluster in
  let host = Db.Cluster_host.get_host ~__context ~self in
  let pbds = List.filter (fun pbd ->
      Db.PBD.get_currently_attached ~__context ~self:pbd)
      (Db.Host.get_PBDs ~__context ~self:host) in
  let srs = List.map (fun pbd -> Db.PBD.get_SR ~__context ~self:pbd) pbds in
  if List.exists
    (fun sr ->
       (* XXX This check is a bit too conservative, because the SR requires
          only one of these cluster stacks to be configured and running. *)
       let sr_sm_type = Db.SR.get_type ~__context ~self:sr in
       List.mem cluster_stack (get_required_cluster_stacks ~__context ~sr_sm_type)
    ) srs
  then raise Api_errors.(Server_error (cluster_stack_in_use, [ cluster_stack ]))

module Daemon = struct
  let maybe_call_script ~__context script params =
    match Context.get_test_clusterd_rpc __context with
    | Some _ -> debug "in unit test, not calling %s %s" script (String.concat " " params)
    | None -> ignore (Helpers.call_script script params)

  let service = "xapi-clusterd"
  let enable ~__context =
    let port = (string_of_int !Xapi_globs.xapi_clusterd_port) in
    debug "Enabling and starting the clustering daemon";
    maybe_call_script ~__context !Xapi_globs.firewall_port_config_script ["open"; port];
    maybe_call_script ~__context "/usr/bin/systemctl" [ "enable"; service ];
    maybe_call_script ~__context "/usr/bin/systemctl" [ "start"; service ];
    debug "Cluster daemon: enabled & started"

  let disable ~__context =
    let port = (string_of_int !Xapi_globs.xapi_clusterd_port) in
    debug "Disabling and stopping the clustering daemon";
    maybe_call_script ~__context "/usr/bin/systemctl" [ "disable"; service ];
    maybe_call_script ~__context "/usr/bin/systemctl" [ "stop"; service ];
    maybe_call_script ~__context !Xapi_globs.firewall_port_config_script ["close"; port];
    debug "Cluster daemon: disabled & stopped"
end

(* xapi-clusterd only listens on message-switch,
 * the URL here would be for calling xapi-clusterd through an HTTP interface,
 * but that is not supported (yet).
 * Instead of returning an empty URL which wouldn't work just raise an
 * exception. *)
let rpc ~__context =
  match Context.get_test_clusterd_rpc __context with
  | Some rpc -> rpc
  | None ->
     Cluster_client.rpc (fun () -> failwith "Can only communicate with xapi-clusterd through message-switch")

let is_clustering_disabled_on_host ~__context host =
  match find_cluster_host ~__context ~host with
  | None -> true (* there is no Cluster_host, therefore it is not enabled, therefore it is disabled *)
  | Some cluster_host -> not (Db.Cluster_host.get_enabled ~__context ~self:cluster_host)

let compute_corosync_max_host_failures ~__context =
  let all_hosts = Db.Host.get_all ~__context in
  let nhosts = List.length (all_hosts) in
  let disabled_hosts = List.length (List.filter (fun host -> is_clustering_disabled_on_host ~__context host = true ) all_hosts) in
  let corosync_ha_max_hosts = ((nhosts - disabled_hosts  - 1) / 2) + disabled_hosts in
  corosync_ha_max_hosts
