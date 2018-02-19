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
open Db_cache_types

module D=Debug.Make(struct let name="xapi_clustering" end)
open D

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
    failwith msg

let ip_of_pif (ref,record) =
  let ip = record.API.pIF_IP in
  if ip = "" then failwith (Printf.sprintf "PIF %s does not have any IP" (Ref.string_of ref));
  debug "Got IP %s for PIF %s" ip (Ref.string_of ref);
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
  let assert_pif_permaplugged (ref,record) =
    if not record.API.pIF_disallow_unplug then failwith (Printf.sprintf "PIF %s allows unplug" record.API.pIF_uuid);
    if not record.pIF_currently_attached then failwith (Printf.sprintf "PIF %s not plugged" record.API.pIF_uuid)
  in
  assert_pif_permaplugged pif;
  ignore(ip_of_pif pif)

let handle_error error =
  (* TODO: replace with API errors? *)
  match error with
  | InternalError message -> failwith ("Internal Error: " ^ message)
  | Unix_error message -> failwith ("Unix Error: " ^ message)

let assert_cluster_host_can_be_created ~__context ~host =
  if Db.Cluster_host.get_refs_where ~__context
      ~expr:Db_filter_types.(Eq(Literal (Ref.string_of host),Field "host")) <> [] then
    failwith "Cluster host cannot be created because it already exists"

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

let with_clustering_lock_if_needed ~__context ~sr_sm_type f =
  match get_required_cluster_stacks ~__context ~sr_sm_type with
    | [] -> f ()
    | _required_cluster_stacks -> with_clustering_lock f

let find_cluster_host ~__context ~host =
  match Db.Cluster_host.get_refs_where ~__context
          ~expr:(Db_filter_types.(Eq (Field "host", Literal (Ref.string_of host)))) with
  | [ref] -> Some ref
  | _::_  -> (* should never happen; this indicates a bug *)
    let msg = "Multiple cluster_hosts found for host" in
    error "%s %s" msg (Db.Host.get_uuid ~__context ~self:host);
    raise Api_errors.(Server_error(internal_error, [msg; (Ref.string_of host)]))
  | _ -> None

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
      begin match find_cluster_host ~__context ~host with
        | Some cluster_host when (List.mem (cluster_stack_of ~cluster_host) required_cluster_stacks) ->
          assert_cluster_host_enabled ~__context ~self:cluster_host ~expected:true
        | Some _ (* incompatible cluster host *) | None ->
          raise Api_errors.(Server_error (no_compatible_cluster_host, [Ref.string_of host]))
      end
  end

(* certain cluster_host operations (such as enable, disable) must run on the host on which it is
   operating on in order to work correctly, as they must communicate directly to the local
   xapi-clusterd daemon running on the target host *)
let assert_operation_host_target_is_localhost ~__context ~host =
  if host <> Helpers.get_localhost ~__context then
    let msg = "A clustering operation was attempted from the wrong host" in
    raise Api_errors.(Server_error (internal_error, [msg]))

let assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack ~__context ~self =
  let cluster = Db.Cluster_host.get_cluster ~__context ~self in
  let cluster_stack = Db.Cluster.get_cluster_stack ~__context ~self:cluster in
  let host = Db.Cluster_host.get_host ~__context ~self in
  let pbds = List.filter (fun pbd ->
      Db.PBD.get_currently_attached ~__context ~self:pbd)
      (Db.Host.get_PBDs ~__context ~self:host) in
  let srs = List.map (fun pbd -> Db.PBD.get_SR ~__context ~self:pbd) pbds in
  List.iter
    (fun sr ->
       let sr_sm_type = Db.SR.get_type ~__context ~self:sr in
       (* XXX This check is a bit too conservative, because the SR requires
          only one of these cluster stacks to be configured and running. *)
       if List.mem cluster_stack (get_required_cluster_stacks ~__context ~sr_sm_type) then begin
         (* TODO: replace with API error *)
         failwith (Printf.sprintf "Host has attached SR whose SM requires cluster stack %s" cluster_stack)
       end
    )
    srs

let is_clustering_disabled_on_host ~__context host =
  match find_cluster_host ~__context ~host with
  | None -> true (* there is no Cluster_host, therefore it is not enabled, therefore it is disabled *)
  | Some cluster_host -> not (Db.Cluster_host.get_enabled ~__context ~self:cluster_host)

let is_clustering_enabled_on_localhost ~__context =
  let host = Helpers.get_localhost ~__context in
  not (is_clustering_disabled_on_host ~__context host)

let set_fsync_mode ~__context enabled =
  let dbconn = Db_connections.preferred_write_db in
  Xapi_stdext_monadic.Opt.iter (fun (db:Parse_db_conf.db_connection) ->
    D.debug "set_fsync_mode: setting %B" enabled;
    db.fsync_enabled <- enabled
  ) dbconn


module Daemon = struct
  let maybe_call_script ~__context script params =
    match Context.get_test_clusterd_rpc __context with
    | Some _ -> debug "in unit test, not calling %s %s" script (String.concat " " params)
    | None -> ignore (Helpers.call_script script params)

  let service = "xapi-clusterd"
  let enable ~__context =
    debug "Enabling and starting the clustering daemon";
    maybe_call_script ~__context "/usr/bin/systemctl" [ "enable"; service ];
    maybe_call_script ~__context "/usr/bin/systemctl" [ "start"; service ];
    set_fsync_mode ~__context true;
    debug "Cluster daemon: enabled & started"

  let disable ~__context =
    debug "Disabling and stopping the clustering daemon";
    maybe_call_script ~__context "/usr/bin/systemctl" [ "disable"; service ];
    maybe_call_script ~__context "/usr/bin/systemctl" [ "stop"; service ];
    set_fsync_mode ~__context false;
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
