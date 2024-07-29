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
open Xapi_cluster_helpers

module D = Debug.Make (struct let name = "xapi_clustering" end)

open D

(* Called by Cluster.create/destroy *)
let set_ha_cluster_stack ~__context =
  let self = Helpers.get_pool ~__context in
  let value = Cluster_stack_constraints.choose_cluster_stack ~__context in
  Db.Pool.set_ha_cluster_stack ~__context ~self ~value

(* host-local clustering lock *)
let clustering_lock_m = Locking_helpers.Named_mutex.create "clustering"

let with_clustering_lock where f =
  debug "Trying to grab host-local clustering lock... (%s)" where ;
  Locking_helpers.Named_mutex.execute clustering_lock_m (fun () ->
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () ->
          debug "Grabbed host-local clustering lock; executing function... (%s)"
            where ;
          f ()
        )
        (fun () ->
          debug
            "Function execution finished; returned host-local clustering lock. \
             (%s)"
            where
        )
  )

(* Note we have to add type annotations to network/host here because they're only used in the context of
   Db.PIF.get_records_where, and they're just strings there *)
let pif_of_host ~__context (network : API.ref_network) (host : API.ref_host) =
  debug "Looking up PIF for network %s" (Ref.string_of network) ;
  let pifs =
    Db.PIF.get_records_where ~__context
      ~expr:
        Xapi_database.Db_filter_types.(
          And
            ( Eq (Literal (Ref.string_of host), Field "host")
            , Eq (Literal (Ref.string_of network), Field "network")
            )
        )
  in
  match pifs with
  | [(ref, record)] ->
      (ref, record)
  | _ ->
      let msg =
        Printf.sprintf "No PIF found for host:%s and network:%s"
          (Ref.string_of host) (Ref.string_of network)
      in
      debug "%s" msg ;
      raise Api_errors.(Server_error (internal_error, [msg]))

let ip_of_pif (ref, record) =
  let ip = record.API.pIF_IP in
  if ip = "" then
    raise
      Api_errors.(
        Server_error (pif_has_no_network_configuration, [Ref.string_of ref])
      ) ;
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
  let pif_ref, record = pif in
  let assert_pif_permaplugged (pif_ref, record) =
    if not record.API.pIF_disallow_unplug then
      raise
        Api_errors.(Server_error (pif_allows_unplug, [Ref.string_of pif_ref])) ;
    if not record.pIF_currently_attached then
      raise
        Api_errors.(
          Server_error (required_pif_is_unplugged, [Ref.string_of pif_ref])
        )
  in
  assert_pif_permaplugged pif ;
  ignore (ip_of_pif pif) ;
  debug "Got IP %s for PIF %s" record.API.pIF_IP (Ref.string_of pif_ref)

let assert_pif_attached_to ~__context ~host ~pIF =
  if not (List.mem pIF (Db.Host.get_PIFs ~__context ~self:host)) then
    raise
      Api_errors.(
        Server_error
          (pif_not_attached_to_host, [Ref.string_of pIF; Ref.string_of host])
      )

let handle_error = function
  | InternalError message ->
      raise Api_errors.(Server_error (internal_error, [message]))
  | Unix_error message ->
      failwith ("Unix Error: " ^ message)

let assert_cluster_host_can_be_created ~__context ~host =
  let expr =
    Xapi_database.Db_filter_types.(
      Eq (Literal (Ref.string_of host), Field "host")
    )
  in
  match Db.Cluster_host.get_refs_where ~__context ~expr with
  | [] ->
      ()
  | _ ->
      raise
        Api_errors.(
          Server_error
            ( internal_error
            , ["Cluster host cannot be created because it already exists"]
            )
        )

(** One of the cluster stacks returned by
    [get_required_cluster_stacks context sr_sm_type]
    should be configured and running for SRs of type [sr_sm_type] to work. *)
let get_required_cluster_stacks ~__context ~sr_sm_type =
  let expr =
    Xapi_database.Db_filter_types.(Eq (Field "type", Literal sr_sm_type))
  in
  let sms_matching_sr_type = Db.SM.get_records_where ~__context ~expr in
  sms_matching_sr_type
  |> List.map (fun (_sm_ref, sm_rec) -> sm_rec.API.sM_required_cluster_stack)
  (* We assume that we only have one SM for each SR type, so this is only to satisfy type checking *)
  |> List.flatten

let assert_cluster_stack_valid ~cluster_stack =
  if not (List.mem cluster_stack Constants.supported_smapiv3_cluster_stacks)
  then
    raise Api_errors.(Server_error (invalid_cluster_stack, [cluster_stack]))

let with_clustering_lock_if_needed ~__context ~sr_sm_type where f =
  match get_required_cluster_stacks ~__context ~sr_sm_type with
  | [] ->
      f ()
  | _required_cluster_stacks ->
      with_clustering_lock where f

let with_clustering_lock_if_cluster_exists ~__context where f =
  match Db.Cluster.get_all ~__context with
  | [] ->
      f ()
  | _ ->
      with_clustering_lock where f

let find_cluster_host ~__context ~host =
  let expr =
    Xapi_database.Db_filter_types.(
      Eq (Field "host", Literal (Ref.string_of host))
    )
  in
  match Db.Cluster_host.get_refs_where ~__context ~expr with
  | [ref] ->
      Some ref
  | _ :: _ ->
      (* should never happen; this indicates a bug *)
      let msg = "Multiple cluster_hosts found for host" in
      error "%s %s" msg (Db.Host.get_uuid ~__context ~self:host) ;
      raise Api_errors.(Server_error (internal_error, [msg; Ref.string_of host]))
  | _ ->
      None

(** Best-effort attempt to find a network common to the entire cluster *)
let get_network_internal ~__context ~self =
  let network_of_cluster_host self =
    Db.Cluster_host.get_PIF ~__context ~self |> fun self ->
    Db.PIF.get_network ~__context ~self
  in
  let common network =
    List.for_all (fun self -> network = network_of_cluster_host self)
  in
  match Db.Cluster.get_cluster_hosts ~__context ~self with
  | [] ->
      failwith ("No cluster_hosts found for cluster " ^ Ref.string_of self)
  | ch :: other_chs when common (network_of_cluster_host ch) other_chs ->
      network_of_cluster_host ch
  | _ ->
      failwith ("No common network found for cluster " ^ Ref.string_of self)

let assert_cluster_host_enabled ~__context ~self ~expected =
  let actual = Db.Cluster_host.get_enabled ~__context ~self in
  if actual <> expected then
    match expected with
    | true ->
        raise
          Api_errors.(Server_error (clustering_disabled, [Ref.string_of self]))
    | false ->
        raise
          Api_errors.(Server_error (clustering_enabled, [Ref.string_of self]))

(* certain cluster_host operations (such as enable, disable) must run on the host on which it is
   operating on in order to work correctly, as they must communicate directly to the local
   xapi-clusterd daemon running on the target host *)
let assert_operation_host_target_is_localhost ~__context ~host =
  if host <> Helpers.get_localhost ~__context then
    raise
      Api_errors.(
        Server_error
          ( internal_error
          , ["A clustering operation was attempted from the wrong host"]
          )
      )

let assert_cluster_host_has_no_attached_sr_which_requires_cluster_stack
    ~__context ~self =
  let cluster = Db.Cluster_host.get_cluster ~__context ~self in
  let cluster_stack = Db.Cluster.get_cluster_stack ~__context ~self:cluster in
  let host = Db.Cluster_host.get_host ~__context ~self in
  let pbds =
    List.filter
      (fun pbd -> Db.PBD.get_currently_attached ~__context ~self:pbd)
      (Db.Host.get_PBDs ~__context ~self:host)
  in
  let srs = List.map (fun pbd -> Db.PBD.get_SR ~__context ~self:pbd) pbds in
  if
    List.exists
      (fun sr ->
        (* XXX This check is a bit too conservative, because the SR requires
           only one of these cluster stacks to be configured and running. *)
        let sr_sm_type = Db.SR.get_type ~__context ~self:sr in
        List.mem cluster_stack
          (get_required_cluster_stacks ~__context ~sr_sm_type)
      )
      srs
  then
    raise Api_errors.(Server_error (cluster_stack_in_use, [cluster_stack]))

module Daemon = struct
  let enabled = ref false

  let maybe_call_script ~__context script params =
    match Context.get_test_clusterd_rpc __context with
    | Some _ ->
        debug "in unit test, not calling %s %s" script (String.concat " " params)
    | None ->
        ignore (Helpers.call_script script params)

  let service = "xapi-clusterd"

  let enable ~__context =
    let port = string_of_int !Xapi_globs.xapi_clusterd_port in
    debug "Enabling and starting the clustering daemon" ;
    ( try maybe_call_script ~__context !Xapi_globs.systemctl ["cat"; service]
      with _ ->
        (* call_script already logged the error *)
        D.info "No clustering implementation is available" ;
        raise Api_errors.(Server_error (not_implemented, ["Cluster.create"]))
    ) ;
    ( try
        maybe_call_script ~__context
          !Xapi_globs.firewall_port_config_script
          ["open"; port] ;
        maybe_call_script ~__context !Xapi_globs.systemctl ["enable"; service] ;
        maybe_call_script ~__context !Xapi_globs.systemctl ["start"; service]
      with _ ->
        raise
          Api_errors.(
            Server_error
              (internal_error, [Printf.sprintf "could not start %s" service])
          )
    ) ;
    enabled := true ;
    debug "Cluster daemon: enabled & started"

  let disable ~__context =
    let port = string_of_int !Xapi_globs.xapi_clusterd_port in
    debug "Disabling and stopping the clustering daemon" ;
    enabled := false ;
    maybe_call_script ~__context !Xapi_globs.systemctl ["disable"; service] ;
    maybe_call_script ~__context !Xapi_globs.systemctl ["stop"; service] ;
    maybe_call_script ~__context
      !Xapi_globs.firewall_port_config_script
      ["close"; port] ;
    debug "Cluster daemon: disabled & stopped"

  let restart ~__context =
    debug "Attempting to restart the clustering daemon" ;
    maybe_call_script ~__context !Xapi_globs.systemctl ["restart"; service] ;
    debug "Cluster daemon: restarted"
end

(* xapi-clusterd only listens on message-switch,
 * the URL here would be for calling xapi-clusterd through an HTTP interface,
 * but that is not supported (yet).
 * Instead of returning an empty URL which wouldn't work just raise an
 * exception. *)
let rpc ~__context =
  if not !Daemon.enabled then
    raise
      Api_errors.(
        Server_error
          ( Api_errors.operation_not_allowed
          , ["clustering daemon has not been started yet"]
          )
      ) ;
  match Context.get_test_clusterd_rpc __context with
  | Some rpc ->
      fun req -> rpc req |> Idl.IdM.return
  | None ->
      Cluster_client.rpc (fun () ->
          failwith
            "Can only communicate with xapi-clusterd through message-switch"
      )

let maybe_switch_cluster_stack_version ~__context ~self ~cluster_stack =
  if Xapi_cluster_helpers.corosync3_enabled ~__context then
    if Xapi_fist.fail_corosync_upgrade () then
      handle_error (InternalError "simulated corosync upgrade failure")
    else
      let dbg = Context.string_of_task_and_tracing __context in
      let result =
        Cluster_client.LocalClient.switch_cluster_stack (rpc ~__context) dbg
          cluster_stack
      in
      match Idl.IdM.run @@ Cluster_client.IDL.T.get result with
      | Ok () ->
          debug "cluster stack switching was successful for cluster_host: %s"
            (Ref.string_of self)
      | Error error ->
          warn "Error encountered when switching cluster stack cluster_host %s"
            (Ref.string_of self) ;
          handle_error error

let assert_cluster_host_quorate ~__context ~self =
  (* With the latest kernel GFS2 would hang on mount if clustering is not working yet,
   * whereas previously we got a 'Transport endpoint not connected' error.
   * Ensure that we are quorate now: even if we have enabled the cluster host we may not have
   * achieved quorum yet if we have just booted and haven't seen enough hosts.
   * Do this via an API call rather than reading a field in the database, because the field in the
   * database could be out of date.
   * *)
  let result =
    Cluster_client.LocalClient.diagnostics (rpc ~__context)
      "assert_cluster_host_quorate"
  in
  match Idl.IdM.run @@ Cluster_client.IDL.T.get result with
  | Ok diag ->
      debug "Local cluster host is quorate: %b"
        diag.Cluster_interface.is_quorate ;
      if not diag.Cluster_interface.is_quorate then
        raise
          Api_errors.(
            Server_error (cluster_host_not_joined, [Ref.string_of self])
          )
  | Error error ->
      warn "Cannot query cluster host quorate status" ;
      handle_error error

let assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type
    =
  match get_required_cluster_stacks ~__context ~sr_sm_type with
  | [] ->
      ()
  | required_cluster_stacks -> (
      (* One of these [required_cluster_stacks] should be configured and running *)
      let cluster_stack_of ~cluster_host =
        let cluster =
          Db.Cluster_host.get_cluster ~__context ~self:cluster_host
        in
        Db.Cluster.get_cluster_stack ~__context ~self:cluster
      in
      let error_no_cluster_host_found condition =
        debug "No_cluster_host found %s" condition ;
        raise
          Api_errors.(
            Server_error (no_compatible_cluster_host, [Ref.string_of host])
          )
      in
      match find_cluster_host ~__context ~host with
      | Some cluster_host
        when List.mem (cluster_stack_of ~cluster_host) required_cluster_stacks
        ->
          assert_cluster_host_enabled ~__context ~self:cluster_host
            ~expected:true ;
          assert_cluster_host_quorate ~__context ~self:cluster_host
      | Some _ ->
          error_no_cluster_host_found " with matching cluster_stack"
      | None ->
          error_no_cluster_host_found ""
    )

let is_clustering_disabled_on_host ~__context host =
  match find_cluster_host ~__context ~host with
  | None ->
      true
      (* there is no Cluster_host, therefore it is not enabled, therefore it is disabled *)
  | Some cluster_host ->
      not (Db.Cluster_host.get_enabled ~__context ~self:cluster_host)

let compute_corosync_max_host_failures ~__context =
  let all_hosts = Db.Host.get_all ~__context in
  let nhosts = List.length all_hosts in
  let disabled_hosts =
    List.length
      (List.filter
         (fun host -> is_clustering_disabled_on_host ~__context host)
         all_hosts
      )
  in
  let corosync_ha_max_hosts =
    ((nhosts - disabled_hosts - 1) / 2) + disabled_hosts
  in
  corosync_ha_max_hosts

module Watcher = struct
  let on_corosync_update ~__context ~cluster updates =
    debug
      "%s: Received %d updates from corosync_notifyd, run diagnostics to get \
       new state"
      __FUNCTION__ (List.length updates) ;
    let m =
      Cluster_client.LocalClient.diagnostics (rpc ~__context)
        "update quorum api fields with diagnostics"
    in
    match Idl.IdM.run @@ Cluster_client.IDL.T.get m with
    | Ok diag ->
        ( Db.Cluster.set_is_quorate ~__context ~self:cluster
            ~value:diag.is_quorate ;
          let all_cluster_hosts = Db.Cluster_host.get_all ~__context in
          let live_hosts =
            Db.Cluster_host.get_refs_where ~__context
              ~expr:(Eq (Field "live", Literal "true"))
          in
          let dead_hosts =
            List.filter (fun h -> not (List.mem h live_hosts)) all_cluster_hosts
          in
          let ip_ch =
            List.map
              (fun ch ->
                let pIF = Db.Cluster_host.get_PIF ~__context ~self:ch in
                let ipstr =
                  ip_of_pif (pIF, Db.PIF.get_record ~__context ~self:pIF)
                  |> ipstr_of_address
                in
                (ipstr, ch)
              )
              all_cluster_hosts
          in
          let current_time = API.Date.now () in
          match diag.quorum_members with
          | None ->
              List.iter
                (fun self ->
                  Db.Cluster_host.set_live ~__context ~self ~value:false ;
                  Db.Cluster_host.set_last_update_live ~__context ~self
                    ~value:current_time
                )
                all_cluster_hosts
          | Some nodel ->
              (* nodel contains the current members of the cluster, according to corosync *)
              let quorum_hosts =
                List.filter_map
                  (fun {addr; _} ->
                    let ipstr = ipstr_of_address addr in
                    match List.assoc_opt ipstr ip_ch with
                    | None ->
                        error
                          "%s: cannot find cluster host with network address \
                           %s, ignoring this host"
                          __FUNCTION__ ipstr ;
                        None
                    | Some ch ->
                        Some ch
                  )
                  nodel
              in

              (* hosts_left contains the hosts that were live, but not in the list
                 of live hosts according to the cluster stack *)
              let hosts_left =
                List.filter (fun h -> not (List.mem h quorum_hosts)) live_hosts
              in
              (* hosts_joined contains the hosts that were dead but exists in the db,
                 and is now viewed as a member of the cluster by the cluster stack *)
              let hosts_joined =
                List.filter (fun h -> List.mem h quorum_hosts) dead_hosts
              in
              debug "%s: there are %d hosts joined and %d hosts left"
                __FUNCTION__ (List.length hosts_joined) (List.length hosts_left) ;

              List.iter
                (fun self ->
                  Db.Cluster_host.set_live ~__context ~self ~value:true ;
                  Db.Cluster_host.set_last_update_live ~__context ~self
                    ~value:current_time
                )
                quorum_hosts ;
              List.filter
                (fun h -> not (List.mem h quorum_hosts))
                all_cluster_hosts
              |> List.iter (fun self ->
                     Db.Cluster_host.set_live ~__context ~self ~value:false ;
                     Db.Cluster_host.set_last_update_live ~__context ~self
                       ~value:current_time
                 ) ;
              maybe_generate_alert ~__context ~hosts_left ~hosts_joined
                ~num_hosts:(List.length quorum_hosts) ~quorum:diag.quorum
        ) ;
        Db.Cluster.set_quorum ~__context ~self:cluster
          ~value:(Int64.of_int diag.quorum) ;
        Db.Cluster.set_live_hosts ~__context ~self:cluster
          ~value:(Int64.of_int diag.total_votes)
    | Error (InternalError message) | Error (Unix_error message) ->
        warn "%s Cannot query diagnostics due to %s, not performing update"
          __FUNCTION__ message
    | exception exn ->
        warn
          "%s: Got exception %s while retrieving diagnostics info, not \
           performing update"
          __FUNCTION__ (Printexc.to_string exn)

  let cluster_change_watcher : bool Atomic.t = Atomic.make false

  (* this is the time it takes for the update request to time out. It is ok to set
     it to a relatively long value since the call will return immediately if there
     is an update *)
  let cluster_change_interval = Mtime.Span.min

  let cluster_stack_watcher : bool Atomic.t = Atomic.make false

  (* we handle unclean hosts join and leave in the watcher, i.e. hosts joining and leaving
     due to network problems, power cut, etc. Join and leave initiated by the
     API will be handled in the API call themselves, but they share the same code
     as the watcher. *)
  let watch_cluster_change ~__context ~host =
    while !Daemon.enabled do
      let m =
        Cluster_client.LocalClient.UPDATES.get (rpc ~__context)
          "call cluster watcher"
          (Clock.Timer.span_to_s cluster_change_interval)
      in
      match Idl.IdM.run @@ Cluster_client.IDL.T.get m with
      | Ok updates -> (
        match find_cluster_host ~__context ~host with
        | Some ch ->
            let cluster = Db.Cluster_host.get_cluster ~__context ~self:ch in
            on_corosync_update ~__context ~cluster updates
        | None ->
            ()
      )
      | Error (InternalError "UPDATES.Timeout") ->
          (* UPDATES.get timed out, this is normal, now retry *)
          ()
      | Error (InternalError message) | Error (Unix_error message) ->
          warn "%s: Cannot query cluster host updates with error %s"
            __FUNCTION__ message
      | exception exn ->
          warn "%s: Got exception %s while query cluster host updates, retrying"
            __FUNCTION__ (Printexc.to_string exn) ;
          Thread.delay (Clock.Timer.span_to_s cluster_change_interval)
    done ;
    Atomic.set cluster_change_watcher false

  let watch_cluster_stack_version ~__context ~host =
    if !Daemon.enabled then
      match find_cluster_host ~__context ~host with
      | Some ch ->
          let cluster_ref = Db.Cluster_host.get_cluster ~__context ~self:ch in
          let cluster_rec =
            Db.Cluster.get_record ~__context ~self:cluster_ref
          in
          if
            Cluster_stack.of_version
              ( cluster_rec.API.cluster_cluster_stack
              , cluster_rec.API.cluster_cluster_stack_version
              )
            = Cluster_stack.Corosync2
          then (
            debug "%s: Detected Corosync 2 running as cluster stack"
              __FUNCTION__ ;
            let body =
              "The current cluster stack version of Corosync 2 is out of date, \
               consider updating to Corosync 3"
            in
            let name, priority = Api_messages.cluster_stack_out_of_date in
            let host_uuid = Db.Host.get_uuid ~__context ~self:host in

            Helpers.call_api_functions ~__context (fun rpc session_id ->
                let _ : [> `message] Ref.t =
                  Client.Client.Message.create ~rpc ~session_id ~name ~priority
                    ~cls:`Host ~obj_uuid:host_uuid ~body
                in
                ()
            )
          )
      | None ->
          debug "%s: No cluster host, no need to watch" __FUNCTION__

  (** [create_as_necessary] will create cluster watchers on the coordinator if they are not
      already created. 
      There is no need to destroy them: once the clustering daemon is disabled, 
      these threads will exit as well. *)
  let create_as_necessary ~__context ~host =
    if Helpers.is_pool_master ~__context ~host then (
      if Xapi_cluster_helpers.cluster_health_enabled ~__context then
        if Atomic.compare_and_set cluster_change_watcher false true then (
          debug "%s: create watcher for corosync-notifyd on coordinator"
            __FUNCTION__ ;
          let _ : Thread.t =
            Thread.create (fun () -> watch_cluster_change ~__context ~host) ()
          in
          ()
        ) else
          (* someone else must have gone into the if branch above and created the thread
             before us, leave it to them *)
          debug
            "%s: not create watcher for corosync-notifyd as it already exists"
            __FUNCTION__ ;

      if Xapi_cluster_helpers.corosync3_enabled ~__context then
        if Atomic.compare_and_set cluster_stack_watcher false true then (
          debug
            "%s: create cluster stack watcher for out-of-date cluster stack \
             (corosync2)"
            __FUNCTION__ ;
          let _ : Thread.t =
            Thread.create
              (fun () -> watch_cluster_stack_version ~__context ~host)
              ()
          in
          ()
        ) else
          debug "%s: not create watcher for cluster stack as it already exists"
            __FUNCTION__
    )
end
