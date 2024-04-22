(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** Common code between the fake and real servers for dealing with Hosts.
 * @group Host Management
*)

module D = Debug.Make (struct let name = "xapi_host_helpers" end)

open D
module Unixext = Xapi_stdext_unix.Unixext
open Db_filter_types
open Record_util (* for host_operation_to_string *)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let all_operations = API.host_allowed_operations__all

(** Returns a table of operations -> API error options (None if the operation would be ok) *)
let valid_operations ~__context record _ref' =
  let _ref = Ref.string_of _ref' in
  let current_ops = List.map snd record.Db_actions.host_current_operations in
  let table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x None) all_operations ;
  let set_errors (code : string) (params : string list)
      (ops : API.host_allowed_operations_set) =
    List.iter
      (fun op ->
        if Hashtbl.find table op = None then
          Hashtbl.replace table op (Some (code, params))
      )
      ops
  in
  (* Operations are divided into two groups:
     	 1. those that create new VMs: `provision, `vm_resume, `vm_migrate
     	 2. those that remove VMs: `evacuate, `reboot, `shutdown *)
  let is_creating_new x = List.mem x [`provision; `vm_resume; `vm_migrate] in
  let is_removing x = List.mem x [`evacuate; `reboot; `shutdown] in
  let creating_new =
    List.fold_left (fun acc op -> acc || is_creating_new op) false current_ops
  in
  let removing =
    List.fold_left (fun acc op -> acc || is_removing op) false current_ops
  in
  List.iter
    (fun op ->
      if (is_creating_new op && removing) || (is_removing op && creating_new)
      then
        set_errors Api_errors.other_operation_in_progress
          ["host"; _ref; host_operation_to_string (List.hd current_ops)]
          [op]
    )
    (List.filter (fun x -> x <> `power_on) all_operations) ;
  (* reboot, shutdown and apply_updates cannot run concurrently *)
  if List.mem `reboot current_ops then
    set_errors Api_errors.other_operation_in_progress
      ["host"; _ref; host_operation_to_string `reboot]
      [`shutdown; `apply_updates] ;
  if List.mem `shutdown current_ops then
    set_errors Api_errors.other_operation_in_progress
      ["host"; _ref; host_operation_to_string `shutdown]
      [`reboot; `apply_updates] ;
  if List.mem `apply_updates current_ops then
    set_errors Api_errors.other_operation_in_progress
      ["host"; _ref; host_operation_to_string `apply_updates]
      [`reboot; `shutdown; `enable] ;
  (* Prevent more than one provision happening at a time to prevent extreme dom0
     load (in the case of the debian template). Once the template becomes a 'real'
     template we can relax this. *)
  if List.mem `provision current_ops then
    set_errors Api_errors.other_operation_in_progress
      ["host"; _ref; host_operation_to_string `provision]
      [`provision] ;
  (* The host must be disabled before reboots or shutdowns are permitted *)
  if record.Db_actions.host_enabled then
    set_errors Api_errors.host_not_disabled []
      [`reboot; `shutdown; `apply_updates] ;
  (* The host must be (thought to be down) before power_on is possible *)
  ( try
      if
        Db.Host_metrics.get_live ~__context ~self:record.Db_actions.host_metrics
      then
        set_errors Api_errors.host_is_live [_ref] [`power_on]
    with _ -> ()
  ) ;
  (* The host power_on_mode must be not disabled *)
  ( try
      if record.Db_actions.host_power_on_mode = "" then
        set_errors Api_errors.host_power_on_mode_disabled [] [`power_on]
    with _ -> ()
  ) ;
  (* The power-on-host plugin must be available before power_on is possible *)
  ( try
      Unix.access
        (Filename.concat !Xapi_globs.xapi_plugins_root Constants.power_on_plugin)
        [Unix.X_OK]
    with _ ->
      set_errors Api_errors.xenapi_missing_plugin
        [Constants.power_on_plugin]
        [`power_on]
  ) ;
  (* Check where there are any attached clustered SRs. If so:
   * - Only one enabled host may be down at a time;
   * - No hosts may go down if the SR is "recovering".
   *)
  let plugged_srs = Helpers.get_all_plugged_srs ~__context in
  let plugged_clustered_srs =
    List.filter (fun self -> Db.SR.get_clustered ~__context ~self) plugged_srs
  in
  if plugged_clustered_srs <> [] then (
    let hosts_down =
      Db.Host_metrics.get_refs_where ~__context
        ~expr:(Eq (Field "live", Literal "false"))
    in
    if
      not
        (List.for_all
           (Xapi_clustering.is_clustering_disabled_on_host ~__context)
           hosts_down
        )
    then
      set_errors Api_errors.clustered_sr_degraded
        [List.hd plugged_clustered_srs |> Ref.string_of]
        [`shutdown; `reboot; `apply_updates] ;
    let recovering_tasks =
      List.map
        (fun sr -> Helpers.find_health_check_task ~__context ~sr)
        plugged_clustered_srs
      |> List.concat
    in
    if recovering_tasks <> [] then
      set_errors Api_errors.clustered_sr_degraded
        [
          Db.Task.get_name_description ~__context
            ~self:(List.hd recovering_tasks)
        ]
        [`shutdown; `reboot; `apply_updates]
  ) ;
  (* All other operations may be parallelised *)
  table

let throw_error table op =
  if not (Hashtbl.mem table op) then
    raise
      (Api_errors.Server_error
         ( Api_errors.internal_error
         , [
             Printf.sprintf
               "xapi_host_helpers.assert_operation_valid unknown operation: %s"
               (host_operation_to_string op)
           ]
         )
      ) ;
  match Hashtbl.find table op with
  | Some (code, params) ->
      raise (Api_errors.Server_error (code, params))
  | None ->
      ()

let assert_operation_valid ~__context ~self ~(op : API.host_allowed_operations)
    =
  let all = Db.Host.get_record_internal ~__context ~self in
  let table = valid_operations ~__context all self in
  throw_error table op

let update_allowed_operations ~__context ~self : unit =
  let all = Db.Host.get_record_internal ~__context ~self in
  let valid = valid_operations ~__context all self in
  let keys =
    Hashtbl.fold (fun k v acc -> if v = None then k :: acc else acc) valid []
  in
  (* CA-18377: If there's a rolling upgrade in progress, only send Miami keys across the wire. *)
  let keys =
    if Helpers.rolling_upgrade_in_progress ~__context then
      Xapi_stdext_std.Listext.List.intersect keys
        Xapi_globs.host_operations_miami
    else
      keys
  in
  Db.Host.set_allowed_operations ~__context ~self ~value:keys

let update_allowed_operations_all_hosts ~__context : unit =
  let hosts = Db.Host.get_all ~__context in
  List.iter (fun self -> update_allowed_operations ~__context ~self) hosts

(** Add to the Host's current operations, call a function and then remove from the
  current operations. Ensure the allowed_operations are kept up to date. *)
let with_host_operation ~__context ~self ~doc ~op f =
  let task_id = Ref.string_of (Context.get_task_id __context) in
  (* CA-18377: If there's a rolling upgrade in progress, only send Miami keys across the wire. *)
  let operation_allowed ~op =
    false
    || (not (Helpers.rolling_upgrade_in_progress ~__context))
    || List.mem op Xapi_globs.host_operations_miami
  in
  Helpers.retry_with_global_lock ~__context ~doc (fun () ->
      assert_operation_valid ~__context ~self ~op ;
      if operation_allowed ~op then
        Db.Host.add_to_current_operations ~__context ~self ~key:task_id
          ~value:op ;
      update_allowed_operations ~__context ~self
  ) ;
  (* Then do the action with the lock released *)
  finally f (* Make sure to clean up at the end *) (fun () ->
      try
        if operation_allowed ~op then (
          Db.Host.remove_from_current_operations ~__context ~self ~key:task_id ;
          Helpers.Early_wakeup.broadcast
            (Datamodel_common._host, Ref.string_of self)
        ) ;
        let clustered_srs =
          Db.SR.get_refs_where ~__context
            ~expr:(Eq (Field "clustered", Literal "true"))
        in
        if clustered_srs <> [] then
          (* Host powerstate operations on one host may affect all other hosts if
           * a clustered SR is in use, so update all hosts' allowed operations. *)
          update_allowed_operations_all_hosts ~__context
        else
          update_allowed_operations ~__context ~self
      with _ -> ()
  )

let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.Host.get_current_operations ~__context ~self in
  let set value = Db.Host.set_current_operations ~__context ~self ~value in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set

(* When the Host.shutdown and Host.reboot calls return to the master, the slave is
   shutting down asycnronously. We immediately set the Host_metrics.live to false
   and add the host to the global list of known-dying hosts. *)
let mark_host_as_dead ~__context ~host ~reason =
  let done_already =
    with_lock Xapi_globs.hosts_which_are_shutting_down_m (fun () ->
        if List.mem host !Xapi_globs.hosts_which_are_shutting_down then
          true
        else (
          Xapi_globs.hosts_which_are_shutting_down :=
            host :: !Xapi_globs.hosts_which_are_shutting_down ;
          false
        )
    )
  in
  if not done_already then (
    (* The heartbeat handling code (HA and non-HA) will hopefully ignore the heartbeats
       and leave the host as dead from now until it comes back with a Pool.hello *)
    Xapi_hooks.host_pre_declare_dead ~__context ~host ~reason ;
    ( try
        let metrics = Db.Host.get_metrics ~__context ~self:host in
        Db.Host_metrics.set_live ~__context ~self:metrics ~value:false ;
        update_allowed_operations ~__context ~self:host
      with e ->
        info "Caught and ignoring exception setting host %s to dead: %s"
          (Ref.string_of host)
          (ExnHelper.string_of_exn e)
    ) ;
    Xapi_hooks.host_post_declare_dead ~__context ~host ~reason
  )

let assert_host_disabled ~__context ~host =
  if Db.Host.get_enabled ~__context ~self:host then
    raise (Api_errors.Server_error (Api_errors.host_not_disabled, []))

(* Toggled by an explicit Host.disable call to prevent a master restart making us bounce back *)
let user_requested_host_disable = ref false

(* Track whether the host considers itself started - we want to block host.enable API calls until this is the case. *)
let startup_complete = ref false

let startup_complete_m = Mutex.create ()

let signal_startup_complete () =
  with_lock startup_complete_m (fun () -> startup_complete := true)

let assert_startup_complete () =
  with_lock startup_complete_m (fun () ->
      if not !startup_complete then
        raise (Api_errors.Server_error (Api_errors.host_still_booting, []))
  )

(* Check whether the currently installed Toolstack is compatible with the
   currently installed xenctrl library and the currently running Xen hypervisor.
*)
let is_xen_compatible () =
  try
    let _ =
      Forkhelpers.execute_command_get_output !Xapi_globs.list_domains []
    in
    info
      "The Toolstack is compatible with the current Xen and libxenctrl versions" ;
    true
  with
  | Forkhelpers.Spawn_internal_error (_, _, Unix.WEXITED 2) ->
      (* list_domains failed with an exception: assuming that the hypercall
         returned -EACCES, indicating a compatibility problem. *)
      warn "The current Xen version is incompatible with the Toolstack" ;
      false
  | Forkhelpers.Spawn_internal_error (_, _, Unix.WEXITED 127) ->
      (* list_domains could not execute due to a missing library: assuming that
         that a compatible version of libxenctrl is not present. *)
      warn "The current libxenctrl version is incompatible with the Toolstack" ;
      false
  | _ ->
      error
        "Unexpected error when calling list_domains; assuming the Toolstack is \
         incompatible with Xen and/or libxenctrl" ;
      false

let xen_compatible = ref None

let assert_xen_compatible () =
  let compatible =
    match !xen_compatible with
    | None ->
        let x = is_xen_compatible () in
        xen_compatible := Some x ;
        x
    | Some x ->
        x
  in
  if not compatible then
    raise Api_errors.(Server_error (xen_incompatible, []))

let remove_pending_guidance ~__context ~self ~value =
  let h = Db.Host.get_name_label ~__context ~self in
  if
    List.exists
      (fun g -> g = value)
      (Db.Host.get_pending_guidances ~__context ~self)
  then (
    debug "Remove guidance [%s] from host [%s]'s pending_guidances list"
      Updateinfo.Guidance.(of_pending_guidance value |> to_string)
      h ;
    Db.Host.remove_pending_guidances ~__context ~self ~value
  ) ;

  if
    List.exists
      (fun g -> g = value)
      (Db.Host.get_pending_guidances_recommended ~__context ~self)
  then (
    debug
      "Remove guidance [%s] from host [%s]'s pending_guidances_recommended list"
      Updateinfo.Guidance.(of_pending_guidance value |> to_string)
      h ;
    Db.Host.remove_pending_guidances_recommended ~__context ~self ~value
  ) ;

  if
    List.exists
      (fun g -> g = value)
      (Db.Host.get_pending_guidances_full ~__context ~self)
  then (
    debug "Remove guidance [%s] from host [%s]'s pending_guidances_full list"
      Updateinfo.Guidance.(of_pending_guidance value |> to_string)
      h ;
    Db.Host.remove_pending_guidances_full ~__context ~self ~value
  )

let consider_enabling_host_nolock ~__context =
  debug "Xapi_host_helpers.consider_enabling_host_nolock called" ;
  (* If HA is enabled only consider marking the host as enabled if all the storage plugs in successfully.
        Disabled hosts are excluded from the HA planning calculations. Otherwise a host may boot,
        fail to plug in a PBD and cause all protected VMs to suddenly become non-agile. *)
  let ha_enabled =
    try bool_of_string (Localdb.get Constants.ha_armed) with _ -> false
  in
  let localhost = Helpers.get_localhost ~__context in
  let pbds = Db.Host.get_PBDs ~__context ~self:localhost in
  Storage_access.resynchronise_pbds ~__context ~pbds ;
  let all_pbds_ok =
    List.fold_left ( && ) true
      (List.map
         (fun self -> Db.PBD.get_currently_attached ~__context ~self)
         pbds
      )
  in
  if
    (not !user_requested_host_disable)
    && ((not ha_enabled) || all_pbds_ok)
    && is_xen_compatible ()
  then (
    (* If we were in the middle of a shutdown or reboot with HA enabled but somehow we failed
       		   and xapi restarted, make sure we don't automatically re-enable ourselves. This is to avoid
       		   letting a machine with no fencing touch any VMs. Once the host reboots we can safely clear
       		   the flag 'host_disabled_until_reboot' *)
    let pool = Helpers.get_pool ~__context in
    let if_no_pending_guidances f =
      let host_pending_mandatory_guidances =
        Db.Host.get_pending_guidances ~__context ~self:localhost
      in
      if host_pending_mandatory_guidances <> [] then
        debug
          "Host.enabled: there are %d pending mandatory guidances on host \
           (%s): [%s]. Leave host disabled."
          (List.length host_pending_mandatory_guidances)
          (Ref.string_of localhost)
          (String.concat ";"
             (List.map Updateinfo.Guidance.to_string
                (List.map Updateinfo.Guidance.of_pending_guidance
                   host_pending_mandatory_guidances
                )
             )
          )
      else
        f ()
    in
    if !Xapi_globs.on_system_boot then (
      debug "Host.enabled: system has just restarted" ;
      if_no_pending_guidances (fun () ->
          debug
            "Host.enabled: system has just restarted and no pending mandatory \
             guidances: setting localhost to enabled" ;
          Db.Host.set_enabled ~__context ~self:localhost ~value:true ;
          update_allowed_operations ~__context ~self:localhost ;
          Localdb.put Constants.host_disabled_until_reboot "false" ;
          (* Start processing pending VM powercycle events *)
          Local_work_queue.start_vm_lifecycle_queue ()
      )
    ) else if
        try bool_of_string (Localdb.get Constants.host_disabled_until_reboot)
        with _ -> false
      then
      debug
        "Host.enabled: system not just rebooted but host_disabled_until_reboot \
         still set. Leaving host disabled"
    else (
      debug
        "Host.enabled: system not just rebooted && host_disabled_until_reboot \
         not set" ;
      if_no_pending_guidances (fun () ->
          debug
            "Host.enabled: system not just rebooted && \
             host_disabled_until_reboot not set and no pending mandatory \
             guidances: setting localhost to enabled" ;
          Db.Host.set_enabled ~__context ~self:localhost ~value:true ;
          update_allowed_operations ~__context ~self:localhost ;
          (* Start processing pending VM powercycle events *)
          Local_work_queue.start_vm_lifecycle_queue ()
      )
    ) ;
    (* If Host has been enabled and HA is also enabled then tell the master to recompute its plan *)
    if
      Db.Host.get_enabled ~__context ~self:localhost
      && Db.Pool.get_ha_enabled ~__context ~self:pool
    then
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.Pool.ha_schedule_plan_recomputation ~rpc ~session_id
      )
  ) ;
  signal_startup_complete ()

(** Attempt to minimise the number of times we call consider_enabling_host_nolock *)
let consider_enabling_host =
  At_least_once_more.make "consider_enabling_host" (fun () ->
      Server_helpers.exec_with_new_task "consider_enabling_host"
        (fun __context -> consider_enabling_host_nolock ~__context
      )
  )

let consider_enabling_host_request ~__context =
  At_least_once_more.again consider_enabling_host

let consider_enabling_host ~__context =
  debug "Xapi_host_helpers.consider_enabling_host called" ;
  consider_enabling_host_request ~__context

module Host_requires_reboot = struct
  let m = Mutex.create ()

  let get () =
    with_lock m (fun () ->
        try
          Unix.access Xapi_globs.requires_reboot_file [Unix.F_OK] ;
          true
        with _ -> false
    )

  let set () =
    with_lock m (fun () -> Unixext.touch_file Xapi_globs.requires_reboot_file)
end

module Configuration = struct
  let make_set_initiator_args iqn hostname =
    (* CA-18000: there is a 30 character limit to the initiator when talking to
       Dell MD3000i filers, so we limit the size of the initiator name in all cases *)
    let hostname_chopped =
      if String.length hostname > 30 then
        String.sub hostname 0 30
      else
        hostname
    in
    [iqn; hostname_chopped]

  let set_initiator_name iqn =
    let hostname = Unix.gethostname () in
    (* CA-377454 - robustness, create dir if necessary *)
    Unixext.mkdir_rec "/var/lock/sm/iscsiadm" 0o700 ;
    let args = make_set_initiator_args iqn hostname in
    ignore (Helpers.call_script !Xapi_globs.set_iSCSI_initiator_script args)

  let set_multipathing enabled =
    let flag = !Xapi_globs.multipathing_config_file in
    if enabled then
      Unixext.touch_file flag
    else
      Unixext.unlink_safe flag

  let sync_config_files ~__context =
    (* If the host fields are not in sync with the values in other_config,
       the other_config watcher thread will make sure that these functions will
       be called again with the up to date values. *)
    let self = Helpers.get_localhost ~__context in
    (* when HA is enabled we expect this to fail because there will be an active session on XAPI
     * startup, and similarly during a toolstack restart. *)
    log_and_ignore_exn (fun () ->
        set_initiator_name (Db.Host.get_iscsi_iqn ~__context ~self)
    ) ;
    set_multipathing (Db.Host.get_multipathing ~__context ~self)

  let watch_other_configs ~__context delay =
    let loop (token, was_in_rpu) =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          let events =
            Client.Client.Event.from ~rpc ~session_id ~classes:["host"; "pool"]
              ~token ~timeout:delay
            |> Event_types.event_from_of_rpc
          in
          let check_host (host_ref, host_rec) =
            let oc = host_rec.API.host_other_config in
            let iscsi_iqn =
              try Some (List.assoc "iscsi_iqn" oc) with _ -> None
            in
            ( match iscsi_iqn with
            | None ->
                ()
            | Some "" ->
                ()
            | Some iqn when iqn <> host_rec.API.host_iscsi_iqn ->
                Client.Client.Host.set_iscsi_iqn ~rpc ~session_id ~host:host_ref
                  ~value:iqn
            | _ ->
                ()
            ) ;
            (* Accepted values are "true" and "false" *)
            (* If someone deletes the multipathing other_config key, we don't do anything *)
            let multipathing =
              try Some (List.assoc "multipathing" oc |> Stdlib.bool_of_string)
              with _ -> None
            in
            match multipathing with
            | None ->
                ()
            | Some multipathing
              when multipathing <> host_rec.API.host_multipathing ->
                Client.Client.Host.set_multipathing ~rpc ~session_id
                  ~host:host_ref ~value:multipathing
            | _ ->
                ()
          in
          let event_recs =
            List.map Event_helper.record_of_event events.Event_types.events
          in
          let in_rpu =
            List.fold_left
              (fun in_rpu ev ->
                match ev with
                | Event_helper.Pool (_pool_ref, Some pool_rec) ->
                    let in_rpu =
                      Helpers.rolling_upgrade_in_progress_of_oc
                        pool_rec.API.pool_other_config
                    in
                    if (not in_rpu) && was_in_rpu then
                      List.iter check_host (Db.Host.get_all_records ~__context) ;
                    in_rpu
                | _ ->
                    in_rpu
              )
              was_in_rpu event_recs
          in
          List.iter
            (function
              | Event_helper.Host (host_ref, Some host_rec) ->
                  if not in_rpu then
                    check_host (host_ref, host_rec)
              | _ ->
                  ()
              )
            event_recs ;
          (events.Event_types.token, in_rpu)
      )
    in
    loop

  let start_watcher_thread ~__context =
    Thread.create
      (fun () ->
        let loop = watch_other_configs ~__context 30.0 in
        while true do
          try
            let rec inner token = inner (loop token) in
            inner ("", Helpers.rolling_upgrade_in_progress ~__context)
          with e ->
            error "Caught exception in Configuration.start_watcher_thread: %s"
              (Printexc.to_string e) ;
            Thread.delay 5.0
        done
      )
      ()
    |> ignore
end
