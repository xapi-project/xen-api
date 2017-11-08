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

module D = Debug.Make(struct let name="xapi" end)
open D

open Stdext
open Db_filter
open Db_filter_types
open Record_util (* for host_operation_to_string *)
open Threadext

let all_operations = [ `provision; `evacuate; `reboot; `shutdown;
                       `vm_start; `vm_resume; `vm_migrate; `power_on ]

(** Returns a table of operations -> API error options (None if the operation would be ok) *)
let valid_operations ~__context record _ref' = 
  let _ref = Ref.string_of _ref' in
  let current_ops = List.map snd record.Db_actions.host_current_operations in

  let table = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.replace table x None) all_operations;
  let set_errors (code: string) (params: string list) (ops: API.host_allowed_operations_set) =
    List.iter (fun op ->
        if Hashtbl.find table op = None
        then Hashtbl.replace table op (Some(code, params))) ops in

  (* Operations are divided into two groups:
     	 1. those that create new VMs: `provision, `vm_resume, `vm_migrate
     	 2. those that remove VMs: `evacuate, `reboot, `shutdown *)
  let is_creating_new x = List.mem x [ `provision; `vm_resume; `vm_migrate ] in
  let is_removing x = List.mem x [ `evacuate; `reboot; `shutdown ] in
  let creating_new = List.fold_left (fun acc op -> acc || (is_creating_new op)) false current_ops in
  let removing = List.fold_left (fun acc op -> acc || (is_removing op)) false current_ops in
  List.iter
    (fun op ->
       if is_creating_new op && removing || (is_removing op && creating_new)
       then set_errors Api_errors.other_operation_in_progress
           [ "host"; _ref; host_operation_to_string (List.hd current_ops) ]
           [ op ]
    ) (List.filter (fun x -> x <> `power_on) all_operations);

  (* reboot and shutdown cannot run concurrently *)
  if List.mem `reboot current_ops
  then set_errors Api_errors.other_operation_in_progress
      [ "host"; _ref; host_operation_to_string `reboot ] [ `shutdown ];
  if List.mem `shutdown current_ops
  then set_errors Api_errors.other_operation_in_progress
      [ "host"; _ref; host_operation_to_string `shutdown ] [ `reboot ];

  (* Prevent more than one provision happening at a time to prevent extreme dom0
     load (in the case of the debian template). Once the template becomes a 'real'
     template we can relax this. *)
  if List.mem `provision current_ops
  then set_errors Api_errors.other_operation_in_progress
      [ "host"; _ref; host_operation_to_string `provision ]
      [ `provision ];

  (* The host must be disabled before reboots or shutdowns are permitted *)
  if record.Db_actions.host_enabled
  then set_errors Api_errors.host_not_disabled [] [ `reboot; `shutdown ];

  (* The host must be (thought to be down) before power_on is possible *)
  begin 
    try 
      if Db.Host_metrics.get_live ~__context ~self:record.Db_actions.host_metrics 
      then set_errors Api_errors.host_is_live [ _ref ] [ `power_on ] 
    with _ -> () 
  end;
  (* The host power_on_mode must be not disabled *)
  begin 
    try 
      if record.Db_actions.host_power_on_mode = ""
      then set_errors Api_errors.host_power_on_mode_disabled [] [ `power_on ]
    with _ -> () 
  end;
  (* The power-on-host plugin must be available before power_on is possible *)
  begin 
    try Unix.access (Filename.concat !Xapi_globs.xapi_plugins_root Constants.power_on_plugin) [ Unix.X_OK ]
    with _ -> set_errors Api_errors.xenapi_missing_plugin [ Constants.power_on_plugin ] [ `power_on ]
  end;

  (* Check where there are any attached clustered SRs. If so:
   * - Only one host may be down at a time;
   * - No hosts may go down if the SR is "recovering".
  *)
  let plugged_srs = Helpers.get_all_plugged_srs ~__context in
  let plugged_clustered_srs = List.filter (fun self -> Db.SR.get_clustered ~__context ~self) plugged_srs in
  if plugged_clustered_srs <> [] then begin
    let hosts_down = Db.Host_metrics.get_refs_where ~__context ~expr:(Eq (Field "live", Literal "false")) in
    if hosts_down <> [] then
      set_errors Api_errors.clustered_sr_degraded [ List.hd plugged_clustered_srs |> Ref.string_of ] [ `shutdown; `reboot ];

    let recovering_tasks =
      List.map (fun sr -> Helpers.find_health_check_task ~__context ~sr) plugged_clustered_srs
      |> List.concat
    in
    if recovering_tasks <> [] then
      set_errors Api_errors.clustered_sr_degraded
        [ Db.Task.get_name_description ~__context ~self:(List.hd recovering_tasks) ] [ `shutdown; `reboot ];
  end;

  (* All other operations may be parallelised *)
  table

let throw_error table op = 
  if not(Hashtbl.mem table op)
  then raise (Api_errors.Server_error(Api_errors.internal_error, [ Printf.sprintf "xapi_host_helpers.assert_operation_valid unknown operation: %s" (host_operation_to_string op) ]));

  match Hashtbl.find table op with
  | Some (code, params) -> raise (Api_errors.Server_error(code, params))
  | None -> ()

let assert_operation_valid ~__context ~self ~(op:API.host_allowed_operations) = 
  let all = Db.Host.get_record_internal ~__context ~self in
  let table = valid_operations ~__context all self in
  throw_error table op

let update_allowed_operations ~__context ~self : unit =
  let all = Db.Host.get_record_internal ~__context ~self in
  let valid = valid_operations ~__context all self in
  let keys = Hashtbl.fold (fun k v acc -> if v = None then k :: acc else acc) valid [] in
  (* CA-18377: If there's a rolling upgrade in progress, only send Miami keys across the wire. *)
  let keys = if Helpers.rolling_upgrade_in_progress ~__context
    then Listext.List.intersect keys Xapi_globs.host_operations_miami
    else keys in
  Db.Host.set_allowed_operations ~__context ~self ~value:keys

let update_allowed_operations_all_hosts ~__context : unit =
  let hosts = Db.Host.get_all ~__context in
  List.iter (fun self -> update_allowed_operations ~__context ~self) hosts

let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.Host.get_current_operations ~__context ~self in
  let set = (fun value -> Db.Host.set_current_operations ~__context ~self ~value) in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set

let disable  ~__context ~host = ()

let enable  ~__context ~host = ()

let shutdown  ~__context ~host = ()

let reboot  ~__context ~host = ()

(* When the Host.shutdown and Host.reboot calls return to the master, the slave is 
   shutting down asycnronously. We immediately set the Host_metrics.live to false 
   and add the host to the global list of known-dying hosts. *)
let mark_host_as_dead ~__context ~host ~reason =
  let done_already = Mutex.execute Xapi_globs.hosts_which_are_shutting_down_m
    (fun () ->
      if List.mem host !Xapi_globs.hosts_which_are_shutting_down then
        true
      else (
        Xapi_globs.hosts_which_are_shutting_down := host :: !Xapi_globs.hosts_which_are_shutting_down;
        false
      )
    ) in
  if not done_already then (
    (* The heartbeat handling code (HA and non-HA) will hopefully ignore the heartbeats
       and leave the host as dead from now until it comes back with a Pool.hello *)
    Xapi_hooks.host_pre_declare_dead ~__context ~host ~reason;
    begin
      try
        let metrics = Db.Host.get_metrics ~__context ~self:host in
        Db.Host_metrics.set_live ~__context ~self:metrics ~value:false;
        update_allowed_operations ~__context ~self:host
      with e ->
        info "Caught and ignoring exception setting host %s to dead: %s" (Ref.string_of host) (ExnHelper.string_of_exn e)
    end;
    Xapi_hooks.host_post_declare_dead ~__context ~host ~reason
  )

(* Toggled by an explicit Host.disable call to prevent a master restart making us bounce back *)
let user_requested_host_disable = ref false

(* Track whether the host considers itself started - we want to block host.enable API calls until this is the case. *)
let startup_complete = ref false
let startup_complete_m = Mutex.create ()

let signal_startup_complete () =
  Mutex.execute startup_complete_m (fun () -> startup_complete := true)

let assert_startup_complete () =
  Mutex.execute startup_complete_m
    (fun () -> if not (!startup_complete) then
        raise (Api_errors.Server_error (Api_errors.host_still_booting, [])))

let consider_enabling_host_nolock ~__context =
  debug "Xapi_host_helpers.consider_enabling_host_nolock called";
  (* If HA is enabled only consider marking the host as enabled if all the storage plugs in successfully.
        Disabled hosts are excluded from the HA planning calculations. Otherwise a host may boot,
        fail to plug in a PBD and cause all protected VMs to suddenly become non-agile. *)
  let ha_enabled = try bool_of_string (Localdb.get Constants.ha_armed) with _ -> false in
  let localhost = Helpers.get_localhost ~__context in
  let pbds = Db.Host.get_PBDs ~__context ~self:localhost in
  Storage_access.resynchronise_pbds ~__context ~pbds;
  let all_pbds_ok = List.fold_left (&&) true (List.map (fun self -> Db.PBD.get_currently_attached ~__context ~self) pbds) in

  if not !user_requested_host_disable && (not ha_enabled || all_pbds_ok) then begin
    (* If we were in the middle of a shutdown or reboot with HA enabled but somehow we failed
       		   and xapi restarted, make sure we don't automatically re-enable ourselves. This is to avoid
       		   letting a machine with no fencing touch any VMs. Once the host reboots we can safely clear
       		   the flag 'host_disabled_until_reboot' *)
    let pool = Helpers.get_pool ~__context in
    if !Xapi_globs.on_system_boot then begin
      debug "Host.enabled: system has just restarted: setting localhost to enabled";
      Db.Host.set_enabled ~__context ~self:localhost ~value:true;
      Localdb.put Constants.host_disabled_until_reboot "false";
      (* Start processing pending VM powercycle events *)
      Local_work_queue.start_vm_lifecycle_queue ();
    end else begin
      if try bool_of_string (Localdb.get Constants.host_disabled_until_reboot) with _ -> false then begin
        debug "Host.enabled: system not just rebooted but host_disabled_until_reboot still set. Leaving host disabled";
      end else begin
        debug "Host.enabled: system not just rebooted && host_disabled_until_reboot not set: setting localhost to enabled";
        Db.Host.set_enabled ~__context ~self:localhost ~value:true;
        (* Start processing pending VM powercycle events *)
        Local_work_queue.start_vm_lifecycle_queue ();
      end
    end;
    (* If Host has been enabled and HA is also enabled then tell the master to recompute its plan *)
    if Db.Host.get_enabled ~__context ~self:localhost && (Db.Pool.get_ha_enabled ~__context ~self:pool)
    then Helpers.call_api_functions ~__context (fun rpc session_id -> Client.Client.Pool.ha_schedule_plan_recomputation rpc session_id)
  end;
  signal_startup_complete ()

(** Attempt to minimise the number of times we call consider_enabling_host_nolock *)
let consider_enabling_host =
  At_least_once_more.make "consider_enabling_host"
    (fun () ->
       Server_helpers.exec_with_new_task "consider_enabling_host"
         (fun __context -> consider_enabling_host_nolock __context)
    )

let consider_enabling_host_request ~__context = At_least_once_more.again consider_enabling_host

let consider_enabling_host ~__context =
  debug "Xapi_host_helpers.consider_enabling_host called";
  consider_enabling_host_request ~__context

module Host_requires_reboot = struct
  let m = Mutex.create ()

  let get () =
    Mutex.execute m (fun () ->
        try Unix.access Xapi_globs.requires_reboot_file [Unix.F_OK]; true with _ -> false
      )

  let set () =
    Mutex.execute m (fun () ->
        Unixext.touch_file Xapi_globs.requires_reboot_file
      )
end

module InitiatorName = struct

  let make_initiatorname_config iqn hostname =
    Printf.sprintf
      "InitiatorName=%s\nInitiatorAlias=%s\n"
      iqn hostname

  let set_initiator_name iqn =
    let hostname = Unix.gethostname () in
    let config_file = make_initiatorname_config iqn hostname in
    Unixext.write_string_to_file !Xapi_globs.iscsi_initiator_config_file config_file

  let watch_other_configs ~__context delay =
    let loop token =
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          let events =
            Client.Client.Event.from rpc session_id ["host"] token delay |>
            Event_types.event_from_of_rpc
          in
          List.iter (fun ev ->
              match Event_helper.record_of_event ev with
                | Event_helper.Host (host_ref, Some host_rec) -> begin
                  let oc = host_rec.API.host_other_config in
                  let iscsi_iqn = try Some (List.assoc "iscsi_iqn" oc) with _ -> None in
                  match iscsi_iqn with
                  | None -> ()
                  | Some "" -> ()
                  | Some iqn when iqn <> host_rec.API.host_iscsi_iqn ->
                      Client.Client.Host.set_iscsi_iqn rpc session_id host_ref (List.assoc "iscsi_iqn" oc)
                  | _ -> ()
                  end
                | _ -> ())
              events.Event_types.events;
          events.Event_types.token)
    in
    loop

  let start_watcher_thread ~__context =
    Thread.create (fun () ->
      let loop = watch_other_configs ~__context 30.0 in
      while true do
        begin
          try
            let rec inner token = inner (loop token) in inner ""
          with e ->
            error "Caught exception in InitiatorName.start_watcher_thread: %s" (Printexc.to_string e);
            Thread.delay 5.0;
        end;
      done) () |> ignore
end