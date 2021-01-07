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

module Rrdd = Rrd_client.Client
module Date = Xapi_stdext_date.Date
module Pervasiveext = Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_threads.Threadext
module Unixext = Xapi_stdext_unix.Unixext
open Xapi_host_helpers
open Xapi_support
open Db_filter_types
open Create_misc
open Network
open Workload_balancing

module D = Debug.Make (struct let name = "xapi_host" end)

open D

let set_emergency_mode_error code params =
  Xapi_globs.emergency_mode_error := Api_errors.Server_error (code, params)

let local_assert_healthy ~__context =
  match Pool_role.get_role () with
  | Pool_role.Master ->
      ()
  | Pool_role.Broken ->
      raise !Xapi_globs.emergency_mode_error
  | Pool_role.Slave _ ->
      if !Xapi_globs.slave_emergency_mode then
        raise !Xapi_globs.emergency_mode_error

let set_power_on_mode ~__context ~self ~power_on_mode ~power_on_config =
  Db.Host.set_power_on_mode ~__context ~self ~value:power_on_mode ;
  let current_config = Db.Host.get_power_on_config ~__context ~self in
  Db.Host.set_power_on_config ~__context ~self ~value:power_on_config ;
  Xapi_secret.clean_out_passwds ~__context current_config ;
  Xapi_host_helpers.update_allowed_operations ~__context ~self

(** Before we re-enable this host we make sure it's safe to do so. It isn't if:
    	+ we're in the middle of an HA shutdown/reboot and have our fencing temporarily disabled.
    	+ xapi hasn't properly started up yet.
    	+ HA is enabled and this host has broken storage or networking which would cause protected VMs
    	to become non-agile
*)
let assert_safe_to_reenable ~__context ~self =
  assert_startup_complete () ;
  let host_disabled_until_reboot =
    try bool_of_string (Localdb.get Constants.host_disabled_until_reboot)
    with _ -> false
  in
  if host_disabled_until_reboot then
    raise (Api_errors.Server_error (Api_errors.host_disabled_until_reboot, [])) ;
  if Db.Pool.get_ha_enabled ~__context ~self:(Helpers.get_pool ~__context) then (
    let pbds = Db.Host.get_PBDs ~__context ~self in
    let unplugged_pbds =
      List.filter
        (fun pbd -> not (Db.PBD.get_currently_attached ~__context ~self:pbd))
        pbds
    in
    (* Make sure it is 'ok' to have these PBDs remain unplugged *)
    List.iter
      (fun self ->
        Xapi_pbd.abort_if_storage_attached_to_protected_vms ~__context ~self)
      unplugged_pbds ;
    let pifs = Db.Host.get_PIFs ~__context ~self in
    let unplugged_pifs =
      List.filter
        (fun pif -> not (Db.PIF.get_currently_attached ~__context ~self:pif))
        pifs
    in
    (* Make sure it is 'ok' to have these PIFs remain unplugged *)
    List.iter
      (fun self ->
        Xapi_pif.abort_if_network_attached_to_protected_vms ~__context ~self)
      unplugged_pifs
  )

(* The maximum pool size allowed must be restricted to 3 hosts for the pool which does not have Pool_size feature *)
let pool_size_is_restricted ~__context =
  let cvm_exception =
    let dom0 = Helpers.get_domain_zero ~__context in
    Db.VM.get_records_where ~__context
      ~expr:(Eq (Field "is_control_domain", Literal "true"))
    |> List.exists (fun (vmref, vmrec) ->
           vmref <> dom0
           && Xapi_stdext_std.Xstringext.String.endswith "-CVM"
                vmrec.API.vM_name_label)
  in
  (not cvm_exception)
  && not (Pool_features.is_enabled ~__context Features.Pool_size)

let xen_bugtool = "/usr/sbin/xen-bugtool"

let bugreport_upload ~__context ~host ~url ~options =
  let proxy =
    if List.mem_assoc "http_proxy" options then
      List.assoc "http_proxy" options
    else
      try Unix.getenv "http_proxy" with _ -> ""
  in
  let cmd =
    Printf.sprintf "%s %s %s"
      !Xapi_globs.host_bugreport_upload
      "(url filtered)" proxy
  in
  try
    let env = Helpers.env_with_path [("INPUT_URL", url); ("PROXY", proxy)] in
    let stdout, stderr =
      Forkhelpers.execute_command_get_output ~env
        !Xapi_globs.host_bugreport_upload
        []
    in
    debug "%s succeeded with stdout=[%s] stderr=[%s]" cmd stdout stderr
  with Forkhelpers.Spawn_internal_error (stderr, stdout, status) as e -> (
    debug "%s failed with stdout=[%s] stderr=[%s]" cmd stdout stderr ;
    (* Attempt to interpret curl's exit code (from curl(1)) *)
    match status with
    | Unix.WEXITED (1 | 3 | 4) ->
        failwith "URL not recognised"
    | Unix.WEXITED (5 | 6) ->
        failwith "Failed to resolve proxy or host"
    | Unix.WEXITED 7 ->
        failwith "Failed to connect to host"
    | Unix.WEXITED 9 ->
        failwith "FTP access denied"
    | _ ->
        raise e
  )

(** Check that a) there are no running VMs present on the host, b) there are no VBDs currently
    	attached to dom0, c) host is disabled.

    	This is approximately maintainance mode as defined by the gui. However, since
    	we haven't agreed on an exact definition of this mode, we'll not call this maintainance mode here, but we'll
    	use a synonym. According to http://thesaurus.com/browse/maintenance, bacon is a synonym
    	for maintainance, hence the name of the following function.
*)
let assert_bacon_mode ~__context ~host =
  if Db.Host.get_enabled ~__context ~self:host then
    raise (Api_errors.Server_error (Api_errors.host_not_disabled, [])) ;
  let selfref = Ref.string_of host in
  let vms =
    Db.VM.get_refs_where ~__context
      ~expr:
        (And
           ( Eq (Field "resident_on", Literal (Ref.string_of host))
           , Eq (Field "power_state", Literal "Running") ))
  in
  (* We always expect a control domain to be resident on a host *)
  ( match
      List.filter
        (fun vm -> not (Db.VM.get_is_control_domain ~__context ~self:vm))
        vms
    with
  | [] ->
      ()
  | guest_vms ->
      let vm_data = [selfref; "vm"; Ref.string_of (List.hd guest_vms)] in
      raise (Api_errors.Server_error (Api_errors.host_in_use, vm_data))
  ) ;
  debug "Bacon test: VMs OK - %d running VMs" (List.length vms) ;
  let control_domain_vbds =
    List.filter
      (fun vm ->
        Db.VM.get_resident_on ~__context ~self:vm = host
        && Db.VM.get_is_control_domain ~__context ~self:vm)
      (Db.VM.get_all ~__context)
    |> List.map (fun self -> Db.VM.get_VBDs ~__context ~self)
    |> List.flatten
    |> List.filter (fun self -> Db.VBD.get_currently_attached ~__context ~self)
  in
  if List.length control_domain_vbds > 0 then
    raise
      (Api_errors.Server_error
         ( Api_errors.host_in_use
         , [
             selfref; "vbd"; List.hd (List.map Ref.string_of control_domain_vbds)
           ] )) ;
  debug "Bacon test: VBDs OK"

let signal_networking_change = Xapi_mgmt_iface.on_dom0_networking_change

let signal_cdrom_event ~__context params =
  let find_vdi_name sr name =
    let ret = ref None in
    let vdis = Db.SR.get_VDIs ~__context ~self:sr in
    List.iter
      (fun vdi ->
        if Db.VDI.get_location ~__context ~self:vdi = name then ret := Some vdi)
      vdis ;
    !ret
  in
  let find_vdis name =
    let srs =
      List.filter
        (fun sr ->
          let ty = Db.SR.get_type ~__context ~self:sr in
          ty = "local" || ty = "udev")
        (Db.SR.get_all ~__context)
    in
    List.fold_left
      (fun acc o -> match o with Some x -> x :: acc | None -> acc)
      []
      (List.map (fun sr -> find_vdi_name sr name) srs)
  in
  let insert dev =
    let vdis = find_vdis dev in
    if List.length vdis = 1 then (
      let vdi = List.hd vdis in
      debug "cdrom inserted notification in vdi %s" (Ref.string_of vdi) ;
      let vbds = Db.VDI.get_VBDs ~__context ~self:vdi in
      List.iter
        (fun vbd -> Xapi_xenops.vbd_insert ~__context ~self:vbd ~vdi)
        vbds
    ) else
      ()
  in
  try
    match String.split_on_char ':' params with
    | ["inserted"; dev] ->
        insert dev
    | "ejected" :: _ ->
        ()
    | _ ->
        ()
  with _ -> ()

let notify ~__context ~ty ~params =
  match ty with "cdrom" -> signal_cdrom_event ~__context params | _ -> ()

let rotate = function [] -> [] | x :: xs -> xs @ [x]

(* A host evacuation plan consists of a hashtable mapping VM refs to instances of per_vm_plan: *)
type per_vm_plan = Migrate of API.ref_host | Error of (string * string list)

let string_of_per_vm_plan p =
  match p with
  | Migrate h ->
      Ref.string_of h
  | Error (e, t) ->
      String.concat "," (e :: t)

(** Return a table mapping VMs to 'per_vm_plan' types indicating either a target
    	Host or a reason why the VM cannot be migrated. *)
let compute_evacuation_plan_no_wlb ~__context ~host =
  let all_hosts = Db.Host.get_all ~__context in
  let enabled_hosts =
    List.filter (fun self -> Db.Host.get_enabled ~__context ~self) all_hosts
  in
  (* Only consider migrating to other enabled hosts (not this one obviously) *)
  let target_hosts = List.filter (fun self -> self <> host) enabled_hosts in
  (* PR-1007: During a rolling pool upgrade, we are only allowed to
     	   migrate VMs to hosts that have the same or higher version as
     	   the source host. So as long as host versions aren't decreasing,
     	   we're allowed to migrate VMs between hosts. *)
  debug "evacuating host version: %s"
    (Helpers.version_string_of ~__context (Helpers.LocalObject host)) ;
  let target_hosts =
    List.filter
      (fun target ->
        debug "host %s version: %s"
          (Db.Host.get_hostname ~__context ~self:target)
          (Helpers.version_string_of ~__context (Helpers.LocalObject target)) ;
        Helpers.host_versions_not_decreasing ~__context
          ~host_from:(Helpers.LocalObject host)
          ~host_to:(Helpers.LocalObject target))
      target_hosts
  in
  debug "evacuation target hosts are [%s]"
    (String.concat "; "
       (List.map
          (fun h -> Db.Host.get_hostname ~__context ~self:h)
          target_hosts)) ;
  let all_vms = Db.Host.get_resident_VMs ~__context ~self:host in
  let all_vms =
    List.map (fun self -> (self, Db.VM.get_record ~__context ~self)) all_vms
  in
  let all_user_vms =
    List.filter (fun (_, record) -> not record.API.vM_is_control_domain) all_vms
  in
  let plans = Hashtbl.create 10 in
  if target_hosts = [] then (
    List.iter
      (fun (vm, _) ->
        Hashtbl.replace plans vm
          (Error (Api_errors.no_hosts_available, [Ref.string_of vm])))
      all_user_vms ;
    plans
  ) else
    (* If HA is enabled we require that non-protected VMs are suspended. This gives us the property that
       			   the result obtained by executing the evacuation plan and disabling the host looks the same (from the HA
       			   planner's PoV) to the result obtained following a host failure and VM restart. *)
    let pool = Helpers.get_pool ~__context in
    let protected_vms, unprotected_vms =
      if Db.Pool.get_ha_enabled ~__context ~self:pool then
        List.partition
          (fun (vm, record) ->
            Helpers.vm_should_always_run record.API.vM_ha_always_run
              record.API.vM_ha_restart_priority)
          all_user_vms
      else
        (all_user_vms, [])
    in
    List.iter
      (fun (vm, _) ->
        Hashtbl.replace plans vm
          (Error (Api_errors.host_not_enough_free_memory, [Ref.string_of vm])))
      unprotected_vms ;
    let migratable_vms, unmigratable_vms =
      List.partition
        (fun (vm, record) ->
          try
            List.iter
              (fun host ->
                Xapi_vm_helpers.assert_can_boot_here ~__context ~self:vm ~host
                  ~snapshot:record ~do_memory_check:false ~do_cpuid_check:true
                  ())
              target_hosts ;
            true
          with Api_errors.Server_error (code, params) ->
            Hashtbl.replace plans vm (Error (code, params)) ;
            false)
        protected_vms
    in
    (* Check for impediments before attempting to perform pool_migrate *)
    List.iter
      (fun (vm, _) ->
        match
          Xapi_vm_lifecycle.get_operation_error ~__context ~self:vm
            ~op:`pool_migrate ~strict:true
        with
        | None ->
            ()
        | Some (a, b) ->
            Hashtbl.replace plans vm (Error (a, b)))
      all_user_vms ;
    (* Compute the binpack which takes only memory size into account. We will check afterwards for storage
       			   and network availability. *)
    let plan =
      Xapi_ha_vm_failover.compute_evacuation_plan ~__context
        (List.length all_hosts) target_hosts migratable_vms
    in
    (* Check if the plan was actually complete: if some VMs are missing it means there wasn't enough memory *)
    let vms_handled = List.map fst plan in
    let vms_missing =
      List.filter
        (fun x -> not (List.mem x vms_handled))
        (List.map fst migratable_vms)
    in
    List.iter
      (fun vm ->
        Hashtbl.replace plans vm
          (Error (Api_errors.host_not_enough_free_memory, [Ref.string_of vm])))
      vms_missing ;
    (* Now for each VM we did place, verify storage and network visibility. *)
    List.iter
      (fun (vm, host) ->
        let snapshot = List.assoc vm all_vms in
        ( try
            Xapi_vm_helpers.assert_can_boot_here ~__context ~self:vm ~host
              ~snapshot ~do_memory_check:false ~do_cpuid_check:true ()
          with Api_errors.Server_error (code, params) ->
            Hashtbl.replace plans vm (Error (code, params))
        ) ;
        if not (Hashtbl.mem plans vm) then
          Hashtbl.replace plans vm (Migrate host))
      plan ;
    plans

(* Old Miami style function with the strange error encoding *)
let assert_can_evacuate ~__context ~host =
  (* call no_wlb function as we only care about errors, and wlb only provides recs for moveable vms *)
  let plans = compute_evacuation_plan_no_wlb ~__context ~host in
  let errors =
    Hashtbl.fold
      (fun vm plan acc ->
        match plan with
        | Error (code, params) ->
            String.concat "," (code :: params) :: acc
        | _ ->
            acc)
      plans []
  in
  if errors <> [] then
    raise
      (Api_errors.Server_error
         (Api_errors.cannot_evacuate_host, [String.concat "|" errors]))

(* New Orlando style function which returns a Map *)
let get_vms_which_prevent_evacuation ~__context ~self =
  let plans = compute_evacuation_plan_no_wlb ~__context ~host:self in
  Hashtbl.fold
    (fun vm plan acc ->
      match plan with
      | Error (code, params) ->
          (vm, code :: params) :: acc
      | _ ->
          acc)
    plans []

let compute_evacuation_plan_wlb ~__context ~self =
  (* We treat xapi as primary when it comes to "hard" errors, i.e. those that aren't down to memory constraints.  These are things like
     	 VM_REQUIRES_SR or VM_LACKS_FEATURE_SUSPEND.

     	 We treat WLB as primary when it comes to placement of things that can actually move.  WLB will return a list of migrations to perform,
     	 and we pass those on.  WLB will only return a partial set of migrations -- if there's not enough memory available, or if the VM can't
     	 move, then it will simply omit that from the results.

     	 So the algorithm is:
     	   Record all the recommendations made by WLB.
     	   Record all the non-memory errors from compute_evacuation_plan_no_wlb.  These might overwrite recommendations by WLB, which is the
     	   right thing to do because WLB doesn't know about all the HA corner cases (for example), but xapi does.
     	   If there are any VMs left over, record them as HOST_NOT_ENOUGH_FREE_MEMORY, because we assume that WLB thinks they don't fit.
  *)
  let error_vms = compute_evacuation_plan_no_wlb ~__context ~host:self in
  let vm_recoms =
    get_evacuation_recoms ~__context ~uuid:(Db.Host.get_uuid ~__context ~self)
  in
  let recs = Hashtbl.create 31 in
  List.iter
    (fun (v, detail) ->
      debug "WLB recommends VM evacuation: %s to %s"
        (Db.VM.get_name_label ~__context ~self:v)
        (String.concat "," detail) ;
      (* Sanity check
         	Note: if the vm being moved is dom0 then this is a power management rec and this check does not apply
      *)
      let resident_h = Db.VM.get_resident_on ~__context ~self:v in
      let target_uuid = List.hd (List.tl detail) in
      let target_host = Db.Host.get_by_uuid ~__context ~uuid:target_uuid in
      if
        Db.Host.get_control_domain ~__context ~self:target_host <> v
        && Db.Host.get_uuid ~__context ~self:resident_h = target_uuid
      then (* resident host and migration host are the same. Reject this plan *)
        raise
          (Api_errors.Server_error
             ( Api_errors.wlb_malformed_response
             , [
                 Printf.sprintf
                   "WLB recommends migrating VM %s to the same server it is \
                    being evacuated from."
                   (Db.VM.get_name_label ~__context ~self:v)
               ] )) ;
      match detail with
      | ["WLB"; host_uuid; _] ->
          Hashtbl.replace recs v
            (Migrate (Db.Host.get_by_uuid ~__context ~uuid:host_uuid))
      | _ ->
          raise
            (Api_errors.Server_error
               ( Api_errors.wlb_malformed_response
               , ["WLB gave malformed details for VM evacuation."] )))
    vm_recoms ;
  Hashtbl.iter
    (fun v detail ->
      match detail with
      | Migrate _ ->
          (* Skip migrations -- WLB is providing these *)
          ()
      | Error (e, _) when e = Api_errors.host_not_enough_free_memory ->
          (* Skip errors down to free memory -- we're letting WLB decide this *)
          ()
      | Error _ as p ->
          debug "VM preventing evacuation: %s because %s"
            (Db.VM.get_name_label ~__context ~self:v)
            (string_of_per_vm_plan p) ;
          Hashtbl.replace recs v detail)
    error_vms ;
  let resident_vms =
    List.filter
      (fun v ->
        (not (Db.VM.get_is_control_domain ~__context ~self:v))
        && not (Db.VM.get_is_a_template ~__context ~self:v))
      (Db.Host.get_resident_VMs ~__context ~self)
  in
  List.iter
    (fun vm ->
      if not (Hashtbl.mem recs vm) then
        (* Anything for which we don't have a recommendation from WLB, but which is agile, we treat as "not enough memory" *)
        Hashtbl.replace recs vm
          (Error (Api_errors.host_not_enough_free_memory, [Ref.string_of vm])))
    resident_vms ;
  Hashtbl.iter
    (fun vm detail ->
      debug "compute_evacuation_plan_wlb: Key: %s Value %s"
        (Db.VM.get_name_label ~__context ~self:vm)
        (string_of_per_vm_plan detail))
    recs ;
  recs

let compute_evacuation_plan ~__context ~host =
  let oc =
    Db.Pool.get_other_config ~__context ~self:(Helpers.get_pool ~__context)
  in
  if
    List.exists
      (fun (k, v) ->
        k = "wlb_choose_host_disable" && String.lowercase_ascii v = "true")
      oc
    || not (Workload_balancing.check_wlb_enabled ~__context)
  then (
    debug
      "Using wlb recommendations for choosing a host has been disabled or wlb \
       is not available. Using original algorithm" ;
    compute_evacuation_plan_no_wlb ~__context ~host
  ) else
    try
      debug "Using WLB recommendations for host evacuation." ;
      compute_evacuation_plan_wlb ~__context ~self:host
    with
    | Api_errors.Server_error (error_type, error_detail) ->
        debug
          "Encountered error when using wlb for choosing host \"%s: %s\". \
           Using original algorithm"
          error_type
          (String.concat "" error_detail) ;
        ( try
            let uuid = Db.Host.get_uuid ~__context ~self:host in
            let message_body =
              Printf.sprintf
                "Wlb consultation for Host '%s' failed (pool uuid: %s)"
                (Db.Host.get_name_label ~__context ~self:host)
                (Db.Pool.get_uuid ~__context
                   ~self:(Helpers.get_pool ~__context))
            in
            let name, priority = Api_messages.wlb_failed in
            ignore
              (Xapi_message.create ~__context ~name ~priority ~cls:`Host
                 ~obj_uuid:uuid ~body:message_body)
          with _ -> ()
        ) ;
        compute_evacuation_plan_no_wlb ~__context ~host
    | _ ->
        debug
          "Encountered an unknown error when using wlb for choosing host. \
           Using original algorithm" ;
        compute_evacuation_plan_no_wlb ~__context ~host

let evacuate ~__context ~host =
  let task = Context.get_task_id __context in
  let plans = compute_evacuation_plan ~__context ~host in
  (* Check there are no errors in this list *)
  Hashtbl.iter
    (fun vm plan ->
      match plan with
      | Error (code, params) ->
          raise (Api_errors.Server_error (code, params))
      | _ ->
          ())
    plans ;
  (* Do it *)
  let individual_progress = 1.0 /. float (Hashtbl.length plans) in
  let migrate_vm vm plan =
    match plan with
    | Migrate host ->
        ( try
            Helpers.call_api_functions ~__context (fun rpc session_id ->
                Client.Client.VM.pool_migrate ~rpc ~session_id ~vm ~host
                  ~options:[("live", "true")])
          with
        | Api_errors.Server_error (code, params)
          when code = Api_errors.vm_bad_power_state ->
            ()
        | e ->
            raise e
        ) ;
        let progress = Db.Task.get_progress ~__context ~self:task in
        TaskHelper.set_progress ~__context (progress +. individual_progress)
    | Error (code, params) ->
        (* should never happen *)
        raise (Api_errors.Server_error (code, params))
  in
  let () = Hashtbl.iter migrate_vm plans in
  (* Now check there are no VMs left *)
  let vms = Db.Host.get_resident_VMs ~__context ~self:host in
  let vms =
    List.filter
      (fun vm -> not (Db.VM.get_is_control_domain ~__context ~self:vm))
      vms
  in
  let remainder = List.length vms in
  if not (remainder = 0) then
    raise
      Api_errors.(
        Server_error
          ( internal_error
          , [
              Printf.sprintf "evacuate: %d VMs are still resident on %s"
                remainder (Ref.string_of host)
            ] ))

let retrieve_wlb_evacuate_recommendations ~__context ~self =
  let plans = compute_evacuation_plan_wlb ~__context ~self in
  Hashtbl.fold
    (fun vm detail acc ->
      let plan =
        match detail with
        | Error (e, t) ->
            e :: t
        | Migrate h ->
            ["WLB"; Db.Host.get_uuid ~__context ~self:h]
      in
      (vm, plan) :: acc)
    plans []

let restart_agent ~__context ~host =
  let syslog_stdout = Forkhelpers.Syslog_WithKey "Host.restart_agent" in
  let pid =
    Forkhelpers.safe_close_and_exec None None None [] ~syslog_stdout
      !Xapi_globs.xe_toolstack_restart
      []
  in
  debug "Created process with pid: %d to perform xe-toolstack-restart"
    (Forkhelpers.getpid pid)

let shutdown_agent ~__context =
  debug "Host.restart_agent: Host agent will shutdown in 1s!!!!" ;
  Xapi_fuse.light_fuse_and_dont_restart ~fuse_length:1. ()

let disable ~__context ~host =
  if Db.Host.get_enabled ~__context ~self:host then (
    info
      "Host.enabled: setting host %s (%s) to disabled because of user request"
      (Ref.string_of host)
      (Db.Host.get_hostname ~__context ~self:host) ;
    Db.Host.set_enabled ~__context ~self:host ~value:false ;
    Xapi_host_helpers.user_requested_host_disable := true
  )

let enable ~__context ~host =
  if not (Db.Host.get_enabled ~__context ~self:host) then (
    assert_safe_to_reenable ~__context ~self:host ;
    Xapi_host_helpers.user_requested_host_disable := false ;
    info "Host.enabled: setting host %s (%s) to enabled because of user request"
      (Ref.string_of host)
      (Db.Host.get_hostname ~__context ~self:host) ;
    Db.Host.set_enabled ~__context ~self:host ~value:true ;
    (* Normally we schedule a plan recomputation when we successfully plug in our storage. In the case
       	   when some of our storage was broken and required maintenance, we end up here, manually re-enabling
       	   the host. If we're overcommitted then this might fix the problem. *)
    let pool = Helpers.get_pool ~__context in
    if
      Db.Pool.get_ha_enabled ~__context ~self:pool
      && Db.Pool.get_ha_overcommitted ~__context ~self:pool
    then
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.Pool.ha_schedule_plan_recomputation rpc session_id)
  )

let prepare_for_poweroff_precheck ~__context ~host =
  Xapi_host_helpers.assert_host_disabled ~__context ~host

let prepare_for_poweroff ~__context ~host =
  (* Do not run assert_host_disabled here, continue even if the host is
      enabled: the host is already shutting down when this function gets called *)
  let i_am_master = Pool_role.is_master () in
  if i_am_master then
    (* We are the master and we are about to shutdown HA and redo log:
       prevent slaves from sending (DB) requests.
          If we are the slave we cannot shutdown the request thread yet
          because we might need it when unplugging the PBDs
    *)
    Remote_requests.stop_request_thread () ;
  Vm_evacuation.ensure_no_vms ~__context ~evacuate_timeout:0. ;
  Xapi_ha.before_clean_shutdown_or_reboot ~__context ~host ;
  Xapi_pbd.unplug_all_pbds ~__context ;
  if not i_am_master then
    Remote_requests.stop_request_thread () ;
  (* Push the Host RRD to the master. Note there are no VMs running here so we don't have to worry about them. *)
  if not (Pool_role.is_master ()) then
    log_and_ignore_exn (fun () ->
        Rrdd.send_host_rrd_to_master (Pool_role.get_master_address ())) ;
  (* Also save the Host RRD to local disk for us to pick up when we return. Note there are no VMs running at this point. *)
  log_and_ignore_exn (Rrdd.backup_rrds None) ;
  (* This prevents anyone actually re-enabling us until after reboot *)
  Localdb.put Constants.host_disabled_until_reboot "true" ;
  (* This helps us distinguish between an HA fence and a reboot *)
  Localdb.put Constants.host_restarted_cleanly "true"

let shutdown_and_reboot_common ~__context ~host label description operation cmd
    =
  (* The actual shutdown actions are done asynchronously, in a call to
     prepare_for_poweroff, so the API user will not be notified of any errors
     that happen during that operation.
     Therefore here we make an additional call to the prechecks of every
     operation that gets called from prepare_for_poweroff, either directly or
     indirectly, to fail early and ensure that a suitable error is returned to
     the XenAPI user. *)
  let shutdown_precheck () =
    prepare_for_poweroff_precheck ~__context ~host ;
    Xapi_ha.before_clean_shutdown_or_reboot_precheck ~__context ~host
  in
  shutdown_precheck () ;
  (* This tells the master that the shutdown is still ongoing: it can be used to continue
     	 masking other operations even after this call return.

     	 If xapi restarts then this task will be reset by the startup code, which is unfortunate
     	 but the host will stay disabled provided host_disabled_until_reboot is still set... so
     	 safe but ugly. *)
  Server_helpers.exec_with_new_task ~subtask_of:(Context.get_task_id __context)
    ~task_description:description ~task_in_database:true label
    (fun __newcontext ->
      Db.Host.add_to_current_operations ~__context ~self:host
        ~key:(Ref.string_of (Context.get_task_id __newcontext))
        ~value:operation ;
      (* Do the shutdown in a background thread with a delay to give this API call
         	 a reasonable chance of succeeding. *)
      ignore
        (Thread.create
           (fun () ->
             Thread.delay 10. ;
             ignore (Sys.command cmd))
           ()))

let shutdown ~__context ~host =
  shutdown_and_reboot_common ~__context ~host "Host is shutting down"
    "Host is shutting down" `shutdown "/sbin/shutdown -h now"

let reboot ~__context ~host =
  shutdown_and_reboot_common ~__context ~host "Host is rebooting"
    "Host is rebooting" `shutdown "/sbin/shutdown -r now"

let power_on ~__context ~host =
  let result =
    Xapi_plugins.call_plugin
      (Context.get_session_id __context)
      Constants.power_on_plugin Constants.power_on_fn
      [("remote_host_uuid", Db.Host.get_uuid ~__context ~self:host)]
  in
  if result <> "True" then
    failwith (Printf.sprintf "The host failed to power on.")

let dmesg ~__context ~host =
  let dbg = Context.string_of_task __context in
  let open Xapi_xenops_queue in
  let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
  Client.HOST.get_console_data dbg

let dmesg_clear ~__context ~host =
  raise (Api_errors.Server_error (Api_errors.not_implemented, ["dmesg_clear"]))

let get_log ~__context ~host =
  raise (Api_errors.Server_error (Api_errors.not_implemented, ["get_log"]))

let send_debug_keys ~__context ~host ~keys =
  let open Xapi_xenops_queue in
  let module Client = (val make_client (default_xenopsd ()) : XENOPS) in
  let dbg = Context.string_of_task __context in
  Client.HOST.send_debug_keys dbg keys

let list_methods ~__context =
  raise (Api_errors.Server_error (Api_errors.not_implemented, ["list_method"]))

let is_slave ~__context ~host = not (Pool_role.is_master ())

let ask_host_if_it_is_a_slave ~__context ~host =
  let ask_and_warn_when_slow ~__context =
    let local_fn = is_slave ~host in
    let timeout = 10. in
    let task_name = Context.get_task_id __context |> Ref.string_of in
    let ip, uuid =
      ( Db.Host.get_address ~__context ~self:host
      , Db.Host.get_uuid ~__context ~self:host )
    in
    let rec log_host_slow_to_respond timeout () =
      D.warn
        "ask_host_if_it_is_a_slave: host taking a long time to respond - IP: \
         %s; uuid: %s"
        ip uuid ;
      Xapi_periodic_scheduler.add_to_queue task_name
        Xapi_periodic_scheduler.OneShot timeout
        (log_host_slow_to_respond (min (2. *. timeout) 300.))
    in
    Xapi_periodic_scheduler.add_to_queue task_name
      Xapi_periodic_scheduler.OneShot timeout
      (log_host_slow_to_respond timeout) ;
    let res =
      Message_forwarding.do_op_on_localsession_nolivecheck ~local_fn ~__context
        ~host (fun session_id rpc ->
          Client.Client.Pool.is_slave rpc session_id host)
    in
    Xapi_periodic_scheduler.remove_from_queue task_name ;
    res
  in
  Server_helpers.exec_with_subtask ~__context "host.ask_host_if_it_is_a_slave"
    ask_and_warn_when_slow

let is_host_alive ~__context ~host =
  (* If the host is marked as not-live then assume we don't need to contact it to verify *)
  let should_contact_host =
    try
      let hm = Db.Host.get_metrics ~__context ~self:host in
      Db.Host_metrics.get_live ~__context ~self:hm
    with _ -> true
  in
  if should_contact_host then (
    debug
      "is_host_alive host=%s is marked as live in the database; asking host to \
       make sure"
      (Ref.string_of host) ;
    try
      ignore (ask_host_if_it_is_a_slave ~__context ~host) ;
      true
    with e ->
      warn
        "is_host_alive host=%s caught %s while querying host liveness: \
         assuming dead"
        (Ref.string_of host)
        (ExnHelper.string_of_exn e) ;
      false
  ) else (
    debug
      "is_host_alive host=%s is marked as dead in the database; treating this \
       as definitive."
      (Ref.string_of host) ;
    false
  )

let create ~__context ~uuid ~name_label ~name_description ~hostname ~address
    ~external_auth_type ~external_auth_service_name ~external_auth_configuration
    ~license_params ~edition ~license_server ~local_cache_sr ~chipset_info
    ~ssl_legacy =
  (* fail-safe. We already test this on the joining host, but it's racy, so multiple concurrent
     pool-join might succeed. Note: we do it in this order to avoid a problem checking restrictions during
     the initial setup of the database *)
  if
    List.length (Db.Host.get_all ~__context) >= Xapi_globs.restricted_pool_size
    && pool_size_is_restricted ~__context
  then
    raise
      (Api_errors.Server_error
         ( Api_errors.license_restriction
         , [Features.name_of_feature Features.Pool_size] )) ;
  let make_new_metrics_object ref =
    Db.Host_metrics.create ~__context ~ref
      ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
      ~live:false ~memory_total:0L ~memory_free:0L ~last_updated:Date.never
      ~other_config:[]
  in
  let name_description = "Default install" and host = Ref.make () in
  let metrics = Ref.make () in
  make_new_metrics_object metrics ;
  let host_is_us = uuid = Helpers.get_localhost_uuid () in
  Db.Host.create ~__context ~ref:host ~current_operations:[]
    ~allowed_operations:[]
    ~software_version:(Xapi_globs.software_version ())
    ~enabled:false ~aPI_version_major:Datamodel_common.api_version_major
    ~aPI_version_minor:Datamodel_common.api_version_minor
    ~aPI_version_vendor:Datamodel_common.api_version_vendor
    ~aPI_version_vendor_implementation:
      Datamodel_common.api_version_vendor_implementation ~name_description
    ~name_label ~uuid ~other_config:[] ~capabilities:[]
    ~cpu_configuration:[] (* !!! FIXME hard coding *) ~cpu_info:[] ~chipset_info
    ~memory_overhead:0L ~sched_policy:"credit" (* !!! FIXME hard coding *)
    ~supported_bootloaders:(List.map fst Xapi_globs.supported_bootloaders)
    ~suspend_image_sr:Ref.null ~crash_dump_sr:Ref.null ~logging:[] ~hostname
    ~address ~metrics ~license_params ~boot_free_mem:0L ~ha_statefiles:[]
    ~ha_network_peers:[] ~blobs:[] ~tags:[] ~external_auth_type
    ~external_auth_service_name ~external_auth_configuration ~edition
    ~license_server ~bios_strings:[] ~power_on_mode:"" ~power_on_config:[]
    ~local_cache_sr ~ssl_legacy:false ~guest_VCPUs_params:[] ~display:`enabled
    ~virtual_hardware_platform_versions:
      ( if host_is_us then
          Xapi_globs.host_virtual_hardware_platform_versions
      else
        [0L]
      )
    ~control_domain:Ref.null ~updates_requiring_reboot:[] ~iscsi_iqn:""
    ~multipathing:false ~uefi_certificates:"" ~editions:[] ;
  (* If the host we're creating is us, make sure its set to live *)
  Db.Host_metrics.set_last_updated ~__context ~self:metrics
    ~value:(Date.of_float (Unix.gettimeofday ())) ;
  Db.Host_metrics.set_live ~__context ~self:metrics ~value:host_is_us ;
  host

let precheck_destroy_declare_dead ~__context ~self call =
  (* Fail if the host is still online: the user should either isolate the machine from the network
     	 or use Pool.eject. *)
  let hostname = Db.Host.get_hostname ~__context ~self in
  if is_host_alive ~__context ~host:self then (
    error
      "Host.%s successfully contacted host %s; host is not offline; refusing \
       to %s"
      call hostname call ;
    raise
      (Api_errors.Server_error (Api_errors.host_is_live, [Ref.string_of self]))
  ) ;
  (* This check is probably redundant since the Pool master should always be 'alive': *)
  (* It doesn't make any sense to destroy the master's own record *)
  let me = Helpers.get_localhost ~__context in
  if self = me then
    raise
      (Api_errors.Server_error (Api_errors.host_is_live, [Ref.string_of self]))

(* Returns a tuple of lists: The first containing the control domains, and the second containing the regular VMs *)
let get_resident_vms ~__context ~self =
  let my_resident_vms = Db.Host.get_resident_VMs ~__context ~self in
  List.partition
    (fun vm -> Db.VM.get_is_control_domain ~__context ~self:vm)
    my_resident_vms

let destroy ~__context ~self =
  precheck_destroy_declare_dead ~__context ~self "destroy" ;
  (* CA-23732: Block if HA is enabled *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then
    raise (Api_errors.Server_error (Api_errors.ha_is_enabled, [])) ;
  let my_control_domains, my_regular_vms = get_resident_vms ~__context ~self in
  if List.length my_regular_vms > 0 then
    raise
      (Api_errors.Server_error
         (Api_errors.host_has_resident_vms, [Ref.string_of self])) ;
  (* Call external host failed hook (allows a third-party to use power-fencing if desired).
   * This will declare the host as dead to the clustering daemon *)
  Xapi_hooks.host_pre_declare_dead ~__context ~host:self
    ~reason:Xapi_hooks.reason__dbdestroy ;
  (* Call the hook before we destroy the stuff as it will likely need the
     database records *)
  Xapi_hooks.host_post_declare_dead ~__context ~host:self
    ~reason:Xapi_hooks.reason__dbdestroy ;
  Db.Host.destroy ~__context ~self ;
  Create_misc.create_pool_cpuinfo ~__context ;
  List.iter (fun vm -> Db.VM.destroy ~__context ~self:vm) my_control_domains ;
  Pool_features.update_pool_features ~__context

let declare_dead ~__context ~host =
  precheck_destroy_declare_dead ~__context ~self:host "declare_dead" ;
  (* Call external host failed hook (allows a third-party to use power-fencing if desired).
   * This needs to happen before we reset the power state of the VMs *)
  Xapi_hooks.host_pre_declare_dead ~__context ~host
    ~reason:Xapi_hooks.reason__user ;
  let my_control_domains, my_regular_vms =
    get_resident_vms ~__context ~self:host
  in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      List.iter
        (fun vm -> Client.Client.VM.power_state_reset rpc session_id vm)
        my_regular_vms) ;
  Db.Host.set_enabled ~__context ~self:host ~value:false ;
  Xapi_hooks.host_post_declare_dead ~__context ~host
    ~reason:Xapi_hooks.reason__user

let ha_disable_failover_decisions ~__context ~host =
  Xapi_ha.ha_disable_failover_decisions __context host

let ha_disarm_fencing ~__context ~host =
  Xapi_ha.ha_disarm_fencing __context host

let ha_stop_daemon ~__context ~host = Xapi_ha.ha_stop_daemon __context host

let ha_release_resources ~__context ~host =
  Xapi_ha.ha_release_resources __context host

let ha_wait_for_shutdown_via_statefile ~__context ~host =
  Xapi_ha.ha_wait_for_shutdown_via_statefile __context host

let ha_xapi_healthcheck ~__context =
  (* Consider checking the status of various internal tasks / tickling locks but for now assume
     	 that, since we got here unharmed, all is well.*)
  not (Xapi_fist.fail_healthcheck ())

let preconfigure_ha ~__context ~host ~statefiles ~metadata_vdi ~generation =
  Xapi_ha.preconfigure_host __context host statefiles metadata_vdi generation

let ha_join_liveset ~__context ~host =
  try Xapi_ha.join_liveset __context host with
  | Xha_scripts.Xha_error Xha_errno.Mtc_exit_bootjoin_timeout ->
      error "HA enable failed with BOOTJOIN_TIMEOUT" ;
      raise (Api_errors.Server_error (Api_errors.ha_failed_to_form_liveset, []))
  | Xha_scripts.Xha_error Xha_errno.Mtc_exit_can_not_access_statefile ->
      error "HA enable failed with CAN_NOT_ACCESS_STATEFILE" ;
      raise
        (Api_errors.Server_error (Api_errors.ha_host_cannot_access_statefile, []))

let propose_new_master ~__context ~address ~manual =
  Xapi_ha.propose_new_master __context address manual

let commit_new_master ~__context ~address =
  Xapi_ha.commit_new_master __context address

let abort_new_master ~__context ~address =
  Xapi_ha.abort_new_master __context address

let update_master ~__context ~host ~master_address = assert false

let emergency_ha_disable ~__context ~soft =
  Xapi_ha.emergency_ha_disable __context soft

(* This call can be used to _instruct_ a slave that it has to take a persistent backup (force=true).
   If force=false then this is a hint from the master that the client may want to take a backup; in this
   latter case the slave applies its write-limiting policy and compares generation counts to determine whether
   it really should take a backup *)

let request_backup ~__context ~host ~generation ~force =
  if Helpers.get_localhost ~__context <> host then
    failwith "Forwarded to the wrong host" ;
  if Pool_role.is_master () then (
    debug "Requesting database backup on master: Using direct sync" ;
    let connections = Db_conn_store.read_db_connections () in
    Db_cache_impl.sync connections (Db_ref.get_database (Db_backend.make ()))
  ) else
    let master_address = Helpers.get_main_ip_address () in
    Pool_db_backup.fetch_database_backup ~master_address
      ~pool_secret:(Xapi_globs.pool_secret ())
      ~force:(if force then None else Some generation)

(* request_config_file_sync is used to inform a slave that it should consider resyncing dom0 config files
   (currently only /etc/passwd) *)
let request_config_file_sync ~__context ~host ~hash =
  debug "Received notification of dom0 config file change" ;
  let master_address = Helpers.get_main_ip_address () in
  Config_file_sync.fetch_config_files ~master_address
    ~pool_secret:(Xapi_globs.pool_secret ())

(* Host parameter will just be me, as message forwarding layer ensures this call has been forwarded correctly *)
let syslog_reconfigure ~__context ~host =
  let localhost = Helpers.get_localhost ~__context in
  let logging = Db.Host.get_logging ~__context ~self:localhost in
  let destination =
    try List.assoc "syslog_destination" logging with _ -> ""
  in
  let flag =
    match destination with "" -> "--noremote" | _ -> "--remote=" ^ destination
  in
  let args = [|!Xapi_globs.xe_syslog_reconfigure; flag|] in
  ignore (Unixext.spawnvp args.(0) args)

let get_management_interface ~__context ~host =
  let pifs =
    Db.PIF.get_refs_where ~__context
      ~expr:
        (And
           ( Eq (Field "host", Literal (Ref.string_of host))
           , Eq (Field "management", Literal "true") ))
  in
  match pifs with [] -> raise Not_found | pif :: _ -> pif

let change_management_interface ~__context interface primary_address_type =
  debug "Changing management interface" ;
  Xapi_mgmt_iface.change interface primary_address_type ;
  Xapi_mgmt_iface.run ~__context ~mgmt_enabled:true ;
  (* once the inventory file has been rewritten to specify new interface, sync up db with
     	   state of world.. *)
  Xapi_mgmt_iface.on_dom0_networking_change ~__context

let local_management_reconfigure ~__context ~interface =
  (* Only let this one through if we are in emergency mode, otherwise use
     	 Host.management_reconfigure *)
  if not !Xapi_globs.slave_emergency_mode then
    raise (Api_errors.Server_error (Api_errors.pool_not_in_emergency_mode, [])) ;
  change_management_interface ~__context interface
    (Record_util.primary_address_type_of_string
       (Xapi_inventory.lookup Xapi_inventory._management_address_type
          ~default:"ipv4"))

let management_reconfigure ~__context ~pif =
  (* Disallow if HA is enabled *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then
    raise (Api_errors.Server_error (Api_errors.ha_is_enabled, [])) ;
  let net = Db.PIF.get_network ~__context ~self:pif in
  let bridge = Db.Network.get_bridge ~__context ~self:net in
  let primary_address_type =
    Db.PIF.get_primary_address_type ~__context ~self:pif
  in
  if Db.PIF.get_managed ~__context ~self:pif = true then (
    Xapi_pif.assert_usable_for_management ~__context ~primary_address_type
      ~self:pif ;
    try
      let mgmt_pif =
        get_management_interface ~__context
          ~host:(Helpers.get_localhost ~__context)
      in
      let mgmt_address_type =
        Db.PIF.get_primary_address_type ~__context ~self:mgmt_pif
      in
      if primary_address_type <> mgmt_address_type then
        raise
          (Api_errors.Server_error
             (Api_errors.pif_incompatible_primary_address_type, []))
    with _ -> ()
    (* no current management interface *)
  ) ;
  if Db.PIF.get_management ~__context ~self:pif then
    debug "PIF %s is already marked as a management PIF; taking no action"
      (Ref.string_of pif)
  else (
    Xapi_network.attach_internal ~management_interface:true ~__context ~self:net
      () ;
    change_management_interface ~__context bridge primary_address_type ;
    Xapi_pif.update_management_flags ~__context
      ~host:(Helpers.get_localhost ~__context)
  )

let management_disable ~__context =
  (* Disallow if HA is enabled *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then
    raise (Api_errors.Server_error (Api_errors.ha_is_enabled, [])) ;
  (* Make sure we aren't about to disable our management interface on a slave *)
  if Pool_role.is_slave () then
    raise
      (Api_errors.Server_error (Api_errors.slave_requires_management_iface, [])) ;
  (* Reset the management server *)
  Xapi_mgmt_iface.change "" `IPv4 ;
  Xapi_mgmt_iface.run ~__context ~mgmt_enabled:false ;
  (* Make sure all my PIFs are marked appropriately *)
  Xapi_pif.update_management_flags ~__context
    ~host:(Helpers.get_localhost ~__context)

let get_system_status_capabilities ~__context ~host =
  if Helpers.get_localhost ~__context <> host then
    failwith "Forwarded to the wrong host" ;
  System_status.get_capabilities ()

let get_sm_diagnostics ~__context ~host = Storage_access.diagnostics ~__context

let get_thread_diagnostics ~__context ~host =
  Locking_helpers.Thread_state.to_graphviz ()

let sm_dp_destroy ~__context ~host ~dp ~allow_leak =
  Storage_access.dp_destroy ~__context dp allow_leak

let get_diagnostic_timing_stats ~__context ~host = Stats.summarise ()

(* CP-825: Serialize execution of host-enable-extauth and host-disable-extauth *)
(* We need to protect against concurrent execution of the extauth-hook script and host.enable/disable extauth, *)
(* because the extauth-hook script expects the auth_type, service_name etc to be constant throughout its execution *)
(* This mutex also serializes the execution of the plugin, to avoid concurrency problems when updating the sshd configuration *)
let serialize_host_enable_disable_extauth = Mutex.create ()

let set_hostname_live ~__context ~host ~hostname =
  Mutex.execute serialize_host_enable_disable_extauth (fun () ->
      let current_auth_type =
        Db.Host.get_external_auth_type ~__context ~self:host
      in
      (* the AD/Likewise extauth plugin is incompatible with a hostname change *)
      ( if current_auth_type = Xapi_globs.auth_type_AD_Likewise then
          let current_service_name =
            Db.Host.get_external_auth_service_name ~__context ~self:host
          in
          raise
            (Api_errors.Server_error
               ( Api_errors.auth_already_enabled
               , [current_auth_type; current_service_name] ))
      ) ;
      (* hostname is valid if contains only alpha, decimals, and hyphen
         	 (for hyphens, only in middle position) *)
      let is_invalid_hostname hostname =
        let len = String.length hostname in
        let i = ref 0 in
        let valid = ref true in
        let range =
          [('a', 'z'); ('A', 'Z'); ('0', '9'); ('-', '-'); ('.', '.')]
        in
        while !valid && !i < len do
          ( try
              ignore
                (List.find
                   (fun (r1, r2) -> r1 <= hostname.[!i] && hostname.[!i] <= r2)
                   range)
            with Not_found -> valid := false
          ) ;
          incr i
        done ;
        if hostname.[0] = '-' || hostname.[len - 1] = '-' then
          true
        else
          not !valid
      in
      if String.length hostname = 0 then
        raise
          (Api_errors.Server_error
             (Api_errors.host_name_invalid, ["hostname empty"])) ;
      if String.length hostname > 255 then
        raise
          (Api_errors.Server_error
             (Api_errors.host_name_invalid, ["hostname is too long"])) ;
      if is_invalid_hostname hostname then
        raise
          (Api_errors.Server_error
             ( Api_errors.host_name_invalid
             , ["hostname contains invalid characters"] )) ;
      ignore
        (Forkhelpers.execute_command_get_output !Xapi_globs.set_hostname
           [hostname]) ;
      Db.Host.set_hostname ~__context ~self:host ~value:hostname ;
      Helpers.update_domain_zero_name ~__context host hostname)

let set_ssl_legacy ~__context ~self ~value =
  if value then
    raise
      Api_errors.(
        Server_error
          ( value_not_supported
          , [
              "value"
            ; string_of_bool value
            ; "Legacy SSL support has been removed"
            ] ))
  else
    D.info "set_ssl_legacy: called with value: %b - doing nothing" value

let is_in_emergency_mode ~__context = !Xapi_globs.slave_emergency_mode

let compute_free_memory ~__context ~host =
  (*** XXX: Use a more appropriate free memory calculation here. *)
  Memory_check.host_compute_free_memory_with_maximum_compression
    ~dump_stats:false ~__context ~host None

let compute_memory_overhead ~__context ~host =
  Memory_check.host_compute_memory_overhead ~__context ~host

let get_data_sources ~__context ~host =
  List.map Rrdd_helper.to_API_data_source (Rrdd.query_possible_host_dss ())

let record_data_source ~__context ~host ~data_source =
  Rrdd.add_host_ds data_source

let query_data_source ~__context ~host ~data_source =
  Rrdd.query_host_ds data_source

let forget_data_source_archives ~__context ~host ~data_source =
  Rrdd.forget_host_ds data_source

let tickle_heartbeat ~__context ~host ~stuff =
  Db_gc.tickle_heartbeat ~__context host stuff

let create_new_blob ~__context ~host ~name ~mime_type ~public =
  let blob = Xapi_blob.create ~__context ~mime_type ~public in
  Db.Host.add_to_blobs ~__context ~self:host ~key:name ~value:blob ;
  blob

let extauth_hook_script_name = Extauth.extauth_hook_script_name

(* this special extauth plugin call is only used inside host.enable/disable extauth to avoid deadlock with the mutex *)
let call_extauth_plugin_nomutex ~__context ~host ~fn ~args =
  let plugin = extauth_hook_script_name in
  debug "Calling extauth plugin %s in host %s with event %s" plugin
    (Db.Host.get_name_label ~__context ~self:host)
    fn ;
  Xapi_plugins.call_plugin (Context.get_session_id __context) plugin fn args

(* this is the generic extauth plugin call available to xapi users that avoids concurrency problems *)
let call_extauth_plugin ~__context ~host ~fn ~args =
  Mutex.execute serialize_host_enable_disable_extauth (fun () ->
      call_extauth_plugin_nomutex ~__context ~host ~fn ~args)

(* this is the generic plugin call available to xapi users *)
let call_plugin ~__context ~host ~plugin ~fn ~args =
  if plugin = extauth_hook_script_name then
    call_extauth_plugin ~__context ~host ~fn ~args
  else
    Xapi_plugins.call_plugin (Context.get_session_id __context) plugin fn args

(* this is the generic extension call available to xapi users *)
let call_extension ~__context ~host ~call =
  let rpc = Jsonrpc.call_of_string call in
  let response = Xapi_extensions.call_extension rpc in
  if response.Rpc.success then
    response.Rpc.contents
  else
    let failure = response.Rpc.contents in
    let protocol_failure () =
      raise
        Api_errors.(
          Server_error (extension_protocol_failure, [Jsonrpc.to_string failure]))
    in
    match failure with
    | Rpc.Enum xs -> (
      (* This really ought to be a list of strings... *)
      match
        List.map (function Rpc.String x -> x | _ -> protocol_failure ()) xs
      with
      | x :: xs ->
          raise (Api_errors.Server_error (x, xs))
      | _ ->
          protocol_failure ()
    )
    | Rpc.String x ->
        raise (Api_errors.Server_error (x, []))
    | _ ->
        protocol_failure ()

let has_extension ~__context ~host ~name =
  try
    let (_ : string) = Xapi_extensions.find_extension name in
    true
  with _ -> false

let sync_data ~__context ~host = Xapi_sync.sync_host __context host

(* Nb, no attempt to wrap exceptions yet *)

let backup_rrds ~__context ~host ~delay =
  Xapi_periodic_scheduler.add_to_queue "RRD backup"
    Xapi_periodic_scheduler.OneShot delay (fun _ ->
      let remote_address =
        try Some (Pool_role.get_master_address ()) with _ -> None
      in
      log_and_ignore_exn (Rrdd.backup_rrds remote_address) ;
      log_and_ignore_exn (fun () ->
          List.iter
            (fun sr -> Xapi_sr.maybe_copy_sr_rrds ~__context ~sr)
            (Helpers.get_all_plugged_srs ~__context)))

let get_servertime ~__context ~host = Date.of_float (Unix.gettimeofday ())

let get_server_localtime ~__context ~host = Date.localtime ()

let enable_binary_storage ~__context ~host =
  Unixext.mkdir_safe Xapi_globs.xapi_blob_location 0o700 ;
  Db.Host.remove_from_other_config ~__context ~self:host
    ~key:Xapi_globs.host_no_local_storage

let disable_binary_storage ~__context ~host =
  ignore
    (Helpers.get_process_output
       (Printf.sprintf "/bin/rm -rf %s" Xapi_globs.xapi_blob_location)) ;
  Db.Host.remove_from_other_config ~__context ~self:host
    ~key:Xapi_globs.host_no_local_storage ;
  Db.Host.add_to_other_config ~__context ~self:host
    ~key:Xapi_globs.host_no_local_storage ~value:"true"

(* Dummy implementation for a deprecated API method. *)
let get_uncooperative_resident_VMs ~__context ~self = []

(* Dummy implementation for a deprecated API method. *)
let get_uncooperative_domains ~__context ~self = []

let certificate_install ~__context ~host ~name ~cert =
  Certificates.(host_install CA_Certificate name cert)

let certificate_uninstall ~__context ~host ~name =
  Certificates.(host_uninstall CA_Certificate name)

let certificate_list ~__context ~host = Certificates.(local_list CA_Certificate)

let crl_install ~__context ~host ~name ~crl =
  Certificates.(host_install CRL name crl)

let crl_uninstall ~__context ~host ~name =
  Certificates.(host_uninstall CRL name)

let crl_list ~__context ~host = Certificates.(local_list CRL)

let certificate_sync ~__context ~host = Certificates.local_sync ()

let get_server_certificate ~__context ~host =
  Certificates.get_server_certificate ()

let install_server_certificate ~__context ~host ~certificate ~private_key
    ~certificate_chain =
  if Db.Pool.get_ha_enabled ~__context ~self:(Helpers.get_pool ~__context) then
    raise Api_errors.(Server_error (ha_is_enabled, [])) ;
  let expr =
    Db_filter_types.(Eq (Field "host", Literal (Ref.string_of host)))
  in
  let current_server_certs = Db.Certificate.get_refs_where ~__context ~expr in
  let pem_chain =
    match certificate_chain with "" -> None | pem_chain -> Some pem_chain
  in
  let certificate =
    Certificates.install_server_certificate ~pem_leaf:certificate
      ~pkcs8_private_key:private_key ~pem_chain
  in
  let date_of_ptime time = Date.of_float (Ptime.to_float_s time) in
  let dates_of_ptimes (a, b) = (date_of_ptime a, date_of_ptime b) in
  let not_before, not_after =
    dates_of_ptimes (X509.Certificate.validity certificate)
  in
  let fingerprint =
    X509.Certificate.fingerprint Mirage_crypto.Hash.(`SHA256) certificate
    |> Certificates.pp_hash
  in
  let uuid = Uuid.(to_string (make_uuid ())) in
  let ref = Ref.make () in
  List.iter
    (fun self -> Db.Certificate.destroy ~__context ~self)
    current_server_certs ;
  Db.Certificate.create ~__context ~ref ~uuid ~host ~not_before ~not_after
    ~fingerprint ;
  let task = Context.get_task_id __context in
  (* mark task as done *)
  Db.Task.set_progress ~__context ~self:task ~value:1.0 ;
  (* sever all connections done with stunnel *)
  Xapi_mgmt_iface.reconfigure_stunnel ~__context

let emergency_reset_server_certificate ~__context =
  let xapi_ssl_pem = !Xapi_globs.server_cert_path in
  let common_name, alt_names =
    match Gencertlib.Lib.hostnames () with
    | cn :: alt ->
        (cn, alt)
    | [] ->
        (Helper_hostname.get_hostname (), [])
    (* should never happen *)
  in
  Gencertlib.Selfcert.host common_name alt_names xapi_ssl_pem ;
  (* Reset stunnel to try to restablish TLS connections *)
  Xapi_mgmt_iface.reconfigure_stunnel ~__context ;
  let self = Helpers.get_localhost ~__context in
  (* Delete records of the server certificate in this host *)
  let expr =
    Db_filter_types.(Eq (Field "host", Literal (Ref.string_of self)))
  in
  Db.Certificate.get_refs_where ~__context ~expr
  |> List.iter (fun self -> Db.Certificate.destroy ~__context ~self)

(* CA-24856: detect non-homogeneous external-authentication config in pool *)
let detect_nonhomogeneous_external_auth_in_host ~__context ~host =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let pool = List.hd (Client.Client.Pool.get_all rpc session_id) in
      let master = Client.Client.Pool.get_master rpc session_id pool in
      let master_rec = Client.Client.Host.get_record rpc session_id master in
      let host_rec = Client.Client.Host.get_record rpc session_id host in
      (* if this host being verified is the master, then we need to verify homogeneity for all slaves in the pool *)
      if host_rec.API.host_uuid = master_rec.API.host_uuid then
        Client.Client.Pool.detect_nonhomogeneous_external_auth rpc session_id
          pool
      else
        (* this host is a slave, let's check if it is homogeneous to the master *)
        let master_external_auth_type =
          master_rec.API.host_external_auth_type
        in
        let master_external_auth_service_name =
          master_rec.API.host_external_auth_service_name
        in
        let host_external_auth_type = host_rec.API.host_external_auth_type in
        let host_external_auth_service_name =
          host_rec.API.host_external_auth_service_name
        in
        if
          host_external_auth_type <> master_external_auth_type
          || host_external_auth_service_name
             <> master_external_auth_service_name
        then (
          (* ... this slave has non-homogeneous external-authentication data *)
          debug
            "Detected non-homogeneous external authentication in host %s: \
             host_auth_type=%s, host_service_name=%s, master_auth_type=%s, \
             master_service_name=%s"
            (Ref.string_of host) host_external_auth_type
            host_external_auth_service_name master_external_auth_type
            master_external_auth_service_name ;
          (* raise alert about this non-homogeneous slave in the pool *)
          let host_uuid = host_rec.API.host_uuid in
          let name, priority =
            Api_messages.auth_external_pool_non_homogeneous
          in
          ignore
            (Client.Client.Message.create ~rpc ~session_id ~name ~priority
               ~cls:`Host ~obj_uuid:host_uuid
               ~body:
                 ("host_external_auth_type="
                 ^ host_external_auth_type
                 ^ ", host_external_auth_service_name="
                 ^ host_external_auth_service_name
                 ^ ", master_external_auth_type="
                 ^ master_external_auth_type
                 ^ ", master_external_auth_service_name="
                 ^ master_external_auth_service_name
                 ))
        ))

(* CP-717: Enables external auth/directory service on a single host within the pool with specified config, *)
(* type and service_name. Fails if an auth/directory service is already enabled for this host (must disable first).*)
(*
 * Each Host object will contain a string field, external_auth_type which will specify the type of the external auth/directory service.
   o In the case of AD, this will contain the string "AD". (If we subsequently allow other types of external auth/directory service to be configured, e.g. LDAP, then new type strings will be defined accordingly)
   o When no external authentication service is configured, this will contain the empty string
 * Each Host object will contain a (string*string) Map field, external_auth_configuration. This field is provided so that a particular xapi authentiation module has the option of persistently storing any configuration parameters (represented as key/value pairs) within the agent database.
 * Each Host object will contain a string field, external_auth_service_name, which contains sufficient information to uniquely identify and address the external authentication/directory service. (e.g. in the case of AD this would be a domain name)
 *)
open Auth_signature
open Extauth

let enable_external_auth ~__context ~host ~config ~service_name ~auth_type =
  (* CP-825: Serialize execution of host-enable-extauth and host-disable-extauth *)
  (* we need to protect against concurrent access to the host.external_auth_type variable *)
  Mutex.execute serialize_host_enable_disable_extauth (fun () ->
      let host_name_label = Db.Host.get_name_label ~__context ~self:host in
      let current_auth_type =
        Db.Host.get_external_auth_type ~__context ~self:host
      in
      let current_service_name =
        Db.Host.get_external_auth_service_name ~__context ~self:host
      in
      debug "current external_auth_type is %s" current_auth_type ;
      if current_auth_type <> "" then (
        (* if auth_type is already defined, then we cannot set up a new one *)
        let msg =
          Printf.sprintf
            "external authentication %s service %s is already enabled"
            current_auth_type current_service_name
        in
        debug
          "Failed to enable external authentication type %s for service name \
           %s in host %s: %s"
          auth_type service_name host_name_label msg ;
        raise
          (Api_errors.Server_error
             ( Api_errors.auth_already_enabled
             , [current_auth_type; current_service_name] ))
      ) else if auth_type = "" then (
        (* we must error out here, because we never enable an _empty_ external auth_type *)
        let msg = "" in
        debug
          "Failed while enabling unknown external authentication type %s for \
           service name %s in host %s"
          msg service_name host_name_label ;
        raise (Api_errors.Server_error (Api_errors.auth_unknown_type, [msg]))
      ) else
        (* if no auth_type is currently defined (it is an empty string), then we can set up a new one *)

        (* we try to use the configuration to set up the new external authentication service *)
        try
          if auth_type = Xapi_globs.auth_type_AD_Likewise then
            Extauth_plugin_ADpbis.Lwsmd.start ~wait_until_success:true ;
          (* we persist as much set up configuration now as we can *)
          Db.Host.set_external_auth_service_name ~__context ~self:host
            ~value:service_name ;

          (* the ext_auth.on_enable dispatcher called below will store the configuration params, and also *)
          (* filter out any one-time credentials such as the administrator password, so we *)
          (* should not call here 'host.set_external_auth_configuration ~config' *)

          (* use the special 'named dispatcher' function to call an extauth plugin function even though we have *)
          (* not yet set up the external_auth_type value that will enable generic access to the extauth plugin. *)
          (Ext_auth.nd auth_type).on_enable config ;

          (* from this point on, we have successfully enabled the external authentication services. *)

          (* Up to this point, we cannot call external auth functions via extauth's generic dispatcher d(). *)
          Db.Host.set_external_auth_type ~__context ~self:host ~value:auth_type ;

          (* From this point on, anyone can call external auth functions via extauth.ml's generic dispatcher d(), which depends on the value of external_auth_type. *)
          (* This enables all functions to the external authentication and directory service that xapi makes available to the user, *)
          (* such as external login, subject id/info queries, group membership etc *)

          (* CP-709: call extauth hook-script after extauth.enable *)
          (* we must not fork, intead block until the script has returned *)
          (* so that at most one enable-external-auth event script is running at any one time in the same host *)
          (* we use its local variation without mutex, otherwise we will deadlock *)
          let call_plugin_fn () =
            call_extauth_plugin_nomutex ~__context ~host
              ~fn:Extauth.event_name_after_extauth_enable
              ~args:(Extauth.get_event_params ~__context host)
          in
          ignore
            (Extauth.call_extauth_hook_script_in_host_wrapper ~__context host
               Extauth.event_name_after_extauth_enable ~call_plugin_fn) ;
          debug
            "external authentication service type %s for service name %s \
             enabled successfully in host %s"
            auth_type service_name host_name_label ;
          Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded := true ;
          (* CA-24856: detect non-homogeneous external-authentication config in this host *)
          detect_nonhomogeneous_external_auth_in_host ~__context ~host
        with
        | Extauth.Unknown_extauth_type msg ->
            (* unknown plugin *)
            (* we rollback to the original xapi configuration *)
            Db.Host.set_external_auth_type ~__context ~self:host
              ~value:current_auth_type ;
            Db.Host.set_external_auth_service_name ~__context ~self:host
              ~value:current_service_name ;
            debug
              "Failed while enabling unknown external authentication type %s \
               for service name %s in host %s"
              msg service_name host_name_label ;
            raise
              (Api_errors.Server_error (Api_errors.auth_unknown_type, [msg]))
        | Auth_signature.Auth_service_error (errtag, msg) ->
            (* plugin returned some error *)
            (* we rollback to the original xapi configuration *)
            Db.Host.set_external_auth_type ~__context ~self:host
              ~value:current_auth_type ;
            Db.Host.set_external_auth_service_name ~__context ~self:host
              ~value:current_service_name ;
            debug
              "Failed while enabling external authentication type %s for \
               service name %s in host %s"
              msg service_name host_name_label ;
            raise
              (Api_errors.Server_error
                 ( Api_errors.auth_enable_failed
                   ^ Auth_signature.suffix_of_tag errtag
                 , [msg] ))
        | e ->
            (* unknown failure, just-enabled plugin might be in an inconsistent state *)
            (* we rollback to the original xapi configuration *)
            Db.Host.set_external_auth_type ~__context ~self:host
              ~value:current_auth_type ;
            Db.Host.set_external_auth_service_name ~__context ~self:host
              ~value:current_service_name ;
            debug
              "Failed while enabling external authentication type %s for \
               service name %s in host %s"
              auth_type service_name host_name_label ;
            raise
              (Api_errors.Server_error
                 (Api_errors.auth_enable_failed, [ExnHelper.string_of_exn e])))

(* CP-718: Disables external auth/directory service for host *)
let disable_external_auth_common ?(during_pool_eject = false) ~__context ~host
    ~config =
  (* CP-825: Serialize execution of host-enable-extauth and host-disable-extauth *)
  (* we need to protect against concurrent access to the host.external_auth_type variable *)
  Mutex.execute serialize_host_enable_disable_extauth (fun () ->
      let host_name_label = Db.Host.get_name_label ~__context ~self:host in
      let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
      if auth_type = "" then
        (* nothing to do, external authentication is already disabled *)
        let msg = "external authentication service is already disabled" in
        debug "Failed to disable external authentication in host %s: %s"
          host_name_label msg
      (* we do not raise an exception here. for our purposes, there's nothing wrong*)
      (* disabling an already disabled authentication plugin *)
      else
        (* this is the case when auth_type <> "" *)
        (* CP-709: call extauth hook-script before extauth.disable *)
        (* we must not fork, instead block until the script has returned, so that the script is able *)
        (* to obtain auth_type and other information from the metadata and there is at most one *)
        (* disable-external-auth event script running at any one time in the same host *)
        (* we use its local variation without mutex, otherwise we will deadlock *)
        let call_plugin_fn () =
          call_extauth_plugin_nomutex ~__context ~host
            ~fn:Extauth.event_name_before_extauth_disable
            ~args:(Extauth.get_event_params ~__context host)
        in
        ignore
          (Extauth.call_extauth_hook_script_in_host_wrapper ~__context host
             Extauth.event_name_before_extauth_disable ~call_plugin_fn) ;
        (* 1. first, we try to call the external auth plugin to disable the external authentication service *)
        let plugin_disable_failure =
          try
            (Ext_auth.d ()).on_disable config ;
            None (* OK, on_disable succeeded *)
          with
          | Auth_signature.Auth_service_error (errtag, msg) ->
              debug
                "Failed while calling on_disable event of external \
                 authentication plugin in host %s: %s"
                host_name_label msg ;
              Some
                (Api_errors.Server_error
                   ( Api_errors.auth_disable_failed
                     ^ Auth_signature.suffix_of_tag errtag
                   , [msg] ))
          | e ->
              (*absorb any exception*)
              debug
                "Failed while calling on_disable event of external \
                 authentication plugin in host %s: %s"
                host_name_label
                (ExnHelper.string_of_exn e) ;
              Some
                (Api_errors.Server_error
                   (Api_errors.auth_disable_failed, [ExnHelper.string_of_exn e]))
        in
        (* 2. then, if no exception was raised, we always remove our persistent extauth configuration *)
        Db.Host.set_external_auth_type ~__context ~self:host ~value:"" ;
        Db.Host.set_external_auth_service_name ~__context ~self:host ~value:"" ;
        debug "external authentication service disabled successfully in host %s"
          host_name_label ;
        (* 2.1 if we are still trying to initialize the external auth service in the xapi.on_xapi_initialize thread, we should stop now *)
        Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded := true ;

        (* succeeds because there's no need to initialize anymore *)

        (* 3. CP-703: we always revalidate all sessions after the external authentication has been disabled *)
        (* so that all sessions that were externally authenticated will be destroyed *)
        debug
          "calling revalidate_all_sessions after disabling external auth for \
           host %s"
          host_name_label ;
        Xapi_session.revalidate_all_sessions ~__context ;
        if not during_pool_eject then
          (* CA-28168 *)
          (* CA-24856: detect non-homogeneous external-authentication config in this host *)
          detect_nonhomogeneous_external_auth_in_host ~__context ~host ;
        (* stop lwsmd if necessary *)
        if auth_type = Xapi_globs.auth_type_AD_Likewise then
          Extauth_plugin_ADpbis.Lwsmd.stop ~wait_until_success:false ;
        match plugin_disable_failure with
        | None ->
            ()
        | Some e ->
            if not during_pool_eject then
              raise e (* bubble up plugin's on_disable exception *)
            else
              ()
      (* we do not want to stop pool_eject *))

let disable_external_auth ~__context ~host ~config =
  disable_external_auth_common ~during_pool_eject:false ~__context ~host ~config

let attach_static_vdis ~__context ~host ~vdi_reason_map =
  (* We throw an exception immediately if any of the VDIs in vdi_reason_map is
     a changed block tracking metadata VDI. *)
  List.iter
    (function
      | vdi, _ ->
          if Db.VDI.get_type ~__context ~self:vdi = `cbt_metadata then (
            error
              "host.attach_static_vdis: one of the given VDIs has type \
               cbt_metadata (at %s)"
              __LOC__ ;
            raise
              (Api_errors.Server_error
                 ( Api_errors.vdi_incompatible_type
                 , [
                     Ref.string_of vdi
                   ; Record_util.vdi_type_to_string `cbt_metadata
                   ] ))
          ))
    vdi_reason_map ;
  let attach (vdi, reason) =
    let static_vdis = Static_vdis_list.list () in
    let check v =
      v.Static_vdis_list.uuid = Db.VDI.get_uuid ~__context ~self:vdi
      && v.Static_vdis_list.currently_attached
    in
    if not (List.exists check static_vdis) then
      Pervasiveext.ignore_string
        (Static_vdis.permanent_vdi_attach ~__context ~vdi ~reason)
  in
  List.iter attach vdi_reason_map

let detach_static_vdis ~__context ~host ~vdis =
  let detach vdi =
    let static_vdis = Static_vdis_list.list () in
    let check v =
      v.Static_vdis_list.uuid = Db.VDI.get_uuid ~__context ~self:vdi
    in
    if List.exists check static_vdis then
      Static_vdis.permanent_vdi_detach ~__context ~vdi
  in
  List.iter detach vdis

let update_pool_secret ~__context ~host ~pool_secret =
  SecretString.write_to_file !Xapi_globs.pool_secret_path pool_secret

let set_localdb_key ~__context ~host ~key ~value =
  Localdb.put key value ;
  debug "Local-db key '%s' has been set to '%s'" key value

(* Licensing *)

let copy_license_to_db ~__context ~host ~features ~additional =
  let restrict_kvpairs = Features.to_assoc_list features in
  let license_params = additional @ restrict_kvpairs in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      (* This will trigger a pool sku/restrictions recomputation *)
      Client.Client.Host.set_license_params rpc session_id
        !Xapi_globs.localhost_ref license_params)

let set_license_params ~__context ~self ~value =
  Db.Host.set_license_params ~__context ~self ~value ;
  Pool_features.update_pool_features ~__context

let apply_edition_internal ~__context ~host ~edition ~additional =
  (* Get localhost's current license state. *)
  let license_server = Db.Host.get_license_server ~__context ~self:host in
  let current_edition = Db.Host.get_edition ~__context ~self:host in
  let current_license_params =
    Db.Host.get_license_params ~__context ~self:host
  in
  (* Make sure the socket count in license_params is correct.
     	 * At first boot, the key won't exist, and it may be wrong if we've restored
     	 * a database dump from a different host. *)
  let cpu_info = Db.Host.get_cpu_info ~__context ~self:host in
  let socket_count = List.assoc "socket_count" cpu_info in
  let current_license_params =
    Xapi_stdext_std.Listext.List.replace_assoc "sockets" socket_count
      current_license_params
  in
  (* Construct the RPC params to be sent to v6d *)
  let params =
    (("current_edition", current_edition) :: license_server)
    @ current_license_params
    @ additional
  in
  let new_ed =
    let dbg = Context.string_of_task __context in
    try V6_client.apply_edition dbg edition params with
    | V6_interface.(V6_error (Invalid_edition e)) ->
        raise Api_errors.(Server_error (invalid_edition, [e]))
    | V6_interface.(V6_error License_processing_error) ->
        raise Api_errors.(Server_error (license_processing_error, []))
    | V6_interface.(V6_error Missing_connection_details) ->
        raise Api_errors.(Server_error (missing_connection_details, []))
    | V6_interface.(V6_error (License_checkout_error s)) ->
        raise Api_errors.(Server_error (license_checkout_error, [s]))
    | V6_interface.(V6_error (Internal_error e)) ->
        raise Api_errors.(Server_error (internal_error, [e]))
  in
  let create_feature fname fenabled =
    Db.Feature.create ~__context
      ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
      ~ref:(Ref.make ()) ~name_label:fname ~name_description:""
      ~enabled:fenabled ~experimental:true ~version:"1.0" ~host
  in
  let update_feature rf fenabled =
    Db.Feature.set_enabled ~__context ~self:rf ~value:fenabled
  in
  let destroy_feature rf = Db.Feature.destroy ~__context ~self:rf in
  let rec remove_obsolete_features_from_db l =
    match l with
    | [] ->
        []
    | (rf, r) :: tl ->
        if List.mem_assoc r.API.feature_name_label new_ed.experimental_features
        then
          (rf, r) :: remove_obsolete_features_from_db tl
        else (
          destroy_feature rf ;
          remove_obsolete_features_from_db tl
        )
  in
  let old_features =
    let expr = Eq (Field "host", Literal (Ref.string_of host)) in
    let all_old = Db.Feature.get_records_where ~__context ~expr in
    remove_obsolete_features_from_db all_old
  in
  let load_feature_to_db (fname, fenabled) =
    old_features |> List.filter (fun (_, r) -> r.API.feature_name_label = fname)
    |> function
    | [] ->
        create_feature fname fenabled
    | [(rf, _)] ->
        update_feature rf fenabled
    | x ->
        List.iter (fun (rf, r) -> destroy_feature rf) x ;
        create_feature fname fenabled
  in
  let open V6_interface in
  List.iter load_feature_to_db new_ed.experimental_features ;
  Db.Host.set_edition ~__context ~self:host ~value:new_ed.edition_name ;
  let features = Features.of_assoc_list new_ed.xapi_params in
  copy_license_to_db ~__context ~host ~features
    ~additional:new_ed.additional_params

let apply_edition ~__context ~host ~edition ~force =
  (* if HA is enabled do not allow the edition to be changed *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then
    raise (Api_errors.Server_error (Api_errors.ha_is_enabled, []))
  else
    let additional = if force then [("force", "true")] else [] in
    apply_edition_internal ~__context ~host ~edition ~additional

let license_add ~__context ~host ~contents =
  let license =
    try Base64.decode_exn contents
    with _ ->
      error "Base64 decoding of supplied license has failed" ;
      raise Api_errors.(Server_error (license_processing_error, []))
  in
  let tmp = "/tmp/new_license" in
  Pervasiveext.finally
    (fun () ->
      ( try Unixext.write_string_to_file tmp license
        with _ ->
          let s = "Failed to write temporary file." in
          raise Api_errors.(Server_error (internal_error, [s]))
      ) ;
      apply_edition_internal ~__context ~host ~edition:""
        ~additional:[("license_file", tmp)])
    (fun () ->
      (* The license will have been moved to a standard location if it was valid, and
         			 * should be removed otherwise -> always remove the file at the tmp path, if any. *)
      Unixext.unlink_safe tmp)

let license_remove ~__context ~host =
  apply_edition_internal ~__context ~host ~edition:""
    ~additional:[("license_file", "")]

(* Supplemental packs *)

let refresh_pack_info ~__context ~host =
  debug "Refreshing software_version" ;
  let host_info = Create_misc.read_localhost_info ~__context in
  Create_misc.create_software_version ~__context host_info

(* Network reset *)

let reset_networking ~__context ~host =
  debug "Resetting networking" ;
  (* This is only ever done on the master, so using "Db.*.get_all " is ok. *)
  let local_pifs =
    List.filter
      (fun pif -> Db.PIF.get_host ~__context ~self:pif = host)
      (Db.PIF.get_all ~__context)
  in
  let bond_is_local bond =
    List.fold_left
      (fun a pif -> Db.Bond.get_master ~__context ~self:bond = pif || a)
      false local_pifs
  in
  let vlan_is_local vlan =
    List.fold_left
      (fun a pif -> Db.VLAN.get_untagged_PIF ~__context ~self:vlan = pif || a)
      false local_pifs
  in
  let tunnel_is_local tunnel =
    List.fold_left
      (fun a pif -> Db.Tunnel.get_access_PIF ~__context ~self:tunnel = pif || a)
      false local_pifs
  in
  let bonds = List.filter bond_is_local (Db.Bond.get_all ~__context) in
  List.iter
    (fun bond ->
      debug "destroying bond %s" (Db.Bond.get_uuid ~__context ~self:bond) ;
      Db.Bond.destroy ~__context ~self:bond)
    bonds ;
  let vlans = List.filter vlan_is_local (Db.VLAN.get_all ~__context) in
  List.iter
    (fun vlan ->
      debug "destroying VLAN %s" (Db.VLAN.get_uuid ~__context ~self:vlan) ;
      Db.VLAN.destroy ~__context ~self:vlan)
    vlans ;
  let tunnels = List.filter tunnel_is_local (Db.Tunnel.get_all ~__context) in
  List.iter
    (fun tunnel ->
      debug "destroying tunnel %s" (Db.Tunnel.get_uuid ~__context ~self:tunnel) ;
      Db.Tunnel.destroy ~__context ~self:tunnel)
    tunnels ;
  List.iter
    (fun self ->
      debug "destroying PIF %s" (Db.PIF.get_uuid ~__context ~self) ;
      ( if
        Db.PIF.get_physical ~__context ~self = true
        || Db.PIF.get_bond_master_of ~__context ~self <> []
      then
          let metrics = Db.PIF.get_metrics ~__context ~self in
          Db.PIF_metrics.destroy ~__context ~self:metrics
      ) ;
      Db.PIF.destroy ~__context ~self)
    local_pifs ;
  let netw_sriov_is_local self =
    List.mem (Db.Network_sriov.get_physical_PIF ~__context ~self) local_pifs
  in
  let netw_sriovs =
    List.filter netw_sriov_is_local (Db.Network_sriov.get_all ~__context)
  in
  List.iter
    (fun self ->
      let uuid = Db.Network_sriov.get_uuid ~__context ~self in
      debug "destroying network_sriov %s" uuid ;
      Db.Network_sriov.destroy ~__context ~self)
    netw_sriovs

(* Local storage caching *)

let enable_local_storage_caching ~__context ~host ~sr =
  assert_bacon_mode ~__context ~host ;
  let ty = Db.SR.get_type ~__context ~self:sr in
  let pbds = Db.SR.get_PBDs ~__context ~self:sr in
  let shared = Db.SR.get_shared ~__context ~self:sr in
  let has_required_capability =
    let caps = Sm.features_of_driver ty in
    List.mem_assoc Smint.Sr_supports_local_caching caps
  in
  debug "shared: %b. List.length pbds: %d. has_required_capability: %b" shared
    (List.length pbds) has_required_capability ;
  if shared = false && List.length pbds = 1 && has_required_capability then (
    let pbd_host = Db.PBD.get_host ~__context ~self:(List.hd pbds) in
    if pbd_host <> host then
      raise
        (Api_errors.Server_error
           ( Api_errors.host_cannot_see_SR
           , [Ref.string_of host; Ref.string_of sr] )) ;
    let old_sr = Db.Host.get_local_cache_sr ~__context ~self:host in
    if old_sr <> Ref.null then
      Db.SR.set_local_cache_enabled ~__context ~self:old_sr ~value:false ;
    Db.Host.set_local_cache_sr ~__context ~self:host ~value:sr ;
    Db.SR.set_local_cache_enabled ~__context ~self:sr ~value:true ;
    log_and_ignore_exn (fun () ->
        Rrdd.set_cache_sr (Db.SR.get_uuid ~__context ~self:sr))
  ) else
    raise (Api_errors.Server_error (Api_errors.sr_operation_not_supported, []))

let disable_local_storage_caching ~__context ~host =
  assert_bacon_mode ~__context ~host ;
  let sr = Db.Host.get_local_cache_sr ~__context ~self:host in
  Db.Host.set_local_cache_sr ~__context ~self:host ~value:Ref.null ;
  log_and_ignore_exn Rrdd.unset_cache_sr ;
  try Db.SR.set_local_cache_enabled ~__context ~self:sr ~value:false
  with _ -> ()

(* Here's how we do VLAN resyncing:
   We take a VLAN master and record (i) the Network it is on; (ii) its VLAN tag;
   (iii) the Network of the PIF that underlies the VLAN (e.g. eth0 underlies eth0.25).
   We then look to see whether we already have a VLAN record that is (i) on the same Network;
   (ii) has the same tag; and (iii) also has a PIF underlying it on the same Network.
   If we do not already have a VLAN that falls into this category then we make one,
   as long as we already have a suitable PIF to base the VLAN off -- if we don't have such a
   PIF (e.g. if the master has eth0.25 and we don't have eth0) then we do nothing.
*)
let sync_vlans ~__context ~host =
  let master = !Xapi_globs.localhost_ref in
  let master_vlan_pifs =
    Db.PIF.get_records_where ~__context
      ~expr:
        (And
           ( Eq (Field "host", Literal (Ref.string_of master))
           , Not (Eq (Field "VLAN_master_of", Literal (Ref.string_of Ref.null)))
           ))
  in
  let slave_vlan_pifs =
    Db.PIF.get_records_where ~__context
      ~expr:
        (And
           ( Eq (Field "host", Literal (Ref.string_of host))
           , Not (Eq (Field "VLAN_master_of", Literal (Ref.string_of Ref.null)))
           ))
  in
  let get_network_of_pif_underneath_vlan vlan_pif_rec =
    match Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec:vlan_pif_rec with
    | VLAN_untagged vlan :: _ ->
        let pif_underneath_vlan =
          Db.VLAN.get_tagged_PIF ~__context ~self:vlan
        in
        Db.PIF.get_network ~__context ~self:pif_underneath_vlan
    | _ ->
        raise
          Api_errors.(
            Server_error
              ( internal_error
              , [
                  Printf.sprintf "Cannot find vlan from a vlan master PIF:%s"
                    vlan_pif_rec.API.pIF_uuid
                ] ))
  in
  let maybe_create_vlan (master_pif_ref, master_pif_rec) =
    (* Check to see if the slave has any existing pif(s) that for the specified device, network, vlan... *)
    (* On the master, we find the pif, p, that underlies the VLAN
     * (e.g. "eth0" underlies "eth0.25") and then find the network that p's on: *)
    let network_of_pif_underneath_vlan_on_master =
      get_network_of_pif_underneath_vlan master_pif_rec
    in
    let existing_pif =
      List.filter
        (fun (slave_pif_ref, slave_pif_record) ->
          (* Is slave VLAN PIF that we're considering (slave_pif_ref) the one that corresponds
             			 * to the master_pif we're considering (master_pif_ref)? *)
          true
          && slave_pif_record.API.pIF_network = master_pif_rec.API.pIF_network
          && slave_pif_record.API.pIF_VLAN = master_pif_rec.API.pIF_VLAN
          && get_network_of_pif_underneath_vlan slave_pif_record
             = network_of_pif_underneath_vlan_on_master)
        slave_vlan_pifs
    in
    (* if I don't have any such pif(s) then make one: *)
    if existing_pif = [] then
      let pifs =
        Db.PIF.get_records_where ~__context
          ~expr:
            (And
               ( Eq (Field "host", Literal (Ref.string_of host))
               , Eq
                   ( Field "network"
                   , Literal
                       (Ref.string_of network_of_pif_underneath_vlan_on_master)
                   ) ))
      in
      match pifs with
      | [] ->
          (* We have no PIF on which to make the VLAN; do nothing *)
          ()
      | [(pif_ref, pif_rec)] ->
          (* This is the PIF on which we want to base our VLAN record; let's make it *)
          debug "Creating VLAN %Ld on slave" master_pif_rec.API.pIF_VLAN ;
          ignore
            (Xapi_vlan.create_internal ~__context ~host ~tagged_PIF:pif_ref
               ~tag:master_pif_rec.API.pIF_VLAN
               ~network:master_pif_rec.API.pIF_network
               ~device:pif_rec.API.pIF_device)
      | _ ->
          (* This should never happen since we should never have more than one of _our_ pifs
             					 * on the same network *)
          ()
  in
  (* For each of the master's PIFs, create a corresponding one on the slave if necessary *)
  List.iter maybe_create_vlan master_vlan_pifs

let sync_tunnels ~__context ~host =
  let master = !Xapi_globs.localhost_ref in
  let master_tunnel_pifs =
    Db.PIF.get_records_where ~__context
      ~expr:
        (And
           ( Eq (Field "host", Literal (Ref.string_of master))
           , Not (Eq (Field "tunnel_access_PIF_of", Literal "()")) ))
  in
  let slave_tunnel_pifs =
    Db.PIF.get_records_where ~__context
      ~expr:
        (And
           ( Eq (Field "host", Literal (Ref.string_of host))
           , Not (Eq (Field "tunnel_access_PIF_of", Literal "()")) ))
  in
  let get_network_of_transport_pif access_pif_rec =
    match Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec:access_pif_rec with
    | Tunnel_access tunnel :: _ ->
        let transport_pif =
          Db.Tunnel.get_transport_PIF ~__context ~self:tunnel
        in
        let protocol = Db.Tunnel.get_protocol ~__context ~self:tunnel in
        (Db.PIF.get_network ~__context ~self:transport_pif, protocol)
    | _ ->
        raise
          Api_errors.(
            Server_error
              ( internal_error
              , [
                  Printf.sprintf "PIF %s has no tunnel_access_PIF_of"
                    access_pif_rec.API.pIF_uuid
                ] ))
  in
  let maybe_create_tunnel_for_me (master_pif_ref, master_pif_rec) =
    (* check to see if I have any existing pif(s) that for the specified device, network, vlan... *)
    let existing_pif =
      List.filter
        (fun (_, slave_pif_record) ->
          (* Is the slave's tunnel access PIF that we're considering (slave_pif_ref)
           * the one that corresponds to the master's tunnel access PIF we're considering (master_pif_ref)? *)
          slave_pif_record.API.pIF_network = master_pif_rec.API.pIF_network)
        slave_tunnel_pifs
    in
    (* If the slave doesn't have any such PIF then make one: *)
    if existing_pif = [] then
      (* On the master, we find the network the tunnel transport PIF is on and its protocol *)
      let network_of_transport_pif_on_master, protocol =
        get_network_of_transport_pif master_pif_rec
      in
      let pifs =
        Db.PIF.get_records_where ~__context
          ~expr:
            (And
               ( Eq (Field "host", Literal (Ref.string_of host))
               , Eq
                   ( Field "network"
                   , Literal (Ref.string_of network_of_transport_pif_on_master)
                   ) ))
      in
      match pifs with
      | [] ->
          (* we have no PIF on which to make the tunnel; do nothing *)
          ()
      | [(pif_ref, _)] ->
          (* this is the PIF on which we want as transport PIF; let's make it *)
          ignore
            (Xapi_tunnel.create_internal ~__context ~transport_PIF:pif_ref
               ~network:master_pif_rec.API.pIF_network ~host ~protocol)
      | _ ->
          (* This should never happen cos we should never have more than one of _our_ pifs
             					 * on the same nework *)
          ()
  in
  (* for each of the master's pifs, create a corresponding one on this host if necessary *)
  List.iter maybe_create_tunnel_for_me master_tunnel_pifs

let sync_pif_currently_attached ~__context ~host ~bridges =
  (* Produce internal lookup tables *)
  let networks = Db.Network.get_all_records ~__context in
  let pifs =
    Db.PIF.get_records_where ~__context
      ~expr:(Eq (Field "host", Literal (Ref.string_of host)))
    |> List.filter (fun (_, pif_rec) ->
           match Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec with
           | VLAN_untagged _ :: Network_sriov_logical _ :: _
           | Network_sriov_logical _ :: _ ->
               false
           | _ ->
               true)
  in
  let network_to_bridge =
    List.map (fun (net, net_r) -> (net, net_r.API.network_bridge)) networks
  in
  (* PIF -> bridge option: None means "dangling PIF" *)
  let pif_to_bridge =
    (* Create a list pairing each PIF with the bridge for the network
       		   that it is on *)
    List.map
      (fun (pif, pif_r) ->
        let net = pif_r.API.pIF_network in
        let bridge =
          if List.mem_assoc net network_to_bridge then
            Some (List.assoc net network_to_bridge)
          else
            None
        in
        (pif, bridge))
      pifs
  in
  (* Perform the database resynchronisation *)
  List.iter
    (fun (pif, pif_r) ->
      let bridge = List.assoc pif pif_to_bridge in
      let currently_attached =
        Option.fold ~none:false ~some:(fun x -> List.mem x bridges) bridge
      in
      if pif_r.API.pIF_currently_attached <> currently_attached then (
        Db.PIF.set_currently_attached ~__context ~self:pif
          ~value:currently_attached ;
        debug "PIF %s currently_attached <- %b" (Ref.string_of pif)
          currently_attached
      ))
    pifs

let migrate_receive ~__context ~host ~network ~options =
  Xapi_vm_migrate.assert_licensed_storage_motion ~__context ;
  let session_id = Context.get_session_id __context in
  let session_rec = Db.Session.get_record ~__context ~self:session_id in
  let new_session_id =
    Xapi_session.login_no_password ~__context ~uname:None ~host
      ~pool:session_rec.API.session_pool
      ~is_local_superuser:session_rec.API.session_is_local_superuser
      ~subject:session_rec.API.session_subject
      ~auth_user_sid:session_rec.API.session_auth_user_sid
      ~auth_user_name:session_rec.API.session_auth_user_name
      ~rbac_permissions:session_rec.API.session_rbac_permissions
  in
  let new_session_id = Ref.string_of new_session_id in
  let pifs = Db.Network.get_PIFs ~__context ~self:network in
  let pif =
    try List.find (fun x -> host = Db.PIF.get_host ~__context ~self:x) pifs
    with Not_found ->
      raise
        (Api_errors.Server_error
           ( Api_errors.host_cannot_attach_network
           , [Ref.string_of host; Ref.string_of network] ))
  in
  let primary_address_type =
    Db.PIF.get_primary_address_type ~__context ~self:pif
  in
  let ip, configuration_mode =
    match primary_address_type with
    | `IPv4 ->
      ( Db.PIF.get_IP ~__context ~self:pif,
      Db.PIF.get_ip_configuration_mode ~__context ~self:pif )
    | `IPv6 ->
      let configuration_mode =
        Db.PIF.get_ipv6_configuration_mode ~__context ~self:pif
      in
      match Db.PIF.get_IPv6 ~__context ~self:pif with
      | [] -> ("", configuration_mode)
      | ip::_ ->
        (* The CIDR is also stored in the IPv6 field of a PIF. *))
        let ipv6 =
          match String.split_on_char '/' ip with
          | hd::_ -> hd
          | _ -> ""
        in
        (ipv6, configuration_mode)
  in
  ( if ip = "" then
    match configuration_mode with
    | `None ->
      raise
        (Api_errors.Server_error
          (Api_errors.pif_has_no_network_configuration, [Ref.string_of pif]))
    | _ ->
      raise
        (Api_errors.Server_error
          (Api_errors.interface_has_no_ip, [Ref.string_of pif]))
  ) ;
  let ip =
    match primary_address_type with
    | `IPv4 -> ip
    | `IPv6 -> Http.Url.maybe_wrap_IPv6_literal ip
  in
  let sm_url =
    Printf.sprintf "http://%s/services/SM?session_id=%s" ip new_session_id
  in
  let xenops_url =
    Printf.sprintf "http://%s/services/xenops?session_id=%s" ip new_session_id
  in
  let master_address =
    try Pool_role.get_master_address ()
    with Pool_role.This_host_is_a_master ->
      Option.get (Helpers.get_management_ip_addr ~__context)
  in
  let master_address =
    match Xapi_stdext_unix.Unixext.domain_of_addr master_address with
    | Some Unix.PF_INET6 ->
      Http.Url.maybe_wrap_IPv6_literal master_address
    | _ ->
      master_address
  in
  let master_url = Printf.sprintf "http://%s/" master_address in
  [
    (Xapi_vm_migrate._sm, sm_url)
  ; (Xapi_vm_migrate._host, Ref.string_of host)
  ; (Xapi_vm_migrate._xenops, xenops_url)
  ; (Xapi_vm_migrate._session_id, new_session_id)
  ; (Xapi_vm_migrate._master, master_url)
  ]

let update_display ~__context ~host ~action =
  let open Xapi_host_display in
  let db_current = Db.Host.get_display ~__context ~self:host in
  let db_new, actual_action =
    match (db_current, action) with
    | `enabled, `enable ->
        (`enabled, None)
    | `disable_on_reboot, `enable ->
        (`enabled, Some `enable)
    | `disabled, `enable ->
        (`enable_on_reboot, Some `enable)
    | `enable_on_reboot, `enable ->
        (`enable_on_reboot, None)
    | `enabled, `disable ->
        (`disable_on_reboot, Some `disable)
    | `disable_on_reboot, `disable ->
        (`disable_on_reboot, None)
    | `disabled, `disable ->
        (`disabled, None)
    | `enable_on_reboot, `disable ->
        (`disabled, Some `disable)
  in
  ( match actual_action with
  | None ->
      ()
  | Some `disable ->
      disable ()
  | Some `enable ->
      enable ()
  ) ;
  if db_new <> db_current then
    Db.Host.set_display ~__context ~self:host ~value:db_new ;
  db_new

let enable_display ~__context ~host =
  update_display ~__context ~host ~action:`enable

let disable_display ~__context ~host =
  if not (Pool_features.is_enabled ~__context Features.Integrated_GPU) then
    raise Api_errors.(Server_error (feature_restricted, [])) ;
  update_display ~__context ~host ~action:`disable

let sync_display ~__context ~host =
  if !Xapi_globs.on_system_boot then (
    let status =
      match Xapi_host_display.status () with
      | `enabled | `unknown ->
          `enabled
      | `disabled ->
          `disabled
    in
    if status = `disabled then
      Xapi_pci.disable_system_display_device () ;
    Db.Host.set_display ~__context ~self:host ~value:status
  )

let apply_guest_agent_config ~__context ~host =
  let pool = Helpers.get_pool ~__context in
  let config = Db.Pool.get_guest_agent_config ~__context ~self:pool in
  Xapi_xenops.apply_guest_agent_config ~__context config

let mxgpu_vf_setup ~__context ~host = Xapi_pgpu.mxgpu_vf_setup __context

let nvidia_vf_setup ~__context ~host ~pf ~enable =
  Xapi_pgpu.nvidia_vf_setup ~__context ~pf ~enable

let allocate_resources_for_vm ~__context ~self ~vm ~live =
  (* Implemented entirely in Message_forwarding *)
  ()

let set_uefi_certificates ~__context ~host ~value =
  Db.Host.set_uefi_certificates ~__context ~self:host ~value ;
  let pool = Helpers.get_pool ~__context in
  if value <> "" then
    Db.Pool.set_uefi_certificates ~__context ~self:pool ~value

let set_iscsi_iqn ~__context ~host ~value =
  if value = "" then
    raise Api_errors.(Server_error (invalid_value, ["value"; value])) ;
  (* Note, the following sequence is carefully written - see the
     other-config watcher thread in xapi_host_helpers.ml *)
  Db.Host.remove_from_other_config ~__context ~self:host ~key:"iscsi_iqn" ;
  (* we need to first set the iscsi_iqn field and then the other-config field:
   * setting the other-config field triggers and update on the iscsi_iqn
   * field if they are different
   *
   * we want to keep the legacy `other_config:iscsi_iqn` and the new `iscsi_iqn`
   * fields in sync:
   * when you update the `iscsi_iqn` field we want to update `other_config`,
   * but when updating `other_config` we want to update `iscsi_iqn` too.
   * we have to be careful not to introduce an infinite loop of updates.
   * *)
  Db.Host.set_iscsi_iqn ~__context ~self:host ~value ;
  Db.Host.add_to_other_config ~__context ~self:host ~key:"iscsi_iqn" ~value ;
  Xapi_host_helpers.Configuration.set_initiator_name value

let set_multipathing ~__context ~host ~value =
  (* Note, the following sequence is carefully written - see the
     other-config watcher thread in xapi_host_helpers.ml *)
  Db.Host.remove_from_other_config ~__context ~self:host ~key:"multipathing" ;
  Db.Host.set_multipathing ~__context ~self:host ~value ;
  Db.Host.add_to_other_config ~__context ~self:host ~key:"multipathing"
    ~value:(string_of_bool value) ;
  Xapi_host_helpers.Configuration.set_multipathing value

let notify_accept_new_pool_secret ~__context ~host ~old_ps ~new_ps =
  Xapi_psr.notify_new ~__context ~old_ps ~new_ps

let notify_send_new_pool_secret ~__context ~host ~old_ps ~new_ps =
  Xapi_psr.notify_send ~__context ~old_ps ~new_ps

let cleanup_pool_secret ~__context ~host ~old_ps ~new_ps =
  Xapi_psr.cleanup ~__context ~old_ps ~new_ps

let set_sched_gran ~__context ~self ~value =
  if Helpers.get_localhost ~__context <> self then
    failwith "Forwarded to the wrong host" ;
  if not !Xapi_globs.allow_host_sched_gran_modification then
    raise
      Api_errors.(
        Server_error (operation_not_allowed, ["Disabled by xapi.conf"])) ;
  let arg =
    Printf.sprintf "sched-gran=%s" (Record_util.host_sched_gran_to_string value)
  in
  let args = ["--set-xen"; arg] in
  try
    let _ = Helpers.call_script !Xapi_globs.xen_cmdline_script args in
    ()
  with e ->
    error "Failed to update sched-gran: %s" (Printexc.to_string e) ;
    raise
      Api_errors.(
        Server_error (internal_error, ["Failed to update sched-gran"]))

let get_sched_gran ~__context ~self =
  if Helpers.get_localhost ~__context <> self then
    failwith "Forwarded to the wrong host" ;
  let args = ["--get-xen"; "sched-gran"] in
  try
    let ret =
      String.trim (Helpers.call_script !Xapi_globs.xen_cmdline_script args)
    in
    match ret with
    | "" ->
        `cpu (* If no entry then default value: cpu *)
    | _ ->
        let value = List.nth (String.split_on_char '=' ret) 1 in
        Record_util.host_sched_gran_of_string value
  with e ->
    error "Failed to get sched-gran: %s" (Printexc.to_string e) ;
    raise
      Api_errors.(Server_error (internal_error, ["Failed to get sched-gran"]))
