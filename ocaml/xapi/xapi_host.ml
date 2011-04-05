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
open Pervasiveext
open Stringext
open Listext
open Threadext
open Xapi_host_helpers
open Xapi_support
open Db_filter_types
open Create_misc
open Workload_balancing

module D = Debug.Debugger(struct let name="xapi" end)
open D

let host_bugreport_upload = "/opt/xensource/libexec/host-bugreport-upload"

let set_emergency_mode_error code params = Xapi_globs.emergency_mode_error := Api_errors.Server_error(code, params)

let local_assert_healthy ~__context = match Pool_role.get_role () with
  | Pool_role.Master -> ()
  | Pool_role.Broken -> raise !Xapi_globs.emergency_mode_error 
  | Pool_role.Slave _ -> if !Xapi_globs.slave_emergency_mode then raise !Xapi_globs.emergency_mode_error
  
let set_power_on_mode ~__context ~self ~power_on_mode ~power_on_config =
  Db.Host.set_power_on_mode ~__context ~self ~value:power_on_mode;
  let current_config=Db.Host.get_power_on_config ~__context ~self in
  Db.Host.set_power_on_config ~__context ~self ~value:power_on_config;
  Xapi_secret.clean_out_passwds ~__context current_config;
  Xapi_host_helpers.update_allowed_operations ~__context ~self
    
(** Before we re-enable this host we make sure it's safe to do so. It isn't if:
    + we're in the middle of an HA shutdown/reboot and have our fencing temporarily disabled. 
    + HA is enabled and this host has broken storage or networking which would cause protected VMs
    to become non-agile
    + our license doesn't support pooling and we're a slave
 *)
let assert_safe_to_reenable ~__context ~self =
    let host_disabled_until_reboot = try bool_of_string (Localdb.get Constants.host_disabled_until_reboot) with _ -> false in
    if host_disabled_until_reboot
    then raise (Api_errors.Server_error(Api_errors.host_disabled_until_reboot, []));
    if Db.Pool.get_ha_enabled ~__context ~self:(Helpers.get_pool ~__context) then begin
      let pbds = Db.Host.get_PBDs ~__context ~self in
      let unplugged_pbds = List.filter (fun pbd -> not(Db.PBD.get_currently_attached ~__context ~self:pbd)) pbds in
      (* Make sure it is 'ok' to have these PBDs remain unplugged *)
      List.iter (fun self -> Xapi_pbd.abort_if_storage_attached_to_protected_vms ~__context ~self) unplugged_pbds;
      let pifs = Db.Host.get_PIFs ~__context ~self in
      let unplugged_pifs = List.filter (fun pif -> not(Db.PIF.get_currently_attached ~__context ~self:pif)) pifs in
      (* Make sure it is 'ok' to have these PIFs remain unplugged *)
      List.iter (fun self -> Xapi_pif.abort_if_network_attached_to_protected_vms ~__context ~self) unplugged_pifs;
      (* Make sure our license hasn't expired (an exception is raised is it is) *)
      License_check.check_expiry ~__context ~host:self
    end

let xen_bugtool = "/usr/sbin/xen-bugtool"

let bugreport_upload ~__context ~host ~url ~options = 
  let proxy = 
    if List.mem_assoc "http_proxy" options
    then List.assoc "http_proxy" options
    else try Unix.getenv "http_proxy" with _ -> "" in

  let cmd = Printf.sprintf "%s %s %s" host_bugreport_upload url proxy in
  try
    let stdout, stderr = Forkhelpers.execute_command_get_output host_bugreport_upload [ url; proxy ] in
    debug "%s succeeded with stdout=[%s] stderr=[%s]" cmd stdout stderr
  with Forkhelpers.Spawn_internal_error(stderr, stdout, status) as e ->
    debug "%s failed with stdout=[%s] stderr=[%s]" cmd stdout stderr;
    (* Attempt to interpret curl's exit code (from curl(1)) *)
    begin match status with
    | Unix.WEXITED (1 | 3 | 4) ->
	failwith "URL not recognised"
    | Unix.WEXITED (5 | 6) ->
	failwith "Failed to resolve proxy or host"
    | Unix.WEXITED 7 ->
	failwith "Failed to connect to host"
    | Unix.WEXITED 9 ->
	failwith "FTP access denied"
    | _ -> raise e
    end

(** Check that a) there are no running VMs present on the host, b) there are no VBDs currently 
    attached to dom0, c) host is disabled.

	This is approximately maintainance mode as defined by the gui. However, since 
	we haven't agreed on an exact definition of this mode, we'll not call this maintainance mode here, but we'll
	use a synonym. According to http://thesaurus.com/browse/maintenance, bacon is a synonym 
	for maintainance, hence the name of the following function.
*)
let assert_bacon_mode ~__context ~host =
	if Db.Host.get_enabled ~__context ~self:host 
	then raise (Api_errors.Server_error (Api_errors.host_not_disabled, []));

	let selfref = Ref.string_of host in
	let vms = Db.VM.get_refs_where ~__context ~expr:(And(Eq (Field "resident_on", Literal (Ref.string_of host)),
									Eq (Field "power_state", Literal "Running"))) in
	(* We always expect a control domain to be resident on a host *)
	(match List.filter (fun vm -> not (Db.VM.get_is_control_domain ~__context ~self:vm)) vms with
	| [] -> ()
	| guest_vms -> 
		let vm_data = [selfref; "vm"; Ref.string_of (List.hd guest_vms)] in
		raise (Api_errors.Server_error (Api_errors.host_in_use, vm_data)));
	debug "Bacon test: VMs OK - %d running VMs" (List.length vms);
	let controldomain = List.find (fun vm -> Db.VM.get_resident_on ~__context ~self:vm = host &&
			Db.VM.get_is_control_domain ~__context ~self:vm) (Db.VM.get_all ~__context) in  
	let vbds = List.filter (fun vbd -> Db.VBD.get_VM ~__context ~self:vbd = controldomain &&
			Db.VBD.get_currently_attached ~__context ~self:vbd) (Db.VBD.get_all ~__context) in
	if List.length vbds > 0 then
		raise (Api_errors.Server_error (Api_errors.host_in_use, [ selfref; "vbd"; List.hd (List.map Ref.string_of vbds) ]));
	debug "Bacon test: VBDs OK"

let pif_update_dhcp_address ~__context ~self =
  let network = Db.PIF.get_network ~__context ~self in
  let bridge = Db.Network.get_bridge ~__context ~self:network in
  match Netdev.Addr.get bridge with
   | (_addr,_netmask)::_ ->
       let addr = (Unix.string_of_inet_addr _addr) in
       let netmask = (Unix.string_of_inet_addr _netmask) in
       if addr <> Db.PIF.get_IP ~__context ~self || netmask <> Db.PIF.get_netmask ~__context ~self then begin
	 debug "PIF %s bridge %s IP address changed: %s/%s" (Db.PIF.get_uuid ~__context ~self) bridge addr netmask;
	 Db.PIF.set_IP ~__context ~self ~value:addr;
	 Db.PIF.set_netmask ~__context ~self ~value:netmask    
       end
   | _ -> ()

let signal_networking_change ~__context =
  let host = Helpers.get_localhost ~__context in
  let pifs = Db.Host.get_PIFs ~__context ~self:host in
  List.iter (fun pif -> if Db.PIF.get_ip_configuration_mode ~__context ~self:pif = `DHCP then
	       pif_update_dhcp_address ~__context ~self:pif
  ) pifs;
  Xapi_mgmt_iface.on_dom0_networking_change ~__context


let signal_cdrom_event ~__context params =
  let find_vdi_name sr name =
    let ret = ref None in
    let vdis = Db.SR.get_VDIs ~__context ~self:sr in
    List.iter (fun vdi -> 
      if Db.VDI.get_location ~__context ~self:vdi = name then ret := Some vdi
    ) vdis;
    !ret
    in
  let find_vdis name =
    let srs = List.filter (fun sr ->
      let ty = Db.SR.get_type ~__context ~self:sr in ty = "local" || ty = "udev"
    ) (Db.SR.get_all ~__context) in
    List.fold_left (fun acc o -> match o with Some x -> x :: acc | None -> acc) []
                   (List.map (fun sr -> find_vdi_name sr name) srs)
    in
  let insert dev =
    let vdis = find_vdis dev in
    if List.length vdis = 1 then (
      let vdi = List.hd vdis in
      debug "cdrom inserted notification in vdi %s" (Ref.string_of vdi);
      let vbds = Db.VDI.get_VBDs ~__context ~self:vdi in
      List.iter (fun vbd -> Xapi_vbd.refresh ~__context ~vbd ~vdi) vbds
    ) else
      ()
    in
  try
    match String.split ':' params with
      | ["inserted";dev] -> insert dev
      | "ejected"::_  -> ()
      | _          -> ()
  with _ ->
    ()

let notify ~__context ~ty ~params =
  match ty with
  | "cdrom" -> signal_cdrom_event ~__context params
  | _       -> ()

let rotate = function
  | [] -> []
  | x::xs -> xs@[x]

(* A host evacuation plan consists of a hashtable mapping VM refs to instances of per_vm_plan: *)
type per_vm_plan = 
  | Migrate of API.ref_host
  | Error of (string * string list)

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
  let enabled_hosts = List.filter (fun self -> Db.Host.get_enabled ~__context ~self) all_hosts in
  (* Only consider migrating to other enabled hosts (not this one obviously) *)
  let target_hosts = List.filter (fun self -> self <> host) enabled_hosts in

  (* CA-24672: During a rolling upgrade, after the master has been upgraded, we
   * are not permitted to migrate to a slave which still uses the old version.
   * Hence only hosts with the same version as the master can be considered as
   * potential targets. *)
  let master_version = Version.product_version in
  debug "master version: %s" master_version;
  let target_hosts = List.filter
    (fun h ->
      let host_software_version_map = Db.Host.get_software_version ~__context ~self:h in
      let host_product_version = List.assoc "product_version" host_software_version_map in
      debug "host %s version: %s" (Db.Host.get_hostname ~__context ~self:h) host_product_version;
      (* We merely check for string equality since inequality implies that the host must be older, since the master is guaranteed to be at least as new as all other nodes in the pool. *)
      master_version = host_product_version)
    target_hosts
  in
  debug "target hosts are [%s]" (String.concat "; " (List.map (fun h -> Db.Host.get_hostname ~__context ~self:h) target_hosts));

  let all_vms = Db.Host.get_resident_VMs ~__context ~self:host in
  let all_vms = List.map (fun self -> self, Db.VM.get_record ~__context ~self) all_vms in
  let all_user_vms = List.filter (fun (_, record) -> not record.API.vM_is_control_domain) all_vms in

  let plans = Hashtbl.create 10 in
  
   
  if target_hosts = [] 
  then 
       begin
      List.iter (fun (vm, _) -> Hashtbl.add plans vm (Error (Api_errors.host_not_enough_free_memory, [ Ref.string_of vm ]))) all_user_vms;
      plans
    end
  else 
    begin
      
  (* If HA is enabled we require that non-protected VMs are suspended. This gives us the property that
     the result obtained by executing the evacuation plan and disabling the host looks the same (from the HA 
     planner's PoV) to the result obtained following a host failure and VM restart. *)
  let pool = Helpers.get_pool ~__context in
  let protected_vms, unprotected_vms = 
    if Db.Pool.get_ha_enabled ~__context ~self:pool
    then List.partition (fun (vm, record) -> Helpers.vm_should_always_run record.API.vM_power_state record.API.vM_ha_restart_priority) all_user_vms
    else all_user_vms, [] in
  List.iter (fun (vm, _) -> Hashtbl.replace plans vm (Error (Api_errors.host_not_enough_free_memory, [ Ref.string_of vm ]))) unprotected_vms;
  let migratable_vms, unmigratable_vms = List.partition (fun (vm, record) ->
    begin
      try 
	List.iter (fun host -> Xapi_vm_helpers.assert_can_boot_here_no_memcheck ~__context ~self:vm ~host ~snapshot:record) target_hosts;
	true
      with (Api_errors.Server_error (code, params)) -> Hashtbl.replace plans vm (Error (code, params)); false
    end) protected_vms in
  (* Check the PV drivers versions are up to date (so migration is possible). Note that we are more strict
     than necessary by demanding the most recent PV drivers are installed and not just 'migration capable' ones.
     This simplifies the test matrix. *)
  List.iter (fun (vm, record) ->
	       let vm_gm = record.API.vM_guest_metrics in
	       let vm_gmr = try Some (Db.VM_guest_metrics.get_record_internal ~__context ~self:vm_gm) with _ -> None in  
	       match Xapi_pv_driver_version.make_error_opt (Xapi_pv_driver_version.of_guest_metrics vm_gmr) vm vm_gm with
	       | Some (code, params) -> Hashtbl.replace plans vm (Error (code, params))
	       | None -> ()) all_user_vms;

  (* Compute the binpack which takes only memory size into account. We will check afterwards for storage
     and network availability. *)
  let plan = Xapi_ha_vm_failover.compute_evacuation_plan ~__context (List.length all_hosts) target_hosts migratable_vms in
  (* Check if the plan was actually complete: if some VMs are missing it means there wasn't enough memory *)
  let vms_handled = List.map fst plan in
  let vms_missing = List.filter (fun x -> not(List.mem x vms_handled)) (List.map fst protected_vms) in
  List.iter (fun vm -> Hashtbl.add plans vm (Error (Api_errors.host_not_enough_free_memory, [ Ref.string_of vm ]))) vms_missing;

  (* Now for each VM we did place, verify storage and network visibility. *)
  List.iter (fun (vm, host) ->
	       let snapshot = List.assoc vm all_vms in
	       begin
		 try Xapi_vm_helpers.assert_can_boot_here_no_memcheck ~__context ~self:vm ~host ~snapshot
		 with (Api_errors.Server_error (code, params)) -> Hashtbl.replace plans vm (Error (code, params))
	       end;
	       if not(Hashtbl.mem plans vm) then Hashtbl.add plans vm (Migrate host)
	    ) plan;
  plans
 end
  
(* Old Miami style function with the strange error encoding *)
let assert_can_evacuate ~__context ~host =
  (* call no_wlb function as we only care about errors, and wlb only provides recs for moveable vms *) 
  let plans = compute_evacuation_plan_no_wlb ~__context ~host in
  let errors = Hashtbl.fold (fun vm plan acc -> match plan with Error(code, params) -> String.concat "," (code::params) :: acc | _ -> acc) plans [] in
  if errors <> []
  then raise (Api_errors.Server_error (Api_errors.cannot_evacuate_host, [ String.concat "|" errors ]))

(* New Orlando style function which returns a Map *)
let get_vms_which_prevent_evacuation ~__context ~self = 
  let plans = compute_evacuation_plan_no_wlb ~__context ~host:self in
  Hashtbl.fold (fun vm plan acc -> match plan with Error(code, params) -> (vm, (code :: params)) :: acc | _ -> acc) plans []

let compute_evacuation_plan_wlb ~__context ~self =
  (* We treat xapi as primary when it comes to "hard" errors, i.e. those that aren't down to memory constraints.  These are things like
     VM_REQUIRES_SR or VM_MISSING_PV_DRIVERS.

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
  let vm_recoms = get_evacuation_recoms ~__context ~uuid:(Db.Host.get_uuid ~__context ~self) in
  let recs = Hashtbl.create 31 in

  List.iter (fun (v, detail) ->
    debug "WLB recommends VM evacuation: %s to %s" (Db.VM.get_name_label ~__context ~self:v) (String.concat "," detail);

    (* Sanity check 
    Note: if the vm being moved is dom0 then this is a power management rec and this check does not apply
    *)
    let resident_h = (Db.VM.get_resident_on ~__context ~self:v) in
    let target_uuid = List.hd (List.tl detail) in
    if get_dom0_vm ~__context target_uuid != v &&  Db.Host.get_uuid ~__context ~self:resident_h = target_uuid
    then
      (* resident host and migration host are the same. Reject this plan *)
      raise (Api_errors.Server_error 
	       (Api_errors.wlb_malformed_response, 
		[Printf.sprintf "WLB recommends migrating VM %s to the same server it is being evacuated from." 
		   (Db.VM.get_name_label ~__context ~self:v)]));

    match detail with
      | ["WLB"; host_uuid; _] ->
          Hashtbl.replace recs v (Migrate (Db.Host.get_by_uuid ~__context ~uuid:host_uuid))
      | _ ->
          raise (Api_errors.Server_error 
	           (Api_errors.wlb_malformed_response, ["WLB gave malformed details for VM evacuation."]))) vm_recoms;

  Hashtbl.iter (fun v detail ->
    match detail with
      | (Migrate _) ->
          (* Skip migrations -- WLB is providing these *)
          ()
      | (Error (e, _)) when e = Api_errors.host_not_enough_free_memory ->
          (* Skip errors down to free memory -- we're letting WLB decide this *)
          ()
      | (Error _) as p ->
          debug "VM preventing evacuation: %s because %s" (Db.VM.get_name_label ~__context ~self:v) (string_of_per_vm_plan p);
          Hashtbl.replace recs v detail) error_vms;

  let resident_vms =
    List.filter (fun v -> (not (Db.VM.get_is_control_domain ~__context ~self:v)) && (not (Db.VM.get_is_a_template ~__context ~self:v)))
    (Db.Host.get_resident_VMs ~__context ~self) in
  List.iter (fun vm ->
    if not (Hashtbl.mem recs vm) then
      (* Anything for which we don't have a recommendation from WLB, but which is agile, we treat as "not enough memory" *)
      Hashtbl.replace recs vm (Error (Api_errors.host_not_enough_free_memory, [Ref.string_of vm]))) resident_vms;

  Hashtbl.iter (fun vm detail ->
    debug "compute_evacuation_plan_wlb: Key: %s Value %s" (Db.VM.get_name_label ~__context ~self:vm) (string_of_per_vm_plan detail)) recs;
  recs

let compute_evacuation_plan ~__context ~host = 
  let oc = Db.Pool.get_other_config ~__context ~self:(Helpers.get_pool ~__context) in
  if ((List.exists (fun (k,v) -> k = "wlb_choose_host_disable" && (String.lowercase v = "true")) oc)
    || not (Workload_balancing.check_wlb_enabled ~__context))
  then
    begin
      debug "Using wlb recommendations for choosing a host has been disabled or wlb is not available. Using original algorithm";
      compute_evacuation_plan_no_wlb ~__context ~host 
    end
  else
    try
      debug "Using WLB recommendations for host evacuation.";
      compute_evacuation_plan_wlb ~__context ~self:host
    with
    | Api_errors.Server_error(error_type, error_detail) -> 
        debug "Encountered error when using wlb for choosing host \"%s: %s\". Using original algorithm" error_type (String.concat "" error_detail);
        (try
          let uuid = Db.Host.get_uuid ~__context ~self:host in
          let message_body = 
            Printf.sprintf "Wlb consultation for Host '%s' failed (pool uuid: %s)" 
            (Db.Host.get_name_label ~__context ~self:host)
            (Db.Pool.get_uuid ~__context ~self:(Helpers.get_pool ~__context))
          in
          ignore(Xapi_message.create ~__context ~name:Api_messages.wlb_failed ~priority:3L ~cls:`Host ~obj_uuid:uuid ~body:message_body) 
        with _ -> ());
      compute_evacuation_plan_no_wlb ~__context ~host 
    | _ ->
        debug "Encountered an unknown error when using wlb for choosing host. Using original algorithm";
        compute_evacuation_plan_no_wlb ~__context ~host 

let evacuate ~__context ~host =
  let task = Context.get_task_id __context in
    begin
      let plans = compute_evacuation_plan ~__context ~host in
	    (* Check there are no errors in this list *)
	    Hashtbl.iter (fun vm plan -> match plan with Error(code, params) -> raise (Api_errors.Server_error(code, params)) | _ -> ()) plans;
        
        (* Do it *)
        let individual_progress = 1.0 /. float (Hashtbl.length plans) in
        let migrate_vm  vm plan = match plan with
	      | Migrate host ->
		      Helpers.call_api_functions ~__context
                (fun rpc session_id -> Client.Client.VM.pool_migrate ~rpc ~session_id ~vm ~host ~options:[ "live", "true" ]);
		      let progress = Db.Task.get_progress ~__context ~self:task in
                TaskHelper.set_progress ~__context (progress +. individual_progress)
	      | Error(code, params) -> (* should never happen *) raise (Api_errors.Server_error(code, params))
        in
        let () = Hashtbl.iter migrate_vm plans in
          
        (* Now check there are no VMs left *)
        let vms = Db.Host.get_resident_VMs ~__context ~self:host in
        let vms = 
          List.filter
            (fun vm ->
               not (Db.VM.get_is_control_domain ~__context ~self:vm)
            )
            vms
        in
          assert (List.length vms = 0)
    end

let retrieve_wlb_evacuate_recommendations ~__context ~self =
  let plans = compute_evacuation_plan_wlb ~__context ~self in
  Hashtbl.fold
    (fun vm detail acc ->
       let plan = match detail with
         | Error (e, t) ->
             e :: t
         | Migrate h ->
             ["WLB"; (Db.Host.get_uuid ~__context ~self:h)]
       in
       (vm, plan) :: acc) plans []

let restart_agent ~__context ~host =
  debug "Host.restart_agent: Host agent will restart in 10s!!!!";
  Xapi_fuse.light_fuse_and_run()

let shutdown_agent ~__context =
  debug "Host.restart_agent: Host agent will shutdown in 1s!!!!";
  Xapi_fuse.light_fuse_and_dont_restart ~fuse_length:1 ()

let disable  ~__context ~host =
  if Db.Host.get_enabled ~__context ~self:host then begin
    info "Host.enabled: setting host %s (%s) to disabled because of user request" (Ref.string_of host) (Db.Host.get_hostname ~__context ~self:host);
    Db.Host.set_enabled ~__context ~self:host ~value:false;
    Helpers.user_requested_host_disable := true
  end

let enable  ~__context ~host =
  if not (Db.Host.get_enabled ~__context ~self:host) then begin
    assert_safe_to_reenable ~__context ~self:host;
    Helpers.user_requested_host_disable := false;
    info "Host.enabled: setting host %s (%s) to enabled because of user request" (Ref.string_of host) (Db.Host.get_hostname ~__context ~self:host);
    Db.Host.set_enabled ~__context ~self:host ~value:true;
    (* Normally we schedule a plan recomputation when we successfully plug in our storage. In the case
       when some of our storage was broken and required maintenance, we end up here, manually re-enabling 
       the host. If we're overcommitted then this might fix the problem. *)
    let pool = Helpers.get_pool ~__context in
    if Db.Pool.get_ha_enabled ~__context ~self:pool && Db.Pool.get_ha_overcommitted ~__context ~self:pool
    then Helpers.call_api_functions ~__context (fun rpc session_id -> Client.Client.Pool.ha_schedule_plan_recomputation rpc session_id)
  end

let shutdown_and_reboot_common ~__context ~host label description operation cmd = 
  assert_bacon_mode ~__context ~host;
  Xapi_ha.before_clean_shutdown_or_reboot ~__context ~host;
  Remote_requests.stop_request_thread();

  (* Push the Host RRD to the master. Note there are no VMs running here so we don't have to worry about them. *)
  if not(Pool_role.is_master ())
  then Monitor_rrds.send_host_rrd_to_master ();
  (* Also save the Host RRD to local disk for us to pick up when we return. Note there are no VMs running at this point. *)
  Monitor_rrds.backup ();

  (* This prevents anyone actually re-enabling us until after reboot *)
  Localdb.put Constants.host_disabled_until_reboot "true";
  (* This helps us distinguish between an HA fence and a reboot *)
  Localdb.put Constants.host_restarted_cleanly "true";
  (* This tells the master that the shutdown is still ongoing: it can be used to continue
     masking other operations even after this call return.

     If xapi restarts then this task will be reset by the startup code, which is unfortunate
     but the host will stay disabled provided host_disabled_until_reboot is still set... so
     safe but ugly. *)
  Server_helpers.exec_with_new_task ~subtask_of:(Context.get_task_id __context) ~task_description:description ~task_in_database:true label (fun __newcontext ->
  Db.Host.add_to_current_operations ~__context ~self:host ~key:(Ref.string_of (Context.get_task_id __newcontext)) ~value:operation;
  (* Do the shutdown in a background thread with a delay to give this API call
     a reasonable chance of succeeding. *)
  ignore(Thread.create (fun () ->
			  Thread.delay 10.;
			  ignore(Sys.command cmd)) ()))

let shutdown ~__context ~host = 
  shutdown_and_reboot_common ~__context ~host "Host is shutting down" "Host is shutting down" 
    `shutdown "/sbin/shutdown -h now"

let reboot ~__context ~host = 
  shutdown_and_reboot_common ~__context ~host "Host is rebooting" "Host is rebooting" 
    `shutdown "/sbin/shutdown -r now"

let power_on ~__context ~host = 
  let result = Xapi_plugins.call_plugin (Context.get_session_id __context) 
    Constants.power_on_plugin Constants.power_on_fn 
    [ "remote_host_uuid", Db.Host.get_uuid ~__context ~self:host ] in
  if result <> "True" then failwith (Printf.sprintf "The host failed to power on.")

let dmesg ~__context ~host =
	Vmopshelpers.with_xc (fun xc -> Xc.readconsolering xc)

let dmesg_clear ~__context ~host =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "dmesg_clear" ]))

let get_log ~__context ~host =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "get_log" ]))
    
let send_debug_keys ~__context ~host ~keys =
  Vmopshelpers.with_xc (fun xc -> Xc.send_debug_keys xc keys)

let list_methods ~__context =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "list_method" ]))

let is_slave ~__context ~host = not (Pool_role.is_master ())

let ask_host_if_it_is_a_slave ~__context ~host = 
  let local_fn = is_slave ~host in
  Message_forwarding.do_op_on_localsession_nolivecheck ~local_fn ~__context
    ~host (fun session_id rpc -> Client.Client.Pool.is_slave rpc session_id host)

let is_host_alive ~__context ~host = 
  (* If the host is marked as not-live then assume we don't need to contact it to verify *)
  let should_contact_host = 
    try
      let hm = Db.Host.get_metrics ~__context ~self:host in
      Db.Host_metrics.get_live ~__context ~self:hm
    with _ ->
      true in
  if should_contact_host then begin
    debug "is_host_alive host=%s is marked as live in the database; asking host to make sure" (Ref.string_of host);
    try
      ignore(ask_host_if_it_is_a_slave ~__context ~host);
      true
    with e ->
      warn "is_host_alive host=%s caught %s while querying host liveness: assuming dead" 
	(Ref.string_of host) (ExnHelper.string_of_exn e);
      false
  end else begin
    debug "is_host_alive host=%s is marked as dead in the database; treating this as definitive." (Ref.string_of host);
    false
  end

let create ~__context ~uuid ~name_label ~name_description ~hostname ~address ~external_auth_type ~external_auth_service_name ~external_auth_configuration ~license_params ~edition ~license_server ~local_cache_sr =

  let existing_host = try Some (Db.Host.get_by_uuid __context uuid) with _ -> None in
  let make_new_metrics_object ref =
    Db.Host_metrics.create ~__context ~ref
      ~uuid:(Uuid.to_string (Uuid.make_uuid ())) ~live:false
      ~memory_total:0L ~memory_free:0L ~last_updated:Date.never ~other_config:[] in
  let xapi_verstring = get_xapi_verstring() in
  let name_description = "Default install of XenServer"
  and host = Ref.make () in
  
  let metrics = Ref.make () in
  make_new_metrics_object metrics;
  
  Db.Host.create ~__context ~ref:host 
    ~current_operations:[] ~allowed_operations:[]
    ~software_version:Xapi_globs.software_version
    ~enabled:true
    ~aPI_version_major:Xapi_globs.api_version_major
    ~aPI_version_minor:Xapi_globs.api_version_minor
    ~aPI_version_vendor:Xapi_globs.api_version_vendor
    ~aPI_version_vendor_implementation:Xapi_globs.api_version_vendor_implementation
    ~name_description ~name_label ~uuid ~other_config:[]
    ~capabilities:[]
    ~cpu_configuration:[]   (* !!! FIXME hard coding *)
    ~cpu_info:[]
	~chipset_info:[]
    ~memory_overhead:0L
    ~sched_policy:"credit"  (* !!! FIXME hard coding *)
    ~supported_bootloaders:(List.map fst Xapi_globs.supported_bootloaders)
    ~suspend_image_sr:Ref.null ~crash_dump_sr:Ref.null
    ~logging:[] ~hostname ~address ~metrics
    ~license_params ~boot_free_mem:0L
    ~ha_statefiles:[] ~ha_network_peers:[] ~blobs:[] ~tags:[]
    ~external_auth_type
    ~external_auth_service_name
    ~external_auth_configuration
    ~edition ~license_server
    ~bios_strings:[]
    ~power_on_mode:""
    ~power_on_config:[]
    ~local_cache_sr
  ;
  (* If the host we're creating is us, make sure its set to live *)
  Db.Host_metrics.set_last_updated ~__context ~self:metrics ~value:(Date.of_float (Unix.gettimeofday ()));
  Db.Host_metrics.set_live ~__context ~self:metrics ~value:(uuid=(Helpers.get_localhost_uuid ()));
  host

let destroy ~__context ~self =
  (* Fail if the host is still online: the user should either isolate the machine from the network 
     or use Pool.eject. *)
  let hostname = Db.Host.get_hostname ~__context ~self in
  if is_host_alive ~__context ~host:self then begin
    error "Host.destroy successfully contacted host %s; host is not offline; refusing to destroy record" hostname;
    raise (Api_errors.Server_error(Api_errors.host_is_live, [ Ref.string_of self ]))
  end;

  (* This check is probably redundant since the Pool master should always be 'alive': *)
  (* It doesn't make any sense to destroy the master's own record *)
  let me = Helpers.get_localhost ~__context in
  if self=me then raise (Api_errors.Server_error(Api_errors.host_cannot_destroy_self, [ Ref.string_of self ]));

  (* CA-23732: Block if HA is enabled *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool 
  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

  let my_resident_vms = Db.Host.get_resident_VMs ~__context ~self in
  let my_control_domains, my_regular_vms = List.partition (fun vm -> Db.VM.get_is_control_domain ~__context ~self:vm) my_resident_vms in
  
  if List.length my_regular_vms > 0
  then raise (Api_errors.Server_error(Api_errors.host_has_resident_vms, [ Ref.string_of self ]));

  Db.Host.destroy ~__context ~self;
  List.iter (fun vm -> Db.VM.destroy ~__context ~self:vm) my_control_domains

let ha_disable_failover_decisions ~__context ~host = Xapi_ha.ha_disable_failover_decisions __context host
let ha_disarm_fencing ~__context ~host = Xapi_ha.ha_disarm_fencing __context host
let ha_stop_daemon ~__context ~host = Xapi_ha.ha_stop_daemon __context host
let ha_release_resources ~__context ~host = Xapi_ha.ha_release_resources __context host
let ha_wait_for_shutdown_via_statefile ~__context ~host = Xapi_ha.ha_wait_for_shutdown_via_statefile __context host
let ha_xapi_healthcheck ~__context = 
  (* Consider checking the status of various internal tasks / tickling locks but for now assume 
     that, since we got here unharmed, all is well.*)
  not(Xapi_fist.fail_healthcheck ())

let preconfigure_ha ~__context ~host ~statefiles ~metadata_vdi ~generation = 
    Xapi_ha.preconfigure_host __context host statefiles metadata_vdi generation

let ha_join_liveset ~__context ~host =
  try
    Xapi_ha.join_liveset __context host
  with 
  | Xapi_ha.Xha_error Xha_errno.Mtc_exit_bootjoin_timeout ->
      error "HA enable failed with BOOTJOIN_TIMEOUT";
      raise (Api_errors.Server_error(Api_errors.ha_failed_to_form_liveset, []))
  | Xapi_ha.Xha_error Xha_errno.Mtc_exit_can_not_access_statefile ->
      error "HA enable failed with CAN_NOT_ACCESS_STATEFILE";
      raise (Api_errors.Server_error(Api_errors.ha_host_cannot_access_statefile, []))

let propose_new_master ~__context ~address ~manual = Xapi_ha.propose_new_master __context address manual
let commit_new_master ~__context ~address = Xapi_ha.commit_new_master __context address
let abort_new_master ~__context ~address = Xapi_ha.abort_new_master __context address

let update_master ~__context ~host ~master_address = assert false

let emergency_ha_disable ~__context = Xapi_ha.emergency_ha_disable __context

(* This call can be used to _instruct_ a slave that it has to take a persistent backup (force=true).
   If force=false then this is a hint from the master that the client may want to take a backup; in this
   latter case the slave applies its write-limiting policy and compares generation counts to determine whether
   it really should take a backup *)
   
let request_backup ~__context ~host ~generation ~force = 
  if Helpers.get_localhost ~__context <> host
  then failwith "Forwarded to the wrong host";
  let master_address = Helpers.get_main_ip_address () in
  Pool_db_backup.fetch_database_backup ~master_address:master_address ~pool_secret:!Xapi_globs.pool_secret
    ~force:(if force then None else (Some generation))

(* request_config_file_sync is used to inform a slave that it should consider resyncing dom0 config files
   (currently only /etc/passwd) *)
let request_config_file_sync ~__context ~host ~hash =
  debug "Received notification of dom0 config file change";
  let master_address = Helpers.get_main_ip_address () in
  Config_file_sync.maybe_fetch_config_files ~master_address:master_address ~pool_secret:!Xapi_globs.pool_secret ~hash

let syslog_config_write host host_only enable_remote =
	(* write the new configuration file *)
	let buf = (if host <> "" then
		     Printf.sprintf "SYSLOG_HOST=\"%s\"\nSYSLOG_REMOTE_ONLY=\"%s\"\n"
		       host (if host_only then "1" else "0")
		   else "") ^
	          (if enable_remote then "SYSLOGD_OPTIONS=\"-r $SYSLOGD_OPTIONS\"\n" else "")
		  in
	let fd = Unix.openfile "/etc/xensource/syslog.conf"
	                       [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ]
			       0o640 in
	ignore (Unix.write fd buf 0 (String.length buf));
	Unix.close fd;
	(* restart the service *)
	let args = [| "/etc/init.d/syslog"; "restart" |] in
	ignore (Unixext.spawnvp args.(0) args);
	()

(* Host parameter will just be me, as message forwarding layer ensures this call has been forwarded correctly *)
let syslog_reconfigure ~__context ~host =
	let localhost = Helpers.get_localhost ~__context in
	let logging = Db.Host.get_logging ~__context ~self:localhost in

	let destination = try List.assoc "syslog_destination" logging with _ -> "" in
	let destination_only = try bool_of_string (List.assoc "syslog_destination_only" logging) with _ -> false in
	let listen_remote = try bool_of_string (List.assoc "syslog_listen_remote" logging) with _ -> false in

	syslog_config_write destination destination_only listen_remote

let get_management_interface ~__context ~host =
	let pifs = Db.PIF.get_all_records ~__context in
	let management_pif, _ = List.find (fun (_, pif) -> pif.API.pIF_management && pif.API.pIF_host = host) pifs in
	management_pif

let change_management_interface ~__context interface = 
    debug "Changing management interface";
    let addrs = Netdev.Addr.get interface in
    if addrs = []
    then raise (Api_errors.Server_error(Api_errors.interface_has_no_ip, [ interface ]));
    Xapi_mgmt_iface.change_ip interface (Unix.string_of_inet_addr (fst (List.hd addrs)));
    (* once the inventory file has been rewritten to specify new interface, sync up db with
       state of world.. *)
    Xapi_mgmt_iface.on_dom0_networking_change ~__context

let local_management_reconfigure ~__context ~interface =
  (* Only let this one through if we are in emergency mode, otherwise use 
     Host.management_reconfigure *)
  if not !Xapi_globs.slave_emergency_mode 
  then raise (Api_errors.Server_error (Api_errors.pool_not_in_emergency_mode, []));
  change_management_interface ~__context interface

let management_reconfigure ~__context ~pif = 
  (* Disallow if HA is enabled *)
  let pool = List.hd (Db.Pool.get_all ~__context) in
  if Db.Pool.get_ha_enabled ~__context ~self:pool 
  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

  (* Plugging a bond slave is not allowed *)
  if Db.PIF.get_bond_slave_of ~__context ~self:pif <> Ref.null then
    raise (Api_errors.Server_error (Api_errors.cannot_plug_bond_slave, [Ref.string_of pif]));
    
  let net = Db.PIF.get_network ~__context ~self:pif in
  let bridge = Db.Network.get_bridge ~__context ~self:net in

  if Db.PIF.get_ip_configuration_mode ~__context ~self:pif = `None then
    raise (Api_errors.Server_error(Api_errors.pif_has_no_network_configuration, []));
  
  if Db.PIF.get_management ~__context ~self:pif
  then debug "PIF %s is already marked as a management PIF; taking no action" (Ref.string_of pif)
  else begin
    Xapi_network.attach_internal ~management_interface:true ~__context ~self:net ();
    change_management_interface ~__context bridge;
  
    Xapi_pif.update_management_flags ~__context ~host:(Helpers.get_localhost ~__context)
  end

let management_disable ~__context = 
  (* Disallow if HA is enabled *)
  let pool = List.hd (Db.Pool.get_all ~__context) in
  if Db.Pool.get_ha_enabled ~__context ~self:pool 
  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

  (* Make sure we aren't about to disable our management interface on a slave *)
  if Pool_role.is_slave () 
  then raise (Api_errors.Server_error (Api_errors.slave_requires_management_iface, []));

  Xapi_mgmt_iface.stop ();
  (* Make sure all my PIFs are marked appropriately *)
  Xapi_pif.update_management_flags ~__context ~host:(Helpers.get_localhost ~__context)

let get_system_status_capabilities ~__context ~host =
  if Helpers.get_localhost ~__context <> host
  then failwith "Forwarded to the wrong host";
  System_status.get_capabilities()

let get_diagnostic_timing_stats ~__context ~host = Stats.summarise ()


(* CP-825: Serialize execution of host-enable-extauth and host-disable-extauth *)
(* We need to protect against concurrent execution of the extauth-hook script and host.enable/disable extauth, *)
(* because the extauth-hook script expects the auth_type, service_name etc to be constant throughout its execution *)
(* This mutex also serializes the execution of the plugin, to avoid concurrency problems when updating the sshd configuration *)
let serialize_host_enable_disable_extauth = Mutex.create()


let set_hostname_live ~__context ~host ~hostname =
  Mutex.execute serialize_host_enable_disable_extauth (fun () ->
	let current_auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
  (* the AD/Likewise extauth plugin is incompatible with a hostname change *)
  (if current_auth_type = Extauth.auth_type_AD_Likewise then
		let current_service_name = Db.Host.get_external_auth_service_name ~__context ~self:host in
		raise (Api_errors.Server_error(Api_errors.auth_already_enabled, [current_auth_type;current_service_name]))
	);
  (* hostname is valid if contains only alpha, decimals, and hyphen
     (for hyphens, only in middle position) *)
  let is_invalid_hostname hostname =
    let len = String.length hostname in
    let i = ref 0 in
    let valid = ref true in
    let range = [ 'a', 'z'; 'A', 'Z'; '0', '9'; '-', '-'; '.', '.' ] in
    while !valid && (!i < len)
    do
      begin try
        ignore (List.find (fun (r1, r2) -> r1 <= hostname.[!i] && hostname.[!i] <= r2) range)
      with Not_found ->
        valid := false
      end;
      incr i;
    done;
    if hostname.[0] = '-' || hostname.[len - 1] = '-' then
      true
    else
      (not !valid)
    in
  if String.length hostname = 0 then
    raise (Api_errors.Server_error (Api_errors.host_name_invalid, [ "hostname empty" ]));
  if String.length hostname > 255 then
    raise (Api_errors.Server_error (Api_errors.host_name_invalid, [ "hostname is too long" ]));
  if is_invalid_hostname hostname then
    raise (Api_errors.Server_error (Api_errors.host_name_invalid, [ "hostname contains invalid characters" ]));
  ignore(Forkhelpers.execute_command_get_output "/opt/xensource/libexec/set-hostname" [hostname]);
  Debug.invalidate_hostname_cache ();
  Db.Host.set_hostname ~__context ~self:host ~value:hostname
  )

let is_in_emergency_mode ~__context =
  !Xapi_globs.slave_emergency_mode

let compute_free_memory ~__context ~host =
	(*** XXX: Use a more appropriate free memory calculation here. *)
	Memory_check.host_compute_free_memory_with_maximum_compression
		~dump_stats:false ~__context ~host None

let compute_memory_overhead ~__context ~host =
	Memory_check.host_compute_memory_overhead ~__context ~host

let get_data_sources ~__context ~host = Monitor_rrds.query_possible_host_dss ()

let record_data_source ~__context ~host ~data_source = Monitor_rrds.add_host_ds data_source

let query_data_source ~__context ~host ~data_source = Monitor_rrds.query_host_dss data_source

let forget_data_source_archives ~__context ~host ~data_source = Monitor_rrds.forget_host_ds data_source

let tickle_heartbeat ~__context ~host ~stuff = Db_gc.tickle_heartbeat ~__context host stuff

let create_new_blob ~__context ~host ~name ~mime_type =
  let blob = Xapi_blob.create ~__context ~mime_type in
  Db.Host.add_to_blobs ~__context ~self:host ~key:name ~value:blob;
  blob

let extauth_hook_script_name = Extauth.extauth_hook_script_name
(* this special extauth plugin call is only used inside host.enable/disable extauth to avoid deadlock with the mutex *)
let call_extauth_plugin_nomutex ~__context ~host ~fn ~args =
	let plugin = extauth_hook_script_name in
	debug "Calling extauth plugin %s in host %s with event %s and params %s" plugin (Db.Host.get_name_label ~__context ~self:host) fn (List.fold_left (fun a (b,y)->a^"("^b^"="^y^"),") "" args);
	Xapi_plugins.call_plugin (Context.get_session_id __context) plugin fn args
(* this is the generic extauth plugin call available to xapi users that avoids concurrency problems *)
let call_extauth_plugin ~__context ~host ~fn ~args =
	Mutex.execute serialize_host_enable_disable_extauth (fun () -> 
		call_extauth_plugin_nomutex ~__context ~host ~fn ~args
	)

(* this is the generic plugin call available to xapi users *)
let call_plugin ~__context ~host ~plugin ~fn ~args =
	if plugin = extauth_hook_script_name
	then call_extauth_plugin ~__context ~host ~fn ~args
	else Xapi_plugins.call_plugin (Context.get_session_id __context) plugin fn args

let sync_data ~__context ~host =
  Xapi_sync.sync_host __context host (* Nb, no attempt to wrap exceptions yet *)

let backup_rrds ~__context ~host ~delay =
  Xapi_periodic_scheduler.add_to_queue "RRD backup" Xapi_periodic_scheduler.OneShot
    delay (fun () -> Monitor_rrds.backup ~save_stats_locally:(Pool_role.is_master ()) ())

let get_servertime ~__context ~host =
  Date.of_float (Unix.gettimeofday ())

let get_server_localtime ~__context ~host =
  let gmt_time= Unix.gettimeofday () in
  let local_time = Unix.localtime gmt_time in
  Date.of_string
  (
    Printf.sprintf "%04d%02d%02dT%02d:%02d:%02d"
      (local_time.Unix.tm_year+1900)
      (local_time.Unix.tm_mon+1)
      local_time.Unix.tm_mday
      local_time.Unix.tm_hour
      local_time.Unix.tm_min
      local_time.Unix.tm_sec
	)

let enable_binary_storage ~__context ~host =
  Unixext.mkdir_safe Xapi_globs.xapi_blob_location 0o700;
  Db.Host.remove_from_other_config ~__context ~self:host ~key:Xapi_globs.host_no_local_storage

let disable_binary_storage ~__context ~host =
  ignore(Helpers.get_process_output (Printf.sprintf "/bin/rm -rf %s" Xapi_globs.xapi_blob_location));
  Db.Host.remove_from_other_config ~__context ~self:host ~key:Xapi_globs.host_no_local_storage;
  Db.Host.add_to_other_config ~__context ~self:host ~key:Xapi_globs.host_no_local_storage ~value:"true"

let get_uncooperative_resident_VMs ~__context ~self = assert false

let get_uncooperative_domains ~__context ~self = Monitor.get_uncooperative_domains ()

let certificate_install ~__context ~host ~name ~cert =
  Certificates.host_install true name cert

let certificate_uninstall ~__context ~host ~name =
  Certificates.host_uninstall true name

let certificate_list ~__context ~host =
  Certificates.local_list true

let crl_install ~__context ~host ~name ~crl =
  Certificates.host_install false name crl

let crl_uninstall ~__context ~host ~name =
  Certificates.host_uninstall false name

let crl_list ~__context ~host =
  Certificates.local_list false

let certificate_sync ~__context ~host =
  Certificates.local_sync()

let get_server_certificate ~__context ~host =
  Certificates.get_server_certificate()

(* CA-24856: detect non-homogeneous external-authentication config in pool *)
let detect_nonhomogeneous_external_auth_in_host ~__context ~host =
	
	Helpers.call_api_functions ~__context (fun rpc session_id ->
		let pool = List.hd (Client.Client.Pool.get_all rpc session_id) in
		let master = Client.Client.Pool.get_master rpc session_id pool in
		let master_rec = Client.Client.Host.get_record rpc session_id master in 
		let host_rec = Client.Client.Host.get_record rpc session_id host in

		(* if this host being verified is the master, then we need to verify homogeneity for all slaves in the pool *)
		if (host_rec.API.host_uuid = master_rec.API.host_uuid)
		then
			Client.Client.Pool.detect_nonhomogeneous_external_auth rpc session_id pool

		else (* this host is a slave, let's check if it is homogeneous to the master *)	

		let master_external_auth_type = master_rec.API.host_external_auth_type in
		let master_external_auth_service_name = master_rec.API.host_external_auth_service_name in
		let host_external_auth_type = host_rec.API.host_external_auth_type in
		let host_external_auth_service_name = host_rec.API.host_external_auth_service_name in
		if (host_external_auth_type <> master_external_auth_type
			||
			host_external_auth_service_name <> master_external_auth_service_name
		) then begin
			(* ... this slave has non-homogeneous external-authentication data *)
			debug "Detected non-homogeneous external authentication in host %s: host_auth_type=%s, host_service_name=%s, master_auth_type=%s, master_service_name=%s" 
				(Ref.string_of host) host_external_auth_type host_external_auth_service_name
				master_external_auth_type master_external_auth_service_name;
			(* raise alert about this non-homogeneous slave in the pool *)
			let host_uuid = host_rec.API.host_uuid in
			ignore(
				Client.Client.Message.create ~rpc ~session_id ~name:Api_messages.auth_external_pool_non_homogeneous 
				~priority:1L ~cls:`Host ~obj_uuid:host_uuid ~body:(
					"host_external_auth_type="^host_external_auth_type^
					", host_external_auth_service_name="^host_external_auth_service_name^
					", master_external_auth_type="^master_external_auth_type^
					", master_external_auth_service_name="^master_external_auth_service_name
				)
			)
		end
	)

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
	let current_auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
	let current_service_name = Db.Host.get_external_auth_service_name ~__context ~self:host in
	debug "current external_auth_type is %s" current_auth_type;
	if (current_auth_type <> "") then
	begin (* if auth_type is already defined, then we cannot set up a new one *)
		let msg = (Printf.sprintf "external authentication %s service %s is already enabled" current_auth_type current_service_name) in
		debug "Failed to enable external authentication type %s for service name %s in host %s: %s" auth_type service_name host_name_label msg;
		raise (Api_errors.Server_error(Api_errors.auth_already_enabled, [current_auth_type;current_service_name]))
	end
	else if auth_type = "" then
		begin (* we must error out here, because we never enable an _empty_ external auth_type *)
			let msg = "" in
			debug "Failed while enabling unknown external authentication type %s for service name %s in host %s" msg service_name host_name_label;
			raise (Api_errors.Server_error(Api_errors.auth_unknown_type, [msg]))
		end
	else
	begin (* if no auth_type is currently defined (it is an empty string), then we can set up a new one *)
		
		(* we try to use the configuration to set up the new external authentication service *)
		try
		(* we persist as much set up configuration now as we can *)
		Db.Host.set_external_auth_service_name ~__context ~self:host ~value:service_name;
		(* the ext_auth.on_enable dispatcher called below will store the configuration params, and also *)
		(* filter out any one-time credentials such as the administrator password, so we *)
		(* should not call here 'host.set_external_auth_configuration ~config' *)

		(* use the special 'named dispatcher' function to call an extauth plugin function even though we have *) 
		(* not yet set up the external_auth_type value that will enable generic access to the extauth plugin. *)
		(Ext_auth.nd(auth_type)).on_enable config;
		(* from this point on, we have successfully enabled the external authentication services. *)

		(* Up to this point, we cannot call external auth functions via extauth's generic dispatcher d(). *)
		Db.Host.set_external_auth_type ~__context ~self:host ~value:auth_type;
		(* From this point on, anyone can call external auth functions via extauth.ml's generic dispatcher d(), which depends on the value of external_auth_type. *)
		(* This enables all functions to the external authentication and directory service that xapi makes available to the user, *)
		(* such as external login, subject id/info queries, group membership etc *)

		(* CP-709: call extauth hook-script after extauth.enable *)
		(* we must not fork, intead block until the script has returned *)
		(* so that at most one enable-external-auth event script is running at any one time in the same host *)
		(* we use its local variation without mutex, otherwise we will deadlock *)
		let call_plugin_fn () = call_extauth_plugin_nomutex ~__context ~host 
			~fn:Extauth.event_name_after_extauth_enable 
			~args:(Extauth.get_event_params ~__context host)
		in ignore(Extauth.call_extauth_hook_script_in_host_wrapper ~__context host Extauth.event_name_after_extauth_enable ~call_plugin_fn);

		debug "external authentication service type %s for service name %s enabled successfully in host %s" auth_type service_name host_name_label;
		
		(* CA-24856: detect non-homogeneous external-authentication config in this host *)
		detect_nonhomogeneous_external_auth_in_host ~__context ~host;

		with 
		| Extauth.Unknown_extauth_type msg -> (* unknown plugin *)
			begin
				(* we rollback to the original xapi configuration *)
				Db.Host.set_external_auth_type ~__context ~self:host ~value:current_auth_type;
				Db.Host.set_external_auth_service_name ~__context ~self:host ~value:current_service_name;
				debug "Failed while enabling unknown external authentication type %s for service name %s in host %s" msg service_name host_name_label;
				raise (Api_errors.Server_error(Api_errors.auth_unknown_type, [msg]))
			end
		| Auth_signature.Auth_service_error (errtag,msg) -> (* plugin returned some error *)
				(* we rollback to the original xapi configuration *)
				Db.Host.set_external_auth_type ~__context ~self:host ~value:current_auth_type;
				Db.Host.set_external_auth_service_name ~__context ~self:host ~value:current_service_name;
				debug "Failed while enabling external authentication type %s for service name %s in host %s" msg service_name host_name_label;
			raise (Api_errors.Server_error(Api_errors.auth_enable_failed^(Auth_signature.suffix_of_tag errtag), [msg]))
		| e -> (* unknown failure, just-enabled plugin might be in an inconsistent state *)
			begin
				(* we rollback to the original xapi configuration *)
				Db.Host.set_external_auth_type ~__context ~self:host ~value:current_auth_type;
				Db.Host.set_external_auth_service_name ~__context ~self:host ~value:current_service_name;
				debug "Failed while enabling external authentication type %s for service name %s in host %s" auth_type service_name host_name_label;
				raise (Api_errors.Server_error(Api_errors.auth_enable_failed, [ExnHelper.string_of_exn e]))
			end
	end
	)

(* CP-718: Disables external auth/directory service for host *)
let disable_external_auth_common ?during_pool_eject:(during_pool_eject=false) ~__context ~host ~config = 

	(* CP-825: Serialize execution of host-enable-extauth and host-disable-extauth *)
	(* we need to protect against concurrent access to the host.external_auth_type variable *)
	Mutex.execute serialize_host_enable_disable_extauth (fun () ->

	let host_name_label = Db.Host.get_name_label ~__context ~self:host in
	let auth_type = Db.Host.get_external_auth_type ~__context ~self:host in
	if (auth_type = "") then
	begin (* nothing to do, external authentication is already disabled *)
		let msg = "external authentication service is already disabled" in
		debug "Failed to disable external authentication in host %s: %s" host_name_label msg;
		(* we do not raise an exception here. for our purposes, there's nothing wrong*)
		(* disabling an already disabled authentication plugin *)
	end
	else (* this is the case when auth_type <> "" *)
	begin
		(* CP-709: call extauth hook-script before extauth.disable *)
		(* we must not fork, instead block until the script has returned, so that the script is able *)
		(* to obtain auth_type and other information from the metadata and there is at most one *)
		(* disable-external-auth event script running at any one time in the same host *)
		(* we use its local variation without mutex, otherwise we will deadlock *)
		let call_plugin_fn () = call_extauth_plugin_nomutex ~__context ~host 
			~fn:Extauth.event_name_before_extauth_disable 
			~args:(Extauth.get_event_params ~__context host)
		in ignore(Extauth.call_extauth_hook_script_in_host_wrapper ~__context host Extauth.event_name_before_extauth_disable ~call_plugin_fn);

		(* 1. first, we try to call the external auth plugin to disable the external authentication service *)
		let plugin_disable_failure =
		(try
			(Ext_auth.d()).on_disable config;
			None (* OK, on_disable succeeded *)
		with 
		| Auth_signature.Auth_service_error (errtag,msg) as e ->
			begin
				debug "Failed while calling on_disable event of external authentication plugin in host %s: %s" host_name_label msg;
				Some (Api_errors.Server_error(Api_errors.auth_disable_failed^(Auth_signature.suffix_of_tag errtag), [msg]))
			end
		| e -> (*absorb any exception*)
			begin
				debug "Failed while calling on_disable event of external authentication plugin in host %s: %s" host_name_label (ExnHelper.string_of_exn e);
				Some (Api_errors.Server_error(Api_errors.auth_disable_failed, [ExnHelper.string_of_exn e]))
			end
		) in

		(* 2. then, if no exception was raised, we always remove our persistent extauth configuration *)
		Db.Host.set_external_auth_type ~__context ~self:host ~value:"";
		Db.Host.set_external_auth_service_name ~__context ~self:host ~value:"";
		debug "external authentication service disabled successfully in host %s" host_name_label;
		(* 2.1 if we are still trying to initialize the external auth service in the xapi.on_xapi_initialize thread, we should stop now *)
		Xapi_globs.event_hook_auth_on_xapi_initialize_succeeded := true;(* succeeds because there's no need to initialize anymore *)
		
		(* 3. CP-703: we always revalidate all sessions after the external authentication has been disabled *)
		(* so that all sessions that were externally authenticated will be destroyed *)
		debug "calling revalidate_all_sessions after disabling external auth for host %s" (host_name_label); 
		Xapi_session.revalidate_all_sessions ~__context;
		
		if (not during_pool_eject) then (* CA-28168 *)
		begin
		(* CA-24856: detect non-homogeneous external-authentication config in this host *)
		detect_nonhomogeneous_external_auth_in_host ~__context ~host;
		end;
		
		match plugin_disable_failure with
			| None -> ()
			| Some e -> if not during_pool_eject
				then raise e (* bubble up plugin's on_disable exception *)
				else () (* we do not want to stop pool_eject *)
	end
	)

let disable_external_auth ~__context ~host ~config = 
	disable_external_auth_common ~during_pool_eject:false ~__context ~host ~config

let attach_static_vdis ~__context ~host ~vdi_reason_map =
  let attach (vdi, reason) =
    let static_vdis = Static_vdis_list.list () in
    let check v =
      (v.Static_vdis_list.uuid = Db.VDI.get_uuid ~__context ~self:vdi &&
       v.Static_vdis_list.currently_attached) in
    if not (List.exists check static_vdis) then
      Pervasiveext.ignore_string (Static_vdis.permanent_vdi_attach ~__context ~vdi ~reason)
  in
  List.iter attach vdi_reason_map

let detach_static_vdis ~__context ~host ~vdis = 
  let detach vdi =
    let static_vdis = Static_vdis_list.list () in
    let check v =
      (v.Static_vdis_list.uuid = Db.VDI.get_uuid ~__context ~self:vdi) in
    if List.exists check static_vdis then
      Static_vdis.permanent_vdi_detach ~__context ~vdi;	
  in
  List.iter detach vdis
  
let update_pool_secret ~__context ~host ~pool_secret =
	Unixext.write_string_to_file Xapi_globs.pool_secret_path pool_secret

let set_localdb_key ~__context ~host ~key ~value =
	Localdb.put key value;
	debug "Local-db key '%s' has been set to '%s'" key value

(* Licensing *)

exception Pool_record_expected_singleton
let copy_license_to_db ~__context ~host ~features ~additional =
	let restrict_kvpairs = Features.to_assoc_list features in
	let license_params = additional @ restrict_kvpairs in
	Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			(* This will trigger a pool sku/restrictions recomputation *)
			Client.Client.Host.set_license_params rpc session_id !Xapi_globs.localhost_ref license_params)

let set_license_params ~__context ~self ~value = 
	Db.Host.set_license_params ~__context ~self ~value;
	Pool_features.update_pool_features ~__context
	
let apply_edition ~__context ~host ~edition =
	(* if HA is enabled do not allow the edition to be changed *)
	let pool = List.hd (Db.Pool.get_all ~__context) in
	if Db.Pool.get_ha_enabled ~__context ~self:pool then
		raise (Api_errors.Server_error (Api_errors.ha_is_enabled, []))
	else begin
		let edition', features, additional = V6client.apply_edition ~__context edition [] in
		Db.Host.set_edition ~__context ~self:host ~value:edition';
		copy_license_to_db ~__context ~host ~features ~additional
	end

let license_apply ~__context ~host ~contents =
	let license = Base64.decode contents in
	let tmp = "/tmp/new_license" in
	let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
	let length = String.length license in
	let written = Unix.write fd license 0 length in
	Unix.close fd;
	finally
		(fun () ->
			if written <> length then begin
				debug "Short write!";
				raise (Api_errors.Server_error(Api_errors.license_processing_error, []))
			end;
			let edition', features, additional = V6client.apply_edition ~__context "" ["license_file", tmp] in
			Db.Host.set_edition ~__context ~self:host ~value:edition';
			copy_license_to_db ~__context ~host ~features ~additional
		)
		(fun () ->
			(* The language will have been moved to a standard location if it was valid, and
			 * should be removed otherwise -> always remove the file at the tmp path, if any. *)
			Unixext.unlink_safe tmp
		)
		
(* Supplemental packs *)

let refresh_pack_info ~__context ~host =
	debug "Refreshing software_version";
	let software_version = Create_misc.make_software_version () in
	Db.Host.set_software_version ~__context ~self:host ~value:software_version

(* Network reset *)

let reset_networking ~__context ~host =
	debug "Resetting networking";
	let local_pifs = List.filter (fun pif -> Db.PIF.get_host ~__context ~self:pif = host) (Db.PIF.get_all ~__context) in
	let bond_is_local bond =
		List.fold_left (fun a pif -> Db.Bond.get_master ~__context ~self:bond = pif || a) false local_pifs
	in
	let vlan_is_local vlan =
		List.fold_left (fun a pif -> Db.VLAN.get_untagged_PIF ~__context ~self:vlan = pif || a) false local_pifs
	in
	let tunnel_is_local tunnel =
		List.fold_left (fun a pif -> Db.Tunnel.get_access_PIF ~__context ~self:tunnel = pif || a) false local_pifs
	in
	let bonds = List.filter bond_is_local (Db.Bond.get_all ~__context) in
	List.iter (fun bond -> debug "destroying bond %s" (Db.Bond.get_uuid ~__context ~self:bond);
		Db.Bond.destroy ~__context ~self:bond) bonds;
	let vlans = List.filter vlan_is_local (Db.VLAN.get_all ~__context) in
	List.iter (fun vlan -> debug "destroying VLAN %s" (Db.VLAN.get_uuid ~__context ~self:vlan);
		Db.VLAN.destroy ~__context ~self:vlan) vlans;
	let tunnels = List.filter tunnel_is_local (Db.Tunnel.get_all ~__context) in
	List.iter (fun tunnel -> debug "destroying tunnel %s" (Db.Tunnel.get_uuid ~__context ~self:tunnel);
		Db.Tunnel.destroy ~__context ~self:tunnel) tunnels;
	List.iter (fun pif -> debug "destroying PIF %s" (Db.PIF.get_uuid ~__context ~self:pif);
		Db.PIF.destroy ~__context ~self:pif) local_pifs

(* CPU feature masking *)

let set_cpu_features ~__context ~host ~features =
	debug "Set CPU features";
	(* check restrictions *)
	if not (Pool_features.is_enabled ~__context Features.CPU_masking) then
		raise (Api_errors.Server_error (Api_errors.feature_restricted, []));
	
	let cpuid = Cpuid.read_cpu_info () in
	
	(* parse features string *)
	let features =
		try Cpuid.string_to_features features
		with Cpuid.InvalidFeatureString e ->
			raise (Api_errors.Server_error (Api_errors.invalid_feature_string, [e]))
	in
	
	(* check masking is possible *)
	begin try
		Cpuid.assert_maskability cpuid cpuid.Cpuid.manufacturer features
	with
	| Cpuid.MaskingNotSupported e -> 
		raise (Api_errors.Server_error (Api_errors.cpu_feature_masking_not_supported, [e]))
	| Cpuid.InvalidFeatureString e ->	
		raise (Api_errors.Server_error (Api_errors.invalid_feature_string, [e]))
	| Cpuid.ManufacturersDiffer -> () (* cannot happen *)
	end;
	
	(* add masks to Xen command line *)
	ignore (Xen_cmdline.delete_cpuid_masks ["cpuid_mask_ecx"; "cpuid_mask_edx"; "cpuid_mask_ext_ecx"; "cpuid_mask_ext_edx"]);
	let new_masks = Cpuid.xen_masking_string cpuid features in
	ignore (Xen_cmdline.set_cpuid_masks new_masks);
	
	(* update database *)
	let cpu_info = Db.Host.get_cpu_info ~__context ~self:host in
	let cpu_info = List.replace_assoc "features_after_reboot" (Cpuid.features_to_string features) cpu_info in
	Db.Host.set_cpu_info ~__context ~self:host ~value:cpu_info
		
let reset_cpu_features ~__context ~host =
	debug "Reset CPU features";
	ignore (Xen_cmdline.delete_cpuid_masks ["cpuid_mask_ecx"; "cpuid_mask_edx"; "cpuid_mask_ext_ecx"; "cpuid_mask_ext_edx"]);
	let cpu_info = Db.Host.get_cpu_info ~__context ~self:host in
	let physical_features = List.assoc "physical_features" cpu_info in
	let cpu_info = List.replace_assoc "features_after_reboot" physical_features cpu_info in
	Db.Host.set_cpu_info ~__context ~self:host ~value:cpu_info

(* Local storage caching *)

let enable_local_storage_caching ~__context ~host ~sr =
	assert_bacon_mode ~__context ~host;
	let ty = Db.SR.get_type ~__context ~self:sr in
	let pbds = Db.SR.get_PBDs ~__context ~self:sr in
	let shared = Db.SR.get_shared ~__context ~self:sr in
	let has_required_capability = 
		let caps = Sm.capabilities_of_driver ty in
		List.mem Smint.Sr_supports_local_caching caps
	in
	debug "shared: %b. List.length pbds: %d. has_required_capability: %b" shared (List.length pbds) has_required_capability;
	if (shared=false) && (List.length pbds = 1) && has_required_capability then begin
		let pbd_host = Db.PBD.get_host ~__context ~self:(List.hd pbds) in
		if pbd_host <> host then raise (Api_errors.Server_error (Api_errors.host_cannot_see_SR,[Ref.string_of host; Ref.string_of sr]));
		Db.Host.set_local_cache_sr ~__context ~self:host ~value:sr;
		Db.SR.set_local_cache_enabled ~__context ~self:sr ~value:true;
		Monitor.set_cache_sr (Db.SR.get_uuid ~__context ~self:sr);		
	end else begin
		raise (Api_errors.Server_error (Api_errors.sr_operation_not_supported,[]))
	end

let disable_local_storage_caching ~__context ~host =
	assert_bacon_mode ~__context ~host;
	let sr = Db.Host.get_local_cache_sr ~__context ~self:host in
	Db.Host.set_local_cache_sr ~__context ~self:host ~value:Ref.null;
	Monitor.unset_cache_sr ();
	try Db.SR.set_local_cache_enabled ~__context ~self:sr ~value:false with _ -> () 


