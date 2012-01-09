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
(** Code to bring the database up-to-date when a host starts up.
 *  @group Main Loop and Start-up
 *)
 
open Stringext
open Listext
open Printf
open Vmopshelpers
open Create_misc
open Client
open Pervasiveext
open Xenstore

module D=Debug.Debugger(struct let name="dbsync" end)
open D

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( ** ) = Int64.mul
let ( // ) = Int64.div

let get_my_ip_addr() =
  match (Helpers.get_management_ip_addr()) with
      Some ip -> ip
    | None -> (error "Cannot read IP address. Check the control interface has an IP address"; "")


let create_localhost ~__context info =
  let ip = get_my_ip_addr () in
  let me = try Some (Db.Host.get_by_uuid ~__context ~uuid:info.uuid) with _ -> None in
  (* me = None on firstboot only *)
  if me = None
  then 
    let (_: API.ref_host) = 
      Xapi_host.create ~__context ~uuid:info.uuid ~name_label:info.hostname ~name_description:"" 
	~hostname:info.hostname ~address:ip 
	~external_auth_type:"" ~external_auth_service_name:"" ~external_auth_configuration:[] 
	~license_params:[] ~edition:"free" ~license_server:["address", "localhost"; "port", "27000"]
	~local_cache_sr:Ref.null ~chipset_info:[]
    in ()		

(* TODO cat /proc/stat for btime ? *)
let get_start_time () =
  try
    debug "Calculating boot time...";
    let now = Unix.time () in
    let uptime = Unixext.string_of_file "/proc/uptime" in
    let uptime = String.strip String.isspace uptime in
    let uptime = String.split ' ' uptime in
    let uptime = List.hd uptime in
    let uptime = float_of_string uptime in
    let boot_time = Date.of_float (now -. uptime) in
      debug " system booted at %s" (Date.to_string boot_time);
      boot_time
  with
      e ->
        debug "Calculating boot time failed with '%s'" (ExnHelper.string_of_exn e);
        Date.never

(** Update the information in the Host structure *)
(* not sufficient just to fill in this data on create time [Xen caps may change if VT enabled in BIOS etc.] *)
let refresh_localhost_info ~__context info =
  let host = !Xapi_globs.localhost_ref in
  let software_version = Create_misc.make_software_version () in

  (* Xapi_ha_flags.resync_host_armed_flag __context host; *)
  debug "Updating host software_version";

    Db.Host.set_software_version ~__context ~self:host ~value:software_version;
    Db.Host.set_API_version_major ~__context ~self:host ~value:Xapi_globs.api_version_major;
    Db.Host.set_API_version_minor ~__context ~self:host ~value:Xapi_globs.api_version_minor;
    Db.Host.set_hostname ~__context ~self:host ~value:info.hostname;
    let caps = String.split ' ' (Xenctrl.with_intf (fun xc -> Xenctrl.version_capabilities xc)) in
    Db.Host.set_capabilities ~__context ~self:host ~value:caps;
    Db.Host.set_address ~__context ~self:host ~value:(get_my_ip_addr());

    let boot_time_key = "boot_time" in
    let boot_time_value = string_of_float (Date.to_float (get_start_time ())) in

    Db.Host.remove_from_other_config ~__context ~self:host ~key:boot_time_key;
    Db.Host.add_to_other_config ~__context ~self:host ~key:boot_time_key ~value:boot_time_value;

    let agent_start_key = "agent_start_time" in 
    let agent_start_time = string_of_float (Unix.time ()) in

    Db.Host.remove_from_other_config ~__context ~self:host ~key:agent_start_key;
    Db.Host.add_to_other_config ~__context ~self:host ~key:agent_start_key ~value:agent_start_time;

    (* Register whether we have local storage or not *)

    if not (Helpers.local_storage_exists ()) then begin
      Db.Host.remove_from_other_config ~__context ~self:host ~key:Xapi_globs.host_no_local_storage;
      Db.Host.add_to_other_config ~__context ~self:host ~key:Xapi_globs.host_no_local_storage ~value:"true"
    end else
      Db.Host.remove_from_other_config ~__context ~self:host ~key:Xapi_globs.host_no_local_storage

(*************** update database tools ******************)

(** Update the list of VMs *)
let update_vms ~xal ~__context =
  debug "Updating the list of VMs";
  let xs = Xal.xs_of_ctx xal in
  let xc = Xal.xc_of_ctx xal in

  (** Reset the state of the VM, and clear current operations on VBDs and VIFs *)
  let force_state_reset ~__context ~self ~value:state =
    Xapi_vm_lifecycle.force_state_reset ~__context ~self ~value:state;
    if state = `Halted 
    then
      begin
	(* if we're halted then also ensure any VBDs and VIFs have their current operations reset *)
	List.iter (fun vbd -> Xapi_vbd_helpers.clear_current_operations ~__context ~self:vbd)
	  (Db.VM.get_VBDs ~__context ~self);
	List.iter (fun vif -> Xapi_vif_helpers.clear_current_operations ~__context ~self:vif)
	  (Db.VM.get_VIFs ~__context ~self)
      end
  in

  let set_db_shutdown vm =
    Db.VM.set_domid ~__context ~self:vm ~value:(-1L);
    force_state_reset ~__context ~self:vm ~value:`Halted in

  let set_db_state_and_domid vm state domid =
    let domid = Int64.of_int domid in
    Db.VM.set_domid ~__context ~self:vm ~value:domid;
    List.iter (fun self -> Xapi_vbd_helpers.clear_current_operations ~__context ~self) (Db.VM.get_VBDs ~__context ~self:vm);
    force_state_reset ~__context ~self:vm ~value:state in

  let all_my_domains = Xenctrl.domain_getinfolist xc 0 in
  let my_active_domains = List.filter (fun dinfo -> (not dinfo.Xenctrl.dying) && (not dinfo.Xenctrl.shutdown)) all_my_domains in
  let my_shutdown_domains = List.filter (fun dinfo -> dinfo.Xenctrl.shutdown) all_my_domains in

  let this_host = Helpers.get_localhost __context in
  (* CA-22309: consider this host to 'own' a domain if:
     * it is resident_on me
     * it is scheduled_to_be_resident_on me AND NOT resident_on somewhere else: CA-29412 *)
  let all_resident_on_vms = Db.VM.get_records_where ~__context ~expr:(Db_filter_types.Eq (Db_filter_types.Field "resident_on", Db_filter_types.Literal (Ref.string_of this_host))) in
  let all_scheduled_to_be_resident_on_vms = Db.VM.get_records_where ~__context ~expr:(Db_filter_types.Eq (Db_filter_types.Field "scheduled_to_be_resident_on", Db_filter_types.Literal (Ref.string_of this_host))) in
  (* Remove all the scheduled_to_be_resident_on VMs which are resident_on somewhere since that host 'owns' them.
     NB if resident_on this host the VM will still be counted in the all_resident_on_vms set *)
  let really_my_scheduled_to_be_resident_on_vms = 
    List.filter (fun (_, vm_r) -> not (Db.is_valid_ref __context vm_r.API.vM_resident_on)) all_scheduled_to_be_resident_on_vms in
  let all_vms_assigned_to_me = Listext.List.setify (all_resident_on_vms @ really_my_scheduled_to_be_resident_on_vms) in

  let power_states_where_domain_exists = [ `Running; `Paused ] in
  let my_running_vms_according_to_db =
    List.filter (fun (_,vmrec) -> (List.mem vmrec.API.vM_power_state power_states_where_domain_exists)) all_vms_assigned_to_me in
  let my_nonrunning_vms_according_to_db = 
    List.filter (fun (_,vmrec) -> not (List.mem vmrec.API.vM_power_state power_states_where_domain_exists)) all_vms_assigned_to_me in
  (* Set scheduled_to_be_resident_on and resident_on to NULL for a domain which is powered off: this avoids confusing
     the host memory check in VM.start *)
  List.iter (fun (r, vmrec) -> 
	       info "Clearing VM.resident_on and current_operations for uuid '%s'" vmrec.API.vM_uuid;
	       Db.VM.set_current_operations ~__context ~self:r ~value:[];
	       Db.VM.set_resident_on ~__context ~self:r ~value:Ref.null;
	       Db.VM.set_scheduled_to_be_resident_on ~__context ~self:r ~value:Ref.null;
	    ) my_nonrunning_vms_according_to_db;

  let my_running_vm_refs_according_to_db = List.map fst my_running_vms_according_to_db in

  let uuid_from_dinfo dinfo =
    Uuid.to_string (Uuid.uuid_of_int_array dinfo.Xenctrl.handle) in

  let uuid_from_vmref vmref =
    try
      let _,vmrec = List.find (fun (ref,_)->ref=vmref) all_vms_assigned_to_me in
      vmrec.API.vM_uuid
    with _ ->
      Db.VM.get_uuid ~__context ~self:vmref in

  let vmrefrec_of_dinfo dinfo =
    let uuid = uuid_from_dinfo dinfo in
    try
      let vmrefrec = List.find (fun (ref,apirec)->apirec.API.vM_uuid=uuid) all_vms_assigned_to_me in
      vmrefrec
    with _ ->
      let _ref = Db.VM.get_by_uuid ~__context ~uuid in
      let _rec = Db.VM.get_record ~__context ~self:_ref in
      (_ref,_rec)
  in

  let sync_devices dinfo =
    let (vmref,vmrec) = vmrefrec_of_dinfo dinfo in
    (* Pretend that an event has been triggered for each VBD.
       Note we call the event module's inner functions synchronously, with no locks.
       This is only safe because we haven't started the background thread monitor yet. *)
    Locking_helpers.with_lock vmref
      (fun token () ->
	 let vm_vbds = vmrec.API.vM_VBDs in
	 List.iter
	   (fun vbd ->
	      try
			if Db.is_valid_ref __context vbd && not (Db.VBD.get_empty ~__context ~self:vbd)
			then Events.Resync.vbd ~__context token vmref vbd
	      with e ->
		warn "Caught error resynchronising VBD: %s" (ExnHelper.string_of_exn e)) vm_vbds;
	 let vm_vifs = vmrec.API.vM_VIFs in
	 List.iter 
		 (fun vif ->
			 try
				 if Db.is_valid_ref __context vif then begin
					 (* Events.Resync.vif assumes that VIF is registered if currently_attached = true.
						It will explicitly unregister it IFF currently_attached transitions to false *)
					 if Db.VIF.get_currently_attached ~__context ~self:vif
					 then Xapi_network.register_vif ~__context vif;
					 Events.Resync.vif ~__context token vmref vif
				 end
			 with e ->
				 warn "Caught error resynchronising VIF: %s" (ExnHelper.string_of_exn e);
		 ) vm_vifs;
		try Events.Resync.pci ~__context token vmref
		with e ->
			warn "Caught error resynchronising PCIs: %s" (ExnHelper.string_of_exn e);
      ) () in

  (* We call a domain "managed" if we have some kind of vm record for
     it [albeit an inconsistent one]; we call a domain "unmanaged" if
     we have no record of it at all *)

  (* Deal with a VM whose resident-on fields indicates it should be running here, but no domain exists here... *)
  let vm_in_db_for_me_but_no_domain_on_me vm =
    debug "domain marked as running on me in db, but no active domain: %s" (uuid_from_vmref vm);
    Db.VM.set_resident_on ~__context ~self:vm ~value:Ref.null;
    Db.VM.set_scheduled_to_be_resident_on ~__context ~self:vm ~value:Ref.null;
    set_db_shutdown vm in
    
  (* Process a "managed domain" that's active here, syncing devices and registering monitoring events *)
  let managed_domain_running dinfo =
    let vmref,vmrec = vmrefrec_of_dinfo dinfo in
      (* If this domain isn't marked as running on my in the database then make it so... *)
      if not (List.mem vmref my_running_vm_refs_according_to_db) then
	begin
	  debug "domain running on me, but corresponding db record doesn't have resident_on=me && powerstate=running: %s" (uuid_from_vmref vmref);
	  Db.VM.set_resident_on ~__context ~self:vmref ~value:this_host;
	end;

    (* CA-13878: if we've restarted xapi in the middle of starting or rebooting a VM, restart
       the VM again under the assumption that the devices haven't been attached or the memory
       image is not built.
       We detect the starting/rebooting VM by the fact that it is paused and has used no CPU time
         and the power-state is not marked as Paused (this distinguishes between a VM which
         has been started paused and left alone for a long time and a VM which is being started
         or rebooted, which would always have the power state to Halted or Running)
       We start it again by setting the domain's state to shutdown with reason reboot (the event
       thread will do the hard work for us). *)
    if dinfo.Xenctrl.paused && not(dinfo.Xenctrl.shutdown) && dinfo.Xenctrl.cpu_time = 0L && 
      (vmrec.API.vM_power_state <> `Paused) then begin
	warn "domain id %d uuid %s is paused but not in the database as paused; assuming it's broken; rebooting" 
	  dinfo.Xenctrl.domid (uuid_from_vmref vmref);
	(* Mark the domain as shutdown(reboot), the power state as running and inject
	   a fake event into the event system. This should provoke the event thread into 
	   restarting the VM *)
	Xenctrl.domain_shutdown xc dinfo.Xenctrl.domid Xenctrl.Reboot;
	set_db_state_and_domid vmref `Running dinfo.Xenctrl.domid;
	Events.callback_release xal dinfo.Xenctrl.domid (Uuid.string_of_uuid (Uuid.uuid_of_int_array dinfo.Xenctrl.handle))
      end else begin
	(* Reset the power state, this also clears VBD operations etc *)
	let state = if dinfo.Xenctrl.paused then `Paused else `Running in
	set_db_state_and_domid vmref state dinfo.Xenctrl.domid;
      end;
    (* Now sync devices *)
    debug "syncing devices and registering vm for monitoring: %s" (uuid_from_dinfo dinfo);
    let uuid = Uuid.uuid_of_int_array dinfo.Xenctrl.handle in
	sync_devices dinfo;
	(* Update the VM's guest metrics since: (i) while we were offline we may
	   have missed an update; and (ii) if the tools .iso has been updated then
	   we wish to re-evaluate whether we believe the VMs have up-to-date
	   tools *)

	Events.guest_agent_update xal dinfo.Xenctrl.domid (uuid_from_dinfo dinfo);
	(* Now register with monitoring thread *)

      Monitor_rrds.load_rrd ~__context (Uuid.to_string uuid) false
  in

  (* Process a managed domain that exists here, but is in the shutdown state *)
  let managed_domain_shutdown dinfo =
    debug "found shutdown domain; trying to clean-up: %s" (uuid_from_dinfo dinfo);
    let vmref,vmrec = vmrefrec_of_dinfo dinfo in
    if vmrec.API.vM_resident_on = this_host then begin
      debug "VM is apparently resident on this host; injecting a fake event into the event thread";
      Events.callback_release xal dinfo.Xenctrl.domid (Uuid.string_of_uuid (Uuid.uuid_of_int_array dinfo.Xenctrl.handle))
    end else begin
      debug "VM is not resident on this host; destroying remnant of managed domain";
      Domain.destroy ~xc ~xs dinfo.Xenctrl.domid
    end in
  
  (* Process an "unmanaged domain" that's running here *)
  let unmanaged_domain_running dinfo =
    debug "killing umanaged domain: %s" (uuid_from_dinfo dinfo);
    Domain.destroy ~xc ~xs dinfo.Xenctrl.domid (* bye-bye... *) in

  let have_record_for dinfo = try let _,_ = vmrefrec_of_dinfo dinfo in true with _ -> false in

  let all_my_managed_domains = List.filter have_record_for all_my_domains in
  let my_active_managed_domains = List.filter have_record_for my_active_domains in
  let my_shutdown_managed_domains = List.filter have_record_for my_shutdown_domains in
  let all_my_unmanaged_domains = List.filter (fun dinfo -> not (have_record_for dinfo)) all_my_domains in

  let vm_has_domain_here vmref =
    try
      let vmrec = List.assoc vmref all_vms_assigned_to_me in
      let uuid = vmrec.API.vM_uuid in
      ignore(List.find (fun dinfo -> (uuid_from_dinfo dinfo)=uuid) all_my_managed_domains);
      true
    with
	Not_found -> false in

  let resident_on_but_no_domain =
    List.filter (fun x -> not (vm_has_domain_here x)) my_running_vm_refs_according_to_db in

    (* Run syncing functions on the various lists we've constructed *)
    List.iter unmanaged_domain_running all_my_unmanaged_domains;
    List.iter vm_in_db_for_me_but_no_domain_on_me resident_on_but_no_domain;
    List.iter managed_domain_running my_active_managed_domains;
    List.iter managed_domain_shutdown my_shutdown_managed_domains

(** Record host memory properties in database *)
let record_host_memory_properties ~__context =
	let self = !Xapi_globs.localhost_ref in
	let total_memory_bytes =
		with_xc (fun xc -> Memory.get_total_memory_bytes ~xc) in
	let metrics = Db.Host.get_metrics ~__context ~self in
	Db.Host_metrics.set_memory_total ~__context ~self:metrics ~value:total_memory_bytes;
	let boot_memory_file = Xapi_globs.initial_host_free_memory_file in
	let boot_memory_string =
		try
			Some (Unixext.string_of_file boot_memory_file)
		with e ->
			warn "Could not read host free memory file. This may prevent \
			VMs from being started on this host. (%s)" (Printexc.to_string e);
			None in
	maybe
		(fun boot_memory_string ->
			let boot_memory_bytes = Int64.of_string boot_memory_string in
			(* Host memory overhead comes from multiple sources:         *)
			(* 1. obvious overhead: (e.g. Xen, crash kernel).            *)
			(*    appears as used memory.                                *)
			(* 2. non-obvious overhead: (e.g. low memory emergency pool) *)
			(*    appears as free memory but can't be used in practice.  *)
			let obvious_overhead_memory_bytes =
				total_memory_bytes -- boot_memory_bytes in
			let nonobvious_overhead_memory_kib = 
				try
					Vmopshelpers.with_xs
						(fun xs -> Int64.of_string
							(xs.Xs.read
								Xapi_globs.squeezed_reserved_host_memory))
				with _ ->
					error "Failed to read %s: \
						host memory overhead may be too small"
						Xapi_globs.squeezed_reserved_host_memory;
					0L
			in
			let nonobvious_overhead_memory_bytes =
				Memory.bytes_of_kib nonobvious_overhead_memory_kib in
			Db.Host.set_boot_free_mem ~__context ~self
				~value:boot_memory_bytes;
			Db.Host.set_memory_overhead ~__context ~self ~value:
				(obvious_overhead_memory_bytes ++ nonobvious_overhead_memory_bytes);
		)
		boot_memory_string

(* -- used this for testing uniqueness constraints executed on slave do not kill connection.
   Committing commented out vsn of this because it might be useful again..
let test_uniqueness_doesnt_kill_us ~__context =
  let duplicate_uuid = Uuid.to_string (Uuid.make_uuid()) in
    Db.Network.create ~__context ~ref:(Ref.make()) ~uuid:duplicate_uuid
      ~current_operations:[] ~allowed_operations:[]
      ~name_label:"Test uniqueness constraint"
      ~name_description:"Testing"
      ~bridge:"bridge" ~other_config:[];
    Db.Network.create ~__context ~ref:(Ref.make()) ~uuid:duplicate_uuid
      ~current_operations:[] ~allowed_operations:[]
      ~name_label:"Test uniqueness constraint"
      ~name_description:"Testing"
      ~bridge:"bridge" ~other_config:[];
    ()
*)

(** Make sure the PIF we're using as a management interface is marked as attached
    otherwise we might blow it away by accident *)
(* CA-23803:
 * As well as marking the management interface as attached, mark any other important 
 * interface (defined by what is brought up before xapi starts) as attached too.
 * For example, this will prevent needless glitches in storage interfaces.
 *)
let resynchronise_pif_params ~__context =
	let localhost = Helpers.get_localhost ~__context in

	(* Determine all bridges that are currently up, and ask the master to sync the currently_attached
	 * fields on all my PIFs *)
	Helpers.call_api_functions ~__context (fun rpc session_id ->
		let bridges = Netdev.network.Netdev.list () in
		Client.Host.sync_pif_currently_attached rpc session_id localhost bridges
	);

	(* sync management *)
	Xapi_pif.update_management_flags ~__context ~host:localhost;

	(* sync MACs and MTUs *)
	Xapi_pif.refresh_all ~__context ~host:localhost

(** Update the database to reflect current state. Called for both start of day and after
   an agent restart. *)
let update_env __context sync_keys =
  (* -- used this for testing uniqueness constraints executed on slave do not kill connection.
     Committing commented out vsn of this because it might be useful again..
  try
    test_uniqueness_doesnt_kill_us ~__context
  with e -> debug "Result of uniqueness constraint check = %s" (Printexc.to_string e);
  *)

  (* Helper function to allow us to switch off particular types of syncing *)
  let switched_sync key f = 
    let skip_sync = 
      try
	List.assoc key sync_keys = Xapi_globs.sync_switch_off
      with _ -> false
    in 
    if (not skip_sync)
    then (debug "Sync: %s" key; f ())
    else debug "Skipping sync keyed: %s" key
  in

  (* Ensure basic records exist: *)

  let info = Create_misc.read_localhost_info () in

  (* create localhost record if doesn't already exist *)
  switched_sync Xapi_globs.sync_create_localhost (fun () -> 
    debug "creating localhost";
    create_localhost ~__context info; 
  );

  (* record who we are in xapi_globs *)
  Xapi_globs.localhost_ref := Helpers.get_localhost ~__context;

  (* Set the cache_sr *)
  begin 
	  try
		  let cache_sr = Db.Host.get_local_cache_sr ~__context ~self:(Helpers.get_localhost ~__context) in
		  let cache_sr_uuid = Db.SR.get_uuid ~__context ~self:cache_sr in
		  Db.SR.set_local_cache_enabled ~__context ~self:cache_sr ~value:true;
		  Monitor.set_cache_sr cache_sr_uuid
	  with _ -> Monitor.unset_cache_sr () 
  end;

  begin try
    Unix.access "/tmp/do-not-use-networkd" [Unix.F_OK];
    Nm.use_networkd := false;
    debug "Using interface-reconfigure to setup networking"
  with _ ->
    Nm.use_networkd := true;
    debug "Using xcp-network to setup networking"
  end;

  (* Load the host rrd *)
  Monitor_rrds.load_rrd ~__context (Helpers.get_localhost_uuid ()) true;

  (* maybe record host memory properties in database *)
  switched_sync Xapi_globs.sync_record_host_memory_properties (fun () ->
    record_host_memory_properties ~__context;
  );

  switched_sync Xapi_globs.sync_create_host_cpu (fun () ->
    debug "creating cpu";
    Create_misc.create_host_cpu ~__context;
  );

  switched_sync Xapi_globs.sync_create_domain_zero (fun () ->
    debug "creating domain 0";
    Create_misc.ensure_domain_zero_records ~__context info;
  );

  let localhost = Helpers.get_localhost ~__context in

  switched_sync Xapi_globs.sync_crashdump_resynchronise (fun () ->
    debug "resynchronising host crashdumps";
    Xapi_host_crashdump.resynchronise ~__context ~host:localhost;
  );

  (* We need to re-establish our binding of PBD/SR -> driver plugin
     before we call 'update_vms' which calls 'Resync.vbd' which uses
     the storage driver interface. *)
  switched_sync Xapi_globs.sync_pbds (fun () ->
	  debug "resynchronising host PBDs";
	  Storage_access.resynchronise_pbds ~__context ~pbds:(Db.Host.get_PBDs ~__context ~self:localhost);
  );

  switched_sync Xapi_globs.sync_update_vms (fun () -> 
    debug "updating VM states";
    with_xal (fun xal -> update_vms ~xal ~__context);
  );

  switched_sync Xapi_globs.sync_remove_leaked_vbds (fun () ->
    debug "removing any leaked dom0 block-attached VBDs (if any)";
    Attach_helpers.remove_all_leaked_vbds __context;
  );

(*
  debug "resynchronising db with host physical interfaces";
  update_physical_networks ~__context;
*)

  switched_sync Xapi_globs.sync_pif_params (fun () ->
    debug "resynchronising PIF params";
    resynchronise_pif_params ~__context;
  );

  switched_sync Xapi_globs.sync_patch_update_db (fun () ->
    debug "checking patch status";
    Xapi_pool_patch.update_db ~__context
  );
  
  switched_sync Xapi_globs.sync_bios_strings (fun () ->
    debug "get BIOS strings on startup";
    if Db.Host.get_bios_strings ~__context ~self:localhost = [] then
      Bios_strings.set_host_bios_strings ~__context ~host:localhost
  );

  (* CA-35549: In a pool rolling upgrade, the master will detect the end of upgrade when the software versions
	 of all the hosts are the same. It will then assume that (for example) per-host patch records have
	 been tidied up and attempt to delete orphaned pool-wide patch records. *)

  (* refresh host info fields *)
  switched_sync Xapi_globs.sync_refresh_localhost_info (fun () -> 
    refresh_localhost_info ~__context info;
  );

  switched_sync Xapi_globs.sync_local_vdi_activations (fun () ->
	  Storage_access.refresh_local_vdi_activations ~__context;
  );

  switched_sync Xapi_globs.sync_chipset_info (fun () ->
    Create_misc.create_chipset_info ~__context;
  );

  switched_sync Xapi_globs.sync_pci_devices (fun () ->
    Xapi_pci.update_pcis ~__context ~host:localhost;
  );

  switched_sync Xapi_globs.sync_gpus (fun () ->
    Xapi_pgpu.update_gpus ~__context ~host:localhost;
  );

