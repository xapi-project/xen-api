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
open Stringext
open Listext
open Printf
open Vmopshelpers
open Create_misc
open Client
open Pervasiveext

module D=Debug.Debugger(struct let name="dbsync" end)
open D

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( ** ) = Int64.mul
let ( // ) = Int64.div

let trim_end s =
        let i = ref (String.length s - 1) in
        while !i > 0 && (List.mem s.[!i] [ ' '; '\t'; '\n'; '\r' ])
	do
		decr i
	done;
        if !i >= 0 then String.sub s 0 (!i + 1) else ""

(* create localhost record *)

let get_my_ip_addr() =
  match (Helpers.get_management_ip_addr()) with
      Some ip -> ip
    | None -> (error "Cannot read IP address. Check the control interface has an IP address"; "")

let create_localhost ~__context =
  let info = Create_misc.read_localhost_info () in
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
    in ()

(* TODO cat /proc/stat for btime ? *)
let get_start_time () =
  try
    debug "Calculating boot time...";
    let now = Unix.time () in
    let uptime = Unixext.read_whole_file_to_string "/proc/uptime" in
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

(* not sufficient just to fill in this data on create time [Xen caps may change if VT enabled in BIOS etc.] *)
let refresh_localhost_info ~__context =
  let host = !Xapi_globs.localhost_ref in
  let info = read_localhost_info () in
  let software_version = Create_misc.make_software_version () in
  Xapi_globs.localhost_software_version := software_version; (* Cache this *)

  (* Xapi_ha_flags.resync_host_armed_flag __context host; *)

    Db.Host.set_software_version ~__context ~self:host ~value:software_version;
    Db.Host.set_API_version_major ~__context ~self:host ~value:Xapi_globs.api_version_major;
    Db.Host.set_API_version_minor ~__context ~self:host ~value:Xapi_globs.api_version_minor;
    Db.Host.set_hostname ~__context ~self:host ~value:info.hostname;
    let caps = String.split ' ' (Xc.with_intf (fun xc -> Xc.version_capabilities xc)) in
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

(** Copy Bonds from master *)
let copy_bonds_from_master ~__context =
  if Pool_role.is_master () then () (* if master do nothing *)
  else
    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
	 (* if slave: then inherit network config (bonds and vlans) from master (if we don't already have them) *)
	 let me = !Xapi_globs.localhost_ref in
	 let pool = List.hd (Db.Pool.get_all ~__context) in
	 let master = Db.Pool.get_master ~__context ~self:pool in

	 let all_pifs = Db.PIF.get_records_where ~__context ~expr:Db_filter_types.True in
	 let all_master_pifs = List.filter (fun (_, prec) -> prec.API.pIF_host=master) all_pifs in
	 let my_pifs = List.filter (fun (_, pif) -> pif.API.pIF_host=me) all_pifs in
    
	 (* Consider Bonds *)
	 debug "Resynchronising bonds";
	 let all_bonds = Db.Bond.get_records_where ~__context ~expr:Db_filter_types.True in
	 let maybe_create_bond_for_me bond = 
	   let network = Db.PIF.get_network ~__context ~self:bond.API.bond_master in
	   let slaves_to_mac_and_device_map =
	     List.map (fun self -> self, Db.PIF.get_MAC ~__context ~self, Db.PIF.get_device ~__context ~self) bond.API.bond_slaves in
	   (* Take the MAC addr of the bond and figure out whether this is the MAC address of any of the
	      slaves. If it is then we will use this to ensure that we inherit the MAC address from the _same_
	      slave when we re-create on the slave *)
	   let master_bond_mac = Db.PIF.get_MAC ~__context ~self:bond.API.bond_master in
	   let master_slaves_with_same_mac_as_bond (* expecting a list of at most 1 here *) =
	     List.filter (fun (pifref,mac,device)->mac=master_bond_mac) slaves_to_mac_and_device_map in
	   (* This tells us the device that the master used to inherit the bond's MAC address
	      (if indeed that is what it did; we set it to None if we think it didn't do this) *)
	   let device_of_primary_slave =
	     match master_slaves_with_same_mac_as_bond with
	       [] -> None
	     | [_,_,device] ->
		 debug "Master bond has MAC address derived from %s" device;
		 Some device (* found single slave with mac matching bond master => this was one that we inherited mac from *)
	     | _ -> None in
	   (* Look at the master's slaves and find the corresponding slave PIFs. Note that the slave
	      might not have the necessary devices: in this case we'll try to make partial bonds *)
	   let slave_devices = List.map (fun (_,_,device)->device) slaves_to_mac_and_device_map in
	   let my_slave_pifs = List.filter (fun (_, pif) -> List.mem pif.API.pIF_device slave_devices && pif.API.pIF_VLAN = (-1L)) my_pifs in

	   let my_slave_pif_refs = List.map fst my_slave_pifs in
	   (* Do I have a pif that I should treat as a primary pif - i.e. the one to inherit the MAC address from on my bond create? *)
	   let my_primary_slave =
	     match device_of_primary_slave with
	       None -> None (* don't care cos we couldn't even figure out who master's primary slave was *)
	     | Some master_primary ->
		 begin
		   match List.filter (fun (_,pif) -> pif.API.pIF_device=master_primary) my_slave_pifs with
		     [] -> None
		   | [pifref,_] ->
		       debug "I have found a PIF to use as primary bond slave (will inherit MAC address of bond from this PIF).";
		       Some pifref (* this is my pif corresponding to the master's primary slave *)
		   | _ -> None
		 end in
	   (* If I do have a pif that I need to treat as my primary slave then I need to put it first in the list so the
	      bond master will inherit it's MAC address *)
	   let my_slave_pif_refs =
	     match my_primary_slave with
	       None -> my_slave_pif_refs (* no change *)
	     | Some primary_pif -> primary_pif :: (List.filter (fun x-> x<>primary_pif) my_slave_pif_refs) (* remove primary pif ref and stick it on the front *) in
	   
	   match List.filter (fun (_, pif) -> pif.API.pIF_network = network) my_pifs, my_slave_pifs with
	   | [], [] ->
	       (* No bond currently exists but neither do any slave interfaces -> do nothing *)
	       warn "Cannot create bond %s at all: no PIFs exist on slave" bond.API.bond_uuid
	   | [], _ ->
	       (* No bond currently exists but some slave interfaces do -> create a (partial?) bond *)
	       let (_: API.ref_Bond) = Client.Bond.create rpc session_id network my_slave_pif_refs "" in ()
	   | [ _, { API.pIF_bond_master_of = [ slave_bond ] } ], _ ->
	       (* Some bond exists, check whether the existing set of slaves is the same as the potential set *)
	       let current_slave_pifs = Db.Bond.get_slaves ~__context ~self:slave_bond in
	       if not (List.set_equiv (List.setify current_slave_pifs) (List.setify my_slave_pif_refs)) then begin
		 debug "Partial bond exists; recreating";
		 Client.Bond.destroy rpc session_id slave_bond;
		 let (_: API.ref_Bond) = Client.Bond.create rpc session_id network my_slave_pif_refs "" in ()
	       end
	   | [ _, { API.pIF_uuid = uuid } ], _ ->
	       warn "Couldn't create bond on slave because PIF %s already on network %s"
		 uuid (Db.Network.get_uuid ~__context ~self:network) in
	 let master_bonds = List.filter (fun (_, b) -> List.mem b.API.bond_master (List.map fst all_master_pifs)) all_bonds in
	 List.iter (Helpers.log_exn_continue "resynchronising bonds on slave" maybe_create_bond_for_me)
	   (List.map snd master_bonds))

(** Copy VLANs from master *)
let copy_vlans_from_master ~__context =
  if Pool_role.is_master () then () (* if master do nothing *)
  else
    (* Consider VLANs after bonds so we can add VLANs on top of bonded interface (but not v.v.) *)
    
    (* Here's how we do vlan resyncing:
       We take a VLAN master and record (i) the n/w it's on; (ii) it's VLAN tag; (iii) the network of the pif that underlies the VLAN [e.g. eth0 underlies eth0.25]
       We then look to see whether we already have a VLAN record that is (i) on the same network; (ii) has the same tag; and (iii) also has a pif underlying it on the same network
       If we do not already have a VLAN that falls into this category then we make one (as long as we already have a suitable pif to base the VLAN off -- if we don't have such a
       PIF [e.g. if the master has eth0.25 and we don't have eth0] then we do nothing)
    *)
    Helpers.call_api_functions ~__context
      (fun rpc session_id ->
	 debug "Resynchronising VLANs";

	 let me = !Xapi_globs.localhost_ref in
	 let pool = List.hd (Db.Pool.get_all ~__context) in
	 let master = Db.Pool.get_master ~__context ~self:pool in

	 let all_pifs = Db.PIF.get_records_where ~__context ~expr:Db_filter_types.True in
	 let all_master_pifs = List.filter (fun (_, prec) -> prec.API.pIF_host=master) all_pifs in
	 let my_pifs = List.filter (fun (_, pif) -> pif.API.pIF_host=me) all_pifs in

	 let master_vlan_pifs = List.filter (fun (_,prec) -> prec.API.pIF_VLAN <> -1L) all_master_pifs in
	 let my_vlan_pifs = List.filter (fun (_,prec) -> prec.API.pIF_VLAN <> -1L) my_pifs in

	 let get_network_of_pif_underneath_vlan vlan_pif_ref =
	   let pif_underneath_vlan = Helpers.get_pif_underneath_vlan ~__context vlan_pif_ref in
	   let network_of_pif_underneath_vlan = Db.PIF.get_network ~__context ~self:pif_underneath_vlan in
	   network_of_pif_underneath_vlan in

	 let maybe_create_vlan_pif_for_me (master_pif_ref, master_pif_rec) =
	   (* check to see if I have any existing pif(s) that for the specified device, network, vlan... *)
	   let existing_pif = List.filter 
	     (fun (my_pif_ref,my_pif_record) -> 
		(* Is my VLAN PIF that we're considering (my_pif_ref) the one that corresponds to the master_pif we're considering (master_pif_ref)? *)
		true 
	         && my_pif_record.API.pIF_network = master_pif_rec.API.pIF_network 
		 && my_pif_record.API.pIF_VLAN = master_pif_rec.API.pIF_VLAN
		 && ((get_network_of_pif_underneath_vlan my_pif_ref) = (get_network_of_pif_underneath_vlan master_pif_ref)) 
	     ) my_vlan_pifs in
	   (* if I don't have any such pif(s) then make one: *)
	   if List.length existing_pif = 0 
	   then
	     begin
	       (* On the master, we find the pif, p, that underlies the VLAN (e.g. "eth0" underlies "eth0.25") and then find the network
		  that p's on:
	       *)
	       let network_of_pif_underneath_vlan_on_master = get_network_of_pif_underneath_vlan master_pif_ref in
	       match List.filter (fun (_,prec) -> prec.API.pIF_network=network_of_pif_underneath_vlan_on_master ) my_pifs with
		   [] -> () (* we have no PIF on which to make the vlan; do nothing *)
		 | [(pif_ref,_)] -> (* this is the PIF on which we want to base our vlan record; let's make it *)
		     ignore (Client.VLAN.create ~rpc ~session_id ~tagged_PIF:pif_ref ~tag:master_pif_rec.API.pIF_VLAN ~network:master_pif_rec.API.pIF_network)
		 | _ -> () (* this should never happen cos we should never have more than one of _our_ pifs on the same nework *)
	     end in 
	 (* for each of the master's pifs, create a corresponding one on this host if necessary *)
	 List.iter maybe_create_vlan_pif_for_me master_vlan_pifs
      )

(* CA-25162: Dechainify VLANs. We're actually doing this for _all_
 * PIFs, not just those relevant to localhost. Mostly this will be
 * a no-op, and it shouldn't matter if we fix problems for other hosts
 * here, and it covers the case where we're a slave and the master has
 * broken vlans which need to be corrected before we try to replicate
 * them *)
let fix_chained_vlans ~__context =
  let pifs = Db.PIF.get_all_records ~__context in 
  let (vlan_pifs,underlying_pifs) = List.partition (fun (_,pifr) -> pifr.API.pIF_VLAN >= 0L) pifs in
  List.iter (fun (vlan_pif_ref,vlan_pif_record) ->
    let pif_underneath_vlan = Helpers.get_pif_underneath_vlan ~__context vlan_pif_ref in
    if not (List.exists (fun (pif_ref,_) -> pif_ref = pif_underneath_vlan) underlying_pifs) then begin
      (* There's a problem - the underlying PIF of the vlan might be a vlan itself (or might not exist)
	 Find the real underlying PIF by matching the host and device *)
      try
	let (real_pif_ref,real_pif_rec) = List.find (fun (_,pif_rec) -> 
	  pif_rec.API.pIF_host = vlan_pif_record.API.pIF_host &&
	    pif_rec.API.pIF_device = vlan_pif_record.API.pIF_device) underlying_pifs in
	let vlan = Db.PIF.get_VLAN_master_of ~__context ~self:vlan_pif_ref in
	warn "Resetting tagged PIF of VLAN %s, previously was %s" (Ref.string_of vlan) (Ref.string_of pif_underneath_vlan);
	Db.VLAN.set_tagged_PIF ~__context ~self:vlan ~value:real_pif_ref
      with _ ->
	(* Can't find an underlying PIF - delete the VLAN record. This is pretty unlikely. *)
	error "Destroying dangling VLAN and associated PIF record - the underlying device has disappeared";
	let vlan = Db.PIF.get_VLAN_master_of ~__context ~self:vlan_pif_ref in
	Db.VLAN.destroy ~__context ~self:vlan;
	Db.PIF.destroy ~__context ~self:vlan_pif_ref
    end) vlan_pifs

(*************** update database tools ******************)

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

  let all_my_domains = Xc.domain_getinfolist xc 0 in
  let my_active_domains = List.filter (fun dinfo -> (not dinfo.Xc.dying) && (not dinfo.Xc.shutdown)) all_my_domains in
  let my_shutdown_domains = List.filter (fun dinfo -> dinfo.Xc.shutdown) all_my_domains in

  let this_host = Helpers.get_localhost __context in
  (* CA-22309: consider this host to 'own' a domain if:
     * it is resident_on me
     * it is scheduled_to_be_resident_on me AND NOT resident_on somewhere else: CA-29412 *)
  let all_resident_on_vms = Db.VM.get_records_where ~__context ~expr:(Db_filter_types.Eq (Db_filter_types.Field "resident_on", Db_filter_types.Literal (Ref.string_of this_host))) in
  let all_scheduled_to_be_resident_on_vms = Db.VM.get_records_where ~__context ~expr:(Db_filter_types.Eq (Db_filter_types.Field "scheduled_to_be_resident_on", Db_filter_types.Literal (Ref.string_of this_host))) in
  (* Remove all the scheduled_to_be_resident_on VMs which are resident_on somewhere since that host 'owns' them.
     NB if resident_on this host the VM will still be counted in the all_resident_on_vms set *)
  let really_my_scheduled_to_be_resident_on_vms = 
    List.filter (fun (_, vm_r) -> not (Db.is_valid_ref vm_r.API.vM_resident_on)) all_scheduled_to_be_resident_on_vms in
  let all_vms_assigned_to_me = Listext.List.setify (all_resident_on_vms @ really_my_scheduled_to_be_resident_on_vms) in

  let all_vbds = Db.VBD.get_records_where ~__context ~expr:Db_filter_types.True in
  let all_vifs = Db.VIF.get_records_where ~__context ~expr:Db_filter_types.True in

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
    Uuid.to_string (Uuid.uuid_of_int_array dinfo.Xc.handle) in

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
			if Db.is_valid_ref vbd && not (Db.VBD.get_empty ~__context ~self:vbd)
			then Events.Resync.vbd ~__context token vmref vbd
	      with e ->
		warn "Caught error resynchronising VBD: %s" (ExnHelper.string_of_exn e)) vm_vbds;
	 let vm_vifs = vmrec.API.vM_VIFs in
	 List.iter 
	   (fun vif ->
	      try
			if Db.is_valid_ref vif
			then Events.Resync.vif ~__context token vmref vif
	      with e ->
		warn "Caught error resynchronising VIF: %s" (ExnHelper.string_of_exn e)) vm_vifs
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
    if dinfo.Xc.paused && not(dinfo.Xc.shutdown) && dinfo.Xc.cpu_time = 0L && 
      (vmrec.API.vM_power_state <> `Paused) then begin
	warn "domain id %d uuid %s is paused but not in the database as paused; assuming it's broken; rebooting" 
	  dinfo.Xc.domid (uuid_from_vmref vmref);
	(* Mark the domain as shutdown(reboot), the power state as running and inject
	   a fake event into the event system. This should provoke the event thread into 
	   restarting the VM *)
	Xc.domain_shutdown xc dinfo.Xc.domid Xc.Reboot;
	set_db_state_and_domid vmref `Running dinfo.Xc.domid;
	Events.callback_release xal dinfo.Xc.domid 
      end else begin
	(* Reset the power state, this also clears VBD operations etc *)
	let state = if dinfo.Xc.paused then `Paused else `Running in
	set_db_state_and_domid vmref state dinfo.Xc.domid;
      end;
    (* Now sync devices *)
    debug "syncing devices and registering vm for monitoring: %s" (uuid_from_dinfo dinfo);
    let uuid = Uuid.uuid_of_int_array dinfo.Xc.handle in
	sync_devices dinfo;
	(* Update the VM's guest metrics since: (i) while we were offline we may
	   have missed an update; and (ii) if the tools .iso has been updated then
	   we wish to re-evaluate whether we believe the VMs have up-to-date
	   tools *)

	Events.guest_agent_update xal dinfo.Xc.domid (uuid_from_dinfo dinfo);
	(* Now register with monitoring thread *)

      Monitor_rrds.load_rrd ~__context (Uuid.to_string uuid) false
  in

  (* Process a managed domain that exists here, but is in the shutdown state *)
  let managed_domain_shutdown dinfo =
    debug "found shutdown domain; trying to clean-up: %s" (uuid_from_dinfo dinfo);
    let vmref,vmrec = vmrefrec_of_dinfo dinfo in
    if vmrec.API.vM_resident_on = this_host then begin
      debug "VM is apparently resident on this host; injecting a fake event into the event thread";
      Events.callback_release xal dinfo.Xc.domid 
    end else begin
      debug "VM is not resident on this host; destroying remnant of managed domain";
      Domain.destroy ~xc ~xs dinfo.Xc.domid
    end in
  
  (* Process an "unmanaged domain" that's running here *)
  let unmanaged_domain_running dinfo =
    debug "killing umanaged domain: %s" (uuid_from_dinfo dinfo);
    Domain.destroy ~xc ~xs dinfo.Xc.domid (* bye-bye... *) in

  let all_vm_refs = List.map fst all_vms_assigned_to_me in
  let have_record_for dinfo = try let vmref,_ = vmrefrec_of_dinfo dinfo in true with _ -> false in

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

(* record host memory properties in database *)
let record_host_memory_properties ~__context =
	let self = !Xapi_globs.localhost_ref in
	let total_memory_bytes =
		with_xc (fun xc -> Memory.get_total_memory_bytes ~xc) in
	let metrics = Db.Host.get_metrics ~__context ~self in
	Db.Host_metrics.set_memory_total ~__context ~self:metrics ~value:total_memory_bytes;
	let boot_memory_file = Xapi_globs.initial_host_free_memory_file in
	let boot_memory_string =
		try
			Some (Unixext.read_whole_file_to_string boot_memory_file)
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

(** Important in case the server restarts in the middle of a provision: potentially leaking
    dom0 block-attached VBDs. This will remove them again. *)
let remove_all_leaked_vbds __context =
  let localhost = Helpers.get_localhost ~__context in
  let vms = Db.Host.get_resident_VMs ~__context ~self:localhost in
  let control_domains = List.filter (fun self -> Db.VM.get_is_control_domain ~__context ~self) vms in
  (* there should only be one control domain per host *)
  List.iter 
    (fun control ->
       let vbds = Db.VM.get_VBDs ~__context ~self:control in
       let leaked = List.filter (Attach_helpers.has_vbd_leaked __context) vbds in
       (* Attempt to unplug and destroy them if possible *)
       Helpers.call_api_functions ~__context
	 (fun rpc session_id ->
	    List.iter 
	      (Helpers.log_exn_continue "removing leaked dom0 block-attached VBD"
		 (fun self ->
		    let device = Db.VBD.get_device ~__context ~self in
		    debug "Attempting to unplug dom0 block-attached VBD device %s: %s"
		      device (Ref.string_of self);
		    Helpers.log_exn_continue "attempting to hot-unplug block-attached VBD"
		      (fun self -> Client.VBD.unplug rpc session_id self) self;
		    debug "Attempting to destroy dom0 block-attached VBD device %s: %s"
		      device (Ref.string_of self);
		    (* try this anyway: unplug could have failed if it wasn't plugged in *)
		    Client.VBD.destroy rpc session_id self)) leaked)
    ) control_domains

(** Make sure the PIF we're using as a management interface is marked as attached
    otherwise we might blow it away by accident *)
(* CA-23803:
 * As well as marking the management interface as attached, mark any other important 
 * interface (defined by what is brought up before xapi starts) as attached too.
 * For example, this will prevent needless glitches in storage interfaces.
 *)
let resynchronise_pif_currently_attached ~__context =
  let localhost = Helpers.get_localhost () in
  (* See which PIFs were brought up at start of day, according to the inventory file *)
  let pifs_brought_up =
    try
      let bridges_already_up = Xapi_pif.read_bridges_from_inventory () in
      debug "HA interfaces: [%s]" (String.concat "; " bridges_already_up);
      (* Create a list pairing each PIF with the bridge for the network that it is on *)
      let pifs_with_bridges = List.map (fun (pif,pifr) -> (pif, Db.Network.get_bridge ~__context ~self:pifr.API.pIF_network)) (Db.PIF.get_all_records ~__context) in
      (* Retain the (pif, bridge) pairs where the bridge was listed in the inventory as an HA interface *)
      List.filter (fun (_,bridge_name) -> List.mem bridge_name bridges_already_up) pifs_with_bridges
    with
      Xapi_inventory.Missing_inventory_key _ -> []
    in
  List.iter
    (fun self ->
       let is_management_pif = Xapi_pif.is_my_management_pif ~__context ~self in
       let was_pif_brought_up_at_start_of_day = List.mem self (List.map fst pifs_brought_up) in
       (* Mark important interfaces as attached *)
       let mark_as_attached = is_management_pif || was_pif_brought_up_at_start_of_day in
       Db.PIF.set_currently_attached ~__context ~self ~value:mark_as_attached;
       Db.PIF.set_management ~__context ~self ~value:is_management_pif;
       debug "Marking PIF device %s as %s" (Db.PIF.get_device ~__context ~self) (if mark_as_attached then "attached" else "offline")
    )
    (Db.Host.get_PIFs ~__context ~self:localhost)

(* Update the database to reflect current state. Called for both start of day and after
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

  (* create localhost record if doesn't already exist *)
  switched_sync Xapi_globs.sync_create_localhost (fun () -> 
    debug "creating localhost";
    create_localhost ~__context; 
  );

  (* record who we are in xapi_globs *)
  Xapi_globs.localhost_ref := Helpers.get_localhost ~__context;

  (* Load the host rrd *)
  Monitor_rrds.load_rrd ~__context (Helpers.get_localhost_uuid ()) true;

  (* refresh host info fields *)
  switched_sync Xapi_globs.sync_refresh_localhost_info (fun () -> 
    refresh_localhost_info ~__context;
  );

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
    Create_misc.ensure_domain_zero_records ~__context;
  );

  let localhost = Helpers.get_localhost ~__context in

  switched_sync Xapi_globs.sync_crashdump_resynchronise (fun () ->
    debug "resynchronising host crashdumps";
    Xapi_host_crashdump.resynchronise ~__context ~host:localhost;
  );

  switched_sync Xapi_globs.sync_update_vms (fun () -> 
    debug "updating VM states";
    with_xal (fun xal -> update_vms ~xal ~__context);
  );

  switched_sync Xapi_globs.sync_remove_leaked_vbds (fun () ->
    debug "removing any leaked dom0 block-attached VBDs (if any)";
    remove_all_leaked_vbds __context;
  );

(*
  debug "resynchronising db with host physical interfaces";
  update_physical_networks ~__context;
*)

  switched_sync Xapi_globs.sync_dechainify_vlans (fun () ->
    debug "dechainifying VLANs";
    fix_chained_vlans ~__context
  );

  switched_sync Xapi_globs.sync_copy_pifs_from_master (fun () ->
    debug "resynchronising bonded and vlan pif records with pool master";
(* If you're considering merging up bonds/vlans into a single
   phase, then be careful. I separated them explicitly because previously
   the vlan copy-phase was using out-of-date PIF.get_all_records for itself,
   even though new bonds had been inherited and it needed to
   explicitly refresh its list of PIFs before proceeding to the VLAN phase.. *)
    try
      copy_bonds_from_master ~__context;
      copy_vlans_from_master ~__context;
    with e -> (* Errors here are non-data-corrupting hopefully, so we'll just carry on regardless... *)
      error "Caught exception syncing PIFs from the master: %s" (ExnHelper.string_of_exn e);
      log_backtrace ()
  );

  switched_sync Xapi_globs.sync_resynchronise_pif_currently_attached (fun () ->
    debug "resynchronising PIF.currently_attached";
    resynchronise_pif_currently_attached ~__context;
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
