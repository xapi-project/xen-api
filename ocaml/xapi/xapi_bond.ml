(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
 * GNU Lesser General Public License for more details.
 *)
module D = Debug.Debugger(struct let name="xapi" end)
open D

open Listext
open Threadext

(* Returns the name of a new bond device, which is the string "bond" followed
 * by the smallest integer > 0 that does not yet appear in a bond name on this host. *)
let choose_bond_device_name ~__context ~host =
	(* list all the PIFs on this host *)
	let pifs = List.filter (fun self -> Db.PIF.get_host ~__context ~self = host) (Db.PIF.get_all ~__context) in
	let devices = List.map (fun self -> Db.PIF.get_device ~__context ~self) pifs in
	let rec choose n =
		let name = Printf.sprintf "bond%d" n in
		if List.mem name devices
		then choose (n + 1)
		else name in
	choose 0

let copy_configuration ~__context from_pif to_pif =
	debug "Moving IP configuration from PIF %s to PIF %s" (Db.PIF.get_uuid ~__context ~self:from_pif)
		(Db.PIF.get_uuid ~__context ~self:to_pif);
	let mode =	Db.PIF.get_ip_configuration_mode ~__context ~self:from_pif in
	let ip = Db.PIF.get_IP ~__context ~self:from_pif in
	let netmask = Db.PIF.get_netmask ~__context ~self:from_pif in
	let gateway = Db.PIF.get_gateway ~__context ~self:from_pif in
	let dns = Db.PIF.get_DNS ~__context ~self:from_pif in
	Db.PIF.set_ip_configuration_mode ~__context ~self:to_pif ~value:mode;
	Db.PIF.set_IP ~__context ~self:to_pif ~value:ip;
	Db.PIF.set_netmask ~__context ~self:to_pif ~value:netmask;
	Db.PIF.set_gateway ~__context ~self:to_pif ~value:gateway;
	Db.PIF.set_DNS ~__context ~self:to_pif ~value:dns

(* Determined local VMs: candidates for moving to the bond *)
let get_local_vms ~__context host =
	let check vm =
		(* only move the VIFs of a VM if this VM is resident, or can _only_ start, on _this_ host *)
		let hosts = Xapi_vm.get_possible_hosts ~__context ~vm in
		let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
		resident_on = host || (List.mem host hosts && List.length hosts = 1)
	in
	let local_vms = List.filter check (Db.VM.get_all ~__context) in
	debug "Found these local VMs: %s" (String.concat ", " (List.map (fun v -> Db.VM.get_uuid ~__context ~self:v) local_vms));
	local_vms

let move_vlan ~__context new_slave old_vlan vifs =
	let old_master = Db.VLAN.get_untagged_PIF ~__context ~self:old_vlan in
	let vlan_tag = Db.VLAN.get_tag ~__context ~self:old_vlan in
	let old_network = Db.PIF.get_network ~__context ~self:old_master in
	let plugged = Db.PIF.get_currently_attached ~__context ~self:old_master in
	if plugged then
		Nm.bring_pif_down ~__context old_master;

	(* Only create new objects if the tag does not yet exist *)
	let new_vlan, new_master =
		let existing_vlans = Db.PIF.get_VLAN_slave_of ~__context ~self:new_slave in
		let same_tag = List.filter (fun v -> Db.VLAN.get_tag ~__context ~self:v = vlan_tag) existing_vlans in
		match same_tag with
		| new_vlan :: _ ->
			(* VLAN with this tag already on bond *)
			debug "VLAN already present";
			let new_master = Db.VLAN.get_untagged_PIF ~__context ~self:new_vlan in
			new_vlan, new_master
		| [] ->
			(* VLAN with this tag not yet on bond *)
			debug "Creating new VLAN %d on bond" (Int64.to_int vlan_tag);
			(* Copy the fields from the existing VLAN master PIF *)
			let mTU = Db.PIF.get_MTU ~__context ~self:old_master in
			let host = Db.PIF.get_host ~__context ~self:old_master in
			(* Update device name *)
			let device = Db.PIF.get_device ~__context ~self:new_slave in

			(* Find or create new Network for new VLAN *)
			let name_label = "VLAN " ^ (Int64.to_string vlan_tag) ^ " on " ^ device in
			let network = match Db.Network.get_by_name_label ~__context ~label:name_label with
			| [] -> Xapi_network.create ~__context ~name_label ~name_description:"" ~mTU ~other_config:[] ~tags:[]
			| network :: _ -> network
			in

			(* Create new VLAN master PIF and VLAN objects *)
			let t = Xapi_pif.make_tables ~__context ~host in
			let new_vlan = Ref.make () and vlan_uuid = Uuid.to_string (Uuid.make_uuid ()) in
			let new_master = Xapi_pif.introduce_internal ~physical:false ~t ~__context ~host ~network
				~mAC:Xapi_vlan.vlan_mac ~device ~vLAN:vlan_tag ~mTU ~vLAN_master_of:new_vlan () in
			Db.VLAN.create ~__context ~ref:new_vlan ~uuid:vlan_uuid ~tagged_PIF:new_slave ~untagged_PIF:new_master ~tag:vlan_tag ~other_config:[];
			new_vlan, new_master
	in
	let new_network = Db.PIF.get_network ~__context ~self:new_master in

	(* Destroy old VLAN and VLAN-master objects *)
	debug "Destroying old VLAN %d" (Int64.to_int vlan_tag);
	Db.VLAN.destroy ~__context ~self:old_vlan;
	Db.PIF.destroy ~__context ~self:old_master;

	(* Plug again if plugged before the move *)
	if plugged then begin
		debug "Plugging new VLAN";
		Nm.bring_pif_up ~__context new_master
	end;

	(* Move VIFs to new VLAN *)
	ignore (List.map (Xapi_vif.move ~__context ~network:new_network) vifs)

let move_tunnel ~__context host new_transport_PIF old_tunnel =
	let old_access_PIF = Db.Tunnel.get_access_PIF ~__context ~self:old_tunnel in
	let network = Db.PIF.get_network ~__context ~self:old_access_PIF in
	let plugged = Db.PIF.get_currently_attached ~__context ~self:old_access_PIF in
	if plugged then
		Nm.bring_pif_down ~__context old_access_PIF;

	(* Create new tunnel object and access PIF *)
	let new_tunnel, new_access_PIF =
		Xapi_tunnel.create_internal ~__context ~transport_PIF:new_transport_PIF ~network ~host in
	debug "Created new tunnel %s on bond" (Ref.string_of new_tunnel);

	(* Destroy old VLAN and VLAN-master objects *)
	debug "Destroying old tunnel %s" (Ref.string_of old_tunnel);
	Xapi_tunnel.destroy ~__context ~self:old_tunnel;

	(* Plug again if plugged before the move *)
	if plugged then begin
		debug "Plugging moved tunnel";
		Nm.bring_pif_up ~__context new_access_PIF
	end

let move_management ~__context from_pif to_pif =
	Nm.bring_pif_up ~__context ~management_interface:true to_pif;
	let network = Db.PIF.get_network ~__context ~self:to_pif in
	let bridge = Db.Network.get_bridge ~__context ~self:network in
	Xapi_host.change_management_interface ~__context bridge;
	Xapi_pif.update_management_flags ~__context ~host:(Helpers.get_localhost ~__context)

let get_vlan_vifs ~__context vlan =
	let tagged_pif = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
	let vlan_network = Db.PIF.get_network ~__context ~self:tagged_pif in
	Db.Network.get_VIFs ~__context ~self:vlan_network

let stuff_to_move_up ~__context members host =
	(* VIFS *)
	let member_networks = List.map (fun pif -> Db.PIF.get_network ~__context ~self:pif) members in
	let local_vms = get_local_vms ~__context host in
	let local_vifs = List.concat (List.map (fun vm -> Db.VM.get_VIFs ~__context ~self:vm) local_vms) in
	let slave_vifs_to_move = List.filter (fun vif -> List.mem (Db.VIF.get_network ~__context ~self:vif) member_networks) local_vifs in
	(* VLANS *)
	let local_vlans = List.concat (List.map (fun pif -> Db.PIF.get_VLAN_slave_of ~__context ~self:pif) members) in
	let vlans_with_vifs = List.map (fun vlan -> vlan, List.intersect (get_vlan_vifs ~__context vlan) local_vifs) local_vlans in
	(* Tunnels *)
	let local_tunnels = List.concat (List.map (fun pif -> Db.PIF.get_tunnel_transport_PIF_of ~__context ~self:pif) members) in
	(* return both *)
	slave_vifs_to_move, vlans_with_vifs, local_tunnels

let	rec unplug_vifs ~__context l = function
	| [] -> l
	| hd :: tl ->
		try
			(* try to unplug the VIF *)
			Xapi_vif.unplug ~__context ~self:hd;
			unplug_vifs ~__context (hd :: l) tl
		with e ->
			(* rollback and re-raise exception *)
			List.iter (fun vif -> Xapi_vif.plug ~__context ~self:vif) l;
			raise e (* raise an appropriate exception instead!! *)

let fix_bond ~__context ~bond =
	let bond_rec = Db.Bond.get_record ~__context ~self:bond in
	let members = bond_rec.API.bond_slaves in
	let master = bond_rec.API.bond_master in
	let network = Db.PIF.get_network ~__context ~self:master in
	let host = Db.PIF.get_host ~__context ~self:master in

	(* Try unplugging any plugged VIFs that would need to be moved *)
	let local_vifs_to_move, vlans_with_vifs, local_tunnels = stuff_to_move_up ~__context members host in

	if local_vifs_to_move <> [] || vlans_with_vifs <> [] then begin
		let vifs_to_unplug = List.filter (fun vif -> Db.VIF.get_currently_attached ~__context ~self:vif = true)
			(local_vifs_to_move @ (List.flatten (List.map (fun (a,b) -> b) vlans_with_vifs))) in
		ignore (unplug_vifs ~__context [] vifs_to_unplug);

		(* Move VLANs, with their VIFs, from members to master *)
		debug "Moving VLANs, with their VIFs, from slaves to master";
		List.iter (fun (vlan, vifs) -> move_vlan ~__context master vlan vifs) vlans_with_vifs;

		(* Move tunnels from members to master *)
		debug "Moving tunnels from slaves to master";
		List.iter (move_tunnel ~__context host master) local_tunnels;

		(* Move VIFs from members to master *)
		debug "Moving VIFs from slaves to master";
		List.iter (Xapi_vif.move ~__context ~network) local_vifs_to_move
	end;
	begin match List.filter (fun p -> Db.PIF.get_management ~__context ~self:p) members with
	| management_pif :: _ -> 
		(* The bond contains the management interface: move management to the master.
		 * This interface will be plugged automatically. *)
		debug "Moving management from slave to master";
		move_management ~__context management_pif master
	| [] -> ()
	end

let string_of_mode = function
	| `balanceslb -> "balance-slb"
	| `activebackup -> "active-backup"


(* Protect a bunch of local operations with a mutex *)
let local_m = Mutex.create ()
let with_local_lock f = Mutex.execute local_m f

let create ~__context ~network ~members ~mAC ~mode =
	let host = Db.PIF.get_host ~__context ~self:(List.hd members) in
	Xapi_pif.assert_no_other_local_pifs ~__context ~host ~network;

	(* Validate MAC parameter; note an empty string is OK here, since that means 'inherit MAC from
	 * primary slave PIF' (see below) *)
	if mAC <> "" && (not (Helpers.is_valid_MAC mAC)) then
		raise (Api_errors.Server_error (Api_errors.mac_invalid, [mAC]));

	(* Prevent someone supplying the same PIF multiple times and bypassing the
	 * number of bond members check *)
	let members = List.setify members in
	let master = Ref.make () in
	let bond = Ref.make () in

	with_local_lock (fun () ->
		(* Validation constraints: *)
		(* 1. Members must not be in a bond already *)
		(* 2. Members must not have a VLAN tag set *)
		(* 3. Members must not be tunnel access PIFs *)
		(* 4. Referenced PIFs must be on the same host *)
		(* 5. There must be more than one member for the bond ( ** disabled for now) *)
		List.iter (fun self ->
			let bond = Db.PIF.get_bond_slave_of ~__context ~self in
			let bonded = try ignore(Db.Bond.get_uuid ~__context ~self:bond); true with _ -> false in
			if bonded
			then raise (Api_errors.Server_error (Api_errors.pif_already_bonded, [ Ref.string_of self ]));
			if Db.PIF.get_VLAN ~__context ~self <> -1L
			then raise (Api_errors.Server_error (Api_errors.pif_vlan_exists, [ Db.PIF.get_device_name ~__context ~self] ));
			if Db.PIF.get_tunnel_access_PIF_of ~__context ~self <> []
			then raise (Api_errors.Server_error (Api_errors.is_tunnel_access_pif, [Ref.string_of self]));
		) members;
		let hosts = List.map (fun self -> Db.PIF.get_host ~__context ~self) members in
		if List.length (List.setify hosts) <> 1
		then raise (Api_errors.Server_error (Api_errors.pif_cannot_bond_cross_host, []));
		(*
		if List.length members < 2
		then raise (Api_errors.Server_error (Api_errors.pif_bond_needs_more_members, []));
		*)

		let local_vifs_to_move, vlans_with_vifs, local_tunnels = stuff_to_move_up ~__context members host in

		(* Try unplugging any plugged VIFs that would need to be moved *)
		let vifs_to_unplug = List.filter (fun vif -> Db.VIF.get_currently_attached ~__context ~self:vif = true)
			(local_vifs_to_move @ (List.flatten (List.map (fun (a,b) -> b) vlans_with_vifs))) in
		ignore (unplug_vifs ~__context [] vifs_to_unplug);

		(* Collect information *)
		let management_pif =
			match List.filter (fun p -> Db.PIF.get_management ~__context ~self:p) members with
			| management_pif :: _ -> Some management_pif
			| [] -> None
		in
		let primary_slave =
			(* The primary slave is the management PIF, or the first member with IP configuration,
			 * or otherwise simply the first member in the list. *)
			match management_pif with
			| Some management_pif -> management_pif
			| None ->
				try
					List.hd (List.filter (fun pif -> Db.PIF.get_ip_configuration_mode ~__context ~self:pif <> `None) members)
				with _ ->
					List.hd members
		in
		let mAC =
			if mAC <> "" then
				mAC
			else
				Db.PIF.get_MAC ~__context ~self:primary_slave
		in
		let disallow_unplug =
			List.fold_left (fun a m -> Db.PIF.get_disallow_unplug ~__context ~self:m || a) false members
		in

		(* Create master PIF and Bond objects *)
		let device = choose_bond_device_name ~__context ~host in
		let device_name = device in
		Db.PIF.create ~__context ~ref:master ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
			~device ~device_name ~network ~host ~mAC ~mTU:(-1L) ~vLAN:(-1L) ~metrics:Ref.null
			~physical:false ~currently_attached:false
			~ip_configuration_mode:`None ~iP:"" ~netmask:"" ~gateway:"" ~dNS:"" ~bond_slave_of:Ref.null
			~vLAN_master_of:Ref.null ~management:false ~other_config:[] ~disallow_unplug:false;
		Db.Bond.create ~__context ~ref:bond ~uuid:(Uuid.to_string (Uuid.make_uuid ())) ~master:master ~other_config:[]
			~primary_slave ~mode;

		(* Set the PIF.bond_slave_of fields of the members.
		 * The value of the Bond.slaves field is dynamically computed on request. *)
		List.iter (fun slave -> Db.PIF.set_bond_slave_of ~__context ~self:slave ~value:bond) members;

		(* Copy the IP configuration of the primary member to the master *)
		copy_configuration ~__context primary_slave master;

		(* Temporary measure for compatibility with current interface-reconfigure.
		 * Remove once interface-reconfigure has been updated to recognise bond.mode. *)
		Db.PIF.add_to_other_config ~__context ~self:master ~key:"bond-mode" ~value:(string_of_mode mode);

		(* Move VLANs, with their VIFs, from members to master *)
		debug "Moving VLANs, with their VIFs, from slaves to master";
		List.iter (fun (vlan, vifs) -> move_vlan ~__context master vlan vifs) vlans_with_vifs;

		(* Move tunnels from members to master *)
		debug "Moving tunnels from slaves to master";
		List.iter (move_tunnel ~__context host master) local_tunnels;

		(* Move VIFs from members to master *)
		debug "Moving VIFs from slaves to master";
		List.iter (Xapi_vif.move ~__context ~network) local_vifs_to_move;

		begin match management_pif with
		| Some management_pif ->
			(* The bond contains the management interface: move management to the master.
			 * This interface will be plugged automatically. *)
			debug "Moving management from slave to master";
			move_management ~__context management_pif master
		| None ->
			(* Plug master if one of the slaves was plugged *)
			let plugged = List.fold_left (fun a m -> Db.PIF.get_currently_attached ~__context ~self:m || a) false members in
			if plugged then begin
				debug "Plugging the bond";
				Nm.bring_pif_up ~__context master
			end
		end;

		(* Reset IP configuration and disallow_unplug of members *)
		debug "Resetting IP config and disallow_unplug on slaves";
		List.iter (fun pif ->
			Db.PIF.set_ip_configuration_mode ~__context ~self:pif ~value:`None;
			Db.PIF.set_disallow_unplug ~__context ~self:pif ~value:false)
			members;
	);
	(* return a ref to the new Bond object *)
	bond

let destroy ~__context ~self =
	with_local_lock (fun () ->
		let master = Db.Bond.get_master ~__context ~self in
		let members = Db.Bond.get_slaves ~__context ~self in
		let plugged = Db.PIF.get_currently_attached ~__context ~self:master in
		let master_network = Db.PIF.get_network ~__context ~self:master in
		let host = Db.PIF.get_host ~__context ~self:master in
		let primary_slave = Db.Bond.get_primary_slave ~__context ~self in
		let primary_slave_network = Db.PIF.get_network ~__context ~self:primary_slave in

		(* Try unplugging any plugged VIFs of running VMs that would need to be moved *)
		let local_vms = get_local_vms ~__context host in
		let local_vifs = List.concat (List.map (fun vm -> Db.VM.get_VIFs ~__context ~self:vm) local_vms) in
		let local_vifs_on_master_network =
			List.filter (fun vif -> Db.VIF.get_network ~__context ~self:vif = master_network) local_vifs in
		debug "Found these local VIFs: %s" (String.concat ", " (List.map (fun v -> Db.VIF.get_uuid ~__context ~self:v) local_vifs));

		let local_vlans = Db.PIF.get_VLAN_slave_of ~__context ~self:master in
		let vlans_with_vifs = List.map (fun vlan -> vlan, List.intersect (get_vlan_vifs ~__context vlan) local_vifs) local_vlans in
		let local_tunnels = Db.PIF.get_tunnel_transport_PIF_of ~__context ~self:master in

		ignore (unplug_vifs ~__context [] (local_vifs_on_master_network @ (List.flatten (List.map (fun (a,b) -> b) vlans_with_vifs))));

		(* Copy IP configuration from master to primary member *)
		copy_configuration ~__context master primary_slave;

		(* Move VIFs from master to slaves *)
		debug "Moving VIFs from master to slaves";
		List.map (Xapi_vif.move ~__context ~network:primary_slave_network) local_vifs_on_master_network;

		(* Move VLANs down *)
		debug "Moving VLANs from master to slaves";
		List.iter (fun (vlan, vifs) -> move_vlan ~__context primary_slave vlan vifs) vlans_with_vifs;

		(* Move tunnels down *)
		debug "Moving tunnels from master to slaves";
		List.iter (move_tunnel ~__context host primary_slave) local_tunnels;

		if Db.PIF.get_management ~__context ~self:master = true then begin
			(* The master is the management interface: move management to first slave *)
			debug "Moving management from master to slaves";
			move_management ~__context master primary_slave;
		end else begin
			(* Plug the primary slave if the master was plugged *)
			if plugged then
				Nm.bring_pif_up ~__context primary_slave
		end;

		if Db.PIF.get_disallow_unplug ~__context ~self:master = true then
			Db.PIF.set_disallow_unplug ~__context ~self:primary_slave ~value:true;

		(* Destroy the Bond and master PIF *)
		Db.Bond.destroy ~__context ~self;
		Db.PIF.destroy ~__context ~self:master;

		(* Clear the PIF.bond_slave_of fields of the members. *)
		List.iter (fun slave -> Db.PIF.set_bond_slave_of ~__context ~self:slave ~value:(Ref.null)) members
	)

let set_mode ~__context ~self ~value =
	Db.Bond.set_mode ~__context ~self ~value;
	let master = Db.Bond.get_master ~__context ~self in

	(* Temporary measure for compatibility with current interface-reconfigure.
	 * Remove once interface-reconfigure has been updated to recognise bond.mode. *)
	Db.PIF.remove_from_other_config ~__context ~self:master ~key:"bond-mode";
	Db.PIF.add_to_other_config ~__context ~self:master ~key:"bond-mode" ~value:(string_of_mode value);

	(* Need to set currently_attached to false, otherwise bring_pif_up does nothing... *)
	Db.PIF.set_currently_attached ~__context ~self:master ~value:false;
	Nm.bring_pif_up ~__context master

