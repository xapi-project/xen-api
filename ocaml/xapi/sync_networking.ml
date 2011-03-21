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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
 * GNU Lesser General Public License for more details.
 *)

open Listext
open Client

module D=Debug.Debugger(struct let name="sync_networking" end)
open D

(** Copy Bonds from master *)
let copy_bonds_from_master ~__context =
	Helpers.call_api_functions ~__context (fun rpc session_id ->
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
				List.map (fun self -> self, Db.PIF.get_MAC ~__context ~self, Db.PIF.get_device ~__context ~self)
				bond.API.bond_slaves in
			(* Take the MAC addr of the bond and figure out whether this is the MAC address of any of the
			 * slaves. If it is then we will use this to ensure that we inherit the MAC address from the _same_
			 * slave when we re-create on the slave *)
			let master_bond_mac = Db.PIF.get_MAC ~__context ~self:bond.API.bond_master in
			(* The bond mode used on the master. We will use the same mode on the slave, when creating a new bond. *)
			let bond_mode = bond.API.bond_mode in
			let master_slaves_with_same_mac_as_bond (* expecting a list of at most 1 here *) =
				List.filter (fun (pifref,mac,device) -> mac=master_bond_mac) slaves_to_mac_and_device_map in
			(* This tells us the device that the master used to inherit the bond's MAC address
			 * (if indeed that is what it did; we set it to None if we think it didn't do this) *)
			let device_of_primary_slave =
				match master_slaves_with_same_mac_as_bond with
				| [] -> None
				| [_,_,device] ->
					debug "Master bond has MAC address derived from %s" device;
					(* found single slave with mac matching bond master =>
					 * this was one that we inherited mac from *)
					Some device
				| _ -> None
			in
			(* Look at the master's slaves and find the corresponding slave PIFs. Note that the slave
			 * might not have the necessary devices: in this case we'll try to make partial bonds *)
			let slave_devices = List.map (fun (_,_,device)->device) slaves_to_mac_and_device_map in
			let my_slave_pifs = List.filter
				(fun (_, pif) -> List.mem pif.API.pIF_device slave_devices && pif.API.pIF_VLAN = (-1L)) my_pifs in

			let my_slave_pif_refs = List.map fst my_slave_pifs in
			(* Do I have a pif that I should treat as a primary pif - 
			 * i.e. the one to inherit the MAC address from on my bond create? *)
			let my_primary_slave =
				match device_of_primary_slave with
				| None -> None (* don't care cos we couldn't even figure out who master's primary slave was *)
				| Some master_primary ->
					begin
						match List.filter (fun (_,pif) -> pif.API.pIF_device=master_primary) my_slave_pifs with
						| [] -> None
						| [pifref,_] ->
							debug "I have found a PIF to use as primary bond slave (will inherit MAC address of bond from this PIF).";
							Some pifref (* this is my pif corresponding to the master's primary slave *)
						| _ -> None
					end
			in
			(* If I do have a pif that I need to treat as my primary slave then I need to put it
			 * first in the list so the bond master will inherit it's MAC address *)
			let my_slave_pif_refs =
				match my_primary_slave with
				| None -> my_slave_pif_refs (* no change *)
				| Some primary_pif -> primary_pif :: (List.filter (fun x-> x<>primary_pif) my_slave_pif_refs) (* remove primary pif ref and stick it on the front *)
			in				 
			match List.filter (fun (_, pif) -> pif.API.pIF_network = network) my_pifs, my_slave_pifs with
			| [], [] ->
				(* No bond currently exists but neither do any slave interfaces -> do nothing *)
				warn "Cannot create bond %s at all: no PIFs exist on slave" bond.API.bond_uuid
			| [], _ ->
				(* No bond currently exists but some slave interfaces do -> create a (partial?) bond *)
				let (_: API.ref_Bond) = Client.Bond.create rpc session_id network my_slave_pif_refs "" bond_mode in ()
			| [ _, { API.pIF_bond_master_of = [ slave_bond ] } ], _ ->
				(* Some bond exists, check whether the existing set of slaves is the same as the potential set *)
				let current_slave_pifs = Db.Bond.get_slaves ~__context ~self:slave_bond in
				if not (List.set_equiv (List.setify current_slave_pifs) (List.setify my_slave_pif_refs)) then
				begin
					debug "Partial bond exists; recreating";
					Client.Bond.destroy rpc session_id slave_bond;
					let (_: API.ref_Bond) = Client.Bond.create rpc session_id network my_slave_pif_refs "" bond_mode in ()
				end
			| [ _, { API.pIF_uuid = uuid } ], _ ->
				warn "Couldn't create bond on slave because PIF %s already on network %s"
					uuid (Db.Network.get_uuid ~__context ~self:network)
		in
		let master_bonds =
			List.filter (fun (_, b) -> List.mem b.API.bond_master (List.map fst all_master_pifs)) all_bonds in
		List.iter (Helpers.log_exn_continue "resynchronising bonds on slave" maybe_create_bond_for_me)
		(List.map snd master_bonds)
	)

(** Copy VLANs from master *)
(* Here's how we do VLAN resyncing:
   We take a VLAN master and record (i) the Network it is on; (ii) its VLAN tag;
   (iii) the Network of the PIF that underlies the VLAN (e.g. eth0 underlies eth0.25).
   We then look to see whether we already have a VLAN record that is (i) on the same Network;
   (ii) has the same tag; and (iii) also has a PIF underlying it on the same Network.
   If we do not already have a VLAN that falls into this category then we make one,
   as long as we already have a suitable PIF to base the VLAN off -- if we don't have such a
   PIF (e.g. if the master has eth0.25 and we don't have eth0) then we do nothing.
*)
let copy_vlans_from_master ~__context =
      Helpers.call_api_functions ~__context
		  (fun rpc session_id ->
			  debug "Resynchronising VLANs";
			  (* 1. Download data from the master, minimising round-trips *)
			  let me = !Xapi_globs.localhost_ref in
			  let pool = Helpers.get_pool ~__context in
			  let master = Db.Pool.get_master ~__context ~self:pool in
			  let pifs = Db.PIF.get_all_records ~__context in
			  let vlans = Db.VLAN.get_all_records ~__context in
			  (* 2. Make lists and lookup tables *)
			  let all_master_pifs = List.filter (fun (_, prec) -> prec.API.pIF_host=master) pifs in
			  let my_pifs = List.filter (fun (_, pif) -> pif.API.pIF_host=me) pifs in
			  let master_vlan_pifs = List.filter (fun (_,prec) -> prec.API.pIF_VLAN <> -1L) all_master_pifs in
			  let my_vlan_pifs = List.filter (fun (_,prec) -> prec.API.pIF_VLAN <> -1L) my_pifs in

			  let get_network_of_pif_underneath_vlan vlan_pif_ref =
				  let pif_r = List.assoc vlan_pif_ref pifs in
				  let vlan = pif_r.API.pIF_VLAN_master_of in
				  let vlan_r = List.assoc vlan vlans in
				  let pif_underneath_vlan = vlan_r.API.vLAN_tagged_PIF in
				  let pif_underneath_vlan_r = List.assoc pif_underneath_vlan pifs in
				  pif_underneath_vlan_r.API.pIF_network in

		let maybe_create_vlan_pif_for_me (master_pif_ref, master_pif_rec) =
			(* check to see if I have any existing pif(s) that for the specified device, network, vlan... *)
			let existing_pif = List.filter (fun (my_pif_ref,my_pif_record) -> 
				(* Is my VLAN PIF that we're considering (my_pif_ref) the one that corresponds to the master_pif we're considering (master_pif_ref)? *)
				true 
				&& my_pif_record.API.pIF_network = master_pif_rec.API.pIF_network 
				&& my_pif_record.API.pIF_VLAN = master_pif_rec.API.pIF_VLAN
				&& ((get_network_of_pif_underneath_vlan my_pif_ref) =
					(get_network_of_pif_underneath_vlan master_pif_ref))
				) my_vlan_pifs in
			(* if I don't have any such pif(s) then make one: *)
			if List.length existing_pif = 0 
			then
				begin
					(* On the master, we find the pif, p, that underlies the VLAN 
					 * (e.g. "eth0" underlies "eth0.25") and then find the network that p's on: *)
					let network_of_pif_underneath_vlan_on_master = get_network_of_pif_underneath_vlan master_pif_ref in
					match List.filter (fun (_,prec) -> prec.API.pIF_network=network_of_pif_underneath_vlan_on_master ) my_pifs with
					| [] -> () (* we have no PIF on which to make the vlan; do nothing *)
					| [(pif_ref,_)] -> (* this is the PIF on which we want to base our vlan record; let's make it *)
						ignore (Client.VLAN.create ~rpc ~session_id ~tagged_PIF:pif_ref
							~tag:master_pif_rec.API.pIF_VLAN ~network:master_pif_rec.API.pIF_network)
					| _ -> () (* this should never happen cos we should never have more than one of _our_ pifs on the same nework *)
				end
		in 
		(* for each of the master's pifs, create a corresponding one on this host if necessary *)
		List.iter maybe_create_vlan_pif_for_me master_vlan_pifs
	)

(** Copy tunnels from master *)
let copy_tunnels_from_master ~__context =
	Helpers.call_api_functions ~__context (fun rpc session_id ->
		debug "Resynchronising tunnels";
		
		let me = !Xapi_globs.localhost_ref in
		let pool = List.hd (Db.Pool.get_all ~__context) in
		let master = Db.Pool.get_master ~__context ~self:pool in

		let all_pifs = Db.PIF.get_records_where ~__context ~expr:Db_filter_types.True in
		let all_master_pifs = List.filter (fun (_, prec) -> prec.API.pIF_host=master) all_pifs in
		let my_pifs = List.filter (fun (_, pif) -> pif.API.pIF_host=me) all_pifs in

		let master_tunnel_pifs = List.filter (fun (_,prec) -> prec.API.pIF_tunnel_access_PIF_of <> []) all_master_pifs in
		let my_tunnel_pifs = List.filter (fun (_,prec) -> prec.API.pIF_tunnel_access_PIF_of <> []) my_pifs in

		let get_network_of_transport_pif access_pif =
			let [tunnel] = Db.PIF.get_tunnel_access_PIF_of ~__context ~self:access_pif in
			let transport_pif = Db.Tunnel.get_transport_PIF ~__context ~self:tunnel in
			Db.PIF.get_network ~__context ~self:transport_pif
		in
		
		let maybe_create_tunnel_for_me (master_pif_ref, master_pif_rec) =
			(* check to see if I have any existing pif(s) that for the specified device, network, vlan... *)
			let existing_pif = List.filter (fun (my_pif_ref,my_pif_record) -> 
				(* Is my VLAN PIF that we're considering (my_pif_ref) the one that corresponds to the master_pif we're considering (master_pif_ref)? *)
				my_pif_record.API.pIF_network = master_pif_rec.API.pIF_network
				) my_tunnel_pifs in
			(* if I don't have any such pif(s) then make one: *)
			if List.length existing_pif = 0 
			then
				begin
					(* On the master, we find the network the tunnel transport PIF is on *)
					let network_of_transport_pif_on_master = get_network_of_transport_pif master_pif_ref in
					match List.filter (fun (_,prec) -> prec.API.pIF_network=network_of_transport_pif_on_master) my_pifs with
					| [] -> () (* we have no PIF on which to make the tunnel; do nothing *)
					| [(pif_ref,_)] -> (* this is the PIF on which we want as transport PIF; let's make it *)
						ignore (Client.Tunnel.create ~rpc ~session_id ~transport_PIF:pif_ref
							~network:master_pif_rec.API.pIF_network)
					| _ -> () (* this should never happen cos we should never have more than one of _our_ pifs on the same nework *)
				end
		in 
		(* for each of the master's pifs, create a corresponding one on this host if necessary *)
		List.iter maybe_create_tunnel_for_me master_tunnel_pifs
	)


let sync_slave_with_master ~__context () =
	if Pool_role.is_master () then () (* if master do nothing *)
	else begin
		debug "resynchronising bonded and vlan pif records with pool master";
		try
			(* Sync VLANs after bonds so we can add VLANs on top of bonded interfaces (but not v.v.) *)
			copy_bonds_from_master ~__context;
			copy_vlans_from_master ~__context;
			copy_tunnels_from_master ~__context
		with e -> (* Errors here are non-data-corrupting hopefully, so we'll just carry on regardless... *)
			error "Caught exception syncing PIFs from the master: %s" (ExnHelper.string_of_exn e);
		log_backtrace ()
	end
	
