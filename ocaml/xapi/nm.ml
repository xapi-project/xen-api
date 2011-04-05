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
module D=Debug.Debugger(struct let name="xapi" end)
open D

open Threadext

(* Protect a bunch of local operations with a mutex *)
let local_m = Mutex.create ()

let with_local_lock f = Mutex.execute local_m f

let interface_reconfigure_script = "/opt/xensource/libexec/interface-reconfigure"

let is_dom0_interface pif_r = pif_r.API.pIF_ip_configuration_mode <> `None
		
(* Make sure inventory file has all current interfaces on the local host, so
 * they will all be brought up again at start up. *)
let update_inventory ~__context =
	let localhost = Helpers.get_localhost ~__context in
	let pifs = List.filter 
		(fun (pif, pif_r) -> 
			true &&
				pif_r.API.pIF_host = localhost &&
				is_dom0_interface pif_r &&
				pif_r.API.pIF_currently_attached)
		(Db.PIF.get_all_records ~__context) in
	let bridges = List.map (fun (_, pif_r) -> Db.Network.get_bridge ~__context ~self:pif_r.API.pIF_network) pifs in
	Xapi_inventory.update Xapi_inventory._current_interfaces (String.concat " " bridges)

(* Call the interface reconfigure script. For development ignore the exn if it doesn't exist *)
let reconfigure_pif ~__context (pif: API.ref_PIF) args = 
	try
		Helpers.call_api_functions ~__context (fun _ session_id ->
			let args = "--session" :: (Ref.string_of session_id) :: "--pif" :: (Ref.string_of pif) :: args in
			ignore(Helpers.call_script interface_reconfigure_script args)
		)
	with
	| Forkhelpers.Spawn_internal_error(stderr, stdout, Unix.WEXITED n) ->
		raise (Api_errors.Server_error(Api_errors.pif_configuration_error, [ Ref.string_of pif; stderr ]))

let bring_pif_up ~__context ?(management_interface=false) (pif: API.ref_PIF) =
	with_local_lock (fun () ->
		let uuid = Db.PIF.get_uuid ~__context ~self:pif in
		let currently_attached = Db.PIF.get_currently_attached ~__context ~self:pif in
		(* In the case of the management interface since we need to call out just to 
		refresh the default gateway device setting *)
		if currently_attached = false || management_interface then begin
			debug "PIF %s has currently_attached set to %s%s; bringing up now" uuid
				(string_of_bool currently_attached) 
				(if management_interface then " and this is to be the new management interface" else "");
			let args = "up" :: (if management_interface then [ "--management" ] else []) in
			reconfigure_pif ~__context pif args;

			if management_interface then begin
				warn "About to kill cached stunnels and the master database connection";
				(* The master_connection would otherwise try to take a broken stunnel from the cache *)
				Stunnel_cache.flush (); 
				Master_connection.force_connection_reset ()
			end;

			Db.PIF.set_currently_attached ~__context ~self:pif ~value:true;
			if Db.PIF.get_management ~__context ~self:pif then begin
				debug "PIF %s is an existing management interface: rebinding and restarting server thread" uuid;
				Xapi_mgmt_iface.rebind ()
			end;

			(* If the PIF is a bond master, the bond slaves will now be down *)
			begin match Db.PIF.get_bond_master_of ~__context ~self:pif with
				| [] -> ()
				| bond :: _ ->
					let slaves = Db.Bond.get_slaves ~__context ~self:bond in
					List.iter (fun self -> Db.PIF.set_currently_attached ~__context ~self ~value:false) slaves
			end;

			(* If the PIF is a bond slave, the bond master will now be down *)
			begin match Db.PIF.get_bond_slave_of ~__context ~self:pif with
				| bond when bond = Ref.null -> ()
				| bond ->
					let master = Db.Bond.get_master ~__context ~self:bond in
					Db.PIF.set_currently_attached ~__context ~self:master ~value:false
			end;

			(* sync MTU *)
			(try
				let device = Db.PIF.get_device ~__context ~self:pif in
				let mtu = Int64.of_string (Netdev.get_mtu device) in
				Db.PIF.set_MTU ~__context ~self:pif ~value:mtu
			with _ ->
				debug "could not update MTU field on PIF %s" uuid
			);

			Xapi_mgmt_iface.on_dom0_networking_change ~__context;

			update_inventory ~__context
		end
	)

let bring_pif_down ~__context (pif: API.ref_PIF) =
	with_local_lock (fun () ->
		(* Check that the PIF is not in-use *)
		let uuid = Db.PIF.get_uuid ~__context ~self:pif in
		let network = Db.PIF.get_network ~__context ~self:pif in
		Xapi_network_attach_helpers.assert_network_has_no_vifs_in_use_on_me ~__context ~host:(Helpers.get_localhost ~__context) ~network;
		Xapi_network_attach_helpers.assert_pif_disallow_unplug_not_set ~__context pif;
		if Db.PIF.get_currently_attached ~__context ~self:pif = true then begin
			debug "PIF %s has currently_attached set to true; bringing down now" uuid;
			reconfigure_pif ~__context pif [ "down" ];
			Db.PIF.set_currently_attached ~__context ~self:pif ~value:false;
			update_inventory ~__context
		end
	)

