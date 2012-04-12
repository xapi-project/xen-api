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
open Threadext
open Client

module D=Debug.Debugger(struct let name="xapi" end)
open D

open Db_filter

let create_internal_bridge ~bridge ~uuid =
	debug "Creating internal bridge %s (uuid:%s)" bridge uuid;
	let current = Netdev.network.Netdev.list () in
	if not(List.mem bridge current) then Netdev.network.Netdev.add bridge ?uuid:(Some uuid);
	if not(Netdev.Link.is_up bridge) then Netdev.Link.up bridge

let attach_internal ?(management_interface=false) ~__context ~self () =
	let host = Helpers.get_localhost ~__context in
	let net = Db.Network.get_record ~__context ~self in
	(* This returns a list of local PIFs, of which there should be only by construction *)
	let local_pifs =
		Xapi_network_attach_helpers.assert_can_attach_network_on_host ~__context ~self ~host in

	(* Ensure internal bridge exists and is up. external bridges will be
	   brought up by call to interface-reconfigure. *)
	if List.length(local_pifs) = 0 then create_internal_bridge ~bridge:net.API.network_bridge ~uuid:net.API.network_uuid;

	(* Check if we're a guest-installer network: *)
	if (List.mem_assoc Xapi_globs.is_guest_installer_network net.API.network_other_config)
		&& (List.assoc Xapi_globs.is_guest_installer_network net.API.network_other_config = "true")
	then Xapi_network_real.maybe_start net.API.network_bridge net.API.network_other_config;

	(* Create the new PIF.
	   NB if we're doing this as part of a management-interface-reconfigure then
	   we might be just about to loose our current management interface... *)
	List.iter (fun pif ->
		let uuid = Db.PIF.get_uuid ~__context ~self:pif in
		debug "Trying to attach PIF: %s" uuid;
		Nm.bring_pif_up ~__context ~management_interface pif
	) local_pifs

let detach bridge_name =
	Xapi_network_real.maybe_stop bridge_name;
	if Netdev.network.Netdev.exists bridge_name then begin
		List.iter (fun iface ->
			D.warn "Untracked interface %s exists on bridge %s: deleting" iface bridge_name;
			Netdev.Link.down iface;
			Netdev.network.Netdev.intf_del bridge_name iface
		) (Netdev.network.Netdev.intf_list bridge_name);
		Netdev.Link.down bridge_name;
		Netdev.network.Netdev.del bridge_name
	end

let attach ~__context ~network ~host = attach_internal ~__context ~self:network ()

let active_vifs_to_networks : (API.ref_VIF, API.ref_network) Hashtbl.t = Hashtbl.create 10
let active_vifs_to_networks_m = Mutex.create ()

let register_vif ~__context vif =
	let network = Db.VIF.get_network ~__context ~self:vif in
	Mutex.execute active_vifs_to_networks_m
		(fun () ->
			debug "register_vif vif=%s network=%s" (Ref.string_of vif) (Ref.string_of network);
			Hashtbl.replace active_vifs_to_networks vif network
		)

let deregister_vif ~__context vif =
	let network = Db.VIF.get_network ~__context ~self:vif in
	let bridge = Db.Network.get_bridge ~__context ~self:network in
	let internal_only = Db.Network.get_PIFs ~__context ~self:network = [] in
	Mutex.execute active_vifs_to_networks_m
		(fun () ->
			Hashtbl.remove active_vifs_to_networks vif;
			(* If a network has PIFs, then we create/destroy when the PIFs are plugged/unplugged.
			   If a network is entirely internal, then we remove it after we've stopped using it
			   *unless* someone else is still using it. *)
			if internal_only then begin
				(* Are there any more vifs using this network? *)
				let others = Hashtbl.fold (fun v n acc -> if n = network then v :: acc else acc)
					active_vifs_to_networks [] in
				debug "deregister_vif vif=%s network=%s remaining vifs = [ %s ]" (Ref.string_of vif) (Ref.string_of network) (String.concat "; " (List.map Helpers.short_string_of_ref others));
				if others = [] then begin
					let ifs = Netdev.network.Netdev.intf_list bridge in
					if ifs = []
					then detach bridge
					else error "Cannot remove bridge %s: other interfaces still present [ %s ]" bridge (String.concat "; " ifs)
				end
			end
		)

let counter = ref 0
let mutex = Mutex.create ()
let stem = "xapi"

let pool_introduce ~__context ~name_label ~name_description ~mTU ~other_config ~bridge =
  let r = Ref.make() and uuid = Uuid.make_uuid() in
  Db.Network.create ~__context ~ref:r ~uuid:(Uuid.to_string uuid)
    ~current_operations:[] ~allowed_operations:[]
    ~name_label ~name_description ~mTU ~bridge
    ~other_config ~blobs:[] ~tags:[] ~default_locking_mode:`unlocked;
  r

let create ~__context ~name_label ~name_description ~mTU ~other_config ~tags =
	Mutex.execute mutex (fun () ->
		let networks = Db.Network.get_all ~__context in
		let bridges = List.map (fun self -> Db.Network.get_bridge ~__context ~self) networks in
		let mTU = if mTU <= 0L then 1500L else mTU in
		let rec loop () =
			let name = stem ^ (string_of_int !counter) in
			incr counter;
			if List.mem name bridges then loop ()
			else
				let r = Ref.make () and uuid = Uuid.make_uuid () in
				Db.Network.create ~__context ~ref:r ~uuid:(Uuid.to_string uuid)
					~current_operations:[] ~allowed_operations:[]
					~name_label ~name_description ~mTU ~bridge:name
					~other_config ~blobs:[] ~tags ~default_locking_mode:`unlocked;
				r in
		loop ())

let destroy ~__context ~self =
	let vifs = Db.Network.get_VIFs ~__context ~self in
	let connected = List.filter (fun self ->
		Db.VIF.get_currently_attached ~__context ~self || Db.VIF.get_reserved ~__context ~self
	) vifs in
	if connected <> [] then
		raise (Api_errors.Server_error (Api_errors.network_contains_vif,List.map Ref.string_of connected));
	let pifs = Db.Network.get_PIFs ~__context ~self in
	if pifs <> [] then
		(raise (Api_errors.Server_error (Api_errors.network_contains_pif,List.map Ref.string_of pifs)));
	(* CA-43250: don't let people remove the internal management network *)
	let oc = Db.Network.get_other_config ~__context ~self in
	if List.mem_assoc Xapi_globs.is_host_internal_management_network oc
		&& (try bool_of_string (List.assoc Xapi_globs.is_host_internal_management_network oc) with _ -> false)
	then raise (Api_errors.Server_error (Api_errors.cannot_destroy_system_network, [ Ref.string_of self ]));

	(* destroy all the VIFs now rather than wait for the GC thread. *)
	List.iter (fun vif ->
		Helpers.log_exn_continue (Printf.sprintf "destroying VIF: %s" (Ref.string_of vif))
			(fun vif -> Db.VIF.destroy ~__context ~self:vif) vif
	) vifs;
	Db.Network.destroy ~__context ~self

let create_new_blob ~__context ~network ~name ~mime_type =
	let blob = Xapi_blob.create ~__context ~mime_type in
	Db.Network.add_to_blobs ~__context ~self:network ~key:name ~value:blob;
	blob

let set_default_locking_mode ~__context ~network ~value =
	if (value = `disabled) then begin
		(* Allow unlicensed users to set default_locking_mode = `unlocked, i.e. turn the feature off. *)
		if not(Pool_features.is_enabled ~__context Features.VIF_locking) then
			raise (Api_errors.Server_error(Api_errors.license_restriction, []));
		(* Don't allow locking of a network if a vswitch controller is present. *)
		Helpers.assert_vswitch_controller_not_active ~__context
	end;
	(* Get all VIFs which are attached and associated with this network. *)
	let open Db_filter_types in
	match Db.VIF.get_records_where ~__context
		~expr:(And (
			(Eq (Field "network", Literal (Ref.string_of network))),
			(Eq (Field "currently_attached", Literal "true"))
		))
	with
	| [] -> Db.Network.set_default_locking_mode ~__context ~self:network ~value
	| (vif,_)::_ -> raise (Api_errors.Server_error (Api_errors.vif_in_use, [Ref.string_of network; Ref.string_of vif]))
