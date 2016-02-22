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
open Listext
open Xstringext
open Xapi_vif_helpers
module D = Debug.Make(struct let name="xapi" end)
open D

let assert_operation_valid ~__context ~self ~(op:API.vif_operations) = 
  assert_operation_valid ~__context ~self ~op

let update_allowed_operations ~__context ~self : unit =
  update_allowed_operations ~__context ~self


let plug ~__context ~self =
	Xapi_xenops.vif_plug ~__context ~self

let unplug ~__context ~self =
	Xapi_xenops.vif_unplug ~__context ~self false

let unplug_force ~__context ~self =
	Xapi_xenops.vif_unplug ~__context ~self true

let create  ~__context ~device ~network ~vM
           ~mAC ~mTU ~other_config ~static_ip_setting ~qos_algorithm_type ~qos_algorithm_params ~locking_mode ~ipv4_allowed ~ipv6_allowed : API.ref_VIF =
  create ~__context ~device ~network ~vM ~currently_attached:false
    ~mAC ~mTU ~other_config ~static_ip_setting ~qos_algorithm_type ~qos_algorithm_params ~locking_mode ~ipv4_allowed ~ipv6_allowed

let destroy  ~__context ~self = destroy ~__context ~self

let device_active ~__context ~self =
        let vif_rec = Db.VIF.get_record ~__context ~self in
        let vm_rec = Db.VM.get_record ~__context ~self:vif_rec.API.vIF_VM in
        let attached = vif_rec.API.vIF_currently_attached in
        let suspended = vm_rec.API.vM_power_state = `Suspended in
        attached && not suspended

let refresh_filtering_rules ~__context ~self =
        if device_active ~__context ~self
        then Xapi_xenops.vif_set_locking_mode ~__context ~self

let set_static_ip_setting ~__context ~self ~key ~value =
	if device_active ~__context ~self then begin
		let static_ip_setting = [(key, value)] in
		Xapi_xenops.vif_set_static_ip_setting ~__context ~self static_ip_setting
	end

let unset_static_ip_setting ~__context ~self ~key =
	if device_active ~__context ~self
	then Xapi_xenops.vif_unset_static_ip_setting ~__context ~self key

(* This function moves a dom0 vif device from one bridge to another, without involving the guest,
 * so it also works on guests that do not support hot(un)plug of VIFs. *)
let move ~__context ~network vif =
	debug "Moving VIF %s to network %s" (Db.VIF.get_uuid ~__context ~self:vif)
		(Db.Network.get_uuid ~__context ~self:network);
	Db.VIF.set_network ~__context ~self:vif ~value:network;
	if device_active ~__context ~self:vif
	then Xapi_xenops.vif_move ~__context ~self:vif network

let change_locking_config ~__context ~self ~licence_check f =
	if licence_check then assert_locking_licensed ~__context;
	f ();
	refresh_filtering_rules ~__context ~self

let set_locking_mode ~__context ~self ~value =
	let effective_locking_mode : API.vif_locking_mode =
		match value with
		| `network_default ->
			let network = Db.VIF.get_network ~__context ~self in
			Db.Network.get_default_locking_mode ~__context ~self:network
		| other -> other
	in
	if effective_locking_mode = `locked then
		Helpers.assert_vswitch_controller_not_active ~__context;
	change_locking_config ~__context ~self
		~licence_check:(effective_locking_mode = `locked)
		(fun () -> Db.VIF.set_locking_mode ~__context ~self ~value)

let assert_ip_address_is domain field_name addr =
	match Unixext.domain_of_addr addr with
	| Some x when x = domain -> ()
	| _ -> raise (Api_errors.Server_error (Api_errors.invalid_value, [field_name; addr]))

let assert_cidr_address_is domain field_name addr =
	let items = String.split '/' addr in
        if (List.length items != 2) then
		raise (Api_errors.Server_error (Api_errors.invalid_value, [field_name; addr]));
	
	assert_ip_address_is domain field_name (List.hd items);

	let bits = List.hd (List.rev items) in
	let maskbits = Int32.to_int (Int32.of_string bits) in
	match domain with
	| Unix.PF_INET ->
		if (maskbits < 0 || maskbits > 31) then
			raise (Api_errors.Server_error (Api_errors.invalid_value, [field_name; addr]))
	| Unix.PF_INET6 ->
		if (maskbits < 0 || maskbits > 127) then
			raise (Api_errors.Server_error (Api_errors.invalid_value, [field_name; addr]))
	| _ -> raise (Api_errors.Server_error (Api_errors.invalid_value, [field_name; addr]))

let assert_ip_setting_is field_name key value =
	if key = "enabled" then
		match value with
		| "1" -> ()
		| "0" -> ()
		| _ -> raise (Api_errors.Server_error (Api_errors.invalid_value, [field_name; value]))	
	else if key = "address" then
		assert_cidr_address_is Unix.PF_INET field_name value
	else if key = "gateway" then
		assert_ip_address_is Unix.PF_INET field_name value
	else if key = "address6" then
		assert_cidr_address_is Unix.PF_INET6 field_name value
	else if key = "gateway6" then
		assert_ip_address_is Unix.PF_INET6 field_name value
	else
		raise (Api_errors.Server_error (Api_errors.invalid_value, [field_name; key]))

let set_ipv4_allowed ~__context ~self ~value =
	let setified_value = List.setify value in
	change_locking_config ~__context ~self ~licence_check:(setified_value <> [])
		(fun () ->
			List.iter (assert_ip_address_is Unix.PF_INET "ipv4_allowed") setified_value;
			Db.VIF.set_ipv4_allowed ~__context ~self ~value:setified_value)

let add_ipv4_allowed ~__context ~self ~value =
	change_locking_config ~__context ~self ~licence_check:true
		(fun () ->
			assert_ip_address_is Unix.PF_INET "ipv4_allowed" value;
			Db.VIF.add_ipv4_allowed ~__context ~self ~value)

let remove_ipv4_allowed ~__context ~self ~value =
	change_locking_config ~__context ~self ~licence_check:false
		(fun () -> Db.VIF.remove_ipv4_allowed ~__context ~self ~value)

let set_ipv6_allowed ~__context ~self ~value =
	let setified_value = List.setify value in
	change_locking_config ~__context ~self ~licence_check:(setified_value <> [])
		(fun () ->
			List.iter (assert_ip_address_is Unix.PF_INET6 "ipv6_allowed") setified_value;
			Db.VIF.set_ipv6_allowed ~__context ~self ~value:setified_value)

let add_ipv6_allowed ~__context ~self ~value =
	change_locking_config ~__context ~self ~licence_check:true
		(fun () ->
			assert_ip_address_is Unix.PF_INET6 "ipv6_allowed" value;
			Db.VIF.add_ipv6_allowed ~__context ~self ~value)

let remove_ipv6_allowed ~__context ~self ~value =
	change_locking_config ~__context ~self ~licence_check:false
		(fun () -> Db.VIF.remove_ipv6_allowed ~__context ~self ~value)

let add_to_static_ip_setting ~__context ~self ~key ~value =
	change_locking_config ~__context ~self ~licence_check:false
		(fun () -> 
			assert_ip_setting_is "static_ip_setting" key value;
			let vm = Db.VIF.get_VM ~__context ~self in
			let vm_gm = Db.VM.get_guest_metrics ~__context ~self:vm in 
			let network_optimized = try Db.VM_guest_metrics.get_network_paths_optimized ~__context ~self:vm_gm with _ -> false in
			let storage_optimized = try Db.VM_guest_metrics.get_storage_paths_optimized ~__context ~self:vm_gm with _ -> false in
			if network_optimized && storage_optimized then begin
				Db.VIF.add_to_static_ip_setting ~__context ~self ~key ~value;
				set_static_ip_setting ~__context ~self ~key ~value
			end else
				raise (Api_errors.Server_error(Api_errors.vm_missing_pv_drivers, [ Ref.string_of vm ])))

let remove_from_static_ip_setting ~__context ~self ~key =
	change_locking_config ~__context ~self ~licence_check:false
		(fun () ->
			Db.VIF.remove_from_static_ip_setting ~__context ~self ~key;
			unset_static_ip_setting ~__context ~self ~key)
