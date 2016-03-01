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

let assert_license_feature_enabled ~__context =
	if not (Pool_features.is_enabled ~__context Features.Guest_static_ip_setting)
	then raise (Api_errors.Server_error (Api_errors.feature_restricted, []))

let check_guest_agent_feature_static_ip_setting ~__context ~vm_gm =
	let feature = "feature-static-ip-setting" in
	try
		let other = Db.VM_guest_metrics.get_other ~__context ~self:vm_gm in
		try
			List.assoc feature other = "1"
		with Not_found -> false
	with _ -> false

let check_ipv4_locking_mode_noconflict ~__context ~self ipv4_address =
	let effective_locking_mode = Db.VIF.get_locking_mode ~__context ~self
	in
	if effective_locking_mode = `locked then
		let ipv4_allowed = Db.VIF.get_ipv4_allowed ~__context ~self in
		List.exists (fun x -> x = ipv4_address) ipv4_allowed
	else
		true

let check_ipv6_locking_mode_noconflict ~__context ~self ipv6_address =
	let effective_locking_mode = Db.VIF.get_locking_mode ~__context ~self
	in
	if effective_locking_mode = `locked then
		let ipv6_allowed = Db.VIF.get_ipv6_allowed ~__context ~self in
		List.exists (fun x -> x = ipv6_address) ipv6_allowed
	else
		true

let plug ~__context ~self =
	Xapi_xenops.vif_plug ~__context ~self

let unplug ~__context ~self =
	Xapi_xenops.vif_unplug ~__context ~self false

let unplug_force ~__context ~self =
	Xapi_xenops.vif_unplug ~__context ~self true

let create  ~__context ~device ~network ~vM
           ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params ~locking_mode ~ipv4_allowed ~ipv6_allowed : API.ref_VIF =
  create ~__context ~device ~network ~vM ~currently_attached:false
    ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params ~locking_mode ~ipv4_allowed ~ipv6_allowed
    ~ipv4_configuration_mode:`None ~ipv4_addresses:[] ~ipv4_gateway:""
	~ipv6_configuration_mode:`None ~ipv6_addresses:[] ~ipv6_gateway:""

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

let set_ipv4_configuration ~__context ~self =
	if device_active ~__context ~self
	then Xapi_xenops.vif_set_ipv4_configuration ~__context ~self

let set_ipv6_configuration ~__context ~self =
	if device_active ~__context ~self
	then Xapi_xenops.vif_set_ipv6_configuration ~__context ~self

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

let configure_ipv4 ~__context ~self ~mode ~address ~gateway =
	assert_license_feature_enabled ~__context;
	if mode=`Static
	then begin
		(* require these parameters if mode is static *)
		(* valid addres format is like 192.168.1.10/24 *)
		let index =
			try
				String.index address '/'
			with Not_found ->
				let msg = "Prefix length must be specified (format: <ipaddr>/<prefix>" in
				raise (Api_errors.Server_error
					(Api_errors.invalid_ip_address_specified, [msg]))
		in

		let addr = String.sub address 0 index in
		let prefix_len = String.sub address (index + 1) ((String.length address) - index - 1) in
		assert_ip_address_is Unix.PF_INET "IP" addr;
		if not (check_ipv4_locking_mode_noconflict ~__context ~self addr) then
			raise (Api_errors.Server_error
				(Api_errors.invalid_ip_address_specified, ["IP address conflict with locking_mode config"]));

		let pl_int =
			try
				int_of_string prefix_len
			with _ ->
				let msg = Printf.sprintf "Cannot parse prefix length '%s'" prefix_len in
				raise (Api_errors.Server_error
					(Api_errors.invalid_ip_address_specified, [msg]))
		in
		if (pl_int < 0 || pl_int > 31) then
			raise (Api_errors.Server_error
				(Api_errors.invalid_ip_address_specified, ["Prefix length must be between 0 and 31"]))
	end;

	if gateway <> "" then assert_ip_address_is Unix.PF_INET "gateway" gateway;

	let vm = Db.VIF.get_VM ~__context ~self in
	let vm_gm = Db.VM.get_guest_metrics ~__context ~self:vm in
	let pv_updated = try Db.VM_guest_metrics.get_PV_drivers_up_to_date ~__context ~self:vm_gm with _ -> false in
	let feature_enabled = check_guest_agent_feature_static_ip_setting ~__context ~vm_gm in
	if pv_updated && feature_enabled then begin
		(* save setting to DB *)
		Db.VIF.set_ipv4_configuration_mode ~__context ~self ~value:mode;
		Db.VIF.set_ipv4_addresses ~__context ~self ~value:[address];
		if gateway <> "" then Db.VIF.set_ipv4_gateway ~__context ~self ~value:gateway;
		set_ipv4_configuration ~__context ~self
	end else
		raise (Api_errors.Server_error(Api_errors.vm_lacks_feature_static_ip_setting, [ Ref.string_of vm ]))

let configure_ipv6 ~__context ~self ~mode ~address ~gateway =
	assert_license_feature_enabled ~__context;
	if mode=`Static
	then begin
		(* require these parameters if mode is static *)
		let index =
			try
				String.index address '/'
			with Not_found ->
				let msg = "Prefix length must be specified (format: <ipv6>/<prefix>" in
				raise (Api_errors.Server_error
					(Api_errors.invalid_ip_address_specified, [msg]))
		in

		let addr = String.sub address 0 index in
		let prefix_len = String.sub address (index + 1) ((String.length address) - index - 1) in
		assert_ip_address_is Unix.PF_INET6 "IPv6" addr;
		if not (check_ipv6_locking_mode_noconflict ~__context ~self addr) then
			raise (Api_errors.Server_error
				(Api_errors.invalid_ip_address_specified, ["IPv6 address conflict with locking_mode config"]));

		let pl_int =
			try
				int_of_string prefix_len
			with _ ->
				let msg = Printf.sprintf "Cannot parse prefix length '%s'" prefix_len in
				raise (Api_errors.Server_error
					(Api_errors.invalid_ip_address_specified, [msg]))
		in
		if (pl_int < 0 || pl_int > 127) then
			raise (Api_errors.Server_error
				(Api_errors.invalid_ip_address_specified, ["Prefix length must be between 0 and 127"]))
	end;

	if gateway <> "" then assert_ip_address_is Unix.PF_INET6 "gateway6" gateway;

	let vm = Db.VIF.get_VM ~__context ~self in
	let vm_gm = Db.VM.get_guest_metrics ~__context ~self:vm in
	let pv_updated = try Db.VM_guest_metrics.get_PV_drivers_up_to_date ~__context ~self:vm_gm with _ -> false in
	let feature_enabled = check_guest_agent_feature_static_ip_setting ~__context ~vm_gm in
	if pv_updated && feature_enabled then begin
		(* save setting to DB *)
		Db.VIF.set_ipv6_configuration_mode ~__context ~self ~value:mode;
		Db.VIF.set_ipv6_addresses ~__context ~self ~value:[address];
		if gateway <> "" then Db.VIF.set_ipv6_gateway ~__context ~self ~value:gateway;
		set_ipv6_configuration ~__context ~self
	end else
		raise (Api_errors.Server_error(Api_errors.vm_lacks_feature_static_ip_setting, [ Ref.string_of vm ]))

