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
open Vmopshelpers
open Xapi_vif_helpers
open Xenstore
open D

let assert_operation_valid ~__context ~self ~(op:API.vif_operations) = 
  assert_operation_valid ~__context ~self ~op

let update_allowed_operations ~__context ~self : unit =
  update_allowed_operations ~__context ~self


(* Create device for specified vif and attach to running domain *)
let dynamic_create ~__context ~vif token =
	let vm = Db.VIF.get_VM ~__context ~self:vif in
	let vm_r = Db.VM.get_record ~__context ~self:vm in
	Locking_helpers.assert_locked vm token;

	if Db.VIF.get_currently_attached ~__context ~self:vif then
	  raise (Api_errors.Server_error (Api_errors.device_already_attached,[Ref.string_of vif]));
	let protocol = Helpers.device_protocol_of_string vm_r.API.vM_domarch in
	match Vm_config.vif_of_vif ~__context vm_r (Int64.to_int vm_r.API.vM_domid) protocol vif with
	| None -> 
	    warn "Failed to plug VIF %s: appears to be dangling?" (Ref.string_of vif);
	    raise (Api_errors.Server_error(Api_errors.handle_invalid, [ "VIF"; Ref.string_of vif ]))
	| Some vif_device ->
	    debug "Attempting to dynamically attach VIF to domid %d" vif_device.Vm_config.domid;
	    with_xs (fun xs -> Vmops.add_vif ~__context ~xs vif_device)

(* Helper fn called by dynamic_destroy, below *)
let destroy_vif ~__context ~xs domid self token force =
	let device = Xen_helpers.device_of_vif ~__context ~self in
	try
	  (if force then Device.hard_shutdown else Device.clean_shutdown) ~xs device;
	  Device.Vif.release ~xs device;

	  Xapi_network.deregister_vif ~__context self;
	  Db.VIF.set_currently_attached ~__context ~self ~value:false
	with 
	| Device_common.Device_disconnect_timeout device ->
	    error "Xapi_vif.destroy_vif got a timeout waiting for (%s)" (Device_common.string_of_device device);
	    raise (Api_errors.Server_error(Api_errors.device_detach_timeout, [ "VIF"; Ref.string_of self ]))
	| Device_common.Device_error(device, errmsg) ->
	    error "Xapi_vif.destroy_vif got an error (%s) %s" (Device_common.string_of_device device) errmsg;
	    raise (Api_errors.Server_error(Api_errors.device_detach_rejected, [ "VIF"; Ref.string_of self; errmsg ]))

(* Destroy device for specified vif and detach from running domain *)
let dynamic_destroy ~__context ~vif ~force token =
	Locking_helpers.assert_locked (Db.VIF.get_VM ~__context ~self:vif) token;

	if not (Db.VIF.get_currently_attached ~__context ~self:vif) then
		raise (Api_errors.Server_error (Api_errors.device_already_detached,[Ref.string_of vif]));
	let vm = Db.VIF.get_VM ~__context ~self:vif in
	let domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
	debug "Attempting to dynamically detach VIF from domid %d" domid;
	with_xs (fun xs -> destroy_vif ~__context ~xs domid vif token force)

let plug  ~__context ~self = plug dynamic_create ~__context ~self

let unplug  ~__context ~self = Xapi_vif_helpers.unplug (dynamic_destroy ~force:false)
	~__context ~self

let unplug_force ~__context ~self = Xapi_vif_helpers.unplug (dynamic_destroy ~force:true)
	~__context ~self

let create  ~__context ~device ~network ~vM
           ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params : API.ref_VIF =
  create ~__context ~device ~network ~vM ~currently_attached:false
    ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params
    ~locking_mode:`network_default ~ipv4_allowed:[] ~ipv6_allowed:[]

let destroy  ~__context ~self = destroy ~__context ~self

let do_if_device_active ~__context ~self f =
	let vif_rec = Db.VIF.get_record ~__context ~self in
	let vm_rec = Db.VM.get_record ~__context ~self:vif_rec.API.vIF_VM in
	let attached = vif_rec.API.vIF_currently_attached in
	let suspended = vm_rec.API.vM_power_state = `Suspended in
	if attached && not suspended then begin
		let protocol = Helpers.device_protocol_of_string vm_rec.API.vM_domarch in
		match Vm_config.vif_of_vif ~__context vm_rec (Int64.to_int vm_rec.API.vM_domid) protocol self with
		| None -> ()
		| Some vif_device -> f vif_device
	end

let refresh_filtering_rules ~__context ~self =
	do_if_device_active ~__context ~self (fun vif_device ->
		let private_data_path = Hotplug.get_private_data_path_of_device (Vm_config.device_of_vif vif_device) in
		with_xs (fun xs ->
			xs.Xs.write
				(private_data_path ^ "/locking-mode")
				(Record_util.vif_locking_mode_to_string (Vm_config.effective_locking_mode_of_vif ~__context vif_device));
			xs.Xs.write
				(private_data_path ^ "/ipv4-allowed")
				(String.concat "," vif_device.Vm_config.ipv4_allowed);
			xs.Xs.write
				(private_data_path ^ "/ipv6-allowed")
				(String.concat "," vif_device.Vm_config.ipv6_allowed));
		let domid = string_of_int vif_device.Vm_config.domid in
		let devid = string_of_int vif_device.Vm_config.devid in
		let net_type = match Netdev.network.Netdev.kind with
		| Netdev.Bridge -> "bridge"
		| Netdev.Vswitch -> "openvswitch"
		in
		ignore (Helpers.call_script (Filename.concat Fhs.libexecdir "setup-vif-rules") ["vif"; domid; devid; net_type; "filter"]);
		(* Update rules for the tap device if the VM has booted HVM with no PV drivers. *)
		let vm = Db.VIF.get_VM ~__context ~self in
		let has_booted_hvm = Helpers.has_booted_hvm ~__context ~self:vm in
		let has_pv_drivers = (Db.VM.get_guest_metrics ~__context ~self:vm <> Ref.null) in
		if has_booted_hvm && (not has_pv_drivers) then
			ignore (Helpers.call_script (Filename.concat Fhs.libexecdir "setup-vif-rules") ["tap"; domid; devid; net_type; "filter"]))

(* This function moves a dom0 vif device from one bridge to another, without involving the guest,
 * so it also works on guests that do not support hot(un)plug of VIFs. *)
let move ~__context ~network vif =
	debug "Moving VIF %s to network %s" (Db.VIF.get_uuid ~__context ~self:vif)
		(Db.Network.get_uuid ~__context ~self:network);
	Db.VIF.set_network ~__context ~self:vif ~value:network;
	do_if_device_active ~__context ~self:vif (fun vif_device ->
		let xs_bridge_path = Hotplug.get_private_data_path_of_device (Vm_config.device_of_vif vif_device) ^ "/bridge" in
		with_xs (fun xs -> xs.Xs.write xs_bridge_path vif_device.Vm_config.bridge);
		let domid = string_of_int vif_device.Vm_config.domid in
		let devid = string_of_int vif_device.Vm_config.devid in
		ignore(Helpers.call_script (Filename.concat Fhs.scriptsdir "vif") ["move"; "vif"; domid; devid]))

let assert_locking_licensed ~__context =
	if (not (Pool_features.is_enabled ~__context Features.VIF_locking)) then
		raise (Api_errors.Server_error(Api_errors.license_restriction, []))

let change_locking_config ~__context ~self ~run_prechecks f =
	(* If turning the feature "on", we should check that the feature is licensed *)
	(* and that there is no vswitch controller active. *)
	if run_prechecks then begin
		assert_locking_licensed ~__context;
		Helpers.assert_vswitch_controller_not_active ~__context
	end;
	f ();
	refresh_filtering_rules ~__context ~self

let set_locking_mode ~__context ~self ~value =
	change_locking_config ~__context ~self ~run_prechecks:(value <> `network_default)
		(fun () -> Db.VIF.set_locking_mode ~__context ~self ~value)

let assert_ip_address_is domain field_name addr =
	match Helpers.validate_ip_address addr with
	| Some x when x = domain -> ()
	| _ -> raise (Api_errors.Server_error (Api_errors.invalid_value, [field_name; addr]))

let set_ipv4_allowed ~__context ~self ~value =
	let setified_value = List.setify value in
	change_locking_config ~__context ~self ~run_prechecks:(setified_value <> [])
		(fun () ->
			List.iter (assert_ip_address_is Unix.PF_INET "ipv4_allowed") setified_value;
			Db.VIF.set_ipv4_allowed ~__context ~self ~value:setified_value)

let add_ipv4_allowed ~__context ~self ~value =
	change_locking_config ~__context ~self ~run_prechecks:true
		(fun () ->
			assert_ip_address_is Unix.PF_INET "ipv4_allowed" value;
			Db.VIF.add_ipv4_allowed ~__context ~self ~value)

let remove_ipv4_allowed ~__context ~self ~value =
	change_locking_config ~__context ~self ~run_prechecks:false
		(fun () -> Db.VIF.remove_ipv4_allowed ~__context ~self ~value)

let set_ipv6_allowed ~__context ~self ~value =
	let setified_value = List.setify value in
	change_locking_config ~__context ~self ~run_prechecks:(setified_value <> [])
		(fun () ->
			List.iter (assert_ip_address_is Unix.PF_INET6 "ipv6_allowed") setified_value;
			Db.VIF.set_ipv6_allowed ~__context ~self ~value:setified_value)

let add_ipv6_allowed ~__context ~self ~value =
	change_locking_config ~__context ~self ~run_prechecks:true
		(fun () ->
			assert_ip_address_is Unix.PF_INET6 "ipv6_allowed" value;
			Db.VIF.add_ipv6_allowed ~__context ~self ~value)

let remove_ipv6_allowed ~__context ~self ~value =
	change_locking_config ~__context ~self ~run_prechecks:false
		(fun () -> Db.VIF.remove_ipv4_allowed ~__context ~self ~value)
