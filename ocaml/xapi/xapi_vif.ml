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
	match Vm_config.vif_of_vif ~__context ~vm vm_r (Int64.to_int vm_r.API.vM_domid) protocol vif with
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

let destroy  ~__context ~self = destroy ~__context ~self

(* This function moves a dom0 vif device from one bridge to another, without involving the guest,
 * so it also works on guests that do not support hot(un)plug of VIFs. *)
let move ~__context ~network vif =
	debug "Moving VIF %s to network %s" (Db.VIF.get_uuid ~__context ~self:vif)
		(Db.Network.get_uuid ~__context ~self:network);
	let vif_rec = Db.VIF.get_record ~__context ~self:vif in
	let suspended = Db.VM.get_power_state ~__context ~self:vif_rec.API.vIF_VM = `Suspended in
	let attached = vif_rec.API.vIF_currently_attached && not suspended in
	Db.VIF.set_network ~__context ~self:vif ~value:network;
	if attached then
		let vm_r = Db.VM.get_record ~__context ~self:vif_rec.API.vIF_VM in
		let protocol = Helpers.device_protocol_of_string vm_r.API.vM_domarch in
		match Vm_config.vif_of_vif ~__context ~vm:vif_rec.API.vIF_VM vm_r (Int64.to_int vm_r.API.vM_domid) protocol vif with
		| None -> ()
		| Some vif_device ->
			let xs_bridge_path = Hotplug.get_private_data_path_of_device (Vm_config.device_of_vif vif_device) ^ "/bridge" in
			with_xs (fun xs -> xs.Xs.write xs_bridge_path vif_device.Vm_config.bridge);
			let domid = string_of_int vif_device.Vm_config.domid in
			let devid = string_of_int vif_device.Vm_config.devid in
			ignore(Helpers.call_script (Filename.concat Fhs.scriptsdir "vif") ["move"; "vif"; domid; devid])

