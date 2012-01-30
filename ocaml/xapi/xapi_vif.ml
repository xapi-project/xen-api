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


let plug ~__context ~self =
	Xapi_xenops.vif_plug ~__context ~self

let unplug ~__context ~self =
	Xapi_xenops.vif_unplug ~__context ~self false

let unplug_force ~__context ~self =
	Xapi_xenops.vif_unplug ~__context ~self true

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

