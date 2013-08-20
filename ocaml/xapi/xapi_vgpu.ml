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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
module D=Debug.Debugger(struct let name="xapi" end)
open D

(* Mutex to prevent duplicate VGPUs being created by accident *)
let m = Mutex.create ()

(* Only allow device = "0" for now, as we support just a single vGPU per VM *)
let valid_device device =
	device = "0"

let create ~__context  ~vM ~gPU_group ~device ~other_config ~_type =
	let vgpu = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	if not (Pool_features.is_enabled ~__context Features.GPU) then
		raise (Api_errors.Server_error (Api_errors.feature_restricted, []));
	let vm_power_state = Db.VM.get_power_state ~__context ~self:vM in
	if (vm_power_state <> `Halted) then
		raise (Api_errors.Server_error (Api_errors.vm_bad_power_state,
			Ref.string_of vM :: List.map Record_util.power_to_string [`Halted; vm_power_state]));
	if not(valid_device device) then
		raise (Api_errors.Server_error (Api_errors.invalid_device, [device]));

	(* For backwards compatibility, convert Ref.null into the passthrough type. *)
	let _type =
		if _type = Ref.null
		then Xapi_vgpu_type.find_or_create ~__context Xapi_vgpu_type.entire_gpu
		else begin
			if Db.is_valid_ref __context _type
			then _type
			else raise (Api_errors.Server_error
				(Api_errors.invalid_value, ["type"; Ref.string_of _type]))
		end
	in

	Threadext.Mutex.execute m (fun () ->
		(* Check to make sure the device is unique *)
		let all = Db.VM.get_VGPUs ~__context ~self:vM in
		let all_devices = List.map (fun self -> Db.VGPU.get_device ~__context ~self) all in
		if List.mem device all_devices then
			raise (Api_errors.Server_error (Api_errors.device_already_exists, [device]));

		Db.VGPU.create ~__context ~ref:vgpu ~uuid ~vM ~gPU_group ~device
			~currently_attached:false ~other_config ~_type ~resident_on:Ref.null;
	);
	debug "VGPU ref='%s' created (VM = '%s', type = '%s')" (Ref.string_of vgpu) (Ref.string_of vM) (Ref.string_of _type);
	vgpu

let destroy ~__context ~self =
	let vm = Db.VGPU.get_VM ~__context ~self in
	if Helpers.is_running ~__context ~self:vm then
		raise (Api_errors.Server_error (Api_errors.operation_not_allowed, ["vGPU currently attached to a running VM"]));
	Db.VGPU.destroy ~__context ~self

let copy ~__context ~vm vgpu =
	let all = Db.VGPU.get_record ~__context ~self:vgpu in
	let vgpu = create ~__context
		~device:all.API.vGPU_device
		~gPU_group:all.API.vGPU_GPU_group
		~vM:vm
		~other_config:all.API.vGPU_other_config
		~_type:all.API.vGPU_type
	in
	if all.API.vGPU_currently_attached then
		Db.VGPU.set_currently_attached ~__context ~self:vgpu ~value:true;
	vgpu

let requires_passthrough ~__context ~self =
	let _type = Db.VGPU.get_type ~__context ~self in
	Xapi_vgpu_type.requires_passthrough ~__context ~self:_type
