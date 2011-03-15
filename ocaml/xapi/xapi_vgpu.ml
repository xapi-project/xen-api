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

let create ~__context  ~vM ~gPU_group ~device ~other_config =
	let vgpu = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	if not(valid_device device)
		then raise (Api_errors.Server_error (Api_errors.invalid_device,[device]));
	Threadext.Mutex.execute m (fun () ->
		(* Check to make sure the device is unique *)
		let all = Db.VM.get_VGPUs ~__context ~self:vM in
		let all_devices = List.map (fun self -> Db.VGPU.get_device ~__context ~self) all in
		if List.mem device all_devices then
			raise (Api_errors.Server_error (Api_errors.device_already_exists, [device]));

		Db.VGPU.create ~__context ~ref:vgpu ~uuid ~vM ~gPU_group ~device ~currently_attached:false ~other_config;
	);
	debug "VGPU ref='%s' created (VM = '%s')" (Ref.string_of vgpu) (Ref.string_of vM);
	vgpu

let destroy ~__context ~self =
	Db.VGPU.destroy ~__context ~self

