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
module D=Debug.Debugger(struct let name="vgpuops" end)
open D

type vgpu = {
	domid: int; (** current domain id of the VM *)
	vgpu_ref: API.ref_VGPU;
	gpu_group_ref: API.ref_GPU_group;
	devid: int;
	other_config: (string * string) list;
}

let vgpu_of_vgpu ~__context vm_r domid vgpu =
	let vgpu_r = Db.VGPU.get_record ~__context ~self:vgpu in
	{
		domid = domid;
		vgpu_ref = vgpu;
		gpu_group_ref = vgpu_r.API.vGPU_GPU_group;
		devid = int_of_string vgpu_r.API.vGPU_device;
		other_config = vgpu_r.API.vGPU_other_config
	}

let vgpus_of_vm ~__context ~vm domid =
	let vm_r = Db.VM.get_record ~__context ~self:vm in
	List.map (vgpu_of_vgpu ~__context vm_r domid) vm_r.API.vM_VGPUs
