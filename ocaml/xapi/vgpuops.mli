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

(** Module that handles assigning vGPUs to VMs.
 * @group Virtual-Machine Management
*)


(** Assign a list of PCI devices to a VM for GPU passthrough, store them in
    	other_config:vgpu_pci *)
val create_vgpus :
  __context:Context.t ->
  (API.ref_host) ->(API.ref_VM * API.vM_t) -> bool -> unit

(** Return a list of the GPU PCI devices which have been assigned to this VM *)
val list_pcis_for_passthrough :
  __context:Context.t -> vm:API.ref_VM -> (int * (int * int * int * int)) list

(** Allocate a vGPU to a pGPU of a host for the VM
 *  return a list indicate which pGPU is allocated for the vGPU in following format
 *  [(v1,p1);(v2,p2);(v3,p1)]
 *  Two additional arguments dry_run and pre_allocate_list is added to this fuction.
 *  They are designed to be optional to keep the arguments interface backward-compatibility
 *  dry_run set to "false", pre_allocate_list set to "[]" by default.
 *  if dry_run mode is specified, the function just dry run the allocation process
 *  without any database operation. pre_allocate_list is used to record the dry run
 *  states *)
type vgpu_t
val allocate_vgpu_to_gpu :
  ?dry_run:bool -> ?pre_allocate_list:(API.ref_VGPU * API.ref_PGPU) list ->
  __context:Context.t -> API.ref_VM ->  API.ref_host -> vgpu_t -> (API.ref_VGPU * API.ref_PGPU) list

(** Get a vgpu record from vgpu ref *)
val vgpu_of_ref : __context:Context.t -> API.ref_VGPU -> vgpu_t
