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
