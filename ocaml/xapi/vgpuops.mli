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


(** Return a list of GPU PCI devices that are going to be attached. *)
val create_vgpus :
  __context:Context.t -> (API.ref_VM * API.vM_t) -> bool -> [ `PCI ] Ref.t list

(** Mark all VGPUs for the given VM as unattached. *)
val clear_vgpus : __context:Context.t -> vm:[ `VM ] Ref.t -> unit
