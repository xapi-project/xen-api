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
(** Module that handles assigning PCI devices to VMs.
 * @group Virtual-Machine Management
 *)
 
(** Check if a given PCI device is free. *)
val get_free_functions : __context:Context.t -> [ `PCI ] Ref.t -> int

(** Mark all PCI devices for a given VM as unattached. *)
val unassign_all_for_vm : __context:Context.t -> [ `VM ] Ref.t -> unit

(** Return the PCI DBDF string for a PCI object *)
val pcidev_of_pci: __context:Context.t -> API.ref_PCI -> (int * int * int * int)

(** Return a list of PCIdevs in plug order *)
val sort_pcidevs: ('a * 'b) list -> ('a * 'b list) list

(** Return the PCI devices that are specified in the VM.other_config:pci field. *)
val other_pcidevs_of_vm :
  __context:Context.t -> (string * string) list -> (int * (int * int * int * int)) list

