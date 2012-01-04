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

(** Attach PCI devices to the domain. The should be done before starting the domain. *)
val attach_pcis :
  __context:'a ->
  xc:Xenctrl.handle ->
  xs:Xenstore.Xs.xsh ->
  hvm:bool -> Xenctrl.domid -> (int * (int * int * int * int)) list -> unit

(** Hotplug the PCI devices into the domain (as opposed to 'attach_pcis') *)
val plug_pcis :
  __context:Context.t ->
  vm:'a ->
  Xenctrl.domid ->
  [ `PCI ] Ref.t list -> ('b * (int * int * int * int)) list -> unit

(** Hot unplug the PCI devices from the domain. Note this is done serially due to a limitation of the
   xenstore protocol. *)
val unplug_pcidevs_noexn :
  __context:'a -> vm:'b -> Xenctrl.domid -> ('c * Device.PCI.dev) list -> unit

(** Find all PCI devices that are currently attached to a domain, according to XenStore. *)
val currently_attached_pcis :
  __context:Context.t -> Xenctrl.domid -> 'a Ref.t list
