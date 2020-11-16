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

val pcidev_of_pci : __context:Context.t -> API.ref_PCI -> int * int * int * int
(** Return the PCI DBDF string for a PCI object *)

val sort_pcidevs : ('a * 'b) list -> ('a * 'b list) list
(** Return a list of PCIdevs in plug order *)

val other_pcidevs_of_vm :
     __context:Context.t
  -> (string * string) list
  -> (int * (int * int * int * int)) list
(** Return the PCI devices that are specified in the VM.other_config:pci field. *)

val to_string : int * (int * int * int * int) -> string
(** Return the PCI device as a string, suitable for other_config *)

val of_string : string -> int * (int * int * int * int)
(** Return the PCI device as a tuple *)

val is_bdf_format : string -> bool
(** True if the string matches BDF format, e.g. c002:8c:b3.a (all digits hex) *)

val is_pci_hidden : __context:Context.t -> [`PCI] Ref.t -> bool
(** Check whether a PCI device will be hidden from the dom0 kernel on boot. *)

val hide_pci : __context:Context.t -> [`PCI] Ref.t -> unit
(** Hide a PCI device from the dom0 kernel. (Takes affect after next boot.) *)

val unhide_pci : __context:Context.t -> [`PCI] Ref.t -> unit
(** Unhide a PCI device from the dom0 kernel. (Takes affect after next boot.) *)

val id_of : int * (int * int * int * int) -> int
(** Return the id of a PCI device *)

val domain_of : int * (int * int * int * int) -> int
(** Return the domain of a PCI device *)

val bus_of : int * (int * int * int * int) -> int
(** Return the bus of a PCI device *)

val dev_of : int * (int * int * int * int) -> int
(** Return the device of a PCI device *)

val fn_of : int * (int * int * int * int) -> int
(** Return the function of a PCI device *)

val reserve_free_virtual_function :
  __context:Context.t -> [`VM] Ref.t -> [`PCI] Ref.t -> [`PCI] Ref.t option
(** Find a free virtual function given a physical function (SR-IOV) *)
