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
(** Module that defines API functions for PCI objects
 *)

(** Types of PCI devices. *)
type pci_class = Display_controller | Network_controller

(** Get the PCI class ID for a given class. *)
val find_class_id : pci_class -> string

(** Synchronise the PCI objects in the database with the actual devices in the host. *)
val update_pcis : __context:Context.t -> host:API.ref_host -> unit

(** Get the PCI id of the host's display device. *)
val get_system_display_device : unit -> string option
