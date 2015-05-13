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
type base_class = Display_controller | Network_controller

(* Check if an class ID is of a given base class *)
val is_class_of_kind : base_class -> int -> bool

(** Get int value for PCI {class, vendor, device}_id *)
val int_of_id : string -> int

(** Get an identifier for this PCI device **)
val string_of_pci : __context:Context.t -> self:API.ref_PCI -> string

(** Synchronise the PCI objects in the database with the actual devices in the host. *)
val update_pcis : __context:Context.t -> host:API.ref_host -> unit

(** Get the PCI id of the host's display device. *)
val get_system_display_device : unit -> string option

(** Disable decoding for the host's display device. *)
val disable_system_display_device : unit -> unit
