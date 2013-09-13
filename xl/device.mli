(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Device_common
open Xenops_task

exception Device_shutdown
exception Device_not_found

module Vbd :
sig
	type mode = ReadOnly | ReadWrite
	val string_of_mode : mode -> string
	val mode_of_string : string -> mode

	type physty = File | Phys | Qcow | Vhd | Aio
	val string_of_physty : physty -> string
	val physty_of_string : string -> physty
	val kind_of_physty : physty -> kind
	val uses_blktap : phystype:physty -> bool

	type devty = CDROM | Disk
	val string_of_devty : devty -> string
	val devty_of_string : string -> devty

	type t = {
		mode:mode;
		device_number: Device_number.t option;
		phystype: physty;
		params: string;
		dev_type: devty;
		unpluggable: bool;
		protocol: protocol option;
		extra_backend_keys: (string * string) list;
		extra_private_keys: (string * string) list;
		backend_domid: int;
	}

	val add : Xenops_task.t -> xs:Xenstore.Xs.xsh -> hvm:bool -> ?backend_kind:kind -> t -> Xenctrl.domid -> device
	val clean_shutdown_async : xs:Xenstore.Xs.xsh -> device -> unit
	val clean_shutdown_wait : Xenops_task.t -> xs:Xenstore.Xs.xsh -> ignore_transients:bool -> device -> unit

	(* For migration: *)
	val hard_shutdown_request : xs:Xenstore.Xs.xsh -> device -> unit
	val hard_shutdown_complete : xs:Xenstore.Xs.xsh -> device -> unit Watch.t
	val hard_shutdown_wait : Xenops_task.t -> xs:Xenstore.Xs.xsh -> timeout:float -> device -> unit
end

val can_surprise_remove : xs:Xenstore.Xs.xsh -> device -> bool

module PV_Vnc :
sig
	exception Failed_to_start
	val save : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit
	val get_statefile : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> string option
	val start : ?statefile:string -> xs:Xenstore.Xs.xsh -> ?ip:string -> Xenctrl.domid -> unit
	val stop : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit

	val get_vnc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
	val get_tc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
end

val get_vnc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
val get_tc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
