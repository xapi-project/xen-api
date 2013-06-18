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

exception Ioemu_failed of string
exception Ioemu_failed_dying

exception Device_shutdown
exception Device_not_found

exception Cdrom

module Generic :
sig
	val rm_device_state : xs:Xenstore.Xs.xsh -> device -> unit
	val exists : xs:Xenstore.Xs.xsh -> device -> bool
	val get_private_key: xs:Xenstore.Xs.xsh -> device -> string -> string
	val get_private_key': xs:Xenstore.Xs.xsh -> string -> string -> int -> string -> string
end

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

	val add : Xenops_task.t -> xs:Xenstore.Xs.xsh -> hvm:bool -> t -> Xenctrl.domid -> device

	val release : Xenops_task.t -> xs:Xenstore.Xs.xsh -> device -> unit
	val media_eject : xs:Xenstore.Xs.xsh -> device_number:Device_number.t -> int -> unit
	val media_insert : xs:Xenstore.Xs.xsh -> device_number:Device_number.t
	                -> params:string -> phystype:physty -> int -> unit
	val media_is_ejected : xs:Xenstore.Xs.xsh -> device_number:Device_number.t -> int -> bool
	val media_tray_is_locked : xs:Xenstore.Xs.xsh -> device_number:Device_number.t -> int -> bool

	val clean_shutdown_async : xs:Xenstore.Xs.xsh -> device -> unit
	val clean_shutdown_wait : Xenops_task.t -> xs:Xenstore.Xs.xsh -> ignore_transients:bool -> device -> unit

	(* For migration: *)
	val hard_shutdown_request : xs:Xenstore.Xs.xsh -> device -> unit
	val hard_shutdown_complete : xs:Xenstore.Xs.xsh -> device -> unit Watch.t
	val hard_shutdown_wait : Xenops_task.t -> xs:Xenstore.Xs.xsh -> timeout:float -> device -> unit
end

module Vif :
sig
	val add : Xenops_task.t -> xs:Xenstore.Xs.xsh -> devid:int -> netty:Netman.netty
	       -> mac:string -> carrier:bool 
	       -> ?mtu:int -> ?rate:(int64 * int64) option
	       -> ?protocol:protocol -> ?backend_domid:Xenctrl.domid 
	       -> ?other_config:((string * string) list) 
	       -> ?extra_private_keys:(string * string) list -> Xenctrl.domid
	       -> device
	val set_carrier : xs:Xenstore.Xs.xsh -> device -> bool -> unit
	val release : Xenops_task.t -> xs:Xenstore.Xs.xsh -> device -> unit
	val move : xs:Xenstore.Xs.xsh -> device -> string -> unit
end

val clean_shutdown : Xenops_task.t -> xs:Xenstore.Xs.xsh -> device -> unit
val hard_shutdown  : Xenops_task.t -> xs:Xenstore.Xs.xsh -> device -> unit

val can_surprise_remove : xs:Xenstore.Xs.xsh -> device -> bool

module Vcpu :
sig
	val add : xs:Xenstore.Xs.xsh -> devid:int -> int -> unit
	val del : xs:Xenstore.Xs.xsh -> devid:int -> int -> unit
	val set : xs:Xenstore.Xs.xsh -> devid:int -> int -> bool -> unit
	val status : xs:Xenstore.Xs.xsh -> devid:int -> int -> bool
end

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

module PCI :
sig
	type t = {
		domain: int;
		bus: int;
		slot: int;
		func: int;
		irq: int;
		resources: (int64 * int64 * int64) list;
		driver: string;
	}
	type dev = int * int * int * int
	val to_string: dev -> string
	val of_string: string -> dev

	exception Cannot_use_pci_with_no_pciback of t list

	val add : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> hvm:bool -> msitranslate:int -> pci_power_mgmt:int
	       -> ?flrscript:string option -> dev list -> Xenctrl.domid -> int -> unit
	val release : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> hvm:bool
	       -> (int * int * int * int) list -> Xenctrl.domid -> int -> unit
	val reset : xs:Xenstore.Xs.xsh -> dev -> unit
	val bind : dev list -> unit
	val plug : Xenops_task.t -> xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> dev -> Xenctrl.domid -> unit
	val unplug : Xenops_task.t -> xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> dev -> Xenctrl.domid -> unit
	val list : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> Xenctrl.domid -> (int * dev) list
end

module Vfs :
sig
	val add : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> ?backend_domid:int -> Xenctrl.domid -> unit
end

module Vfb :
sig
	val add : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> ?backend_domid:int -> ?protocol:protocol -> Xenctrl.domid -> unit
end

module Vkbd :
sig 
	val add : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> ?backend_domid:int -> ?protocol:protocol -> Xenctrl.domid -> unit
end

module Dm :
sig
	type disp_intf_opt =
	    | Std_vga
	    | Cirrus
	val disp_intf_opt_of_rpc: Rpc.t -> disp_intf_opt
	val rpc_of_disp_intf_opt: disp_intf_opt -> Rpc.t

	type disp_opt =
		| NONE
		| VNC of disp_intf_opt * string option * bool * int * string (* IP address, auto-allocate, port if previous false, keymap *)
		| SDL of disp_intf_opt * string (* X11 display *)
		| Passthrough of int option
		| Intel of disp_intf_opt * int option

	type media = Disk | Cdrom

	type info = {
		memory: int64;
		boot: string;
		serial: string option;
		monitor: string option;
		vcpus: int;
		usb: string list;
		nics: (string * string * int) list;
		disks: (int * string * media) list;
		acpi: bool;
		disp: disp_opt;
		pci_emulations: string list;
		pci_passthrough: bool;

		(* Xenclient extras *)
		xenclient_enabled: bool;
		hvm: bool;
		sound: string option;
		power_mgmt: int option;
		oem_features: int option;
		inject_sci: int option;
		video_mib: int;

		extras: (string * string option) list;
	}

	val get_vnc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
	val get_tc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option

	val signal : Xenops_task.t -> xs:Xenstore.Xs.xsh -> qemu_domid:int -> domid:Xenctrl.domid -> ?wait_for:string -> ?param:string
	          -> string -> unit

	val cmdline_of_info: info -> bool -> int -> string list

	val start : Xenops_task.t -> xs:Xenstore.Xs.xsh -> dmpath:string -> ?timeout:float -> info -> Xenctrl.domid -> unit
	val start_vnconly : Xenops_task.t -> xs:Xenstore.Xs.xsh -> dmpath:string -> ?timeout:float -> info -> Xenctrl.domid -> unit
	val restore : Xenops_task.t -> xs:Xenstore.Xs.xsh -> dmpath:string -> ?timeout:float -> info -> Xenctrl.domid -> unit
	val suspend : Xenops_task.t -> xs:Xenstore.Xs.xsh -> qemu_domid:int -> Xenctrl.domid -> unit
	val resume : Xenops_task.t -> xs:Xenstore.Xs.xsh -> qemu_domid:int -> Xenctrl.domid -> unit
	val stop : xs:Xenstore.Xs.xsh -> qemu_domid:int -> Xenctrl.domid -> unit
end

val get_vnc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
val get_tc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
