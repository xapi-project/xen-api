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

exception Ioemu_failed of string
exception Ioemu_failed_dying

exception Pause_failed
exception Device_shutdown
exception Pause_token_mismatch
exception Device_not_paused
exception Device_not_found

exception Cdrom

module Generic :
sig
	val rm_device_state : xs:Xs.xsh -> device -> unit
	val exists : xs:Xs.xsh -> device -> bool
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

	val device_name : int -> string
	val device_major_minor : string -> int * int
	val major_minor_to_device : int * int -> string

	val add : xs:Xs.xsh -> hvm:bool -> mode:mode
	       -> device_number:Device_number.t
	       -> phystype:physty -> physpath:string
	       -> dev_type:devty
	       -> unpluggable:bool
	       -> ?protocol:protocol
	       -> ?extra_backend_keys:(string*string) list
	       -> ?extra_private_keys:(string*string) list 
	       -> ?backend_domid:Xc.domid
	       -> Xc.domid -> device

	val release : xs:Xs.xsh -> device -> unit
	val media_eject : xs:Xs.xsh -> device_number:Device_number.t -> int -> unit
	val media_insert : xs:Xs.xsh -> device_number:Device_number.t
	                -> physpath:string -> phystype:physty -> int -> unit
	val media_refresh : xs:Xs.xsh -> device_number:Device_number.t -> physpath:string -> int -> unit
	val media_is_ejected : xs:Xs.xsh -> device_number:Device_number.t -> int -> bool
	val media_tray_is_locked : xs:Xs.xsh -> device_number:Device_number.t -> int -> bool

	val pause : xs:Xs.xsh -> device -> string (* token *)
	val unpause : xs:Xs.xsh -> device -> string (* token *) -> unit
	val is_paused : xs:Xs.xsh -> device -> bool

	(* For migration: *)
	val hard_shutdown_request : xs:Xs.xsh -> device -> unit
	val hard_shutdown_complete : xs:Xs.xsh -> device -> string Watch.t

	(* For testing: *)
	val request_shutdown : xs:Xs.xsh -> device -> bool -> unit
end

module Vif :
sig
	exception Invalid_Mac of string

	val add : xs:Xs.xsh -> devid:int -> netty:Netman.netty
	       -> mac:string -> carrier:bool 
	       -> ?mtu:int -> ?rate:(int64 * int64) option
	       -> ?protocol:protocol -> ?backend_domid:Xc.domid 
	       -> ?other_config:((string * string) list) 
	       -> ?extra_private_keys:(string * string) list -> Xc.domid
	       -> device
	val set_carrier : xs:Xs.xsh -> device -> bool -> unit
	val release : xs:Xs.xsh -> device -> unit
end

val clean_shutdown : xs:Xs.xsh -> device -> unit
val hard_shutdown  : xs:Xs.xsh -> device -> unit

val can_surprise_remove : xs:Xs.xsh -> device -> bool

module Vcpu :
sig
	val add : xs:Xs.xsh -> devid:int -> int -> unit
	val del : xs:Xs.xsh -> devid:int -> int -> unit
	val set : xs:Xs.xsh -> devid:int -> int -> bool -> unit
	val status : xs:Xs.xsh -> devid:int -> int -> bool
end

module PV_Vnc :
sig
	exception Failed_to_start
	val save : xs:Xs.xsh -> Xc.domid -> unit
	val get_statefile : xs:Xs.xsh -> Xc.domid -> string option
	val start : ?statefile:string -> xs:Xs.xsh -> Xc.domid -> int

	val vnc_port_path : Xc.domid -> string
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

	val add : xc:Xc.handle -> xs:Xs.xsh -> hvm:bool -> msitranslate:int -> pci_power_mgmt:int
	       -> ?flrscript:string option -> dev list -> Xc.domid -> int -> unit
	val release : xc:Xc.handle -> xs:Xs.xsh -> hvm:bool
	       -> (int * int * int * int) list -> Xc.domid -> int -> unit
	val reset : xs:Xs.xsh -> dev -> unit
	val bind : dev list -> unit
	val plug : xc:Xc.handle -> xs:Xs.xsh -> dev -> Xc.domid -> unit
	val unplug : xc:Xc.handle -> xs:Xs.xsh -> dev -> Xc.domid -> unit
	val list : xc:Xc.handle -> xs:Xs.xsh -> Xc.domid -> (int * dev) list
end

module Vfb :
sig
	val add : xc:Xc.handle -> xs:Xs.xsh -> hvm:bool -> ?protocol:protocol -> Xc.domid -> unit
end

module Vkbd :
sig 
	val add : xc:Xc.handle -> xs:Xs.xsh -> hvm:bool -> ?protocol:protocol -> Xc.domid -> unit
end

module Dm :
sig
	type disp_intf_opt =
	    | Std_vga
	    | Cirrus

	type disp_opt =
		| NONE
		| VNC of disp_intf_opt * bool * int * string (* auto-allocate, port if previous false, keymap *)
		| SDL of disp_intf_opt * string (* X11 display *)
		| Passthrough of int option
		| Intel of disp_intf_opt * int option

	type info = {
		memory: int64;
		boot: string;
		serial: string;
		vcpus: int;
		usb: string list;
		nics: (string * string * int) list;
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
		videoram: int;
	       
		extras: (string * string option) list;
	}

	val write_logfile_to_log : int -> unit
	val unlink_logfile : int -> unit

	val vnc_port_path : Xc.domid -> string

	val signal : xs:Xs.xsh -> domid:Xc.domid -> ?wait_for:string -> ?param:string
	          -> string -> unit

	val start : xs:Xs.xsh -> dmpath:string -> ?timeout:float -> info -> Xc.domid -> int
	val restore : xs:Xs.xsh -> dmpath:string -> ?timeout:float -> info -> Xc.domid -> int
	val suspend : xs:Xs.xsh -> Xc.domid -> unit
	val resume : xs:Xs.xsh -> Xc.domid -> unit
	val stop : xs:Xs.xsh -> Xc.domid -> unit
end

val vnc_port_path: xc:Xc.handle -> xs:Xs.xsh -> Xc.domid -> string
