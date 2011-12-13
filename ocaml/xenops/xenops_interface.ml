(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
(**
 * @group Xenops
 *)

type ('a, 'b) result =
	| Success of 'a
	| Failure of 'b

type power_state =
	| Running
	| Halted
	| Suspended
	| Paused

type error =
	| Internal_error of string
	| Already_exists
	| Does_not_exist
	| Unimplemented
	| Domain_not_built
	| Bad_power_state of power_state * power_state
	| Failed_to_acknowledge_shutdown_request
	| Failed_to_shutdown
	| Device_is_connected
	| Device_not_connected
	| Media_not_ejectable
	| Media_present
	| Media_not_present
	| No_bootable_device
	| Bootloader_error of string * (string list)
	| Ballooning_error of string * string
	| No_ballooning_service
	| Not_supported
	| IO_error
	| VDI_not_found of string
	| Caller_must_pass_file_descriptor
	| Failed_to_contact_remote_service of string

type error_response = unit option * error option
type string_response = string option * error option

module Query = struct
	type t = {
		name: string;
		vendor: string;
		version: string;
		features: string list;
	}
end
external query: unit -> (Query.t option * error option) = ""

type disk =
	| Local of string (** path to a local block device *)
	| VDI of string   (** typically "SR/VDI" *)

type disk_list = disk list

(** XXX: this code shouldn't care about the vswitch/bridge difference *)
type network =
	| Bridge of string (** name of a local bridge *)
	| VSwitch of string (** name of a local vswitch *)
	| Netback of string * string (** vm.id * backend *)
type network_list = network list

module Vm = struct
	type video_card =
		| Cirrus
		| Standard_VGA

	type hvm_info = {
		hap: bool;
		shadow_multiplier: float;
		timeoffset: string;
		video_mib: int;
		video: video_card;
		acpi: bool;
		serial: string option;
		keymap: string option;
		vnc_ip: string option;
		pci_emulations: string list;
		pci_passthrough: bool;
		boot_order: string;
		qemu_disk_cmdline: bool;
	}

	type pv_direct_boot = {
		kernel: string;
		cmdline: string;
		ramdisk: string option;
	}

	type pv_indirect_boot = {
		bootloader: string;
		extra_args: string;
		legacy_args: string;
		bootloader_args: string;
		devices: disk list;
	}

	type pv_boot =
		| Direct of pv_direct_boot
		| Indirect of pv_indirect_boot

	type pv_info = {
		boot: pv_boot;
		framebuffer: bool;
	}

	type builder_info =
	| HVM of hvm_info
	| PV of pv_info

	type id = string

	type action =
		| Coredump
		| Shutdown
		| Start

	type t = {
		id: id;
		name: string;
		ssidref: int32;
		xsdata: (string * string) list;
		platformdata: (string * string) list;
		bios_strings: (string * string) list;
		ty: builder_info;
		suppress_spurious_page_faults: bool;
		machine_address_size: int option;
		memory_static_max: int64;
		memory_dynamic_max: int64;
		memory_dynamic_min: int64;
		vcpus: int;
		on_crash: action list;
		on_shutdown: action list;
		on_reboot: action list;
	}

	type console_protocol =
		| Rfb
		| Vt100

	type console = {
		protocol: console_protocol;
		port: int;
	}

	type state = {
		power_state: power_state;
		domids: int list;
		consoles: console list;
		memory_target: int64;
		rtc_timeoffset: string;
		uncooperative_balloon_driver: bool;
		guest_agent: (string * string) list;
		last_start_time: float;
	}

end

module Pci = struct

	type id = string * string

	type t = {
		id: id;
		position: int;
		domain: int;
		bus: int;
		dev: int;
		fn: int;
		msitranslate: bool;
		power_mgmt: bool;
	}

	type state = {
		plugged: bool;
	}
end

module Vbd = struct

	type mode = ReadOnly | ReadWrite

	type ty = CDROM | Disk

	type id = string * string

	(* FIXME: take a URL and call VDI.attach ourselves *)

	type t = {
		id: id;
		position: Device_number.t option;
		mode: mode;
		backend: disk option; (* can be empty *)
		ty: ty;
		unpluggable: bool;
		extra_backend_keys: (string * string) list;
		extra_private_keys: (string * string) list;
	}

	type state = {
		plugged: bool;
		kthread_pid: int;
		media_present: bool;
	}

end

module Vif = struct

	type id = string * string

	type t = {
		id: id;
		position: int;
		mac: string;
		carrier: bool;
		mtu: int;
		rate: (int64 * int64) option;
		backend: network;
		other_config: (string * string) list;
		extra_private_keys: (string * string) list;
	}

	type state = {
		plugged: bool;
		kthread_pid: int;
		media_present: bool;
	}
end

module Metadata = struct
	type t = {
		vm: Vm.t;
		vbds: Vbd.t list;
		vifs: Vif.t list;
		domains: string option;
		(** Opaque data describing per-domain state *)
	}
end

module Task = struct
	type id = string

	type result =
		| Pending of float
		| Completed of float
		| Failed of error

	type t = {
		id: id;
		result: result;
		subtasks: (string * result) list;
	}
end

module Dynamic = struct
	type id =
		| Vm of Vm.id
		| Vbd of Vbd.id
		| Vif of Vif.id
		| Task of Task.id
	type t =
		| Vm_t of Vm.t * Vm.state
		| Vbd_t of Vbd.t * Vbd.state
		| Vif_t of Vif.t * Vif.state
		| Task_t of Task.t
end

module TASK = struct
	external stat: Task.id -> (Task.t option) * (error option) = ""
	external cancel: Task.id -> (unit option) * (error option) = ""
end

module VM = struct
	external add: Vm.t -> (Vm.id option) * (error option) = ""
	external remove: Vm.id -> (unit option) * (error option) = ""

	external create: Vm.id -> (Task.id option) * (error option) = ""
	external build: Vm.id -> (Task.id option) * (error option) = ""
	external create_device_model: Vm.id -> bool -> (Task.id option) * (error option) = ""
	external destroy: Vm.id -> (Task.id option) * (error option) = ""
	external pause: Vm.id -> (Task.id option) * (error option) = ""
	external unpause: Vm.id -> (Task.id option) * (error option) = ""
	external stat: Vm.id -> ((Vm.t * Vm.state) option) * (error option) = ""
	external list: unit -> ((Vm.t * Vm.state) list option) * (error option) = ""

	external start: Vm.id -> (Task.id option) * (error option) = ""
	external shutdown: Vm.id -> float option -> (Task.id option) * (error option) = ""
	external reboot: Vm.id -> float option -> (Task.id option) * (error option) = ""
	external suspend: Vm.id -> disk -> (Task.id option) * (error option) = ""
	external resume: Vm.id -> disk -> (Task.id option) * (error option) = ""

	external migrate: Vm.id -> string -> (Task.id option) * (error option) = ""

	external export_metadata: Vm.id -> (string option) * (error option) = ""
	external import_metadata: string -> (Vm.id option) * (error option) = ""
end

module PCI = struct
	external add: Pci.t -> (Pci.id option) * (error option) = ""
	external remove: Pci.id -> (unit option) * (error option) = ""
	external list: Vm.id -> ((Pci.t * Pci.state) list option) * (error option) = ""
end

module VBD = struct
	external add: Vbd.t -> (Vbd.id option) * (error option) = ""
	external plug: Vbd.id -> (Task.id option) * (error option) = ""
	external unplug: Vbd.id -> (Task.id option) * (error option) = ""
	external eject: Vbd.id -> (Task.id option) * (error option) = ""
	external insert: Vbd.id -> disk -> (Task.id option) * (error option) = ""
	external stat: Vbd.id -> ((Vbd.t * Vbd.state) option) * (error option) = ""
	external list: Vm.id -> ((Vbd.t * Vbd.state) list option) * (error option) = ""
	external remove: Vbd.id -> (unit option) * (error option) = ""
end

module VIF = struct
	external add: Vif.t -> (Vif.id option) * (error option) = ""
	external plug: Vif.id -> (Task.id option) * (error option) = ""
	external unplug: Vif.id -> (Task.id option) * (error option) = ""
	external stat: Vif.id -> ((Vif.t * Vif.state) option) * (error option) = ""
	external list: Vm.id -> ((Vif.t * Vif.state) list option) * (error option) = ""
	external remove: Vif.id -> (unit option) * (error option) = ""
end

module UPDATES = struct
	external get: int option -> int option -> (Dynamic.t list * int option) option * (error option) = "" 
end

module DEBUG = struct
	external trigger: string -> string list -> (unit option) * (error option) = ""
end

