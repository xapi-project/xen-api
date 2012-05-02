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

let service_name = "xenops"

type power_state =
	| Running
	| Halted
	| Suspended
	| Paused

exception Already_exists of (string * string)
exception Does_not_exist of (string * string)
exception Unimplemented of string
exception Domain_not_built
exception Maximum_vcpus of int
exception Bad_power_state of (power_state * power_state)
exception Failed_to_acknowledge_shutdown_request
exception Failed_to_shutdown of (string * float)
exception Device_is_connected
exception Device_not_connected
exception Device_detach_rejected of (string * string * string)
exception Media_not_ejectable
exception Media_present
exception Media_not_present
exception No_bootable_device
exception Bootloader_error of (string * (string list))
exception Cannot_free_this_much_memory of (int64 * int64)
exception Vms_failed_to_cooperate of string list
exception Ballooning_error of (string * string)
exception IO_error
exception Failed_to_contact_remote_service of string
exception Hook_failed of (string * string * string * string)
exception Not_enough_memory of int64
exception Cancelled of string
exception Storage_backend_error of (string * (string list))

type debug_info = string

module Query = struct
	type t = {
		name: string;
		vendor: string;
		version: string;
		features: string list;
		instance_id: string; (* Unique to this invocation of xenopsd *)
	}
end
external query: debug_info -> unit -> Query.t = ""
external get_diagnostics: debug_info -> unit -> string = ""

type disk =
	| Local of string (** path to a local block device *)
	| VDI of string   (** typically "SR/VDI" *)

type disk_list = disk list

(** XXX: this code shouldn't care about the vswitch/bridge difference *)
module Network = struct
	type t =
		| Local of string (** name of a local switch *)
		| Remote of string * string (** vm.id * switch *)
	type ts = t list
end

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
		qemu_stubdom: bool;
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
		framebuffer_ip: string option;
		vncterm: bool;
		vncterm_ip: string option;
	}

	type builder_info =
	| HVM of hvm_info
	| PV of pv_info

	type id = string

	type action =
		| Coredump
		| Shutdown
		| Start
		| Pause

	type scheduler_params = {
		priority: (int * int) option; (* weight, cap *)
		affinity: int list list (* vcpu -> pcpu list *)
	}

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
		vcpu_max: int; (* boot-time maximum *)
		vcpus: int;    (* ideal number to use *)
		scheduler_params: scheduler_params;
		on_crash: action list;
		on_shutdown: action list;
		on_reboot: action list;
		pci_msitranslate: bool;
		pci_power_mgmt: bool;
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
		memory_actual: int64;
		memory_limit: int64;
		vcpu_target: int; (* actual number of vcpus *)
		shadow_multiplier_target: float; (* actual setting *)
		rtc_timeoffset: string;
		uncooperative_balloon_driver: bool;
		guest_agent: (string * string) list;
		xsdata_state: (string * string) list;
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
		msitranslate: bool option;
		power_mgmt: bool option;
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

	type qos_class =
		| Highest | High | Normal | Low | Lowest | Other of int
	type qos_scheduler =
		| RealTime of qos_class
		| Idle
		| BestEffort of qos_class
	type qos =
		| Ionice of qos_scheduler

	type t = {
		id: id;
		position: Device_number.t option;
		mode: mode;
		backend: disk option; (* can be empty *)
		ty: ty;
		unpluggable: bool;
		extra_backend_keys: (string * string) list;
		extra_private_keys: (string * string) list;
		qos: qos option;
	}

	type state = {
		plugged: bool;
		qos_target: qos option;
		media_present: bool;
	}

end

module Vif = struct

	type id = string * string

	type locked_addresses = {
        	ipv4: string list;
        	ipv6: string list;
	}

        type locking_mode =
        	| Unlocked (* all traffic permitted *)
		| Disabled (* no traffic permitted *)
		| Locked of locked_addresses

	type t = {
		id: id;
		position: int;
		mac: string;
		carrier: bool;
		mtu: int;
		rate: (int64 * int64) option;
		backend: Network.t;
		other_config: (string * string) list;
		locking_mode: locking_mode;
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
		pcis: Pci.t list;
		domains: string option;
		(** Opaque data describing per-domain state *)
	}
end

module Task = struct
	type id = string

	type result =
		| Pending of float
		| Completed of float
		| Failed of Rpc.t

	type t = {
		id: id;
		debug_info: string;
		ctime: float;
		result: result;
		subtasks: (string * result) list;
	}
end

module Dynamic = struct
	type id =
		| Vm of Vm.id
		| Vbd of Vbd.id
		| Vif of Vif.id
		| Pci of Pci.id
		| Task of Task.id
		| Barrier of int
	type t =
		| Vm_t of Vm.id * ((Vm.t * Vm.state) option)
		| Vbd_t of Vbd.id * ((Vbd.t * Vbd.state) option)
		| Vif_t of Vif.id * ((Vif.t * Vif.state) option)
		| Pci_t of Pci.id * ((Pci.t * Pci.state) option)
		| Task_t of Task.id * (Task.t option)
end

module TASK = struct
	external stat: debug_info -> Task.id -> Task.t = ""
	external cancel: debug_info -> Task.id -> unit = ""
	external destroy: debug_info -> Task.id -> unit = ""
	external list: debug_info -> Task.t list = ""
end

module HOST = struct
	external get_console_data: debug_info -> string = ""
	external get_total_memory_mib: debug_info -> int64 = ""
	external send_debug_keys: debug_info -> string -> unit = ""
	external set_worker_pool_size: debug_info -> int -> unit = ""
end

module VM = struct
	external add: debug_info -> Vm.t -> Vm.id = ""
	external remove: debug_info -> Vm.id -> unit = ""

	external migrate: debug_info -> Vm.id -> (string * string) list -> (string * Network.t) list -> string -> Task.id = ""

	external create: debug_info -> Vm.id -> Task.id = ""
	external build: debug_info -> Vm.id -> Task.id = ""
	external create_device_model: debug_info -> Vm.id -> bool -> Task.id = ""
	external destroy: debug_info -> Vm.id -> Task.id = ""
	external pause: debug_info -> Vm.id -> Task.id = ""
	external unpause: debug_info -> Vm.id -> Task.id = ""
	external set_xsdata: debug_info -> Vm.id -> (string * string) list -> Task.id = ""
	external set_vcpus: debug_info -> Vm.id -> int -> Task.id = ""
	external set_shadow_multiplier : debug_info -> Vm.id -> float -> Task.id = ""
	external set_memory_dynamic_range : debug_info -> Vm.id -> int64 -> int64 -> Task.id = ""
	external stat: debug_info -> Vm.id -> (Vm.t * Vm.state) = ""
	external list: debug_info -> unit -> (Vm.t * Vm.state) list = ""
	external delay: debug_info -> Vm.id -> float -> Task.id = ""

	external start: debug_info -> Vm.id -> Task.id = ""
	external shutdown: debug_info -> Vm.id -> float option -> Task.id = ""
	external reboot: debug_info -> Vm.id -> float option -> Task.id = ""
	external suspend: debug_info -> Vm.id -> disk -> Task.id = ""
	external resume: debug_info -> Vm.id -> disk -> Task.id = ""

	external s3suspend: debug_info -> Vm.id -> Task.id = ""
	external s3resume: debug_info -> Vm.id -> Task.id = ""

	external generate_state_string: debug_info -> Vm.t -> string = ""
	external export_metadata: debug_info -> Vm.id -> string  = ""
	external import_metadata: debug_info -> string -> Vm.id  = ""
end

module PCI = struct
	external add: debug_info -> Pci.t -> Pci.id  = ""
	external remove: debug_info -> Pci.id -> unit = ""
	external stat: debug_info -> Pci.id -> (Pci.t * Pci.state) = ""
	external list: debug_info -> Vm.id -> (Pci.t * Pci.state) list  = ""
end

module VBD = struct
	external add: debug_info -> Vbd.t -> Vbd.id = ""
	external plug: debug_info -> Vbd.id -> Task.id = ""
	external unplug: debug_info -> Vbd.id -> bool -> Task.id = ""
	external eject: debug_info -> Vbd.id -> Task.id = ""
	external insert: debug_info -> Vbd.id -> disk -> Task.id = ""
	external stat: debug_info -> Vbd.id -> (Vbd.t * Vbd.state) = ""
	external list: debug_info -> Vm.id -> (Vbd.t * Vbd.state) list  = ""
	external remove: debug_info -> Vbd.id -> unit = ""
end

module VIF = struct
	external add: debug_info -> Vif.t -> Vif.id = ""
	external plug: debug_info -> Vif.id -> Task.id = ""
	external unplug: debug_info -> Vif.id -> bool -> Task.id = ""
	external move: debug_info -> Vif.id -> Network.t -> Task.id = ""
	external stat: debug_info -> Vif.id -> (Vif.t * Vif.state) = ""
	external list: debug_info -> Vm.id -> (Vif.t * Vif.state) list  = ""
	external remove: debug_info -> Vif.id -> unit = ""
	external set_carrier: debug_info -> Vif.id -> bool -> Task.id = ""
	external set_locking_mode: debug_info -> Vif.id -> Vif.locking_mode -> Task.id = ""
end

module UPDATES = struct
	external get: debug_info -> int option -> int option -> Dynamic.id list * int option = ""
    external inject_barrier: debug_info -> int -> unit = ""
	external remove_barrier: debug_info -> int -> unit = ""
	external refresh_vm: debug_info -> Vm.id -> unit = ""
end

module DEBUG = struct
	external trigger: debug_info -> string -> string list -> unit = ""
	external shutdown: debug_info -> unit -> unit = ""
end

