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

open Sexplib.Std

let service_name = "xenops"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir "xenopsd")
let forwarded_path = ref (Filename.concat default_sockets_dir "xenopsd" ^ ".forwarded")

let set_sockets_dir x =
	default_path := Filename.concat x "xenopsd";
	forwarded_path := !default_path ^ ".forwarded"

let default_uri () = "file:" ^ !default_path
let json_url () = Printf.sprintf "file:%s.json" !default_path

type power_state =
	| Running
	| Halted
	| Suspended
	| Paused
with sexp

exception Already_exists of (string * string)
exception Does_not_exist of (string * string)
exception Unimplemented of string
exception Domain_not_built
exception Invalid_vcpus of int
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
exception Bootloader_error of (string * string)
exception Cannot_free_this_much_memory of (int64 * int64)
exception Vms_failed_to_cooperate of string list
exception Ballooning_error of (string * string)
exception IO_error
exception Failed_to_contact_remote_service of string
exception Hook_failed of (string * string * string * string)
exception Not_enough_memory of int64
exception Cancelled of string
exception Storage_backend_error of (string * (string list))
exception PCIBack_not_loaded
exception Failed_to_run_script of string

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
with sexp

type disk_list = disk list

(** XXX: this code shouldn't care about the vswitch/bridge difference *)
module Network = struct
	type t =
		| Local of string (** name of a local switch *)
		| Remote of string * string (** vm.id * switch *)
	type ts = t list
end

module Pci = struct
	type address = {
		domain: int;
		bus: int;
		dev: int;
		fn: int;
	}
	with sexp

	let address_of_string str =
		Scanf.sscanf str "%04x:%02x:%02x.%02x"
			(fun domain bus dev fn -> {domain; bus; dev; fn})

	let string_of_address address =
		Printf.sprintf "%04x:%02x:%02x.%01x"
			address.domain address.bus address.dev address.fn

	type id = string * string

	type t = {
		id: id;
		position: int;
		address: address;
		msitranslate: bool option;
		power_mgmt: bool option;
	}

	type state = {
		plugged: bool;
	}
end

module Vgpu = struct
	type gvt_g = {
		physical_pci_address: Pci.address;
		low_gm_sz: int64;
		high_gm_sz: int64;
		fence_sz: int64;
	}
	with sexp

	type nvidia = {
		physical_pci_address: Pci.address;
		config_file: string;
	}
	with sexp

	type implementation =
		| GVT_g of gvt_g
		| Nvidia of nvidia

	type id = string * string

	type t = {
		id: id;
		position: int;
		implementation: implementation;
	}

	type state = {
		plugged: bool;
		emulator_pid: int option;
	}
end

module Vm = struct
	type igd_passthrough =
		| GVT_d
	with sexp

	type video_card =
		| Cirrus
		| Standard_VGA
		| Vgpu
		| IGD_passthrough of igd_passthrough
	with sexp

	let default_video_card = Cirrus
 
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
	with sexp

	let default_hvm_info = {
		hap = true;
		shadow_multiplier = 1.0;
		timeoffset = "";
		video_mib = 4;
		video = default_video_card;
		acpi = true;
		serial = None;
		keymap = None;
		vnc_ip = None;
		pci_emulations = [];
		pci_passthrough = false;
		boot_order = "";
		qemu_disk_cmdline = false;
		qemu_stubdom = false;
	}

	type pv_direct_boot = {
		kernel: string;
		cmdline: string;
		ramdisk: string option;
	}
	with sexp

	let default_pv_direct_boot = {
		kernel = "";
		cmdline = "";
		ramdisk = None;
	}

	type pv_indirect_boot = {
		bootloader: string;
		extra_args: string;
		legacy_args: string;
		bootloader_args: string;
		devices: disk list;
	}
	with sexp

	let default_pv_indirect_boot = {
		bootloader = "";
		extra_args = "";
		legacy_args = "";
		bootloader_args = "";
		devices = [];
	}

	type pv_boot =
		| Direct of pv_direct_boot
		| Indirect of pv_indirect_boot
	with sexp

	let default_pv_boot = Direct default_pv_direct_boot

	type pv_info = {
		boot: pv_boot;
		framebuffer: bool;
		framebuffer_ip: string option;
		vncterm: bool;
		vncterm_ip: string option;
	}
	with sexp

	let default_pv_info = {
		boot = default_pv_boot;
		framebuffer = true;
		framebuffer_ip = None;
		vncterm = true;
		vncterm_ip = None;
	}

	type builder_info =
	| HVM of hvm_info
	| PV of pv_info
	with sexp

	let default_builder_info = HVM default_hvm_info

	type id = string with sexp

	let default_id = ""

	type action =
		| Coredump
		| Shutdown
		| Start
		| Pause
	with sexp

	let default_action = Coredump

	type scheduler_params = {
		priority: (int * int) option; (* weight, cap *)
		affinity: int list list (* vcpu -> pcpu list *)
	} with sexp

	let default_scheduler_params = {
		priority = None;
		affinity = [];
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
		auto_update_drivers: bool;
	} with sexp

	let default_t = {
		id = default_id;
		name = "unnamed";
		ssidref = 0l;
		xsdata = [];
		platformdata = [];
		bios_strings = [];
		ty = default_builder_info;
		suppress_spurious_page_faults = false;
		machine_address_size = None;
		memory_static_max = 0L;
		memory_dynamic_max = 0L;
		memory_dynamic_min = 0L;
		vcpu_max = 0;
		vcpus = 0;
		scheduler_params = default_scheduler_params;
		on_crash = [];
		on_shutdown = [];
		on_reboot = [];
		pci_msitranslate = false;
		pci_power_mgmt = false;
		auto_update_drivers = false;
	}

	let t_of_rpc rpc = Rpc.struct_extend rpc (rpc_of_t default_t) |> t_of_rpc

	type console_protocol =
		| Rfb
		| Vt100
	with sexp

	type console = {
		protocol: console_protocol;
		port: int;
		path: string;
	} with sexp

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
		hvm: bool;
	} with sexp

end

module Vbd = struct

	type mode = ReadOnly | ReadWrite

	type ty = CDROM | Disk | Floppy

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
		persistent: bool;
	}

	let default_t = {
		id = "", "";
		position = None;
		mode = ReadWrite;
		backend = None;
		ty = Disk;
		unpluggable = true;
		extra_backend_keys = [];
		extra_private_keys = [];
		qos = None;
		persistent = true;
	}

	let t_of_rpc rpc = Rpc.struct_extend rpc (rpc_of_t default_t) |> t_of_rpc

	type state = {
		active: bool;
		plugged: bool;
		qos_target: qos option;
		backend_present: disk option;
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
		active: bool;
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
		vgpus: Vgpu.t list;
		domains: string option;
		(** Opaque data describing per-domain state *)
	}

	let default_t = {
		vm = Vm.default_t;
		vbds = [];
		vifs = [];
		pcis = [];
		vgpus = [];
		domains = None;
	}

	let t_of_rpc rpc = Rpc.struct_extend rpc (rpc_of_t default_t) |> t_of_rpc
end

module Task = struct
	type id = string

	type async_result = Rpc.t

	type completion_t = {
		duration : float;
		result : async_result option
	}

	type state =
		| Pending of float
		| Completed of completion_t
		| Failed of Rpc.t

	type t = {
		id: id;
		dbg: string;
		ctime: float;
		state: state;
		subtasks: (string * state) list;
		debug_info: (string * string) list;
		backtrace: string; (* An s-expression encoded Backtrace.t *)
	}
end

module Dynamic = struct
	type id =
		| Vm of Vm.id
		| Vbd of Vbd.id
		| Vif of Vif.id
		| Pci of Pci.id
		| Vgpu of Vgpu.id
		| Task of Task.id
	type barrier = int * (id list)
	type t =
		| Vm_t of Vm.id * ((Vm.t * Vm.state) option)
		| Vbd_t of Vbd.id * ((Vbd.t * Vbd.state) option)
		| Vif_t of Vif.id * ((Vif.t * Vif.state) option)
		| Pci_t of Pci.id * ((Pci.t * Pci.state) option)
		| Vgpu_t of Vgpu.id * ((Vgpu.t * Vgpu.state) option)
		| Task_t of Task.id * (Task.t option)
end

module Host = struct
	type cpu_info = {
		cpu_count: int;
		socket_count: int;
		vendor: string;
		speed: string;
		modelname: string;
		family: string;
		model: string;
		stepping: string;
		flags: string;
		features: int64 array;
		features_pv: int64 array;
		features_hvm: int64 array;
	}
	type hypervisor = {
		version: string;
		capabilities: string;
	}

	type t = {
		cpu_info: cpu_info;
		hypervisor: hypervisor;
	}

	type guest_agent_feature = {
		name : string;
		licensed : bool;
		parameters : (string * string) list;
	}
end

module TASK = struct
	external stat: debug_info -> Task.id -> Task.t = ""
	external cancel: debug_info -> Task.id -> unit = ""
	external destroy: debug_info -> Task.id -> unit = ""
	external list: debug_info -> Task.t list = ""
end

module HOST = struct
	external stat: debug_info -> Host.t = ""
	external get_console_data: debug_info -> string = ""
	external get_total_memory_mib: debug_info -> int64 = ""
	external send_debug_keys: debug_info -> string -> unit = ""
	external set_worker_pool_size: debug_info -> int -> unit = ""
	external update_guest_agent_features: debug_info ->
		Host.guest_agent_feature list -> unit = ""
	external upgrade_cpu_features: debug_info ->
		int64 array -> bool -> int64 array = ""
end

module VM = struct
	external add: debug_info -> Vm.t -> Vm.id = ""
	external remove: debug_info -> Vm.id -> unit = ""

	external generate_state_string: debug_info -> Vm.t -> string = ""

	external migrate: debug_info -> Vm.id -> (string * string) list -> (string * Network.t) list -> string -> Task.id = ""
	external migrate_receive_memory: debug_info -> Vm.id -> int64 -> string -> Xcp_channel.t -> Task.id option = ""

	external create: debug_info -> Vm.id -> Task.id = ""
	external build: debug_info -> Vm.id -> Task.id = ""
	external create_device_model: debug_info -> Vm.id -> bool -> Task.id = ""
	external destroy: debug_info -> Vm.id -> Task.id = ""
	external pause: debug_info -> Vm.id -> Task.id = ""
	external unpause: debug_info -> Vm.id -> Task.id = ""
	external request_rdp: debug_info -> Vm.id -> bool -> Task.id = ""
	external run_script: debug_info -> Vm.id -> string -> Task.id = ""
	external set_xsdata: debug_info -> Vm.id -> (string * string) list -> Task.id = ""
	external set_vcpus: debug_info -> Vm.id -> int -> Task.id = ""
	external set_shadow_multiplier : debug_info -> Vm.id -> float -> Task.id = ""
	external set_memory_dynamic_range : debug_info -> Vm.id -> int64 -> int64 -> Task.id = ""
	external stat: debug_info -> Vm.id -> (Vm.t * Vm.state) = ""
	external exists: debug_info -> Vm.id -> bool = ""
	external list: debug_info -> unit -> (Vm.t * Vm.state) list = ""
	external delay: debug_info -> Vm.id -> float -> Task.id = ""

	external start: debug_info -> Vm.id -> Task.id = ""
	external shutdown: debug_info -> Vm.id -> float option -> Task.id = ""
	external reboot: debug_info -> Vm.id -> float option -> Task.id = ""
	external suspend: debug_info -> Vm.id -> disk -> Task.id = ""
	external resume: debug_info -> Vm.id -> disk -> Task.id = ""

	external s3suspend: debug_info -> Vm.id -> Task.id = ""
	external s3resume: debug_info -> Vm.id -> Task.id = ""

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

module VGPU = struct
	external add: debug_info -> Vgpu.t -> Vgpu.id = ""
	external remove: debug_info -> Vgpu.id -> unit = ""
	external stat: debug_info -> Vgpu.id -> (Vgpu.t * Vgpu.state) = ""
	external list: debug_info -> Vm.id -> (Vgpu.t * Vgpu.state) list = ""
end

module UPDATES = struct
	external get: debug_info -> int option -> int option -> Dynamic.barrier list * Dynamic.id list * int = ""
	external last_id: debug_info -> int = ""
	external inject_barrier: debug_info -> Vm.id -> int -> unit = ""
	external remove_barrier: debug_info -> int -> unit = ""
	external refresh_vm: debug_info -> Vm.id -> unit = ""
end

module DEBUG = struct
	external trigger: debug_info -> string -> string list -> unit = ""
	external shutdown: debug_info -> unit -> unit = ""
end

