(*
 * Copyright (C) Citrix Systems Inc.
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

open Xenops_interface
open Xenops_utils
open Xenops_task

type domain_action_request =
	| Needs_poweroff
	| Needs_reboot
	| Needs_suspend
	| Needs_crashdump	
with rpc

type device_action_request =
	| Needs_unplug
	| Needs_set_qos

type shutdown_request =
	| Halt
	| Reboot
	| PowerOff
	| S3Suspend
	| Suspend
with rpc
let string_of_shutdown_request x = x |> rpc_of_shutdown_request |> Jsonrpc.to_string

let test = Updates.empty
let string_of_disk d = d |> rpc_of_disk |> Jsonrpc.to_string
type data =
	| Disk of disk
	| FD of Unix.file_descr
with rpc
let string_of_data x = x |> rpc_of_data |> Jsonrpc.to_string

type flag =
	| Live
with rpc
let string_of_flag x = x |> rpc_of_flag |> Jsonrpc.to_string
let string_of_flag = function
	| Live -> "Live"

type progress_cb = float -> unit

module type S = sig
	val init: unit -> unit
	module HOST : sig
		val stat: unit -> Host.t
		val get_console_data: unit -> string
		val get_total_memory_mib: unit -> int64
		val send_debug_keys: string -> unit
		val mask_features: string -> string -> string
	end
	module VM : sig
		val add: Vm.t -> unit
		val remove: Vm.t -> unit
		val create: Xenops_task.t -> int64 option -> Vm.t -> unit
		val build: Xenops_task.t -> Vm.t -> Vbd.t list -> Vif.t list -> unit (* XXX cancel *)
		val create_device_model: Xenops_task.t -> Vm.t -> Vbd.t list -> Vif.t list -> bool -> unit
		val destroy_device_model: Xenops_task.t -> Vm.t -> unit
		val destroy: Xenops_task.t -> Vm.t -> unit
		val pause: Xenops_task.t -> Vm.t -> unit
		val unpause: Xenops_task.t -> Vm.t -> unit
		val set_xsdata: Xenops_task.t -> Vm.t -> (string * string) list -> unit
		val set_vcpus: Xenops_task.t -> Vm.t -> int -> unit
		val set_shadow_multiplier: Xenops_task.t -> Vm.t -> float -> unit
		val set_memory_dynamic_range: Xenops_task.t -> Vm.t -> int64 -> int64 -> unit
		val request_shutdown: Xenops_task.t -> Vm.t -> shutdown_request -> float -> bool
		val wait_shutdown: Xenops_task.t -> Vm.t -> shutdown_request -> float -> bool

		val save: Xenops_task.t -> progress_cb -> Vm.t -> flag list -> data -> unit
		val restore: Xenops_task.t -> progress_cb -> Vm.t -> Vbd.t list -> Vif.t list -> data -> unit

		val s3suspend: Xenops_task.t -> Vm.t -> unit
		val s3resume: Xenops_task.t -> Vm.t -> unit

		val get_state: Vm.t -> Vm.state

		val set_domain_action_request: Vm.t -> domain_action_request option -> unit
		val get_domain_action_request: Vm.t -> domain_action_request option

		val generate_state_string: Vm.t -> string
		val get_internal_state: (string * string) list -> (string * Network.t) list -> Vm.t -> string
		val set_internal_state: Vm.t -> string -> unit

		val minimum_reboot_delay: float
	end
	module PCI : sig
		val get_state: Vm.id -> Pci.t -> Pci.state
		val plug: Xenops_task.t -> Vm.id -> Pci.t -> unit
		val unplug: Xenops_task.t -> Vm.id -> Pci.t -> unit
		val get_device_action_request: Vm.id -> Pci.t -> device_action_request option
	end
	module VBD : sig
		val set_active: Xenops_task.t -> Vm.id -> Vbd.t -> bool -> unit
		val epoch_begin: Xenops_task.t -> Vm.id -> disk -> unit
		val epoch_end: Xenops_task.t -> Vm.id -> disk -> unit
		val plug: Xenops_task.t -> Vm.id -> Vbd.t -> unit
		val unplug: Xenops_task.t -> Vm.id -> Vbd.t -> bool -> unit
		val insert: Xenops_task.t -> Vm.id -> Vbd.t -> disk -> unit
		val eject: Xenops_task.t -> Vm.id -> Vbd.t -> unit

		val set_qos: Xenops_task.t -> Vm.id -> Vbd.t -> unit

		val get_state: Vm.id -> Vbd.t -> Vbd.state

		val get_device_action_request: Vm.id -> Vbd.t -> device_action_request option
	end
	module VIF : sig
		val set_active: Xenops_task.t -> Vm.id -> Vif.t -> bool -> unit
		val plug: Xenops_task.t -> Vm.id -> Vif.t -> unit
		val unplug: Xenops_task.t -> Vm.id -> Vif.t -> bool -> unit
		val move: Xenops_task.t -> Vm.id -> Vif.t -> Network.t -> unit
		val set_carrier: Xenops_task.t -> Vm.id -> Vif.t -> bool -> unit
		val set_locking_mode: Xenops_task.t -> Vm.id -> Vif.t -> Vif.locking_mode -> unit

		val get_state: Vm.id -> Vif.t -> Vif.state

		val get_device_action_request: Vm.id -> Vif.t -> device_action_request option
	end
	module UPDATES : sig
		val get: Updates.id option -> int option -> Dynamic.barrier list * Dynamic.id list * Updates.id 
	end
	module DEBUG : sig
		val trigger: string -> string list -> unit
	end
end
