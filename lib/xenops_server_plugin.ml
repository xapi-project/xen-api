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
[@@deriving rpc]

type device_action_request =
  | Needs_unplug
  | Needs_set_qos

type shutdown_request =
  | Halt
  | Reboot
  | PowerOff
  | S3Suspend
  | Suspend
[@@deriving rpc]
let string_of_shutdown_request x = x |> rpc_of_shutdown_request |> Jsonrpc.to_string

let test = Updates.empty
let string_of_disk d = d |> rpc_of_disk |> Jsonrpc.to_string
type data =
  | Disk of disk
  | FD of Unix.file_descr
[@@deriving rpc]
let string_of_data x = x |> rpc_of_data |> Jsonrpc.to_string

type flag =
  | Live
[@@deriving rpc]
let string_of_flag x = x |> rpc_of_flag |> Jsonrpc.to_string
let string_of_flag = function
  | Live -> "Live"

type progress_cb = float -> unit

module type S = sig
  val init: unit -> unit
  val simplified: bool
  module HOST : sig
    val stat: unit -> Host.t
    val get_console_data: unit -> string
    val get_total_memory_mib: unit -> int64
    val send_debug_keys: string -> unit
    val update_guest_agent_features : Host.guest_agent_feature list -> unit
    val upgrade_cpu_features : int64 array -> bool -> int64 array
  end
  module VM : sig
    val add: Vm.t -> unit
    val remove: Vm.t -> unit
    val create: Xenops_task.task_handle -> int64 option -> Vm.t -> unit
    val build: ?restore_fd:Unix.file_descr -> Xenops_task.task_handle -> Vm.t -> Vbd.t list -> Vif.t list -> Vgpu.t list -> Vusb.t list-> string list -> bool ->  unit (* XXX cancel *)
    val create_device_model: Xenops_task.task_handle -> Vm.t -> Vbd.t list -> Vif.t list -> Vgpu.t list -> Vusb.t list -> bool -> unit
    val destroy_device_model: Xenops_task.task_handle -> Vm.t -> unit
    val destroy: Xenops_task.task_handle -> Vm.t -> unit
    val pause: Xenops_task.task_handle -> Vm.t -> unit
    val unpause: Xenops_task.task_handle -> Vm.t -> unit
    val set_xsdata: Xenops_task.task_handle -> Vm.t -> (string * string) list -> unit
    val set_vcpus: Xenops_task.task_handle -> Vm.t -> int -> unit
    val set_shadow_multiplier: Xenops_task.task_handle -> Vm.t -> float -> unit
    val set_memory_dynamic_range: Xenops_task.task_handle -> Vm.t -> int64 -> int64 -> unit
    val request_shutdown: Xenops_task.task_handle -> Vm.t -> shutdown_request -> float -> bool
    val wait_shutdown: Xenops_task.task_handle -> Vm.t -> shutdown_request -> float -> bool

    val save: Xenops_task.task_handle -> progress_cb -> Vm.t -> flag list -> data -> data option -> unit
    val restore: Xenops_task.task_handle -> progress_cb -> Vm.t -> Vbd.t list -> Vif.t list -> data -> data option -> string list -> unit

    val s3suspend: Xenops_task.task_handle -> Vm.t -> unit
    val s3resume: Xenops_task.task_handle -> Vm.t -> unit

    val get_state: Vm.t -> Vm.state
    val request_rdp: Vm.t -> bool -> unit
    val run_script: Xenops_task.task_handle -> Vm.t -> string -> Rpc.t
    val set_domain_action_request: Vm.t -> domain_action_request option -> unit
    val get_domain_action_request: Vm.t -> domain_action_request option

    val generate_state_string: Vm.t -> string
    val get_internal_state: (string * string) list -> (string * Network.t) list -> Vm.t -> string
    val set_internal_state: Vm.t -> string -> unit

    val wait_ballooning: Xenops_task.task_handle -> Vm.t -> unit

    val minimum_reboot_delay: float
  end
  module PCI : sig
    val get_state: Vm.id -> Pci.t -> Pci.state
    val plug: Xenops_task.task_handle -> Vm.id -> Pci.t -> unit
    val unplug: Xenops_task.task_handle -> Vm.id -> Pci.t -> unit
    val get_device_action_request: Vm.id -> Pci.t -> device_action_request option
  end
  module VBD : sig
    val set_active: Xenops_task.task_handle -> Vm.id -> Vbd.t -> bool -> unit
    val epoch_begin: Xenops_task.task_handle -> Vm.id -> disk -> bool -> unit
    val epoch_end: Xenops_task.task_handle -> Vm.id -> disk -> unit
    val plug: Xenops_task.task_handle -> Vm.id -> Vbd.t -> unit
    val unplug: Xenops_task.task_handle -> Vm.id -> Vbd.t -> bool -> unit
    val insert: Xenops_task.task_handle -> Vm.id -> Vbd.t -> disk -> unit
    val eject: Xenops_task.task_handle -> Vm.id -> Vbd.t -> unit

    val set_qos: Xenops_task.task_handle -> Vm.id -> Vbd.t -> unit

    val get_state: Vm.id -> Vbd.t -> Vbd.state

    val get_device_action_request: Vm.id -> Vbd.t -> device_action_request option
  end
  module VIF : sig
    val set_active: Xenops_task.task_handle -> Vm.id -> Vif.t -> bool -> unit
    val plug: Xenops_task.task_handle -> Vm.id -> Vif.t -> unit
    val unplug: Xenops_task.task_handle -> Vm.id -> Vif.t -> bool -> unit
    val move: Xenops_task.task_handle -> Vm.id -> Vif.t -> Network.t -> unit
    val set_carrier: Xenops_task.task_handle -> Vm.id -> Vif.t -> bool -> unit
    val set_locking_mode: Xenops_task.task_handle -> Vm.id -> Vif.t -> Vif.locking_mode -> unit
    val set_ipv4_configuration: Xenops_task.task_handle -> Vm.id -> Vif.t -> Vif.ipv4_configuration -> unit
    val set_ipv6_configuration: Xenops_task.task_handle -> Vm.id -> Vif.t -> Vif.ipv6_configuration -> unit
    val set_pvs_proxy: Xenops_task.task_handle -> Vm.id -> Vif.t -> Vif.PVS_proxy.t option -> unit

    val get_state: Vm.id -> Vif.t -> Vif.state

    val get_device_action_request: Vm.id -> Vif.t -> device_action_request option
  end
  module VGPU : sig
    val start: Xenops_task.task_handle -> Vm.id -> Vgpu.t -> bool -> unit
    val get_state: Vm.id -> Vgpu.t -> Vgpu.state
  end
  module VUSB :sig
    val plug: Xenops_task.task_handle -> Vm.id -> Vusb.t -> unit
    val unplug: Xenops_task.task_handle -> Vm.id -> Vusb.t -> unit
    val get_state: Vm.id -> Vusb.t -> Vusb.state
    val get_device_action_request: Vm.id -> Vusb.t -> device_action_request option
  end
  module UPDATES : sig
    val get: Updates.id option -> int option -> Dynamic.barrier list * Dynamic.id list * Updates.id
  end
  module DEBUG : sig
    val trigger: string -> string list -> unit
  end
end
