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

let service_name = "gpumon"
let queue_name = Xcp_service.common_prefix ^ service_name
let xml_path = "/var/xapi/" ^ service_name

type debug_info = string
type domid = int

type incompatibility_reason = Host_driver | Guest_driver | GPU | Other
type compatibility = Compatible | Incompatible of incompatibility_reason list

type pgpu_address = string
type nvidia_pgpu_metadata = string
type nvidia_vgpu_metadata = string

(** Exception raised when gpumon is unable to load the nvml nvidia library *)
exception NvmlInterfaceNotAvailable
(** Exception raised by the c bindings to the nvml nvidia library*)
exception NvmlFailure of string


module Nvidia = struct
  (** Compatibility checking interface for Nvidia vGPUs *)

  (** Get the metadata for a pGPU, given its address (PCI bus ID). *)
  external get_pgpu_metadata: debug_info -> pgpu_address -> nvidia_pgpu_metadata = ""

  (** Check compatibility between a VM's vGPU(s) and another pGPU.
    * pgpu_address = PCI bus ID of the pGPU in which the VM is currently running
    *                in the form `domain:bus:device.function` PCI identifier.
    * domid = domain ID of the VM in which the vGPU(s) is running.
    * pgpu_metadata = metadata of the pGPU to check compatibility for. *)
  external get_pgpu_vm_compatibility: debug_info -> pgpu_address -> domid -> nvidia_pgpu_metadata -> compatibility = ""

  external get_pgpu_vgpu_compatibility
    : debug_info
    -> nvidia_pgpu_metadata
    -> nvidia_vgpu_metadata list
    -> compatibility
    = ""
end
