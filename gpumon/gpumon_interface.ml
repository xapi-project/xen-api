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

open Rpc
open Idl

let service_name = "gpumon"
let queue_name = Xcp_service.common_prefix ^ service_name
let xml_path = "/var/xapi/" ^ service_name

(** Uninterpreted string associated with the operation *)
type debug_info = string
[@@deriving rpcty]

(* Domain ID of VM *)
type domid = int
[@@deriving rpcty]

(** Reason for incompatibility *)
type incompatibility_reason =
  | Host_driver
  | Guest_driver
  | GPU
  | Other
[@@deriving rpcty]

(** Boolean: compatible? *)
type compatibility =
  | Compatible
  | Incompatible of incompatibility_reason list
[@@deriving rpcty]

(** PCI identifier of physical GPU *)
type pgpu_address = string
[@@deriving rpcty]

(** Metadata of Nvidia physical GPU *)
type nvidia_pgpu_metadata = string
[@@deriving rpcty]

(** Metadata of Nvidia virtual GPU *)
type nvidia_vgpu_metadata = string
[@@deriving rpcty]

(** List of Nvidia virtual GPU metadata records *)
type nvidia_vgpu_metadata_list = nvidia_vgpu_metadata list
[@@deriving rpcty]


(** Error wrapper *)
type gpu_errors =
  | NvmlInterfaceNotAvailable
  (** Exception raised when gpumon is unable to load the nvml nvidia library *)
  | NvmlFailure of string
  (** Exception raised by the c bindings to the nvml nvidia library*)
  | Gpumon_failure
  (** Default exception raised upon daemon failure *)
[@@default Gpumon_failure]
[@@deriving rpcty]

exception Gpumon_error of gpu_errors

(** Error handler *)
module GpuErrors = Error.Make(struct
    type t = gpu_errors
    let t = gpu_errors
  end)
let gpu_err = GpuErrors.error

(** Functor to autogenerate API calls *)
module RPC_API(R : RPC) = struct
  open R

  let param = Param.mk

  let description =
    Interface.{ name = "Gpumon"
              ; namespace = None
              ; description =
                  [ "This interface is used by Xapi and Gpumon to monitor "
                  ; "physical and virtual GPUs."]
              ;
                version=(1,0,0)
              }

  let implementation = implement description

  (** common API call parameters *)

  let debug_info_p = param ~description:
      ["Uninterpreted string used for debugging."]
      debug_info

  let domid_p = param ~description:
      ["Domain ID of the VM in which the vGPU(s) is running."]
      domid

  let pgpu_address_p = param ~description:
      ["PCI bus ID of the pGPU in which the VM is currently running"
      ;"in the form `domain:bus:device.function` PCI identifier."]
      pgpu_address

  let nvidia_pgpu_metadata_p = param ~description:
      ["Metadata of Nvidia physical GPU."]
      nvidia_pgpu_metadata

  let nvidia_vgpu_metadata_p = param ~description:
      ["Metadata of Nvidia virtual GPU."]
      nvidia_vgpu_metadata

  let nvidia_vgpu_metadata_list_p = param ~description:
      ["Metadata list of Nvidia virtual GPU."]
      nvidia_vgpu_metadata_list

  let compatibility_p = param ~description:
      ["Value indicating whether two or more GPUs are compatible with each other."]
      compatibility

  (** Compatibility checking interface for Nvidia vGPUs *)
  module Nvidia = struct

    let get_pgpu_metadata =
      declare "get_pgpu_metadata"
        ["Gets the metadata for a pGPU, given its address (PCI bus ID)."]
        (debug_info_p @->  pgpu_address_p @-> returning nvidia_pgpu_metadata_p gpu_err )

    let get_pgpu_vm_compatibility =
      declare "get_pgpu_vm_compatibility"
        ["Checks compatibility between a VM's vGPU(s) and another pGPU."]
        (debug_info_p @->  pgpu_address_p @-> domid_p @-> nvidia_pgpu_metadata_p @-> returning compatibility_p gpu_err )

    let get_vgpu_metadata =
      declare "get_vgpu_metadata"
        ["Obtains metadata for all vGPUs running in a domain."]
        ( debug_info_p @->  domid_p @-> pgpu_address_p @-> returning nvidia_vgpu_metadata_list_p gpu_err )

    (** The use case is VM.suspend/VM.resume: before
     * VM.resume [nvidia_vgpu_metadata] of the suspended VM is checked
     * against the [nvidia_pgpu_metadata] on the host where the VM is
     * resumed.
     * *)
    let get_pgpu_vgpu_compatibility =
      declare "get_pgpu_vgpu_compatibility"
        ["Checks compatibility between a pGPU (on a host) and a list of vGPUs (assigned to a VM). Note: A VM may use several vGPUs."]
        ( debug_info_p @->  nvidia_pgpu_metadata_p @-> nvidia_vgpu_metadata_list_p @-> returning compatibility_p gpu_err )
  end
end
