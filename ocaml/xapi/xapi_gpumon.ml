(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

module D = Debug.Make(struct let name="xapi" end)
open D

let systemctl = "/usr/bin/systemctl"
let gpumon = "xcp-rrdd-gpumon"

module Gpumon = Daemon_manager.Make(struct
    let check = Daemon_manager.Function (fun () ->
        try
          ignore
            (Forkhelpers.execute_command_get_output systemctl
               ["is-active"; "-q"; gpumon]);
          true
        with _ -> false)

    let start () =
      debug "Starting %s" gpumon;
      ignore (Forkhelpers.execute_command_get_output systemctl ["start"; gpumon])

    let stop () =
      debug "Stopping %s" gpumon;
      ignore (Forkhelpers.execute_command_get_output systemctl ["stop"; gpumon])
  end)

let with_gpumon_stopped = Gpumon.with_daemon_stopped

module Nvidia = struct
  let key = "nvidia"

  (* N.B. the pgpu must be in the local host where this function runs *)
  let get_pgpu_compatibility_metadata ~dbg ~pgpu_pci_address =
    let metadata =
      pgpu_pci_address
      |> Gpumon_client.Client.Nvidia.get_pgpu_metadata dbg 
      |> Stdext.Base64.encode
    in [key, metadata]

  (* N.B. the vgpu (and the vm) must be in the local host where this function runs *)
  let assert_pgpu_is_compatible_with_vm ~__context ~vm ~vgpu ~dest_host ~encoded_pgpu_metadata =
    let dbg = Context.string_of_task __context in
    let vm_domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
    let pgpu_metadata = Stdext.Base64.decode encoded_pgpu_metadata in
    let vgpu_impl = 
      vgpu
      |> (fun self -> Db.VGPU.get_type ~__context ~self)
      |> (fun self -> Db.VGPU_type.get_implementation ~__context ~self)
    in
    match vgpu_impl with
    | `passthrough | `gvt_g | `mxgpu -> 
      debug "Skipping, vGPU %s implementation for VM %s is not Nvidia" (Ref.string_of vgpu) (Ref.string_of vm)
    | `nvidia ->
      let local_pgpu_address = 
        Db.VGPU.get_resident_on ~__context ~self:vgpu
        |> (fun self -> Db.PGPU.get_PCI ~__context ~self)
        |> (fun self -> Db.PCI.get_pci_id ~__context ~self)
      in
      let compatibility = 
        try
          Gpumon_client.Client.Nvidia.get_pgpu_vm_compatibility dbg 
            local_pgpu_address vm_domid pgpu_metadata
        with
        | Gpumon_interface.NvmlInterfaceNotAvailable ->
          let host = Db.VM.get_resident_on ~__context ~self:vm in
          raise Api_errors.(Server_error (nvidia_tools_error, [Ref.string_of host]))
        | err -> raise Api_errors.(Server_error (internal_error, [Printexc.to_string err]))
      in
      let open Gpumon_interface in
      match compatibility with
      | Compatible -> 
        info "VM %s Nvidia vGPU is compatible with the destination pGPU on host %s"
          (Ref.string_of vm) (Ref.string_of dest_host)
      | Incompatible reasons ->
        let stringify = function
          | Host_driver  -> "host-driver"
          | Guest_driver -> "guest-driver"
          | GPU          -> "gpu"
          | Other        -> "other"
        in
        raise Api_errors.(Server_error (
            vgpu_destination_incompatible,
            [ String.concat ", " (List.map stringify reasons) (* There could be multiple reasons *)
            ; Ref.string_of vgpu
            ; Ref.string_of dest_host
            ]))

end (* Nvidia *)
