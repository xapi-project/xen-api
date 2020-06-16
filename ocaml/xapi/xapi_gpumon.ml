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

module D = Debug.Make (struct let name = "xapi_gpumon" end)

open D

let gpumon = "xcp-rrdd-gpumon"

module Gpumon = Daemon_manager.Make (struct
  let check =
    Daemon_manager.Function
      (fun () ->
        try
          ignore
            (Forkhelpers.execute_command_get_output !Xapi_globs.systemctl
               ["is-active"; "-q"; gpumon]) ;
          true
        with _ -> false)

  let start () =
    debug "Starting %s" gpumon ;
    ignore
      (Forkhelpers.execute_command_get_output !Xapi_globs.systemctl
         ["start"; gpumon])

  let stop () =
    debug "Stopping %s" gpumon ;
    ignore
      (Forkhelpers.execute_command_get_output !Xapi_globs.systemctl
         ["stop"; gpumon])
end)

let with_gpumon_stopped = Gpumon.with_daemon_stopped

module Nvidia = struct
  let key = "nvidia"

  (* N.B. the pgpu must be in the local host where this function runs *)
  let get_pgpu_compatibility_metadata ~dbg ~pgpu_pci_address =
    let metadata =
      pgpu_pci_address
      |> Gpumon_client.Client.Nvidia.get_pgpu_metadata dbg
      |> Base64.encode_string
    in
    [(key, metadata)]

  let vgpu_impl ~__context vgpu =
    (vgpu |> fun self -> Db.VGPU.get_type ~__context ~self) |> fun self ->
    Db.VGPU_type.get_implementation ~__context ~self

  (** [is_nvidia] is true, if [vgpu] is an NVIDIA vGPU *)
  let is_nvidia ~__context ~vgpu =
    match vgpu_impl ~__context vgpu with
    | `nvidia ->
        true
    | `nvidia_sriov ->
        true
    | _ ->
        false

  (** [reason_to_string] turns an incompatibility reason into a string
   * for reporting it in an error message *)
  let reason_to_string = function
    | Gpumon_interface.Host_driver ->
        "host-driver"
    | Gpumon_interface.Guest_driver ->
        "guest-driver"
    | Gpumon_interface.GPU ->
        "gpu"
    | Gpumon_interface.Other ->
        "other"

  let get_vgpu_compatibility_metadata ~__context ~vm ~vgpu =
    let this = "get_vgpu_compatibility_metadata" in
    try
      let dbg = Context.string_of_task __context in
      let domid = Db.VM.get_domid ~__context ~self:vm |> Int64.to_int in
      let vgpu_uuid = Db.VGPU.get_uuid ~__context ~self:vgpu in
      let pgpu_address =
        ( Db.VGPU.get_resident_on ~__context ~self:vgpu |> fun self ->
          Db.PGPU.get_PCI ~__context ~self )
        |> fun self -> Db.PCI.get_pci_id ~__context ~self
      in
      let get uuid =
        match
          Gpumon_client.Client.Nvidia.get_vgpu_metadata dbg domid pgpu_address
            uuid
        with
        | [] ->
            []
        | [meta] ->
            [(key, Base64.encode_string meta)]
        | _ :: _ ->
            failwith
            @@ Printf.sprintf
                 "%s: More than one NVIDIA vGPUs are found in VM %s (dom %d) \
                  with UUID %s (%s)"
                 this (Ref.string_of vm) domid vgpu_uuid __LOC__
      in
      match get vgpu_uuid with
      | [] ->
          get "" (* upgrade case: retry without VGPU UUID filtering *)
      | x ->
          x
    with
    | Gpumon_interface.(Gpumon_error NvmlInterfaceNotAvailable) ->
        let host = Helpers.get_localhost ~__context |> Ref.string_of in
        raise Api_errors.(Server_error (nvidia_tools_error, [host]))
    | err ->
        let msg = Printexc.to_string err in
        raise Api_errors.(Server_error (internal_error, [msg]))

  (* N.B. the vgpu (and the vm) must be in the local host where this function runs *)
  let assert_pgpu_is_compatible_with_vm ~__context ~vm ~vgpu ~dest_host
      ~encoded_pgpu_metadata =
    let dbg = Context.string_of_task __context in
    let vm_domid = Int64.to_int (Db.VM.get_domid ~__context ~self:vm) in
    let pgpu_metadata = Base64.decode_exn encoded_pgpu_metadata in
    match vgpu_impl ~__context vgpu with
    | `passthrough | `gvt_g | `mxgpu ->
        debug "Skipping, vGPU %s implementation for VM %s is not Nvidia"
          (Ref.string_of vgpu) (Ref.string_of vm)
    | `nvidia | `nvidia_sriov -> (
        let local_pgpu_address =
          ( Db.VGPU.get_resident_on ~__context ~self:vgpu |> fun self ->
            Db.PGPU.get_PCI ~__context ~self )
          |> fun self -> Db.PCI.get_pci_id ~__context ~self
        in
        let compatibility =
          try
            Gpumon_client.Client.Nvidia.get_pgpu_vm_compatibility dbg
              local_pgpu_address vm_domid pgpu_metadata
          with
          | Gpumon_interface.(Gpumon_error NvmlInterfaceNotAvailable) ->
              let host = Db.VM.get_resident_on ~__context ~self:vm in
              raise
                Api_errors.(
                  Server_error (nvidia_tools_error, [Ref.string_of host]))
          | err ->
              raise
                Api_errors.(
                  Server_error (internal_error, [Printexc.to_string err]))
        in
        match compatibility with
        | Gpumon_interface.Compatible ->
            info
              "VM %s Nvidia vGPU is compatible with the destination pGPU on \
               host %s"
              (Ref.string_of vm) (Ref.string_of dest_host)
        | Gpumon_interface.(Incompatible reasons)
          when List.mem Gpumon_interface.Guest_driver reasons ->
            raise
              Api_errors.(
                Server_error
                  ( vgpu_guest_driver_limit
                  , [
                      String.concat ", " (List.map reason_to_string reasons)
                      (* There could be multiple reasons *)
                    ; Ref.string_of vm
                    ; Ref.string_of dest_host
                    ] ))
        | Gpumon_interface.(Incompatible reasons) ->
            raise
              Api_errors.(
                Server_error
                  ( vgpu_destination_incompatible
                  , [
                      String.concat ", " (List.map reason_to_string reasons)
                      (* There could be multiple reasons *)
                    ; Ref.string_of vgpu
                    ; Ref.string_of dest_host
                    ] ))
      )

  (** Predicate [vgpu_pgpu_are_compatible] checks that vGPU and pGPU are
   * compatible according to their abstract compatibility metadata. This
   * code can run on any host. If no vGPU or pGPU metadata is
   * available, compatibility is assumed.
  *)
  let vgpu_pgpu_are_compatible ~__context ~vgpu ~pgpu =
    let dbg = Context.string_of_task __context in
    let localhost = Helpers.get_localhost ~__context in
    let localhost' = Ref.string_of localhost in
    let this = "assert_vgpu_pgpu_are_compatible" in
    let check vgpu =
      let pgpu_metadata () =
        Db.PGPU.get_compatibility_metadata ~__context ~self:pgpu
        |> List.assoc key
        |> Base64.decode_exn
      in
      let vgpu_metadata () =
        Db.VGPU.get_compatibility_metadata ~__context ~self:vgpu
        |> (fun md -> [List.assoc key md])
        |> List.map Base64.decode_exn
      in
      match
        Gpumon_client.Client.Nvidia.get_pgpu_vgpu_compatibility dbg
          (pgpu_metadata ()) (vgpu_metadata ())
      with
      (* compatible *)
      | Gpumon_interface.Compatible ->
          info "%s: vGPU/pGPU are compatible %s/%s" this (Ref.string_of vgpu)
            (Ref.string_of pgpu) ;
          true
      | exception Not_found ->
          debug "%s: vGPU/pGPU are compatible by default %s/%s" this
            (Ref.string_of vgpu) (Ref.string_of pgpu) ;
          true
      (* incompatible *)
      | Gpumon_interface.Incompatible reasons ->
          debug "%s: vGPU/pGPU are incompatible %s/%s: %s" this
            (Ref.string_of vgpu) (Ref.string_of pgpu)
            (reasons |> List.map reason_to_string |> String.concat "; ") ;
          false
      (* errors *)
      | exception Gpumon_interface.(Gpumon_error NvmlInterfaceNotAvailable) ->
          raise Api_errors.(Server_error (nvidia_tools_error, [localhost']))
      | exception err ->
          raise
            Api_errors.(Server_error (internal_error, [Printexc.to_string err]))
    in
    if is_nvidia ~__context ~vgpu then
      check vgpu
    else (
      debug "%s: called non-Nvidia vGPU %s (true by default)" this
        (Ref.string_of vgpu) ;
      true
    )
end

(* Nvidia *)

(** [update_vgpu_metadata] updates the compatibility metadata of all vGPUs
 * of [vm]. Currently only NVIDIA vGPUs provide real meta data *)
let update_vgpu_metadata ~__context ~vm =
  Db.VM.get_VGPUs ~__context ~self:vm
  |> List.filter (fun vgpu -> Db.is_valid_ref __context vgpu)
  |> List.iter (fun vgpu ->
         let value =
           if Nvidia.is_nvidia ~__context ~vgpu then
             Nvidia.get_vgpu_compatibility_metadata ~__context ~vm ~vgpu
           else
             []
         in
         Db.VGPU.set_compatibility_metadata ~__context ~self:vgpu ~value ;
         debug "Writing vGPU compat metadata for vGPU %s: [%s]"
           (Ref.string_of vgpu)
           (List.map fst value |> String.concat ":"))

(** [clear_vgpu_metadata] removes compatibility metadata from all
 * vgpus of [vm]. *)
let clear_vgpu_metadata ~__context ~vm =
  Db.VM.get_VGPUs ~__context ~self:vm
  |> List.filter (fun vgpu -> Db.is_valid_ref __context vgpu)
  |> List.iter (fun vgpu ->
         Db.VGPU.get_compatibility_metadata ~__context ~self:vgpu
         |> List.filter (fun (k, _) -> k <> Nvidia.key)
         |> fun value ->
         Db.VGPU.set_compatibility_metadata ~__context ~self:vgpu ~value ;
         debug "clearing vGPU compat metadata for vGPU %s" (Ref.string_of vgpu))
