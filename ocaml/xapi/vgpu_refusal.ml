(*
 * Copyright (C) Cloud Software Group
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

(* Neither build profile makes warning 8 an error, so without this a new
   constructor with no mapping in to_api_error would still compile. *)
[@@@warning "@8"]

type t =
  | Type_not_supported of {vgpu_type: string; supported: string list}
  | Type_not_enabled of {vgpu_type: string; enabled: string list}
  | Type_incompatible_with_running of {
        pgpu: string
      ; vgpu_type: string
      ; running: string
    }
  | Insufficient_capacity of {pgpu: string; vgpu_type: string}
  | Group_contains_no_pgpus of {gpu_group: string}
  | Unexpected of exn

(* Clients read error parameters by position, so do not add a parameter to
   any of these codes: a reason that needs more detail gets a new code. *)
let to_api_error = function
  | Type_not_supported {vgpu_type; supported} ->
      Api_errors.Server_error
        (Api_errors.vgpu_type_not_supported, vgpu_type :: supported)
  | Type_not_enabled {vgpu_type; enabled} ->
      Api_errors.Server_error
        (Api_errors.vgpu_type_not_enabled, vgpu_type :: enabled)
  | Type_incompatible_with_running {pgpu; vgpu_type; running} ->
      Api_errors.Server_error
        ( Api_errors.vgpu_type_not_compatible_with_running_type
        , [pgpu; vgpu_type; running]
        )
  | Insufficient_capacity {pgpu; vgpu_type} ->
      Api_errors.Server_error
        (Api_errors.pgpu_insufficient_capacity_for_vgpu, [pgpu; vgpu_type])
  | Group_contains_no_pgpus {gpu_group} ->
      Api_errors.Server_error
        (Api_errors.gpu_group_contains_no_pgpus, [gpu_group])
  | Unexpected e ->
      e

let to_string = function
  | Type_not_supported {vgpu_type; supported} ->
      Printf.sprintf "type %s is not supported by this PGPU (supported: %s)"
        vgpu_type
        (String.concat ";" supported)
  | Type_not_enabled {vgpu_type; enabled} ->
      Printf.sprintf "type %s is not enabled on this PGPU (enabled: %s)"
        vgpu_type
        (String.concat ";" enabled)
  | Type_incompatible_with_running {pgpu; vgpu_type; running} ->
      Printf.sprintf
        "type %s is not compatible with the types already allocated on PGPU %s \
         (allocated: %s)"
        vgpu_type pgpu running
  | Insufficient_capacity {pgpu; vgpu_type} ->
      Printf.sprintf "PGPU %s has insufficient capacity for a VGPU of type %s"
        pgpu vgpu_type
  | Group_contains_no_pgpus {gpu_group} ->
      Printf.sprintf "GPU group %s contains no PGPUs" gpu_group
  | Unexpected e ->
      Printf.sprintf "unexpected failure while deciding: %s"
        (Printexc.to_string e)
