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
module D=Debug.Make(struct let name="xapi" end)
open D

open Xapi_stdext_monadic
open Xapi_stdext_std

let assert_VGPU_type_supported ~__context ~self ~vgpu_type =
  let supported_VGPU_types =
    Db.PGPU.get_supported_VGPU_types ~__context ~self
  in
  if not (List.mem vgpu_type supported_VGPU_types)
  then raise (Api_errors.Server_error
                (Api_errors.vgpu_type_not_supported,
                 List.map Ref.string_of (vgpu_type :: supported_VGPU_types)))

let assert_VGPU_type_enabled ~__context ~self ~vgpu_type =
  assert_VGPU_type_supported ~__context ~self ~vgpu_type;
  let enabled_VGPU_types =
    Db.PGPU.get_enabled_VGPU_types ~__context ~self
  in
  if not (List.mem vgpu_type enabled_VGPU_types)
  then raise (Api_errors.Server_error
                (Api_errors.vgpu_type_not_enabled,
                 List.map Ref.string_of (vgpu_type :: enabled_VGPU_types)))

let get_scheduled_VGPUs ~__context ~self =
  let open Db_filter_types in
  Db.VGPU.get_refs_where ~__context ~expr:(Eq
                                             (Field "scheduled_to_be_resident_on", Literal (Ref.string_of self)))

(* Get this list of VGPUs which are either resident on, or scheduled to be
 * resident on, this PGPU. *)
let get_allocated_VGPUs ~__context ~self =
  let resident_VGPUs = Db.PGPU.get_resident_VGPUs ~__context ~self in
  let scheduled_VGPUs = get_scheduled_VGPUs ~__context ~self in
  resident_VGPUs @ scheduled_VGPUs

let assert_VGPU_type_allowed ~__context ~self ~vgpu_type =
  assert_VGPU_type_enabled ~__context ~self ~vgpu_type;
  let allocated_vgpu_list = get_allocated_VGPUs ~__context ~self in
  (* Theh new vGPU must compatible with all the allocated vGPUs on the pGPU,
   * Namely, the new vGPU type must in the compatible_types_on_pgpu of all the allocated vGPUs *)
  let compatible_with_new_vGPU_type vgpu =
    vgpu
    |> (fun self -> Db.VGPU.get_type ~__context ~self)
    |> (fun self -> Db.VGPU_type.get_compatible_types_on_pgpu ~__context ~self)
    |> List.map Ref.of_string
    |> List.mem vgpu_type in

  let compatible_on_pgpu = List.for_all compatible_with_new_vGPU_type allocated_vgpu_list in

  if not compatible_on_pgpu then
    let sep = ";" in
    raise (Api_errors.Server_error (
        Api_errors.vgpu_type_not_compatible_with_running_type, [
          Ref.string_of self;
          Ref.string_of vgpu_type;
          List.map (fun self-> Db.VGPU.get_type ~__context ~self) allocated_vgpu_list
          |> List.sort_uniq Pervasives.compare
          |> List.map (fun vgpu_type_ref -> Ref.string_of vgpu_type_ref)
          |> String.concat sep
        ]))

let assert_no_resident_VGPUs_of_type ~__context ~self ~vgpu_type =
  let open Db_filter_types in
  match Db.VGPU.get_records_where ~__context
          ~expr:(And
                   (Eq (Field "resident_on", Literal (Ref.string_of self)),
                    Eq (Field "type", Literal (Ref.string_of vgpu_type))))
  with
  | [] -> ()
  | vgpus_and_records ->
    let vms =
      List.map
        (fun (vgpu, _) -> Db.VGPU.get_VM ~__context ~self:vgpu)
        vgpus_and_records
    in
    raise (Api_errors.Server_error
             (Api_errors.pgpu_in_use_by_vm, List.map Ref.string_of vms))

let get_remaining_capacity_internal ~__context ~self ~vgpu_type ~pre_allocate_list  =
  try
    assert_VGPU_type_allowed ~__context ~self ~vgpu_type;
    let convert_capacity capacity =
      if capacity > 0L
      then Either.Right capacity
      else Either.Left
          (Api_errors.Server_error
             (Api_errors.pgpu_insufficient_capacity_for_vgpu, [
                 Ref.string_of self;
                 Ref.string_of vgpu_type
               ]))
    in
    if Xapi_vgpu_type.requires_passthrough ~__context ~self:vgpu_type = Some `PF then
      (* For passthrough VGPUs, which means passing through an entire PGPU to a
       * VM, the remaining capacity is binary. We simply return 0 if the PCI
       * device is currently passed-through to a VM, or is scheduled to be,
       * and 1 otherwise. *)
      let pre_allocated = List.exists (fun (_,pgpu) -> pgpu = self ) pre_allocate_list in
      let pci = Db.PGPU.get_PCI ~__context ~self in
      let scheduled = List.length (get_scheduled_VGPUs ~__context ~self) > 0 in
      let attached = Db.PCI.get_attached_VMs ~__context ~self:pci <> [] in
      convert_capacity (if scheduled || attached || pre_allocated then 0L else 1L)
    else begin
      (* For virtual VGPUs, we calculate the number of times the VGPU_type's
       * size fits into the PGPU's (size - utilisation). *)
      let pgpu_size = Db.PGPU.get_size ~__context ~self in
      let pgpu_pre_allocated_vGPUs = List.filter (fun (_,pgpu) -> pgpu = self) pre_allocate_list
                                    |> List.map (fun (vgpu,_) -> vgpu) in
      let utilisation =
        List.fold_left
          (fun acc vgpu ->
             let _type = Db.VGPU.get_type ~__context ~self:vgpu in
             let vgpu_size =
               Db.VGPU_type.get_size ~__context ~self:_type
             in
             Int64.add acc vgpu_size)
          0L ((get_allocated_VGPUs ~__context ~self) @ pgpu_pre_allocated_vGPUs)
      in
      let new_vgpu_size =
        Db.VGPU_type.get_size ~__context ~self:vgpu_type
      in
      convert_capacity (Int64.div (Int64.sub pgpu_size utilisation) new_vgpu_size)
    end
  with e ->
    Either.Left e

let get_remaining_capacity ~__context ~self ~vgpu_type ~pre_allocate_list =
  match get_remaining_capacity_internal ~__context ~self ~vgpu_type ~pre_allocate_list with
  | Either.Left _ -> 0L
  | Either.Right capacity -> capacity

let assert_capacity_exists_for_VGPU_type ~__context ~self ~vgpu_type =
  match get_remaining_capacity_internal ~__context ~self ~vgpu_type ~pre_allocate_list:[] with
  | Either.Left e -> raise e
  | Either.Right capacity -> ()


(* extract vgpu implementation *)
let vgpu_impl ~__context vgpu = 
  vgpu
  |> (fun self -> Db.VGPU.get_type ~__context ~self)
  |> (fun self -> Db.VGPU_type.get_implementation ~__context ~self)


let assert_destination_pgpu_is_compatible_with_vm ~__context ~vm ~vgpu ~pgpu ~host ?remote () =
  let module XenAPI = Client.Client in
  let get_compatibility_metadata pgpu =
    match remote with
    | None -> Db.PGPU.get_compatibility_metadata ~__context ~self:pgpu
    | Some (rpc, session_id) ->XenAPI.PGPU.get_compatibility_metadata rpc session_id pgpu
  in
  let test_nvidia_compatibility vgpu pgpu = 
    let pgpu_metadata =
      try
        get_compatibility_metadata pgpu
        |> List.assoc Xapi_gpumon.Nvidia.key
      with Not_found -> 
        debug "Key %s is missing from the compatibility_metadata for pgpu %s on the host %s." Xapi_gpumon.Nvidia.key (Ref.string_of pgpu) (Ref.string_of host);
        raise Api_errors.(Server_error (nvidia_tools_error, [Ref.string_of host]))
    in
    Xapi_gpumon.Nvidia.assert_pgpu_is_compatible_with_vm ~__context ~vm ~vgpu ~dest_host:host ~encoded_pgpu_metadata:pgpu_metadata
  in
  match vgpu_impl ~__context vgpu with
  | `passthrough | `gvt_g | `mxgpu -> ()
  | `nvidia ->
    test_nvidia_compatibility vgpu pgpu


let assert_destination_has_pgpu_compatible_with_vm ~__context ~vm ~vgpu_map ~host ?remote () =
  let module XenAPI = Client.Client in
  let get_pgpus_of_host host =
    match remote with
    | None -> Db.Host.get_PGPUs ~__context ~self:host
    | Some (rpc, session_id) -> XenAPI.Host.get_PGPUs rpc session_id host
  in
  let get_gpu_group_of_pgpu pgpu =
    match remote with
    | None -> Db.PGPU.get_GPU_group ~__context ~self:pgpu
    | Some (rpc, session_id) -> XenAPI.PGPU.get_GPU_group rpc session_id pgpu
  in
  let get_gpu_types_of_gpu_group gpu_group =
    match remote with
    | None -> Db.GPU_group.get_GPU_types ~__context ~self:gpu_group
    | Some (rpc, session_id) -> XenAPI.GPU_group.get_GPU_types rpc session_id gpu_group
  in
  let get_pgpus_of_gpu_group gpu_group =
    match remote with
    | None -> Db.GPU_group.get_PGPUs ~__context ~self:gpu_group
    | Some (rpc, session_id) -> XenAPI.GPU_group.get_PGPUs rpc session_id gpu_group
  in
  (* get first pgpu with the correct implementation, the vgpu is needed for the error metadata *)
  let rec get_first_suitable_pgpu pgpu_types vgpu = function
    | [] ->
      debug "No compatible pGPU found on host %s, needed by VM %s" (Ref.string_of host) (Ref.string_of vm);
      let pgpu_group = Db.VGPU.get_GPU_group ~__context ~self:vgpu in
      raise Api_errors.(Server_error (vm_requires_gpu, [Ref.string_of vm; Ref.string_of pgpu_group]))
    | pgpu :: rest ->
      let types =
        get_gpu_group_of_pgpu pgpu
        |> get_gpu_types_of_gpu_group
      in
      if Listext.List.set_equiv pgpu_types types
      then pgpu
      else get_first_suitable_pgpu pgpu_types vgpu rest
  in
  let test_compatibility vgpu pgpus =
    match vgpu_impl ~__context vgpu with
    | `passthrough | `gvt_g | `mxgpu -> ()
    | `nvidia ->
      Db.VGPU.get_GPU_group ~__context ~self:vgpu
      |> fun self -> Db.GPU_group.get_GPU_types ~__context ~self
                     |> fun pgpu_types -> get_first_suitable_pgpu pgpu_types vgpu pgpus
                                          |> fun pgpu ->
                                          assert_destination_pgpu_is_compatible_with_vm ~__context ~vm ~vgpu ~pgpu ~host ?remote ()
  in
  let vgpus = Db.VM.get_VGPUs ~__context ~self:vm in
  let _mapped, unmapped = List.partition (fun vgpu -> List.mem_assoc vgpu vgpu_map) vgpus in

  (* Check that vgpus in the vgpu_map are correctly associable *)
  List.iter (fun (vgpu, gpu_group) ->
      match vgpu_impl ~__context vgpu with
      | `passthrough | `gvt_g | `mxgpu -> ()
      | `nvidia -> begin
          let pgpus = get_pgpus_of_gpu_group gpu_group in
          match pgpus with
          | [] ->
            debug "No compatible pGPU found using the provided vgpu_map, needed by VM %s" (Ref.string_of vm);
            raise Api_errors.(Server_error (vm_requires_gpu, [Ref.string_of vm; Ref.string_of gpu_group]))
          | pgpu :: _ ->
            (* We can run the compatibility test with any of the PGPUs *)
            assert_destination_pgpu_is_compatible_with_vm ~__context ~vm ~vgpu ~pgpu ~host ?remote ()
        end
    ) vgpu_map;

  (* Check that there is a potential pgpu candidate for each of the other vgpus *)
  let pgpus = get_pgpus_of_host host in
  List.iter (fun vgpu -> 
      test_compatibility vgpu pgpus
    ) unmapped
