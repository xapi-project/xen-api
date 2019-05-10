(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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
open Stdext
let create ~__context ~name_label ~name_description ~other_config =
  let group = Ref.make () in
  let uuid = Uuid.to_string (Uuid.make_uuid ()) in
  Db.GPU_group.create ~__context ~ref:group ~uuid ~name_label ~name_description
    ~gPU_types:[] ~other_config ~allocation_algorithm:`depth_first;
  group

let destroy ~__context ~self =
  let vgpus = Db.GPU_group.get_VGPUs ~__context ~self in
  let connected = List.filter (fun self ->
      Db.VGPU.get_currently_attached ~__context ~self
    ) vgpus in
  if connected <> [] then
    raise (Api_errors.Server_error (Api_errors.gpu_group_contains_vgpu, List.map Ref.string_of connected));

  let pgpus = Db.GPU_group.get_PGPUs ~__context ~self in
  if pgpus <> [] then
    raise (Api_errors.Server_error (Api_errors.gpu_group_contains_pgpu, List.map Ref.string_of pgpus));

  (* Destroy all vGPUs *)
  List.iter (fun vgpu ->
      Helpers.log_exn_continue (Printf.sprintf "destroying VGPU: %s" (Ref.string_of vgpu))
        (fun vgpu -> Db.VGPU.destroy ~__context ~self:vgpu) vgpu) vgpus;

  Db.GPU_group.destroy ~__context ~self

let find_or_create ~__context pgpu =
  let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
  let pci_rec = Db.PCI.get_record_internal ~__context ~self:pci in
  let gpu_type = Xapi_pci.string_of_pci ~__context ~self:pci in
  try
    List.find (fun rf->
        let rc = Db.GPU_group.get_record_internal ~__context ~self:rf in
        rc.Db_actions.gPU_group_GPU_types = [gpu_type]
      )
      (Db.GPU_group.get_all ~__context)
  with Not_found ->
    let name_label = "Group of " ^ pci_rec.Db_actions.pCI_vendor_name ^ " " ^ pci_rec.Db_actions.pCI_device_name ^ " GPUs" in
    let group = create ~__context ~name_label ~name_description:"" ~other_config:[] in
    group

module VGPU_type_set = Set.Make(struct type t = API.ref_VGPU_type let compare = compare end)
let union_type_lists ~type_lists =
  (* Fold each item of each list into a set,
     	 * then return the elements of the set as a list. *)
  let union_set = List.fold_left
      (fun acc type_list ->
         List.fold_left
           (fun acc vgpu_type -> VGPU_type_set.add vgpu_type acc)
           acc type_list)
      VGPU_type_set.empty type_lists
  in
  VGPU_type_set.elements union_set

let update_enabled_VGPU_types ~__context ~self =
  let pgpus = Db.GPU_group.get_PGPUs ~__context ~self in
  let enabled_VGPU_types = union_type_lists
      (List.map
         (fun pgpu -> Db.PGPU.get_enabled_VGPU_types ~__context ~self:pgpu)
         pgpus)
  in
  Db.GPU_group.set_enabled_VGPU_types ~__context ~self ~value:enabled_VGPU_types

let update_supported_VGPU_types ~__context ~self =
  let pgpus = Db.GPU_group.get_PGPUs ~__context ~self in
  let supported_VGPU_types = union_type_lists
      (List.map
         (fun pgpu -> Db.PGPU.get_supported_VGPU_types ~__context ~self:pgpu)
         pgpus)
  in
  Db.GPU_group.set_supported_VGPU_types ~__context ~self ~value:supported_VGPU_types

let get_remaining_capacity_internal ~__context ~self ~vgpu_type =
  (* If there is capacity in the group for this VGPU type, we return the
     	 * capacity. If there is not then we will have an exception for each PGPU,
     	 * explaining why it cannot run a VGPU of this type. We need to look through
     	 * this list of exceptions and pick one to raise as the "overall" error. *)
  let choose_exception = function
    | [] ->
      (* Should only ever get here if there are no PGPUs in the GPU group. *)
      Api_errors.Server_error
        (Api_errors.gpu_group_contains_no_pgpus, [Ref.string_of self])
    | exceptions ->
      let error_code_scores = [
        Api_errors.pgpu_insufficient_capacity_for_vgpu, 10;
        Api_errors.vgpu_type_not_compatible_with_running_type, 8;
        Api_errors.vgpu_type_not_enabled, 6;
        Api_errors.vgpu_type_not_supported, 4;
      ] in
      let score_exception = function
        | Api_errors.Server_error (code, _) ->
          if List.mem_assoc code error_code_scores
          then List.assoc code error_code_scores
          else 0
        | _ -> 0
      in
      List.hd
        (Helpers.sort_by_schwarzian ~descending:true score_exception exceptions)
  in
  (* For each PGPU in the group, if it is unable to run the specified VGPU type,
     	 * save the exception returned. Otherwise just add its capacity
     	 * to the total. *)
  let pgpus = Db.GPU_group.get_PGPUs ~__context ~self in
  let capacity, exceptions = List.fold_left
      (fun (capacity, exceptions) pgpu ->
         match
           Xapi_pgpu_helpers.get_remaining_capacity_internal ~pre_allocate_list:[]
             ~__context ~self:pgpu ~vgpu_type
         with
         | Either.Left e -> (capacity, e :: exceptions)
         | Either.Right n -> (Int64.add n capacity, exceptions))
      (0L, []) pgpus
  in
  if capacity > 0L
  then Either.Right capacity
  else Either.Left (choose_exception exceptions)

let get_remaining_capacity ~__context ~self ~vgpu_type =
  match get_remaining_capacity_internal ~__context ~self ~vgpu_type with
  | Either.Left e -> 0L
  | Either.Right capacity -> capacity
