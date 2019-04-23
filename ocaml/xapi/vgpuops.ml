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
module D=Debug.Make(struct let name="vgpuops" end)
open D

open Stdext
open Listext
open Xstringext

type vgpu_t = {
  vgpu_ref: API.ref_VGPU;
  gpu_group_ref: API.ref_GPU_group;
  devid: int;
  other_config: (string * string) list;
  type_ref: API.ref_VGPU_type;
  requires_passthrough: [ `PF | `VF ] option;
}

let vgpu_of_ref ~__context vgpu =
  let vgpu_r = Db.VGPU.get_record ~__context ~self:vgpu in
  {
    vgpu_ref = vgpu;
    gpu_group_ref = vgpu_r.API.vGPU_GPU_group;
    devid = int_of_string vgpu_r.API.vGPU_device;
    other_config = vgpu_r.API.vGPU_other_config;
    type_ref = vgpu_r.API.vGPU_type;
    requires_passthrough = Xapi_vgpu.requires_passthrough ~__context ~self:vgpu;
  }

let vgpus_of_vm ~__context vm_r =
  List.map (vgpu_of_ref ~__context ) vm_r.API.vM_VGPUs

let fail_creation vm vgpu =
  match vgpu.requires_passthrough with
  | None | Some `VF ->
    raise (Api_errors.Server_error (Api_errors.vm_requires_vgpu, [
        Ref.string_of vm;
        Ref.string_of vgpu.gpu_group_ref;
        Ref.string_of vgpu.type_ref
      ]))
  | Some `PF ->
    raise (Api_errors.Server_error (Api_errors.vm_requires_gpu, [
        Ref.string_of vm;
        Ref.string_of vgpu.gpu_group_ref
      ]))

let allocate_vgpu_to_gpu ?(dry_run=false) ?(pre_allocate_list=[]) ~__context vm host vgpu =
  (* Get all pGPU from the host *)
  let available_pgpus = Db.Host.get_PGPUs ~__context ~self:host in
  (* Get all pGPU from the required groups *)
  let compatible_pgpus = Db.GPU_group.get_PGPUs ~__context ~self:vgpu.gpu_group_ref in
  let pgpus = List.intersect compatible_pgpus available_pgpus in

  let pgpu_can_hold_vgpu pgpu vgpu =
    try Xapi_pgpu.assert_can_run_VGPU ~__context ~self:pgpu ~vgpu;true
    with e -> false in
  (* Get all pGPU that can hold the vGPU *)
  let active_pgpus = List.filter ( fun pgpu -> pgpu_can_hold_vgpu pgpu vgpu.vgpu_ref ) pgpus in

  let remaining_capacity_for_vgpu_from_pgpu vgpu pgpu =
    let db_remaining =  Helpers.call_api_functions ~__context
        (fun rpc session_id ->
           Client.Client.PGPU.get_remaining_capacity ~rpc ~session_id
             ~self:pgpu ~vgpu_type:vgpu.type_ref) in

    let size_of_vgpu vgpu_ref =
        let vgpu_type = Db.VGPU.get_type ~__context ~self:vgpu_ref in
        Int64.to_float (Db.VGPU_type.get_size ~__context ~self:vgpu_type) in

    let current_vgpu_size = size_of_vgpu vgpu.vgpu_ref in

    let size_over_current_vGPU other =
      let other_size = size_of_vgpu other in
      other_size /. current_vgpu_size in

    (* Check if any pre_allocation existed, the pre_allocation is a set of vGPU allocation that
     * not reflected in the database, usually in the dry run mode, with following format
     * [(v1,p1);(v2,p2);(v3,p1)...]*)
    let virtual_allocation = List.fold_left (fun num ele ->
        match ele with
        |(cvgpu,cpgpu) when cpgpu = pgpu -> num +. (size_over_current_vGPU cvgpu)
        |(_,_) -> num )
        0. pre_allocate_list in
    let virtual_available_allocation = Int64.of_float (ceil virtual_allocation) in
    let result = Int64.sub db_remaining virtual_available_allocation in
    if result < 0L then raise Api_errors.(Server_error (internal_error, ["The remaining cappacity cannot be negative"]))
    else result in

  (* Sort the pgpus in lists of equal optimality for vGPU placement based on
   * the GPU groups allocation algorithm *)
  let sort_desc =
    match Db.GPU_group.get_allocation_algorithm ~__context ~self:vgpu.gpu_group_ref with
    | `depth_first -> false
    | `breadth_first -> true
  in
  let sorted_pgpus = Helpers.sort_by_schwarzian ~descending:sort_desc
      (fun pgpu -> remaining_capacity_for_vgpu_from_pgpu vgpu pgpu )
      active_pgpus
  in
  let rec choose_pgpu = function
    | [] -> None
    | pgpu :: remaining ->
      try
        Xapi_pgpu_helpers.assert_capacity_exists_for_VGPU_type ~__context
          ~self:pgpu ~vgpu_type:vgpu.type_ref;
        Some pgpu
      with _ -> choose_pgpu remaining
  in
  match choose_pgpu sorted_pgpus with
  | None -> fail_creation vm vgpu
  | Some pgpu ->
    if not dry_run then
      Db.VGPU.set_scheduled_to_be_resident_on ~__context ~self:vgpu.vgpu_ref ~value:pgpu;
    (vgpu.vgpu_ref,pgpu)::pre_allocate_list

(* Take a PCI device and assign it, and any dependent devices, to the VM *)
let add_pcis_to_vm ~__context host vm pci =
  (* Add a platform key to the VM if any of the PCIs are integrated GPUs;
   * otherwise remove the key. *)
  Db.VM.remove_from_platform ~__context
    ~self:vm ~key:Xapi_globs.igd_passthru_key;
  let (_, pci_bus, _, _) = Pciops.pcidev_of_pci ~__context pci in
  if (pci_bus = 0) && (Xapi_pci_helpers.igd_is_whitelisted ~__context pci) then
    Db.VM.add_to_platform ~__context ~self:vm ~key:Xapi_globs.igd_passthru_key ~value:"true";
  (* The GPU PCI device which xapi manages may have dependencies: *)
  let dependent_pcis = Db.PCI.get_dependencies ~__context ~self:pci in
  let devs : (int * int * int * int) list = List.sort compare (List.map (Pciops.pcidev_of_pci ~__context) (pci :: dependent_pcis)) in
  (* Add a hotplug ordering (see pcidevs_of_pci) *)
  let devs : ((int * (int * int * int * int))) list = List.rev (snd (List.fold_left (fun (i, acc) pci -> i + 1, (i, pci) :: acc) (0, []) devs)) in
  (* Update VM other_config for PCI passthrough *)
  let value = String.concat "," (List.map Pciops.to_string devs) in
  debug "Adding PCIs to a VM with 'other config': %s" value; 
  Db.VM.add_to_other_config ~__context ~self:vm ~key:Xapi_globs.vgpu_pci ~value

let reserve_free_virtual_function ~__context vm pf =
  let rec get retry =
    match Pciops.reserve_free_virtual_function ~__context vm pf with
    | Some vf -> vf
    | None ->
      if retry then begin
        (* We may still need to load the driver... do that and try again *)
        let pf_host = Db.PCI.get_host ~__context ~self:pf in
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Client.Host.mxgpu_vf_setup rpc session_id pf_host
          );
        get false
      end else
        (* This probably means that our capacity checking went wrong! *)
        raise Api_errors.(Server_error (internal_error, ["No free virtual function found"]))
  in
  get true

let add_vgpus_to_vm ~__context host vm vgpus =
  (* Only support a maximum of one virtual GPU per VM for now. *)
  (try Db.VM.remove_from_other_config ~__context ~self:vm ~key:Xapi_globs.vgpu_pci with _ -> ());
  debug "vGPUs allocated to VM (%s) are: %s" (Ref.string_of vm)
    (List.map (fun vgpu -> Ref.string_of vgpu.vgpu_ref) vgpus
     |> String.concat ";");
  List.iter (fun vgpu ->
      match vgpu.requires_passthrough with
      | Some `PF ->
        debug "Creating passthrough VGPUs";
        let pgpu = List.assoc vgpu.vgpu_ref (allocate_vgpu_to_gpu ~__context vm host vgpu) in
        let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
        add_pcis_to_vm ~__context host vm pci
      | Some `VF ->
        Pool_features.assert_enabled ~__context ~f:Features.VGPU;
        debug "Creating SR-IOV VGPUs";
        let pgpu = List.assoc vgpu.vgpu_ref (allocate_vgpu_to_gpu ~__context vm host vgpu) in
        Db.PGPU.get_PCI ~__context ~self:pgpu
        |> reserve_free_virtual_function ~__context vm
        |> add_pcis_to_vm ~__context host vm
      | None ->
        Pool_features.assert_enabled ~__context ~f:Features.VGPU;
        debug "Creating virtual VGPUs";
        ignore (allocate_vgpu_to_gpu ~__context vm host vgpu)
    ) vgpus


(* The two functions below are the main entry points of this module *)

(* Note that this function is called from Message_forwarding.allocate_vm_to_host,
 * only on the pool master, and with the global lock held. We therefore do not
 * need any further locking in any of the functions above, where resources are
 * reserved. *)
let create_vgpus ~__context host (vm, vm_r) hvm =
  let vgpus = vgpus_of_vm ~__context vm_r in
  if vgpus <> [] && not hvm then
    raise (Api_errors.Server_error (Api_errors.feature_requires_hvm, ["vGPU- and GPU-passthrough needs HVM"]));
  add_vgpus_to_vm ~__context host vm vgpus

(* This function is called from Xapi_xenops, after forwarding, so possibly on a slave. *)
let list_pcis_for_passthrough ~__context ~vm =
  try
    let value = List.assoc Xapi_globs.vgpu_pci (Db.VM.get_other_config ~__context ~self:vm) in
    List.map Pciops.of_string (String.split ',' value)
  with _ -> []
