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
open Listext
open Threadext

let calculate_max_capacities ~__context ~pCI ~size ~supported_VGPU_types =
  List.map
    (fun vgpu_type ->
       let max_capacity =
         if Xapi_vgpu_type.requires_passthrough ~__context ~self:vgpu_type = Some `PF
         then 1L
         else Int64.div size (Db.VGPU_type.get_size ~__context ~self:vgpu_type)
       in
       vgpu_type, max_capacity)
    supported_VGPU_types

let fetch_compatibility_metadata ~__context ~pgpu_pci =
  if Db.PCI.get_vendor_id ~__context ~self:pgpu_pci =
     Xapi_pci.id_of_int Xapi_vgpu_type.Nvidia.vendor_id
  then (
    let dbg = Context.string_of_task __context in
    let pgpu_pci_address = Db.PCI.get_pci_id ~__context ~self:pgpu_pci in
    Xapi_gpumon.Nvidia.get_pgpu_compatibility_metadata ~dbg ~pgpu_pci_address
  ) else []

let maybe_fetch_compatibility_metadata ~__context ~pgpu_pci =
  try fetch_compatibility_metadata ~__context ~pgpu_pci
  with
  | Gpumon_interface.(Gpumon_error NvmlInterfaceNotAvailable) -> []
  | err ->
    debug "fetch_compatibility_metadata for pgpu_pci:%s failed with %s"
      (Ref.string_of pgpu_pci) (Printexc.to_string err);
    []

let populate_compatibility_metadata ~__context ~pgpu ~pgpu_pci =
  let this = "populate_compatibility_metadata" in
  try
    let value = fetch_compatibility_metadata ~__context ~pgpu_pci in
    Db.PGPU.set_compatibility_metadata ~__context ~self:pgpu ~value
  with
  | Gpumon_interface.(Gpumon_error NvmlInterfaceNotAvailable) ->
    info "%s: can't get compat data for pgpu_pci:%s, keeping existing data"
      this (Ref.string_of pgpu_pci)
  | err ->
    debug "%s: obtaining compat data for pgpu_pci:%s failed with %s"
      this (Ref.string_of pgpu_pci) (Printexc.to_string err)

let create ~__context ~pCI ~gPU_group ~host ~other_config
    ~supported_VGPU_types ~size ~dom0_access
    ~is_system_display_device =
  let pgpu = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  let supported_VGPU_max_capacities =
    calculate_max_capacities ~__context ~pCI ~size ~supported_VGPU_types
  in
  Db.PGPU.create ~__context ~ref:pgpu ~uuid ~pCI
    ~gPU_group ~host ~other_config ~size
    ~supported_VGPU_max_capacities ~dom0_access
    ~is_system_display_device
    ~compatibility_metadata:(maybe_fetch_compatibility_metadata ~__context ~pgpu_pci:pCI);
  Db.PGPU.set_supported_VGPU_types ~__context
    ~self:pgpu ~value:supported_VGPU_types;
  Db.PGPU.set_enabled_VGPU_types ~__context
    ~self:pgpu ~value:supported_VGPU_types;
  debug "PGPU ref='%s' created (host = '%s')" (Ref.string_of pgpu) (Ref.string_of host);
  pgpu

let sync_pci_hidden ~__context ~pgpu ~pci =
  (* Determine whether dom0 can access the GPU. On boot, we determine
     	 * this from the boot config and put the result in the database.
     	 * Otherwise, we determine this from the database. *)
  if !Xapi_globs.on_system_boot
  then begin
    let is_pci_hidden = Pciops.is_pci_hidden ~__context pci in
    let dom0_access =
      if is_pci_hidden
      then `disabled
      else `enabled
    in
    Db.PGPU.set_dom0_access ~__context ~self:pgpu ~value:dom0_access;
    is_pci_hidden
  end else begin
    match Db.PGPU.get_dom0_access ~__context ~self:pgpu with
    | `disabled | `enable_on_reboot -> true
    | `enabled | `disable_on_reboot -> false
  end

(* If this PCI device is by AMD and if (ON THE HOST RUNNING THIS CODE) it has
 * a valid-looking phys_fn symlink in its entry under /sys/bus/pci/...
 * then:
 *   assume it is a Virtual Function (we should already have checked it is a display device)
 *   set the physical_function field in its DB object to point to its PF PCI object
 *   return true meaning it is a VF (even if we couldn't find a PF PCI object for it)
 * else return false *)
(* This is not in the .mli *)
let mxgpu_set_phys_fn_ref ~__context pci_ref pci_rec =
  Xapi_pci.int_of_id (pci_rec.Db_actions.pCI_vendor_id) = Xapi_vgpu_type.Vendor_amd.vendor_id &&
  pci_rec.Db_actions.pCI_virtual_functions = [] && (* i.e. we don't already know it is a phys fn *)
  (* A better name for pci_id would be pci_address. *)
  let pci_addr = pci_rec.Db_actions.pCI_pci_id in
  (* E.g. path = "/sys/bus/pci/devices/0000:88:00.0/" *)
  let path = Printf.sprintf "/sys/bus/pci/devices/%s/physfn" pci_addr in
  try (
    (* No problem if there's no such symlink: we'll handle the exception. *)
    let physfn_addr = Filename.basename(Unix.readlink path) in
    (* Expect physfn_addr to look like "0000:8c:01.0" from link-target "../0000:8c:01.0" *)
    (* If it does then look up Db ref with that pci_id (i.e. address), and create a link in DB *)
    if Pciops.is_bdf_format physfn_addr then (
      let host = Helpers.get_localhost ~__context in
      let expr = Db_filter_types.(And (Eq (Field "pci_id", Literal physfn_addr),
                                       Eq (Field "host", Literal (Ref.string_of host)))) in
      ( match Db.PCI.get_refs_where ~__context ~expr with (* Expect exactly one *)
        | [pf_ref] ->
          Db.PCI.set_physical_function ~__context ~self:pci_ref ~value:pf_ref;
          Db.PCI.set_dependencies ~__context ~self:pci_ref ~value:[]
        | [] -> error "Found no pci with address %s but physfn-link of %s points to it!" physfn_addr pci_addr
        | _ -> error "Found more than one pci with same address! %s" physfn_addr
      );
      true (* It was a Virtual Function PCI device so we don't want a pgpu for it. *)
    ) else false
  ) (* Unix_error from blind attempt to read symlink that might not exist *)
  with Unix.Unix_error _ -> false

(* This has the important side-effect of updating the physical_function field
 * of the PCI object in the database iff the PCI device turns out to be a VF
 * of an AMD MxGPU on the local host (in which case we return false).
 * Returns true iff pci_ref seems to represent a PHYSICAL gpu (not a VF) on the
 * LOCAL host. *)
let is_local_pgpu ~__context (pci_ref, pci_rec) =
  let localhost = Helpers.get_localhost ~__context in
  pci_rec.Db_actions.pCI_host = localhost
  && Xapi_pci.(is_class_of_kind Display_controller (int_of_id (pci_rec.Db_actions.pCI_class_id)))
  && not (mxgpu_set_phys_fn_ref ~__context pci_ref pci_rec) (* Ignore PCIs discovered to be Virtual Functions. *)

(* Makes DB match reality for pgpus on local host *)
let update_gpus ~__context =
  let host = Helpers.get_localhost ~__context in
  let system_display_device = Xapi_pci.get_system_display_device () in
  let existing_pgpus = List.filter (fun (rf, rc) -> rc.API.pGPU_host = host) (Db.PGPU.get_all_records ~__context) in
  let pcis =
    (* Important side-effects in is_local_pgpu *)
    List.filter (is_local_pgpu ~__context)
      (Xapi_pci.get_local_pcis_and_records ~__context)
    |> List.map (function pci_ref, _ -> pci_ref) in
  let is_host_display_enabled =
    match Db.Host.get_display ~__context ~self:host with
    | `enabled | `disable_on_reboot -> true
    | `disabled | `enable_on_reboot -> false
  in
  let rec find_or_create cur = function
    | [] -> cur
    | pci :: remaining_pcis ->
      let pci_addr =  Some (Db.PCI.get_pci_id ~__context ~self:pci) in
      let is_system_display_device = (system_display_device = pci_addr) in
      let pgpu =
        try
          let (rf, rc) = List.find (fun (_, rc) -> rc.API.pGPU_PCI = pci) existing_pgpus in
          let is_pci_hidden = sync_pci_hidden ~__context ~pgpu:rf ~pci in
          (* Now we've determined whether the PCI is hidden, we can work out the
             					 * list of supported VGPU types. *)
          let supported_VGPU_types =
            Xapi_vgpu_type.find_or_create_supported_types ~__context ~pci
              ~is_system_display_device
              ~is_host_display_enabled
              ~is_pci_hidden
          in
          let old_supported_VGPU_types =
            Db.PGPU.get_supported_VGPU_types ~__context ~self:rf in
          let old_enabled_VGPU_types =
            Db.PGPU.get_enabled_VGPU_types ~__context ~self:rf in
          (* Pick up any new supported vGPU configs on the host *)
          Db.PGPU.set_supported_VGPU_types ~__context ~self:rf ~value:supported_VGPU_types;
          (* Calculate the maximum capacities of the supported types. *)
          let max_capacities =
            calculate_max_capacities
              ~__context
              ~pCI:pci
              ~size:(Db.PGPU.get_size ~__context ~self:rf)
              ~supported_VGPU_types
          in
          Db.PGPU.set_supported_VGPU_max_capacities ~__context
            ~self:rf ~value:max_capacities;
          (* Enable any new supported types. *)
          let new_types_to_enable =
            List.filter
              (fun t -> not (List.mem t old_supported_VGPU_types))
              supported_VGPU_types
          in
          (* Disable any types which are no longer supported. *)
          let pruned_enabled_types =
            List.filter
              (fun t -> List.mem t supported_VGPU_types)
              old_enabled_VGPU_types
          in
          Db.PGPU.set_enabled_VGPU_types ~__context
            ~self:rf
            ~value:(pruned_enabled_types @ new_types_to_enable);
          Db.PGPU.set_is_system_display_device ~__context
            ~self:rf
            ~value:is_system_display_device;
          populate_compatibility_metadata ~__context ~pgpu:rf ~pgpu_pci:pci;
          (rf, rc)
        with Not_found ->
          (* If a new PCI has appeared then we know this is a system boot.
             					 * We determine whether dom0 can access the device by looking in the
             					 * boot config. *)
          let is_pci_hidden = Pciops.is_pci_hidden ~__context pci in
          let supported_VGPU_types =
            Xapi_vgpu_type.find_or_create_supported_types ~__context ~pci
              ~is_system_display_device
              ~is_host_display_enabled
              ~is_pci_hidden
          in
          let dom0_access =
            if is_pci_hidden
            then `disabled
            else `enabled
          in
          let self = create ~__context ~pCI:pci
              ~gPU_group:(Ref.null) ~host ~other_config:[]
              ~supported_VGPU_types
              ~size:Constants.pgpu_default_size ~dom0_access
              ~is_system_display_device
          in
          let group = Xapi_gpu_group.find_or_create ~__context self in
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              Client.Client.PGPU.set_GPU_group rpc session_id self group);
          self, Db.PGPU.get_record ~__context ~self
      in
      find_or_create (pgpu :: cur) remaining_pcis
  in
  let current_pgpus = find_or_create [] pcis in
  let obsolete_pgpus = List.set_difference existing_pgpus current_pgpus in
  List.iter (fun (self, _) -> Db.PGPU.destroy ~__context ~self) obsolete_pgpus;
  (* Update the supported/enabled VGPU types on any affected GPU groups. *)
  let groups_to_update = List.setify
      (List.map
         (fun (_, pgpu_rec) -> pgpu_rec.API.pGPU_GPU_group)
         (current_pgpus @ obsolete_pgpus))
  in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       List.iter
         (fun gpu_group ->
            let open Client in
            Client.GPU_group.update_enabled_VGPU_types
              ~rpc ~session_id ~self:gpu_group;
            Client.GPU_group.update_supported_VGPU_types
              ~rpc ~session_id ~self:gpu_group)
         groups_to_update)

let update_group_enabled_VGPU_types ~__context ~self =
  let group = Db.PGPU.get_GPU_group ~__context ~self in
  if Db.is_valid_ref __context group
  then Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:group

let pgpu_m = Mutex.create ()

let add_enabled_VGPU_types ~__context ~self ~value =
  Mutex.execute pgpu_m (fun () ->
      Xapi_pgpu_helpers.assert_VGPU_type_supported ~__context
        ~self ~vgpu_type:value;
      Db.PGPU.add_enabled_VGPU_types ~__context ~self ~value;
      update_group_enabled_VGPU_types ~__context ~self
    )

let remove_enabled_VGPU_types ~__context ~self ~value =
  Mutex.execute pgpu_m (fun () ->
      Xapi_pgpu_helpers.assert_no_resident_VGPUs_of_type ~__context
        ~self ~vgpu_type:value;
      Db.PGPU.remove_enabled_VGPU_types ~__context ~self ~value;
      update_group_enabled_VGPU_types ~__context ~self
    )

let set_enabled_VGPU_types ~__context ~self ~value =
  Mutex.execute pgpu_m (fun () ->
      let current_types = Db.PGPU.get_enabled_VGPU_types ~__context ~self in
      let to_enable = List.set_difference value current_types
      and to_disable = List.set_difference current_types value in
      List.iter (fun vgpu_type ->
          Xapi_pgpu_helpers.assert_VGPU_type_supported ~__context ~self ~vgpu_type)
        to_enable;
      List.iter (fun vgpu_type ->
          Xapi_pgpu_helpers.assert_no_resident_VGPUs_of_type ~__context ~self ~vgpu_type)
        to_disable;
      Db.PGPU.set_enabled_VGPU_types ~__context ~self ~value;
      update_group_enabled_VGPU_types ~__context ~self
    )

let set_GPU_group ~__context ~self ~value =
  debug "Move PGPU %s -> GPU group %s" (Db.PGPU.get_uuid ~__context ~self)
    (Db.GPU_group.get_uuid ~__context ~self:value);
  Mutex.execute pgpu_m (fun () ->
      (* Precondition: PGPU has no resident VGPUs *)
      let resident_vgpus = Db.PGPU.get_resident_VGPUs ~__context ~self in
      if resident_vgpus <> [] then begin
        let resident_vms = List.map
            (fun self -> Db.VGPU.get_VM ~__context ~self) resident_vgpus in
        raise (Api_errors.Server_error (Api_errors.pgpu_in_use_by_vm,
                                        List.map Ref.string_of resident_vms))
      end;

      let check_compatibility gpu_type group_types =
        match group_types with
        | [] -> true, [gpu_type]
        | _ -> List.mem gpu_type group_types, group_types in

      let pci = Db.PGPU.get_PCI ~__context ~self in
      let gpu_type = Xapi_pci.string_of_pci ~__context ~self:pci
      and group_types = Db.GPU_group.get_GPU_types ~__context ~self:value in
      match check_compatibility gpu_type group_types with
      | true, new_types ->
        let old_group = Db.PGPU.get_GPU_group ~__context ~self in
        Db.PGPU.set_GPU_group ~__context ~self ~value;
        (* Group inherits the device type *)
        Db.GPU_group.set_GPU_types ~__context ~self:value ~value:new_types;
        debug "PGPU %s moved to GPU group %s. Group GPU types = [ %s ]."
          (Db.PGPU.get_uuid ~__context ~self)
          (Db.GPU_group.get_uuid ~__context ~self:value)
          (String.concat "; " new_types);
        (* Update the old and new groups' cached lists of VGPU_types. *)
        if Db.is_valid_ref __context old_group
        then begin
          Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:old_group;
          Xapi_gpu_group.update_supported_VGPU_types ~__context ~self:old_group;
        end;
        Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:value;
        Xapi_gpu_group.update_supported_VGPU_types ~__context ~self:value
      | false, _ ->
        raise (Api_errors.Server_error
                 (Api_errors.pgpu_not_compatible_with_gpu_group,
                  [gpu_type; "[" ^ String.concat ", " group_types ^ "]"]))
    )

let get_remaining_capacity ~__context ~self ~vgpu_type =
  match Xapi_pgpu_helpers.get_remaining_capacity_internal ~__context ~self ~vgpu_type with
  | Either.Left _ -> 0L
  | Either.Right capacity -> capacity

let assert_can_run_VGPU ~__context ~self ~vgpu =
  let vgpu_type = Db.VGPU.get_type ~__context ~self:vgpu in
  Xapi_pgpu_helpers.assert_capacity_exists_for_VGPU_type ~__context ~self ~vgpu_type

let update_dom0_access ~__context ~self ~action =
  let db_current = Db.PGPU.get_dom0_access ~__context ~self in
  let db_new = match db_current, action with
    | `enabled,           `enable
    | `disable_on_reboot, `enable  -> `enabled
    | `disabled,          `enable
    | `enable_on_reboot,  `enable  -> `enable_on_reboot
    | `enabled,           `disable
    | `disable_on_reboot, `disable -> `disable_on_reboot
    | `disabled,          `disable
    | `enable_on_reboot,  `disable -> `disabled
  in

  let pci = Db.PGPU.get_PCI ~__context ~self in
  begin
    match db_new with
    | `enabled
    | `enable_on_reboot -> Pciops.unhide_pci ~__context pci
    | `disabled
    | `disable_on_reboot -> Pciops.hide_pci ~__context pci
  end;

  Db.PGPU.set_dom0_access ~__context ~self ~value:db_new;
  db_new

let enable_dom0_access ~__context ~self =
  update_dom0_access ~__context ~self ~action:`enable

let disable_dom0_access ~__context ~self =
  if not (Pool_features.is_enabled ~__context Features.Integrated_GPU)
  then raise Api_errors.(Server_error (feature_restricted, []));
  update_dom0_access ~__context ~self ~action:`disable

(* This must be run LOCALLY on the host that is about to start a VM that is
 * going to use a vgpu backed by an AMD MxGPU pgpu. *)
let mxgpu_vf_setup ~__context =
  (* From the modprobe(8) manpage:
   * --first-time
   *     Normally, modprobe will succeed (and do nothing) if told to insert a
   *     module which is already present or to remove a module which isn't
   *     present. This is ideal for simple scripts; however, more complicated
   *     scripts often want to know whether modprobe really did something: this
   *     option makes modprobe fail in the case that it actually didn't do
   *     anything. *)
  ignore (Forkhelpers.execute_command_get_output !Xapi_globs.modprobe_path ["gim"]);
  (* Update the gpus even if the module was present already, in case it was
   * already loaded before xapi was (re)started. *)
  Xapi_pci.update_pcis ~__context;
  (* Potential optimisation: make update_pcis return a value telling whether
   * it changed anything, and stop here if it did not. *)
  List.iter
    (* Important side-effects in is_local_pgpu *)
    (fun pci_ref -> ignore (is_local_pgpu ~__context pci_ref))
    (Xapi_pci.get_local_pcis_and_records ~__context)
