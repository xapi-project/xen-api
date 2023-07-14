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
module D = Debug.Make (struct let name = "xapi_pgpu" end)

open D
module Listext = Xapi_stdext_std.Listext

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module Unixext = Xapi_stdext_unix.Unixext

let calculate_max_capacities ~__context ~pCI:_ ~size ~supported_VGPU_types =
  List.map
    (fun vgpu_type ->
      let max_capacity =
        if
          Xapi_vgpu_type.requires_passthrough ~__context ~self:vgpu_type
          = Some `PF
        then
          1L
        else
          Int64.div size (Db.VGPU_type.get_size ~__context ~self:vgpu_type)
      in
      (vgpu_type, max_capacity)
    )
    supported_VGPU_types

let fetch_compatibility_metadata ~__context ~pgpu_pci =
  if
    Db.PCI.get_vendor_id ~__context ~self:pgpu_pci
    = Xapi_pci.id_of_int Xapi_vgpu_type.Nvidia.vendor_id
  then
    let dbg = Context.string_of_task __context in
    let pgpu_pci_address = Db.PCI.get_pci_id ~__context ~self:pgpu_pci in
    Xapi_gpumon.Nvidia.get_pgpu_compatibility_metadata ~dbg ~pgpu_pci_address
  else
    []

let maybe_fetch_compatibility_metadata ~__context ~pgpu_pci =
  try fetch_compatibility_metadata ~__context ~pgpu_pci with
  | Gpumon_interface.(Gpumon_error NvmlInterfaceNotAvailable) ->
      []
  | err ->
      debug "fetch_compatibility_metadata for pgpu_pci:%s failed with %s"
        (Ref.string_of pgpu_pci) (Printexc.to_string err) ;
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
      debug "%s: obtaining compat data for pgpu_pci:%s failed with %s" this
        (Ref.string_of pgpu_pci) (Printexc.to_string err)

let create ~__context ~pCI ~gPU_group ~host ~other_config ~supported_VGPU_types
    ~size ~dom0_access ~is_system_display_device =
  let pgpu = Ref.make () in
  let uuid = Uuidx.(to_string (make ())) in
  let supported_VGPU_max_capacities =
    calculate_max_capacities ~__context ~pCI ~size ~supported_VGPU_types
  in
  Db.PGPU.create ~__context ~ref:pgpu ~uuid ~pCI ~gPU_group ~host ~other_config
    ~size ~supported_VGPU_max_capacities ~dom0_access ~is_system_display_device
    ~compatibility_metadata:
      (maybe_fetch_compatibility_metadata ~__context ~pgpu_pci:pCI) ;
  Db.PGPU.set_supported_VGPU_types ~__context ~self:pgpu
    ~value:supported_VGPU_types ;
  Db.PGPU.set_enabled_VGPU_types ~__context ~self:pgpu
    ~value:supported_VGPU_types ;
  debug "PGPU ref='%s' created (host = '%s')" (Ref.string_of pgpu)
    (Ref.string_of host) ;
  pgpu

let sync_pci_hidden ~__context ~pgpu ~pci =
  (* Determine whether dom0 can access the GPU. On boot, we determine
     	 * this from the boot config and put the result in the database.
     	 * Otherwise, we determine this from the database. *)
  if !Xapi_globs.on_system_boot then (
    let is_pci_hidden = Pciops.is_pci_hidden ~__context pci in
    let dom0_access =
      if is_pci_hidden then
        `disabled
      else
        `enabled
    in
    Db.PGPU.set_dom0_access ~__context ~self:pgpu ~value:dom0_access ;
    is_pci_hidden
  ) else
    match Db.PGPU.get_dom0_access ~__context ~self:pgpu with
    | `disabled | `enable_on_reboot ->
        true
    | `enabled | `disable_on_reboot ->
        false

let is_local_pgpu ~__context (_, pci_rec) =
  let localhost = Helpers.get_localhost ~__context in
  pci_rec.Db_actions.pCI_host = localhost
  && Xapi_pci.(
       is_class_of_kind Display_controller
         (int_of_id pci_rec.Db_actions.pCI_class_id)
     )
  && pci_rec.Db_actions.pCI_physical_function = Ref.null

(* Makes DB match reality for pgpus on local host *)
let update_gpus ~__context =
  let host = Helpers.get_localhost ~__context in
  let system_display_device = Xapi_pci.get_system_display_device () in
  let existing_pgpus =
    List.filter
      (fun (_, rc) -> rc.API.pGPU_host = host)
      (Db.PGPU.get_all_records ~__context)
  in
  let pcis =
    Xapi_pci.get_local_pcis_and_records ~__context
    |> List.filter (is_local_pgpu ~__context)
    |> List.map (function pci_ref, _ -> pci_ref)
  in
  let is_host_display_enabled =
    match Db.Host.get_display ~__context ~self:host with
    | `enabled | `disable_on_reboot ->
        true
    | `disabled | `enable_on_reboot ->
        false
  in
  let find_or_create pci =
    let pci_addr = Some (Db.PCI.get_pci_id ~__context ~self:pci) in
    let is_system_display_device = system_display_device = pci_addr in
    try
      let rf, rc =
        List.find (fun (_, rc) -> rc.API.pGPU_PCI = pci) existing_pgpus
      in
      let is_pci_hidden = sync_pci_hidden ~__context ~pgpu:rf ~pci in
      (* Now we've determined whether the PCI is hidden, we can work out the
         list of supported VGPU types. *)
      let supported_VGPU_types =
        Xapi_vgpu_type.find_or_create_supported_types ~__context ~pci
          ~is_system_display_device ~is_host_display_enabled ~is_pci_hidden
      in
      let old_supported_VGPU_types =
        Db.PGPU.get_supported_VGPU_types ~__context ~self:rf
      in
      let old_enabled_VGPU_types =
        Db.PGPU.get_enabled_VGPU_types ~__context ~self:rf
      in
      (* Pick up any new supported vGPU configs on the host *)
      Db.PGPU.set_supported_VGPU_types ~__context ~self:rf
        ~value:supported_VGPU_types ;
      (* Calculate the maximum capacities of the supported types. *)
      let max_capacities =
        calculate_max_capacities ~__context ~pCI:pci
          ~size:(Db.PGPU.get_size ~__context ~self:rf)
          ~supported_VGPU_types
      in
      Db.PGPU.set_supported_VGPU_max_capacities ~__context ~self:rf
        ~value:max_capacities ;
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
      Db.PGPU.set_enabled_VGPU_types ~__context ~self:rf
        ~value:(pruned_enabled_types @ new_types_to_enable) ;
      Db.PGPU.set_is_system_display_device ~__context ~self:rf
        ~value:is_system_display_device ;
      populate_compatibility_metadata ~__context ~pgpu:rf ~pgpu_pci:pci ;
      (rf, rc)
    with Not_found ->
      (* If a new PCI has appeared then we know this is a system boot.
         We determine whether dom0 can access the device by looking in the
         boot config. *)
      let is_pci_hidden = Pciops.is_pci_hidden ~__context pci in
      let supported_VGPU_types =
        Xapi_vgpu_type.find_or_create_supported_types ~__context ~pci
          ~is_system_display_device ~is_host_display_enabled ~is_pci_hidden
      in
      let dom0_access =
        if is_pci_hidden then
          `disabled
        else
          `enabled
      in
      let self =
        create ~__context ~pCI:pci ~gPU_group:Ref.null ~host ~other_config:[]
          ~supported_VGPU_types ~size:Constants.pgpu_default_size ~dom0_access
          ~is_system_display_device
      in
      let group = Xapi_gpu_group.find_or_create ~__context self in
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          Client.Client.PGPU.set_GPU_group ~rpc ~session_id ~self ~value:group
      ) ;
      (self, Db.PGPU.get_record ~__context ~self)
  in
  let current_pgpus = List.map find_or_create pcis in
  let obsolete_pgpus =
    Listext.List.set_difference existing_pgpus current_pgpus
  in
  List.iter (fun (self, _) -> Db.PGPU.destroy ~__context ~self) obsolete_pgpus ;
  (* Update the supported/enabled VGPU types on any affected GPU groups. *)
  let groups_to_update =
    Listext.List.setify
      (List.map
         (fun (_, pgpu_rec) -> pgpu_rec.API.pGPU_GPU_group)
         (current_pgpus @ obsolete_pgpus)
      )
  in
  Xapi_vgpu_type.Nvidia_compat.create_compat_config_file __context ;
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      List.iter
        (fun gpu_group ->
          let open Client in
          Client.GPU_group.update_enabled_VGPU_types ~rpc ~session_id
            ~self:gpu_group ;
          Client.GPU_group.update_supported_VGPU_types ~rpc ~session_id
            ~self:gpu_group
        )
        groups_to_update
  )

let update_group_enabled_VGPU_types ~__context ~self =
  let group = Db.PGPU.get_GPU_group ~__context ~self in
  if Db.is_valid_ref __context group then
    Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:group

let pgpu_m = Mutex.create ()

let add_enabled_VGPU_types ~__context ~self ~value =
  with_lock pgpu_m (fun () ->
      Xapi_pgpu_helpers.assert_VGPU_type_supported ~__context ~self
        ~vgpu_type:value ;
      Db.PGPU.add_enabled_VGPU_types ~__context ~self ~value ;
      update_group_enabled_VGPU_types ~__context ~self
  )

let remove_enabled_VGPU_types ~__context ~self ~value =
  with_lock pgpu_m (fun () ->
      Xapi_pgpu_helpers.assert_no_resident_VGPUs_of_type ~__context ~self
        ~vgpu_type:value ;
      Db.PGPU.remove_enabled_VGPU_types ~__context ~self ~value ;
      update_group_enabled_VGPU_types ~__context ~self
  )

let set_enabled_VGPU_types ~__context ~self ~value =
  with_lock pgpu_m (fun () ->
      let current_types = Db.PGPU.get_enabled_VGPU_types ~__context ~self in
      let to_enable = Listext.List.set_difference value current_types
      and to_disable = Listext.List.set_difference current_types value in
      List.iter
        (fun vgpu_type ->
          Xapi_pgpu_helpers.assert_VGPU_type_supported ~__context ~self
            ~vgpu_type
        )
        to_enable ;
      List.iter
        (fun vgpu_type ->
          Xapi_pgpu_helpers.assert_no_resident_VGPUs_of_type ~__context ~self
            ~vgpu_type
        )
        to_disable ;
      Db.PGPU.set_enabled_VGPU_types ~__context ~self ~value ;
      update_group_enabled_VGPU_types ~__context ~self
  )

let set_GPU_group ~__context ~self ~value =
  debug "Move PGPU %s -> GPU group %s"
    (Db.PGPU.get_uuid ~__context ~self)
    (Db.GPU_group.get_uuid ~__context ~self:value) ;
  with_lock pgpu_m (fun () ->
      (* Precondition: PGPU has no resident VGPUs *)
      let resident_vgpus = Db.PGPU.get_resident_VGPUs ~__context ~self in
      ( if resident_vgpus <> [] then
          let resident_vms =
            List.map
              (fun self -> Db.VGPU.get_VM ~__context ~self)
              resident_vgpus
          in
          raise
            (Api_errors.Server_error
               ( Api_errors.pgpu_in_use_by_vm
               , List.map Ref.string_of resident_vms
               )
            )
      ) ;
      let check_compatibility gpu_type group_types =
        match group_types with
        | [] ->
            (true, [gpu_type])
        | _ ->
            (List.mem gpu_type group_types, group_types)
      in
      let pci = Db.PGPU.get_PCI ~__context ~self in
      let gpu_type = Xapi_pci.string_of_pci ~__context ~self:pci
      and group_types = Db.GPU_group.get_GPU_types ~__context ~self:value in
      match check_compatibility gpu_type group_types with
      | true, new_types ->
          let old_group = Db.PGPU.get_GPU_group ~__context ~self in
          Db.PGPU.set_GPU_group ~__context ~self ~value ;
          (* Group inherits the device type *)
          Db.GPU_group.set_GPU_types ~__context ~self:value ~value:new_types ;
          debug "PGPU %s moved to GPU group %s. Group GPU types = [ %s ]."
            (Db.PGPU.get_uuid ~__context ~self)
            (Db.GPU_group.get_uuid ~__context ~self:value)
            (String.concat "; " new_types) ;
          (* Update the old and new groups' cached lists of VGPU_types. *)
          if Db.is_valid_ref __context old_group then (
            Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:old_group ;
            Xapi_gpu_group.update_supported_VGPU_types ~__context
              ~self:old_group
          ) ;
          Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:value ;
          Xapi_gpu_group.update_supported_VGPU_types ~__context ~self:value
      | false, _ ->
          raise
            (Api_errors.Server_error
               ( Api_errors.pgpu_not_compatible_with_gpu_group
               , [gpu_type; "[" ^ String.concat ", " group_types ^ "]"]
               )
            )
  )

let get_remaining_capacity ~__context ~self ~vgpu_type =
  match
    Xapi_pgpu_helpers.get_remaining_capacity_internal ~__context ~self
      ~vgpu_type ~pre_allocate_list:[]
  with
  | Error _ ->
      0L
  | Ok capacity ->
      capacity

let assert_can_run_VGPU ~__context ~self ~vgpu =
  let vgpu_type = Db.VGPU.get_type ~__context ~self:vgpu in
  Xapi_pgpu_helpers.assert_capacity_exists_for_VGPU_type ~__context ~self
    ~vgpu_type

let update_dom0_access ~__context ~self ~action =
  let db_current = Db.PGPU.get_dom0_access ~__context ~self in
  let db_new =
    match (db_current, action) with
    | `enabled, `enable | `disable_on_reboot, `enable ->
        `enabled
    | `disabled, `enable | `enable_on_reboot, `enable ->
        `enable_on_reboot
    | `enabled, `disable | `disable_on_reboot, `disable ->
        `disable_on_reboot
    | `disabled, `disable | `enable_on_reboot, `disable ->
        `disabled
  in
  let pci = Db.PGPU.get_PCI ~__context ~self in
  ( match db_new with
  | `enabled | `enable_on_reboot ->
      Pciops.unhide_pci ~__context pci
  | `disabled | `disable_on_reboot ->
      Pciops.hide_pci ~__context pci
  ) ;
  Db.PGPU.set_dom0_access ~__context ~self ~value:db_new ;
  db_new

let enable_dom0_access ~__context ~self =
  update_dom0_access ~__context ~self ~action:`enable

let disable_dom0_access ~__context ~self =
  if not (Pool_features.is_enabled ~__context Features.Integrated_GPU) then
    raise Api_errors.(Server_error (feature_restricted, [])) ;
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
  ignore
    (Forkhelpers.execute_command_get_output !Xapi_globs.modprobe_path ["gim"]) ;
  (* Update the gpus even if the module was present already, in case it was
   * already loaded before xapi was (re)started. *)
  Xapi_pci.update_pcis ~__context

(* This must be run LOCALLY on the host that is about to start a VM that is
 * going to use a vgpu backed by an nvidia pgpu. *)
let nvidia_vf_setup_mutex = Mutex.create ()

let nvidia_vf_setup ~__context ~pf ~enable =
  let sprintf = Printf.sprintf in
  let fail msg = Api_errors.(Server_error (internal_error, [msg])) in
  let script = !Xapi_globs.nvidia_sriov_manage_script in
  let enable' = if enable then "-e" else "-d" in
  let bind_path = "/sys/bus/pci/drivers/nvidia/bind" in
  let unbind_path pci = sprintf "/sys/bus/pci/devices/%s/driver/unbind" pci in
  let pci = Db.PCI.get_pci_id ~__context ~self:pf in
  let addr_of pci = Xenops_interface.Pci.address_of_string pci in
  let dequarantine pci = Xapi_pci.dequarantine ~__context (addr_of pci) in
  (* [num_vfs pci] returns the number of PCI VFs of [pci] or 0 if
     [pci] is not an SRIOV device
  *)
  let num_vfs pci =
    let path = Printf.sprintf "/sys/bus/pci/devices/%s/sriov_numvfs" pci in
    try Some (Unixext.string_of_file path |> String.trim |> int_of_string) with
    | Unix.(Unix_error (ENOENT, _, _)) ->
        debug "File %s does not exist - assuming no SRIOV devices in use" path ;
        None
    | exn ->
        let msg =
          Printf.sprintf "Can't read %s to activate Nvidia GPU %s: %s" path pci
            (Printexc.to_string exn)
        in
        error "%s" msg ;
        raise (fail msg)
  in
  let write_to path pci =
    try
      let fn fd = Unixext.really_write fd pci 0 (String.length pci) in
      Unixext.with_file path [Unix.O_WRONLY] 0o640 fn
    with e ->
      error "failed to write to %s to re-bind PCI %s to Nvidia driver: %s" path
        pci (Printexc.to_string e) ;
      raise (fail (sprintf "Can't rebind PCI %s driver" pci))
  in
  let bind_to_nvidia pci =
    match Xapi_pci_helpers.get_driver_name pci with
    | Some "nvidia" ->
        debug "PCI %s already bound to NVidia driver" pci
    | Some driver ->
        debug "PCI %s bound to %s, rebinding to NVidia" pci driver ;
        write_to (unbind_path pci) pci ;
        write_to bind_path pci
    | None ->
        debug "PCI %s not bound, binding to NVidia" pci ;
        write_to bind_path pci
  in
  let activate_vfs pci =
    match num_vfs pci with
    | None ->
        let host = Db.PCI.get_host ~__context ~self:pf in
        let device = Db.PCI.get_device_name ~__context ~self:pf in
        error "Can't determine number of VFs for PCI %s" pci ;
        raise
          Api_errors.(
            Server_error
              (nvidia_sriov_misconfigured, [Ref.string_of host; device])
          )
    | Some 0 when Sys.file_exists script ->
        debug "PCI %s has 0 VFs - calling %s" pci script ;
        let out, _ =
          Forkhelpers.execute_command_get_output script [enable'; pci]
        in
        debug "Activating NVidia vGPUs %s yielded: '%s'" pci (String.escaped out)
    | Some n when n > 0 ->
        debug "PCI %s already has %n VFs - not calling %s" pci n script
    | _ ->
        error "nvdia_vf_setup %s does not exist" script ;
        raise (fail (sprintf "Can't locate %s" script))
  in
  (* Update the gpus even if the VFs were present already, in case they were
   * already created before xapi was (re)started. *)
  with_lock nvidia_vf_setup_mutex @@ fun () ->
  debug "nvidia_vf_setup_mutex - enter" ;
  dequarantine pci ;
  (* this is always safe to do *)
  bind_to_nvidia pci ;
  activate_vfs pci ;
  debug "nvidia_vf_setup_mutex - exit" ;
  Xapi_pci.update_pcis ~__context
