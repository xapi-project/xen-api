(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** HTTP handler for importing a VM from a stream.
 * @group Import and Export
*)

module D = Debug.Make (struct let name = "import" end)

open D
module Listext = Xapi_stdext_std.Listext
module Xstringext = Xapi_stdext_std.Xstringext
module Unixext = Xapi_stdext_unix.Unixext
open Http
open Importexport
open Xapi_stdext_pervasives.Pervasiveext
open Client

type import_failure =
  | Some_checksums_failed
  | Cannot_handle_chunked
  | Failed_to_find_object of string
  | Attached_disks_not_found
  | Unexpected_file of string (* expected *) * string

(* actual *)

exception IFailure of import_failure

open Xapi_vm_memory_constraints
open Vm_memory_constraints

type metadata_options = {
    (* If true, don't create any database objects. *)
    dry_run: bool
  ; (* If true, treat the import as if it is preparation for a live migration.
       	 * This has the following consequences:
       	 * - We must perform extra checks on the VM object - do we have enough memory? Are the CPU flags compatible? Is there an HA plan for it?
       	 * - If the migration is a dry run we don't need to check for VDIs, since VDI.mirror will have created them during a real migration.
       	 * - If the migration is for real, we will expect the VM export code on the source host to have mapped the VDI locations onto their
       	 *   mirrored counterparts which are present on this host. *)
    live: bool
  ; check_cpu: bool
  ; (* An optional src VDI -> destination VDI rewrite list *)
    vdi_map: (string * string) list
}

type import_type =
  (* Import the metadata of a VM whose disks already exist. *)
  | Metadata_import of metadata_options
  (* Import a VM and stream its disks into the specified SR. *)
  | Full_import of API.ref_SR

(** Allows the import to be customised *)
type config = {
    (* Determines how to handle the import - see above. *)
    import_type: import_type
  ; (* true if we want to restore as a perfect backup. Currently we preserve the
       		   interface MAC addresses but we still regenerate UUIDs (because we lack the
       		   internal APIs to keep them *)
    full_restore: bool
  ; (* true if the user has provided '--force' *)
    force: bool
}

let is_live config =
  match config.import_type with Metadata_import {live; _} -> live | _ -> false

let needs_cpu_check config vm_record =
  vm_record.API.vM_power_state <> `Halted
  &&
  match config.import_type with
  | Metadata_import {check_cpu; _} ->
      check_cpu
  | _ ->
      false

(** List of (datamodel classname * Reference in export * Reference in database) *)
type table = (string * string * string) list

(** Track the table of external reference -> internal reference and a list of cleanup
    functions to delete all the objects we've created, in the event of error. *)
type state = {
    mutable table: table
  ; mutable created_vms: table
  ; mutable cleanup:
      (Context.t -> (Rpc.call -> Rpc.response) -> API.ref_session -> unit) list
  ; export: obj list
}

let initial_state export = {table= []; created_vms= []; cleanup= []; export}

let log_reraise msg f x =
  try f x
  with e ->
    Backtrace.is_important e ;
    error "Import failed: %s" msg ;
    raise e

let lookup x (table : table) =
  let id = Ref.string_of x in
  try
    let _, _, r = List.find (fun (_, i, _) -> i = id) table in
    Ref.of_string r
  with Not_found as e ->
    Backtrace.reraise e (IFailure (Failed_to_find_object id))

let exists x (table : table) =
  let id = Ref.string_of x in
  List.filter (fun (_, i, _) -> i = id) table <> []

(* Using a reference string from the original export, find the XMLRPC snapshot
   of the appropriate object. *)
let find_in_export x export =
  try
    let obj = List.find (fun obj -> obj.id = x) export in
    obj.snapshot
  with Not_found as e ->
    Backtrace.reraise e (IFailure (Failed_to_find_object x))

let choose_one = function [x] -> Some x | x :: _ -> Some x | [] -> None

(* Return the list of non-CDROM VDIs ie those which will be streamed-in *)
let non_cdrom_vdis (x : header) =
  let all_vbds =
    List.filter (fun x -> x.cls = Datamodel_common._vbd) x.objects
  in
  let all_vbds = List.map (fun x -> API.vBD_t_of_rpc x.snapshot) all_vbds in
  let all_disk_vbds = List.filter (fun x -> x.API.vBD_type <> `CD) all_vbds in
  let all_disk_vdis =
    List.map (fun x -> Ref.string_of x.API.vBD_VDI) all_disk_vbds
  in
  (* Remove all those whose SR has content-type = "iso" *)
  let all_disk_vdis =
    List.filter
      (fun vdi ->
        let vdir = API.vDI_t_of_rpc (find_in_export vdi x.objects) in
        let sr =
          API.sR_t_of_rpc
            (find_in_export (Ref.string_of vdir.API.vDI_SR) x.objects)
        in
        sr.API.sR_content_type <> "iso"
      )
      all_disk_vdis
  in
  let all_vdis =
    List.filter (fun x -> x.cls = Datamodel_common._vdi) x.objects
  in
  List.filter
    (fun x ->
      false
      || List.mem x.id all_disk_vdis
      || (API.vDI_t_of_rpc x.snapshot).API.vDI_type = `suspend
    )
    all_vdis

let get_vm_record snapshot =
  let vm_record = API.vM_t_of_rpc snapshot in
  (* Ensure that the domain_type is set correctly *)
  if vm_record.API.vM_domain_type = `unspecified then
    {
      vm_record with
      API.vM_domain_type=
        Xapi_vm_helpers.derive_domain_type
          ~hVM_boot_policy:vm_record.API.vM_HVM_boot_policy
    }
  else
    vm_record

(* Check to see if another VM exists with the same MAC seed. *)
(* Check VM uuids don't already exist. Check that if a VDI exists then it is a CDROM. *)
let assert_can_restore_backup ~__context rpc session_id (x : header) =
  let get_mac_seed vm =
    if List.mem_assoc Xapi_globs.mac_seed vm.API.vM_other_config then
      Some (List.assoc Xapi_globs.mac_seed vm.API.vM_other_config, vm)
    else
      None
  in
  let get_vm_uuid_of_snap s =
    let snapshot_of = Ref.string_of s.API.vM_snapshot_of in
    try
      if String.starts_with ~prefix:"Ref:" snapshot_of then
        (* This should be a snapshot in the archive *)
        let v =
          List.find
            (fun v -> v.cls = Datamodel_common._vm && v.id = snapshot_of)
            x.objects
        in
        let v = get_vm_record v.snapshot in
        Some v.API.vM_uuid
      else if String.starts_with ~prefix:Ref.ref_prefix snapshot_of then
        (* This should be a snapshot in a live system *)
        if Db.is_valid_ref __context s.API.vM_snapshot_of then
          Some (Db.VM.get_uuid ~__context ~self:s.API.vM_snapshot_of)
        else
          Some
            (List.assoc Db_names.uuid
               (Helpers.vm_string_to_assoc s.API.vM_snapshot_metadata)
            )
      else
        None
    with _ -> None
  in

  (* This function should be called when a VM/snapshot to import has the same
     	   mac seed as an existing VM. They are considered compatible only in the
     	   following cases:

     	   - Both are VMs, and having the same uuid
     	   - Both are snapshots, and the VMs they were derived from are the same one
     	   - One is snapshot, one is VM, and the snapshot was derived from the VM
  *)
  let is_compatible v1 v2 =
    match (v1.API.vM_is_a_snapshot, v2.API.vM_is_a_snapshot) with
    | false, false ->
        v1.API.vM_uuid = v2.API.vM_uuid
    | true, true ->
        let v1' = get_vm_uuid_of_snap v1 in
        let v2' = get_vm_uuid_of_snap v2 in
        v1' <> None && v2' <> None && v1' = v2'
    | true, false ->
        let v1' = get_vm_uuid_of_snap v1 in
        v1' = Some v2.API.vM_uuid
    | false, true ->
        let v2' = get_vm_uuid_of_snap v2 in
        v2' = Some v1.API.vM_uuid
  in
  let import_vms =
    List.filter_map
      (fun x ->
        if x.cls <> Datamodel_common._vm then
          None
        else
          let x = get_vm_record x.snapshot in
          get_mac_seed x
      )
      x.objects
  in
  let existing_vms =
    List.filter_map
      (fun (_, v) -> get_mac_seed v)
      (Client.VM.get_all_records ~rpc ~session_id)
  in
  List.iter
    (fun (mac, vm) ->
      List.iter
        (fun (mac', vm') ->
          if mac = mac' && not (is_compatible vm vm') then
            raise Api_errors.(Server_error (duplicate_mac_seed, [mac']))
        )
        existing_vms
    )
    import_vms

let assert_can_live_import __context vm_record =
  let host = Helpers.get_localhost ~__context in
  let assert_memory_available () =
    let host_mem_available =
      Memory_check.host_compute_free_memory_with_maximum_compression ~__context
        ~host None
    in
    let main, shadow =
      Memory_check.vm_compute_start_memory ~__context vm_record
    in
    let mem_reqd_for_vm = Int64.add main shadow in
    if host_mem_available < mem_reqd_for_vm then
      raise
        (Api_errors.Server_error
           ( Api_errors.host_not_enough_free_memory
           , [
               Int64.to_string mem_reqd_for_vm
             ; Int64.to_string host_mem_available
             ]
           )
        )
  in
  if
    vm_record.API.vM_power_state = `Running
    || vm_record.API.vM_power_state = `Paused
  then
    assert_memory_available ()

(* Assert that the local host, which is the host we are live-migrating the VM to,
 * has free capacity on a PGPU from the given VGPU's GPU group. *)
let assert_can_live_import_vgpu ~__context vgpu_record =
  let host = Helpers.get_localhost ~__context in
  let local_pgpus =
    Db.PGPU.get_refs_where ~__context
      ~expr:
        Xapi_database.Db_filter_types.(
          And
            ( Eq
                ( Field "GPU_group"
                , Literal (Ref.string_of vgpu_record.API.vGPU_GPU_group)
                )
            , Eq (Field "host", Literal (Ref.string_of host))
            )
        )
  in
  let capacity_exists =
    List.exists
      (fun pgpu ->
        try
          Xapi_pgpu_helpers.assert_capacity_exists_for_VGPU_type ~__context
            ~self:pgpu ~vgpu_type:vgpu_record.API.vGPU_type ;
          true
        with _ -> false
      )
      local_pgpus
  in
  if not capacity_exists then
    raise
      Api_errors.(
        Server_error
          ( vm_requires_gpu
          , [
              Ref.string_of vgpu_record.API.vGPU_VM
            ; Ref.string_of vgpu_record.API.vGPU_GPU_group
            ]
          )
      )

(* The signature for a set of functions which we must provide to be able to import an object type. *)
module type HandlerTools = sig
  (* A type which represents how we should deal with the import of an object. *)
  type precheck_t

  (* Compare the state of the database with the metadata to be imported. *)
  (* Returns a result which signals what we should do to import the metadata. *)
  val precheck :
       Context.t
    -> config
    -> (Rpc.call -> Rpc.response)
    -> API.ref_session
    -> state
    -> obj
    -> precheck_t

  (* Handle the result of the precheck function, but don't create any database objects. *)
  (* Add objects to the state table if necessary, to keep track of what would have been imported.*)
  val handle_dry_run :
       Context.t
    -> config
    -> (Rpc.call -> Rpc.response)
    -> API.ref_session
    -> state
    -> obj
    -> precheck_t
    -> unit

  (* Handle the result of the check function, creating database objects if necessary. *)
  (* For certain combinations of result and object type, this can be aliased to handle_dry_run. *)
  val handle :
       Context.t
    -> config
    -> (Rpc.call -> Rpc.response)
    -> API.ref_session
    -> state
    -> obj
    -> precheck_t
    -> unit
end

(* Make a handler for a set of handler functions. *)
module MakeHandler =
functor
  (M : HandlerTools)
  ->
  struct
    let handle __context config rpc session_id state obj =
      let dry_run =
        match config.import_type with
        | Metadata_import {dry_run= true; _} ->
            true
        | _ ->
            false
      in
      let precheck_result =
        M.precheck __context config rpc session_id state obj
      in
      if dry_run then
        M.handle_dry_run __context config rpc session_id state obj
          precheck_result
      else
        M.handle __context config rpc session_id state obj precheck_result
  end

module Host : HandlerTools = struct
  type precheck_t = Found_host of API.ref_host | Found_no_host

  let precheck __context _config _rpc _session_id _state x =
    let host_record = API.host_t_of_rpc x.snapshot in
    try
      Found_host (Db.Host.get_by_uuid ~__context ~uuid:host_record.API.host_uuid)
    with _ -> Found_no_host

  let handle_dry_run __context _config _rpc _session_id state x precheck_result
      =
    let host =
      match precheck_result with
      | Found_host host' ->
          host'
      | Found_no_host ->
          Ref.null
    in
    state.table <- (x.cls, x.id, Ref.string_of host) :: state.table

  let handle = handle_dry_run
end

module VM : HandlerTools = struct
  type precheck_t =
    | Default_template of API.ref_VM
    | Replace of API.ref_VM * API.vM_t
    | Fail of exn
    | Skip
    | Clean_import of API.vM_t

  let precheck __context config _rpc _session_id state x =
    let vm_record = get_vm_record x.snapshot in
    let is_default_template =
      vm_record.API.vM_is_default_template
      || vm_record.API.vM_is_a_template
         && List.mem_assoc Xapi_globs.default_template_key
              vm_record.API.vM_other_config
         && List.assoc Xapi_globs.default_template_key
              vm_record.API.vM_other_config
            = "true"
    in

    let maybe_template =
      List.nth_opt
        (Db.VM.get_by_name_label ~__context ~label:vm_record.API.vM_name_label)
        0
    in
    match (is_default_template, maybe_template) with
    | true, Some template ->
        Default_template template
    | _ -> (
        let import_action =
          (* Check for an existing VM with the same UUID - if one exists, what we do next *)
          (* will depend on the state of the VM and whether the import is forced. *)
          let get_vm_by_uuid () =
            Db.VM.get_by_uuid ~__context ~uuid:vm_record.API.vM_uuid
          in
          let vm_uuid_exists () =
            try
              ignore (get_vm_by_uuid ()) ;
              true
            with _ -> false
          in
          (* If full_restore is true then we want to keep the VM uuid - this may involve replacing an existing VM. *)
          if config.full_restore && vm_uuid_exists () then
            let vm = get_vm_by_uuid () in
            (* The existing VM cannot be replaced if it is running. *)
            (* If import is forced then skip the VM, else throw an error. *)
            let power_state = Db.VM.get_power_state ~__context ~self:vm in
            if power_state <> `Halted then
              if config.force then (
                debug
                  "Forced import skipping VM %s as VM to replace was not \
                   halted."
                  vm_record.API.vM_uuid ;
                Skip
              ) else
                Fail
                  (Api_errors.Server_error
                     ( Api_errors.vm_bad_power_state
                     , [
                         Ref.string_of vm
                       ; Record_util.vm_power_state_to_string `Halted
                       ; Record_util.vm_power_state_to_string power_state
                       ]
                     )
                  )
            else
              (* The existing VM should not be replaced if the version to be imported is no newer, *)
              (* unless the import is forced. *)
              let existing_version = Db.VM.get_version ~__context ~self:vm in
              let version_to_import = vm_record.API.vM_version in
              if existing_version >= version_to_import && config.force = false
              then
                Fail
                  (Api_errors.Server_error
                     ( Api_errors.vm_to_import_is_not_newer_version
                     , [
                         Ref.string_of vm
                       ; Int64.to_string existing_version
                       ; Int64.to_string version_to_import
                       ]
                     )
                  )
              else
                Replace (vm, vm_record)
          else
            let vm_record =
              if not config.force then
                {
                  vm_record with
                  API.vM_is_default_template= false
                ; vM_other_config=
                    List.remove_assoc Xapi_globs.default_template_key
                      vm_record.API.vM_other_config
                }
              else
                vm_record
            in
            Clean_import vm_record
        in
        match import_action with
        | Replace (_, vm_record) | Clean_import vm_record ->
            if is_live config then
              assert_can_live_import __context vm_record ;
            ( if needs_cpu_check config vm_record then
                let vmm_record =
                  find_in_export
                    (Ref.string_of vm_record.API.vM_metrics)
                    state.export
                  |> API.vM_metrics_t_of_rpc
                in
                let host = Helpers.get_localhost ~__context in
                (* Don't check CPUID on 'unspecified' snapshots *)
                match
                  ( vm_record.API.vM_is_a_snapshot
                  , vmm_record.API.vM_metrics_current_domain_type
                  )
                with
                | true, `unspecified ->
                    (* A snapshot which was taken from a shutdown VM. *)
                    ()
                | _, dt ->
                    Cpuid_helpers.assert_vm_is_compatible ~__context
                      ~vm:(`import (vm_record, dt))
                      ~host
            ) ;
            import_action
        | _ ->
            import_action
      )

  let handle_dry_run __context _config _rpc _session_id state x precheck_result
      =
    match precheck_result with
    | Skip ->
        ()
    | Fail e ->
        raise e
    | Default_template template ->
        state.table <- (x.cls, x.id, Ref.string_of template) :: state.table ;
        state.created_vms <-
          (x.cls, x.id, Ref.string_of template) :: state.created_vms
    | Clean_import _ | Replace _ ->
        let dummy_vm = Ref.make () in
        state.table <- (x.cls, x.id, Ref.string_of dummy_vm) :: state.table

  let handle __context config rpc session_id state x precheck_result =
    (* This function assumes we've already checked for and dealt with any existing VM with the same UUID. *)
    let do_import vm_record =
      let task_id = Ref.string_of (Context.get_task_id __context) in
      (* Remove the grant guest API access key unconditionally (it's only for our RHEL4 templates atm) *)
      let other_config =
        List.filter
          (fun (key, _) -> key <> Xapi_globs.grant_api_access)
          vm_record.API.vM_other_config
      in
      (* If not performing a full restore then generate a fresh MAC seed *)
      let other_config =
        if config.full_restore then
          other_config
        else
          (Xapi_globs.mac_seed, Uuidx.(to_string (make ())))
          :: List.filter (fun (x, _) -> x <> Xapi_globs.mac_seed) other_config
      in
      let vm_record = {vm_record with API.vM_other_config= other_config} in
      (* Preserve genid for cross-pool migrates, because to the guest the
         			 * disk looks like it hasn't changed.
         			 * Preserve genid for templates, since they're not going to be started.
         			 * Generate a fresh genid for normal VM imports. *)
      let vm_record =
        if is_live config || vm_record.API.vM_is_a_template then
          vm_record
        else
          {
            vm_record with
            API.vM_generation_id=
              Xapi_vm_helpers.fresh_genid
                ~current_genid:vm_record.API.vM_generation_id ()
          }
      in
      let vm_record =
        if vm_exported_pre_dmc x then (
          let safe_constraints =
            Vm_memory_constraints.reset_to_safe_defaults
              ~constraints:(Vm_memory_constraints.extract ~vm_record)
          in
          debug "VM %s was exported pre-DMC; dynamic_{min,max},target <- %Ld"
            vm_record.API.vM_name_label safe_constraints.static_max ;
          {
            vm_record with
            API.vM_memory_static_min= safe_constraints.static_min
          ; vM_memory_dynamic_min= safe_constraints.dynamic_min
          ; vM_memory_target= safe_constraints.target
          ; vM_memory_dynamic_max= safe_constraints.dynamic_max
          ; vM_memory_static_max= safe_constraints.static_max
          }
        ) else
          vm_record
      in
      let vm_record =
        if vm_has_field ~x ~name:"has_vendor_device" then
          vm_record
        else
          {vm_record with API.vM_has_vendor_device= false}
      in
      let vm_record =
        {
          vm_record with
          API.vM_memory_overhead=
            Memory_check.vm_compute_memory_overhead ~vm_record
        }
      in
      let vm_record = {vm_record with API.vM_protection_policy= Ref.null} in
      (* Full restore preserves UUIDs, so if we are replacing an existing VM the version number should be incremented *)
      (* to keep track of how many times this VM has been restored. If not a full restore, then we don't need to keep track. *)
      let vm_record =
        if config.full_restore then
          {vm_record with API.vM_version= Int64.add vm_record.API.vM_version 1L}
        else
          {vm_record with API.vM_version= 0L}
      in
      (* Clear the appliance field - in the case of DR we will reconstruct the appliance separately. *)
      let vm_record = {vm_record with API.vM_appliance= Ref.null} in
      (* Correct ha-restart-priority for pre boston imports*)
      let vm_record =
        match vm_record.API.vM_ha_restart_priority with
        | ("0" | "1" | "2" | "3") as order ->
            {
              vm_record with
              API.vM_ha_restart_priority= "restart"
            ; API.vM_order= Int64.of_string order
            }
        | _ ->
            vm_record
      in
      (* Initialize platform["device-model"] if it is not set *)
      let vm_record =
        {
          vm_record with
          API.vM_platform=
            Xapi_vm_helpers.ensure_device_model_profile_present ~__context
              ~domain_type:vm_record.API.vM_domain_type
              ~is_a_template:vm_record.API.vM_is_a_template
              vm_record.API.vM_platform
        }
      in
      let vm =
        log_reraise
          ("failed to create VM with name-label " ^ vm_record.API.vM_name_label)
          (fun value ->
            let vm = Client.VM.create_from_record ~rpc ~session_id ~value in
            if config.full_restore then
              Db.VM.set_uuid ~__context ~self:vm ~value:value.API.vM_uuid ;
            vm
          )
          {
            vm_record with
            API.vM_suspend_VDI= Ref.null
          ; API.vM_power_state= `Halted
          }
      in
      state.cleanup <-
        (fun __context rpc session_id ->
          (* Need to get rid of the import task or we cannot destroy the VM *)
          Helpers.log_exn_continue
            (Printf.sprintf
               "Attempting to remove import from current_operations of VM: %s"
               (Ref.string_of vm)
            )
            (fun () ->
              Db.VM.remove_from_current_operations ~__context ~self:vm
                ~key:task_id
            )
            () ;
          Db.VM.set_power_state ~__context ~self:vm ~value:`Halted ;
          Client.VM.destroy ~rpc ~session_id ~self:vm
        )
        :: state.cleanup ;
      (* Restore the last_booted_record too (critical if suspended but might as well do it all the time) *)
      Db.VM.set_last_booted_record ~__context ~self:vm
        ~value:vm_record.API.vM_last_booted_record ;
      Db.VM.set_last_boot_CPU_flags ~__context ~self:vm
        ~value:vm_record.API.vM_last_boot_CPU_flags ;
      TaskHelper.operate_on_db_task ~__context (fun t ->
          ( try
              Db.VM.remove_from_other_config ~__context ~self:vm
                ~key:Xapi_globs.import_task
            with _ -> ()
          ) ;
          Db.VM.add_to_other_config ~__context ~self:vm
            ~key:Xapi_globs.import_task ~value:(Ref.string_of t)
      ) ;
      (* Set the power_state and suspend_VDI if the VM is suspended.
         			 * If anything goes wrong, still continue if forced. *)
      if vm_record.API.vM_power_state = `Suspended then (
        try
          let vdi = (lookup vm_record.API.vM_suspend_VDI) state.table in
          Db.VM.set_power_state ~__context ~self:vm ~value:`Suspended ;
          Db.VM.set_suspend_VDI ~__context ~self:vm ~value:vdi ;
          let vm_metrics = Db.VM.get_metrics ~__context ~self:vm in
          Db.VM_metrics.set_current_domain_type ~__context ~self:vm_metrics
            ~value:vm_record.API.vM_domain_type
        with e ->
          if not config.force then (
            Backtrace.is_important e ;
            let msg =
              "Failed to find VM's suspend_VDI: "
              ^ Ref.string_of vm_record.API.vM_suspend_VDI
            in
            error "Import failed: %s" msg ;
            raise e
          )
      ) else
        Db.VM.set_power_state ~__context ~self:vm ~value:`Halted ;
      (* We might want to import a control domain *)
      Db.VM.set_is_control_domain ~__context ~self:vm
        ~value:vm_record.API.vM_is_control_domain ;
      Db.VM.set_resident_on ~__context ~self:vm
        ~value:
          ( try lookup vm_record.API.vM_resident_on state.table
            with _ -> Ref.null
          ) ;
      Db.VM.set_affinity ~__context ~self:vm
        ~value:
          (try lookup vm_record.API.vM_affinity state.table with _ -> Ref.null) ;
      (* Update the snapshot metadata. At this points, the snapshot_of field is not relevant as
         				 it use the export ref. However, as the corresponding VM object may have not been created
         				 yet, this fiels contains some useful information to update it later. *)
      Db.VM.set_is_a_snapshot ~__context ~self:vm
        ~value:vm_record.API.vM_is_a_snapshot ;
      Db.VM.set_snapshot_info ~__context ~self:vm
        ~value:vm_record.API.vM_snapshot_info ;
      Db.VM.set_snapshot_of ~__context ~self:vm
        ~value:vm_record.API.vM_snapshot_of ;
      Db.VM.set_snapshot_time ~__context ~self:vm
        ~value:vm_record.API.vM_snapshot_time ;
      Db.VM.set_transportable_snapshot_id ~__context ~self:vm
        ~value:vm_record.API.vM_transportable_snapshot_id ;
      (* VM might have suspend_SR that does not exist on this pool *)
      if
        None
        = Helpers.check_sr_exists ~__context ~self:vm_record.API.vM_suspend_SR
      then
        Db.VM.set_suspend_SR ~__context ~self:vm ~value:Ref.null ;
      Db.VM.set_parent ~__context ~self:vm ~value:vm_record.API.vM_parent ;
      ( try
          let vmm = lookup vm_record.API.vM_metrics state.table in
          (* We have VM_metrics in the imported metadata, so use it, and destroy
             the record created by VM.create_from_record above. *)
          let replaced_vmm = Db.VM.get_metrics ~__context ~self:vm in
          Db.VM.set_metrics ~__context ~self:vm ~value:vmm ;
          Db.VM_metrics.destroy ~__context ~self:replaced_vmm
        with _ -> ()
      ) ;
      ( try
          let gm = lookup vm_record.API.vM_guest_metrics state.table in
          Db.VM.set_guest_metrics ~__context ~self:vm ~value:gm
        with _ -> ()
      ) ;
      Db.VM.set_bios_strings ~__context ~self:vm
        ~value:vm_record.API.vM_bios_strings ;
      debug "Created VM: %s (was %s)" (Ref.string_of vm) x.id ;
      (* Although someone could sneak in here and attempt to power on the VM, it
         				 doesn't really matter since no VBDs have been created yet.
         				 We don't bother doing this if --force is set otherwise on error the VM
         				 remains locked. *)
      if not config.force then
        Db.VM.add_to_current_operations ~__context ~self:vm ~key:task_id
          ~value:`import ;
      Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm ;
      state.table <- (x.cls, x.id, Ref.string_of vm) :: state.table ;
      state.created_vms <- (x.cls, x.id, Ref.string_of vm) :: state.created_vms
    in
    match precheck_result with
    | Skip | Fail _ | Default_template _ ->
        handle_dry_run __context config rpc session_id state x precheck_result
    | Clean_import vm_record ->
        do_import vm_record
    | Replace (vm, vm_record) ->
        (* Destroy the existing VM, along with its VIFs and VBDs. *)
        debug "Replacing VM %s" vm_record.API.vM_uuid ;
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            let vifs = Db.VM.get_VIFs ~__context ~self:vm in
            List.iter
              (fun vif -> Client.VIF.destroy ~rpc ~session_id ~self:vif)
              vifs ;
            let vbds = Db.VM.get_VBDs ~__context ~self:vm in
            List.iter
              (fun vbd -> Client.VBD.destroy ~rpc ~session_id ~self:vbd)
              vbds ;
            Client.VM.destroy ~rpc ~session_id ~self:vm
        ) ;
        do_import vm_record
end

(** Create the guest metrics *)
module GuestMetrics : HandlerTools = struct
  type precheck_t = OK

  let precheck __context _config _rpc _session_id _state _x = OK

  let handle_dry_run __context _config _rpc _session_id state x _precheck_result
      =
    let dummy_gm = Ref.make () in
    state.table <- (x.cls, x.id, Ref.string_of dummy_gm) :: state.table

  let handle __context _config _rpc _session_id state x _precheck_result =
    let gm_record = API.vM_guest_metrics_t_of_rpc x.snapshot in
    let gm = Ref.make () in
    Db.VM_guest_metrics.create ~__context ~ref:gm
      ~uuid:(Uuidx.to_string (Uuidx.make ()))
      ~os_version:gm_record.API.vM_guest_metrics_os_version
      ~netbios_name:gm_record.API.vM_guest_metrics_netbios_name
      ~pV_drivers_version:gm_record.API.vM_guest_metrics_PV_drivers_version
      ~pV_drivers_up_to_date:
        gm_record.API.vM_guest_metrics_PV_drivers_up_to_date
      ~memory:gm_record.API.vM_guest_metrics_memory
      ~disks:gm_record.API.vM_guest_metrics_disks
      ~networks:gm_record.API.vM_guest_metrics_networks
      ~services:gm_record.API.vM_guest_metrics_services
      ~pV_drivers_detected:gm_record.API.vM_guest_metrics_PV_drivers_detected
      ~other:gm_record.API.vM_guest_metrics_other
      ~last_updated:gm_record.API.vM_guest_metrics_last_updated
      ~other_config:gm_record.API.vM_guest_metrics_other_config
      ~live:gm_record.API.vM_guest_metrics_live
      ~can_use_hotplug_vbd:gm_record.API.vM_guest_metrics_can_use_hotplug_vbd
      ~can_use_hotplug_vif:gm_record.API.vM_guest_metrics_can_use_hotplug_vif ;
    state.table <- (x.cls, x.id, Ref.string_of gm) :: state.table
end

(** Create the VM metrics *)
module Metrics : HandlerTools = struct
  type precheck_t = OK

  let precheck __context _config _rpc _session_id _state _x = OK

  let handle_dry_run __context _config _rpc _session_id state x _precheck_result
      =
    let dummy_gm = Ref.make () in
    state.table <- (x.cls, x.id, Ref.string_of dummy_gm) :: state.table

  let handle __context _config _rpc _session_id state x _precheck_result =
    let vmm_record = API.vM_metrics_t_of_rpc x.snapshot in
    let vmm = Ref.make () in
    Db.VM_metrics.create ~__context ~ref:vmm
      ~uuid:(Uuidx.to_string (Uuidx.make ()))
      ~memory_actual:vmm_record.API.vM_metrics_memory_actual
      ~vCPUs_number:vmm_record.API.vM_metrics_VCPUs_number
      ~vCPUs_utilisation:vmm_record.API.vM_metrics_VCPUs_utilisation
      ~vCPUs_CPU:vmm_record.API.vM_metrics_VCPUs_CPU
      ~vCPUs_params:vmm_record.API.vM_metrics_VCPUs_params
      ~vCPUs_flags:vmm_record.API.vM_metrics_VCPUs_flags
      ~state:vmm_record.API.vM_metrics_state
      ~start_time:vmm_record.API.vM_metrics_start_time
      ~install_time:vmm_record.API.vM_metrics_install_time
      ~last_updated:vmm_record.API.vM_metrics_last_updated
      ~other_config:vmm_record.API.vM_metrics_other_config
      ~hvm:vmm_record.API.vM_metrics_hvm
      ~nested_virt:vmm_record.API.vM_metrics_nested_virt
      ~nomigrate:vmm_record.API.vM_metrics_nomigrate
      ~current_domain_type:vmm_record.API.vM_metrics_current_domain_type ;
    state.table <- (x.cls, x.id, Ref.string_of vmm) :: state.table
end

(** If we're restoring VM metadata only then lookup the SR by uuid. If we can't find
    the SR then we will still try to match VDIs later (except CDROMs) *)
module SR : HandlerTools = struct
  type precheck_t =
    | Found_SR of API.ref_SR
    | Found_no_SR
    | Will_use_SR of API.ref_SR
    | SR_not_needed

  let precheck __context config _rpc _session_id _state x =
    let sr_record = API.sR_t_of_rpc x.snapshot in
    match config.import_type with
    | Metadata_import _ -> (
      match
        (* Look up the existing SR record *)
        (* Use an internal DB call - this avoids raising an exception and logging
           the backtrace internally in case of a (reasonably expected) absence of
           the object with this UUID *)
        Db.SR.get_by_uuid_opt ~__context ~uuid:sr_record.API.sR_uuid
      with
      | Some sr ->
          Found_SR sr
      | None ->
          let msg =
            match sr_record.API.sR_content_type with
            | "iso" ->
                "- will eject disk"
                (* Will be handled specially in handle_vdi *)
            | _ ->
                "- will still try to find individual VDIs"
          in
          warn "Failed to find SR with UUID: %s content-type: %s %s"
            sr_record.API.sR_uuid sr_record.API.sR_content_type msg ;
          Found_no_SR
    )
    | Full_import sr ->
        if sr_record.API.sR_content_type = "iso" then
          SR_not_needed (* this one will be ejected *)
        else
          Will_use_SR sr

  let handle_dry_run __context _config _rpc _session_id state x precheck_result
      =
    match precheck_result with
    | Found_SR sr | Will_use_SR sr ->
        state.table <- (x.cls, x.id, Ref.string_of sr) :: state.table
    | Found_no_SR | SR_not_needed ->
        ()

  let handle = handle_dry_run
end

(** If we're restoring VM metadata only then lookup the VDI by uuid.
    If restoring metadata only: lookup the VDI by location, falling back to content_id if available.
    If importing everything: create a new VDI in the SR

    On any error:
    If the SR cannot be found then we skip this VDI.
    If the SR can be found AND is an iso SR then we attempt to lookup the VDI by name_label
    If the SR can be found AND is not an iso SR then we attempt to create the VDI in it *)
module VDI : HandlerTools = struct
  type precheck_t =
    | Found_iso of API.ref_VDI
    | Found_no_iso
    | Found_disk of API.ref_VDI
    | Found_no_disk of string * exn
    | Skip
    | Create of API.vDI_t

  let precheck __context config rpc session_id state x =
    let vdi_record = API.vDI_t_of_rpc x.snapshot in
    let original_sr =
      API.sR_t_of_rpc
        (find_in_export (Ref.string_of vdi_record.API.vDI_SR) state.export)
    in
    if original_sr.API.sR_content_type = "iso" then (
      (* Best effort: locate a VDI in any shared ISO SR with a matching VDI.location *)
      let iso_srs =
        List.filter
          (fun self ->
            Client.SR.get_content_type ~rpc ~session_id ~self = "iso"
            && Client.SR.get_type ~rpc ~session_id ~self <> "udev"
          )
          (Client.SR.get_all ~rpc ~session_id)
      in
      match
        List.filter
          (fun (_, vdir) ->
            vdir.API.vDI_location = vdi_record.API.vDI_location
            && List.mem vdir.API.vDI_SR iso_srs
          )
          (Client.VDI.get_all_records ~rpc ~session_id)
        |> choose_one
      with
      | Some (vdi, _) ->
          Found_iso vdi
      | None ->
          warn "Found no ISO VDI with location = %s; attempting to eject"
            vdi_record.API.vDI_location ;
          Found_no_iso
    ) else
      match config.import_type with
      | Metadata_import {vdi_map; _} -> (
          let mapto =
            if
              List.mem_assoc Constants.storage_migrate_vdi_map_key
                vdi_record.API.vDI_other_config
            then
              Some
                (Ref.of_string
                   (List.assoc Constants.storage_migrate_vdi_map_key
                      vdi_record.API.vDI_other_config
                   )
                )
            else
              None
          in
          let vdi_records = Client.VDI.get_all_records ~rpc ~session_id in
          let find_by_sr_and_location sr location =
            vdi_records
            |> List.filter (fun (_, vdir) ->
                   vdir.API.vDI_location = location && vdir.API.vDI_SR = sr
               )
            |> choose_one
            |> Option.map fst
          in
          let find_by_uuid uuid =
            vdi_records
            |> List.filter (fun (_, vdir) -> vdir.API.vDI_uuid = uuid)
            |> choose_one
            |> Option.map fst
          in
          let _scsiid = "SCSIid" in
          let scsiid_of vdi_record =
            if List.mem_assoc _scsiid vdi_record.API.vDI_sm_config then
              Some (List.assoc _scsiid vdi_record.API.vDI_sm_config)
            else
              None
          in
          let find_by_scsiid x =
            vdi_records
            |> List.filter_map (fun (rf, vdir) ->
                   if scsiid_of vdir = Some x then Some (rf, vdir) else None
               )
            |> choose_one
          in
          let by_vdi_map =
            (* Look up the mapping by both uuid and SCSIid *)
            match
              if List.mem_assoc vdi_record.API.vDI_uuid vdi_map then
                Some (List.assoc vdi_record.API.vDI_uuid vdi_map)
              else
                match scsiid_of vdi_record with
                | None ->
                    None
                | Some x ->
                    if List.mem_assoc x vdi_map then
                      Some (List.assoc x vdi_map)
                    else
                      None
            with
            | Some destination -> (
              match find_by_uuid destination with
              | Some x ->
                  Some x
              | None -> (
                match find_by_scsiid destination with
                | Some (rf, rc) ->
                    info "VDI %s (SCSIid %s) mapped to %s (SCSIid %s) by user"
                      vdi_record.API.vDI_uuid
                      (Option.value ~default:"None" (scsiid_of vdi_record))
                      rc.API.vDI_uuid
                      (Option.value ~default:"None" (scsiid_of rc)) ;
                    Some rf
                | None ->
                    None
              )
            )
            | None -> (
              match scsiid_of vdi_record with
              | None ->
                  None
              | Some x -> (
                match find_by_scsiid x with
                | Some (rf, rc) ->
                    info "VDI %s (SCSIid %s) mapped to %s (SCSIid %s) by user"
                      vdi_record.API.vDI_uuid
                      (Option.value ~default:"None" (scsiid_of vdi_record))
                      rc.API.vDI_uuid
                      (Option.value ~default:"None" (scsiid_of rc)) ;
                    Some rf
                | None ->
                    None
              )
            )
          in
          match by_vdi_map with
          | Some vdi ->
              Found_disk vdi
          | None -> (
            match
              if exists vdi_record.API.vDI_SR state.table then
                let sr = lookup vdi_record.API.vDI_SR state.table in
                match
                  find_by_sr_and_location sr vdi_record.API.vDI_location
                with
                | Some x ->
                    Some x
                | None ->
                    mapto
              else
                mapto
            with
            | Some vdi ->
                Found_disk vdi
            | None ->
                let error_string =
                  Printf.sprintf "Found no VDI with location = %s%s"
                    vdi_record.API.vDI_location
                    ( if config.force then
                        ": ignoring error because '--force' is set"
                      else
                        ""
                    )
                in
                if config.force then (
                  warn "%s" error_string ; Skip
                ) else if exists vdi_record.API.vDI_SR state.table then
                  let sr = lookup vdi_record.API.vDI_SR state.table in
                  Found_no_disk
                    ( error_string
                    , Api_errors.Server_error
                        ( Api_errors.vdi_location_missing
                        , [Ref.string_of sr; vdi_record.API.vDI_location]
                        )
                    )
                else
                  Found_no_disk
                    ( error_string
                    , Api_errors.Server_error
                        (Api_errors.vdi_content_id_missing, [])
                    )
          )
        )
      | Full_import _ ->
          Create vdi_record

  let handle_dry_run __context config _rpc _session_id state x precheck_result =
    match precheck_result with
    | Found_iso vdi | Found_disk vdi ->
        state.table <- (x.cls, x.id, Ref.string_of vdi) :: state.table
    | Found_no_iso ->
        () (* VDI will be ejected. *)
    | Found_no_disk (error_string, e) -> (
      match config.import_type with
      | Metadata_import {live= true; _} ->
          (* We expect the disk to be missing during a live migration dry run. *)
          info
            "Ignoring missing disk %s - this will be mirrored during a real \
             live migration. (Suppressed error: '%s')"
            x.id error_string ;
          (* Create a dummy disk in the state table so the VBD import has a disk to look up. *)
          let dummy_vdi = Ref.make () in
          state.table <- (x.cls, x.id, Ref.string_of dummy_vdi) :: state.table
      | _ ->
          error "%s - treating as fatal and abandoning import" error_string ;
          raise e
    )
    | Skip ->
        ()
    | Create _ ->
        let dummy_vdi = Ref.make () in
        state.table <- (x.cls, x.id, Ref.string_of dummy_vdi) :: state.table

  let handle __context config rpc session_id state x precheck_result =
    match precheck_result with
    | Found_iso _ | Found_no_iso | Skip ->
        handle_dry_run __context config rpc session_id state x precheck_result
    | Found_disk vdi ->
        handle_dry_run __context config rpc session_id state x precheck_result ;
        let other_config_record =
          (API.vDI_t_of_rpc x.snapshot).API.vDI_other_config
        in
        List.iter
          (fun key ->
            Db.VDI.remove_from_other_config ~__context ~self:vdi ~key ;
            try
              Db.VDI.add_to_other_config ~__context ~self:vdi ~key
                ~value:(List.assoc key other_config_record)
            with Not_found -> ()
          )
          Xapi_globs.vdi_other_config_sync_keys
    | Found_no_disk (error_string, e) ->
        error "%s - treating as fatal and abandoning import" error_string ;
        raise e
    | Create vdi_record ->
        (* Make a new VDI for streaming data into; adding task-id to sm-config on VDI.create so SM backend can see this is an import *)
        let sr = lookup vdi_record.API.vDI_SR state.table in
        let task_id = Ref.string_of (Context.get_task_id __context) in
        let sm_config =
          List.filter
            (fun (k, _) -> k <> Xapi_globs.import_task)
            vdi_record.API.vDI_sm_config
        in
        let sm_config = (Xapi_globs.import_task, task_id) :: sm_config in
        let vdi =
          Client.VDI.create_from_record ~rpc ~session_id
            ~value:
              {vdi_record with API.vDI_SR= sr; API.vDI_sm_config= sm_config}
        in
        state.cleanup <-
          (fun __context rpc session_id ->
            Client.VDI.destroy ~rpc ~session_id ~self:vdi
          )
          :: state.cleanup ;
        state.table <- (x.cls, x.id, Ref.string_of vdi) :: state.table
end

(** Lookup the network by name_label only. Previously we used UUID which worked if importing
    to the same host that originated the export but would fail if the network UUID had changed
    even if (from the user's PoV) the "backend network" had not. Since we don't model networks
    it seems less confusing to match on names: whether networks are the same or different is then
    under the control of the user. *)
module Net : HandlerTools = struct
  type precheck_t = Found_net of API.ref_network | Create of API.network_t

  let precheck __context _config rpc session_id _state x =
    let net_record = API.network_t_of_rpc x.snapshot in
    let possibilities =
      Client.Network.get_by_name_label ~rpc ~session_id
        ~label:net_record.API.network_name_label
    in
    match possibilities with
    | [] -> (
        (* Lookup by bridge name as fallback *)
        let expr =
          "field \"bridge\"=\"" ^ net_record.API.network_bridge ^ "\""
        in
        let nets =
          Client.Network.get_all_records_where ~rpc ~session_id ~expr
        in
        match nets with
        | [] ->
            Create net_record
        | (net, _) :: _ ->
            Found_net net
      )
    | n :: _ ->
        Found_net n

  let handle_dry_run __context _config _rpc _session_id state x precheck_result
      =
    match precheck_result with
    | Found_net net ->
        state.table <- (x.cls, x.id, Ref.string_of net) :: state.table
    | Create _ ->
        let dummy_net = Ref.make () in
        state.table <- (x.cls, x.id, Ref.string_of dummy_net) :: state.table

  let handle __context config rpc session_id state x precheck_result =
    match precheck_result with
    | Found_net _ ->
        handle_dry_run __context config rpc session_id state x precheck_result
    | Create net_record ->
        let net =
          log_reraise
            ("failed to create Network with name_label "
            ^ net_record.API.network_name_label
            )
            (fun value ->
              Client.Network.create_from_record ~rpc ~session_id ~value
            )
            net_record
        in
        (* Only add task flag to networks which get created in this import *)
        TaskHelper.operate_on_db_task ~__context (fun t ->
            ( try
                Db.Network.remove_from_other_config ~__context ~self:net
                  ~key:Xapi_globs.import_task
              with _ -> ()
            ) ;
            Db.Network.add_to_other_config ~__context ~self:net
              ~key:Xapi_globs.import_task ~value:(Ref.string_of t)
        ) ;
        state.cleanup <-
          (fun __context rpc session_id ->
            Client.Network.destroy ~rpc ~session_id ~self:net
          )
          :: state.cleanup ;
        state.table <- (x.cls, x.id, Ref.string_of net) :: state.table
end

(** Lookup the GPU group by GPU_types only. Currently, the GPU_types field contains the prototype
 *  of just a single pGPU. We would probably have to extend this function once we support GPU groups
 *  for multiple compatible GPU types. *)
module GPUGroup : HandlerTools = struct
  type precheck_t =
    | Found_GPU_group of API.ref_GPU_group
    | Found_no_GPU_group of exn
    | Create of API.gPU_group_t

  let precheck __context config rpc session_id _state x =
    let gpu_group_record = API.gPU_group_t_of_rpc x.snapshot in
    let groups = Client.GPU_group.get_all_records ~rpc ~session_id in
    try
      let group, _ =
        List.find
          (fun (_, groupr) ->
            groupr.API.gPU_group_GPU_types
            = gpu_group_record.API.gPU_group_GPU_types
          )
          groups
      in
      Found_GPU_group group
    with Not_found -> (
      match config.import_type with
      | Metadata_import _ ->
          (* In vm_metadata_only mode the GPU group must exist *)
          let msg =
            Printf.sprintf
              "Unable to find GPU group with matching GPU_types = '[%s]'"
              (String.concat "," gpu_group_record.API.gPU_group_GPU_types)
          in
          error "%s" msg ; Found_no_GPU_group (Failure msg)
      | Full_import _ ->
          (* In normal mode we attempt to create any missing GPU groups *)
          Create gpu_group_record
    )

  let handle_dry_run __context _config _rpc _session_id state x precheck_result
      =
    match precheck_result with
    | Found_GPU_group group ->
        state.table <- (x.cls, x.id, Ref.string_of group) :: state.table
    | Found_no_GPU_group e ->
        raise e
    | Create _ ->
        let dummy_gpu_group = Ref.make () in
        state.table <-
          (x.cls, x.id, Ref.string_of dummy_gpu_group) :: state.table

  let handle __context config rpc session_id state x precheck_result =
    match precheck_result with
    | Found_GPU_group _ | Found_no_GPU_group _ ->
        handle_dry_run __context config rpc session_id state x precheck_result
    | Create gpu_group_record ->
        let group =
          log_reraise
            ("Unable to create GPU group with GPU_types = '[%s]'"
            ^ String.concat "," gpu_group_record.API.gPU_group_GPU_types
            )
            (fun value ->
              let group =
                Client.GPU_group.create ~rpc ~session_id
                  ~name_label:value.API.gPU_group_name_label
                  ~name_description:value.API.gPU_group_name_description
                  ~other_config:value.API.gPU_group_other_config
              in
              Db.GPU_group.set_GPU_types ~__context ~self:group
                ~value:value.API.gPU_group_GPU_types ;
              group
            )
            gpu_group_record
        in
        (* Only add task flag to GPU groups which get created in this import *)
        TaskHelper.operate_on_db_task ~__context (fun t ->
            ( try
                Db.GPU_group.remove_from_other_config ~__context ~self:group
                  ~key:Xapi_globs.import_task
              with _ -> ()
            ) ;
            Db.GPU_group.add_to_other_config ~__context ~self:group
              ~key:Xapi_globs.import_task ~value:(Ref.string_of t)
        ) ;
        state.cleanup <-
          (fun __context rpc session_id ->
            Client.GPU_group.destroy ~rpc ~session_id ~self:group
          )
          :: state.cleanup ;
        state.table <- (x.cls, x.id, Ref.string_of group) :: state.table
end

(** Create a new VBD record, add the reference to the table.
    The VM and VDI must already have been handled first.
    If the VDI doesn't exist and the VBD is a CDROM then eject it.
    Note that any currently attached disk MUST be present, unless it's an HVM guest and a
    CDROM in which case we eject it anyway.
*)
module VBD : HandlerTools = struct
  type precheck_t = Found_VBD of API.ref_VBD | Skip | Create of API.vBD_t

  let precheck __context config _rpc _session_id state x =
    let vbd_record = API.vBD_t_of_rpc x.snapshot in
    let get_vbd () =
      (* Use an internal DB call - this avoids raising an exception and logging
         the backtrace internally in case of a (reasonably expected) absence of
         the object with this UUID *)
      Db.VBD.get_by_uuid_opt ~__context ~uuid:vbd_record.API.vBD_uuid
    in
    let vbd_opt =
      (* If there's already a VBD with the same UUID and we're preserving UUIDs, use that one. *)
      if config.full_restore then (
        match get_vbd () with
        | Some x ->
            Some x
        | None ->
            info
              "Did not find an already existing VBD with the same uuid=%s, try \
               to create a new one"
              vbd_record.API.vBD_uuid ;
            None
      ) else
        None
    in
    match vbd_opt with
    | Some vbd ->
        Found_VBD vbd
    | None -> (
        let vm =
          log_reraise
            ("Failed to find VBD's VM: " ^ Ref.string_of vbd_record.API.vBD_VM)
            (lookup vbd_record.API.vBD_VM)
            state.table
        in
        (* If the VBD is supposed to be attached to a PV guest (which doesn't support
           currently_attached empty drives) then throw a fatal error. *)
        let original_vm =
          get_vm_record
            (find_in_export (Ref.string_of vbd_record.API.vBD_VM) state.export)
        in
        (* Note: the following is potentially inaccurate: the find out whether a running or
         * suspended VM has booted HVM, we must consult the VM metrics, but those aren't
         * available in the exported metadata. *)
        let has_qemu = Helpers.will_have_qemu_from_record original_vm in
        (* In the case of dry_run live migration, don't check for
           missing disks as CDs will be ejected before the real migration. *)
        let dry_run, live =
          match config.import_type with
          | Metadata_import {dry_run; live; _} ->
              (dry_run, live)
          | _ ->
              (false, false)
        in
        ( if
            vbd_record.API.vBD_currently_attached
            && not (exists vbd_record.API.vBD_VDI state.table)
          then
            (* It's only ok if it's a CDROM attached to an HVM guest, or it's part of SXM and we know the sender would eject it. *)
            let will_eject =
              dry_run && live && original_vm.API.vM_power_state <> `Suspended
            in
            if not (vbd_record.API.vBD_type = `CD && (has_qemu || will_eject))
            then
              raise (IFailure Attached_disks_not_found)
        ) ;
        let vbd_record = {vbd_record with API.vBD_VM= vm} in
        match
          (vbd_record.API.vBD_type, exists vbd_record.API.vBD_VDI state.table)
        with
        | `CD, false | `Floppy, false ->
            if has_qemu || original_vm.API.vM_power_state <> `Suspended then
              Create
                {vbd_record with API.vBD_VDI= Ref.null; API.vBD_empty= true}
            (* eject *)
            else
              Create vbd_record
        | `Disk, false ->
            (* omit: cannot have empty disks *)
            warn
              "Cannot import VM's disk: was it an .iso attached as a disk \
               rather than CD?" ;
            Skip
        | _, true ->
            Create
              {
                vbd_record with
                API.vBD_VDI= lookup vbd_record.API.vBD_VDI state.table
              }
      )

  let handle_dry_run __context _config _rpc _session_id state x precheck_result
      =
    match precheck_result with
    | Found_VBD vbd ->
        state.table <- (x.cls, x.id, Ref.string_of vbd) :: state.table ;
        state.table <-
          (x.cls, Ref.string_of vbd, Ref.string_of vbd) :: state.table
    | Skip ->
        ()
    | Create _ ->
        let dummy_vbd = Ref.make () in
        state.table <- (x.cls, x.id, Ref.string_of dummy_vbd) :: state.table

  let handle __context config rpc session_id state x precheck_result =
    match precheck_result with
    | Found_VBD _ | Skip ->
        handle_dry_run __context config rpc session_id state x precheck_result
    | Create vbd_record ->
        let vbd =
          log_reraise "failed to create VBD"
            (fun value ->
              let vbd =
                Client.VBD.create_from_record ~rpc ~session_id
                  ~value:
                    {
                      value with
                      API.vBD_device= ""
                    ; API.vBD_currently_attached= false
                    }
              in
              if config.full_restore then
                Db.VBD.set_uuid ~__context ~self:vbd ~value:value.API.vBD_uuid ;
              vbd
            )
            vbd_record
        in
        state.cleanup <-
          (fun __context rpc session_id ->
            Client.VBD.destroy ~rpc ~session_id ~self:vbd
          )
          :: state.cleanup ;
        (* Now that we can import/export suspended VMs we need to preserve the
           			   currently_attached flag *)
        Db.VBD.set_currently_attached ~__context ~self:vbd
          ~value:vbd_record.API.vBD_currently_attached ;
        state.table <- (x.cls, x.id, Ref.string_of vbd) :: state.table
end

(** Create a new VIF record, add the reference to the table.
    The VM and Network must have already been handled first. *)
module VIF : HandlerTools = struct
  type precheck_t = Found_VIF of API.ref_VIF | Create of API.vIF_t

  let precheck __context config _rpc _session_id state x =
    let vif_record = API.vIF_t_of_rpc x.snapshot in
    let get_vif () =
      (* Use an internal DB call - this avoids raising an exception and logging
         the backtrace internally in case of a (reasonably expected) absence of
         the object with this UUID *)
      Db.VIF.get_by_uuid_opt ~__context ~uuid:vif_record.API.vIF_uuid
    in
    let vif_opt =
      if config.full_restore then (
        (* If there's already a VIF with the same UUID and we're preserving UUIDs, use that one. *)
        match get_vif () with
        | Some x ->
            Some x
        | None ->
            info
              "Did not find an already existing VIF with the same uuid=%s, try \
               to create a new one"
              vif_record.API.vIF_uuid ;
            None
      ) else
        None
    in
    match vif_opt with
    | Some vif ->
        Found_VIF vif
    | None ->
        (* If not restoring a full backup then blank the MAC so it is regenerated *)
        let vif_record =
          {
            vif_record with
            API.vIF_MAC=
              (if config.full_restore then vif_record.API.vIF_MAC else "")
          }
        in
        (* Determine the VM to which we're going to attach this VIF. *)
        let vm =
          log_reraise
            ("Failed to find VIF's VM: " ^ Ref.string_of vif_record.API.vIF_VM)
            (lookup vif_record.API.vIF_VM)
            state.table
        in
        (* Determine the network to which we're going to attach this VIF. *)
        let net =
          (* If we find the cross-pool migration key, attach the VIF to that network... *)
          if
            List.mem_assoc Constants.storage_migrate_vif_map_key
              vif_record.API.vIF_other_config
          then
            Ref.of_string
              (List.assoc Constants.storage_migrate_vif_map_key
                 vif_record.API.vIF_other_config
              )
          else
            (* ...otherwise fall back to looking up the network from the state table. *)
            log_reraise
              ("Failed to find VIF's Network: "
              ^ Ref.string_of vif_record.API.vIF_network
              )
              (lookup vif_record.API.vIF_network)
              state.table
        in
        (* Make sure we remove the cross-pool migration VIF mapping key from the other_config
         * before creating a VIF - otherwise we'll risk sending this key on to another pool
         * during a future cross-pool migration and it won't make sense. *)
        let other_config =
          List.filter
            (fun (k, _) -> k <> Constants.storage_migrate_vif_map_key)
            vif_record.API.vIF_other_config
        in
        (* Construct the VIF record we're going to try to create locally. *)
        let vif_record =
          if Pool_features.is_enabled ~__context Features.VIF_locking then
            vif_record
          else if vif_record.API.vIF_locking_mode = `locked then
            {
              vif_record with
              API.vIF_locking_mode= `network_default
            ; API.vIF_ipv4_allowed= []
            ; API.vIF_ipv6_allowed= []
            }
          else
            {vif_record with API.vIF_ipv4_allowed= []; API.vIF_ipv6_allowed= []}
        in
        let vif_record =
          {
            vif_record with
            API.vIF_VM= vm
          ; API.vIF_network= net
          ; API.vIF_other_config= other_config
          }
        in
        Create vif_record

  let handle_dry_run __context _config _rpc _session_id state x precheck_result
      =
    match precheck_result with
    | Found_VIF vif ->
        state.table <- (x.cls, x.id, Ref.string_of vif) :: state.table ;
        state.table <-
          (x.cls, Ref.string_of vif, Ref.string_of vif) :: state.table
    | Create _ ->
        let dummy_vif = Ref.make () in
        state.table <- (x.cls, x.id, Ref.string_of dummy_vif) :: state.table

  let handle __context config rpc session_id state x precheck_result =
    match precheck_result with
    | Found_VIF _ ->
        handle_dry_run __context config rpc session_id state x precheck_result
    | Create vif_record ->
        let vif =
          log_reraise "failed to create VIF"
            (fun value ->
              let vif =
                Client.VIF.create_from_record ~rpc ~session_id
                  ~value:{value with API.vIF_currently_attached= false}
              in
              if config.full_restore then
                Db.VIF.set_uuid ~__context ~self:vif ~value:value.API.vIF_uuid ;
              vif
            )
            vif_record
        in
        state.cleanup <-
          (fun __context rpc session_id ->
            Client.VIF.destroy ~rpc ~session_id ~self:vif
          )
          :: state.cleanup ;
        (* Now that we can import/export suspended VMs we need to preserve the
           				 currently_attached flag *)
        if
          Db.VM.get_power_state ~__context ~self:vif_record.API.vIF_VM
          <> `Halted
        then
          Db.VIF.set_currently_attached ~__context ~self:vif
            ~value:vif_record.API.vIF_currently_attached ;
        state.table <- (x.cls, x.id, Ref.string_of vif) :: state.table
end

module VGPUType : HandlerTools = struct
  type precheck_t =
    | Found_VGPU_type of API.ref_VGPU_type
    | Create of API.vGPU_type_t

  let precheck __context _config rpc session_id _state x =
    let vgpu_type_record = API.vGPU_type_t_of_rpc x.snapshot in
    (* First look up VGPU types using the identifier string. *)
    let compatible_types =
      match
        Client.VGPU_type.get_all_records_where ~rpc ~session_id
          ~expr:
            (Printf.sprintf "field \"identifier\"=\"%s\""
               vgpu_type_record.API.vGPU_type_identifier
            )
      with
      | [] ->
          (* If that fails, look up using the vendor name and model name. *)
          Client.VGPU_type.get_all_records_where ~rpc ~session_id
            ~expr:
              (Printf.sprintf
                 "field \"vendor_name\"=\"%s\" and field \"model_name\"=\"%s\""
                 vgpu_type_record.API.vGPU_type_vendor_name
                 vgpu_type_record.API.vGPU_type_model_name
              )
      | types ->
          types
    in
    match choose_one compatible_types with
    | Some (vgpu_type, _) ->
        Found_VGPU_type vgpu_type
    | None ->
        warn "Unable to find VGPU_type (%s,%s,%s) - creating a new record"
          vgpu_type_record.API.vGPU_type_identifier
          vgpu_type_record.API.vGPU_type_vendor_name
          vgpu_type_record.API.vGPU_type_model_name ;
        Create vgpu_type_record

  let handle_dry_run __context _config _rpc _session_id state x precheck_result
      =
    match precheck_result with
    | Found_VGPU_type vgpu_type ->
        state.table <- (x.cls, x.id, Ref.string_of vgpu_type) :: state.table ;
        state.table <-
          (x.cls, Ref.string_of vgpu_type, Ref.string_of vgpu_type)
          :: state.table
    | Create _ ->
        let dummy_vgpu_type = Ref.make () in
        state.table <-
          (x.cls, x.id, Ref.string_of dummy_vgpu_type) :: state.table

  let handle __context config rpc session_id state x precheck_result =
    match precheck_result with
    | Found_VGPU_type _ ->
        handle_dry_run __context config rpc session_id state x precheck_result
    | Create vgpu_type_record ->
        let vgpu_type =
          log_reraise "failed to create VGPU_type"
            (fun value ->
              (* size and internal_config are left as defaults for now. They'll
                 						 * be updated if and when xapi comes across the real config file. *)
              Xapi_vgpu_type.create ~__context
                ~vendor_name:value.API.vGPU_type_vendor_name
                ~model_name:value.API.vGPU_type_model_name
                ~framebuffer_size:value.API.vGPU_type_framebuffer_size
                ~max_heads:value.API.vGPU_type_max_heads
                ~max_resolution_x:value.API.vGPU_type_max_resolution_x
                ~max_resolution_y:value.API.vGPU_type_max_resolution_y ~size:0L
                ~internal_config:[]
                ~implementation:value.API.vGPU_type_implementation
                ~identifier:value.API.vGPU_type_identifier
                ~experimental:value.API.vGPU_type_experimental
                ~compatible_model_names_in_vm:
                  value.API.vGPU_type_compatible_types_in_vm
                ~compatible_model_names_on_pgpu:[]
            )
            vgpu_type_record
        in
        state.cleanup <-
          (fun __context _ _ -> Db.VGPU_type.destroy ~__context ~self:vgpu_type)
          :: state.cleanup ;
        state.table <- (x.cls, x.id, Ref.string_of vgpu_type) :: state.table
end

(** Create a new VGPU record, add the reference to the table.
    The VM and GPU_group must have already been handled first. *)
module VGPU : HandlerTools = struct
  type precheck_t = Found_VGPU of API.ref_VGPU | Create of API.vGPU_t

  let precheck __context config _rpc _session_id state x =
    let vgpu_record = API.vGPU_t_of_rpc x.snapshot in
    let get_vgpu () =
      (* Use an internal DB call - this avoids raising an exception and logging
         the backtrace internally in case of a (reasonably expected) absence of
         the object with this UUID *)
      Db.VGPU.get_by_uuid_opt ~__context ~uuid:vgpu_record.API.vGPU_uuid
    in
    let vgpu_opt =
      if config.full_restore then (
        (* If there's already a VGPU with the same UUID and we're preserving UUIDs, use that one. *)
        match get_vgpu () with
        | Some x ->
            Some x
        | None ->
            info
              "Did not find an already existing VGPU with the same uuid=%s, \
               try to create a new one"
              vgpu_record.API.vGPU_uuid ;
            None
      ) else
        None
    in
    match vgpu_opt with
    | Some vgpu ->
        Found_VGPU vgpu
    | None ->
        let vm =
          log_reraise
            ("Failed to find VGPU's VM: "
            ^ Ref.string_of vgpu_record.API.vGPU_VM
            )
            (lookup vgpu_record.API.vGPU_VM)
            state.table
        in
        let group =
          (* If we find the cross-pool migration key, attach the vgpu to the provided gpu_group... *)
          if
            List.mem_assoc Constants.storage_migrate_vgpu_map_key
              vgpu_record.API.vGPU_other_config
          then
            Ref.of_string
              (List.assoc Constants.storage_migrate_vgpu_map_key
                 vgpu_record.API.vGPU_other_config
              )
          else
            (* ...otherwise fall back to looking up the vgpu from the state table. *)
            log_reraise
              ("Failed to find VGPU's GPU group: "
              ^ Ref.string_of vgpu_record.API.vGPU_GPU_group
              )
              (lookup vgpu_record.API.vGPU_GPU_group)
              state.table
        in
        let _type =
          log_reraise
            ("Failed to find VGPU's type: "
            ^ Ref.string_of vgpu_record.API.vGPU_type
            )
            (lookup vgpu_record.API.vGPU_type)
            state.table
        in
        (* Make sure we remove the cross-pool migration VGPU mapping key from the other_config
         * before creating a VGPU - otherwise we'll risk sending this key on to another pool
         * during a future cross-pool migration and it won't make sense. *)
        let other_config =
          List.filter
            (fun (k, _) -> k <> Constants.storage_migrate_vgpu_map_key)
            vgpu_record.API.vGPU_other_config
        in
        let vgpu_record =
          {
            vgpu_record with
            API.vGPU_VM= vm
          ; API.vGPU_GPU_group= group
          ; API.vGPU_type= _type
          ; API.vGPU_other_config= other_config
          }
        in
        if is_live config then
          assert_can_live_import_vgpu ~__context vgpu_record ;
        Create vgpu_record

  let handle_dry_run __context _config _rpc _session_id state x precheck_result
      =
    match precheck_result with
    | Found_VGPU vgpu ->
        state.table <- (x.cls, x.id, Ref.string_of vgpu) :: state.table ;
        state.table <-
          (x.cls, Ref.string_of vgpu, Ref.string_of vgpu) :: state.table
    | Create _ ->
        let dummy_vgpu = Ref.make () in
        state.table <- (x.cls, x.id, Ref.string_of dummy_vgpu) :: state.table

  let handle __context config rpc session_id state x precheck_result =
    match precheck_result with
    | Found_VGPU _ ->
        handle_dry_run __context config rpc session_id state x precheck_result
    | Create vgpu_record ->
        let vgpu =
          log_reraise "failed to create VGPU"
            (fun value ->
              let vgpu =
                Client.VGPU.create ~rpc ~session_id ~vM:value.API.vGPU_VM
                  ~gPU_group:value.API.vGPU_GPU_group
                  ~device:value.API.vGPU_device
                  ~other_config:value.API.vGPU_other_config
                  ~_type:value.API.vGPU_type
              in
              if config.full_restore then
                Db.VGPU.set_uuid ~__context ~self:vgpu
                  ~value:value.API.vGPU_uuid ;
              vgpu
            )
            vgpu_record
        in
        state.cleanup <-
          (fun __context rpc session_id ->
            Client.VGPU.destroy ~rpc ~session_id ~self:vgpu
          )
          :: state.cleanup ;
        (* Now that we can import/export suspended VMs we need to preserve the currently_attached flag *)
        if
          Db.VM.get_power_state ~__context ~self:vgpu_record.API.vGPU_VM
          <> `Halted
        then
          Db.VGPU.set_currently_attached ~__context ~self:vgpu
            ~value:vgpu_record.API.vGPU_currently_attached ;
        state.table <- (x.cls, x.id, Ref.string_of vgpu) :: state.table
end

module PVS_Proxy : HandlerTools = struct
  type precheck_t =
    | Drop (* can't find a PVS Site at destination to use *)
    | Create of API.pVS_proxy_t

  (* find a PVS site of a given [uuid] and [name] with [uuid] taking
     	 * precedence *)
  let find_pvs_site __context _config _rpc _session_id pvs_uuid =
    let sites = Db.PVS_site.get_all_records ~__context in
    let has_uuid (_, site) = site.API.pVS_site_PVS_uuid = pvs_uuid in
    let candidates = List.filter has_uuid sites in
    match candidates with (ref, _) :: _ -> Some ref | [] -> None

  (** We obtain the name and uuid of the PVS site this
      	 * proxy was linked to. Then we use these trying to find
      	 * a matching site on this (destination) side. The result is recorded
      	 * in the [precheck_t] value.
      	 *)
  let precheck __context config rpc session_id state obj =
    let proxy = API.pVS_proxy_t_of_rpc obj.snapshot in
    let site =
      proxy.API.pVS_proxy_site |> fun ref ->
      find_in_export (Ref.string_of ref) state.export |> API.pVS_site_t_of_rpc
    in
    let pvs_uuid = site.API.pVS_site_PVS_uuid in
    match find_pvs_site __context config rpc session_id pvs_uuid with
    | None ->
        Drop
    | Some site ->
        Create
          {
            proxy with
            API.pVS_proxy_site= site
          ; API.pVS_proxy_VIF= lookup proxy.API.pVS_proxy_VIF state.table
          }

  let handle_dry_run __context _config _rpc _session_id state obj = function
    | Drop ->
        debug "no matching PVS Site found for PVS Proxy %s" obj.id
    | Create _ ->
        let dummy = Ref.make () in
        state.table <- (obj.cls, obj.id, Ref.string_of dummy) :: state.table

  let handle __context _config rpc session_id state obj = function
    | Drop ->
        debug "no matching PVS Site found for PVS Proxy %s" obj.id
    | Create p ->
        let proxy =
          Client.PVS_proxy.create ~rpc ~session_id ~site:p.API.pVS_proxy_site
            ~vIF:p.API.pVS_proxy_VIF
        in
        debug "creating PVS Proxy %s btw PVS Site %s <-> %s VIF during import"
          (Ref.string_of proxy)
          (Ref.string_of p.API.pVS_proxy_site)
          (Ref.string_of p.API.pVS_proxy_VIF) ;
        state.cleanup <-
          (fun __context rpc session_id ->
            Client.PVS_proxy.destroy ~rpc ~session_id ~self:proxy
          )
          :: state.cleanup ;
        state.table <- (obj.cls, obj.id, Ref.string_of proxy) :: state.table
end

module PVS_Site : HandlerTools = struct
  (* A PVS site is never re-created as part of the import of a VM that
     	 * refers to it. We just forget it.
  *)

  type precheck_t = unit

  let precheck __context _ _ _ _ _ = ()

  let handle_dry_run __context _ _ _ _ _ () = ()

  let handle __context _ _ _ _ _ () = ()
end

module VTPM : HandlerTools = struct
  (* We expect a VTPM to be part of a VM's meta data and not to exist
     independently. Hence, we never look for the the VTPM to exist
     already but always create it *)

  type precheck_t = Import of vtpm'

  let fail fmt =
    Printf.ksprintf
      (fun msg -> raise Api_errors.(Server_error (import_error_generic, [msg])))
      fmt

  let colliding ~__context ~uuid =
    try
      ignore (Db.VTPM.get_by_uuid ~__context ~uuid) ;
      true
    with
    | Not_found ->
        false
    | e ->
        debug "%s: VTPM %s not found: %s" __FUNCTION__ uuid
          (Printexc.to_string e) ;
        false

  (* Compare the state of the database with the metadata to be imported.
     Returns a result which signals what we should do to import the
     metadata. *)

  let precheck __context config _rpc _session _state obj =
    let vtpm' = vtpm'_of_rpc obj.snapshot in
    debug "%s: importing VTPM" __FUNCTION__ ;
    match config.full_restore with
    | false ->
        Import vtpm'
    | true ->
        let uuid = vtpm'.vTPM'_uuid in
        if colliding ~__context ~uuid then
          fail "A VTPM with UUID %s already exists" uuid
        else
          Import vtpm'

  (* Handle the result of the precheck function, but don't create any
     database objects.  Add objects to the state table if necessary, to
     keep track of what would have been imported.*)
  let handle_dry_run __context _config _rpc _session state obj = function
    | Import _vtpm' ->
        debug "%s: importing VTPM" __FUNCTION__ ;
        let ref = Ref.make () in
        state.table <- (obj.cls, obj.id, Ref.string_of ref) :: state.table

  (* Handle the result of the check function, creating database objects
     if necessary.  For certain combinations of result and object type,
     this can be aliased to handle_dry_run. *)
  let handle __context config _rpc _session state _obj = function
    | Import vtpm' -> (
        debug "%s: importing VTPM" __FUNCTION__ ;
        let vm =
          log_reraise
            ("Failed to find VTPM's VM: " ^ Ref.string_of vtpm'.vTPM'_VM)
            (lookup vtpm'.vTPM'_VM) state.table
        in
        let self =
          Xapi_vtpm.import ~__context ~vM:vm ~is_unique:vtpm'.vTPM'_is_unique
        in
        (* there is no API to set the protected field *)
        Db.VTPM.set_is_protected ~__context ~self
          ~value:vtpm'.vTPM'_is_protected ;
        Xapi_vtpm.set_contents ~__context ~self ~contents:vtpm'.vTPM'_content ;
        match config.full_restore with
        | false ->
            (* we get a new UUID *)
            ()
        | true ->
            (* we restore the UUID; precheck made sure this is not
               colliding *)
            Db.VTPM.set_uuid ~__context ~self ~value:vtpm'.vTPM'_uuid
      )
end

(** Create a handler for each object type. *)
module HostHandler = MakeHandler (Host)

module SRHandler = MakeHandler (SR)
module VDIHandler = MakeHandler (VDI)
module GuestMetricsHandler = MakeHandler (GuestMetrics)
module MetricsHandler = MakeHandler (Metrics)
module VMHandler = MakeHandler (VM)
module NetworkHandler = MakeHandler (Net)
module GPUGroupHandler = MakeHandler (GPUGroup)
module VBDHandler = MakeHandler (VBD)
module VIFHandler = MakeHandler (VIF)
module VGPUTypeHandler = MakeHandler (VGPUType)
module VGPUHandler = MakeHandler (VGPU)
module PVS_SiteHandler = MakeHandler (PVS_Site)
module PVS_ProxyHandler = MakeHandler (PVS_Proxy)
module VTPMHandler = MakeHandler (VTPM)

(** Table mapping datamodel class names to handlers, in order we have to run them *)
let handlers =
  [
    (Datamodel_common._host, HostHandler.handle)
  ; (Datamodel_common._sr, SRHandler.handle)
  ; (Datamodel_common._vdi, VDIHandler.handle)
  ; (Datamodel_common._vm_guest_metrics, GuestMetricsHandler.handle)
  ; (Datamodel_common._vm_metrics, MetricsHandler.handle)
  ; (Datamodel_common._vm, VMHandler.handle)
  ; (Datamodel_common._network, NetworkHandler.handle)
  ; (Datamodel_common._gpu_group, GPUGroupHandler.handle)
  ; (Datamodel_common._vbd, VBDHandler.handle)
  ; (Datamodel_common._vif, VIFHandler.handle)
  ; (Datamodel_common._vgpu_type, VGPUTypeHandler.handle)
  ; (Datamodel_common._vgpu, VGPUHandler.handle)
  ; (Datamodel_common._pvs_site, PVS_SiteHandler.handle)
  ; (Datamodel_common._pvs_proxy, PVS_ProxyHandler.handle)
  ; (Datamodel_common._vtpm, VTPMHandler.handle)
  ]

let update_snapshot_and_parent_links ~__context state =
  let aux (cls, _, ref) =
    let ref = Ref.of_string ref in
    ( if
        cls = Datamodel_common._vm
        && Db.VM.get_is_a_snapshot ~__context ~self:ref
      then
        let snapshot_of = Db.VM.get_snapshot_of ~__context ~self:ref in
        if snapshot_of <> Ref.null then (
          debug "lookup for snapshot_of = '%s'" (Ref.string_of snapshot_of) ;
          log_reraise
            ("Failed to find the VM which is snapshot of "
            ^ Db.VM.get_name_label ~__context ~self:ref
            )
            (fun table ->
              let snapshot_of = (lookup snapshot_of) table in
              Db.VM.set_snapshot_of ~__context ~self:ref ~value:snapshot_of
            )
            state.table
        )
    ) ;
    if cls = Datamodel_common._vm then (
      let parent = Db.VM.get_parent ~__context ~self:ref in
      debug "lookup for parent = '%s'" (Ref.string_of parent) ;
      try
        let parent = lookup parent state.table in
        Db.VM.set_parent ~__context ~self:ref ~value:parent
      with _ -> debug "no parent found"
    )
  in
  List.iter aux state.table

(** Take a list of objects, lookup the handlers by class name and 'handle' them *)
let handle_all __context config rpc session_id (xs : obj list) =
  let state = initial_state xs in
  try
    let one_type (cls, handler) =
      let instances = List.filter (fun x -> x.cls = cls) xs in
      debug "Importing %i %s(s)" (List.length instances) cls ;
      List.iter
        (fun x -> handler __context config rpc session_id state x)
        instances
    in
    List.iter one_type handlers ;
    let dry_run =
      match config.import_type with
      | Metadata_import {dry_run= true; _} ->
          true
      | _ ->
          false
    in
    if not dry_run then
      update_snapshot_and_parent_links ~__context state ;
    state
  with e ->
    Backtrace.is_important e ;
    error "Caught exception in import: %s" (ExnHelper.string_of_exn e) ;
    (* execute all the cleanup actions now *)
    if config.force then
      warn "Not cleaning up after import failure since --force provided: %s"
        (ExnHelper.string_of_exn e)
    else
      cleanup state.cleanup ;
    raise e

(** Read the next file in the archive as xml *)
let read_xml hdr fd =
  Unixext.really_read_string fd (Int64.to_int hdr.Tar.Header.file_size)

let assert_filename_is hdr =
  let expected = Xapi_globs.ova_xml_filename in
  let actual = hdr.Tar.Header.file_name in
  if expected <> actual then (
    let hex = Tar.Header.to_hex in
    error "import expects the next file in the stream to be [%s]; got [%s]"
      (hex expected) (hex actual) ;
    raise (IFailure (Unexpected_file (expected, actual)))
  )

(* We want to read tar headers from a file descriptor (possibly a socket/pipe),
   and retry with decompression if that fails.
   To be able to do that we store the first block that we read, and try to parse it with Tar first,
   and if that fails try zstd/gzip.

   We need a Tar HeaderReader that reads either from a buffer, and then continues reading from an FD
*)
module BufferOrFile = struct
  type in_channel = {
      mutable buffer: Cstruct.t option
    ; fd: Unix.file_descr
    ; retry_with_compression: bool ref
  }

  type 'a t = 'a

  let make retry_with_compression buffer fd =
    {buffer= Some buffer; retry_with_compression; fd}

  let really_read t dest =
    match t.buffer with
    | Some first ->
        t.buffer <- None ;
        assert (Cstruct.length dest = Cstruct.length first) ;
        Cstruct.blit first 0 dest 0 (Cstruct.length first)
    | None ->
        (* if we've successfully parsed the first block as a tar header, then don't retry:
           we are going to read more from the file anyway.
        *)
        t.retry_with_compression := false ;
        Tar_unix.really_read t.fd dest

  let skip t n =
    assert (Option.is_none t.buffer) ;
    (* while reading headers we only skip small distances *)
    let buf = Cstruct.create n in
    (* can't seek, because the FD might be a socket/pipe *)
    Tar_unix.really_read t.fd buf
end

module Direct = struct
  type 'a t = 'a

  let ( >>= ) x f = f x

  let return x = x
end

module TarHeaderReader = Tar.HeaderReader (Direct) (BufferOrFile)

(** Takes an fd and a function, tries first to read the first tar block
    and checks for the existence of 'ova.xml'. If that fails then pipe
    the lot through an appropriate decompressor and try again *)
let with_open_archive fd ?length f =
  (* Read the first header's worth into a buffer *)
  let buffer = Cstruct.create Tar.Header.length in
  let retry_with_compression = ref true in
  try
    Tar_unix.really_read fd buffer ;
    (* we assume the first block is not all zeroes *)
    let hdr =
      TarHeaderReader.read (BufferOrFile.make retry_with_compression buffer fd)
      |> Result.get_ok
    in
    assert_filename_is hdr ;
    (* successfully opened uncompressed stream *)
    retry_with_compression := false ;
    let xml = read_xml hdr fd in
    Tar_helpers.skip fd (Tar.Header.compute_zero_padding_length hdr) ;
    f xml fd
  with e ->
    if not !retry_with_compression then raise e ;
    let decompress =
      (* If the file starts with the zstd magic string decompress with zstd;
         otherwise fall back to trying gzip. *)
      let zstd_magic = "\x28\xb5\x2f\xfd" in
      let zstd =
        Cstruct.equal
          (Cstruct.of_string zstd_magic)
          (Cstruct.sub buffer 0 (String.length zstd_magic))
      in
      if zstd then (
        debug "Failed to directly open the archive; trying zstd" ;
        Zstd.Default.decompress
      ) else (
        debug "Failed to directly open the archive; trying gzip" ;
        Gzip.Default.decompress
      )
    in
    let feeder pipe_in =
      finally
        (fun () ->
          decompress pipe_in (fun compressed_in ->
              (* Write the initial buffer *)
              Unix.set_close_on_exec compressed_in ;
              debug "Writing initial buffer" ;
              Tar_unix.really_write compressed_in buffer ;
              let limit =
                Option.map
                  (fun x -> Int64.sub x (Int64.of_int Tar.Header.length))
                  length
              in
              let n = Unixext.copy_file ?limit fd compressed_in in
              debug "Written a total of %d + %Ld bytes" Tar.Header.length n
          )
        )
        (fun () -> ignore_exn (fun () -> Unix.close pipe_in))
    in
    let consumer pipe_out feeder_t =
      finally
        (fun () ->
          let hdr = Tar_unix.get_next_header pipe_out in
          assert_filename_is hdr ;
          let xml = read_xml hdr pipe_out in
          Tar_helpers.skip pipe_out (Tar.Header.compute_zero_padding_length hdr) ;
          f xml pipe_out
        )
        (fun () ->
          ignore_exn (fun () -> Unix.close pipe_out) ;
          Thread.join feeder_t
        )
    in
    let pipe_out, pipe_in = Unix.pipe () in
    let feeder_t = Thread.create feeder pipe_in in
    consumer pipe_out feeder_t

(** Remove "import" from the current operations of all created VMs, complete the
    task including the VM references *)
let complete_import ~__context vmrefs =
  debug "length of vmrefs: %d" (List.length vmrefs) ;
  debug "content: %s" (String.concat "," (List.map Ref.string_of vmrefs)) ;
  try
    (* Remove the "import" current operation, recompute allowed operations *)
    let task_id = Ref.string_of (Context.get_task_id __context) in
    List.iter
      (fun vm ->
        Db.VM.remove_from_current_operations ~__context ~self:vm ~key:task_id ;
        Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm
      )
      vmrefs ;
    (* When only snapshots have been imported, return all of them.
       Otherwise, only keep VMs which are not snapshots *)
    let vmrefs =
      let non_snapshots =
        List.filter
          (fun x -> not (Db.VM.get_is_a_snapshot ~__context ~self:x))
          vmrefs
      in
      if non_snapshots = [] then
        vmrefs
      else
        non_snapshots
    in
    (* We only set the result on the task since it is officially completed later. *)
    TaskHelper.set_result ~__context (Some (API.rpc_of_ref_VM_set vmrefs))
  with e ->
    Backtrace.is_important e ;
    error "Caught exception completing import: %s" (ExnHelper.string_of_exn e) ;
    raise e

let find_query_flag query key =
  List.mem_assoc key query && List.assoc key query = "true"

let read_map_params name params =
  let len = String.length name + 1 in
  (* include ':' *)
  let filter_params =
    List.filter
      (fun (p, _) -> String.starts_with ~prefix:name p && String.length p > len)
      params
  in
  List.map
    (fun (k, v) -> (String.sub k len (String.length k - len), v))
    filter_params

let with_error_handling f =
  match Backtrace.with_backtraces f with
  | `Ok result ->
      result
  | `Error (e, backtrace) -> (
      Debug.log_backtrace e backtrace ;
      let reraise = Backtrace.reraise e in
      match e with
      | IFailure failure -> (
        match failure with
        | Cannot_handle_chunked ->
            error "import code cannot handle chunked encoding" ;
            reraise
              (Api_errors.Server_error
                 (Api_errors.import_error_cannot_handle_chunked, [])
              )
        | Some_checksums_failed ->
            error "some checksums failed" ;
            reraise
              (Api_errors.Server_error
                 (Api_errors.import_error_some_checksums_failed, [])
              )
        | Failed_to_find_object id ->
            error "Failed to find object with ID: %s" id ;
            reraise
              (Api_errors.Server_error
                 (Api_errors.import_error_failed_to_find_object, [id])
              )
        | Attached_disks_not_found ->
            error
              "Cannot import guest with currently attached disks which cannot \
               be found" ;
            reraise
              (Api_errors.Server_error
                 (Api_errors.import_error_attached_disks_not_found, [])
              )
        | Unexpected_file (expected, actual) ->
            let hex = Tar.Header.to_hex in
            error
              "Invalid XVA file: import expects the next file in the stream to \
               be \"%s\" [%s]; got \"%s\" [%s]"
              expected (hex expected) actual (hex actual) ;
            reraise
              (Api_errors.Server_error
                 (Api_errors.import_error_unexpected_file, [expected; actual])
              )
      )
      | Api_errors.Server_error _ as e ->
          Backtrace.is_important e ; raise e
      | End_of_file ->
          error "Prematurely reached end-of-file during import" ;
          reraise
            (Api_errors.Server_error (Api_errors.import_error_premature_eof, []))
      | e ->
          let msg_exn = ExnHelper.string_of_exn e in
          error "Import caught exception: %s" msg_exn ;
          reraise Api_errors.(Server_error (import_error_generic, [msg_exn]))
    )

(** Import metadata only *)
let metadata_handler (req : Request.t) s _ =
  debug "metadata_handler called" ;
  req.Request.close <- true ;
  Xapi_http.with_context "VM.metadata_import" req s (fun __context ->
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          let full_restore = find_query_flag req.Request.query "restore" in
          let force = find_query_flag req.Request.query "force" in
          let dry_run = find_query_flag req.Request.query "dry_run" in
          let live = find_query_flag req.Request.query "live" in
          let check_cpu = find_query_flag req.Request.query "check_cpu" in
          let vdi_map = read_map_params "vdi" req.Request.query in
          info
            "VM.import_metadata: force = %b; full_restore = %b dry_run = %b; \
             live = %b; check_cpu = %b; vdi_map = [ %s ]"
            force full_restore dry_run live check_cpu
            (String.concat "; " (List.map (fun (a, b) -> a ^ "=" ^ b) vdi_map)) ;
          let metadata_options = {dry_run; live; vdi_map; check_cpu} in
          let config =
            {import_type= Metadata_import metadata_options; full_restore; force}
          in
          let headers =
            Http.http_200_ok ~keep_alive:false ()
            @ [
                Http.Hdr.task_id
                ^ ":"
                ^ Ref.string_of (Context.get_task_id __context)
              ; content_type
              ]
          in
          Http_svr.headers s headers ;
          with_open_archive s ?length:req.Request.content_length
            (fun metadata s ->
              debug "Got XML" ;
              (* Skip trailing two zero blocks *)
              Tar_helpers.skip s (Tar.Header.length * 2) ;
              let header = metadata |> Xmlrpc.of_string |> header_of_rpc in
              assert_compatible ~__context header.version ;
              if full_restore then
                assert_can_restore_backup ~__context rpc session_id header ;
              with_error_handling (fun () ->
                  let state =
                    handle_all __context config rpc session_id header.objects
                  in
                  let table = state.table in
                  let on_cleanup_stack = state.cleanup in
                  try
                    List.iter
                      (fun (cls, id, r) ->
                        debug
                          "Imported object type %s: external ref: %s internal \
                           ref: %s"
                          cls id r
                      )
                      table ;
                    let vmrefs =
                      List.map
                        (fun (_, _, r) -> Ref.of_string r)
                        state.created_vms
                    in
                    let vmrefs = Listext.List.setify vmrefs in
                    complete_import ~__context vmrefs ;
                    info "import_metadata successful"
                  with e ->
                    Backtrace.is_important e ;
                    error "Caught exception during import: %s"
                      (ExnHelper.string_of_exn e) ;
                    if force then
                      warn
                        "Not cleaning up after import failure since --force \
                         provided: %s"
                        (ExnHelper.string_of_exn e)
                    else (
                      debug "Cleaning up after import failure: %s"
                        (ExnHelper.string_of_exn e) ;
                      cleanup on_cleanup_stack
                    ) ;
                    raise e
              )
          )
      )
  )

let stream_import __context rpc session_id s content_length refresh_session
    config =
  with_open_archive s ?length:content_length (fun metadata s ->
      debug "Got XML" ;
      let vmrefs =
        let header = metadata |> Xmlrpc.of_string |> header_of_rpc in
        assert_compatible ~__context header.version ;
        if config.full_restore then
          assert_can_restore_backup ~__context rpc session_id header ;
        (* objects created here: *)
        let state = handle_all __context config rpc session_id header.objects in
        let table, on_cleanup_stack = (state.table, state.cleanup) in
        (* signal to GUI that object have been created and they can now go off and remapp networks *)
        TaskHelper.add_to_other_config ~__context "object_creation" "complete" ;
        try
          List.iter
            (fun (cls, id, r) ->
              debug "Imported object type %s: external ref: %s internal ref: %s"
                cls id r
            )
            table ;
          (* now stream the disks. We expect not to stream CDROMs *)
          let all_vdis = non_cdrom_vdis header in
          (* some CDROMs might be in as disks, don't stream them either *)
          let all_vdis =
            List.filter (fun x -> exists (Ref.of_string x.id) table) all_vdis
          in
          let vdis =
            List.map
              (fun x ->
                let vdir =
                  API.vDI_t_of_rpc (find_in_export x.id state.export)
                in
                ( x.id
                , lookup (Ref.of_string x.id) table
                , vdir.API.vDI_virtual_size
                )
              )
              all_vdis
          in
          List.iter
            (fun (extid, intid, size) ->
              debug "Expecting to import VDI %s into %s (size=%Ld)" extid
                (Ref.string_of intid) size
            )
            vdis ;
          let checksum_table =
            Stream_vdi.recv_all refresh_session s __context rpc session_id
              header.version config.force vdis
          in
          (* CA-48768: Stream_vdi.recv_all only checks for task cancellation
             						   every ten seconds, so we need to check again now. After this
             						   point, we disable cancellation for this task. *)
          TaskHelper.exn_if_cancelling ~__context ;
          TaskHelper.set_not_cancellable ~__context ;
          (* Pre-miami GA exports have a checksum table at the end of the export. Check the calculated checksums *)
          (* against the table here. Nb. Rio GA-Miami B2 exports get their checksums checked twice! *)
          ( if header.version.export_vsn < 2 then
              let xml =
                Tar_unix.Archive.with_next_file s (fun s hdr -> read_xml hdr s)
              in
              let expected_checksums =
                xml |> Xmlrpc.of_string |> checksum_table_of_rpc
              in
              if not (compare_checksums checksum_table expected_checksums) then (
                error "Some data checksums were incorrect: VM may be corrupt" ;
                if not config.force then
                  raise (IFailure Some_checksums_failed)
                else
                  error
                    "Ignoring incorrect checksums since 'force' flag was \
                     supplied"
              )
          ) ;
          (* return vmrefs *)
          Listext.List.setify
            (List.map (fun (_, _, r) -> Ref.of_string r) state.created_vms)
        with e ->
          Backtrace.is_important e ;
          error "Caught exception during import: %s" (ExnHelper.string_of_exn e) ;
          if config.force then
            warn
              "Not cleaning up after import failure since --force provided: %s"
              (ExnHelper.string_of_exn e)
          else (
            debug "Cleaning up after import failure: %s"
              (ExnHelper.string_of_exn e) ;
            cleanup on_cleanup_stack
          ) ;
          raise e
      in
      complete_import ~__context vmrefs ;
      debug "import successful" ;
      vmrefs
  )

let handler (req : Request.t) s _ =
  req.Request.close <- true ;
  Xapi_http.assert_credentials_ok "VM.import" ~http_action:"put_import" req s ;
  debug "import handler" ;
  let full_restore = find_query_flag req.Request.query "restore" in
  let force = find_query_flag req.Request.query "force" in
  let all = req.Request.cookie @ req.Request.query in
  let subtask_of =
    if List.mem_assoc "subtask_of" all then
      Some (Ref.of_string (List.assoc "subtask_of" all))
    else
      None
  in
  (* Perform the SR reachability check using a fresh context/task because
     	   we don't want to complete the task in the forwarding case *)
  Server_helpers.exec_with_new_task ?subtask_of "VM.import" (fun __context ->
      Helpers.call_api_functions ~__context (fun rpc session_id ->
          let sr =
            match Importexport.sr_of_req ~__context req with
            | Some x ->
                x
            | None ->
                log_reraise
                  "request was missing both sr_id and sr_uuid: one must be \
                   provided"
                  (fun () ->
                    Helpers.call_api_functions ~__context get_default_sr
                  )
                  ()
          in
          info "VM.import: SR = '%s%s'; force = %b; full_restore = %b"
            (try Db.SR.get_uuid ~__context ~self:sr with _ -> "invalid")
            ( try
                Printf.sprintf " (%s)" (Db.SR.get_name_label ~__context ~self:sr)
              with _ -> ""
            )
            force full_restore ;
          if not (check_sr_availability ~__context sr) then (
            debug "sr not available - redirecting" ;
            let host = find_host_for_sr ~__context sr in
            let address = Db.Host.get_address ~__context ~self:host in
            let url =
              Uri.(
                make ~scheme:"https" ~host:address ~path:req.Request.path
                  ~query:(List.map (fun (a, b) -> (a, [b])) req.Request.query)
                  ()
                |> to_string
              )
            in
            let headers = Http.http_302_redirect url in
            debug "new location: %s" url ;
            Http_svr.headers s headers
          ) else
            Xapi_http.with_context "VM.import" req s (fun __context ->
                (* This is the signal to say we've taken responsibility from the CLI server for completing the task *)
                (* The GUI can deal with this itself, but the CLI is complicated by the thin cli/cli server split *)
                TaskHelper.set_progress ~__context 0.0 ;
                (* Block VM.import operation during RPU *)
                debug "Check RPU status before VM.import" ;
                if Helpers.rolling_upgrade_in_progress ~__context then (
                  warn "VM.import is not supported during RPU" ;
                  Http_svr.headers s (Http.http_400_badrequest ()) ;
                  raise
                    (Api_errors.Server_error
                       (Api_errors.not_supported_during_upgrade, [])
                    )
                ) ;
                if force then
                  warn "Force option supplied: will ignore checksum failures" ;
                (* Let's check that we're not trying to import into an iso library! *)
                if Db.SR.get_content_type ~__context ~self:sr = "iso" then (
                  Http_svr.headers s (Http.http_400_badrequest ()) ;
                  raise
                    (Api_errors.Server_error
                       (Api_errors.sr_operation_not_supported, [])
                    )
                ) ;
                with_error_handling (fun () ->
                    let refresh_external =
                      if List.mem_assoc "session_id" all then
                        let external_session_id = List.assoc "session_id" all in
                        Xapi_session.consider_touching_session rpc
                          (Ref.of_secret_string external_session_id)
                      else
                        fun () -> ()
                    in
                    let refresh_internal =
                      Xapi_session.consider_touching_session rpc session_id
                    in
                    let refresh_session () =
                      refresh_external () ; refresh_internal ()
                    in
                    debug "Importing %s"
                      (if full_restore then "(as 'restore')" else "(as new VM)") ;
                    let config =
                      {import_type= Full_import sr; full_restore; force}
                    in
                    match
                      (req.Request.transfer_encoding, req.Request.content_length)
                    with
                    | Some x, _ ->
                        error
                          "Encoding not yet implemented in the import code: %s"
                          x ;
                        Http_svr.headers s (http_403_forbidden ()) ;
                        raise (IFailure Cannot_handle_chunked)
                    | None, content_length ->
                        let headers =
                          Http.http_200_ok ~keep_alive:false ()
                          @ [
                              Http.Hdr.task_id
                              ^ ":"
                              ^ Ref.string_of (Context.get_task_id __context)
                            ; content_type
                            ]
                        in
                        Http_svr.headers s headers ;
                        debug "Reading XML" ;
                        ignore
                          (stream_import __context rpc session_id s
                             content_length refresh_session config
                          )
                )
            )
      ) ;
      debug "import successful"
  )
