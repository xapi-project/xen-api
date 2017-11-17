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
(** Helper functions relating to VM lifecycle operations.
 * @group Virtual-Machine Management
*)

open Stdext
open Listext

module D = Debug.Make(struct let name="xapi" end)
open D

module Rrdd = Rrd_client.Client

let assoc_opt key assocs = Opt.of_exception (fun () -> List.assoc key assocs)
let bool_of_assoc key assocs = match assoc_opt key assocs with
  | Some v -> v = "1" || String.lowercase_ascii v = "true"
  | _ -> false

(** Given an operation, [allowed_power_states] returns all the possible power state for
    	wich this operation can be performed. *)
let allowed_power_states ~__context ~vmr ~(op:API.vm_operations) =
  let all_power_states =
    [`Halted; `Paused; `Suspended; `Running] in
  match op with
  (* a VM.import is done on file and not on VMs, so there is not power-state there! *)
  | `import
    -> []
  | `changing_VCPUs
  | `changing_static_range
  | `changing_memory_limits       -> `Halted :: (if vmr.Db_actions.vM_is_control_domain then [`Running] else [])
  | `changing_shadow_memory
  | `make_into_template
  | `provision
  | `start
  | `start_on
    -> [`Halted]
  | `unpause
    -> [`Paused]
  | `csvm
  | `resume
  | `resume_on
    -> [`Suspended]
  | `awaiting_memory_live
  | `call_plugin
  | `clean_reboot
  | `clean_shutdown
  | `changing_memory_live
  | `changing_shadow_memory_live
  | `changing_VCPUs_live
  | `data_source_op
  | `pause
  | `pool_migrate
  | `send_sysrq
  | `send_trigger
  | `snapshot_with_quiesce
  | `suspend
    -> [`Running]
  | `changing_dynamic_range
    -> [`Halted; `Running]
  | `clone
  | `copy
    -> `Halted :: (if vmr.Db_actions.vM_is_a_snapshot || Helpers.clone_suspended_vm_enabled ~__context then [`Suspended] else [])
  | `create_template (* Don't touch until XMLRPC unmarshal code is able to pre-blank fields on input. *)
  | `destroy
  | `export
    -> [`Halted; `Suspended]
  | `hard_reboot
    -> [`Paused; `Running]
  | `checkpoint
  | `get_boot_record
  | `shutdown
  | `hard_shutdown
    -> [`Paused; `Suspended; `Running]
  | `migrate_send
    -> [`Halted; `Suspended; `Running]
  | `assert_operation_valid
  | `metadata_export
  | `power_state_reset
  | `revert
  | `reverting
  | `snapshot
  | `update_allowed_operations
  | `query_services
    -> all_power_states

(** check if [op] can be done when [vmr] is in [power_state], when no other operation is in progress *)
let is_allowed_sequentially ~__context ~vmr ~power_state ~op =
  List.mem power_state (allowed_power_states ~__context ~vmr ~op)

(**	check if [op] can be done while [current_ops] are already in progress.
   	Remark: we do not test whether the power-state is valid. *)
let is_allowed_concurrently ~(op:API.vm_operations) ~current_ops =
  (* declare below the non-conflicting concurrent sets. *)
  let long_copies = [`clone; `copy; `export ]
  and boot_record = [`get_boot_record]
  and snapshot    = [`snapshot; `checkpoint]
  and allowed_operations = (* a list of valid state -> operation *)
    [ [`snapshot_with_quiesce], `snapshot;
      [`migrate_send],          `metadata_export;
      [`migrate_send],          `clean_shutdown;
      [`migrate_send],          `clean_reboot;
      [`migrate_send],          `start;
      [`migrate_send],          `start_on;
    ] in
  let state_machine () =
    let current_state = List.map snd current_ops in
    match op with
    | `hard_shutdown
      -> not (List.mem op current_state)
    | `hard_reboot -> not (List.exists
                             (fun o -> List.mem o [`hard_shutdown; `hard_reboot]) current_state)
    | _ -> List.exists (fun (state, transition) ->
        state = current_state && transition = op) allowed_operations
  in
  let aux ops =
    List.mem op ops && List.for_all (fun (_,o) -> List.mem o ops) current_ops
  in
  aux long_copies || aux snapshot || aux boot_record || state_machine ()

(** True iff the vm guest metrics "other" field includes (feature, "1")
    	as a key-value pair. *)
let has_feature ~vmgmr ~feature =
  match vmgmr with
  | None -> false
  | Some gmr ->
    let other = gmr.Db_actions.vM_guest_metrics_other in
    try
      List.assoc feature other = "1"
    with Not_found -> false

(* Returns `true` only if we are certain that the VM has booted PV (if there
 * is no metrics record, then we can't tell) *)
let has_definitely_booted_pv ~vmmr =
  match vmmr with
  | None -> false
  | Some r -> r.Db_actions.vM_metrics_hvm = false

(** Return an error iff vmr is an HVM guest and lacks a needed feature.
 *  Note: it turned out that the Windows guest agent does not write "feature-suspend"
 *  on resume (only on startup), so we cannot rely just on that flag. We therefore
 *  add a clause that enables all features when PV drivers are present using the
 *  old-style check.
 *  The "strict" param should be true for determining the allowed_operations list
 *  (which is advisory only) and false (more permissive) when we are potentially about
 *  to perform an operation. This makes a difference for ops that require the guest to
 *  react helpfully. *)
let check_op_for_feature ~__context ~vmr ~vmmr ~vmgmr ~power_state ~op ~ref ~strict =
  if power_state <> `Running ||
     (* PV guests offer support implicitly *)
     has_definitely_booted_pv ~vmmr ||
     Xapi_pv_driver_version.(has_pv_drivers (of_guest_metrics vmgmr)) (* Full PV drivers imply all features *)
  then None
  else
    let some_err e =
      Some (e, [ Ref.string_of ref ])
    in
    let lack_feature feature =
      not (has_feature ~vmgmr ~feature)
    in
    match op with
    | `clean_shutdown
      when strict && lack_feature "feature-shutdown" && lack_feature "feature-poweroff"
      -> some_err Api_errors.vm_lacks_feature
    | `clean_reboot
      when strict && lack_feature "feature-shutdown" && lack_feature "feature-reboot"
      -> some_err Api_errors.vm_lacks_feature
    | `changing_VCPUs_live
      when lack_feature "feature-vcpu-hotplug"
      -> some_err Api_errors.vm_lacks_feature
    | `suspend | `checkpoint | `pool_migrate | `migrate_send
      when strict && lack_feature "feature-suspend"
      -> some_err Api_errors.vm_lacks_feature
    | _ -> None
(* N.B. In the pattern matching above, "pat1 | pat2 | pat3" counts as
   	 * one pattern, and the whole thing can be guarded by a "when" clause. *)

(* templates support clone operations, destroy and cross-pool migrate (if not default),
   export, provision, and memory settings change *)
let check_template ~vmr ~op ~ref_str =
  let default_template =
    vmr.Db_actions.vM_is_default_template ||
    bool_of_assoc Xapi_globs.default_template_key vmr.Db_actions.vM_other_config in
  let allowed_operations = [
    `changing_dynamic_range;
    `changing_static_range;
    `changing_memory_limits;
    `changing_shadow_memory;
    `changing_VCPUs;
    `clone;
    `copy;
    `export;
    `metadata_export;
    `provision;
  ] in
  if false
  || List.mem op allowed_operations
  || ((op = `destroy || op = `migrate_send) && not default_template)
  then None
  else Some (Api_errors.vm_is_template, [ref_str; Record_util.vm_operation_to_string op])

let check_snapshot ~vmr ~op ~ref_str =
  let allowed = [`revert; `clone; `copy; `export; `destroy; `hard_shutdown; `metadata_export] in
  if List.mem op allowed
  then None
  else Some (Api_errors.vm_is_snapshot, [ref_str; Record_util.vm_operation_to_string op])

(* report a power_state/operation error *)
let report_power_state_error ~__context ~vmr ~power_state ~op ~ref_str =
  let expected = allowed_power_states ~__context ~vmr ~op in
  let expected = String.concat ", " (List.map Record_util.power_to_string expected) in
  let actual = Record_util.power_to_string power_state in
  Some (Api_errors.vm_bad_power_state, [ref_str; expected; actual])

let report_concurrent_operations_error ~current_ops ~ref_str =
  let current_ops_str =
    match current_ops with
    | [] -> failwith "No concurrent operation to report"
    | [_,cop] -> Record_util.vm_operation_to_string cop
    | l -> "{" ^ (String.concat "," (List.map Record_util.vm_operation_to_string (List.map snd l))) ^ "}"
  in
  Some (Api_errors.other_operation_in_progress,["VM." ^ current_ops_str; ref_str])

(* Suspending, checkpointing and live-migrating are not (yet) allowed if a PCI device is passed through *)
let check_pci ~op ~ref_str =
  match op with
  | `suspend | `checkpoint | `pool_migrate | `migrate_send -> Some (Api_errors.vm_has_pci_attached, [ref_str])
  | _ -> None

let check_vgpu ~__context ~op ~ref_str ~vgpus =
  let vgpu_migration_enabled () =
    let pool = Helpers.get_pool ~__context in
    let restrictions = Db.Pool.get_restrictions ~__context ~self:pool in
    try
      List.assoc "restrict_vgpu_migration" restrictions = "false"
    with Not_found -> false
  in
  let is_nvidia_vgpu vgpu =
    Db.VGPU.get_type ~__context ~self:vgpu
    |> fun self -> Db.VGPU_type.get_implementation ~__context ~self
    |> function
    | `nvidia -> true
    | _       -> false
  in
  let is_suspendable vgpu =
    match Db.VGPU.get_resident_on ~__context ~self:vgpu with
    | pgpu when not (Db.is_valid_ref __context pgpu) -> false
    | self -> Db.PGPU.get_compatibility_metadata ~__context ~self
      |> function
      | [] -> false (* no meta data: we can't check compatibility *)
      | _  -> true
  in
  match op with
  | `pool_migrate | `migrate_send | `suspend | `checkpoint
    when
      vgpu_migration_enabled ()
      && List.for_all is_nvidia_vgpu vgpus
      && List.for_all is_suspendable vgpus -> None
  | `pool_migrate | `migrate_send | `suspend | `checkpoint ->
    Some (Api_errors.vm_has_vgpu, [ref_str])
  | _ -> None

(* VM cannot be converted into a template while it is a member of an appliance. *)
let check_appliance ~vmr ~op ~ref_str =
  match op with
  | `make_into_template -> Some (Api_errors.vm_is_part_of_an_appliance,
                                 [ref_str; Ref.string_of vmr.Db_actions.vM_appliance])
  | _ -> None

(* VM cannot be converted into a template while it is assigned to a protection policy. *)
let check_protection_policy ~vmr ~op ~ref_str =
  match op with
  | `make_into_template -> Some (Api_errors.vm_assigned_to_protection_policy,
                                 [ref_str; Ref.string_of vmr.Db_actions.vM_protection_policy])
  | _ -> None

(* VM cannot be converted into a template while it is assigned to a snapshot schedule. *)
let check_snapshot_schedule ~vmr ~ref_str = function
  | `make_into_template -> Some (Api_errors.vm_assigned_to_snapshot_schedule,
                                 [ref_str; Ref.string_of vmr.Db_actions.vM_snapshot_schedule])
  | _ -> None

(** Some VMs can't migrate. The predicate [is_mobile] is true, if and
 * only if a VM is mobile.
 *
 * A VM is not mobile if any following values are true:
 * [platform:nomigrate] or [platform:nested-virt].  A VM can always
 * migrate if strict=false.
 *
 * The values of [platform:nomigrate] and [platform:nested-virt] are
 * captured by Xenopsd when a VM starts, reported to Xapi, and kept in
 * the VM_metrics data model.
 *
 * If the VM_metrics object does not exist, it implies the VM is not
 * running - in which case we use the current values from the database.
 **)

let nomigrate ~__context vm metrics =
  try
    Db.VM_metrics.get_nomigrate ~__context ~self:metrics
  with _ ->
    let platformdata = Db.VM.get_platform ~__context ~self:vm in
    let key = "nomigrate" in
    Vm_platform.is_true ~key ~platformdata ~default:false

let nested_virt ~__context vm metrics =
  try
    Db.VM_metrics.get_nested_virt ~__context ~self:metrics
  with _ ->
    let platformdata = Db.VM.get_platform ~__context ~self:vm in
    let key = "nested-virt" in
    Vm_platform.is_true ~key ~platformdata ~default:false

let is_mobile ~__context vm strict =
  let metrics = Db.VM.get_metrics ~__context ~self:vm in
  (not @@ nomigrate ~__context vm metrics
   && not @@ nested_virt ~__context vm metrics)
  || not strict

let maybe_get_metrics ~__context ~ref =
  if Db.is_valid_ref __context ref
  then Some (Db.VM_metrics.get_record_internal ~__context ~self:ref)
  else None

let maybe_get_guest_metrics ~__context ~ref =
  if Db.is_valid_ref __context ref
  then Some (Db.VM_guest_metrics.get_record_internal ~__context ~self:ref)
  else None

(** Take an internal VM record and a proposed operation. Return None iff the operation
    would be acceptable; otherwise Some (Api_errors.<something>, [list of strings])
    corresponding to the first error found. Checking stops at the first error.
    The "strict" param sets whether we require feature-flags for ops that need guest
    support: ops in the suspend-like and shutdown-like categories. *)
let check_operation_error ~__context ~ref =
  let vmr = Db.VM.get_record_internal ~__context ~self:ref in
  let vmmr = maybe_get_metrics ~__context ~ref:(vmr.Db_actions.vM_metrics) in
  let vmgmr = maybe_get_guest_metrics ~__context ~ref:(vmr.Db_actions.vM_guest_metrics) in
  let ref_str = Ref.string_of ref in
  let power_state = vmr.Db_actions.vM_power_state in
  let is_template = vmr.Db_actions.vM_is_a_template in
  let is_snapshot = vmr.Db_actions.vM_is_a_snapshot in
  let vdis = List.filter_map (fun vbd -> try Some (Db.VBD.get_VDI ~__context ~self:vbd) with _ -> None) vmr.Db_actions.vM_VBDs |> List.filter (Db.is_valid_ref __context) in

  (fun ~op ~strict ->

  (* Check if the operation has been explicitly blocked by the/a user *)
  let current_error = None in

  let check c f = match c with | Some e -> Some e | None -> f () in

  let current_error = check current_error (fun () ->
      Opt.map (fun v -> Api_errors.operation_blocked, [ref_str; v])
        (assoc_opt op vmr.Db_actions.vM_blocked_operations)) in

  (* Always check the power state constraint of the operation first *)
  let current_error = check current_error (fun () ->
      if not (is_allowed_sequentially ~__context ~vmr ~power_state ~op)
      then report_power_state_error ~__context ~vmr ~power_state ~op ~ref_str
      else None) in

  (* if other operations are in progress, check that the new operation is allowed concurrently with them. *)
  let current_error = check current_error (fun () ->
      let current_ops = vmr.Db_actions.vM_current_operations in
      if List.length current_ops <> 0 && not (is_allowed_concurrently ~op ~current_ops)
      then report_concurrent_operations_error ~current_ops ~ref_str
      else None) in

  (* if the VM is a template, check the template behavior exceptions. *)
  let current_error = check current_error (fun () ->
      if is_template && not is_snapshot
      then check_template ~vmr ~op ~ref_str
      else None) in

  (* if the VM is a snapshot, check the snapshot behavior exceptions. *)
  let current_error = check current_error (fun () ->
      if is_snapshot
      then check_snapshot ~vmr ~op ~ref_str
      else None) in

  (* if the VM is neither a template nor a snapshot, do not allow provision and revert. *)
  let current_error = check current_error (fun () ->
      if op = `provision && (not is_template)
      then Some (Api_errors.only_provision_template, [])
      else None) in

  let current_error = check current_error (fun () ->
      if op = `revert && (not is_snapshot)
      then Some (Api_errors.only_revert_snapshot, [])
      else None) in

  (* Some ops must be blocked if VM is not mobile *)
  let current_error = check current_error (fun () ->
      match op with
      | `suspend
      | `checkpoint
      | `pool_migrate
      | `migrate_send
        when not (is_mobile ~__context ref strict) ->
        Some (Api_errors.vm_is_immobile, [ref_str])
      | _ -> None
    ) in

  let current_error =
    let metrics = Db.VM.get_metrics ~__context ~self:ref in
    check current_error (fun () ->
      match op with
      | `changing_dynamic_range
        when nested_virt ~__context ref metrics && strict ->
        Some (Api_errors.vm_is_using_nested_virt, [ref_str])
      | _ -> None
    ) in


  (* Check if the VM is a control domain (eg domain 0).            *)
  (* FIXME: Instead of special-casing for the control domain here, *)
  (* make use of the Helpers.ballooning_enabled_for_vm function.   *)
  let current_error = check current_error (fun () ->
         let vm_ref () = Db.VM.get_by_uuid ~__context ~uuid:vmr.Db_actions.vM_uuid in
         if (op = `changing_VCPUs || op = `destroy) && Helpers.is_domain_zero ~__context (vm_ref ())
      then Some (Api_errors.operation_not_allowed, ["This operation is not allowed on dom0"])
      else if vmr.Db_actions.vM_is_control_domain
           && op <> `data_source_op
           && op <> `changing_memory_live
           && op <> `awaiting_memory_live
           && op <> `metadata_export
           && op <> `changing_dynamic_range
           && op <> `changing_memory_limits
           && op <> `changing_static_range
           && op <> `start
           && op <> `start_on
           && op <> `changing_VCPUs
           && op <> `destroy
      then Some (Api_errors.operation_not_allowed, ["This operation is not allowed on a control domain"])
      else None) in

  (* check for any HVM guest feature needed by the op *)
  let current_error = check current_error (fun () ->
      check_op_for_feature ~__context ~vmr ~vmmr ~vmgmr ~power_state ~op ~ref ~strict
    ) in

  (* check if the dynamic changeable operations are still valid *)
  let current_error = check current_error (fun () ->
      if op = `snapshot_with_quiesce &&
         (Pervasiveext.maybe_with_default true
            (fun gm -> let other = gm.Db_actions.vM_guest_metrics_other in
              not (List.mem_assoc "feature-quiesce" other || List.mem_assoc "feature-snapshot" other))
            vmgmr)
      then Some (Api_errors.vm_snapshot_with_quiesce_not_supported, [ ref_str ])
      else None) in

  (* Check for an error due to VDI caching/reset behaviour *)
  let current_error = check current_error (fun () ->
      let vdis_reset_and_caching = List.filter_map (fun vdi ->
          try
            let sm_config = Db.VDI.get_sm_config ~__context ~self:vdi in
            Some ((assoc_opt "on_boot" sm_config = Some "reset"), (bool_of_assoc "caching" sm_config))
          with _ -> None) vdis in
      if op = `checkpoint || op = `snapshot || op = `suspend || op = `snapshot_with_quiesce
      then (* If any vdi exists with on_boot=reset, then disallow checkpoint, snapshot, suspend *)
        if List.exists fst vdis_reset_and_caching
        then Some (Api_errors.vdi_on_boot_mode_incompatible_with_operation,[])
        else None
      else if op = `pool_migrate then
        (* If any vdi exists with on_boot=reset and caching is enabled, disallow migrate *)
        if List.exists (fun (reset,caching) -> reset && caching) vdis_reset_and_caching
        then Some (Api_errors.vdi_on_boot_mode_incompatible_with_operation,[])
        else None
      else None) in

  (* If a PCI device is passed-through, check if the operation is allowed *)
  let current_error = check current_error (fun () ->
      if vmr.Db_actions.vM_attached_PCIs <> []
      then check_pci ~op ~ref_str
      else None) in

  (* The VM has a VGPU, check if the operation is allowed*)
  let current_error = check current_error (fun () ->
      if vmr.Db_actions.vM_VGPUs <> []
      then check_vgpu ~__context ~op ~ref_str ~vgpus:vmr.Db_actions.vM_VGPUs
      else None) in

  (* The VM has a VUSB, check if the operation is allowed*)
  let current_error = check current_error (fun () ->
    match op with
      | `suspend
      | `snapshot
      | `checkpoint
      | `migrate_send
      | `pool_migrate when vmr.Db_actions.vM_VUSBs <> [] -> Some (Api_errors.vm_has_vusbs, [ref_str])
      | _ -> None) in

  (* Check for errors caused by VM being in an appliance. *)
  let current_error = check current_error (fun () ->
      if Db.is_valid_ref __context vmr.Db_actions.vM_appliance
      then check_appliance ~vmr ~op ~ref_str
      else None) in

  (* Check for errors caused by VM being assigned to a protection policy. *)
  let current_error = check current_error (fun () ->
      if Db.is_valid_ref __context vmr.Db_actions.vM_protection_policy
      then check_protection_policy ~vmr ~op ~ref_str
      else None) in

  (* Check for errors caused by VM being assigned to a snapshot schedule. *)
  let current_error = check current_error (fun () ->
      if Db.is_valid_ref __context vmr.Db_actions.vM_snapshot_schedule
      then check_snapshot_schedule ~vmr ~ref_str op
      else None) in

  (* Check whether this VM needs to be a system domain. *)
  let current_error = check current_error (fun () ->
      if op = `query_services && not (bool_of_assoc "is_system_domain" vmr.Db_actions.vM_other_config)
      then Some (Api_errors.not_system_domain, [ ref_str ])
      else None) in

  let current_error = check current_error (fun () ->
    if Helpers.rolling_upgrade_in_progress ~__context &&
       not (List.mem op Xapi_globs.rpu_allowed_vm_operations)
    then Some (Api_errors.not_supported_during_upgrade, [])
    else None)
  in

  current_error
  )

let get_operation_error ~__context ~self ~op ~strict =
  check_operation_error ~__context ~ref:self ~op ~strict

let assert_operation_valid ~__context ~self ~op ~strict =
  match get_operation_error ~__context ~self ~op ~strict with
  | None       -> ()
  | Some (a,b) -> raise (Api_errors.Server_error (a,b))

let update_allowed_operations ~__context ~self =
  let check_operation_error = check_operation_error ~__context ~ref:self in
  let check accu op =
    match check_operation_error ~op ~strict:true with
    | None -> op :: accu
    | _    -> accu
  in
  let allowed =
    List.fold_left check []
      [`snapshot; `copy; `clone; `revert; `checkpoint; `snapshot_with_quiesce;
       `start; `start_on; `pause; `unpause; `clean_shutdown; `clean_reboot;
       `hard_shutdown; `hard_reboot; `suspend; `resume; `resume_on; `export; `destroy;
       `provision; `changing_VCPUs_live; `pool_migrate; `migrate_send; `make_into_template; `changing_static_range;
       `changing_shadow_memory; `changing_dynamic_range]
  in
  (* FIXME: need to be able to deal with rolling-upgrade for orlando as well *)
  let allowed =
    if Helpers.rolling_upgrade_in_progress ~__context
    then Listext.List.intersect allowed Xapi_globs.rpu_allowed_vm_operations
    else allowed
  in
  Db.VM.set_allowed_operations ~__context ~self ~value:allowed;
  (* Update the appliance's allowed operations. *)
  let appliance = Db.VM.get_appliance ~__context ~self in
  if Db.is_valid_ref __context appliance then
    Xapi_vm_appliance_lifecycle.update_allowed_operations ~__context ~self:appliance

(** Called on new VMs (clones, imports) and on server start to manually refresh
    the power state, allowed_operations field etc.  Current-operations won't be
    cleaned *)
let force_state_reset_keep_current_operations ~__context ~self ~value:state =
  if state = `Halted then begin
    (* mark all devices as disconnected *)
    List.iter
      (fun vbd ->
         Db.VBD.set_currently_attached ~__context ~self:vbd ~value:false;
         Db.VBD.set_reserved ~__context ~self:vbd ~value:false;
         Xapi_vbd_helpers.clear_current_operations ~__context ~self:vbd)
      (Db.VM.get_VBDs ~__context ~self);
    List.iter
      (fun vif ->
         Db.VIF.set_currently_attached ~__context ~self:vif ~value:false;
         Db.VIF.set_reserved ~__context ~self:vif ~value:false;
         Xapi_vif_helpers.clear_current_operations ~__context ~self:vif;
         Opt.iter
           (fun p -> Pvs_proxy_control.clear_proxy_state ~__context vif p)
           (Pvs_proxy_control.find_proxy_for_vif ~__context ~vif))
      (Db.VM.get_VIFs ~__context ~self);
    List.iter
      (fun vgpu ->
         Db.VGPU.set_currently_attached ~__context ~self:vgpu ~value:false;
         Db.VGPU.set_resident_on ~__context ~self:vgpu ~value:Ref.null;
         Db.VGPU.set_scheduled_to_be_resident_on
           ~__context ~self:vgpu ~value:Ref.null)
      (Db.VM.get_VGPUs ~__context ~self);
    List.iter
      (fun pci ->
         Db.PCI.remove_attached_VMs ~__context ~self:pci ~value:self)
      (Db.VM.get_attached_PCIs ~__context ~self);
    List.iter
      (fun pci ->
         (* The following should not be necessary if many-to-many relations in the DB
          * work properly. People have reported issues that may indicate that this is
          * not the case, but we have not yet found the root cause. Therefore, the
          * following code is there "just to be sure".*)
         if List.mem self (Db.PCI.get_attached_VMs ~__context ~self:pci) then
           Db.PCI.remove_attached_VMs ~__context ~self:pci ~value:self;
         (* Clear any PCI device reservations for this VM. *)
         if Db.PCI.get_scheduled_to_be_attached_to ~__context ~self:pci = self then
           Db.PCI.set_scheduled_to_be_attached_to ~__context ~self:pci ~value:Ref.null
      )
      (Db.PCI.get_all ~__context);
    List.iter
      (fun vusb ->
         Db.VUSB.set_currently_attached ~__context ~self:vusb ~value:false;
         Xapi_vusb_helpers.clear_current_operations ~__context ~self:vusb;
      ) (Db.VM.get_VUSBs ~__context ~self);
    (* Blank the requires_reboot flag *)
    Db.VM.set_requires_reboot ~__context ~self ~value:false
  end;

  if state = `Halted || state = `Suspended then begin
    Db.VM.set_resident_on ~__context ~self ~value:Ref.null;
    (* make sure we aren't reserving any memory for this VM *)
    Db.VM.set_scheduled_to_be_resident_on ~__context ~self ~value:Ref.null;
    Db.VM.set_domid ~__context ~self ~value:(-1L)
  end;

  Db.VM.set_power_state ~__context ~self ~value:state;
  update_allowed_operations ~__context ~self;

  if state = `Halted then begin
    (* archive the rrd for this vm *)
    let vm_uuid = Db.VM.get_uuid ~__context ~self in
    log_and_ignore_exn (fun () -> Rrdd.archive_rrd ~vm_uuid ~remote_address:(try Some (Pool_role.get_master_address ()) with _ -> None))
  end

(** Called on new VMs (clones, imports) and on server start to manually refresh
    the power state, allowed_operations field etc.  Clean current-operations
    as well *)
let force_state_reset ~__context ~self ~value:state =
  if (Db.VM.get_current_operations ~__context ~self) <> [] then
    Db.VM.set_current_operations ~__context ~self ~value:[];
  force_state_reset_keep_current_operations ~__context ~self ~value:state

(** Someone is cancelling a task so remove it from the current_operations *)
let cancel_task ~__context ~self ~task_id =
  let all = List.map fst (Db.VM.get_current_operations ~__context ~self) in
  if List.mem task_id all
  then begin
    Db.VM.remove_from_current_operations ~__context ~self ~key:task_id;
    update_allowed_operations ~__context ~self
  end

let cancel_tasks ~__context ~self ~all_tasks_in_db ~task_ids =
  let ops = Db.VM.get_current_operations ~__context ~self in
  let set = (fun value -> Db.VM.set_current_operations ~__context ~self ~value) in
  Helpers.cancel_tasks ~__context ~ops ~all_tasks_in_db ~task_ids ~set

(** VM is considered as "live" when it's either Running or Paused, i.e. with a live domain *)
let is_live ~__context ~self =
  let power_state = Db.VM.get_power_state ~__context ~self in
  power_state = `Running || power_state = `Paused

(** Assert that VM is in a certain set of states before starting an operation *)
let assert_initial_power_state_in ~__context ~self ~allowed =
  let actual = Db.VM.get_power_state ~__context ~self in
  if not (List.mem actual allowed)
  then raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, [
      Ref.string_of self;
      List.map Record_util.power_to_string allowed |> String.concat ";";
      Record_util.power_to_string actual ]))

(** Assert that VM is in a certain state before starting an operation *)
let assert_initial_power_state_is ~expected = assert_initial_power_state_in ~allowed:[expected]

(** Assert that VM is in a certain set of states after completing an operation *)
let assert_final_power_state_in ~__context ~self ~allowed =
  let actual = Db.VM.get_power_state ~__context ~self in
  if not (List.mem actual allowed)
  then raise (Api_errors.Server_error(Api_errors.internal_error, [
      "VM not in expected power state after completing operation";
      Ref.string_of self;
      List.map Record_util.power_to_string allowed |> String.concat ";";
      Record_util.power_to_string actual ]))

(** Assert that VM is in a certain state after completing an operation *)
let assert_final_power_state_is ~expected = assert_final_power_state_in ~allowed:[expected]
