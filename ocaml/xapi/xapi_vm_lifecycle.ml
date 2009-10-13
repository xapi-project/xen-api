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
(** Helper functions relating to VM lifecycle operations *)

open Xapi_pv_driver_version

module D = Debug.Debugger(struct let name="xapi" end)
open D

(** Given an operation, [allowed_power_states] returns all the possible power state for
	wich this operation can be performed. *)
let allowed_power_states ~(op:API.vm_operations) =
	let all_power_states =
		[`Halted; `Paused; `Suspended; `Running] in
	match op with
	(* a VM.import is done on file and not on VMs, so there is not power-state there! *)
	| `import
	                                -> []
	| `changing_memory_limits
	| `changing_shadow_memory
	| `changing_static_range
	| `changing_VCPUs
	| `destroy
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
	| `clean_reboot
	| `clean_shutdown
	| `changing_memory_live
	| `changing_shadow_memory_live
	| `changing_VCPUs_live 
	| `data_source_op
	| `migrate
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
	| `create_template
	| `export
	                                -> [`Halted; `Suspended]
	| `hard_reboot
	                                -> [`Paused; `Running]
	| `checkpoint
	| `get_boot_record
	| `hard_shutdown
	                                -> [`Paused; `Suspended; `Running]
	| `assert_operation_valid
	| `metadata_export 
	| `power_state_reset
	| `revert
	| `reverting
	| `snapshot
	| `update_allowed_operations
	| `get_cooperative
	                                -> all_power_states

(** check if [op] can be done in [power_state], when no other operation is in progress *)
let is_allowed_sequentially ~power_state ~op =
	List.mem power_state (allowed_power_states op)

(**	check if [op] can be done while [current_ops] are already in progress.
	Remark: we do not test whether the power-state is valid. *)
let is_allowed_concurrently ~(op:API.vm_operations) ~current_ops =
	(* declare below the non-conflicting concurrent sets. *)
	let long_copies = [`clone; `copy; `export; `create_template]
	and boot_record = [`get_boot_record]
	and snapshot    = [`snapshot; `checkpoint]
	and allowed_operations = (* a list of valid state -> operation *)
		[ [`snapshot_with_quiesce], `snapshot;
		  [`reverting],             `hard_shutdown ] in                
	let state_machine () = 
		let current_state = List.map snd current_ops in
		List.exists (fun (state, transition) -> state = current_state && transition = op) allowed_operations
	in
	let aux ops =
		List.mem op ops && List.for_all (fun (_,o) -> List.mem o ops) current_ops
	in
	aux long_copies || aux snapshot || aux boot_record || state_machine ()

(** Special handling is required for RedHat version 3 *)
let is_rhel3 gmr =
	match gmr with
	| Some gmr ->
		let version = gmr.Db_actions.vM_guest_metrics_os_version in
		true
		&& List.mem_assoc "distro" version && List.assoc "distro" version = "rhel"
		&& List.mem_assoc "major" version && List.assoc "major" version = "3"
	| None ->
		false

(** Check if we have PV drivers and if we are not an SMP RHEL 3 guest
		1) Suspend, Pool_migrate, Clean_shutdown, Clean_reboot and Changing_VCPUs_live
			are not possible for HVM domains whitout PV drivers;
		2) Suspend and pool_migrate is only possible with drivers whose version is >= 4.
    Note CA-26764: although we don't have a good policy for dealing with old suspended guests
    since we still allow them to be migrated we might as well allow them to be suspended because
    the code is mostly the same.
 *)
let check_drivers ~vmr ~vmgmr ~op ~ref =
	let has_booted_hvm = Helpers.has_booted_hvm_of_record vmr in
	let pv_drivers = of_guest_metrics vmgmr in
	let has_pv_drivers = has_pv_drivers pv_drivers in

	(* FIXME: need to update the code for is_of_for_nigrate *)
	let has_good_drivers =
		match op with
		| `pool_migrate | `suspend | `checkpoint                     -> is_ok_for_migrate pv_drivers
		| `clean_shutdown | `clean_reboot | `changing_VCPUs_live | _ -> true
	in
	let is_a_rhel3_bug =
		match op with
		| `suspend | `pool_migrate            -> is_rhel3 vmgmr && vmr.Db_actions.vM_VCPUs_max <> 1L
		| `changing_VCPUs_live                -> is_rhel3 vmgmr
		| `clean_shutdown | `clean_reboot | _ -> false
	in
	let op_str = Record_util.vm_operation_to_string op in

	if has_booted_hvm && not has_pv_drivers
	then Some (Api_errors.operation_not_allowed, [ op_str ^ " not possible for HVM domains (are PV drivers installed?)"])
	else if is_a_rhel3_bug
	then Some (Api_errors.operation_not_allowed, [ op_str ^ " not possible for SMP RHEL 3 linux guests" ])
	else if has_good_drivers
	then None
	else make_error_opt pv_drivers ref vmr.Db_actions.vM_guest_metrics

let need_pv_drivers_check ~power_state ~op =
	let op_list = [ `suspend; `checkpoint; `pool_migrate; `clean_shutdown; `clean_reboot; `changing_VCPUs_live ] in
	power_state = `Running && List.mem op op_list

(* templates support clone operations, destroy (if not default), export, provision and memory settings change *)
let check_template ~vmr ~op ~ref_str =
	let default_template =
		List.mem_assoc Xapi_globs.default_template_key vmr.Db_actions.vM_other_config
		&& (List.assoc Xapi_globs.default_template_key vmr.Db_actions.vM_other_config) = "true"
	in
	if false
	  || List.mem op [`clone; `copy; `export; `provision; `metadata_export; `create_template; `changing_dynamic_range; `changing_static_range] 
	  || (op = `destroy && not default_template)
	then None
	else Some (Api_errors.vm_is_template, [ref_str; Record_util.vm_operation_to_string op])

let check_snapshot ~vmr ~op ~ref_str =
	let allowed = [`revert; `create_template; `export; `destroy; `hard_shutdown; `metadata_export] in
	if List.mem op allowed
	then None
	else Some (Api_errors.vm_is_snapshot, [ref_str; Record_util.vm_operation_to_string op])

(* report a power_state/operation error *)
let report_power_state_error ~power_state ~op ~ref_str =
	let expected = allowed_power_states op in
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

(** Take an internal VM record and a proposed operation, return true if the operation
    would be acceptable *)
let check_operation_error ~vmr ~vmgmr ~ref ~clone_suspended_vm_enabled ~op =
	let ref_str = Ref.string_of ref in
	let power_state = vmr.Db_actions.vM_power_state in
	let current_ops = vmr.Db_actions.vM_current_operations in

	(* Check if the operation has been explicitly blocked by the/a user *)
	if List.mem_assoc op vmr.Db_actions.vM_blocked_operations
	then Some (Api_errors.operation_blocked, [ ref_str; List.assoc op vmr.Db_actions.vM_blocked_operations ])

	(* if no other operations are done at the same time, first check if the new operation can be done *)
	else if List.length current_ops = 0 &&  not (is_allowed_sequentially ~power_state ~op)
	then report_power_state_error ~power_state ~op ~ref_str

	(* if other operations are in progress, check that the new operation concurrently to these ones. *)
	else if List.length current_ops <> 0 && not (is_allowed_concurrently ~op ~current_ops)
	then report_concurrent_operations_error ~current_ops ~ref_str

	(* if the VM is a template, check the template behavior exceptions. *)
	else if vmr.Db_actions.vM_is_a_template && not vmr.Db_actions.vM_is_a_snapshot
	then check_template ~vmr ~op ~ref_str
	
	(* if the VM is a snapshot, check the snapshot behavior exceptions. *)
	else if vmr.Db_actions.vM_is_a_snapshot
	then check_snapshot ~vmr ~op ~ref_str

	(* if the VM is neither a template nor a snapshot, do not allow provision and revert. *)
	else if op = `provision
	then Some (Api_errors.only_provision_template, [])

	else if op = `revert
	then Some (Api_errors.only_revert_snapshot, [])

	(* Check if the VM is a control domain (eg domain 0).            *)
	(* FIXME: Instead of special-casing for the control domain here, *)
	(* make use of the Helpers.ballooning_enabled_for_vm function.   *)
	else if vmr.Db_actions.vM_is_control_domain
		&& op <> `data_source_op
		&& op <> `changing_memory_live
		&& op <> `awaiting_memory_live
		&& op <> `metadata_export
		&& op <> `changing_dynamic_range
	then Some (Api_errors.operation_not_allowed, ["Operations on domain 0 are not allowed"])

	(* check PV drivers constraints if needed *)
	else if need_pv_drivers_check ~power_state ~op
	then check_drivers ~vmr ~vmgmr ~op ~ref

	(* check is the correct flag is set to allow clone/copy on suspended VM. *)
	else if power_state = `Suspended && (op = `clone || op = `copy) && not clone_suspended_vm_enabled
	then Some (Api_errors.vm_bad_power_state, [ref_str; "halted"; Record_util.power_to_string power_state])

	(* check if the dynamic changeable operations are still valid *)
	else if op = `snapshot_with_quiesce && 
		(Pervasiveext.maybe_with_default true
			 (fun gm -> let other = gm.Db_actions.vM_guest_metrics_other in 
			  not (List.mem_assoc "feature-quiesce" other || List.mem_assoc "feature-snapshot" other)) 
			 vmgmr)
	then Some (Api_errors.vm_snapshot_with_quiesce_not_supported, [ ref_str ])
	else None

let maybe_get_guest_metrics ~__context ~ref =
	if Db.is_valid_ref ref
	then Some (Db.VM_guest_metrics.get_record_internal ~__context ~self:ref)
	else None

let get_info ~__context ~self =
	let all = Db.VM.get_record_internal ~__context ~self in
	let gm = maybe_get_guest_metrics ~__context ~ref:(all.Db_actions.vM_guest_metrics) in
	let clone_suspended_vm_enabled = Helpers.clone_suspended_vm_enabled ~__context in
	all, gm, clone_suspended_vm_enabled

let is_operation_valid ~__context ~self ~op =
	let all, gm, clone_suspended_vm_enabled = get_info ~__context ~self in
	match check_operation_error all gm self clone_suspended_vm_enabled op with
	| None   -> true
	| Some _ -> false

let assert_operation_valid ~__context ~self ~op =
	let all, gm, clone_suspended_vm_enabled = get_info ~__context ~self in
	match check_operation_error all gm self clone_suspended_vm_enabled op with
	| None       -> ()
	| Some (a,b) -> raise (Api_errors.Server_error (a,b))

let update_allowed_operations ~__context ~self =
	let all, gm, clone_suspended_vm_enabled = get_info ~__context ~self in
	let check accu op =
		match check_operation_error all gm self clone_suspended_vm_enabled op with
		| None -> op :: accu
		| _    -> accu
	in
	let allowed = 
		List.fold_left check []
			[`snapshot; `copy; `clone; `create_template; `revert; `checkpoint; `snapshot_with_quiesce;
			 `start; `start_on; `pause; `unpause; `clean_shutdown; `clean_reboot;
			`hard_shutdown; `hard_reboot; `suspend; `resume; `resume_on; `export; `destroy;
			`provision; `changing_VCPUs_live; `pool_migrate; `make_into_template; `changing_static_range;
			`changing_dynamic_range]
	in
	(* FIXME: need to be able to deal with rolling-upgrade for orlando as well *)
	let allowed =
		if Helpers.rolling_upgrade_in_progress ~__context
		then Listext.List.intersect allowed Xapi_globs.vm_operations_miami
		else allowed
	in
	Db.VM.set_allowed_operations ~__context ~self ~value:allowed

(** Called on new VMs (clones, imports) and on server start to manually refresh
    the power state, allowed_operations field etc *)
let force_state_reset ~__context ~self ~value:state =
	warn("Forcibly resetting VM state");

	Db.VM.set_power_state ~__context ~self ~value:state;
	if (Db.VM.get_current_operations ~__context ~self)<>[] then
		Db.VM.set_current_operations ~__context ~self ~value:[];
	update_allowed_operations ~__context ~self;

	if state = `Halted then begin
		(* mark all devices as disconnected *)
		List.iter 
			(fun vbd ->
				 Db.VBD.set_currently_attached ~__context ~self:vbd ~value:false;
				 Db.VBD.set_reserved ~__context ~self:vbd ~value:false;)
			(Db.VM.get_VBDs ~__context ~self);
		List.iter 
			(fun vif ->
				 Db.VIF.set_currently_attached ~__context ~self:vif ~value:false;
				 Db.VIF.set_reserved ~__context ~self:vif ~value:false)
			(Db.VM.get_VIFs ~__context ~self);
		Db.VM.set_resident_on ~__context ~self ~value:Ref.null;
		(* make sure we aren't reserving any memory for this VM *)
		Db.VM.set_scheduled_to_be_resident_on ~__context ~self ~value:Ref.null
	end

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
