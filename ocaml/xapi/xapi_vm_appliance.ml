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

open Client
open Pervasiveext
open Fun
open Listext

module D = Debug.Debugger(struct let name="xapi" end)
open D

module Int64Map = Map.Make(struct type t = int64 let compare = compare end)
module TaskSet = Set.Make(struct type t = API.ref_task let compare = compare end)

type appliance_operation = {
	name : string;
	vm_operation : (API.ref_VM -> (Xml.xml -> Xml.xml) -> API.ref_session -> API.ref_task);
	required_state : API.vm_power_state;
}

let assert_operation_valid = Xapi_vm_appliance_lifecycle.assert_operation_valid

let update_allowed_operations = Xapi_vm_appliance_lifecycle.update_allowed_operations

let create ~__context ~name_label ~name_description =
	let uuid = Uuid.make_uuid () in
	let ref = Ref.make() in
	Db.VM_appliance.create ~__context ~ref ~uuid:(Uuid.to_string uuid) ~name_label ~name_description ~allowed_operations:[] ~current_operations:[];
	update_allowed_operations ~__context ~self:ref;
	ref

let destroy ~__context ~self =
	Db.VM_appliance.destroy ~__context ~self

(* Takes a list of VMs and returns a map binding each boot order *)
(* found in the list to a list of VMs with that boot order. *)
let group_vms_by_order ~__context vms =
	List.fold_left (fun map vm ->
		let order = Db.VM.get_order ~__context ~self:vm in
		let existing = if Int64Map.mem order map then Int64Map.find order map else [] in
		Int64Map.add order (vm::existing) map) Int64Map.empty vms

(* Return a list of lists of VMs where each list contains *)
(* VMs with the same boot order. *)
let create_action_list ~__context start vms =
	let order_map = group_vms_by_order ~__context vms in
	(if start then List.rev else (fun x -> x))
		(Int64Map.fold (fun _ vms groups -> vms::groups) order_map [])

(* Return once none of the tasks have a `pending status. *)
let wait_for_all_tasks ~rpc ~session_id ~tasks =
	let classes = ["task"] in
	let timeout = 5.0 in
	let rec wait ~token ~task_set =
		if TaskSet.is_empty task_set then ()
		else begin
			let open Event_types in
			let event_from = Client.Event.from ~rpc ~session_id ~classes ~token ~timeout |> event_from_of_xmlrpc in
			let records = List.map Event_helper.record_of_event event_from.events in
			(* If any records indicate that a task is no longer pending, remove that task from the set. *)
			let pending_task_set = List.fold_left (fun task_set' record ->
				match record with
				| Event_helper.Task (t, Some t_rec) ->
					if (TaskSet.mem t task_set') && (t_rec.API.task_status <> `pending) then
						TaskSet.remove t task_set'
					else
						task_set'
				| _ -> task_set') task_set records in
			wait ~token:(string_of_token event_from.token) ~task_set:pending_task_set
		end
	in
	let token = "" in
	let task_set = List.fold_left (fun task_set' task -> TaskSet.add task task_set') TaskSet.empty tasks in
	wait ~token ~task_set;
	let failed_tasks = List.filter (fun task -> Client.Task.get_status ~rpc ~session_id ~self:task <> `success) tasks in
	failed_tasks

(* Run the given operation on all VMs in the list, and record the tasks created. *)
(* Return once all the tasks have completed, with a list of VMs which threw an exception. *)
let run_operation_on_vms ~__context operation vms =
	Helpers.call_api_functions ~__context (fun rpc session_id ->
		let (tasks, failed_vms) = List.fold_left (fun (tasks, failed_vms) vm ->
			try
				let task = operation vm rpc session_id in
				(task::tasks, failed_vms)
			with e ->
				(tasks, vm::failed_vms)) ([], []) vms in
		let failed_tasks = wait_for_all_tasks ~rpc ~session_id ~tasks in
	(* These two values could be used to determine which VMs have failed without having to check at the end. *)
		ignore (failed_vms, failed_tasks);
		())

let perform_operation ~__context ~self ~operation ~ascending_priority =
	let appliance_uuid = (Db.VM_appliance.get_uuid ~__context ~self) in
	let contained_vms = Db.VM_appliance.get_VMs ~__context ~self in
	(* Obtain a list of VMs which are not already in the required power state. *)
	let target_vms = List.filter (fun vm -> Db.VM.get_power_state ~__context ~self:vm <> operation.required_state) contained_vms in
	let action_list = create_action_list ~__context ascending_priority target_vms in
	debug "Beginning operation %s on appliance %s" operation.name appliance_uuid;
	List.iter (fun vm_list -> run_operation_on_vms ~__context operation.vm_operation vm_list) action_list;
	(* Check whether all the VMs have transitioned to the required power state. *)
	let failed_vms = List.filter (fun vm -> Db.VM.get_power_state ~__context ~self:vm <> operation.required_state) target_vms in
	match failed_vms with
	| [] -> debug "Operation %s on appliance with uuid %s completed successfully" operation.name appliance_uuid
	| _ ->
		debug "Operation %s on appliance with uuid %s partially failed" operation.name appliance_uuid;
		raise (Api_errors.Server_error(Api_errors.operation_partially_failed,
			operation.name::(List.map Ref.string_of failed_vms)))

let start ~__context ~self ~paused =
	let operation = {
		name = "VM_appliance.start";
		vm_operation = (fun vm rpc session_id -> Client.Async.VM.start ~rpc ~session_id ~vm ~start_paused:paused ~force:false);
		required_state = if paused then `Paused else `Running;
	} in
	perform_operation ~__context ~self ~operation ~ascending_priority:true

let clean_shutdown ~__context ~self =
	let operation = {
		name = "VM_appliance.clean_shutdown";
		vm_operation = (fun vm rpc session_id -> Client.Async.VM.clean_shutdown ~rpc ~session_id ~vm);
		required_state = `Halted;
	} in
	perform_operation ~__context ~self ~operation ~ascending_priority:false

let hard_shutdown ~__context ~self =
	let operation = {
		name = "VM_appliance.hard_shutdown";
		vm_operation = (fun vm rpc session_id -> Client.Async.VM.hard_shutdown ~rpc ~session_id ~vm);
		required_state = `Halted;
	} in
	perform_operation ~__context ~self ~operation ~ascending_priority:false

let shutdown ~__context ~self = hard_shutdown ~__context ~self

(* Check that VDI SRs are present for each VM in the appliance. *)
let assert_can_be_recovered ~__context ~self ~session_to =
	let vms = Db.VM_appliance.get_VMs ~__context ~self in
	List.iter
		(fun vm -> Xapi_vm_helpers.assert_can_be_recovered ~__context ~self:vm ~session_to)
		vms

let recover ~__context ~self ~session_to ~force =
	Xapi_dr.assert_session_allows_dr ~session_id:session_to ~action:"VM_appliance.recover";
	assert_can_be_recovered ~__context ~self ~session_to;
	let vms = Db.VM_appliance.get_VMs ~__context ~self in
	let recovered_vms = Xapi_dr.recover_vms ~__context ~vms ~session_to ~force in
	(* Deal with the VM appliance object. *)
	let old_appliance = Db.VM_appliance.get_record ~__context ~self in
	Server_helpers.exec_with_new_task ~session_id:session_to "Recreating VM appliance object"
		(fun __context_to ->
			let recovered_appliance = try
				(* If an appliance with the same UUID exists, remove all VMs from the appliance and update its name_label/name_description. *)
				let existing_appliance = Db.VM_appliance.get_by_uuid ~__context:__context_to ~uuid:old_appliance.API.vM_appliance_uuid in
				debug "An appliance with UUID %s already exists - reusing it." old_appliance.API.vM_appliance_uuid;
				let vms = Db.VM_appliance.get_VMs ~__context:__context_to ~self:existing_appliance in
				List.iter
					(fun vm -> Db.VM.set_appliance ~__context:__context_to ~self:vm ~value:Ref.null)
					vms;
				Db.VM_appliance.set_name_label ~__context:__context_to ~self:existing_appliance ~value:old_appliance.API.vM_appliance_name_label;
				Db.VM_appliance.set_name_description ~__context:__context_to ~self:existing_appliance ~value:old_appliance.API.vM_appliance_name_description;
				existing_appliance
			with Db_exn.Read_missing_uuid("VM_appliance", _, _) ->
				(* If no appliance with the same UUID exists, create a new one from the old appliance's data. *)
				debug "No appliance with UUID %s exists - creating a new one." old_appliance.API.vM_appliance_uuid;
				begin
					let new_appliance = create ~__context:__context_to
						~name_label:old_appliance.API.vM_appliance_name_label
						~name_description:old_appliance.API.vM_appliance_name_description in
					Db.VM_appliance.set_uuid ~__context:__context_to
						~self:new_appliance
						~value:old_appliance.API.vM_appliance_uuid;
					new_appliance
				end
			in
			(* Add all the non-template VMs to the appliance. *)
			List.iter
				(fun vm ->
					if not (Db.VM.get_is_a_template ~__context:__context_to ~self:vm) then
						Db.VM.set_appliance ~__context:__context_to ~self:vm ~value:recovered_appliance)
				recovered_vms;
			update_allowed_operations ~__context:__context_to ~self:recovered_appliance)
