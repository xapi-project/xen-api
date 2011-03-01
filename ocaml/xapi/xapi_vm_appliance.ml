open Client
open Pervasiveext

module D = Debug.Debugger(struct let name="xapi" end)
open D

module Int64Map = Map.Make(struct type t = int64 let compare = compare end)
module TaskSet = Set.Make(struct type t = API.ref_task let compare = compare end)

type appliance_operation = {
	name : string;
	vm_operation : (API.ref_VM -> API.ref_task);
	required_state : [ `Halted | `Paused | `Running | `Suspended ];
}

let create ~__context ~name_label ~name_description =
	let uuid = Uuid.make_uuid () in
	let ref = Ref.make() in
	Db.VM_appliance.create ~__context ~ref ~uuid:(Uuid.to_string uuid) ~name_label ~name_description ~allowed_operations:[] ~current_operations:[];
	ref

let destroy ~__context ~self =
	Db.VM_appliance.destroy ~__context ~self

(* Checks to see if an operation is valid in this state. Returns Some exception *)
(* if not and None if everything is ok. *)
let check_operation_error ~__context record _ref' op =
	let _ref = Ref.string_of _ref' in
	let current_ops = record.Db_actions.vM_appliance_current_operations in
	(* Only allow one operation of [`start | `clean_shutdown | `hard_shutdown ] at a time. *)
	if List.length current_ops > 0 then
		Some (Api_errors.other_operation_in_progress, ["VM_appliance"; _ref])
	else
		None

let assert_operation_valid ~__context ~self ~(op:API.vm_appliance_operation) =
	let all = Db.VM_appliance.get_record_internal ~__context ~self in
	match check_operation_error ~__context all self op with
	| None -> ()
	| Some (a,b) -> raise (Api_errors.Server_error (a,b))

let update_allowed_operations ~__context ~self =
	let all = Db.VM_appliance.get_record_internal ~__context ~self in
	let allowed_ops =
		let allowed x = match check_operation_error ~__context all self x with None -> true | _ -> false in
		List.filter allowed [`start; `clean_shutdown; `hard_shutdown] in
	Db.VM_appliance.set_allowed_operations ~__context ~self ~value:allowed_ops

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
	let rec wait ~task_set ~registered =
		if (TaskSet.is_empty task_set) then
			(debug "Task set is empty - returning";
			if registered then Client.Event.unregister ~rpc ~session_id ~classes else ())
		else
			(debug "Waiting for tasks [%s]"
				(String.concat ";" (List.map (fun task -> Client.Task.get_uuid rpc session_id task ) (TaskSet.elements task_set)));
			if registered then
				(* Try to get all the task events - if events are lost we need to reregister. *)
				try
					let events = Event_types.events_of_xmlrpc (Client.Event.next ~rpc ~session_id) in
					let records = List.map Event_helper.record_of_event events in
					(* If any records indicate that a task is no longer pending, *)
					(* remove that task from the set. *)
					let pending_task_set = List.fold_left (fun task_set' record ->
						match record with
						| Event_helper.Task (t, t_rec) ->
							if (TaskSet.mem t task_set') && (t_rec.API.task_status <> `pending) then
								TaskSet.remove t task_set'
							else
								task_set'
						| _ -> task_set') task_set records in
					wait ~task_set:pending_task_set ~registered:true
				with Api_errors.Server_error(code, _) when code = Api_errors.events_lost ->
					(* Client.Event.next threw an exception. *)
					debug "Caught EVENTS_LOST; reregistering";
					Client.Event.unregister ~rpc ~session_id ~classes;
					wait ~task_set ~registered:false
			else
				(debug "Not registered with event system - registering.";
				Client.Event.register ~rpc ~session_id ~classes;
				(* Check which tasks currently have status `pending - if none do then we can return. *)
				(* We need to check task status after evey registration to avoid a race. *)
				let pending_task_set = TaskSet.filter (fun task ->
					let status = Client.Task.get_status ~rpc ~session_id ~self:task in
					debug "Task %s has status %s" (Client.Task.get_uuid rpc session_id task) (Record_util.task_status_type_to_string status);
					status = `pending) task_set in
				wait ~task_set:pending_task_set ~registered:true))
	in
	(* Generate a set containing all the tasks. *)
	let task_set = List.fold_left (fun task_set' task -> TaskSet.add task task_set') TaskSet.empty tasks in
	wait ~task_set ~registered:false;
	let failed_tasks = List.filter (fun task -> Client.Task.get_status ~rpc ~session_id ~self:task <> `success) tasks in
	failed_tasks

(* Run the given operation on all VMs in the list, and record the tasks created. *)
(* Return once all the tasks have completed, with a list of VMs which threw an exception. *)
let run_operation_on_vms ~__context operation vms =
	let (tasks, failed_vms) = List.fold_left (fun (tasks, failed_vms) vm ->
		try
			let task = operation vm in
			(task::tasks, failed_vms)
		with e ->
			(tasks, vm::failed_vms)) ([], []) vms in
	let failed_tasks = Helpers.call_api_functions ~__context (fun rpc session_id ->
		wait_for_all_tasks ~rpc ~session_id ~tasks) in
	(* These two values could be used to determine which VMs have failed without having to check at the end. *)
	ignore (failed_vms, failed_tasks);
	()

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
		vm_operation = (fun vm -> Helpers.call_api_functions ~__context
			(fun rpc session_id -> Client.Async.VM.start ~rpc ~session_id ~vm ~start_paused:paused ~force:false));
		required_state = if paused then `Paused else `Running;
	} in
	perform_operation ~__context ~self ~operation ~ascending_priority:true

let clean_shutdown ~__context ~self =
	let operation = {
		name = "VM_appliance.clean_shutdown";
		vm_operation = (fun vm -> Helpers.call_api_functions ~__context
			(fun rpc session_id -> Client.Async.VM.clean_shutdown ~rpc ~session_id ~vm));
		required_state = `Halted;
	} in
	perform_operation ~__context ~self ~operation ~ascending_priority:false

let hard_shutdown ~__context ~self =
	let operation = {
		name = "VM_appliance.hard_shutdown";
		vm_operation = (fun vm -> Helpers.call_api_functions ~__context
			(fun rpc session_id -> Client.Async.VM.hard_shutdown ~rpc ~session_id ~vm));
		required_state = `Halted;
	} in
	perform_operation ~__context ~self ~operation ~ascending_priority:false
