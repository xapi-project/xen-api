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

open Client

module TaskSet = Set.Make(struct type t = API.ref_task let compare = compare end)

(* Return once none of the tasks have a `pending status. *)
let wait_for_all ~rpc ~session_id ~tasks =
	let classes = List.map
		(fun task -> Printf.sprintf "task/%s" (Ref.string_of task))
		tasks
	in
	let timeout = 5.0 in
	let rec wait ~token ~task_set =
		if TaskSet.is_empty task_set then ()
		else begin
			let open Event_types in
			let event_from_rpc = Client.Event.from ~rpc ~session_id ~classes ~token ~timeout in
			let event_from = Event_types.event_from_of_rpc event_from_rpc in
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
			wait ~token:(event_from.Event_types.token) ~task_set:pending_task_set
		end
	in
	let token = "" in
	let task_set = List.fold_left (fun task_set' task -> TaskSet.add task task_set') TaskSet.empty tasks in
	wait ~token ~task_set

