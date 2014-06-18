open Xenops_utils

module Xenops_task = Task_server.Task(Xenops_interface)
module Updates = Updates.Updates(Xenops_interface)

let updates = Updates.empty ()
let tasks = Xenops_task.empty ()

let event_wait local_updates task ?from timeout p =
	let start = Unix.gettimeofday () in
	let rec inner remaining event_id =
		if (remaining > 0.0) then begin
			let _, deltas, next_id = Updates.get (Printf.sprintf "event_wait task %s" task.Xenops_task.id)
				~with_cancel:(Xenops_task.with_cancel task) event_id (Some (remaining |> ceil |> int_of_float)) local_updates in
			let success = List.fold_left (fun acc d -> acc || (p d)) false deltas in
			let finished = success || deltas = [] in
			if not finished
			then
				let elapsed = Unix.gettimeofday () -. start in
				inner (timeout -. elapsed) (Some next_id)
			else
				success
		end else false
	in
	let result = inner timeout from in
	Xenops_task.check_cancelling task;
	result

let task_ended id =
  let open Xenops_interface.Task in
  match Mutex.execute tasks.Xenops_task.m (fun () -> Xenops_task.((find_locked tasks id).state)) with
  | Completed _
  | Failed _ -> true
  | Pending _ -> false

let task_finished_p task =
  let open Xenops_interface in
  function
  | Dynamic.Task id -> id=task && (task_ended id)
  | _ -> false
