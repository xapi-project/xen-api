open Xenops_utils

module XI = struct
  include Xenops_interface

  let cancelled s = Cancelled s
  let does_not_exist (x,y) = Does_not_exist (x,y)
  let marshal_exn e =
     e |> exnty_of_exn |> Exception.rpc_of_exnty
end


module Xenops_task = Task_server.Task(XI)
module Updates = Updates.Updates(XI)

let scheduler = Scheduler.make ()
let updates = Updates.empty scheduler
let tasks = Xenops_task.empty ()

let event_wait local_updates task ?from ?timeout_start timeout p =
  let start = match timeout_start with
    | Some s -> s
    | None -> Unix.gettimeofday () in
  let rec inner remaining event_id =
    if (remaining > 0.0) then begin
      let _, deltas, next_id = Updates.get (Printf.sprintf "event_wait task %s" (Xenops_task.id_of_handle task))
          ~with_cancel:(Xenops_task.with_cancel task) event_id (Some (remaining |> ceil |> int_of_float)) local_updates in
      let success = List.fold_left (fun acc d -> acc || (p d)) false deltas in
      let finished = success in
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
  let handle = Xenops_task.handle_of_id tasks id in
  match Xenops_task.get_state handle with
  | Completed _
  | Failed _ -> true
  | Pending _ -> false

let task_finished_p task =
  let open Xenops_interface in
  function
  | Dynamic.Task id -> id=task && (task_ended id)
  | _ -> false
