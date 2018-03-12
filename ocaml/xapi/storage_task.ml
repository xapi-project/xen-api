open Storage_interface

module D=Debug.Make(struct let name=Storage_interface.service_name end)
open D

module Storage_task = Task_server.Task(Storage_interface)
module Updates = Updates.Updates(Storage_interface)

let scheduler = Scheduler.make ()
let updates = Updates.empty scheduler
let tasks = Storage_task.empty ()

let signal id =
  let open Storage_task in
  try
    let handle = handle_of_id tasks id in
    let state = get_state handle in
    debug "TASK.signal %s = %s" id (state |> Task.rpc_of_state |> Jsonrpc.to_string);
    Updates.add (Dynamic.Task id) updates
  with
    Does_not_exist _ -> debug "TASK.signal %s (object deleted)" id
