open Storage_interface

module D=Debug.Make(struct let name=Storage_interface.service_name end)
open D

module SI = struct
  include Storage_interface

  let cancelled s = Storage_error (Cancelled s)
  let does_not_exist (x,y) = Storage_error (Does_not_exist (x,y))
  let marshal_exn e = match e with
    | Storage_error e -> Rpcmarshal.marshal Errors.error.Rpc.Types.ty e
    | _ -> raise e
end

module Storage_task = Task_server.Task(SI)
module Updates = Updates.Updates(SI)

let scheduler = Scheduler.make ()
let updates = Updates.empty scheduler
let tasks = Storage_task.empty ()

let signal id =
  let open Storage_task in
  try
    let handle = handle_of_id tasks id in
    let state = get_state handle in
    debug "TASK.signal %s = %s" id (state |> rpc_of Task.state |> Jsonrpc.to_string);
    Updates.add (Dynamic.Task id) updates
  with
    Storage_error Does_not_exist _ -> debug "TASK.signal %s (object deleted)" id
