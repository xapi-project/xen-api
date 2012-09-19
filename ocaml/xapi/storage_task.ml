open Threadext
open Fun
open Storage_interface

module D=Debug.Debugger(struct let name=Storage_interface.service_name end)
open D

module Storage_task = Task_server.Task(Storage_interface)
module Updates = Updates.Updates(Storage_interface)

let updates = Updates.empty ()
let tasks = Storage_task.empty ()
	
let signal task =
	let open Storage_task in
	Mutex.execute tasks.m
		(fun () ->
			if exists_locked tasks task then begin
				debug "TASK.signal %s = %s" task ((find_locked tasks task).state |> Task.rpc_of_state |> Jsonrpc.to_string);
				Updates.add (Dynamic.Task task) updates;
			end else debug "TASK.signal %s (object deleted)" task
		)
		
