
module Xenops_task = Task_server.Task(Xenops_interface)
module Updates = Updates.Updates(Xenops_interface)

let updates = Updates.empty ()
	

