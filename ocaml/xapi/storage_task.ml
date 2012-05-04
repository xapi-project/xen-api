
module Storage_task = Task_server.Task(Storage_interface)
module Updates = Updates.Updates(Storage_interface)

let updates = Updates.empty ()

