open Xal

let domain ty ctx domid = 
	Printf.printf "%d: %s\n" domid ty; flush stdout

let device ctx domid dev_event = 
	Printf.printf "%d: %s\n" domid (string_of_dev_event dev_event); flush stdout

let _ =
	Arg.parse [] (fun x -> print_endline ("Ignoring: " ^ x))
	  "XAL test program";
	loop ~callback_devices:device ()
