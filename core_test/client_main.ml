open Cohttp_lwt_unix
open Lwt
open Protocol
open Protocol_lwt

let port = ref 8080
let name = ref "server"
let payload = ref "hello"

let main () =
	lwt c = Client.connect !port !name in

	lwt x = Client.rpc c !payload in
	Printf.fprintf stderr "%s\n%!" x;
	return ()

let _ =
	Arg.parse [
		"-port", Arg.Set_int port, (Printf.sprintf "port broker listens on (default %d)" !port);
		"-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
		"-payload", Arg.Set_string payload, (Printf.sprintf "payload of message to send (default %s)" !payload);

	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Send a message to a name, optionally waiting for a response";

	Lwt_unix.run (main ()) 
