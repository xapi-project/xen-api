open Cohttp_lwt_unix
open Lwt
open Protocol

let port = ref 8080
let name = ref "server"

let process x = return x

let main () =
	Protocol_lwt.Server.listen process !port !name

let _ =
	Arg.parse [
		"-port", Arg.Set_int port, (Printf.sprintf "port broker listens on (default %d)" !port);
		"-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Respond to RPCs on a name";

	Lwt_unix.run (main ()) 
