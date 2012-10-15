open Cohttp_lwt_unix
open Lwt
open Protocol

let port = ref 8080
let name = ref "default_name"

let main () =
	let token = Printf.sprintf "%d" (Unix.getuid ()) in
	lwt c = Connection.make !port token in

	lwt (_: string) = Connection.rpc c (Frame.Bind (Some !name)) in
	Printf.fprintf stderr "Serving requests forever\n%!";
	let rec loop ack_to =
		let timeout = 5. in
		let frame = Frame.Transfer(ack_to, timeout) in
		lwt _ = Connection.rpc c frame in
		(* XXX: read responses *)
		loop ack_to in
	loop ""

let _ =
	Arg.parse [
		"-port", Arg.Set_int port, (Printf.sprintf "port broker listens on (default %d)" !port);
		"-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Respond to RPCs on a name";

	Lwt_unix.run (main ()) 
