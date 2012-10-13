open Cohttp_lwt_unix
open Lwt
open Protocol

let port = ref 8080
let name = ref "default_name"
let payload = ref "hello"
let reply = ref false

let main () =
	lwt ic, oc = Connection.make !port in
	let frame = Frame.Send(!name, !payload) in
	let http_request = Frame.to_request frame in
	lwt () = Request.write (fun _ _ -> return ()) http_request oc in
	match_lwt Response.read ic with
	| Some http_response ->
		if Response.status http_response <> `OK then begin
			Printf.fprintf stderr "Failed to read response\n%!";
			lwt () = Response.write (fun _ _ -> return ()) http_response Lwt_io.stderr in
			return ()
		end else begin
			Printf.fprintf stderr "OK\n%!";
			return ()
		end
	| None ->
		Printf.fprintf stderr "Failed to read response\n%!";
		return ()

let _ =
	Arg.parse [
		"-port", Arg.Set_int port, (Printf.sprintf "port broker listens on (default %d)" !port);
		"-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
		"-payload", Arg.Set_string payload, (Printf.sprintf "payload of message to send (default %s)" !payload);
		"-reply", Arg.Set reply, (Printf.sprintf "wait for a reply (default %b)" !reply);
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Send a message to a name, optionally waiting for a response";

	Lwt_unix.run (main ()) 
