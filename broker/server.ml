open Cohttp_lwt_unix
open Lwt
open Protocol

let port = ref 8080
let name = ref "default_name"

let main () =
	let token = Printf.sprintf "%d" (Unix.getuid ()) in
	lwt c = Connection.make !port token in

	lwt (_: string) = Connection.rpc c (In.Bind (Some !name)) in
	Printf.fprintf stderr "Serving requests forever\n%!";
	let rec loop ack_to =
		let timeout = 5. in
		let frame = In.Transfer(ack_to, timeout) in
		lwt raw = Connection.rpc c frame in
		let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
		match transfer.Out.messages with
		| [] -> loop ack_to
		| m :: ms ->
			lwt () = Lwt_list.iter_s
				(fun (i, m) ->
					match m.Message.reply_to with
					| None -> return ()
					| Some reply_to ->
						let request = In.Send(reply_to, { m with Message.reply_to = None }) in
						lwt (_: string) = Connection.rpc c request in
						return ()
				) transfer.Out.messages in
			let ack_to = Int64.to_string (List.fold_left max (fst m) (List.map fst ms)) in
			loop ack_to in
	loop "-1"

let _ =
	Arg.parse [
		"-port", Arg.Set_int port, (Printf.sprintf "port broker listens on (default %d)" !port);
		"-name", Arg.Set_string name, (Printf.sprintf "name to send message to (default %s)" !name);
	] (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
		"Respond to RPCs on a name";

	Lwt_unix.run (main ()) 
