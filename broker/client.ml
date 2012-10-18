open Cohttp_lwt_unix
open Lwt
open Protocol

let port = ref 8080
let name = ref "server"
let payload = ref "hello\r\n"

module Client = struct
	type t = {
		requests_conn: Connection.t;
		events_conn: Connection.t;
		requests_m: Lwt_mutex.t;
		wakener: (int, Message.t Lwt.u) Hashtbl.t;
		reply_queue_name: string;
	}

	let fresh_correlation_id =
		let counter = ref 0 in
		fun () ->
			let result = !counter in
			incr counter;
			result

	let connect port =
		let token = Printf.sprintf "%d" (Unix.getpid ()) in
		lwt requests_conn = Connection.make port token
		and events_conn = Connection.make port token in
 
		let wakener = Hashtbl.create 10 in
		let (_ : unit Lwt.t) =
			let rec loop from =
				let timeout = 5. in
				let frame = In.Transfer(from, timeout) in
				lwt raw = Connection.rpc events_conn frame in
				let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
				match transfer.Out.messages with
				| [] -> loop from
				| m :: ms ->
					List.iter
						(fun (i, m) ->
							if Hashtbl.mem wakener m.Message.correlation_id
							then Lwt.wakeup_later (Hashtbl.find wakener m.Message.correlation_id) m;
						) transfer.Out.messages;
					let from = List.fold_left max (fst m) (List.map fst ms) in
					loop from in
			loop (-1L) in
		lwt reply_queue_name = Connection.rpc requests_conn (In.Create None) in
		lwt (_: string) = Connection.rpc requests_conn (In.Subscribe reply_queue_name) in
		return {
			requests_conn = requests_conn;
			events_conn = events_conn;
			requests_m = Lwt_mutex.create ();
			wakener = wakener;
			reply_queue_name = reply_queue_name;
		}

	let rpc c name x =
		let correlation_id = fresh_correlation_id () in
		let t, u = Lwt.task () in
		Hashtbl.add c.wakener correlation_id u;
		let msg = In.Send(name, {
			Message.payload = x;
			correlation_id;
			reply_to = Some c.reply_queue_name
		}) in
		lwt (_: string) = Connection.rpc c.requests_conn msg in
		lwt response = t in
		return response.Message.payload
end

let main () =
	lwt c = Client.connect !port in

	lwt x = Client.rpc c !name !payload in
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
