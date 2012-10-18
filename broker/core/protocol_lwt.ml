open Protocol
open Lwt
open Cohttp
open Cohttp_lwt_unix

module Connection = struct
	type t = Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel

	exception Failed_to_read_response

	exception Unsuccessful_response

	let rpc (ic, oc) frame =
		let req, body = In.to_request frame in
		lwt () = Request.write (fun req oc -> match body with
		| Some body ->
			Request.write_body req oc body
		| None -> return ()
		) req oc in
		lwt () = Lwt_io.flush oc in

		match_lwt Response.read ic with
		| Some response ->
			if Response.status response <> `OK then begin
				Printf.fprintf stderr "Failed to read response\n%!";
				lwt () = Response.write (fun _ _ -> return ()) response Lwt_io.stderr in
				fail Unsuccessful_response
			end else begin
				match_lwt Response.read_body response ic with
				| Transfer.Final_chunk x -> return x
				| Transfer.Chunk x -> return x
				| Transfer.Done -> return ""
			end
		| None ->
			Printf.fprintf stderr "Failed to read response\n%!";
			fail Failed_to_read_response

	let make port token =
		let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", port) in
		let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
		lwt () = Lwt_unix.connect fd sockaddr in
		let ic = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.input fd in
		let oc = Lwt_io.of_fd ~close:(fun () -> return ()) ~mode:Lwt_io.output fd in
		let c = ic, oc in
		lwt _ = rpc c (In.Login token) in
		return c

end

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

module Server = struct

	let listen process port name =
		let token = Printf.sprintf "%d" (Unix.getpid ()) in
		lwt c = Connection.make port token in

		lwt (_: string) = Connection.rpc c (In.Create (Some name)) in
		lwt (_: string) = Connection.rpc c (In.Subscribe name) in
		Printf.fprintf stdout "Serving requests forever\n%!";

		let rec loop from =
			let timeout = 5. in
			let frame = In.Transfer(from, timeout) in
			lwt raw = Connection.rpc c frame in
			let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
			match transfer.Out.messages with
			| [] -> loop from
			| m :: ms ->
				lwt () = Lwt_list.iter_s
					(fun (i, m) ->
						lwt response = process m.Message.payload in
						lwt () =
							match m.Message.reply_to with
							| None ->
								Printf.fprintf stderr "No reply_to\n%!";
								return ()
							| Some reply_to ->
								Printf.fprintf stderr "Sending reply to %s\n%!" reply_to;
								let request = In.Send(reply_to, { m with Message.reply_to = None; payload = response }) in
								lwt (_: string) = Connection.rpc c request in
								return () in
						let request = In.Ack i in
						lwt (_: string) = Connection.rpc c request in
						return ()
					) transfer.Out.messages in
				let from = List.fold_left max (fst m) (List.map fst ms) in
				loop from in
		loop (-1L)
end
