open Protocol
open Lwt
open Cohttp
open Cohttp_lwt_unix

module IO = struct
	type 'a t = 'a Lwt.t
	let ( >>= ) = Lwt.bind
	let return = Lwt.return

	type ic = Lwt_io.input Lwt_io.channel
	type oc = Lwt_io.output Lwt_io.channel

	let iter fn x = Lwt_list.iter_s fn x

	let read_line = Lwt_io.read_line_opt
	let read ic count =
		try_lwt Lwt_io.read ~count ic
		with End_of_file -> return ""
	let read_exactly ic buf off len =
		try_lwt
			lwt () = Lwt_io.read_into_exactly ic buf off len in
			return true
		with End_of_file -> return false
	let write = Lwt_io.write
	let write_line = Lwt_io.write_line

	let connect port =
		let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", port) in
		let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
		lwt () = Lwt_unix.connect fd sockaddr in
		let ic = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.input fd in
		let oc = Lwt_io.of_fd ~close:(fun () -> return ()) ~mode:Lwt_io.output fd in
		return (ic, oc)
end

module Connection = Protocol.Connection(IO)

module Client = struct
	type t = {
		requests_conn: (IO.ic * IO.oc);
		events_conn: (IO.ic * IO.oc);
		requests_m: Lwt_mutex.t;
		wakener: (int, Message.t Lwt.u) Hashtbl.t;
		dest_queue_name: string;
		reply_queue_name: string;
	}

	let fresh_correlation_id =
		let counter = ref 0 in
		fun () ->
			let result = !counter in
			incr counter;
			result

	let rpc c frame = match_lwt Connection.rpc c frame with
		| Error e -> fail e
		| Ok raw -> return raw

	let connect port dest_queue_name =
		let token = Printf.sprintf "%d" (Unix.getpid ()) in
		lwt requests_conn = IO.connect port in
		lwt (_: string) = rpc requests_conn (In.Login token) in
		lwt events_conn = IO.connect port in
		lwt (_: string) = rpc events_conn (In.Login token) in

		let wakener = Hashtbl.create 10 in
		let (_ : unit Lwt.t) =
			let rec loop from =
				let timeout = 5. in
				let frame = In.Transfer(from, timeout) in
				lwt raw = rpc events_conn frame in
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
		lwt reply_queue_name = rpc requests_conn (In.Create None) in
		lwt (_: string) = rpc requests_conn (In.Subscribe reply_queue_name) in
		lwt (_: string) = rpc requests_conn (In.Create (Some dest_queue_name)) in
		return {
			requests_conn = requests_conn;
			events_conn = events_conn;
			requests_m = Lwt_mutex.create ();
			wakener = wakener;
			dest_queue_name = dest_queue_name;
			reply_queue_name = reply_queue_name;
		}

	let rpc c x =
		let correlation_id = fresh_correlation_id () in
		let t, u = Lwt.task () in
		Hashtbl.add c.wakener correlation_id u;
		let msg = In.Send(c.dest_queue_name, {
			Message.payload = x;
			correlation_id;
			reply_to = Some c.reply_queue_name
		}) in
		lwt (_: string) = rpc c.requests_conn msg in
		lwt response = t in
		return response.Message.payload
end

module Server = Protocol.Server(IO)
