open Cohttp


module Message = struct
	type t = {
		payload: string; (* switch to Rpc.t *)
		correlation_id: int;
		reply_to: string option;
	} with rpc

	let one_way payload = {
		payload = payload;
		correlation_id = 0;
		reply_to = None;
	}
end

module Event = struct
	type message =
		| Message of int64 * Message.t
		| Ack of int64
	with rpc

	type t = {
		time: float;
		input: string option;
		queue: string;
		output: string option;
		message: message
	} with rpc

end

module In = struct
	type t =
	| Login of string            (** Associate this transport-level channel with a session *)
	| Create of string option    (** Create a queue with a well-known or fresh name *)
	| Subscribe of string        (** Subscribe to messages from a queue *)
	| Send of string * Message.t (** Send a message to a queue *)
	| Transfer of int64 * float  (** blocking wait for new messages *)
	| Trace of int64 * float     (** blocking wait for trace data *)
	| Ack of int64               (** ACK this particular message *)
	| List of string             (** return a list of queue names with a prefix *)
	| Diagnostics                (** return a diagnostic dump *)
	| Get of string list         (** return a web interface resource *)
	with rpc

	let slash = Re_str.regexp_string "/"
	let split = Re_str.split_delim slash

	let of_request body meth path =
		match body, meth, split path with
		| None, `GET, "" :: "admin" :: path     -> Some (Get path)
		| None, `GET, "" :: ((("js" | "css" | "images") :: _) as path) -> Some (Get path)
		| None, `GET, [ ""; "" ]                -> Some Diagnostics
		| None, `GET, [ ""; "login"; token ]    -> Some (Login token)
		| None, `GET, [ ""; "create" ]          -> Some (Create None)
		| None, `GET, [ ""; "create"; name ]    -> Some (Create (Some name))
		| None, `GET, [ ""; "subscribe"; name ] -> Some (Subscribe name)
		| None, `GET, [ ""; "ack"; id ]         -> Some (Ack (Int64.of_string id))
		| None, `GET, [ ""; "list"; prefix ]    -> Some (List prefix)
		| None, `GET, [ ""; "transfer"; ack_to; timeout ] ->
			Some (Transfer(Int64.of_string ack_to, float_of_string timeout))
		| None, `GET, [ ""; "trace"; ack_to; timeout ] ->
			Some (Trace(Int64.of_string ack_to, float_of_string timeout))
		| None, `GET, [ ""; "trace" ] ->
			Some (Trace(-1L, 5.))
		| Some body, `POST, [ ""; "send"; name; correlation_id ] ->
			Some (Send (name, { Message.correlation_id = int_of_string correlation_id; reply_to = None; payload = body }))
		| Some body, `POST, [ ""; "send"; name; correlation_id; reply_to ] ->
			Some (Send (name, { Message.correlation_id = int_of_string correlation_id; reply_to = Some reply_to; payload = body }))
		| _, _, _ -> None

	let headers payload =
		Header.of_list [
            "user-agent", "cohttp";
            "content-length", string_of_int (String.length payload);
            "connection", "keep-alive";
        ]


	let to_request = function
		| Login token ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/login/%s" token) ())
		| Create None ->
			None, `GET, (Uri.make ~path:"/create" ())
		| Create (Some name) ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/create/%s" name) ())
		| Subscribe name ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/subscribe/%s" name) ())
		| Ack x ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/ack/%Ld" x) ())
		| List x ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/list/%s" x) ())
		| Transfer(ack_to, timeout) ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/transfer/%Ld/%.16g" ack_to timeout) ())
		| Trace(ack_to, timeout) ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/trace/%Ld/%.16g" ack_to timeout) ())
		| Send (name, { Message.correlation_id = c; reply_to = None; payload = p }) ->
			Some p, `POST, (Uri.make ~path:(Printf.sprintf "/send/%s/%d" name c) ())
		| Send (name, { Message.correlation_id = c; reply_to = Some r; payload = p }) ->
			Some p, `POST, (Uri.make ~path:(Printf.sprintf "/send/%s/%d/%s" name c r) ())
		| Diagnostics ->
			None, `GET, (Uri.make ~path:"/" ())
		| Get path ->
			None, `GET, (Uri.make ~path:(String.concat "/" ("" :: "admin" :: path)) ())
end

type origin =
	| Anonymous of int (** An un-named connection, probably a temporary client connection *)
	| Name of string   (** A service with a well-known name *)
with rpc
(** identifies where a message came from *)

module Entry = struct
	type t = {
		origin: origin;
		time: int64;
		message: Message.t;
	} with rpc
	(** an enqueued message *)

	let make time origin message =
		{ origin; time; message }
end

type message_id = int64 with rpc
(** uniquely identifier for this message *)

module Diagnostics = struct
	type queue_contents = (message_id * Entry.t) list with rpc

	type queue = {
		next_transfer_expected: int64 option;
		queue_contents: queue_contents;
	} with rpc

	type t = {
		start_time: int64;
		current_time: int64;
		permanent_queues: (string * queue) list;
		transient_queues: (string * queue) list;
	}
	with rpc
end

module Out = struct
	type transfer = {
		messages: (message_id * Message.t) list;
	} with rpc

	type trace = {
		events: (int64 * Event.t) list;
	} with rpc

	type queue_list = string list with rpc
	let rpc_of_string_list = rpc_of_queue_list
	let string_list_of_rpc = queue_list_of_rpc

	type t =
	| Login
	| Create of string
	| Subscribe
	| Send
	| Transfer of transfer
	| Trace of trace
	| Ack
	| List of string list
	| Diagnostics of Diagnostics.t
	| Not_logged_in
	| Get of string

	let to_response = function
		| Login
		| Ack
		| Subscribe
		| Send ->
			`OK, ""
		| Create name ->
			`OK, name
		| Transfer transfer ->
			`OK, (Jsonrpc.to_string (rpc_of_transfer transfer))
		| Trace trace ->
			`OK, (Jsonrpc.to_string (rpc_of_trace trace))
		| List l ->
			`OK, (Jsonrpc.to_string (rpc_of_queue_list l))
		| Diagnostics x ->
			`OK, (Jsonrpc.to_string (Diagnostics.rpc_of_t x))
		| Not_logged_in ->
			`Not_found, "Please log in."
		| Get x ->
			`OK, x
end

exception Failed_to_read_response

exception Unsuccessful_response

type ('a, 'b) result =
| Ok of 'a
| Error of 'b

module Connection = functor(IO: Cohttp.IO.S) -> struct
	open IO
	module Request = Cohttp.Request.Make(IO)
	module Response = Cohttp.Response.Make(IO)

	let rpc (ic, oc) frame =
		let b, meth, uri = In.to_request frame in
		let body = match b with None -> "" | Some x -> x in
		let headers = In.headers body in
		let req = Request.make ~meth ~headers uri in
		Request.write (fun req oc -> match b with
		| Some body ->
			Request.write_body req oc body
		| None -> return ()
		) req oc >>= fun () ->

		Response.read ic >>= function
		| Some response ->
			if Response.status response <> `OK then begin
				Printf.fprintf stderr "Server sent: %s\n%!" (Cohttp.Code.string_of_status (Response.status response));
				(* Response.write (fun _ _ -> return ()) response Lwt_io.stderr >>= fun () -> *)
				return (Error Unsuccessful_response)
			end else begin
				Response.read_body_chunk response ic >>= function
				| Transfer.Final_chunk x -> return (Ok x)
				| Transfer.Chunk x -> return (Ok x)
				| Transfer.Done -> return (Ok "")
			end
		| None ->
			Printf.fprintf stderr "Empty response\n%!";
			return (Error Failed_to_read_response)
end

module Server = functor(IO: Cohttp.IO.S) -> struct

	module Connection = Connection(IO)

	let listen process c name =
		let open IO in
		let token = Printf.sprintf "%d" (Unix.getpid ()) in
		Connection.rpc c (In.Login token) >>= fun _ ->
		Connection.rpc c (In.Create (Some name)) >>= fun _ ->
		Connection.rpc c (In.Subscribe name) >>= fun _ ->
		Printf.fprintf stdout "Serving requests forever\n%!";

		let rec loop from =
			let timeout = 5. in
			let frame = In.Transfer(from, timeout) in
			Connection.rpc c frame >>= function
			| Error e ->
				Printf.fprintf stderr "Server.listen.loop: %s\n%!" (Printexc.to_string e);
				return ()
			| Ok raw ->
				let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
				begin match transfer.Out.messages with
				| [] -> loop from
				| m :: ms ->
					iter
						(fun (i, m) ->
							process m.Message.payload >>= fun response ->
							begin
								match m.Message.reply_to with
								| None ->
									return ()
								| Some reply_to ->
									let request = In.Send(reply_to, { m with Message.reply_to = None; payload = response }) in
									Connection.rpc c request >>= fun _ ->
									return ()
					 		end >>= fun () ->
							let request = In.Ack i in
							Connection.rpc c request >>= fun _ ->
							return ()
						) transfer.Out.messages >>= fun () ->
					let from = List.fold_left max (fst m) (List.map fst ms) in
					loop from
				end in
		loop (-1L)
end


let fresh_correlation_id =
	let counter = ref 0 in
	fun () ->
		let result = !counter in
		incr counter;
		result

