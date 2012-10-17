open Cohttp
open Cohttp_lwt_unix
open Lwt


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

module In = struct
	type t =
	| Login of string            (** Associate this transport-level channel with a session *)
	| Create of string option    (** Create a queue with a well-known or fresh name *)
	| Subscribe of string        (** Subscribe to messages from a queue *)
	| Send of string * Message.t (** Send a message to a queue *)
	| Transfer of int64 * float  (** blocking wait for new messages *)
	| Ack of int64               (** ACK this particular message *)
	| Diagnostics                (** return a diagnostic dump *)
	with rpc

	let rec split ?limit:(limit=(-1)) c s =
		let i = try String.index s c with Not_found -> -1 in
		let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
		if i = -1 || nlimit = 0 then
			[ s ]
		else
			let a = String.sub s 0 i
			and b = String.sub s (i + 1) (String.length s - i - 1) in
			a :: (split ~limit: nlimit c b)

	let of_request (req, body) = match body, Request.meth req, split '/' (Request.path req) with
		| None, `GET, [ ""; "" ]                -> Some Diagnostics
		| None, `GET, [ ""; "login"; token ]    -> Some (Login token)
		| None, `GET, [ ""; "create" ]          -> Some (Create None)
		| None, `GET, [ ""; "create"; name ]    -> Some (Create (Some name))
		| None, `GET, [ ""; "subscribe"; name ] -> Some (Subscribe name)
		| None, `GET, [ ""; "ack"; id ]         -> Some (Ack (Int64.of_string id))
		| None, `GET, [ ""; "transfer"; ack_to; timeout ] ->
			Some (Transfer(Int64.of_string ack_to, float_of_string timeout))
		| Some body, `POST, [ ""; "send"; name; correlation_id ] ->
			Some (Send (name, { Message.correlation_id = int_of_string correlation_id; reply_to = None; payload = body }))
		| Some body, `POST, [ ""; "send"; name; correlation_id; reply_to ] ->
			Some (Send (name, { Message.correlation_id = int_of_string correlation_id; reply_to = Some reply_to; payload = body }))
		| _, _, _ -> None

	let make_headers payload =
		Header.of_list [
            "user-agent", "cohttp";
            "content-length", string_of_int (String.length payload);
            "connection", "keep-alive";
        ]


	let to_request = function
		| Login token ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/login/%s" token) ()), None
		| Create None ->
			Request.make ~meth:`GET (Uri.make ~path:"/create" ()), None
		| Create (Some name) ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/create/%s" name) ()), None
		| Subscribe name ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/subscribe/%s" name) ()), None
		| Ack x ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/ack/%Ld" x) ()), None
		| Transfer(ack_to, timeout) ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/transfer/%Ld/%.16g" ack_to timeout) ()), None
		| Send (name, { Message.correlation_id = c; reply_to = None; payload = p }) ->
			let headers = make_headers p in
			Request.make ~meth:`POST ~headers ?body:(Body.body_of_string p) (Uri.make ~path:(Printf.sprintf "/send/%s/%d" name c) ()), Some p
		| Send (name, { Message.correlation_id = c; reply_to = Some r; payload = p }) ->
			let headers = make_headers p in
			Request.make ~meth:`POST ~headers ?body:(Body.body_of_string p) (Uri.make ~path:(Printf.sprintf "/send/%s/%d/%s" name c r) ()), Some p
		| Diagnostics ->
			Request.make ~meth:`GET (Uri.make ~path:"/" ()), None
end

module Out = struct
	type transfer = {
		messages: (int64 * Message.t) list;
	} with rpc

	type t =
	| Login
	| Create of string
	| Subscribe
	| Send
	| Transfer of transfer
	| Ack
	| Diagnostics of string

	let to_response = function
		| Login
		| Ack
		| Subscribe
		| Send -> Server.respond_string ~status:`OK ~body:"" ()
		| Create name ->
			Server.respond_string ~status:`OK ~body:name ()
		| Transfer transfer ->
			Server.respond_string ~status:`OK ~body:(Jsonrpc.to_string (rpc_of_transfer transfer)) ()
		| Diagnostics x ->
			Server.respond_string ~status:`OK ~body:x ()
end



module Connection = struct
	type t = Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel

	exception Failed_to_read_response

	exception Unsuccessful_response

	let rpc (ic, oc) frame =
		let req, body = In.to_request frame in
		let write oc =
			lwt () = Request.write (fun req oc -> match body with Some body ->

			Printf.fprintf stderr "writing body %s\n%!" body;
Request.write_body req oc body

| None -> return ()) req oc in
			Lwt_io.flush oc in
		lwt () = write Lwt_io.stdout in
		lwt () = write oc in
		
	Printf.fprintf stderr "reading response\n%!";
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

