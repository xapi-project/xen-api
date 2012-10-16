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

	let of_request req = match Request.meth req, split '/' (Request.path req) with
		| `GET, [ ""; "" ]                -> Some Diagnostics
		| `GET, [ ""; "login"; token ]    -> Some (Login token)
		| `GET, [ ""; "create" ]          -> Some (Create None)
		| `GET, [ ""; "create"; name ]    -> Some (Create (Some name))
		| `GET, [ ""; "subscribe"; name ] -> Some (Subscribe name)
		| `GET, [ ""; "ack"; id ]         -> Some (Ack (Int64.of_string id))
		| `GET, [ ""; "transfer"; ack_to; timeout ] ->
			Some (Transfer(Int64.of_string ack_to, float_of_string timeout))
		| `GET, [ ""; "send"; name; data ] ->
			let message = Message.one_way data in
			Some (Send (name, message))
		| _, _ -> None

	let to_request = function
		| Login token ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/login/%s" token) ())
		| Create None ->
			Request.make ~meth:`GET (Uri.make ~path:"/create" ())
		| Create (Some name) ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/create/%s" name) ())
		| Subscribe name ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/subscribe/%s" name) ())
		| Ack x ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/ack/%Ld" x) ())
		| Transfer(ack_to, timeout) ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/transfer/%Ld/%.16g" ack_to timeout) ())
		| Send (name, message) ->
			Request.make ~meth:`GET (Uri.make ~path:(Printf.sprintf "/send/%s/%s" name message.Message.payload) ())
		| Diagnostics ->
			Request.make ~meth:`GET (Uri.make ~path:"/" ())
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
		lwt () = Request.write (fun _ _ -> return ()) (In.to_request frame) oc in
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

