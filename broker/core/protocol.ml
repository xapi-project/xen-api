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

	let of_request body meth path = match body, meth, split '/' path with
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
		| Transfer(ack_to, timeout) ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/transfer/%Ld/%.16g" ack_to timeout) ())
		| Send (name, { Message.correlation_id = c; reply_to = None; payload = p }) ->
			Some p, `POST, (Uri.make ~path:(Printf.sprintf "/send/%s/%d" name c) ())
		| Send (name, { Message.correlation_id = c; reply_to = Some r; payload = p }) ->
			Some p, `POST, (Uri.make ~path:(Printf.sprintf "/send/%s/%d/%s" name c r) ())
		| Diagnostics ->
			None, `GET, (Uri.make ~path:"/" ())
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
	| Not_logged_in

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
		| Not_logged_in ->
			Server.respond_string ~status:`Not_found ~body:"Please log in." ()
end



