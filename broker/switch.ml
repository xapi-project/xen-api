open Lwt
open Cohttp

let debug fmt = Logging.debug "server_xen" fmt
let warn  fmt = Logging.warn  "server_xen" fmt
let error fmt = Logging.error "server_xen" fmt


let syslog = Lwt_log.syslog ~facility:`Local3 ()

let rec logging_thread logger =
    lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
            (fun x ->
                lwt () = Lwt_log.log ~logger:syslog ~level:Lwt_log.Notice x in
				return ()
			) lines in
	logging_thread logger


let make_path p = Store.Path.create p (Store.Path.getdomainpath 0)


let startswith prefix x = String.length x >= (String.length prefix) && (String.sub x 0 (String.length prefix) = prefix)


let port = ref 8080

open Cohttp_lwt_unix

let no_name () =
	Server.respond_string ~status:`Bad_request ~body:"no name has been bound" ()

let no_binding x =
	Server.respond_string ~status:`Not_found ~body:(Printf.sprintf "name %s is not bound" x) ()

let redirect x =
	let headers = Header.add (Header.init()) "Location" x in
	Server.respond_string ~headers ~status:`Found ~body:"" ()

let make_unique_id =
	let counter = ref 0L in
	fun () ->
		let result = !counter in
		counter := Int64.add 1L !counter;
		result

let make_fresh_name () = Printf.sprintf "client-%Ld" (make_unique_id ())

module Int64Map = Map.Make(struct type t = int64 let compare = Int64.compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringSet = Set.Make(struct type t = string let compare = String.compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

type origin =
	| Anonymous of int (** An un-named connection, probably a temporary client connection *)
	| Name of string   (** A service with a well-known name *)
with rpc

module Entry = struct
	type t = {
		origin: origin;
		time: float; (* XXX this is for judging age: use oclock/clock_monotonic *)
		message: Protocol.Message.t;
	} with rpc

	let make origin message =
		let time = 0. in
		{ origin; time; message }
end

module Subscription = struct
	(* Session token -> queue bindings *)
	let session_to_subscriptions : (string, StringSet.t) Hashtbl.t = Hashtbl.create 128

	let add session subscription =
		let existing =
			if Hashtbl.mem session_to_subscriptions session
			then Hashtbl.find session_to_subscriptions session
			else StringSet.empty in
		Hashtbl.replace session_to_subscriptions session (StringSet.add subscription existing)

	let get session =
		if Hashtbl.mem session_to_subscriptions session
		then Hashtbl.find session_to_subscriptions session
		else StringSet.empty
end

module Connections = struct
	(* Connection id -> session *)
	let connections : string IntMap.t ref = ref IntMap.empty

	(* session -> active connection ids *)
	let sessions : (string, IntSet.t) Hashtbl.t = Hashtbl.create 128

	let get_origin conn_id =
		if IntMap.mem conn_id !connections
		then Name (IntMap.find conn_id !connections)
		else Anonymous conn_id

	let get_session conn_id =
		IntMap.find conn_id !connections

	let add conn_id session =
		connections := IntMap.add conn_id session !connections;
		let existing =
			if Hashtbl.mem sessions session
			then Hashtbl.find sessions session
			else IntSet.empty in
		Hashtbl.replace sessions session (IntSet.add conn_id existing)

	let remove conn_id =
		let session = get_session conn_id in
		let existing =
			if Hashtbl.mem sessions session
			then Hashtbl.find sessions session
			else IntSet.empty in
		connections := IntMap.remove conn_id !connections;
		let remaining = IntSet.remove conn_id existing in
		if existing = IntSet.empty
		then Hashtbl.remove sessions session
		else Hashtbl.replace sessions session remaining

	let is_session_active session = Hashtbl.mem sessions session

end

(* Session -> set of queues which will be GCed on session cleanup *)
let private_queues : (string, StringSet.t) Hashtbl.t = Hashtbl.create 128

let queues : (string, Entry.t Int64Map.t) Hashtbl.t = Hashtbl.create 128
let message_id_to_queue : string Int64Map.t ref = ref Int64Map.empty
let queues_c = Hashtbl.create 128

module Diagnostics = struct
	type queue = (int64 * Entry.t) list with rpc
	let queue q = Int64Map.fold (fun i e acc -> (i, e) :: acc) q []

	type queues = (string * queue) list with rpc

	let snapshot () = Hashtbl.fold (fun n q acc -> (n, queue q) :: acc) queues []
end


let create_queue name =
	Hashtbl.replace queues name Int64Map.empty;
	Hashtbl.replace queues_c name (Lwt_condition.create ())

let queue_broadcast name =
	if not(Hashtbl.mem queues_c name)
	then Printf.fprintf stderr "ERROR: queue_broadcast: queue %s doesn't exist\n%!" name
	else Lwt_condition.broadcast (Hashtbl.find queues_c name) ()

let queue_wait name =
	if not(Hashtbl.mem queues_c name)
	then (Printf.fprintf stderr "ERROR: queue_wait: queue %s doesn't exist\n%!" name; fail Not_found)
	else Lwt_condition.wait (Hashtbl.find queues_c name)

let make_server () =
	debug "Started server on unix domain socket";

  	(* (Response.t * Body.t) Lwt.t *)
	let callback conn_id ?body req =
		let open Protocol in
		match In.of_request req with
		| None ->
			Server.respond_not_found ~uri:(Request.uri req) ()
		| Some request ->
			Printf.fprintf stderr "%s\n%!" (Jsonrpc.to_string (In.rpc_of_t request));
			begin match request with
			| In.Login session ->
				(* associate conn_id with 'session' *)
				Connections.add conn_id session;
				Out.to_response Out.Login
			| In.Create name ->
				let name = begin match name with
				| Some name ->
					name
				| None ->
					(* Create a session-local private queue name *)
					let name = make_fresh_name () in
					let session = Connections.get_session conn_id in
					let existing =
						if Hashtbl.mem private_queues session
						then Hashtbl.find private_queues session
						else StringSet.empty in
					Hashtbl.replace private_queues session (StringSet.add name existing);
					name
				end in
				if not(Hashtbl.mem queues name) then create_queue name;
				Printf.fprintf stderr "Responding\n%!";
				Out.to_response (Out.Create name)
			| In.Subscribe name ->
				let session = Connections.get_session conn_id in
				Subscription.add session name;
				Out.to_response Out.Subscribe
			| In.Ack id ->
				let name = Int64Map.find id !message_id_to_queue in
				if Hashtbl.mem queues name then begin
					let q = Hashtbl.find queues name in
					message_id_to_queue := Int64Map.remove id !message_id_to_queue;
					Printf.fprintf stderr "Removing id %Ld from queue %s\n%!" id name;
					Hashtbl.replace queues name (Int64Map.remove id q);
				end;
				Out.to_response Out.Ack
			| In.Transfer(from, timeout) ->
				let session = Connections.get_session conn_id in
				let names = Subscription.get session in

				let start = Unix.gettimeofday () in
				let rec wait () =
					let not_seen = StringSet.fold (fun name map ->
						if Hashtbl.mem queues name then begin
							let q = Hashtbl.find queues name in
							let _, _, not_seen = Int64Map.split from q in
							Int64Map.fold Int64Map.add map not_seen
						end else map
					) names Int64Map.empty in
					if not_seen <> Int64Map.empty
					then return not_seen
					else
						let remaining_timeout = max 0. (start +. timeout -. (Unix.gettimeofday ())) in
						let timeout = Lwt.map (fun () -> `Timeout) (Lwt_unix.sleep remaining_timeout) in
						let more = StringSet.fold (fun name acc ->
							Lwt.map (fun () -> `Data) (queue_wait name) :: acc
						) names [] in
						match_lwt Lwt.pick (timeout :: more) with
						| `Timeout -> return Int64Map.empty
						| `Data -> wait () in
				lwt messages = wait () in
				let transfer = {
					Out.messages = Int64Map.fold (fun id e acc -> (id, e.Entry.message) :: acc) messages [];
				} in
				Out.to_response (Out.Transfer transfer)

			| In.Send (name, data) ->
				(* If a queue doesn't exist then drop the message *)
				if Hashtbl.mem queues name then begin
					let q = Hashtbl.find queues name in
					let origin = Connections.get_origin conn_id in
					let id = make_unique_id () in
					message_id_to_queue := Int64Map.add id name !message_id_to_queue;
					Hashtbl.replace queues name (Int64Map.add id (Entry.make origin data) q);
					queue_broadcast name;
				end;
				Out.to_response Out.Send
			| In.Diagnostics ->
				let d = Diagnostics.(Jsonrpc.to_string (rpc_of_queues (snapshot ()))) in
				Out.to_response (Out.Diagnostics d)
			end
			in
			let conn_closed conn_id () =
				Printf.eprintf "conn %d closed\n%!" conn_id;
				let session = Connections.get_session conn_id in
				Connections.remove conn_id;
				if not(Connections.is_session_active session) then begin
					Printf.fprintf stderr "Session %s cleaning up\n%!" session;
					if Hashtbl.mem private_queues session then begin
						let qs = Hashtbl.find private_queues session in
						StringSet.iter
							(fun name ->
								Printf.fprintf stderr "Deleting private queue: %s\n%!" name;
								Hashtbl.remove queues name;
								Hashtbl.remove queues_c name;
							) qs;
						Hashtbl.remove private_queues session
					end
				end in

			debug "Message switch starting";
			let (_: 'a Lwt.t) = logging_thread Logging.logger in

			let config = { Server.callback; conn_closed } in
			server ~address:"127.0.0.1" ~port:!port config
    
let _ =
	Arg.parse [
		"-port", Arg.Set_int port, "port to listen on";
	] (fun x -> Printf.fprintf stderr "Ignoring: %s" x)
		"A simple message switch";

	Lwt_unix.run (make_server ()) 

