open Lwt
open Cohttp

module UnixServer = Xs_server.Server(Xs_transport_unix)
module Client = Xs_client.Client(Xs_transport_unix)
open Client

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

let make_fresh_name () = Printf.sprintf "queue-%Ld" (make_unique_id ())

module IntMap = Map.Make(struct type t = int64 let compare = Int64.compare end)

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

let queues : (string, Entry.t IntMap.t) Hashtbl.t = Hashtbl.create 128
let message_id_to_queue : string IntMap.t ref = ref IntMap.empty
let queues_c = Hashtbl.create 128

module Diagnostics = struct
	type queue = (int64 * Entry.t) list with rpc
	let queue q = IntMap.fold (fun i e acc -> (i, e) :: acc) q []

	type queues = (string * queue) list with rpc

	let snapshot () = Hashtbl.fold (fun n q acc -> (n, queue q) :: acc) queues []
end


let find_or_create_queue name =
	if not(Hashtbl.mem queues name) then begin
		Hashtbl.replace queues name IntMap.empty;
		Hashtbl.replace queues_c name (Lwt_condition.create ());
	end;
	Hashtbl.find queues name

let queue_broadcast name =
	if not(Hashtbl.mem queues_c name)
	then Printf.fprintf stderr "ERROR: queue_broadcast: queue %s doesn't exist\n%!" name
	else Lwt_condition.broadcast (Hashtbl.find queues_c name) ()

let queue_wait name =
	if not(Hashtbl.mem queues_c name)
	then (Printf.fprintf stderr "ERROR: queue_wait: queue %s doesn't exist\n%!" name; fail Not_found)
	else Lwt_condition.wait (Hashtbl.find queues_c name)

let make_server () =
	let (_: unit Lwt.t) = UnixServer.serve_forever () in
	debug "Started server on unix domain socket";
	(* XXX: we need to synchronise with the server starting *)
	lwt () = Lwt_unix.sleep 2. in

	lwt client = make () in
	with_xs client
		(fun xs ->
			let origin_of_conn_id conn_id =
				try_lwt
					lwt name = read xs (Printf.sprintf "/id/%s" (Server.string_of_conn_id conn_id)) in
					return (Name name)
				with _ ->
					return (Anonymous conn_id) in

  			(* (Response.t * Body.t) Lwt.t *)
			let callback conn_id ?body req =
				let open Protocol in
				match In.of_request req with
				| Some (In.Login token) ->
					(* associate conn_id with token *)
					let conn_id = string_of_int conn_id in
					lwt () = write xs (Printf.sprintf "/id/%s" conn_id) token in
					lwt () = write xs (Printf.sprintf "/session/%s/%s" token conn_id) "" in
					Out.to_response Out.Login
				| Some (In.Bind name) ->
					let name = match name with Some name -> name | None -> make_fresh_name () in
					(* If a queue for [name] doesn't already exist, make it now *)
					if not(Hashtbl.mem queues name) then begin
						Printf.fprintf stderr "Created queue %s\n%!" name;
						Hashtbl.add queues name IntMap.empty;
						Hashtbl.add queues_c name (Lwt_condition.create ());
					end;
					let conn_id = string_of_int conn_id in
					lwt token = read xs (Printf.sprintf "/id/%s" conn_id) in
					lwt () = write xs (Printf.sprintf "/by_name/%s/%s" name token) "" in
					lwt () = write xs (Printf.sprintf "/by_session/%s/%s" token name) "" in
					Printf.fprintf stderr "Responding\n%!";
					Out.to_response (Out.Bind name)
				| Some (In.Ack id) ->
					let name = IntMap.find id !message_id_to_queue in
					let q = find_or_create_queue name in
					message_id_to_queue := IntMap.remove id !message_id_to_queue;
					Hashtbl.replace queues name (IntMap.remove id q);
					Out.to_response Out.Ack
				| Some (In.Transfer(from, timeout)) ->
					let conn_id = string_of_int conn_id in
					lwt token = read xs (Printf.sprintf "/id/%s" conn_id) in
					lwt names = directory xs (Printf.sprintf "/by_session/%s" token) in
					let name = List.hd names (* XXX *) in
					let start = Unix.gettimeofday () in
					let rec wait () =
						let q = find_or_create_queue name in
						let _, _, not_seen = IntMap.split from q in
						if not_seen <> IntMap.empty
						then return not_seen
						else
							let remaining_timeout = max 0. (start +. timeout -. (Unix.gettimeofday ())) in
							let timeout = Lwt.map (fun () -> `Timeout) (Lwt_unix.sleep remaining_timeout) in
							let more = Lwt.map (fun () -> `Data) (queue_wait name) in
							match_lwt Lwt.pick [ timeout; more ] with
							| `Timeout -> return IntMap.empty
							| `Data -> wait () in
					lwt messages = wait () in
					let transfer = {
						Out.messages = IntMap.fold (fun id e acc -> (id, e.Entry.message) :: acc) messages [];
					} in
					Out.to_response (Out.Transfer transfer)

				| Some (In.Send (name, data)) ->
					(* If a queue for [name] doesn't already exist, make it now *)
					let q = find_or_create_queue name in
					lwt origin = origin_of_conn_id conn_id in
					let id = make_unique_id () in
					message_id_to_queue := IntMap.add id name !message_id_to_queue;
					Hashtbl.replace queues name (IntMap.add id (Entry.make origin data) q);
					queue_broadcast name;
					Out.to_response Out.Send
				| Some In.Diagnostics ->
					let d = Diagnostics.(Jsonrpc.to_string (rpc_of_queues (snapshot ()))) in
					Out.to_response (Out.Diagnostics d)
				| None ->
					Server.respond_not_found ~uri:(Request.uri req) ()
			in
			let conn_closed conn_id () =
				let c = Server.string_of_conn_id conn_id in
				Printf.eprintf "conn %s closed\n%!" c;
				let _ : unit Lwt.t =
					lwt name = read xs (Printf.sprintf "/id/%s" c) in
					lwt () = rm xs (Printf.sprintf "/name/%s" name) in
					lwt () = rm xs (Printf.sprintf "/id/%s" c) in
					return () in
				()
			in

			debug "Message switch starting";
			let (_: 'a Lwt.t) = logging_thread Logging.logger in

			let config = { Server.callback; conn_closed } in
			server ~address:"127.0.0.1" ~port:!port config
		)
    
let _ =
	Arg.parse [
		"-port", Arg.Set_int port, "port to listen on";
	] (fun x -> Printf.fprintf stderr "Ignoring: %s" x)
		"A simple message switch";

	Lwt_unix.run (make_server ()) 

