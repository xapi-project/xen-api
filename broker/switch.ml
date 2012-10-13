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

type transfer = {
	dropped: int;
	messages: (int64 * Entry.t) list;
} with rpc

let queues : (string, Entry.t IntMap.t) Hashtbl.t = Hashtbl.create 128

let find_or_create_queue name =
	if not(Hashtbl.mem queues name) then Hashtbl.replace queues name IntMap.empty;
	Hashtbl.find queues name

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
				let open Protocol.Frame in match of_request req with
				| Some (Bind name) ->
					(* If a queue for [name] doesn't already exist, make it now *)
					if not(Hashtbl.mem queues name) then Hashtbl.add queues name IntMap.empty;
					lwt () = write xs (Printf.sprintf "/name/%s" name) (string_of_int conn_id) in
					lwt () = write xs (Printf.sprintf "/id/%s" (Server.string_of_conn_id conn_id)) name in
					redirect "/connection"
				| Some Connect ->
					lwt name = read xs (Printf.sprintf "/id/%s" (Server.string_of_conn_id conn_id)) in
					let q = if Hashtbl.mem queues name then Some (Hashtbl.find queues name) else None in
					let qlen = match q with Some x -> IntMap.cardinal x | None -> -1 in
					Server.respond_string ~status:`OK ~body:(Printf.sprintf "name = %s; length = %d" name qlen) ()
				| Some (Transfer ack_to) ->
					lwt name = read xs (Printf.sprintf "/id/%s" (Server.string_of_conn_id conn_id)) in
					let q = find_or_create_queue name in
					let dropped, also_dropped, not_acked = IntMap.split (Int64.of_string ack_to) q in
					Hashtbl.replace queues name not_acked;
					let transfer = {
						dropped = IntMap.cardinal dropped + (match also_dropped with Some _ -> 1 | None -> 0);
						messages = IntMap.fold (fun id m acc -> (id, m) :: acc) not_acked [];
					} in
					Server.respond_string ~status:`OK ~body:(Jsonrpc.to_string (rpc_of_transfer transfer)) ()
				| Some (Send (name, data)) ->
					(* If a queue for [name] doesn't already exist, make it now *)
					let q = find_or_create_queue name in
					lwt origin = origin_of_conn_id conn_id in
					Hashtbl.replace queues name (IntMap.add (make_unique_id ()) (Entry.make origin data) q);
					Server.respond_string ~status:`OK ~body:(Printf.sprintf "queue now has length %d" (IntMap.cardinal q + 1)) ()
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

