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

let rec split ?limit:(limit=(-1)) c s =
    let i = try String.index s c with Not_found -> -1 in
    let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
    if i = -1 || nlimit = 0 then
        [ s ]
    else
        let a = String.sub s 0 i
        and b = String.sub s (i + 1) (String.length s - i - 1) in
        a :: (split ~limit: nlimit c b)


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
	let counter = ref 0 in
	fun () ->
		let result = !counter in
		incr counter;
		string_of_int result

let queues : (string, string Queue.t) Hashtbl.t = Hashtbl.create 128

let find_or_create_queue name =
	if not(Hashtbl.mem queues name) then Hashtbl.replace queues name (Queue.create ());
	Hashtbl.find queues name

let make_server () =
	let (_: unit Lwt.t) = UnixServer.serve_forever () in
	debug "Started server on unix domain socket";
	(* XXX: we need to synchronise with the server starting *)
	lwt () = Lwt_unix.sleep 2. in

	lwt client = make () in
	with_xs client
		(fun xs ->
  			(* (Response.t * Body.t) Lwt.t *)
			let callback conn_id ?body req = match Request.meth req, split '/' (Request.path req) with
				| `GET, [ ""; "bind"; name ] ->
					(* If a queue for [name] doesn't already exist, make it now *)
					let (_: 'a Queue.t) = find_or_create_queue name in
					lwt () = write xs (Printf.sprintf "/name/%s" name) (string_of_int conn_id) in
					lwt () = write xs (Printf.sprintf "/id/%s" (Server.string_of_conn_id conn_id)) name in
					redirect "/connection"
				| `GET, [ ""; "connection" ] ->
					lwt name = read xs (Printf.sprintf "/id/%s" (Server.string_of_conn_id conn_id)) in
					let q = if Hashtbl.mem queues name then Some (Hashtbl.find queues name) else None in
					let qlen = match q with Some x -> Queue.length x | None -> -1 in
					Server.respond_string ~status:`OK ~body:(Printf.sprintf "name = %s; length = %d" name qlen) ()
				| `GET, [ ""; "send"; name; data ] ->
					(* If a queue for [name] doesn't already exist, make it now *)
					let q = find_or_create_queue name in
					Queue.push data q;
					Server.respond_string ~status:`OK ~body:(Printf.sprintf "queue now has length %d" (Queue.length q)) ()
				| _, _ ->
					Server.respond_not_found (Request.uri req) ()
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
		"-port", Arg.Set_int port, "port to listen on"
	] (fun _ -> ())
		"Start listening for requests";

	Lwt_unix.run (make_server ()) 

