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
					let unique_id = "foo" in
					lwt () = write xs (Printf.sprintf "/name/%s" name) unique_id in
		  			(* Set cookie so we can identify this connection *)
					let headers = Header.add (Header.init ()) "Set-Cookie" (Printf.sprintf "connection=%s" unique_id) in
					Server.respond_string ~headers ~status:`OK ~body:"bind name" ()
				| `POST, [ ""; "send"; destination ] ->
					Server.respond_string ~status:`OK ~body:"send message, has id 1" ()
				| _, _ ->
					Server.respond_not_found (Request.uri req) ()
			in
			let conn_closed conn_id () =
				Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id)
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

