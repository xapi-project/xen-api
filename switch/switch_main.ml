(*
Copyright (c) Citrix Systems Inc.
All rights reserved.

Redistribution and use in source and binary forms, 
with or without modification, are permitted provided 
that the following conditions are met:

*   Redistributions of source code must retain the above 
    copyright notice, this list of conditions and the 
    following disclaimer.
*   Redistributions in binary form must reproduce the above 
    copyright notice, this list of conditions and the 
    following disclaimer in the documentation and/or other 
    materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE.
*)

open Lwt
open Cohttp
open Logging
open Clock
open Switch

let port = ref 8080
let ip = ref "0.0.0.0"

open Cohttp_lwt_unix

let make_server () =
	info "Started server on localhost:%d" !port;

	let (_: 'a) = logging_thread () in

  	(* (Response.t * Body.t) Lwt.t *)
	let callback conn_id req body =
		let conn_id_s = Cohttp.Connection.to_string conn_id in
		let open Protocol in
		lwt body = Cohttp_lwt_body.to_string body in 
		let uri = Cohttp.Request.uri req in
		let path = Uri.path uri in
		match In.of_request (Some body) (Cohttp.Request.meth req) path with
		| None ->
			error "<- [unparsable request; path = %s; body = %s]" path ("\"" ^ body ^ "\"");
			error "-> 404 [Not_found]";
			Cohttp_lwt_unix.Server.respond_not_found ~uri ()
		| Some request ->
			debug "<- %s [%s]" path body;
			let session = Connections.get_session conn_id_s in
			lwt response = process_request conn_id_s session request in
			let status, body = Out.to_response response in
			debug "-> %s [%s]" (Cohttp.Code.string_of_status status) body;
			Cohttp_lwt_unix.Server.respond_string ~status ~body ()
		in
	let conn_closed conn_id () =
		let conn_id_s = Cohttp.Connection.to_string conn_id in
		let session = Connections.get_session conn_id_s in
		Connections.remove conn_id_s;
		match session with
		| None -> ()
		| Some session ->
			if not(Connections.is_session_active session) then begin
				info "Session %s cleaning up" session;
				Transient_queue.remove session
			end in

	info "Message switch starting";
	let config = { Cohttp_lwt_unix.Server.callback; conn_closed } in
	Cohttp_lwt_unix.Server.create ~address:!ip ~port:!port config
    
let _ =
	let daemonize = ref false in
	let pidfile = ref None in
	Arg.parse [
		"-daemon", Arg.Set daemonize, "run as a background daemon";
		"-port", Arg.Set_int port, "port to listen on";
		"-ip", Arg.Set_string ip, "IP to bind to";
		"-pidfile", Arg.String (fun x -> pidfile := Some x), "write PID to file";
	] (fun x -> Printf.fprintf stderr "Ignoring: %s" x)
		"A simple message switch";

	if !daemonize
	then Lwt_daemon.daemonize ();

	let (_ : unit Lwt.t) =
		match !pidfile with
		| None -> return ()
		| Some x ->
			Lwt_io.with_file ~flags:[Unix.O_WRONLY; Unix.O_CREAT] ~perm:0o0644
			  ~mode:Lwt_io.output x (fun oc ->
				lwt () = Lwt_io.write oc (Printf.sprintf "%d" (Unix.getpid ())) in
				Lwt_io.flush oc
			) in

	Lwt_unix.run (make_server ()) 

