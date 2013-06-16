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


let port = ref 8080
let ip = ref "0.0.0.0"

open Cohttp_lwt_unix

let no_name () =
	Server.respond_string ~status:`Bad_request ~body:"no name has been bound" ()

let no_binding x =
	Server.respond_string ~status:`Not_found ~body:(Printf.sprintf "name %s is not bound" x) ()

let redirect x =
	let headers = Header.add (Header.init()) "Location" x in
	Server.respond_string ~headers ~status:`Found ~body:"" ()



module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringSet = Set.Make(struct type t = string let compare = String.compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)
module StringMap = Map.Make(struct type t = string let compare = compare end)

module StringStringRelation = Relation.Make(String)(String)

module Subscription = struct
	(* Session token -> queue bindings *)
	let t = ref (StringStringRelation.empty)

	let session_to_wakeup : (string, unit Lwt.u) Hashtbl.t = Hashtbl.create 128

	let add session subscription =
		t := StringStringRelation.add session subscription !t;
		if Hashtbl.mem session_to_wakeup session then begin
			Lwt.wakeup_later (Hashtbl.find session_to_wakeup session) ()
		end

	let get session =
		StringStringRelation.B_Set.fold (fun name acc -> name :: acc)
		(StringStringRelation.get_bs session !t)
		[]

	let remove subscription =
		t := StringStringRelation.remove_b subscription !t

end

module IntStringRelation = Relation.Make(struct type t = int let compare = compare end)(String)

module Connections = struct
	let t = ref (IntStringRelation.empty)

	let get_session conn_id =
		(* Nothing currently stops you registering multiple sessions per connection *)
		let sessions = IntStringRelation.get_bs conn_id !t in
		if sessions = IntStringRelation.B_Set.empty
		then None
		else Some(IntStringRelation.B_Set.choose sessions)

	let get_origin conn_id = match get_session conn_id with
		| None -> Protocol.Anonymous conn_id
		| Some x -> Protocol.Name x

	let add conn_id session =
		debug "+ connection %d" conn_id;
		t := IntStringRelation.add conn_id session !t

	let remove conn_id =
		debug "- connection %d" conn_id;
		t := IntStringRelation.remove_a conn_id !t

	let is_session_active session =
		IntStringRelation.get_as session !t <> IntStringRelation.A_Set.empty

end

module Transient_queue = struct

	(* Session -> set of queues which will be GCed on session cleanup *)
	let queues : (string, StringSet.t) Hashtbl.t = Hashtbl.create 128

	let add session name =
		let existing =
			if Hashtbl.mem queues session
			then Hashtbl.find queues session
			else StringSet.empty in
		Hashtbl.replace queues session (StringSet.add name existing)

	let remove session =
		if Hashtbl.mem queues session then begin
			let qs = Hashtbl.find queues session in
			StringSet.iter
				(fun name ->
					info "Deleting transient queue: %s" name;
					Q.Directory.remove name;
					Subscription.remove name
				) qs;
			Hashtbl.remove queues session
		end

	let all () = Hashtbl.fold (fun _ set acc -> StringSet.union set acc) queues StringSet.empty
end

let next_transfer_expected : (string, int64) Hashtbl.t = Hashtbl.create 128
let get_next_transfer_expected name =
	if Hashtbl.mem next_transfer_expected name
	then Some (Hashtbl.find next_transfer_expected name)
	else None
let record_transfer time name =
	Hashtbl.replace next_transfer_expected name time

let snapshot () =
	let open Protocol.Diagnostics in
	let queues =
		List.fold_left (fun acc (n, q)->
			let queue_contents = Q.contents q in
			let next_transfer_expected = get_next_transfer_expected n in
			(n, { queue_contents; next_transfer_expected }) :: acc
		) [] in
	let is_transient =
		let all = Transient_queue.all () in
		fun (x, _) -> StringSet.mem x all in
	let all_queues = queues (List.map (fun n -> n, (Q.Directory.find n)) (Q.Directory.list "")) in
	let transient_queues, permanent_queues = List.partition is_transient all_queues in
	let current_time = time () in
	{ start_time; current_time; permanent_queues; transient_queues }

open Protocol
let process_request conn_id session request = match session, request with
	(* Only allow Login, Get, Trace and Diagnostic messages if there is no session *)
	| _, In.Login session ->
		(* associate conn_id with 'session' *)
		Connections.add conn_id session;
		return Out.Login
	| _, In.Diagnostics ->
		return (Out.Diagnostics (snapshot ()))
	| _, In.Trace(from, timeout) ->
		lwt events = Trace.get from timeout in
		return (Out.Trace {Out.events = events})
	| _, In.Get path ->
		let path = if path = [] || path = [ "" ] then [ "index.html" ] else path in
		lwt ic = Lwt_io.open_file ~mode:Lwt_io.input (String.concat "/" ("www" :: path)) in
		lwt txt = Lwt_stream.to_string (Lwt_io.read_chars ic) in
		lwt () = Lwt_io.close ic in
		return (Out.Get txt)
	| None, _ ->
		return Out.Not_logged_in
	| Some session, In.List prefix ->
		return (Out.List (Q.Directory.list prefix))
	| Some session, In.CreatePersistent name ->
		Q.Directory.add name;
		return (Out.Create name)
	| Some session, In.CreateTransient name ->
		Transient_queue.add session name;
		Q.Directory.add name;
		return (Out.Create name)
	| Some session, In.Destroy name ->
		Q.Directory.remove name;
		Subscription.remove name;
		return Out.Destroy
	| Some session, In.Subscribe name ->
		Subscription.add session name;
		return Out.Subscribe
	| Some session, In.Ack (name, id) ->
		Trace.add (Event.({time = Unix.gettimeofday (); input = Some session; queue = name; output = None; message = Ack (name, id); processing_time = None }));
		Q.ack (name, id);
		return Out.Ack
	| Some session, In.Transfer(from, timeout) ->
		let start = Unix.gettimeofday () in
		let rec wait () =
			let names = Subscription.get session in
			let time = Int64.add (time ()) (Int64.of_float (timeout *. 1e9)) in
			List.iter (record_transfer time) names;

			let not_seen = Q.transfer from names in
			if not_seen <> []
			then return not_seen
			else
				let remaining_timeout = max 0. (start +. timeout -. (Unix.gettimeofday ())) in
				if remaining_timeout <= 0.
				then return []
				else
					let timeout = Lwt.map (fun () -> `Timeout) (Lwt_unix.sleep remaining_timeout) in
					let more = List.map (fun name ->
						Lwt.map (fun () -> `Data) (Q.wait from name)
					) names in
					let t, u = Lwt.task () in
					Hashtbl.replace Subscription.session_to_wakeup session u;
					let sub = Lwt.map (fun () -> `Subscription) t in
					try_lwt
						match_lwt Lwt.pick (sub :: timeout :: more) with
						| `Timeout -> return []
						| `Data ->
							wait ()
						| `Subscription ->
							wait ()
					finally
		   				Hashtbl.remove Subscription.session_to_wakeup session;
			   			return ()
				in
		lwt messages = wait () in
		let next = match messages with
		| [] -> from
		| x :: xs -> List.fold_left max (snd (fst x)) (List.map (fun x -> snd (fst x)) xs) in
		let transfer = {
			Out.messages = messages;
			next = next
		} in
		let now = Unix.gettimeofday () in
		List.iter
			(fun (id, m) ->
				let name = Q.queue_of_id id in
				let processing_time = match m.Message.kind with
				| Message.Request _ -> None
				| Message.Response id' -> begin match Q.entry id' with
					| Some request_entry ->
						Some (Int64.sub (time ()) request_entry.Entry.time)
					| None ->
						None
				end in
				Trace.add (Event.({time = now; input = None; queue = name; output = Some session; message = Message (id, m); processing_time }))
			) transfer.Out.messages;
		return (Out.Transfer transfer)
	| Some session, In.Send (name, data) ->
		let origin = Connections.get_origin conn_id in
		begin match_lwt Q.send origin name data with
		| None -> return (Out.Send None)
		| Some id ->
			Trace.add (Event.({time = Unix.gettimeofday (); input = Some session; queue = name; output = None; message = Message (id, data); processing_time = None }));
			return (Out.Send (Some id))
		end

let make_server () =
	info "Started server on localhost:%d" !port;

	let (_: 'a) = logging_thread () in

  	(* (Response.t * Body.t) Lwt.t *)
	let callback conn_id ?body req =
		let open Protocol in
		lwt body = match body with
			| None -> return None
			| Some b ->
				lwt s = Cohttp_lwt_body.string_of_body (Some b) in
				return (Some s) in
		let uri = Cohttp.Request.uri req in
		let path = Uri.path uri in
		match In.of_request body (Cohttp.Request.meth req) path with
		| None ->
			error "<- [unparsable request; path = %s; body = %s]" path (match body with Some x -> "\"" ^ x ^ "\"" | None -> "None");
			error "-> 404 [Not_found]";
			Cohttp_lwt_unix.Server.respond_not_found ~uri ()
		| Some request ->
			debug "<- %s [%s]" path (match body with None -> "" | Some x -> x);
			let session = Connections.get_session conn_id in
			lwt response = process_request conn_id session request in
			let status, body = Out.to_response response in
			debug "-> %s [%s]" (Cohttp.Code.string_of_status status) body;
			Cohttp_lwt_unix.Server.respond_string ~status ~body ()
		in
	let conn_closed conn_id () =
		let session = Connections.get_session conn_id in
		Connections.remove conn_id;
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

