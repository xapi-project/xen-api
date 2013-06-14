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

let program = Filename.basename Sys.argv.(0)

let ignore_fmt fmt = Printf.ksprintf (fun _ -> ()) fmt

(* General system logging *)
let logger = Logging.create 512

type level = Debug | Info | Warn | Error | Null

let log_level = ref Warn

let string_of_level = function
        | Debug -> "debug" | Info -> "info" | Warn -> "warn"
        | Error -> "error" | Null -> "null"

let log level key (fmt: (_,_,_,_) format4) =
        let level = string_of_level level in
        Printf.ksprintf logger.Logging.push ("[%5s|%s] " ^^ fmt) level key

let key = "message-switch"

(* let debug = log Debug key *)
let debug fmt = ignore_fmt fmt
let info fmt = log Info key fmt
let warn fmt = log Warn key fmt
let error fmt = log Error key fmt

let get_time () = Oclock.gettime Oclock.monotonic
let start_time = get_time ()

let rec logging_thread logger =
    lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
            (fun x ->
                lwt () = Lwt_log.log ~logger:!Lwt_log.default ~level:Lwt_log.Notice x in
				return ()
			) lines in
	logging_thread logger

let startswith prefix x = String.length x >= (String.length prefix) && (String.sub x 0 (String.length prefix) = prefix)


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

	let get session = StringStringRelation.get_bs session !t

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

module Q = struct

	let queues : (string, Protocol.Entry.t Int64Map.t) Hashtbl.t = Hashtbl.create 128
	let queue_lengths : (string, int) Hashtbl.t = Hashtbl.create 128
	let next_transfer_expected : (string, int64) Hashtbl.t = Hashtbl.create 128
	let message_id_to_queue : string Int64Map.t ref = ref Int64Map.empty

	let list prefix = Hashtbl.fold (fun name _ acc ->
		if startswith prefix name
		then name :: acc
		else acc) queues []

	module Lengths = struct
		open Measurable
		let d x =Description.({ description = "length of queue " ^ x; units = "" })
		let list_available () =
			Hashtbl.fold (fun name _ acc ->
				(name, d name) :: acc
			) queues []
		let measure name =
			if Hashtbl.mem queue_lengths name
			then Some (Measurement.Int (Hashtbl.find queue_lengths name))
			else None
	end

	type wait = {
		c: unit Lwt_condition.t;
		m: Lwt_mutex.t
	}

	let waiters = Hashtbl.create 128

	let exists name = Hashtbl.mem queues name

	let add name =
		if not(exists name) then begin
			Hashtbl.replace queues name Int64Map.empty;
			Hashtbl.replace queue_lengths name 0;
			Hashtbl.replace waiters name {
				c = Lwt_condition.create ();
				m = Lwt_mutex.create ()
			}
		end

	let get name =
		if exists name
		then Hashtbl.find queues name
		else Int64Map.empty

	let remove name =
		let entries = get name in
		Int64Map.iter (fun id _ ->
			message_id_to_queue := Int64Map.remove id !message_id_to_queue
		) entries;
		Hashtbl.remove queues name;
		Hashtbl.remove queue_lengths name;
		Hashtbl.remove next_transfer_expected name;
		Hashtbl.remove waiters name;
		Subscription.remove name

	let transfer name next_expected =
		let time = Int64.add (get_time ()) (Int64.of_float (next_expected *. 1e9)) in
		Hashtbl.replace next_transfer_expected name time

	let get_next_transfer_expected name =
		if Hashtbl.mem next_transfer_expected name
		then Some (Hashtbl.find next_transfer_expected name)
		else None

	let queue_of_id id =
		if Int64Map.mem id !message_id_to_queue
		then Some (Int64Map.find id !message_id_to_queue)
		else begin
			error "Message id %Ld not in message_id_to_queue" id;
			None
		end

	let find id = match queue_of_id id with
	| None -> None
	| Some name ->
		let q = get name in
		if Int64Map.mem id q
		then Some (Int64Map.find id q)
		else None

	let ack id = match queue_of_id id with
	| None -> ()
	| Some name ->
		if exists name then begin
			let q = get name in
			message_id_to_queue := Int64Map.remove id !message_id_to_queue;
			if Int64Map.mem id q
			then Hashtbl.replace queue_lengths name (Hashtbl.find queue_lengths name - 1);
			Hashtbl.replace queues name (Int64Map.remove id q);
		end

	let wait from name =
		if Hashtbl.mem waiters name then begin
			let w = Hashtbl.find waiters name in
			Lwt_mutex.with_lock w.m
				(fun () ->
					let rec loop () =
						let _, _, not_seen = Int64Map.split from (get name) in
						if not_seen = Int64Map.empty then begin
							lwt () = Lwt_condition.wait ~mutex:w.m w.c in
							loop ()
						end else return () in
					loop ()
				)
		end else begin
			let t, _ = Lwt.task () in
			t (* block forever *)
		end

	let send conn_id name data =
		(* If a queue doesn't exist then drop the message *)
		if exists name then begin
			let w = Hashtbl.find waiters name in
			Lwt_mutex.with_lock w.m
				(fun () ->
					let q = get name in
					let origin = Connections.get_origin conn_id in
					let id = make_unique_id () in
					message_id_to_queue := Int64Map.add id name !message_id_to_queue;
					Hashtbl.replace queues name (Int64Map.add id (Protocol.Entry.make (get_time ()) origin data) q);
					Hashtbl.replace queue_lengths name (Hashtbl.find queue_lengths name + 1);
					Lwt_condition.broadcast w.c ();
					return (Some id)
				)
		end else return None

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
					Q.remove name;
				) qs;
			Hashtbl.remove queues session
		end

	let all () = Hashtbl.fold (fun _ set acc -> StringSet.union set acc) queues StringSet.empty
end

let snapshot () =
	let get_queue_contents q = Int64Map.fold (fun i e acc -> (i, e) :: acc) q [] in
	let open Protocol.Diagnostics in
	let queues qs =
		Hashtbl.fold (fun n q acc ->
			let queue_contents = get_queue_contents q in
			let next_transfer_expected = Q.get_next_transfer_expected n in
			(n, { queue_contents; next_transfer_expected }) :: acc
		) qs [] in
	let is_transient =
		let all = Transient_queue.all () in
		fun (x, _) -> StringSet.mem x all in
	let all_queues = queues Q.queues in
	let transient_queues, permanent_queues = List.partition is_transient all_queues in
	let current_time = get_time () in
	{ start_time; current_time; permanent_queues; transient_queues }

module Trace_buffer = struct
	let size = 128

	let buffer : (int64 * Protocol.Event.t) option array = Array.create size None
	let c = Lwt_condition.create ()

	let next_id = ref 0L

	let add event =
		let next_slot = Int64.(to_int (rem !next_id (of_int size))) in
		buffer.(next_slot) <- Some (!next_id, event);
		next_id := Int64.succ !next_id;
		Lwt_condition.broadcast c ()

	(* fold [f] over buffered items in chronological order *)
	let fold f acc =
		let next_slot = Int64.(to_int (rem !next_id (of_int size))) in
		let rec range start finish acc =
			if start > finish
			then acc
			else range (start + 1) finish (f buffer.(start) acc) in
		range 0 (next_slot - 1) (range next_slot (size - 1) acc)

	let get from timeout : (int64 * Protocol.Event.t) list Lwt.t =
		let sleep = Lwt_unix.sleep timeout in
		let wait_for_data =
			while_lwt !next_id <= from do
	   			Lwt_condition.wait c
			done in
		(* Wait until some data is available ie. when next_id > from (or timeout) *)
		lwt () = Lwt.pick [ sleep; wait_for_data ] in
		(* start from next_slot, looking for non-None entries which
		   are > from *)
		let reversed_results = fold (fun x acc -> match x with
			| None -> acc
			| Some (id, _) when id < from -> acc
			| Some (id, x) -> (id, x) :: acc) [] in
		return (List.rev reversed_results)

end

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
		lwt events = Trace_buffer.get from timeout in
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
		return (Out.List (Q.list prefix))
	| Some session, In.CreatePersistent name ->
		Q.add name;
		return (Out.Create name)
	| Some session, In.CreateTransient name ->
		Transient_queue.add session name;
		Q.add name;
		return (Out.Create name)
	| Some session, In.Destroy name ->
		Q.remove name;
		return Out.Destroy
	| Some session, In.Subscribe name ->
		Subscription.add session name;
		return Out.Subscribe
	| Some session, In.Ack id ->
		begin match Q.queue_of_id id with
		| None ->
			error "Ack %Ld: message doesn't exist" id
		| Some name ->
			Trace_buffer.add (Event.({time = Unix.gettimeofday (); input = Some session; queue = name; output = None; message = Ack id; processing_time = None }));
			Q.ack id;
		end;
		return Out.Ack
	| Some session, In.Transfer(from, timeout) ->
		let start = Unix.gettimeofday () in
		let rec wait () =
			let names = Subscription.get session in
			let not_seen = StringStringRelation.B_Set.fold (fun name map ->
				let q = Q.get name in
				Q.transfer name timeout;
				let _, _, not_seen = Int64Map.split from q in
				Int64Map.fold Int64Map.add map not_seen
			) names Int64Map.empty in
			if not_seen <> Int64Map.empty
			then return not_seen
			else
				let remaining_timeout = max 0. (start +. timeout -. (Unix.gettimeofday ())) in
				if remaining_timeout <= 0.
				then return Int64Map.empty
				else
					let timeout = Lwt.map (fun () -> `Timeout) (Lwt_unix.sleep remaining_timeout) in
					let more = StringStringRelation.B_Set.fold (fun name acc ->
						Lwt.map (fun () -> `Data) (Q.wait from name) :: acc
					) names [] in
					let t, u = Lwt.task () in
					Hashtbl.replace Subscription.session_to_wakeup session u;
					let sub = Lwt.map (fun () -> `Subscription) t in
					try_lwt
						match_lwt Lwt.pick (sub :: timeout :: more) with
						| `Timeout -> return Int64Map.empty
						| `Data ->
							wait ()
						| `Subscription ->
							wait ()
					finally
		   				Hashtbl.remove Subscription.session_to_wakeup session;
			   			return ()
				in
		lwt messages = wait () in
		let transfer = {
			Out.messages = Int64Map.fold (fun id e acc -> (id, e.Protocol.Entry.message) :: acc) messages [];
		} in
		let now = Unix.gettimeofday () in
		List.iter
			(fun (id, m) -> match Q.queue_of_id id with
			| None -> ()
			| Some name ->
				let processing_time = match m.Message.kind with
				| Message.Request _ -> None
				| Message.Response id' -> begin match Q.find id' with
					| Some request_entry ->
						Some (Int64.sub (get_time ()) request_entry.Entry.time)
					| None ->
						None
				end in
				Trace_buffer.add (Event.({time = now; input = None; queue = name; output = Some session; message = Message (id, m); processing_time }))
			) transfer.Out.messages;
		return (Out.Transfer transfer)
	| Some session, In.Send (name, data) ->
		begin match_lwt Q.send conn_id name data with
		| None -> return (Out.Send None)
		| Some id ->
			Trace_buffer.add (Event.({time = Unix.gettimeofday (); input = Some session; queue = name; output = None; message = Message (id, data); processing_time = None }));
			return (Out.Send (Some id))
		end

let make_server () =
	info "Started server on localhost:%d" !port;

	let (_: 'a) = logging_thread logger in

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

