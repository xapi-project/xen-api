(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Printf
open Threadext
open Pervasiveext
open Listext
open Event_types
open Stringext

module D=Debug.Debugger(struct let name="xapi_event" end)
open D

module Message = struct
	type t =
	| Create of (API.ref_message * API.message_t)
	| Del of API.ref_message

	let get_since_for_events : (__context:Context.t -> int64 -> (int64 * t list)) ref = ref ( fun ~__context _ -> ignore __context; (0L, []))
end

module Token = struct

	type t = int64 * int64 (* last id, message id *)

	exception Failed_to_parse of string

	let of_string token =
		match String.split ',' token with
			| [from;from_t] -> 
				(Int64.of_string from, Int64.of_string from_t)
			| [""] -> (0L, 0L)
			| _ ->
				raise (Failed_to_parse token)

	let to_string (last,last_t) =
		(* We prefix with zeroes so tokens which differ only in the generation
		   can be compared lexicographically as strings. *)
		Printf.sprintf "%020Ld,%020Ld" last last_t
end

module Subscription = struct
	type t = 
	| Class of string
	| Object of string * string
	| All

	let of_string x = if x = "*" then All else match String.split ~limit:2 '/' x with
	| [ cls ] -> Class (String.lowercase cls)
	| [ cls; id ] -> Object(String.lowercase cls, id)
	| _ ->
		raise (Api_errors.Server_error(Api_errors.event_subscription_parse_failure, [ x ]))

	let any = List.fold_left (fun acc x -> acc || x) false

	(** [table_matches subs tbl]: true if at least one subscription from [subs] would select some events from [tbl] *)
	let table_matches subs tbl =
		let tbl = String.lowercase tbl in
		let matches = function
		| All -> true
		| Class x -> x = tbl
		| Object (x, _) -> x = tbl in
		any (List.map matches subs)

	(** [event_matches subs ev]: true if at least one subscription from [subs] selects for event [ev] *)
	let event_matches subs ev =
		let tbl = String.lowercase ev.ty in
		let matches = function
		| All -> true
		| Class x -> x = tbl
		| Object (x, y) -> x = tbl && (y = ev.reference) in
		any (List.map matches subs)
end

module Next = struct
	(* Infrastructure for the deprecated Event.next *)

	(** Limit the event queue to this many events: *)
	let max_stored_events = 500

	(** Ordered list of events, newest first *)
	let queue = ref []

	(** Monotonically increasing event ID. One higher than the highest event ID in the queue *)
	let id = ref 0L 

	(** When we GC events we track how many we've deleted so we can send an error to the client *)
	let highest_forgotten_id = ref (-1L)

	type subscription = {
		mutable last_id: int64;            (* last event ID to sent to this client *)
		mutable subs: Subscription.t list; (* list of all the subscriptions *)
		m: Mutex.t;                        (* protects access to the mutable fields in this record *)
		session: API.ref_session;          (* session which owns this subscription *)
		mutable session_invalid: bool;     (* set to true if the associated session has been deleted *)
		mutable timeout: float;            (* Timeout *)
	}

	(* For Event.next, the single subscription associated with a session *)
	let subscriptions : (API.ref_session, subscription) Hashtbl.t = Hashtbl.create 10

	let m = Mutex.create ()
	let c = Condition.create ()

	(* Add an event to the queue if it matches any active subscriptions *)
	let add ev =
		Mutex.execute m
		(fun () ->
			let matches = Hashtbl.fold
				(fun _ s acc ->
					if Subscription.event_matches s.subs ev
					then true
					else acc
				) subscriptions false in
			if matches then begin
				queue := ev :: !queue;
				(* debug "Adding event %Ld: %s" (!id) (string_of_event ev); *)
				id := Int64.add !id Int64.one;
				Condition.broadcast c;
			end else begin
				(* debug "Dropping event %s" (string_of_event ev) *)
			end;
		
			(* GC the events in the queue *)
			let too_many = List.length !queue - max_stored_events in
			let to_keep, to_drop =
				if too_many <= 0
				then !queue, []
				else
					(* Reverse-sort by ID and preserve the first 'max_stored_events' *)
					List.chop max_stored_events (List.sort (fun a b -> compare b.id a.id) !queue) in
			queue := to_keep;
			(* Remember the highest ID of the list of events to drop *)
			if to_drop <> [] then
			highest_forgotten_id := (List.hd to_drop).id;
			(* debug "After event queue GC: keeping %d; dropping %d (highest dropped id = %Ld)" 
			(List.length to_keep) (List.length to_drop) !highest_forgotten_id *)
		)

	let assert_subscribed session =
		Mutex.execute m
		(fun () ->
			if not(Hashtbl.mem subscriptions session) 
			then raise (Api_errors.Server_error(Api_errors.session_not_registered, [ Context.trackid_of_session (Some session) ])))

	(* Fetch the single subscription_record associated with a session or create
	   one if one doesn't exist already *)
	let get_subscription session = 
		Mutex.execute m
		(fun () ->
			if Hashtbl.mem subscriptions session then begin
				Hashtbl.find subscriptions session
			end else begin 
				let subscription = { last_id = !id; subs = []; m = Mutex.create(); session = session; session_invalid = false; timeout=0.0; } in
				Hashtbl.replace subscriptions session subscription;
				subscription
		end)

	let on_session_deleted session_id =
		Mutex.execute m
		(fun () -> 

			let mark_invalid sub =
				(* Mark the subscription as invalid and wake everyone up *)
				Mutex.execute sub.m (fun () -> sub.session_invalid <- true);
				Condition.broadcast c in

			if Hashtbl.mem subscriptions session_id then begin 
				let sub = Hashtbl.find subscriptions session_id in
				mark_invalid sub;
				Hashtbl.remove subscriptions session_id;
			end;
		)

	let session_is_invalid sub = Mutex.execute sub.m (fun () -> sub.session_invalid)

	(* Blocks the caller until the current ID has changed OR the session has been 
	    invalidated. *)
	let wait subscription from_id = 
		let result = ref 0L in
		Mutex.execute m
		(fun () ->
			(* NB we occasionally grab the specific session lock while holding the general lock *)
			while !id = from_id && not (session_is_invalid subscription) do Condition.wait c m done;
		result := !id);
		if session_is_invalid subscription
		then raise (Api_errors.Server_error(Api_errors.session_invalid, [ Ref.string_of subscription.session ]))
		else !result

	(* Thrown if the user requests events which we don't have because we've thrown
	   then away. This should only happen if more than max_stored_events are produced 
	   between successive calls to Event.next (). The client should refresh all its state
	   manually before calling Event.next () again.
	*)
	let events_lost () = raise (Api_errors.Server_error (Api_errors.events_lost, []))

	(* Return events from the queue between a start and an end ID. Throws
	   an API error if some events have been lost, signalling the client to
	   re-register. *)
	let events_read id_start id_end =
		let check_ev ev = id_start <= ev.id && ev.id < id_end in

		let some_events_lost = ref false in
		let selected_events =
			Mutex.execute m
			(fun () ->
				some_events_lost := !highest_forgotten_id >= id_start;
				List.find_all (fun ev -> check_ev ev) !queue
			) in
		(* Note we may actually retrieve fewer events than we expect because the
		   queue may have been coalesced. *)
		if !some_events_lost (* is true *) then events_lost ();

		(* NB queue is kept in reverse order *)
		List.rev selected_events

end

module From = struct

	let m = Mutex.create ()
	let c = Condition.create ()

	let next_index =
		let id = ref 0L in
		fun () ->
			Mutex.execute m (fun () ->
				let result = !id in
				id := Int64.succ !id;
				result
			)

	(* A (blocking) call which should be unblocked on logout *)
	type call = {
		index: int64;                  (* Unique id for this call *)
		mutable cur_id: int64;         (* Most current generation count relevant to the client *)
		subs: Subscription.t list;     (* list of all the subscriptions *)
		session: API.ref_session;      (* the session associated with this call *)
		mutable session_invalid: bool; (* set to true if the associated session has been deleted *)
		m: Mutex.t;                    (* protects access to the mutable fields in this record *)
	}

	(* The set of (blocking) calls associated with a session *)
	let calls : (API.ref_session, call list) Hashtbl.t = Hashtbl.create 10

	let get_current_event_number () =
		(Db_cache_types.Manifest.generation (Db_cache_types.Database.manifest (Db_ref.get_database (Db_backend.make ()))))

	(* Add an event to the queue if it matches any active subscriptions *)
	let add ev =
		Mutex.execute m
		(fun () ->
			let matches_per_thread = Hashtbl.fold
			(fun _ s acc ->
				List.fold_left (fun acc s ->
					if Subscription.event_matches s.subs ev
					then (s.cur_id <- get_current_event_number (); true)
					else acc) acc s
			) calls false in
			if matches_per_thread then Condition.broadcast c;
		)

	(* Call a function with a registered call which will be woken up if
	   the session is destroyed in the background.  *)
	let with_call session subs f =
		let index = next_index () in
		let fresh = { index; cur_id = 0L; subs; m = Mutex.create(); session; session_invalid = false; } in
		Mutex.execute m
		(fun () ->
			let existing =
				if Hashtbl.mem calls session
				then Hashtbl.find calls session
				else [] in
			Hashtbl.replace calls session (fresh :: existing)
		);
		finally
			(fun () -> f fresh)
			(fun () -> Mutex.execute m (fun () ->
				if Hashtbl.mem calls session then begin
					let existing = Hashtbl.find calls session in
					let remaining = List.filter (fun x -> not(x.index = fresh.index)) existing in
					if remaining = []
					then Hashtbl.remove calls session
					else Hashtbl.replace calls session remaining
				end
			))

	(* Is called by the session timeout code *)
	let on_session_deleted session_id =
		Mutex.execute m
		(fun () -> 
			let mark_invalid sub =
				(* Mark the subscription as invalid and wake everyone up *)
				Mutex.execute sub.m (fun () -> sub.session_invalid <- true);
				Condition.broadcast c in

			if Hashtbl.mem calls session_id then begin
				List.iter mark_invalid (Hashtbl.find calls session_id);
				Hashtbl.remove calls session_id;
			end;
		)

	let session_is_invalid call = Mutex.execute call.m (fun () -> call.session_invalid)

	let wait2 call from_id deadline =
		let timeoutname = Printf.sprintf "event_from_timeout_%Ld" call.index in
		Mutex.execute m
		(fun () ->
			while from_id = call.cur_id && not (session_is_invalid call) && Unix.gettimeofday () < deadline do 
				Xapi_periodic_scheduler.add_to_queue timeoutname Xapi_periodic_scheduler.OneShot (deadline -. Unix.gettimeofday () +. 0.5) (fun () -> Condition.broadcast c);
				Condition.wait c m;
				Xapi_periodic_scheduler.remove_from_queue timeoutname
			done;
		);
		if session_is_invalid call then begin
			info "%s raising SESSION_INVALID *because* subscription is invalid" (Context.trackid_of_session (Some call.session));
			raise (Api_errors.Server_error(Api_errors.session_invalid, [ Ref.string_of call.session ]))
		end
end

(** Register an interest in events generated on objects of class <class_name> *)
let register ~__context ~classes =
	let session = Context.get_session_id __context in
	let open Next in
	let subs = List.map Subscription.of_string classes in
	let sub = Next.get_subscription session in
	Mutex.execute sub.m (fun () -> sub.subs <- subs @ sub.subs)

(** Unregister interest in events generated on objects of class <class_name> *)
let unregister ~__context ~classes = 
	let session = Context.get_session_id __context in
	let open Next in
	let subs = List.map Subscription.of_string classes in
	let sub = Next.get_subscription session in
	Mutex.execute sub.m
		(fun () -> sub.subs <- List.filter (fun x -> not(List.mem x subs)) sub.subs)

(** Blocking call which returns the next set of events relevant to this session. *)
let rec next ~__context =
	let session = Context.get_session_id __context in
	let open Next in
	assert_subscribed session;

	let subscription = get_subscription session in

	(* Return a <from_id, end_id> exclusive range that is guaranteed to be specific to this 
	   thread. Concurrent calls will grab wholly disjoint ranges. Note the range might be
	   empty. *)
	let grab_range () = 
		(* Briefly hold both the general and the specific mutex *)
	  	Mutex.execute m
		  (fun () -> Mutex.execute subscription.m
		     (fun () ->
			let last_id = subscription.last_id in
			(* Bump our last_id counter: these events don't have to be looked at again *)
			subscription.last_id <- !id ;
			last_id, !id)) in
	(* Like grab_range () only guarantees to return a non-empty range by blocking if necessary *)
	let rec grab_nonempty_range () = 
		let last_id, end_id = grab_range () in
		if last_id = end_id then begin
			let (_: int64) = wait subscription end_id in
			grab_nonempty_range ()
		end else last_id, end_id in

	let last_id, end_id = grab_nonempty_range () in
	(* debug "next examining events in range %Ld <= x < %Ld" last_id end_id; *)
	(* Are any of the new events interesting? *)
	let events = events_read last_id end_id in
	let subs = Mutex.execute subscription.m (fun () -> subscription.subs) in
	let relevant = List.filter (fun ev -> Subscription.event_matches subs ev) events in
	(* debug "number of relevant events = %d" (List.length relevant); *)
	if relevant = [] then next ~__context 
	else XMLRPC.To.array (List.map xmlrpc_of_event relevant)

let from_inner __context session subs from from_t deadline = 
	let open From in

	(* The database tables involved in our subscription *)
	let tables =
		let all =
			let objs = List.filter (fun x->x.Datamodel_types.gen_events) (Dm_api.objects_of_api Datamodel.all_api) in
			let objs = List.map (fun x->x.Datamodel_types.name) objs in
			objs in
		List.filter (fun table -> Subscription.table_matches subs table) all in

	let last_generation = ref from in
	let last_msg_gen = ref from_t in

	let grab_range t =
		let tableset = Db_cache_types.Database.tableset (Db_ref.get_database t) in
		let (msg_gen,messages) =
			if Subscription.table_matches subs "message" then (!Message.get_since_for_events) ~__context !last_msg_gen else (0L, []) in
		(msg_gen, messages, tableset, List.fold_left
			(fun acc table ->
		 		Db_cache_types.Table.fold_over_recent !last_generation
					 (fun ctime mtime dtime objref (creates,mods,deletes,last) ->
						let last = max last (max mtime dtime) in (* mtime guaranteed to always be larger than ctime *)
				  		if dtime > 0L then begin
							if ctime > !last_generation then
								(creates,mods,deletes,last) (* It was created and destroyed since the last update *)
							else
								(creates,mods,(table, objref, dtime)::deletes,last) (* It might have been modified, but we can't tell now *)
						end else begin
							((if ctime > !last_generation then (table, objref, ctime)::creates else creates),
							(if mtime > !last_generation then (table, objref, mtime)::mods else mods),
							deletes, last)
						end
					) (fun () -> ()) (Db_cache_types.TableSet.find table tableset) acc
			) ([],[],[],!last_generation) tables) in

	(* Each event.from should have an independent subscription record *)
	let msg_gen, messages, tableset, (creates, mods, deletes, last) =
		with_call session subs
			(fun sub ->
				let rec grab_nonempty_range () =
					let (msg_gen, messages, tableset, (creates,mods,deletes,last)) as result = Db_lock.with_lock (fun () -> grab_range (Db_backend.make ())) in
					if creates = [] && mods = [] && deletes = [] && messages = [] && Unix.gettimeofday () < deadline then begin
						last_generation := last; (* Cur_id was bumped, but nothing relevent fell out of the db. Therefore the *)
						sub.cur_id <- last; (* last id the client got is equivalent to the current one *)
						last_msg_gen := msg_gen;
						wait2 sub last deadline;
						Thread.delay 0.05;
						grab_nonempty_range ()
					end else
						result in
				grab_nonempty_range ()
			) in

	last_generation := last;

	let event_of op ?snapshot (table, objref, time) = {
		id=time; ts=0.0; ty=String.lowercase table; op=op; reference=objref; snapshot=snapshot
	} in
	let events = List.fold_left (fun acc x ->
		let ev = event_of Del x in
		if Subscription.event_matches subs ev then ev::acc else acc
	) [] deletes in
	let events = List.fold_left (fun acc (table, objref, mtime) ->
		let serialiser = Eventgen.find_get_record table in
		try 
			let xml = serialiser ~__context ~self:objref () in
			let ev = event_of Mod ?snapshot:xml (table, objref, mtime) in
			if Subscription.event_matches subs ev then ev::acc else acc
		with _ -> acc
	) events mods in
	let events = List.fold_left (fun acc (table, objref, ctime) ->
		let serialiser = Eventgen.find_get_record table in
		try 
			let xml = serialiser ~__context ~self:objref () in
			let ev = event_of Add ?snapshot:xml (table, objref, ctime) in
			if Subscription.event_matches subs ev then ev::acc else acc
		with _ -> acc
	) events creates in
	let events = List.fold_left (fun acc mev ->
		let event = match mev with 
		| Message.Create (_ref,message) -> event_of Add ?snapshot:(Some (API.To.message_t message)) ("message", Ref.string_of _ref, 0L)
		| Message.Del _ref -> event_of Del ("message",Ref.string_of _ref, 0L) in
		event::acc) events messages in

	let valid_ref_counts =
		Db_cache_types.TableSet.fold
			(fun tablename _ _ table acc ->
				(String.lowercase tablename,
					(Db_cache_types.Table.fold
						(fun r _ _ _ acc -> Int32.add 1l acc) table 0l))::acc)
		tableset [] in

	{
		events; valid_ref_counts;
		token = Token.to_string (last,msg_gen);
	}

let from ~__context ~classes ~token ~timeout =
	let session = Context.get_session_id __context in
	let from, from_t = 
		try
			Token.of_string token
		with e ->
			warn "Failed to parse event.from token: %s (%s)" token (Printexc.to_string e);
			raise (Api_errors.Server_error(Api_errors.event_from_token_parse_failure, [ token ])) in

	let subs = List.map Subscription.of_string classes in

	let deadline = Unix.gettimeofday () +. timeout in

	(* We need to iterate because it's possible for an empty event set
	   to be generated if we peek in-between a Modify and a Delete; we'll
	   miss the Delete event and fail to generate the Modify because the
	   snapshot can't be taken. *)
	let rec loop () =
		let event_from = from_inner __context session subs from from_t deadline in
		if event_from.events = [] && (Unix.gettimeofday () < deadline) then begin
			debug "suppressing empty event.from";
			loop ()
		end else begin
			xmlrpc_of_event_from event_from
		end in
	loop ()

let get_current_id ~__context = Mutex.execute Next.m (fun () -> !Next.id)

let inject ~__context ~_class ~ref =
	let open Db_cache_types in
	let generation : int64 = Db_lock.with_lock
		(fun () ->
			let db_ref = Db_backend.make () in
			let g = Manifest.generation (Database.manifest (Db_ref.get_database db_ref)) in
			Db_cache_impl.refresh_row db_ref _class ref; (* consumes this generation *)
			g
		) in
	let token = Int64.sub generation 1L, 0L in
	Token.to_string token

(* Internal interface ****************************************************)

let event_add ?snapshot ty op reference  =
	let objs = List.filter (fun x->x.Datamodel_types.gen_events) (Dm_api.objects_of_api Datamodel.all_api) in
	let objs = List.map (fun x->x.Datamodel_types.name) objs in
	if List.mem ty objs then begin
		let ts = Unix.time () in
		let op = op_of_string op in

		let ev = { id = !Next.id; ts; ty = String.lowercase ty; op; reference; snapshot } in
		From.add ev;
		Next.add ev
	end

let register_hooks () = Db_action_helper.events_register event_add

(* Called whenever a session is being destroyed i.e. by Session.logout and db_gc *)
let on_session_deleted session_id = 
	(* Unregister this session if is associated with in imported DB. *)
	(* FIXME: this doesn't logically belong in the event code *)
	Db_backend.unregister_session session_id;

	Next.on_session_deleted session_id;
	From.on_session_deleted session_id

(* Inject an unnecessary update as a heartbeat. This will:
    1. hopefully prevent some firewalls from silently closing the connection
    2. allow the server to detect when a client has failed *)
let heartbeat ~__context =
	try
		Db_lock.with_lock 
		(fun () ->
			(* We must hold the database lock since we are sending an update for a real object
			   and we don't want to accidentally transmit an older snapshot. *)
			let pool = try Some (Helpers.get_pool ~__context) with _ -> None in
			match pool with
			| Some pool ->
				let pool_r = Db.Pool.get_record ~__context ~self:pool in
				let pool_xml = API.To.pool_t pool_r in
				event_add ~snapshot:pool_xml "pool" "mod" (Ref.string_of pool)
			| None -> () (* no pool object created during initial boot *)
		)
	with e ->
		error "Caught exception sending event heartbeat: %s" (ExnHelper.string_of_exn e)
