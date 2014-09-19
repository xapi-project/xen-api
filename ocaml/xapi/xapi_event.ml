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
open Listext
open Event_types
open Stringext

module D=Debug.Debugger(struct let name="xapi_event" end)
open D

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


type message_event = MCreate of (API.ref_message * API.message_t) | MDel of API.ref_message
let message_get_since_for_events : (__context:Context.t -> int64 -> (int64 * message_event list)) ref = ref ( fun ~__context _ -> ignore __context; (0L, []))

(** Limit the event queue to this many events: *)
let max_stored_events = 500

(** Ordered list of events, newest first *)
let queue = ref []
(** Monotonically increasing event ID. One higher than the highest event ID in the queue *)
let id = ref 0L 
(** When we GC events we track how many we've deleted so we can send an error to the client *)
let highest_forgotten_id = ref (-1L)

(** Types used to store user event subscriptions: ***********************************************)
type subscription = 
    | Class of string           (** subscribe to all events for objects of this class *)
	| Object of string * string (** subscribe to all events for this specific object *)
    | All                       (** subscribe to everything *)

let subscription_of_string x = if x = "*" then All else match String.split ~limit:2 '/' x with
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

(** Every session that calls 'register' gets a subscription*)
type subscription_record = {
	mutable last_id: int64;           (** last event ID to sent to this client *)
	mutable last_msg_gen : int64;     (** last generation count from the messages *)
	mutable last_generation : int64;  (** Generation count of the last event *)
	mutable cur_id: int64;            (** Most current generation count relevant to the client - only used in new events mechanism *)
	mutable subs: subscription list;  (** list of all the subscriptions *)
	m: Mutex.t;                       (** protects access to the mutable fields in this record *)
	session: API.ref_session;         (** session which owns this subscription *)
	mutable session_invalid: bool;    (** set to true if the associated session has been deleted *)
	mutable timeout: float;           (** Timeout *)
}


(** Thrown if the user requests events which we don't have because we've thrown
    then away. This should only happen if more than max_stored_events are produced 
    between successive calls to Event.next (). The client should refresh all its state
    manually before calling Event.next () again.
*)
let events_lost () = raise (Api_errors.Server_error (Api_errors.events_lost, []))

let get_current_event_number () =
  (Db_cache_types.Manifest.generation (Db_cache_types.Database.manifest (Db_ref.get_database (Db_backend.make ()))))

(* Mapping of session IDs to lists of subscribed classes *)
let subscriptions = Hashtbl.create 10

(* Lock protects the global event queue reference and the subscriptions hashtable *)
let event_lock = Mutex.create ()
let newevents = Condition.create ()

let event_add ?snapshot ty op reference  =

  let gen_events_for tbl =
    let objs = List.filter (fun x->x.Datamodel_types.gen_events) (Dm_api.objects_of_api Datamodel.all_api) in
    let objs = List.map (fun x->x.Datamodel_types.name) objs in
      List.mem tbl objs in

    if not (gen_events_for ty) then ()
    else
      begin

	let ts = Unix.time () in
	let op = op_of_string op in

	Mutex.execute event_lock
	(fun () ->
		let ev = { id = !id; ts = ts; ty = String.lowercase ty; op = op; reference = reference; 
			   snapshot = snapshot } in

		let matches_anything = Hashtbl.fold
			(fun _ s acc ->
				 if event_matches s.subs ev
				 then (s.cur_id <- get_current_event_number (); true)
				 else acc) subscriptions false in
		if matches_anything then begin
			queue := ev :: !queue;
			(* debug "Adding event %Ld: %s" (!id) (string_of_event ev); *)
			id := Int64.add !id Int64.one;
			Condition.broadcast newevents;
		end else begin
			(* debug "Dropping event %s" (string_of_event ev) *)
		end;
		
		(* GC the events in the queue *)
		let too_many = List.length !queue - max_stored_events in
		let to_keep, to_drop = if too_many <= 0 then !queue, []
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
      end


let register_hooks () =
	Db_action_helper.events_register event_add

(** Return the subscription associated with a session, or create a new blank one if none
    has yet been created. *)
let get_subscription ~__context = 
	let session = Context.get_session_id __context in
	Mutex.execute event_lock
	(fun () ->
	   if Hashtbl.mem subscriptions session then Hashtbl.find subscriptions session
	   else 
		   let subscription = { last_id = !id; last_msg_gen = 0L; last_generation=0L; cur_id = 0L; subs = []; m = Mutex.create(); session = session; session_invalid = false; timeout=0.0; } in
	     Hashtbl.replace subscriptions session subscription;
	     subscription)

(** Raises an exception if the provided session has not already registered for some events *)
let assert_subscribed ~__context = 
	let session = Context.get_session_id __context in
	Mutex.execute event_lock
	(fun () ->
	   if not(Hashtbl.mem subscriptions session) 
	   then raise (Api_errors.Server_error(Api_errors.session_not_registered, [ Context.trackid_of_session (Some session) ])))

(** Register an interest in events generated on objects of class <class_name> *)
let register ~__context ~classes = 
	let subs = List.map subscription_of_string classes in
	let sub = get_subscription ~__context in
	Mutex.execute sub.m (fun () -> sub.subs <- subs @ sub.subs)


(** Unregister interest in events generated on objects of class <class_name> *)
let unregister ~__context ~classes = 
	let subs = List.map subscription_of_string classes in
	let sub = get_subscription ~__context in
	Mutex.execute sub.m
		(fun () -> sub.subs <- List.filter (fun x -> not(List.mem x subs)) sub.subs)

(** Is called by the session timeout code *)
let on_session_deleted session_id = Mutex.execute event_lock 
	(fun () -> 
	   (* Unregister this session if is associated with in imported DB. *)
	   Db_backend.unregister_session session_id;
	   if Hashtbl.mem subscriptions session_id then begin 
	     let sub = Hashtbl.find subscriptions session_id in
	     (* Mark the subscription as invalid and wake everyone up *)
	     Mutex.execute sub.m (fun () -> sub.session_invalid <- true);
	     Hashtbl.remove subscriptions session_id;
	     Condition.broadcast newevents;
	   end)

let session_is_invalid sub = Mutex.execute sub.m (fun () -> sub.session_invalid)

(** Blocks the caller until the current ID has changed OR the session has been 
    invalidated. *)
let wait subscription from_id = 
	let result = ref 0L in
	Mutex.execute event_lock
	  (fun () ->
	     (* NB we occasionally grab the specific session lock while holding the general lock *)
	     while !id = from_id && not (session_is_invalid subscription) do Condition.wait newevents event_lock done;
	     result := !id);
	if session_is_invalid subscription
	then raise (Api_errors.Server_error(Api_errors.session_invalid, [ Ref.string_of subscription.session ]))
	else !result

let wait2 subscription from_id =
	let timeoutname = Printf.sprintf "event_from_timeout_%s" (Ref.string_of subscription.session) in
  Mutex.execute event_lock
	(fun () ->
	  while from_id = subscription.cur_id && not (session_is_invalid subscription) && Unix.gettimeofday () < subscription.timeout 
	  do 
		  Xapi_periodic_scheduler.add_to_queue timeoutname Xapi_periodic_scheduler.OneShot (subscription.timeout -. Unix.gettimeofday () +. 0.5) (fun () -> Condition.broadcast newevents);
		  Condition.wait newevents event_lock; 
		  Xapi_periodic_scheduler.remove_from_queue timeoutname
	  done;
	);
  if session_is_invalid subscription
  then raise (Api_errors.Server_error(Api_errors.session_invalid, [ Ref.string_of subscription.session ]))
  else ()

(** Internal function to return a list of events between a start and an end ID. 
    We assume that our 64bit counter never wraps. *)
let events_read id_start id_end =
	let check_ev ev = id_start <= ev.id && ev.id < id_end in

	let some_events_lost = ref false in
	let selected_events = Mutex.execute event_lock
	  (fun () ->
	     some_events_lost := !highest_forgotten_id >= id_start;
	     List.find_all (fun ev -> check_ev ev) !queue) in
	(* Note we may actually retrieve fewer events than we expect because the
	   queue may have been coalesced. *)
	if !some_events_lost (* is true *) then events_lost ();

	(* NB queue is kept in reverse order *)
	List.rev selected_events

(** Blocking call which returns the next set of events relevant to this session. *)
let rec next ~__context =
	assert_subscribed ~__context;

	let subscription = get_subscription ~__context in

	(* Return a <from_id, end_id> exclusive range that is guaranteed to be specific to this 
	   thread. Concurrent calls will grab wholly disjoint ranges. Note the range might be
	   empty. *)
	let grab_range () = 
		(* Briefly hold both the general and the specific mutex *)
	  	Mutex.execute event_lock 
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
	let relevant = List.filter (fun ev -> event_matches subs ev) events in
	(* debug "number of relevant events = %d" (List.length relevant); *)
	if relevant = [] then next ~__context 
	else XMLRPC.To.array (List.map xmlrpc_of_event relevant)

let from ~__context ~classes ~token ~timeout = 
	let from, from_t = 
		try
			Token.of_string token
		with e ->
			warn "Failed to parse event.from token: %s (%s)" token (Printexc.to_string e);
			raise (Api_errors.Server_error(Api_errors.event_from_token_parse_failure, [ token ])) in

	(* Temporarily create a subscription for the duration of this call *)
	let subs = List.map subscription_of_string classes in
	let sub = get_subscription ~__context in

	sub.timeout <- Unix.gettimeofday () +. timeout;

	sub.last_generation <- from;
	sub.last_msg_gen <- from_t;

	Mutex.execute sub.m (fun () -> sub.subs <- subs @ sub.subs);

	let all_event_tables =
		let objs = List.filter (fun x->x.Datamodel_types.gen_events) (Dm_api.objects_of_api Datamodel.all_api) in
		let objs = List.map (fun x->x.Datamodel_types.name) objs in
		objs
	in

	let tables = List.filter (fun table -> table_matches sub.subs table) all_event_tables in

	let events_lost = ref [] in

	let grab_range t =
		let tableset = Db_cache_types.Database.tableset (Db_ref.get_database t) in
		let (msg_gen,messages) =
			if table_matches sub.subs "message" then (!message_get_since_for_events) ~__context sub.last_msg_gen else (0L, []) in
		(msg_gen, messages, tableset, List.fold_left
			(fun acc table ->
				 Db_cache_types.Table.fold_over_recent sub.last_generation
					 (fun ctime mtime dtime objref (creates,mods,deletes,last) ->
						  let last = max last (max mtime dtime) in (* mtime guaranteed to always be larger than ctime *)
						  if dtime > 0L then begin
							  if ctime > sub.last_generation then
								  (creates,mods,deletes,last) (* It was created and destroyed since the last update *)
							  else
								  (creates,mods,(table, objref, dtime)::deletes,last) (* It might have been modified, but we can't tell now *)
						  end else begin
							  ((if ctime > sub.last_generation then (table, objref, ctime)::creates else creates),
							   (if mtime > sub.last_generation then (table, objref, mtime)::mods else mods),
							   deletes, last)
						  end
					 ) (fun () -> events_lost := table :: !events_lost) (Db_cache_types.TableSet.find table tableset) acc
			) ([],[],[],sub.last_generation) tables)
	in

	let rec grab_nonempty_range () =
		let (msg_gen, messages, tableset, (creates,mods,deletes,last)) as result = Db_lock.with_lock (fun () -> grab_range (Db_backend.make ())) in
		if List.length creates = 0 && List.length mods = 0 && List.length deletes = 0 && List.length messages = 0 && Unix.gettimeofday () < sub.timeout
		then
			(
				sub.last_generation <- last; (* Cur_id was bumped, but nothing relevent fell out of the db. Therefore the *)
				sub.cur_id <- last; (* last id the client got is equivalent to the current one *)
				sub.last_msg_gen <- msg_gen;
				wait2 sub last;
				Thread.delay 0.05;
				grab_nonempty_range ())
		else
			result
	in

	let (msg_gen, messages, tableset, (creates,mods,deletes,last)) = grab_nonempty_range () in

	sub.last_generation <- last;

	let event_of op ?snapshot (table, objref, time) =
		{
			id=time;
			ts=0.0;
			ty=String.lowercase table;
			op=op;
			reference=objref;
			snapshot=snapshot
		} in
	let delevs = List.fold_left (fun acc x ->
		let ev = event_of Del x in
		if event_matches sub.subs ev then ev::acc else acc
	) [] deletes in

	let modevs = List.fold_left (fun acc (table, objref, mtime) ->
		let serialiser = Eventgen.find_get_record table in
		try 
			let xml = serialiser ~__context ~self:objref () in
			let ev = event_of Mod ?snapshot:xml (table, objref, mtime) in
			if event_matches sub.subs ev then ev::acc else acc
		with _ -> acc
	) delevs mods in

	let createevs = List.fold_left (fun acc (table, objref, ctime) ->
		let serialiser = Eventgen.find_get_record table in
		try 
			let xml = serialiser ~__context ~self:objref () in
			let ev = event_of Add ?snapshot:xml (table, objref, ctime) in
			if event_matches sub.subs ev then ev::acc else acc
		with _ -> acc
	) modevs creates in
	
	let message_events = List.fold_left (fun acc mev ->
		let event = match mev with 
			| MCreate (_ref,message) -> event_of Add ?snapshot:(Some (API.To.message_t message)) ("message", Ref.string_of _ref, 0L)
			| MDel _ref -> event_of Del ("message",Ref.string_of _ref, 0L)
		in
		event::acc) createevs messages in

	let valid_ref_counts =
        Db_cache_types.TableSet.fold
            (fun tablename _ _ table acc ->
                (String.lowercase tablename,
                    (Db_cache_types.Table.fold
                        (fun r _ _ _ acc -> Int32.add 1l acc) table 0l))::acc)
            tableset [] in

	let session = Context.get_session_id __context in

	on_session_deleted session;

	let result = {
		events = message_events;
		valid_ref_counts = valid_ref_counts;
		token = Token.to_string (last,msg_gen);
	} in
	xmlrpc_of_event_from result

let get_current_id ~__context = Mutex.execute event_lock (fun () -> !id)

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

(** Inject an unnecessary update as a heartbeat. This will:
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
