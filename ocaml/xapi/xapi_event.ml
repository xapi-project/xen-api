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

	type t = int64 * int64 * Bloom.t with rpc (* last id, message id, bloom filter *)

	exception Failed_to_parse of string

	let of_string token =
		match String.split ~limit:3 ',' token with
			| [from;from_t;rest] -> 
				(Int64.of_string from, Int64.of_string from_t,Bloom.t_of_rpc (Jsonrpc.of_string rest))
			| [""] -> (0L, 0L,Bloom.create 1 1)
			| _ -> 
				raise (Failed_to_parse token)

	let to_string (last,last_t,bloom) = 
		Printf.sprintf "%020Ld,%020Ld,%s" last last_t (Jsonrpc.to_string (Bloom.rpc_of_t bloom))

end

(** Types used to store user event subscriptions: ***********************************************)
type subscription =
	| Class of string * (string list) * (Db_filter_types.expr)           (** subscribe to all events for objects of this class *)
	| All                                       (** subscribe to everything *)


(** Every session that calls 'register' gets a subscription*)
type next_subscription_record = {
	mutable last_id: int64;           (** last event ID to sent to this client *)
}

type from_subscription_record = {
	mutable last_msg_gen : int64;     (** last generation count from the messages *)
	mutable last_generation : int64;  (** Generation count of the last event *)
	mutable should_wait: bool;        (** True if the call should currently be waiting (nothing interesting the the db) *)
	from_id : int;
	cond : Condition.t;
	bloom: Bloom.t;
	timeout: float;
}

type gen_subscription_record = {
	mutable subs: subscription list;  (** list of all the subscriptions *)
	m: Mutex.t;                       (** protects access to the mutable fields in this record *)
	session: API.ref_session;         (** session which owns this subscription *)
	mutable session_invalid: bool;    (** set to true if the associated session has been deleted *)
}

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

let subscription_of_string x =
	match x with
		| "*" -> All
		| x ->
			let fields_of_sub sub =
				try
					let i = String.index sub '[' in
					let e = String.index sub ']' in
					let fields = String.sub sub (i+1) (e-i-1) in
					let pre_sub = String.sub sub 0 i in
					(pre_sub,String.split ',' fields)
				with _ -> (sub,[])
			in
			let expr_of_sub sub =
				try
					let i = String.index sub '(' in
					let e = String.index sub ')' in
					let expr = String.sub sub (i+1) (e-i-1) in
					debug "XXXX: Got expr: %s" expr;
					let pre_sub = String.sub sub 0 i in
					(pre_sub,Db_filter.expr_of_string expr)
				with e -> 
					debug "Caught exn: %s" (Printexc.to_string e);
					(sub,Db_filter_types.True)
			in
			let pre_sub,expr = expr_of_sub x in
			let pre_sub,fields = fields_of_sub pre_sub in
			debug "XXXX: Expr: %s" (Db_filter.string_of_expr expr);
			match String.split ~limit:2 '/' pre_sub with
				| [ cls ] -> Class (String.lowercase cls, fields, expr)
				| [ cls; id ] -> Class (String.lowercase cls, fields, Db_filter_types.(And (Eq (Field "_ref", Literal id), expr)))
				| _ ->
					raise (Api_errors.Server_error(Api_errors.event_subscription_parse_failure, [ x ]))

let any = List.fold_left (fun acc x -> acc || x) false

(** [table_matches subs tbl]: true if at least one subscription from [subs] would select some events from [tbl] *)
let table_matches subs tbl =
	let tbl = String.lowercase tbl in
	let matches = function
		| All -> true
		| Class (x, _, _) -> x = tbl in
	any (List.map matches subs)

(** [event_matches subs ev]: true if at least one subscription from [subs] selects for event [ev] *)
let event_from_matches_expr gen_subscr from_subscr tbl objref row =
	let tbl = String.lowercase tbl in
	debug "event_from_matches_expr: tbl=%s objref=%s" tbl objref;

	let matches_expr cls expr =
		match row with
			| None -> true
			| Some r ->
				let eval_val = function
					| Db_filter_types.Literal x -> x
					| Db_filter_types.Field x -> Db_cache_types.Row.find x r in
				Db_filter.eval_expr eval_val expr
	in

	let matches = function
		| All -> true
		| Class (x, _, expr) ->
			debug "Testing for class=%s, expr=%s" x (Db_filter.string_of_expr expr);
			(x=tbl) && (matches_expr x expr)
	in

	let result = any (List.map matches gen_subscr.subs) in
	debug "Result=%b" result;
	result

let event_from_matches_bloom from_subscr objref =
	Bloom.test from_subscr.bloom objref
	
let event_next_matches gen_subscr ev =
	let tbl = String.lowercase ev.ty in
	let matches = function | All -> true | Class (x, _, _) -> x=tbl in
	let subs = Mutex.execute gen_subscr.m (fun () -> gen_subscr.subs) in
	any (List.map matches subs)

let fieldset_of_sub subs cls objref =
	if List.mem All subs
	then [] (* == all *)
	else begin
		let to_filter = List.filter_map
			(function
				| Class (x,fields,_) ->
					if x=String.lowercase cls
					then Some fields
					else None
				| _ -> None) subs in
		let f = List.concat to_filter in
		List.setify f
	end


(** Thrown if the user requests events which we don't have because we've thrown
	then away. This should only happen if more than max_stored_events are produced
	between successive calls to Event.next (). The client should refresh all its state
	manually before calling Event.next () again.
*)
let events_lost () = raise (Api_errors.Server_error (Api_errors.events_lost, []))

let get_current_event_number () =
  (Db_cache_types.Manifest.generation (Db_cache_types.Database.manifest (Db_ref.get_database (Db_backend.make ()))))

(* Mapping of session IDs to lists of subscribed classes - protected by event_lock *)
let next_subscriptions = Hashtbl.create 10

(* List of currently-in-progress Event.from / Event.fields_from subscriptions *)
let from_subscriptions = ref []

(* Lock protects the global event queue reference and the subscriptions hashtable *)
let event_lock = Mutex.create ()
let newevents = Condition.create ()
let from_id = ref 0 (* Unique ID for each Event.from/fields_from call *)

let event_add ?snapshot ?row ty op reference  =
	let gen_events_for tbl =
		let objs = List.filter (fun x->x.Datamodel_types.gen_events) (Dm_api.objects_of_api Datamodel.all_api) in
		let objs = List.map (fun x->x.Datamodel_types.name) objs in
		List.mem tbl objs 
	in
	debug "Got db event: ty=%s op=%s reference=%s" ty op reference;
	if not (gen_events_for ty) then ()
	else
		begin	
			let ts = Unix.time () in
			let op = op_of_string op in

			debug "event_add - about to lock event_lock";
			Mutex.execute event_lock
				(fun () ->
					debug "event_add - locked";
					let ty = String.lowercase ty in
					let ev = { id = !id; ts; ty; op; reference; snapshot } in
					
					List.iter
						(fun (g,f) ->
							debug "Checking event_from belonging to session: %s" (Ref.string_of g.session);
							if event_from_matches_expr g f ty reference row || event_from_matches_bloom f reference 
							then (
								debug "It matches! About to lock g.m";
								Mutex.execute g.m (fun () -> 
									debug "Locked g.m - setting should_wait to false and waking other thread";
									f.should_wait <- false; Condition.broadcast f.cond)))
						!from_subscriptions;

					debug "Checking to see if any matches in event.next registrations";
					let matches_nexts = Hashtbl.fold
						(fun _ (g,n) acc ->
							acc || event_next_matches g ev) next_subscriptions false in

					debug "matches_nexts=%b" matches_nexts;

					if matches_nexts then begin
						queue := ev :: !queue;
						(* debug "Adding event %Ld: %s" (!id) (string_of_event ev); *)
						id := Int64.add !id Int64.one;
						Condition.broadcast newevents
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
	Db_cache_types.events_register event_add

(** Return the subscription associated with a session, or create a new blank one if none
	has yet been created. *)
let get_next_subscription ~__context =
	let session = Context.get_session_id __context in
	debug "get_next_subscription: getting event_lock";
	Mutex.execute event_lock
		(fun () ->
			debug "get_next_subscription: got event_lock";
			if Hashtbl.mem next_subscriptions session then Hashtbl.find next_subscriptions session
			else
				let next_subscription = { last_id = !id; } in
				let gen_subscription = { subs = []; m = Mutex.create(); session = session; session_invalid = false} in
				Hashtbl.replace next_subscriptions session (gen_subscription, next_subscription);
				(gen_subscription, next_subscription))

(** Raises an exception if the provided session has not already registered for some events *)
let assert_next_subscribed ~__context =
	let session = Context.get_session_id __context in
	debug "assert_next_subscribed: getting event_lock";
	Mutex.execute event_lock
	(fun () ->
		debug "assert_next_subscribed: got event_lock";
	   if not(Hashtbl.mem next_subscriptions session)
	   then raise (Api_errors.Server_error(Api_errors.session_not_registered, [ Context.trackid_of_session (Some session) ])))

(** Register an interest in events generated on objects of class <class_name> *)
let register ~__context ~classes =
	let subs = List.map subscription_of_string classes in
	let (sub,_) = get_next_subscription ~__context in
	Mutex.execute sub.m (fun () -> sub.subs <- subs @ sub.subs)


(** Unregister interest in events generated on objects of class <class_name> *)
let unregister ~__context ~classes =
	let subs = List.map subscription_of_string classes in
	let (sub,_) = get_next_subscription ~__context in
	Mutex.execute sub.m
		(fun () -> sub.subs <- List.filter (fun x -> not(List.mem x subs)) sub.subs)

let with_from_sub __context classes bloom timeout last_generation last_msg_gen f =
	let session = Context.get_session_id __context in
	let timeout = Unix.gettimeofday () +. timeout in
	debug "with_from_sub: getting event_lock";
	let my_from_id = Mutex.execute event_lock (fun () ->
		debug "with_from_sub: got event_lock";
		incr from_id;
		!from_id)
	in
	Pervasiveext.finally
		(fun () ->
			let subs = List.map subscription_of_string classes in
			let gen_subscription = { subs; m = Mutex.create(); session = session; session_invalid = false} in
			let from_subscription = {
				last_msg_gen;
				last_generation;
				should_wait=false;
				from_id=my_from_id;
				cond=Condition.create ();
				bloom;
				timeout; } in
			debug "with_from_sub2: getting event_lock";
			Mutex.execute event_lock
				(fun () ->
					debug "with_from_sub2: got event_lock";
					from_subscriptions := (gen_subscription,from_subscription):: !from_subscriptions);
			debug "Added to from_subscriptions";
			f (gen_subscription, from_subscription))
		(fun () ->
			debug "with_from_sub3: getting event_lock";
			Mutex.execute event_lock
				(fun () ->
					debug "with_from_sub3: got event_lock";
					from_subscriptions := List.filter (fun (_,f) -> f.from_id <> my_from_id) !from_subscriptions);
			debug "Removed from from_subscriptions")

(** Is called by the session timeout code *)
let on_session_deleted session_id = 
	debug "on_session_deleted: getting event_lock";
	Mutex.execute event_lock
	(fun () ->
		debug "on_session_deleted: got event_lock";
	   (* Unregister this session if is associated with in imported DB. *)
	   Db_backend.unregister_session session_id;
	   if Hashtbl.mem next_subscriptions session_id then begin
		 let (gen_sub,_) = Hashtbl.find next_subscriptions session_id in
		 (* Mark the subscription as invalid and wake everyone up *)
		 Mutex.execute gen_sub.m (fun () -> gen_sub.session_invalid <- true);
		 Hashtbl.remove next_subscriptions session_id;
		 Condition.broadcast newevents;
	   end;
	   List.iter (fun (gen_sub,from_sub) ->
		   if gen_sub.session = session_id then 
			   Mutex.execute gen_sub.m (fun () ->
				   gen_sub.session_invalid <- true;
				   Condition.broadcast from_sub.cond)) !from_subscriptions
	)

let session_is_invalid sub = Mutex.execute sub.m (fun () -> sub.session_invalid)

(** Blocks the caller until the current ID has changed OR the session has been
	invalidated. *)
let next_wait gen_sub from_id =
	let result = ref 0L in
	Mutex.execute event_lock
		(fun () ->
			(* NB we occasionally grab the specific session lock while holding the general lock *)
			while !id = from_id && not (session_is_invalid gen_sub) do 
				debug "Waiting in next_wait"; Condition.wait newevents event_lock done;
			
			result := !id);
	if session_is_invalid gen_sub
	then raise (Api_errors.Server_error(Api_errors.session_invalid, [ Ref.string_of gen_sub.session ]))
	else !result

let from_wait gen_sub from_sub =
	let timeoutname = Printf.sprintf "event_from_timeout_%s" (Ref.string_of gen_sub.session) in
	Mutex.execute gen_sub.m
		(fun () ->
			while from_sub.should_wait && not gen_sub.session_invalid && Unix.gettimeofday () < from_sub.timeout
			do
				Xapi_periodic_scheduler.add_to_queue timeoutname Xapi_periodic_scheduler.OneShot (from_sub.timeout -. Unix.gettimeofday () +. 0.5) (fun () -> Mutex.execute gen_sub.m (fun () -> Condition.broadcast from_sub.cond));
				debug "Waiting in event.from";
				Condition.wait from_sub.cond gen_sub.m;
				Xapi_periodic_scheduler.remove_from_queue timeoutname
			done;
		);
	if session_is_invalid gen_sub
	then raise (Api_errors.Server_error(Api_errors.session_invalid, [ Ref.string_of gen_sub.session ]))
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
	assert_next_subscribed ~__context;

	let gen_sub,next_sub = get_next_subscription ~__context in

	(* Return a <from_id, end_id> exclusive range that is guaranteed to be specific to this
	   thread. Concurrent calls will grab wholly disjoint ranges. Note the range might be
	   empty. *)
	let grab_range () =
		(* Briefly hold both the general and the specific mutex *)
	  	Mutex.execute event_lock
		  (fun () -> Mutex.execute gen_sub.m
			 (fun () ->
			let last_id = next_sub.last_id in
			(* Bump our last_id counter: these events don't have to be looked at again *)
			next_sub.last_id <- !id ;
			last_id, !id)) in
	(* Like grab_range () only guarantees to return a non-empty range by blocking if necessary *)
	let rec grab_nonempty_range () =
		let last_id, end_id = grab_range () in
		if last_id = end_id then begin
			let (_: int64) = next_wait gen_sub end_id in
			grab_nonempty_range ()
		end else last_id, end_id in

	let last_id, end_id = grab_nonempty_range () in
	(* debug "next examining events in range %Ld <= x < %Ld" last_id end_id; *)
	(* Are any of the new events interesting? *)
	let events = events_read last_id end_id in
	let relevant = List.filter (fun ev -> event_next_matches gen_sub ev) events in
	(* debug "number of relevant events = %d" (List.length relevant); *)
	if relevant = [] then next ~__context
	else XMLRPC.To.array (List.map xmlrpc_of_event relevant)

let all_event_tables =
	let objs = List.filter (fun x->x.Datamodel_types.gen_events) (Dm_api.objects_of_api Datamodel.all_api) in
	let objs = List.map (fun x->x.Datamodel_types.name) objs in
	objs

(* Some database fields have more underscores than the api fields, e.g. name__label.
   This function simply removes them and replaces them with single underscores *)
let remap_fields fields =
	List.map
		(fun field ->
			String.concat "_"
				(List.filter
					(fun x -> String.length x > 0)
					(String.split '_' field)
				)
		) fields

let from_real filter_fields ~__context ~classes ~token ~timeout =
	let last_generation, last_msg_gen, bloom =
		try
			Token.of_string token
		with e ->
			warn "Failed to parse event.from token: %s (%s)" token (Printexc.to_string e);
			raise (Api_errors.Server_error(Api_errors.event_from_token_parse_failure, [ token ])) in

	with_from_sub __context classes bloom timeout last_generation last_msg_gen (fun (gen_sub, from_sub) ->
		let tables = List.filter (fun table -> table_matches gen_sub.subs table) all_event_tables in

		let grab_range t =
			let tableset = Db_cache_types.Database.tableset (Db_lock.with_lock (fun () -> Db_ref.get_database t)) in
			let (msg_gen,messages) =
				if table_matches gen_sub.subs "message" then (!message_get_since_for_events) ~__context from_sub.last_msg_gen else (0L, []) in
			(msg_gen, messages, tableset, List.fold_left
				(fun acc table ->
					Db_cache_types.Table.fold_over_recent from_sub.last_generation
						(fun ctime mtime dtime objref row (creates,mods,deletes,last,regen_bloom) ->
							let matches_expr = event_from_matches_expr gen_sub from_sub table objref row in
							let matches_bloom = event_from_matches_bloom from_sub objref in
							debug "matches_expr=%b matches_bloom=%b" matches_expr matches_bloom;
							if matches_expr || matches_bloom then begin 
								let last = max last (max mtime dtime) in (* mtime guaranteed to always be larger than ctime *)
								if dtime > 0L then begin
									if ctime > from_sub.last_generation then
										(creates,mods,deletes,last,regen_bloom) (* It was created and destroyed since the last update *)
									else
										(creates,mods,(table, objref, dtime)::deletes,last,true) (* It might have been modified, but we can't tell now *)
								end else begin
									((if ctime > from_sub.last_generation then (table, objref, ctime)::creates else creates),
									(if mtime > from_sub.last_generation && ctime <= from_sub.last_generation
									then begin
										let relevant_fields = fieldset_of_sub gen_sub.subs table objref in
										
										let row = Opt.unbox row in
										let fields = Db_cache_types.Row.fold_over_recent from_sub.last_generation
											(fun ctime mtime dtime fieldname _ fields -> fieldname::fields) row [] in
										
										let fields = List.filter (fun x -> x <> "_ref") fields in
										let fields = remap_fields fields in
										
										let fields = List.intersect relevant_fields fields in
										if List.length fields > 0 || (not filter_fields) || List.length relevant_fields=0 then
											(table, objref, mtime, fields)::mods
										else
											mods
									end else mods),
									deletes, last, (
										let res=regen_bloom || (matches_expr && (not matches_bloom)) in
										debug "regen_bloom=%b matches_expr=%b matches_bloom=%b: compound=%b" regen_bloom matches_expr matches_bloom res;
										res))
								end
							end else (creates, mods, deletes, last, regen_bloom))
						(fun () -> ()) (Db_cache_types.TableSet.find table tableset) acc
				) ([],[],[],from_sub.last_generation,false) tables)
			in

			let rec grab_nonempty_range () =
				let (msg_gen, messages, tableset, (creates,mods,deletes,last,regen_bloom)) as result = grab_range (Db_backend.make ()) in
				if List.length creates = 0 && List.length mods = 0 && List.length deletes = 0 && List.length messages = 0 && Unix.gettimeofday () < from_sub.timeout
				then
					(
						from_sub.last_generation <- last; (* Cur_id was bumped, but nothing relevent fell out of the db. Therefore the *)
						from_sub.last_msg_gen <- msg_gen;

						Mutex.execute gen_sub.m (fun () -> from_sub.should_wait <- true); (* last id the client got is equivalent to the current one *)
						from_wait gen_sub from_sub;
						Thread.delay 0.05;
						grab_nonempty_range ())
				else
					result
			in

			let (msg_gen, messages, tableset, (creates,mods,deletes,last,regen_bloom)) = grab_nonempty_range () in

			from_sub.last_generation <- last;

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
				(event_of Del x :: acc)
			) [] deletes in

			let gen_snapshot serialiser objref fields =
				let xml = serialiser ~__context ~self:objref () in
				Opt.map (fun xml ->
					if filter_fields && List.length fields > 0
					then begin
						let str = XMLRPC.From.structure xml in
						let str = List.filter (fun (f,_) -> List.mem f fields) str in
						XMLRPC.To.structure str
					end else xml) xml
			in

			let modevs = List.fold_left (fun acc (table, objref, mtime, fields) ->
				let serialiser = Eventgen.find_get_record table in
				try
					(* It would be handy if the serialiser took a list of fields
					   to serialise. Instead, here we construct the XML tree then
					   filter out the irrelevant fields *)
					let xml = gen_snapshot serialiser objref fields in
					let ev = event_of Mod ?snapshot:xml (table, objref, mtime) in
					ev::acc
				with _ -> acc
			) delevs mods in

			let createevs = List.fold_left (fun acc (table, objref, ctime) ->
				let serialiser = Eventgen.find_get_record table in
				try
					let relevant_fields = fieldset_of_sub gen_sub.subs table objref in
					let xml = gen_snapshot serialiser objref relevant_fields in
					let ev = event_of Add ?snapshot:xml (table, objref, ctime) in
					ev::acc
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

			debug "regen_bloom=%b" regen_bloom;

			let bloom = 
				if not regen_bloom
				then from_sub.bloom
				else begin
					debug "Regenerating bloom filter";
					let db_size = List.fold_left (Int32.add) 0l (List.map snd valid_ref_counts) in
					let bloom = Bloom.create 7 ((Int32.to_int db_size) * 10) in
					Db_cache_types.TableSet.iter
						(fun tablename table ->
							Db_cache_types.Table.iter
								(fun r _ ->
									Bloom.add bloom r) table) tableset;
					bloom
				end
			in

			let result = {
				events = message_events;
				valid_ref_counts = valid_ref_counts;
				token = Token.to_string (last,msg_gen,bloom);
			} in

			xmlrpc_of_event_from result)

let from = from_real false
let fields_from = from_real true

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
	let token = Int64.sub generation 1L, 0L, Bloom.create 1 1 in
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
