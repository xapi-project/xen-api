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

module D=Debug.Debugger(struct let name="xapi_event" end)
open D


(** Limit the event queue to this many events: *)
let max_stored_events = 500
(** Limit the maximum age of an event in the event queue to this value in seconds: *)
let max_event_age = 15. *. 60. (* 15 minutes *)

(** Ordered list of events, newest first *)
let queue = ref []
(** Monotonically increasing event ID. One higher than the highest event ID in the queue *)
let id = ref 0L 
(** When we GC events we track how many we've deleted so we can send an error to the client *)
let highest_forgotten_id = ref (-1L)

(** Types used to store user event subscriptions: ***********************************************)
type subscription = 
    | Class of string (** subscribe to all events for objects of this class *)
    | All             (** subscribe to everything *)

let subscription_of_string x = if x = "*" then All else Class x

let event_matches subs ev = List.mem All subs || (List.mem (Class ev.ty) subs)

(** Every session that calls 'register' gets a subscription*)
type subscription_record = {
	mutable last_id: int64;           (** last event ID to sent to this client *)
	mutable subs: subscription list;  (** list of all the subscriptions *)
	m: Mutex.t;                       (** protects access to the mutable fields in this record *)
                                          (** can be called with the event_lock also held *)
	session: API.ref_session;         (** session which owns this subscription *)
	mutable session_invalid: bool;    (** set to true if the associated session has been deleted *)
}


(** Thrown if the user requests events which we don't have because we've thrown
    then away. This should only happen if the client (or network) becomes unresponsive
    for the max_event_age interval or if more than max_stored_events are produced 
    between successive calls to Event.next (). The client should refresh all its state
    manually before calling Event.next () again.
*)
let events_lost () = raise (Api_errors.Server_error (Api_errors.events_lost, []))

(* Mapping of session IDs to lists of subscribed classes *)
let subscriptions = Hashtbl.create 10

(* Lock protects the global event queue reference and the subscriptions hashtable *)
let event_lock = Mutex.create ()
let newevents = Condition.create ()

(** This function takes a set of events (requested by the client) and removes
    redundancies (like multiple Mods of the same object or Mod of a deleted object).
    NB we cannot safely remove object Deletes because a client might receive the Add event
    if one call and a Del event in a subsequent call.
    NB events are stored in reverse order. *)
let coalesce_events events = 
	(* We're not trying to be super-efficient here, we scan the list multiple times. 
	   Let's keep the queue short. *)
	let refs_of_op op events = List.concat 
		(List.map (function { op = op'; reference = reference } -> 
			     if op = op' then [ reference ] else []) events) in

	let dummy x = { x with op = Dummy } in

	(* If an object has been deleted, remove any Modification events *)
	let all_dead = refs_of_op Del events in
	let events' = List.map (function { op = Mod; reference = reference } as x ->
				 if List.mem reference all_dead 
				 then dummy x else x
				| x -> x) events in

	(* If one Mod event has been seen, remove the rest (we keep the latest (ie 
	   the first one in the list) since the list is reversed)) *)
	let events' = List.rev (snd (List.fold_left 
	  (fun (already_seen, acc) x -> 
	     if x.op = Mod then
	       let x = if List.mem x.reference already_seen then dummy x else x in
	       x.reference :: already_seen, x :: acc
	     else already_seen, x :: acc) ([], []) events')) in

	(* For debugging we may wish to keep the dummy events so we can account for 
	   every event ID. However normally we want to zap them. *)
	let events' = List.filter (fun ev -> ev.op <> Dummy) events' in
	(* debug "Removed %d redundant events" (List.length events - (List.length events')); *)
	events'

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

		let all_subs = Hashtbl.fold (fun _ s acc -> s.subs @ acc) subscriptions [] in
		if event_matches all_subs ev then begin
			queue := ev :: !queue;
			(* debug "Adding event %Ld: %s" (!id) (string_of_event ev); *)
			id := Int64.add !id Int64.one;
			Condition.broadcast newevents;
		end else begin
			(* debug "Dropping event %s" (string_of_event ev) *)
		end;
		
		(* Remove redundant events from the queue *)
		(* queue := coalesce_events !queue;*)
		
		(* GC the events in the queue *)
		let young, old = List.partition (fun ev -> ts -. ev.ts <= max_event_age) !queue in
		let too_many = List.length young - max_stored_events in
		let to_keep, to_drop = if too_many <= 0 then young, old
		  else
		    (* Reverse-sort by ID and preserve the first 'max_stored_events' *)
		    let lucky, unlucky = 
		      List.chop max_stored_events (List.sort (fun a b -> compare b.id a.id) young) in
		    lucky, old @ unlucky in
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
	     let subscription = { last_id = !id; subs = []; m = Mutex.create(); session = session; session_invalid = false } in
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
	let subs = List.map subscription_of_string (List.map String.lowercase classes) in
	let sub = get_subscription ~__context in
	Mutex.execute sub.m (fun () -> sub.subs <- subs @ sub.subs)


(** Unregister interest in events generated on objects of class <class_name> *)
let unregister ~__context ~classes = 
	let subs = List.map subscription_of_string (List.map String.lowercase classes) in
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
			wait subscription end_id; 
			grab_nonempty_range ()
		end else last_id, end_id in

	let last_id, end_id = grab_nonempty_range () in
	(* debug "next examining events in range %Ld <= x < %Ld" last_id end_id; *)
	(* Are any of the new events interesting? *)
	let events = events_read last_id end_id in
	let subs = Mutex.execute subscription.m (fun () -> subscription.subs) in
	let relevant = List.filter (event_matches subs) events in
	(* debug "number of relevant events = %d" (List.length relevant); *)
	if relevant = [] then next ~__context 
	else XMLRPC.To.array (List.map xmlrpc_of_event relevant)

let get_current_id ~__context = Mutex.execute event_lock (fun () -> !id)

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
