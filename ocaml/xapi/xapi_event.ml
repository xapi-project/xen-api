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

(** When we GC events we track how many we've deleted so we can send an error to the client *)
let highest_forgotten_id = ref (-1L)

(** Types used to store user event subscriptions: ***********************************************)
type subscription = 
    | Class of string (** subscribe to all events for objects of this class *)
    | All             (** subscribe to everything *)

let subscription_of_string x = if x = "*" then All else Class x

let event_matches subs ty = List.mem All subs || (List.mem (Class ty) subs)

(** Every session that calls 'register' gets a subscription*)
type subscription_record = {
	mutable last_id: int64;           (** last event ID (generation count) to sent to this client *)
	mutable cur_id: int64;            (** Most current generation count relevant to the client *)
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

(* Called with the db_lock held *)
let event_add ?snapshot ty op reference  =
  let gen_events_for tbl =
    let objs = List.filter (fun x->x.Datamodel_types.gen_events) (Dm_api.objects_of_api Datamodel.all_api) in
    let objs = List.map (fun x->x.Datamodel_types.name) objs in
    List.mem tbl objs in
  if not (gen_events_for ty) then ()
  else
    Mutex.execute event_lock
      (fun () ->
        Hashtbl.iter (fun _ s -> 
          if event_matches s.subs ty then begin
            s.cur_id <- Db_cache_types.get_current_event_number ();
            Condition.broadcast newevents 
          end) subscriptions)

let register_hooks () =
	Db_action_helper.events_register event_add

let get_current_id ~__context = Db_lock.with_lock Db_cache_types.get_current_event_number

(** Return the subscription associated with a session, or create a new blank one if none
    has yet been created. *)
let get_subscription ~__context = 
	let session = Context.get_session_id __context in
	let cur_id = get_current_id ~__context in
	Mutex.execute event_lock
	(fun () ->
	   if Hashtbl.mem subscriptions session then Hashtbl.find subscriptions session
	   else 
	     let subscription = { last_id = cur_id; cur_id = cur_id;
	       subs = []; m = Mutex.create(); session = session; session_invalid = false } in
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
let wait_until_id_reached subscription from_id =
  Mutex.execute event_lock
    (fun () ->
      while from_id = subscription.cur_id && not (session_is_invalid subscription) do Condition.wait newevents event_lock done;
    );
  if session_is_invalid subscription
  then raise (Api_errors.Server_error(Api_errors.session_invalid, [ Ref.string_of subscription.session ]))
  else ()

let rec next ~__context =
  assert_subscribed ~__context;

  let sub = get_subscription ~__context in

  let all_event_tables =
    let objs = List.filter (fun x->x.Datamodel_types.gen_events) (Dm_api.objects_of_api Datamodel.all_api) in
    let objs = List.map (fun x->x.Datamodel_types.name) objs in
    objs
  in

  let all_subs = Mutex.execute sub.m (fun () -> Hashtbl.fold (fun _ s acc -> s.subs @ acc) subscriptions []) in
  let tables = List.filter (fun table -> event_matches all_subs table) all_event_tables in

  let grab_range () = 
    List.fold_left (fun acc table ->
      try
        Db_cache_types.fold_over_recent_rows 
          (fun ctime mtime dtime objref (creates,mods,deletes,last) ->
            debug "Event.next: found event with ctime: %Ld mtime:%Ld dtime:%Ld objref:%s" ctime mtime dtime objref;
            let last = max last (max mtime dtime) in (* mtime guaranteed to always be larger than ctime *)
            if dtime > 0L then begin
              if ctime > sub.last_id then 
                (creates,mods,deletes,last) (* It was created and destroyed since the last update *)
              else
                (creates,mods,(table, objref, dtime)::deletes,last) (* It might have been modified, but we can't tell now *)
            end else begin
              ((if ctime > sub.last_id then (table, objref, ctime)::creates else creates),
              (if mtime > sub.last_id then (table, objref, mtime)::mods else mods),
              deletes, last)
            end
          ) (Db_cache_types.lookup_table_in_cache Db_backend.cache table) sub.last_id acc
      with Db_cache_types.Too_many_deletion_events -> events_lost()
    ) ([],[],[],sub.last_id) tables
  in
  
  let rec grab_nonempty_range () =
    let (creates,mods,deletes,last) as result = Db_lock.with_lock grab_range in
    if List.length creates = 0 && List.length mods = 0 && List.length deletes = 0 
    then 
      (
        sub.last_id <- last; (* Cur_id was bumped, but nothing relevent fell out of the db. Therefore the *)
        sub.cur_id <- last; (*                   last id the client got is equivalent to the current one. *)
        wait_until_id_reached sub last;
        (* Wait for an imperceptibly small time just in case there are other
         * events happening cotemporaneously that we can bundle up together. *)
        Thread.delay 0.05;
        grab_nonempty_range ())
    else
      result
  in

  let creates,mods,deletes,last = grab_nonempty_range () in

  sub.last_id <- last;

  let delevs = List.fold_left (fun acc (table, objref, dtime) ->
    if event_matches sub.subs table then begin
      let ev = {id=dtime;
        ts=0.0;
        ty=String.lowercase table;
        op=Del;
        reference=objref;
        snapshot=None} in
      ev::acc
    end else acc
  ) [] deletes in

  let modevs = List.fold_left (fun acc (table, objref, mtime) ->
    if event_matches sub.subs table then begin
      let serialiser = Eventgen.find_get_record table in
      let xml = serialiser ~__context ~self:objref () in
      let ev = { id=mtime;
        ts=0.0;
        ty=String.lowercase table;
        op=Mod;
        reference=objref;
        snapshot=Some xml } in
      ev::acc
    end else acc
  ) delevs mods in
  
  let createevs = List.fold_left (fun acc (table, objref, ctime) ->
    if event_matches sub.subs table then begin
      let serialiser = Eventgen.find_get_record table in
      let xml = serialiser ~__context ~self:objref () in
      let ev = { id=ctime;
        ts=0.0;
        ty=String.lowercase table;
        op=Add;
        reference=objref;
        snapshot=Some xml } in
      ev::acc
    end else acc
  ) modevs creates in
  
  XMLRPC.To.array (List.map xmlrpc_of_event createevs)

(** Inject an unnecessary update as a heartbeat. This will:
    1. hopefully prevent some firewalls from silently closing the connection
    2. allow the server to detect when a client has failed *)
let heartbeat ~__context =
  try
    Db_lock.with_lock 
      (fun () ->
	 (* We must hold the database lock since we are sending an update for a real object
	    and we don't want to accidentally transmit an older snapshot. *)
	 let pool = Helpers.get_pool ~__context in
	 let pool_r = Db.Pool.get_record ~__context ~self:pool in
	 let pool_xml = API.To.pool_t pool_r in
	 event_add ~snapshot:pool_xml "pool" "mod" (Ref.string_of pool)
      )
  with e ->
    error "Caught exception sending event heartbeat: %s" (ExnHelper.string_of_exn e)
