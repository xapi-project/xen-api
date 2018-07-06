(*
 * Copyright (C) Citrix Systems Inc.
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

(** A multiplexing xenstore protocol client over a byte-level transport *)

open Xs_protocol

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e
let with_mutex m f =
  Mutex.lock m;
  finally f (fun () -> Mutex.unlock m)
let find_opt h x =
  if Hashtbl.mem h x
  then Some (Hashtbl.find h x)
  else None

module type IO = sig
  type 'a t = 'a
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t

  type channel
  val create: unit -> channel t
  val destroy: channel -> unit t
  val read: channel -> bytes -> int -> int -> int t
  val write: channel -> bytes -> int -> int -> unit t
end

module StringSet = Xs_handle.StringSet

exception Watch_overflow

module Watcher = struct

  (** Someone who is watching paths is represented by one of these: *)
  type t = {
    mutable paths: StringSet.t; (* we never care about events or ordering, only paths *)
    mutable cancelling: bool; (* we need to stop watching and clean up *)
    c: Condition.t;
    m: Mutex.t;
  }

  let make () = {
    paths = StringSet.empty;
    cancelling = false;
    c = Condition.create ();
    m = Mutex.create ();
  }

  (** Register that a watched path has been changed *)
  let put (x: t) path =
    with_mutex x.m
      (fun () ->
        x.paths <- StringSet.add path x.paths;
        Condition.signal x.c
      )

  (** Return a set of modified paths, or an empty set if we're cancelling *)
  let get (x: t) =
    with_mutex x.m
      (fun () ->
        while x.paths = StringSet.empty && not x.cancelling do
          Condition.wait x.c x.m
        done;
        let results = x.paths in
        x.paths <- StringSet.empty;
        results
      )

  (** Called to shutdown the watcher and trigger an orderly cleanup *)
  let cancel (x: t) =
    with_mutex x.m
      (fun () ->
        x.cancelling <- true;
        Condition.signal x.c
      )
end

exception Malformed_watch_event
exception Unexpected_rid of int32
exception Dispatcher_failed

exception Cancelled

module Task = struct
  type 'a u = {
    mutable thing: 'a option;
    mutable cancelling: bool;
    mutable on_cancel: unit -> unit;
    m: Mutex.t;
    c: Condition.t
  }
  let make () = {
    thing = None;
    cancelling = false;
    on_cancel = (fun () -> ());
    m = Mutex.create ();
    c = Condition.create ();
  }
  let wakeup u thing = with_mutex u.m
    (fun () ->
      u.thing <- Some thing;
      Condition.signal u.c
    )
  let on_cancel u on_cancel = u.on_cancel <- on_cancel
  let cancel u = with_mutex u.m
    (fun () ->
      u.cancelling <- true;
      Condition.signal u.c
    );
    u.on_cancel ()
  let wait u = with_mutex u.m  (fun () ->
    let rec loop () =
      if u.cancelling then raise Cancelled
      else match u.thing with
      | None -> Condition.wait u.c u.m; loop ()
      | Some thing -> thing in
    loop ()
  )
end

type watch_callback = string * string -> unit

let auto_watch_prefix = "auto:"

let startswith prefix x =
  let prefix' = String.length prefix and x' = String.length x in
  x' >= prefix' && (String.sub x 0 prefix') = prefix

module Client = functor(IO: IO with type 'a t = 'a) -> struct
  module PS = PacketStream(IO)

  let logger = ref (fun s -> let _ : string = s in ())
  let error fmt = Printf.kprintf !logger fmt
  let set_logger f = logger := f

  (* Represents a single acive connection to a server *)
  type client = {
    transport: IO.channel;
    ps: PS.stream;
    rid_to_wakeup: (int32, Xs_protocol.t Task.u) Hashtbl.t;
    mutable dispatcher_thread: Thread.t option;
    mutable dispatcher_shutting_down: bool;

    mutable watch_callback_thread: Thread.t option;

    watchevents: (string, Watcher.t) Hashtbl.t;

    incoming_watches : (string * string) Queue.t;
    queue_overflowed : bool ref;
    incoming_watches_m : Mutex.t;
    incoming_watches_c : Condition.t;
      
    mutable extra_watch_callback: ((string * string) -> unit);
    m: Mutex.t;
  }

  type handle = client Xs_handle.t

  let recv_one t = match (PS.recv t.ps) with
    | Ok x -> x
    | Exception e -> raise e
  let send_one t = PS.send t.ps

  let handle_exn t e =
    error "Caught: %s\n%!" (Printexc.to_string e);
    begin match e with
      | Xs_protocol.Response_parser_failed _ ->
      (* Lwt_io.hexdump Lwt_io.stderr x *)
         ()
      | _ -> ()
    end;
    t.dispatcher_shutting_down <- true;
    raise e

  let enqueue_watch t event =
    with_mutex t.incoming_watches_m
      (fun () ->
	if Queue.length t.incoming_watches = 65536
	then t.queue_overflowed := true
	else Queue.push event t.incoming_watches;
	Condition.signal t.incoming_watches_c
      )

  let rec dispatcher t =
    let pkt = try recv_one t with e -> handle_exn t e in
    match get_ty pkt with
      | Op.Watchevent  ->
        begin match Unmarshal.list pkt with
          | Some [path; token] ->
            (* All 'extra' non-automatic watches are passed to the extra_watch_callback.
               Note this can include old watches which were still queued in
			   the server when an 'unwatch' is received. *)
            let w = with_mutex t.m (fun () -> find_opt t.watchevents token) in
            begin match w with
            | Some w -> Watcher.put w path
            | None -> if not(startswith auto_watch_prefix token) then enqueue_watch t (path, token)
            end;
          | _ ->
            handle_exn t Malformed_watch_event
        end;
        dispatcher t
      | _ ->
        let rid = get_rid pkt in
        let u = with_mutex t.m (fun () -> find_opt t.rid_to_wakeup rid) in
        begin match u with
        | Some u -> Task.wakeup u pkt
        | None -> error "Unexpected rid: %ld\n%!" rid
        end;
        dispatcher t

  let dequeue_watches t =
    while true do
      try 
	let event = with_mutex t.incoming_watches_m
	  (fun () ->
	    while Queue.is_empty t.incoming_watches && not(!(t.queue_overflowed)) do
	      Condition.wait t.incoming_watches_c t.incoming_watches_m
	    done;
	    if !(t.queue_overflowed) then begin
	      raise Watch_overflow;
	    end;
	    Queue.pop t.incoming_watches
	  ) in
	let () = t.extra_watch_callback event in
	()
      with 
	| Watch_overflow as e ->
	  error "Caught watch_overflow. Not retrying.";
	  raise e
	| e ->
	  error "Caught '%s' while dequeuing watches. Ignoring.\n%!" (Printexc.to_string e);
    done
      
	

  let make () =
    let transport = IO.create () in
    let t = {
      transport = transport;
      ps = PS.make transport;
      rid_to_wakeup = Hashtbl.create 10;
      dispatcher_thread = None;
      dispatcher_shutting_down = false;

      watch_callback_thread = None;

      watchevents = Hashtbl.create 10;

      incoming_watches = Queue.create ();
      queue_overflowed = ref false;
      incoming_watches_m = Mutex.create ();
      incoming_watches_c = Condition.create ();

      extra_watch_callback = (fun _ -> ());
      m = Mutex.create ();
    } in
    t.dispatcher_thread <- Some (Thread.create dispatcher t);
    t.watch_callback_thread <- Some (Thread.create dequeue_watches t);
    t

  let set_watch_callback client cb = client.extra_watch_callback <- cb

  let make_rid =
	  let counter = ref 0l in
	  let m = Mutex.create () in
	  fun () ->
		  with_mutex m
			  (fun () ->
				  let result = !counter in
				  counter := Int32.succ !counter;
				  result
			  )

  let rpc hint h payload unmarshal =
    let open Xs_handle in
    let rid = make_rid () in
    let request = Request.print payload (get_tid h) rid in
    let t = Task.make () in
    let c = get_client h in
    if c.dispatcher_shutting_down
    then raise Dispatcher_failed
    else begin
        with_mutex c.m (fun () -> Hashtbl.add c.rid_to_wakeup rid t);
        send_one c request;
        let res = Task.wait t in
        with_mutex c.m (fun () -> Hashtbl.remove c.rid_to_wakeup rid);
        response hint request res unmarshal
    end

  let directory h path = rpc "directory" (Xs_handle.accessed_path h path) Request.(PathOp(path, Directory)) Unmarshal.list
  let read h path = rpc "read" (Xs_handle.accessed_path h path) Request.(PathOp(path, Read)) Unmarshal.string
  let write h path data = rpc "write" (Xs_handle.accessed_path h path) Request.(PathOp(path, Write data)) Unmarshal.ok
  let rm h path = rpc "rm" (Xs_handle.accessed_path h path) Request.(PathOp(path, Rm)) Unmarshal.ok
  let mkdir h path = rpc "mkdir" (Xs_handle.accessed_path h path) Request.(PathOp(path, Mkdir)) Unmarshal.ok
  let setperms h path acl = rpc "setperms" (Xs_handle.accessed_path h path) Request.(PathOp(path, Setperms acl)) Unmarshal.ok
  let debug h cmd_args = rpc "debug" h (Request.Debug cmd_args) Unmarshal.list
  let restrict h domid = rpc "restrict" h (Request.Restrict domid) Unmarshal.ok
  let getdomainpath h domid = rpc "getdomainpath" h (Request.Getdomainpath domid) Unmarshal.string
  let watch h path token = rpc "watch" (Xs_handle.watch h path) (Request.Watch(path, token)) Unmarshal.ok
  let unwatch h path token = rpc "unwatch" (Xs_handle.unwatch h path) (Request.Unwatch(path, token)) Unmarshal.ok
  let introduce h domid store_mfn store_port = rpc "introduce" h (Request.Introduce(domid, store_mfn, store_port)) Unmarshal.ok
  let set_target h stubdom_domid domid = rpc "set_target" h (Request.Set_target(stubdom_domid, domid)) Unmarshal.ok
  let immediate client f = f (Xs_handle.no_transaction client)

  let counter = ref 0l

  let wait client f =
    let open StringSet in
    counter := Int32.succ !counter;
    let token = Printf.sprintf "%s%ld" auto_watch_prefix !counter in
    (* When we register the 'watcher', the dispatcher thread will signal us when
       watches arrive. *)
    let watcher = Watcher.make () in
    with_mutex client.m (fun () -> Hashtbl.add client.watchevents token watcher);

    (* We signal the caller via this cancellable task: *)
    let t = Task.make () in
    Task.on_cancel t
      (fun () ->
        (* Trigger an orderly cleanup in the background: *)
        Watcher.cancel watcher
      );
    let h = Xs_handle.watching client in
    (* Adjust the paths we're watching (if necessary) and block (if possible) *)
    let adjust_paths () =
      let current_paths = Xs_handle.get_watched_paths h in
      (* Paths which weren't read don't need to be watched: *)
      let old_paths = diff current_paths (Xs_handle.get_accessed_paths h) in
      List.iter (fun p -> unwatch h p token) (elements old_paths);
      (* Paths which were read do need to be watched: *)
      let new_paths = diff (Xs_handle.get_accessed_paths h) current_paths in
      List.iter (fun p -> watch h p token) (elements new_paths);
      (* If we're watching the correct set of paths already then just block *)
      if old_paths = empty && (new_paths = empty)
      then begin
        let results = Watcher.get watcher in
        (* an empty results set means we've been cancelled: trigger cleanup *)
        if results = empty
        then raise (Failure "goodnight")
      end in
    (* Main client loop: *)
    let rec loop () =
      let finished =
        try
          let result = f h in
          Task.wakeup t result;
          true
        with Eagain ->
          false in
      if not finished then begin
        adjust_paths ();
        loop ()
      end
    in
    let (_: Thread.t) =
      Thread.create (fun () ->
        finally loop (fun () ->
          let current_paths = Xs_handle.get_watched_paths h in
          List.iter (fun p -> unwatch h p token) (elements current_paths);
          with_mutex client.m (fun () -> Hashtbl.remove client.watchevents token);
        )
      ) ()
    in
    t

  let _transaction_leave_open client f =
    let tid = rpc "transaction_start" (Xs_handle.no_transaction client) Request.Transaction_start Unmarshal.int32 in
    let h = Xs_handle.transaction client tid in
    let result = f h in
    (h, result)

  let _commit h result =
    let res' = rpc "transaction_end" h (Request.Transaction_end true) Unmarshal.string in
    if res' = "OK" then result else raise (Error (Printf.sprintf "Unexpected transaction result: %s" res'))

  let transaction_one_try client f =
    let (h, result) = _transaction_leave_open client f in
    _commit h result

  let rec transaction_attempts attempts client f =
    let (h, result) = _transaction_leave_open client f in
    try _commit h result
    with Eagain when (attempts > 1) ->
      transaction_attempts (attempts-1) client f

  (** Deprecated: retries for ever on repeated Eagain *)
  let rec transaction client f =
    let (h, result) = _transaction_leave_open client f in
    try _commit h result
    with Eagain -> transaction client f

end

