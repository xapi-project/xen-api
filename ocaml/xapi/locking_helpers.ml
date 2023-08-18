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

module IntMap = Map.Make (Int)

module Thread_local_storage = struct
  module Thread_key : sig
    type t = private int

    val of_thread : Thread.t -> t

    val hash : t -> int

    val equal : t -> t -> bool
  end = struct
    type t = int

    let of_thread = Thread.id

    let hash x = x

    let equal = Int.equal
  end

  module LiveThreads = Hashtbl.Make (Thread_key)

  (* While a thread is alive we keep some per-thread data,
     after the thread dies the data will be GC-ed.
     Ephemerons would allocate some internal options on each lookup,
     so we cannot use them here. Instead we add a finaliser on the Thread.t.
  *)
  type 'a t = {lock: Mutex.t; tbl: 'a LiveThreads.t; init: unit -> 'a}

  let with_lock t f arg =
    Mutex.lock t.lock ;
    match f t arg with
    | result ->
        Mutex.unlock t.lock ; result
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        Mutex.unlock t.lock ;
        Printexc.raise_with_backtrace e bt

  let on_thread_gc t thread_id () =
    Mutex.lock t.lock ;
    LiveThreads.remove t.tbl thread_id ;
    Mutex.unlock t.lock

  let find_or_create_unlocked t self =
    (* try/with avoids allocation on fast-path *)
    let id = Thread_key.of_thread self in
    try LiveThreads.find t.tbl id
    with Not_found ->
      (* slow-path: first time use on current thread *)
      let v = t.init () in
      LiveThreads.replace t.tbl id v ;
      (* do not use a closure here, it might keep 'self' alive forver *)
      Gc.finalise_last (on_thread_gc t id) self ;
      v

  let get t =
    let self = Thread.self () in
    with_lock t find_or_create_unlocked self

  let make init : 'a t =
    let lock = Mutex.create () in
    let tbl = LiveThreads.create 47 in
    let t = {lock; tbl; init} in
    (* preallocate storage for current thread *)
    let (_ : 'a) = get t in
    t

  let set_unlocked t v =
    let self = Thread.self () in
    LiveThreads.replace t.tbl (Thread_key.of_thread self) v

  let _set t v = with_lock t set_unlocked v

  let snapshot_unlocked t () =
    LiveThreads.fold
      (fun thr v acc -> IntMap.add (thr :> int) v acc)
      t.tbl IntMap.empty

  let snapshot t = with_lock t snapshot_unlocked ()

  let count_unlocked t () = LiveThreads.length t.tbl

  let count t = with_lock t count_unlocked ()
end

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

(** Allow VMs to be locked to prevent API calls racing with the background event thread *)

module D = Debug.Make (struct let name = "locking_helpers" end)

open D

type resource_kind = No_resource | Lock of string | Process of string * int

let string_of_resource_kind = function
  | No_resource ->
      ""
  | Lock x ->
      Printf.sprintf "Lock(%s)" x
  | Process (name, pid) ->
      Printf.sprintf "Process(%s, %d)" name pid

let kill_resource = function
  | No_resource ->
      ()
  | Lock x ->
      debug "There is no way to forcibly remove Lock(%s)" x
  | Process (name, pid) ->
      info "Sending SIGKILL to %s pid %d" name pid ;
      Unix.kill pid Sys.sigkill

type resource = {
    kind: resource_kind
  ; str: string
  ; waiting_str: string
  ; acquired_str: string
}

let none = {kind= No_resource; str= ""; waiting_str= ""; acquired_str= ""}

let make kind =
  let str = string_of_resource_kind kind in
  let name state = String.concat "" ["Thread_state."; state; "("; str; ")"] in
  {kind; str; waiting_str= name "waiting_for"; acquired_str= "acquired"}

let lock name = make (Lock name)

let process (name, pid) = make (Process (name, pid))

let kill_resource r = kill_resource r.kind

let is_process name = function
  | {kind= Process (p, _); _} ->
      p = name
  | {kind= No_resource | Lock _; _} ->
      false

let string_of_resource r = r.str

module Thread_state = struct
  type waiting = (Tracing.Span.t option * Tracing.Span.t option) option

  type acquired = Tracing.Span.t option

  type time = float

  type t = {
      mutable acquired_resources: (resource * time) list
    ; mutable task: API.ref_task
    ; mutable name: string
    ; mutable waiting_for: resource
  }

  let make_empty () =
    {acquired_resources= []; task= Ref.null; name= ""; waiting_for= none}

  let thread_states = Thread_local_storage.make make_empty

  (* to be able to debug locking problems we need a consistent snapshot:
     if we're waiting for a lock, who's holding it currently and what locks are they holding or waiting for?
  *)

  let get_acquired_resources_by_task task =
    let snapshot = Thread_local_storage.snapshot thread_states in
    let all, _ = IntMap.partition (fun _ ts -> ts.task = task) snapshot in
    List.map fst
      (IntMap.fold (fun _ ts acc -> ts.acquired_resources @ acc) all [])

  let get_all_acquired_resources () =
    let snapshot = Thread_local_storage.snapshot thread_states in
    List.map fst
      (IntMap.fold (fun _ ts acc -> ts.acquired_resources @ acc) snapshot [])

  let get_states () = Thread_local_storage.get thread_states

  let with_named_thread name task f =
    let ts = get_states () in
    ts.name <- name ;
    ts.task <- task ;
    finally f (fun () ->
        let ts = get_states () in
        ts.name <- "" ;
        ts.task <- Ref.null
    )

  let now () = Unix.gettimeofday ()

  let waiting_for ?parent resource =
    let span =
      match (parent : Tracing.Span.t option) with
      | None ->
          None
      | Some _ -> (
          let name = resource.waiting_str in
          let tracer = Tracing.get_tracer ~name in
          match Tracing.Tracer.start ~tracer ~name ~parent () with
          | Ok span ->
              Some (parent, span)
          | Error e ->
              D.warn "Failed to start tracing: %s" (Printexc.to_string e) ;
              None
        )
    in
    let ts = get_states () in
    ts.waiting_for <- resource ;
    span

  let acquired resource parent =
    let span =
      match parent with
      | None ->
          None
      | Some (parent, span) -> (
          let (_ : (_, _) result) = Tracing.Tracer.finish span in
          let name = resource.acquired_str in
          let tracer = Tracing.get_tracer ~name in
          match Tracing.Tracer.start ~tracer ~name ~parent () with
          | Ok span ->
              span
          | Error e ->
              D.warn "Failed to start tracing: %s" (Printexc.to_string e) ;
              None
        )
    in
    let ts = get_states () in
    ts.waiting_for <- none ;
    ts.acquired_resources <- (resource, now ()) :: ts.acquired_resources ;
    span

  let released resource span =
    let (_ : (_, _) result) = Tracing.Tracer.finish span in
    let ts = get_states () in
    ts.acquired_resources <-
      List.filter (fun (r, _) -> r <> resource) ts.acquired_resources

  let to_graphviz () =
    let t' = now () in
    let snapshot = Thread_local_storage.snapshot thread_states in
    (* Map from thread ids -> record rows *)
    let threads =
      IntMap.map
        (fun ts ->
          [ts.name]
          :: [Ref.really_pretty_and_small ts.task]
          :: List.map
               (fun (r, t) ->
                 [string_of_resource r; Printf.sprintf "%.0f" (t' -. t)]
               )
               ts.acquired_resources
        )
        snapshot
    in
    let resources_of_ts ts =
      List.map fst ts.acquired_resources
      @ if ts.waiting_for.kind = No_resource then [] else [ts.waiting_for]
    in
    let all_resources =
      Xapi_stdext_std.Listext.List.setify
        (IntMap.fold (fun _ ts acc -> resources_of_ts ts @ acc) snapshot [])
    in
    let resources_to_ids =
      List.combine all_resources (List.init (List.length all_resources) Fun.id)
    in
    let resources_to_sll =
      List.filter_map
        (function
          | {kind= No_resource; _} ->
              None
          | {kind= Lock x; _} as y ->
              Some (y, [["lock"]; [x]])
          | {kind= Process (name, pid); _} as y ->
              Some (y, [["process"]; [name]; [string_of_int pid]])
          )
        all_resources
    in
    let resources_to_threads =
      IntMap.fold
        (fun id ts acc ->
          List.map
            (fun (r, _) -> (id, List.assoc r resources_to_ids))
            ts.acquired_resources
          @ acc
        )
        snapshot []
    in
    let threads_to_resources =
      IntMap.fold
        (fun id ts acc ->
          match ts.waiting_for with
          | {kind= No_resource; _} ->
              acc
          | r ->
              (id, List.assoc r resources_to_ids) :: acc
        )
        snapshot []
    in
    let label_of_sll sll =
      let bar = String.concat " | " in
      bar (List.map (fun sl -> "{" ^ bar sl ^ "}") sll)
    in
    let all =
      ["digraph Resources {"; "node [shape=Mrecord];"]
      @ IntMap.fold
          (fun id sll acc ->
            Printf.sprintf "t%d [label=\"%s\"];" id (label_of_sll sll) :: acc
          )
          threads []
      @ ["node [shape=record];"]
      @ List.map
          (fun (resource, id) ->
            Printf.sprintf "r%d [style=filled label=\"%s\"];" id
              (label_of_sll (List.assoc resource resources_to_sll))
          )
          resources_to_ids
      @ List.map
          (fun (t, r) -> Printf.sprintf "t%d -> r%d" t r)
          threads_to_resources
      @ List.map
          (fun (t, r) -> Printf.sprintf "r%d -> t%d" r t)
          resources_to_threads
      @ [
          "rankdir=LR"
        ; "overlap=false"
        ; "label=\"Threads and resources\""
        ; "fontsize=12"
        ; "}"
        ]
    in
    String.concat "\n" all

  let known_threads () = Thread_local_storage.count thread_states

  let with_resource resource acquire f release arg =
    let acquired = acquire resource arg in
    match f () with
    | r ->
        release resource acquired ; r
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        D.log_and_ignore_exn (fun () -> release resource acquired) ;
        Printexc.raise_with_backtrace e bt
end

module Named_mutex = struct
  type t = {
      name: string
    ; m: Mutex.t
    ; r: resource
    ; acquire: t -> Tracing.Span.t option -> Thread_state.acquired
    ; release: t -> Thread_state.acquired -> unit
  }

  let create name =
    let acquire t parent =
      let waiting = Thread_state.waiting_for ?parent t.r in
      Mutex.lock t.m ;
      Thread_state.acquired t.r waiting
    in
    let release t waiting =
      Mutex.unlock t.m ;
      Thread_state.released t.r waiting
    in
    {name; m= Mutex.create (); r= lock name; acquire; release}

  let execute ?__context ?parent (x : t) f =
    let parent =
      match parent with
      | None ->
          Option.bind __context Context.tracing_of
      | Some _ as p ->
          p
    in
    Thread_state.with_resource x x.acquire f x.release parent
end
