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
let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

(** Allow VMs to be locked to prevent API calls racing with the background event thread *)

module D = Debug.Make (struct let name = "locking_helpers" end)

open D

type resource = Lock of string | Process of string * int

let string_of_resource = function
  | Lock x ->
      Printf.sprintf "Lock(%s)" x
  | Process (name, pid) ->
      Printf.sprintf "Process(%s, %d)" name pid

let kill_resource = function
  | Lock x ->
      debug "There is no way to forcibly remove Lock(%s)" x
  | Process (name, pid) ->
      info "Sending SIGKILL to %s pid %d" name pid ;
      Unix.kill pid Sys.sigkill

module Thread_state = struct
  type time = float

  type t = {
      acquired_resources: (resource * time) list
    ; task: API.ref_task
    ; name: string
    ; waiting_for: (resource * time) option
  }

  let empty =
    {acquired_resources= []; task= Ref.null; name= ""; waiting_for= None}

  let m = Mutex.create ()

  module IntMap = Map.Make (struct
    type t = int

    let compare = compare
  end)

  let thread_states = ref IntMap.empty

  let get_acquired_resources_by_task task =
    let snapshot = with_lock m (fun () -> !thread_states) in
    let all, _ = IntMap.partition (fun _ ts -> ts.task = task) snapshot in
    List.map fst
      (IntMap.fold (fun _ ts acc -> ts.acquired_resources @ acc) all [])

  let get_all_acquired_resources () =
    let snapshot = with_lock m (fun () -> !thread_states) in
    List.map fst
      (IntMap.fold (fun _ ts acc -> ts.acquired_resources @ acc) snapshot [])

  let me () = Thread.id (Thread.self ())

  let update f =
    let id = me () in
    let snapshot = with_lock m (fun () -> !thread_states) in
    let ts =
      if IntMap.mem id snapshot then
        f (IntMap.find id snapshot)
      else
        f empty
    in
    with_lock m (fun () ->
        thread_states :=
          if ts = empty then
            IntMap.remove id !thread_states
          else
            IntMap.add id ts !thread_states
    )

  let with_named_thread name task f =
    update (fun ts -> {ts with name; task}) ;
    finally f (fun () -> update (fun ts -> {ts with name= ""; task= Ref.null}))

  let now () = Unix.gettimeofday ()

  let waiting_for resource =
    update (fun ts -> {ts with waiting_for= Some (resource, now ())})

  let acquired resource =
    update (fun ts ->
        {
          ts with
          waiting_for= None
        ; acquired_resources= (resource, now ()) :: ts.acquired_resources
        }
    )

  let released resource =
    update (fun ts ->
        {
          ts with
          acquired_resources=
            List.filter (fun (r, _) -> r <> resource) ts.acquired_resources
        }
    )

  let to_graphviz () =
    let t' = now () in
    let snapshot = with_lock m (fun () -> !thread_states) in
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
      @ Option.fold ~none:[] ~some:(fun (r, _) -> [r]) ts.waiting_for
    in
    let all_resources =
      Xapi_stdext_std.Listext.List.setify
        (IntMap.fold (fun _ ts acc -> resources_of_ts ts @ acc) snapshot [])
    in
    let resources_to_ids =
      List.combine all_resources (List.init (List.length all_resources) Fun.id)
    in
    let resources_to_sll =
      List.map
        (function
          | Lock x as y ->
              (y, [["lock"]; [x]])
          | Process (name, pid) as y ->
              (y, [["process"]; [name]; [string_of_int pid]])
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
          | None ->
              acc
          | Some (r, _) ->
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
end

module Named_mutex = struct
  type t = {name: string; m: Mutex.t}

  let create name = {name; m= Mutex.create ()}

  let execute (x : t) f =
    let r = Lock x.name in
    Thread_state.waiting_for r ;
    with_lock x.m (fun () ->
        Thread_state.acquired r ;
        finally f (fun () -> Thread_state.released r)
    )
end
