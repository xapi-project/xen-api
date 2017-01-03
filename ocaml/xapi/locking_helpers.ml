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
open Stdext
open Threadext
open Pervasiveext
open Listext

(** Allow VMs to be locked to prevent API calls racing with the background event thread *)

module D = Debug.Make(struct let name = "locking_helpers" end)
open D

type resource =
  | Lock of string
  | Process of string * int

let string_of_resource = function
  | Lock x -> Printf.sprintf "Lock(%s)" x
  | Process (name, pid) -> Printf.sprintf "Process(%s, %d)" name pid

let kill_resource = function
  | Lock x -> debug "There is no way to forcibly remove Lock(%s)" x
  | Process (name, pid) ->
    info "Sending SIGKILL to %s pid %d" name pid;
    Unix.kill pid Sys.sigkill

module Thread_state = struct
  type time = float
  type t = {
    acquired_resources: (resource * time) list;
    task: API.ref_task;
    name: string;
    waiting_for: (resource * time) option;
  }
  let empty = {
    acquired_resources = [];
    task = Ref.null;
    name = "";
    waiting_for = None;
  }
  let m = Mutex.create ()
  module IntMap = Map.Make(struct type t = int let compare = compare end)
  let thread_states = ref IntMap.empty

  let get_acquired_resources_by_task task =
    let snapshot = Mutex.execute m (fun () -> !thread_states) in
    let all, _ = IntMap.partition (fun _ ts -> ts.task = task) snapshot in
    List.map fst (IntMap.fold (fun _ ts acc -> ts.acquired_resources @ acc) all [])

  let get_all_acquired_resources () =
    let snapshot = Mutex.execute m (fun () -> !thread_states) in
    List.map fst (IntMap.fold (fun _ ts acc -> ts.acquired_resources @ acc) snapshot [])

  let me () = Thread.id (Thread.self ())

  let update f =
    let id = me () in
    let snapshot = Mutex.execute m (fun () -> !thread_states) in
    let ts =
      if IntMap.mem id snapshot
      then f (IntMap.find id snapshot)
      else f empty in
    Mutex.execute m
      (fun () ->
         thread_states := if ts = empty then IntMap.remove id !thread_states else IntMap.add id ts !thread_states
      )

  let with_named_thread name task f =
    update (fun ts -> { ts with name = name; task = task });
    finally f
      (fun () -> update (fun ts -> { ts with name = ""; task = Ref.null }))

  let now () = Unix.gettimeofday ()
  let waiting_for resource =
    update (fun ts -> { ts with waiting_for = Some (resource, now()) })

  let acquired resource =
    update (fun ts -> { ts with waiting_for = None; acquired_resources = (resource, now()) :: ts.acquired_resources })

  let released resource =
    update (fun ts -> { ts with acquired_resources = List.filter (fun (r,_) -> r <> resource) ts.acquired_resources })

  let to_graphviz () =
    let t' = now () in
    let snapshot = Mutex.execute m (fun () -> !thread_states) in
    (* Map from thread ids -> record rows *)
    let threads = IntMap.map (fun ts ->
        [ ts.name ]::[ Ref.really_pretty_and_small ts.task ]::(List.map (fun (r, t) -> [ string_of_resource r; Printf.sprintf "%.0f" (t' -. t) ]) ts.acquired_resources)
      ) snapshot in
    let resources_of_ts ts =
      List.map fst ts.acquired_resources @
      (Opt.default [] (Opt.map (fun (r, _) -> [ r ]) ts.waiting_for)) in
    let all_resources =
      List.setify
        (IntMap.fold (fun _ ts acc -> resources_of_ts ts @ acc ) snapshot [] ) in

    let resources_to_ids = List.combine all_resources (Range.to_list (Range.make 0 (List.length all_resources))) in
    let resources_to_sll = List.map
        (function
          | Lock x as y -> y, [ [ "lock" ]; [ x ] ]
          | Process (name, pid) as y -> y, [ [ "process" ]; [ name ]; [ string_of_int pid ] ]
        ) all_resources in

    let resources_to_threads =
      IntMap.fold (fun id ts acc ->
          List.map (fun (r, _) -> id, List.assoc r resources_to_ids) ts.acquired_resources @ acc
        ) snapshot [] in
    let threads_to_resources =
      IntMap.fold (fun id ts acc ->
          match ts.waiting_for with
          | None -> acc
          | Some (r,  _) -> (id, List.assoc r resources_to_ids) :: acc
        ) snapshot [] in
    let label_of_sll sll =
      let bar = String.concat " | " in
      bar (List.map (fun sl -> "{" ^ (bar sl) ^ "}") sll) in
    let all = [
      "digraph Resources {";
      "node [shape=Mrecord];"
    ] @ (IntMap.fold (fun id sll acc -> Printf.sprintf "t%d [label=\"%s\"];" id (label_of_sll sll) :: acc) threads []) @ [
        "node [shape=record];"
      ] @ (List.map (fun (resource, id) -> Printf.sprintf "r%d [style=filled label=\"%s\"];" id (label_of_sll (List.assoc resource resources_to_sll))) resources_to_ids) @
      (List.map (fun (t, r) -> Printf.sprintf "t%d -> r%d" t r) threads_to_resources) @
      (List.map (fun (t, r) -> Printf.sprintf "r%d -> t%d" r t) resources_to_threads) @ [
        "rankdir=LR";
        "overlap=false";
        "label=\"Threads and resources\"";
        "fontsize=12";
        "}";
      ] in
    String.concat "\n" all
end

module Named_mutex = struct
  type t = {
    name: string;
    m: Mutex.t;
  }
  let create name = {
    name = name;
    m = Mutex.create ();
  }
  let execute (x:t) f =
    let r = Lock x.name in
    Thread_state.waiting_for r;
    Mutex.execute x.m
      (fun () ->
         Thread_state.acquired r;
         finally
           f
           (fun () -> Thread_state.released r)
      )
end

(* We store the locks in a hashtable whose keys are VM references and values are abstract
   locking tokens. We can check we are the current lock owner by calling "assert_locked" with
   the token -- this helps us avoid screwups like the following:
      let f () () = ... do something which needs a lock ...
      let result = with_lock f () in
      (* result is a higher order function *)
      result () (* oops: side-effect happens here outside the lock *)
*)

let mutex = Mutex.create ()
let cvar = Condition.create ()
type token = int64 (* abstract *)

let locks : (API.ref_VM, token) Hashtbl.t = Hashtbl.create 10

exception Lock_not_held (** Raised by assert_locked if we screw up the use of 'with_lock' *)

let assert_locked (target: API.ref_VM) (token: token) =
  let held =
    Mutex.execute mutex
      (fun () -> Hashtbl.fold (fun target' token' acc -> acc || ((target = target') && (token = token'))) locks false) in
  if not held then begin
    error "VM lock not held for VM %s" (Ref.string_of target);
    raise Lock_not_held
  end


let next_token = ref 0L

let lock_of_vm target = Lock ("VM/" ^ (Ref.really_pretty_and_small target))

let acquire_lock (target: API.ref_VM)  =
  let already_locked () = Hashtbl.mem locks target in

  Thread_state.waiting_for (lock_of_vm target);

  let token = Mutex.execute mutex
      (fun () ->
         while already_locked () do Condition.wait cvar mutex done;
         let token = !next_token in
         next_token := Int64.add !next_token 1L;
         Hashtbl.replace locks target token;
         token
      ) in
  debug "Acquired lock on VM %s with token %Ld" (Ref.string_of target) token;
  Thread_state.acquired (lock_of_vm target);
  token

(* Two internal errors which should never happen by construction *)
exception Internal_error_vm_not_locked
exception Internal_error_token_mismatch

let release_lock (target: API.ref_VM) (token: token) =
  Mutex.execute mutex
    (fun () ->
       if not(Hashtbl.mem locks target) then begin
         error "VM %s not locked at all: lock cannot be released" (Ref.string_of target);
         raise Internal_error_vm_not_locked
       end;
       let token' = Hashtbl.find locks target in
       if token <> token' then begin
         error "VM %s locked with token %Ld: cannot be unlocked by token %Ld" (Ref.string_of target) token' token;
         raise Internal_error_token_mismatch
       end;
       Hashtbl.remove locks target;
       Condition.broadcast cvar
    );
  Thread_state.released (lock_of_vm target);
  debug "Released lock on VM %s with token %Ld" (Ref.string_of target) token

