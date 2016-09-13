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
(** Queues of jobs to perform, represented as unit -> unit thunks *)

open Stdext
open Threadext

let vm_lifecycle_queue_started = ref false
let m = Mutex.create ()
let c = Condition.create ()


let vm_lifecycle_queue_process_fn f =
  Mutex.execute m (fun () -> while not !vm_lifecycle_queue_started do Condition.wait c m done);
  f ()

let start_vm_lifecycle_queue () =
  Mutex.execute m (fun () -> vm_lifecycle_queue_started := true; Condition.signal c)

(* NB VM.start for PV guests performs VBD.unplug operations which require the dom0 device resync ops
   to be decoupled from the rest. *)

(** Put "long running/streaming operations" into their own queue, so vm lifecycle ops can be parallelized with them *)
let long_running_queue = Thread_queue.make ~name:"long_running_op" vm_lifecycle_queue_process_fn

(** VM.{start,shutdown,copy,clone} etc are queued here *)
let normal_vm_queue = Thread_queue.make ~name:"vm_lifecycle_op" vm_lifecycle_queue_process_fn

(** Resynchronising dom0 VBDs and VIFs are handled here. *)
let dom0_device_resync_queue = Thread_queue.make ~name:"dom0_device_resync" (fun f -> f ())

(** Internal reboots and shutdowns are queued here *)
let domU_internal_shutdown_queue = Thread_queue.make ~name:"domU_internal_shutdown" (fun f -> f())

open Pervasiveext

(** Join a given queue and execute the function 'f' when its our turn. Actually perform the computation in
    this thread so we can return a result. *)
let wait_in_line q description f =
  let m = Mutex.create () in
  let c = Condition.create () in
  let state = ref `Pending in
  Locking_helpers.Thread_state.waiting_for (Locking_helpers.Lock q.Thread_queue.name);
  let ok = q.Thread_queue.push_fn description
      (fun () ->
         (* Signal the mothership to run the computation now *)
         Mutex.execute m
           (fun () ->
              state := `Running;
              Condition.signal c
           );
         (* Wait for the computation to complete *)
         Mutex.execute m (fun () -> while !state = `Running do Condition.wait c m done)
      ) in
  assert ok; (* queue has no length limit *)
  (* Wait for the signal from the queue processor *)
  Mutex.execute m (fun () -> while !state = `Pending do Condition.wait c m done);
  Locking_helpers.Thread_state.acquired (Locking_helpers.Lock q.Thread_queue.name);
  finally f
    (fun () ->
       Locking_helpers.Thread_state.released (Locking_helpers.Lock q.Thread_queue.name);
       Mutex.execute m (fun () -> state := `Finished; Condition.signal c))
