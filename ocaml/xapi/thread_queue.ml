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

(* A simple locked queue implementation where a background thread pulls jobs serially from the queue and executes them.
   Useful for offloading potentially blocking but not critical tasks to background threads (like HA alerts) *)

open Stdext
open Pervasiveext
open Threadext

module D = Debug.Make(struct let name="thread_queue" end)
open D

(** The type of the function which processes elements taken from the queue *)
type 'a process_fn = 'a -> unit

(** The type of the function which pushes new elements into the queue *)
type 'a push_fn = string -> 'a -> bool

type 'a t = {
  push_fn: 'a push_fn;
  name: string;
}

(** Given an optional maximum queue length and a function for processing elements (which will be called in a
    single background thread), return a function which pushes items onto the queue. *)
let make ?max_q_length ?(name="unknown") (process_fn: 'a process_fn) : 'a t =
  let q = Queue.create () in
  let c = Condition.create () in
  let m = Mutex.create () in

  let string_of_queue q =
    let items = List.rev (Queue.fold (fun acc (description, _) -> description::acc) [] q) in
    Printf.sprintf "[ %s ](%d)" (String.concat "; " items) (List.length items) in

  (** The background thread *)
  let t = ref None in

  let thread_body () =
    Mutex.execute m
      (fun () ->
         while true do
           (* Wait until there is work to do *)
           while Queue.length q = 0 do Condition.wait c m done;
           (* Make a copy of the items in the q so we can drop the lock and process them *)
           let local_q = Queue.copy q in
           Queue.clear q;

           Mutex.unlock m;
           (* Process the items dropping any exceptions (process function should do whatever logging it wants) *)
           finally
             (fun () ->
                Queue.iter
                  (fun (description, x) ->
                     debug "pop(%s) = %s" name description;
                     try process_fn x with _ -> ())
                  local_q
             )
             (fun () -> Mutex.lock m);
           debug "%s: completed processing %d items: queue = %s" name (Queue.length local_q) (string_of_queue q);
         done
      ) in

  (* Called with lock already held *)
  let maybe_start_thread () =
    match !t with
    | Some _ -> ()
    | None -> t := Some (Thread.create thread_body ()) in

  let push description x =
    Mutex.execute m
      (fun () ->
         let q_length = Queue.length q in
         match max_q_length with
         | Some max when q_length > max ->
           warn "%s: Maximum length exceeded (%d): dropping item" name max;
           false
         | _ ->
           Queue.push (description, x) q;
           debug "push(%s, %s): queue = %s" name description (string_of_queue q);
           Condition.signal c;
           maybe_start_thread ();
           true
      )

  in { push_fn = push; name = name }
