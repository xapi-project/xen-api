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
open Stringext
open Pervasiveext
open Threadext

(** Associate a task with each active thread *)
let thread_tasks : (int, string) Hashtbl.t = Hashtbl.create 256 
let thread_tasks_m = Mutex.create ()

let get_thread_id () =
  try Thread.id (Thread.self ()) with _ -> -1 

(* Theses functions need to be defined later in the code. *)
let get_hostname = 
  let f () = "Debug.get_hostname not set" in
  ref f

let associate_thread_with_task task = 
  let id = get_thread_id () in
  if id <> -1
  then begin
    Mutex.execute thread_tasks_m (fun () -> Hashtbl.add thread_tasks id task); 
  end

let get_task_from_thread () = 
  let id = get_thread_id () in
  Mutex.execute thread_tasks_m 
    (fun () -> if Hashtbl.mem thread_tasks id then Some(Hashtbl.find thread_tasks id) else None)

let dissociate_thread_from_task () =
  let id = get_thread_id () in
  if id <> -1
  then match get_task_from_thread () with
  | Some _ ->
      Mutex.execute thread_tasks_m (fun () -> Hashtbl.remove thread_tasks id)
  | None ->
      let extra = Printf.sprintf "[thread: debug (%n)] " id in
      Logs.info ~extra "debug" "Thread id %d is not associated with any task" id

let with_thread_associated task f x = 
  associate_thread_with_task task;
  finally
    (fun () -> f x)
    dissociate_thread_from_task

let threadnames = Hashtbl.create 256
let tnmutex = Mutex.create () 
module StringSet = Set.Make(struct type t=string let compare=Pervasives.compare end)
let debug_keys = ref StringSet.empty 
let get_all_debug_keys () =
	StringSet.fold (fun key keys -> key::keys) !debug_keys []

let dkmutex = Mutex.create ()

let _ = Hashtbl.add threadnames (-1) "no thread"

let get_thread_id () =
    try Thread.id (Thread.self ()) with _ -> -1 

let name_thread name =
    let id = get_thread_id () in
    Mutex.execute tnmutex (fun () -> Hashtbl.add threadnames id name)

let remove_thread_name () =
    let id = get_thread_id () in
    Mutex.execute tnmutex (fun () -> Hashtbl.remove threadnames id)

module type BRAND = sig
  val name: string
end

module Debugger = functor(Brand: BRAND) -> struct
  let hostname = Unix.gethostname ()
  let _ =
    Mutex.execute dkmutex (fun () -> 
      debug_keys := StringSet.add Brand.name !debug_keys)

  let get_thread_name () =
    let id = get_thread_id () in
    Mutex.execute tnmutex 
      (fun () -> 
        try
          Printf.sprintf "%d %s" id (Hashtbl.find threadnames id)
        with _ -> 
          Printf.sprintf "%d" id)

  let get_task () =
    default "" (may (fun s -> s) (get_task_from_thread ()))

  let output (f:string -> ?extra:string -> ('a, unit, string, 'b) format4 -> 'a) fmt =
    let extra = 
      Printf.sprintf "%s|%s|%s|%s" 
      hostname
      (get_thread_name ()) 
      (get_task ())
      Brand.name
    in
    f Brand.name ~extra fmt

	let output_and_return ?raw (f:string -> ?raw:bool -> ?extra:string -> ('a, unit, string, 'b) format4 -> 'a) fmt =
    let extra =
      Printf.sprintf "%s|%s|%s|%s"
      hostname
      (get_thread_name ())
      (get_task ())
      Brand.name
    in
    f Brand.name ?raw ~extra fmt
    
  let debug fmt = output Logs.debug fmt
  let warn fmt = output Logs.warn fmt
  let info fmt = output Logs.info fmt
  let error fmt = output Logs.error fmt
  let audit ?raw fmt = output_and_return ?raw Logs.audit fmt

  let log_backtrace () =
    let backtrace = Backtrace.get_backtrace () in
    debug "%s" (String.escaped backtrace)

end
