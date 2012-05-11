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
	  ()

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

let hostname_cache = ref None
let hostname_m = Mutex.create ()
let get_hostname () =
  match Mutex.execute hostname_m (fun () -> !hostname_cache) with
  | Some h -> h
  | None ->
		let h = Unix.gethostname () in
		Mutex.execute hostname_m (fun () -> hostname_cache := Some h);
		h
let invalidate_hostname_cache () = Mutex.execute hostname_m (fun () -> hostname_cache := None)

let facility = ref Syslog.Daemon
let facility_m = Mutex.create ()
let set_facility f = Mutex.execute facility_m (fun () -> facility := f)
let get_facility () = Mutex.execute facility_m (fun () -> !facility)

let logging_disabled_for = ref []
let logging_disabled_for_m = Mutex.create ()
let disable brand =
	Mutex.execute logging_disabled_for_m
		(fun () -> logging_disabled_for := brand :: !logging_disabled_for)
let enable brand =
	Mutex.execute logging_disabled_for_m
		(fun () -> logging_disabled_for := List.filter (fun x -> x <> brand) !logging_disabled_for)
let is_disabled brand =
	Mutex.execute logging_disabled_for_m
		(fun () -> List.mem brand !logging_disabled_for)

let gettimestring () =
	let time = Unix.gettimeofday () in
	let tm = Unix.gmtime time in
	let msec = time -. (floor time) in
	Printf.sprintf "%d%.2d%.2dT%.2d:%.2d:%.2d.%.3dZ|" (1900 + tm.Unix.tm_year)
		(tm.Unix.tm_mon + 1) tm.Unix.tm_mday
		tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
		(int_of_float (1000.0 *. msec))

let print_debug = ref false
let log_to_stdout () = print_debug := true

module Debugger = functor(Brand: BRAND) -> struct
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

	let make_log_message include_time brand priority message =
		let extra =
			Printf.sprintf "%s|%s|%s|%s"
				(get_hostname ())
				(get_thread_name ())
				(get_task ())
				brand in
		Printf.sprintf "[%s%.5s|%s] %s" (if include_time then gettimestring () else "") priority extra message



	let output level priority (fmt: ('a, unit, string, 'b) format4) =
		Printf.kprintf
			(fun s ->
				if not(is_disabled Brand.name) then begin
					let msg = make_log_message false Brand.name priority s in

					if !print_debug
					then Printf.printf "%s\n%!" (make_log_message true Brand.name priority s);

					Syslog.log (get_facility ()) level msg
				end
			) fmt
    
	let debug fmt = output Syslog.Debug "debug" fmt
	let warn fmt = output Syslog.Warning "warn" fmt
	let info fmt = output Syslog.Info "info" fmt
	let error fmt = output Syslog.Err "error" fmt
	let audit ?(raw=false) (fmt: ('a, unit, string, 'b) format4) =
		Printf.kprintf
			(fun s ->
				let msg = if raw then s else make_log_message true Brand.name "audit" s in
				Syslog.log Syslog.Local6 Syslog.Info msg;
				msg
			) fmt

  let log_backtrace () =
    let backtrace = Backtrace.get_backtrace () in
    debug "%s" (String.escaped backtrace)

end
