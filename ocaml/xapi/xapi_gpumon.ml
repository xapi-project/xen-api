(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

module D = Debug.Make(struct let name="xapi" end)
open D

open Fun
open Xstringext
open Threadext

let service = "/sbin/service"
let gpumon = "xcp-rrdd-gpumon"
let pidfile = "/var/run/xcp-rrdd-gpumon.pid"

let get_pid () =
	try
		let pid =
			(* TODO: Use String.trim when porting this to OCaml 4.x *)
			Unixext.string_of_file pidfile
			|> String.strip String.isspace
			|> int_of_string
		in
		Unix.kill pid 0;
		Some pid
	with _ ->
		None

let start () =
	debug "Starting %s" gpumon;
	ignore (Forkhelpers.execute_command_get_output service [gpumon; "start"])

let stop () =
	debug "Stopping %s" gpumon;
	ignore (Forkhelpers.execute_command_get_output service [gpumon; "stop"])

module IntSet = Set.Make(struct type t = int let compare = compare end)
let registered_threads = ref IntSet.empty

let register_thread id =
	let state = !registered_threads in
	registered_threads := (IntSet.add id state)

let deregister_thread id =
	let state = !registered_threads in
	registered_threads := (IntSet.remove id state)

let are_threads_registered () =
	let state = !registered_threads in
	not (IntSet.is_empty state)

(* None
 * - means no threads which require gpumon to be stopped are running.
 * Some true
 * - means gpumon must be started when the last thread
 *   leaves with_gpumon_stopped.
 * Some false
 * - means gpumon should not be started when the last thread
 *   leaves with_gpumon_stopped. *)
let restart_gpumon = ref None
let m = Mutex.create ()

(* gpumon must be stopped while any thread is running the function f
 * passed to this function.
 *
 * The first thread to enter this function will stop gpumon if it is running,
 * and set the restart_gpumon flag accordingly.
 *
 * The last thread to leave this function will start gpumon, if
 * restart_gpumon is set to Some true. *)
let with_gpumon_stopped ~f =
	let thread_id = Thread.(id (self ())) in
	(* Stop gpumon if it's running, then register this thread. *)
	Mutex.execute m
		(fun () ->
			begin
				match get_pid (), !restart_gpumon with
				| Some pid, _ -> (restart_gpumon := Some true; stop ())
				| None, None -> restart_gpumon := Some false
				| None, _ -> ()
			end;
			register_thread thread_id);
	Pervasiveext.finally
		f
		(* Deregister this thread, and if there are no more threads registered,
		 * start gpumon if it was running in the first place. *)
		(fun () ->
			Mutex.execute m
				(fun () ->
					deregister_thread thread_id;
					match are_threads_registered (), !restart_gpumon with
					| true, _ -> ()
					| false, Some true -> (start (); restart_gpumon := None)
					| false, _ -> restart_gpumon := None))
