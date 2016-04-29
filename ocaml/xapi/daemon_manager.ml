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

open Threadext

module IntSet = Set.Make(struct type t = int let compare = compare end)

type daemon_check =
	| Pidfile of string
	| Function of (unit -> bool)

type daemon_state = [
	`unmanaged |
	`should_start |
	`should_not_start
]

module type DAEMON = sig
	val check : daemon_check

	val start : unit -> unit

	val stop : unit -> unit
end

module Make(D : DAEMON) = struct
	let registered_threads = ref IntSet.empty

	let register_thread id =
		let registered = !registered_threads in
		registered_threads := (IntSet.add id registered)

	let deregister_thread id =
		let registered = !registered_threads in
		registered_threads := (IntSet.remove id registered)

	let are_threads_registered () =
		let registered = !registered_threads in
		not (IntSet.is_empty registered)

	let daemon_state : daemon_state ref = ref `unmanaged
	let m = Mutex.create ()

	let is_running () =
		match D.check with
		| Pidfile file -> begin
			try
				let pid = Unixext.string_of_file file |> String.trim |> int_of_string in
				Unix.kill pid 0;
				true
			with _ -> false
		end
	| Function f -> f ()

	let with_daemon_stopped f =
		let thread_id = Thread.(id (self ())) in
		(* Stop the daemon if it's running, then register this thread. *)
		Mutex.execute m
			(fun () ->
				begin
					match is_running (), !daemon_state with
					| true, _ -> (daemon_state := `should_start; D.stop ())
					| false, `unmanaged -> daemon_state := `should_not_start
					| false, _ -> ()
				end;
				register_thread thread_id);
		Pervasiveext.finally
			f
			(* Deregister this thread, and if there are no more threads registered,
			 * start the daemon if it was running in the first place. *)
			(fun () ->
				Mutex.execute m
					(fun () ->
						deregister_thread thread_id;
						match are_threads_registered (), !daemon_state with
						| true, _ -> ()
						| false, `should_start -> (D.start (); daemon_state := `unmanaged)
						| false, _ -> daemon_state := `unmanaged))
end
