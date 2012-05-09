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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)
(* Monitor the xapi server process, periodically logging stats.
 * @group Performance Monitoring
 *)

module D = Debug.Debugger(struct let name = "rrdd_xapi" end)
open D

(** Represents a subset of the data in /proc/meminfo *)
type meminfo = {
	total: int;			(* KiB *)
	free: int;			 (* KiB *)
	buffered: int;	 (* KiB *)
	cached: int;		 (* KiB *)
	swap_total: int; (* KiB *)
	swap_free: int;	(* KiB *)
}

(** Represents a subset of the data in /proc/<pid>/status *)
type process_memory_info = {
	peak: int; (* KiB *)
	size: int; (* KiB *)
	locked: int; (* KiB *)
	hwm: int; (* KiB *)
	rss: int; (* KiB *)
	data: int; (* KiB *)
	stack: int; (* KiB *)
	exe: int; (* KiB *)
	lib: int; (* KiB *)
}

open Stringext
open Threadext

(* TODO: Move this function (and its clones) to xen-api-libs. *)
let split_colon line =
	List.filter (fun x -> x <> "")
		(List.map (String.strip String.isspace) (String.split ' ' line))

let meminfo () = 
	let all = Unixext.string_of_file "/proc/meminfo" in
	let total = ref (-1)
	and free = ref (-1)
	and buffered = ref (-1)
	and cached = ref (-1)
	and swap_total = ref (-1)
	and swap_free = ref (-1) in
	List.iter (fun line -> match split_colon line with
		| [ "MemTotal:"; x; "kB" ] -> total := int_of_string x
		| [ "MemFree:"; x; "kB" ] -> free := int_of_string x
		| [ "Buffers:"; x; "kB" ] -> buffered := int_of_string x
		| [ "Cached:"; x; "kB" ] -> cached := int_of_string x
		| [ "SwapTotal:"; x; "kB" ] -> swap_total := int_of_string x
		| [ "SwapFree:"; x; "kB" ] -> swap_free := int_of_string x
		| _ -> ()
	) (String.split '\n' all);
	{total = !total; free = !free; buffered = !buffered;
	cached = !cached; swap_total = !swap_total; swap_free = !swap_free}

let string_of_meminfo (x: meminfo) =
	Printf.sprintf
		"MemTotal: %d KiB; MemFree: %d KiB; Buffered: %d KiB; Cached: %d KiB; SwapTotal: %d KiB; SwapFree: %d KiB"
		x.total x.free x.buffered x.cached x.swap_total x.swap_free

let process_memory_info_of_pid (pid: int) =
	let all = Unixext.string_of_file (Printf.sprintf "/proc/%d/status" pid) in
	let peak = ref (-1)
	and size = ref (-1)
	and locked = ref (-1)
	and hwm = ref (-1)
	and rss = ref (-1)
	and data = ref (-1)
	and stack = ref (-1)
	and exe = ref (-1)
	and lib = ref (-1) in
	List.iter (fun line -> match split_colon line with
		| [ "VmPeak:"; x; "kB" ] -> peak := int_of_string x
		| [ "VmSize:"; x; "kB" ] -> size := int_of_string x
		| [ "VmLck:"; x; "kB" ] -> locked := int_of_string x
		| [ "VmHWM:"; x; "kB" ] -> hwm := int_of_string x
		| [ "VmRSS:"; x; "kB" ] -> rss := int_of_string x
		| [ "VmData:"; x; "kB" ] -> data := int_of_string x
		| [ "VmStk:"; x; "kB" ] -> stack := int_of_string x
		| [ "VmExe:"; x; "kB" ] -> exe := int_of_string x
		| [ "VmLib:"; x; "kB" ] -> lib := int_of_string x
		| _ -> ()
	) (String.split '\n' all);
	{peak = !peak; size = !size; locked = !locked; hwm = !hwm;
	rss = !rss; data = !data; stack = !stack; exe = !exe; lib = !lib}

let string_of_process_memory_info (x: process_memory_info) = 
	Printf.sprintf
		"size: %d KiB; rss: %d KiB; data: %d KiB; stack: %d KiB"
		x.size x.rss x.data x.stack

let one () =
	let pid = Unix.getpid () in
	let pmi = process_memory_info_of_pid pid in
	let mi = string_of_meminfo (meminfo ()) in
	debug "Process: %s" (string_of_process_memory_info pmi);
	debug "System: %s" mi

let last_log = ref 0.
let log_interval = 60.

(** Called from the main monitoring loop *)
let go () =
	try
		(* Only run once every minute to avoid spamming the logs *)
		let now = Unix.gettimeofday () in
		if now -. !last_log > log_interval then (
			last_log := now;
			one ()
		)
	with e ->
		debug "Monitor_self caught: %s" (Printexc.to_string e)
