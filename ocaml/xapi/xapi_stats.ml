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

open Rrdd_plugin
open Threadext
module D = Debug.Make(struct let name = "xapi_stats" end)

let generate_master_stats ~__context =
	let session_count =
		Db.Session.get_all ~__context
		|> List.length
		|> Int64.of_int
	in
	let session_count_ds =
		Rrd.Host,
		Ds.ds_make
			~name:"pool_session_count"
			~description:"Number of sessions"
			~value:(Rrd.VT_Int64 session_count)
			~ty:Rrd.Gauge
			~default:true
			~min:0.0
			~units:"sessions"
			()
	in
	let task_count = Db.Task.get_all ~__context
		|> List.length
		|> Int64.of_int
	in
	let task_count_ds =
		Rrd.Host,
		Ds.ds_make
			~name:"pool_task_count"
			~description:"Number of tasks"
			~value:(Rrd.VT_Int64 task_count)
			~ty:Rrd.Gauge
			~default:true
			~min:0.0
			~units:"tasks"
			()
	in
	[session_count_ds; task_count_ds]

let gc_debug = ref true
let previous_oldness = ref 0
let previous_free_words = ref 0
let previous_live_words = ref 0

let generate_gc_stats () =
	let gcstat =
		if !gc_debug then (
			if !previous_oldness > 5 then (
				let stat = Gc.stat () in
				previous_free_words := stat.Gc.free_words;
				previous_live_words := stat.Gc.live_words;
				previous_oldness := 0;
				stat
			) else (
				incr previous_oldness;
				{(Gc.quick_stat ()) with
					Gc.free_words = !previous_free_words;
					Gc.live_words = !previous_live_words;}
			)
		) else Gc.quick_stat ()
	in
	let xapigrad_kib =
		(gcstat.Gc.minor_words +. gcstat.Gc.major_words -. gcstat.Gc.promoted_words)
		/. 256.
	in
	let xapitotal_kib = Int64.of_int (gcstat.Gc.heap_words / 256) in
	let xapiactualfree_kib = Int64.of_int (gcstat.Gc.free_words / 256) in
	let xapiactuallive_kib = Int64.of_int (gcstat.Gc.live_words / 256) in
	[
		(Rrd.Host, Ds.ds_make ~name:"xapi_memory_usage_kib" ~units:"KiB"
			~description:"Total memory allocated used by xapi daemon"
			~value:(Rrd.VT_Int64 xapitotal_kib) ~ty:Rrd.Gauge ~min:0.0
			~default:true ());
		(Rrd.Host, Ds.ds_make ~name:"xapi_free_memory_kib" ~units:"KiB"
			~description:"Free memory available to the xapi daemon"
			~value:(Rrd.VT_Int64 xapiactualfree_kib) ~ty:Rrd.Gauge ~min:0.0
			~default:true ());
		(Rrd.Host, Ds.ds_make ~name:"xapi_live_memory_kib" ~units:"KiB"
			~description:"Live memory used by xapi daemon"
			~value:(Rrd.VT_Int64 xapiactuallive_kib) ~ty:Rrd.Gauge ~min:0.0
			~default:true ());
		(Rrd.Host, Ds.ds_make ~name:"xapi_allocation_kib" ~units:"KiB"
			~description:"Memory allocation done by the xapi daemon"
			~value:(Rrd.VT_Float xapigrad_kib) ~ty:Rrd.Derive ~min:0.0
			~default:true ());
	]

let generate_other_stats () =
	let open_fds =
		Utils.list_directory_entries_unsafe "/proc/self/fd"
		|> List.length
		|> Int64.of_int
	in
	let open_fds_ds =
		Rrd.Host,
		Ds.ds_make
			~name:"xapi_open_fds"
			~description:"Number of open file descriptors held by xapi"
			~value:(Rrd.VT_Int64 open_fds)
			~ty:Rrd.Gauge
			~default:true
			~min:0.0
			~units:"file descriptors"
			()
	in
	[open_fds_ds]

let generate_stats ~__context ~master =
	let master_only_stats =
		if master
		then generate_master_stats ~__context
		else []
	in
	let gc_stats = generate_gc_stats () in
	let other_stats = generate_other_stats () in
	List.fold_left
		(fun acc stats -> List.rev_append acc stats)
		[]
		[master_only_stats; gc_stats; other_stats]

let reporter_cache : Reporter.t option ref = ref None
let reporter_m = Mutex.create ()

let start () =
	let __context = Context.make "xapi_stats" in
	let master = (Pool_role.is_master ()) in
	Mutex.execute reporter_m
		(fun () ->
			match !reporter_cache with
			| Some _ -> ()
			| None ->
				let reporter =
					Reporter.start_async
						(module D : Debug.DEBUG)
						~uid:"xapi-stats"
						~neg_shift:0.5
						~target:Reporter.Local
						~protocol:Rrd_interface.V2
						~dss_f:(fun () -> generate_stats ~__context ~master)
				in
				reporter_cache := (Some reporter))

let stop () =
	Mutex.execute reporter_m
		(fun () ->
			match !reporter_cache with
			| None -> ()
			| Some reporter ->
				Reporter.cancel reporter)
