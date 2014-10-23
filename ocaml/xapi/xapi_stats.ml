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
	if master
	then (generate_master_stats ~__context) @ (generate_other_stats ())
	else (generate_other_stats ())

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
