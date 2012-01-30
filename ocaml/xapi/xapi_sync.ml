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
(* Synchronise the locally stored objects between the hosts in a pool *)

module D = Debug.Debugger(struct let name="sync" end)
open D

open Threadext

let sync_lock = Mutex.create ()

let sync_host ~__context host =
	Mutex.execute sync_lock (fun () ->
		try
			let localhost        = host = !Xapi_globs.localhost_ref
			and host_has_storage = not (List.mem_assoc Xapi_globs.host_no_local_storage (Db.Host.get_other_config ~__context ~self:host)) in

			if (not localhost) && host_has_storage then begin
				let address = Db.Host.get_address ~__context ~self:host in
				debug "Beginning sync with host at address: %s" address;

				let localpath  = Printf.sprintf "%s/" Xapi_globs.xapi_blob_location
				and remotepath = Printf.sprintf "%s:%s" address Xapi_globs.xapi_blob_location
				and session = Xapi_session.slave_login ~__context ~host:(Helpers.get_localhost ~__context) ~psecret:!Xapi_globs.pool_secret in
				Unix.putenv "XSH_SESSION" (Ref.string_of session);

				let output,log = Forkhelpers.execute_command_get_output
					~env:(Unix.environment ())
					"/usr/bin/rsync"
					["--delete";"--stats";"-az";localpath;remotepath;"-e"; Filename.concat Fhs.bindir "xsh"] in
				debug "sync output: \n%s" output;
				debug "log output: '%s'" log;

				(* Store the last blob sync time in the Host.other_config *)
				(try Db.Host.remove_from_other_config ~__context ~self:host ~key:Xapi_globs.last_blob_sync_time with _ -> ());
				Db.Host.add_to_other_config ~__context ~self:host ~key:Xapi_globs.last_blob_sync_time ~value:(string_of_float (Unix.gettimeofday ()));
			end

			else begin
				debug "Ignoring host synchronise: localhost=%b host_has_storage=%b" localhost host_has_storage
			end;

		with Forkhelpers.Spawn_internal_error(log,output,status) ->
			(* Do we think the host is supposed to be online? *)
			let online =
				try
					let m = Db.Host.get_metrics ~__context ~self:host in
					Db.Host_metrics.get_live ~__context ~self:m
				with _ -> false in

			(* In rolling upgrade mode we would also expect a failure *)
			let rolling_upgrade = Helpers.rolling_upgrade_in_progress ~__context in
			if online && not rolling_upgrade
			then error "Unexpected failure synchronising blobs to host %s; log='%s'; output='%s'" (Ref.string_of host) log output;
	)

let do_sync () =
	Server_helpers.exec_with_new_task "blob sync" (fun __context ->
		let hosts = Db.Host.get_all ~__context in
		List.iter (sync_host ~__context) hosts)
