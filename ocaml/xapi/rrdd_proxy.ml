(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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
(**
 * @group Performance Monitoring
 *)

(* This module is used for easier interaction of xapi with rrdd. Mainly, it
 * looks up the required information that is available to xapi, and calls
 * same-named methods in rrdd.
 *)

let handler (req: Http.Request.t) s _ = ()
	(* Monitor_rrds.handler *)

let receive_handler (req: Http.Request.t) (bio: Buf_io.t) _ = ()
	(* Monitor_rrds.receieve_handler *)

let handler_host (req: Http.Request.t) s _ = ()
	(* Monitor_rrds.handler_host *)

let handler_rrd_updates (req: Http.Request.t) s _ = ()
	(* Monitor_rrds.handler_host *)

module Deprecated = struct
	let get_timescale ~__context =
		let host = Helpers.get_localhost ~__context in
		let other_config = Db.Host.get_other_config ~__context ~self:host in
		try int_of_string (List.assoc Constants.rrd_update_interval other_config)
		with _ -> 0

	let load_rrd ~__context ~uuid ~is_host =
		let master_address = Pool_role.get_master_address () in
		let is_master = Pool_role.is_master () in
		let timescale = get_timescale ~__context in
		Rrdd.Deprecated.load_rrd ~master_address ~is_master ~uuid ~is_host
			~timescale ()
end

let backup_rrds ?(save_stats_locally : bool option) () =
	let master_address = Pool_role.get_master_address () in
	let localhost_uuid = Helpers.get_localhost_uuid () in
	Rrdd.backup_rrds ~master_address ?save_stats_locally ~localhost_uuid ()

let migrate_rrd ~__context ?remote_address ?session_id ~vm_uuid ~host_uuid () =
	let remote_address = match remote_address with
		| None -> Db.Host.get_address ~__context ~self:(Ref.of_string host_uuid)
		| Some a -> a
	in Rrdd.migrate_rrd ~remote_address ?session_id ~vm_uuid ~host_uuid ()
