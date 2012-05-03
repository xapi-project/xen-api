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

module D = Debug.Debugger(struct let name="rrdd_proxy" end)
open D

(* Helper methods. Should probably be moved to the Http.Request module. *)
let get_query_string_from_query ~(query : (string * string) list) : string =
	String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) query)

let get_query_string ~(req : Http.Request.t) : string =
	get_query_string_from_query ~query:req.Http.Request.query

let make_url_from_query ~(address : string) ~(uri : string)
		~(query : (string * string) list) : string =
	let query_string = get_query_string_from_query query in
	Printf.sprintf "https://%s%s?%s" address uri query_string

let make_url ~(address : string) ~(req : Http.Request.t) : string =
	let open Http.Request in
	make_url_from_query ~address ~uri:req.uri ~query:req.query
(* End of helper methods. *)

(* If the host contains the RRD for the requested VM then simply forward the
 * HTTP request to rrdd_http_handler. Otherwise, we redirect to the host that
 * contains the corresponding VM. The last resort is to unarchive the RRD on
 * the master. The exact logic can be seen under "The logic." below.
 *)
let get_vm_rrd_forwarder (req : Http.Request.t) (s : Unix.file_descr) _ =
	debug "put_rrd_forwarder: start";
	let query = req.Http.Request.query in
	req.Http.Request.close <- true;
	if not (List.mem_assoc "ref" query) && not (List.mem_assoc "uuid" query) then begin
		error "HTTP request for RRD is missing the 'uuid' parameter.";
		Http_svr.headers s (Http.http_400_badrequest ());
	end;
	let vm_uuid = List.assoc "uuid" query in
	if Rrdd.has_vm_rrd ~vm_uuid then (
		ignore (Xapi_services.hand_over_connection req s Rrdd_interface.fd_path)
	) else (
		Xapi_http.with_context ~dummy:true "Obtaining VM RRD metrics." req s
			(fun __context ->
				let open Http.Request in
				let unarchive_query_key = "rrd_unarchive" in
				(* List of possible actions. *)
				let read_at_owner owner =
					let address = Db.Host.get_address ~__context ~self:owner in
					let url = make_url ~address ~req in
					Http_svr.headers s (Http.http_302_redirect url) in
				let unarchive_at_master () =
					let address = Pool_role.get_master_address () in
					let query = (unarchive_query_key, "") :: query in
					let url = make_url_from_query ~address ~uri:req.uri ~query in
					Http_svr.headers s (Http.http_302_redirect url) in
				let unarchive () =
					let req = {req with uri = Constants.rrd_unarchive_uri} in
					ignore (Xapi_services.hand_over_connection req s Rrdd_interface.fd_path) in
				(* List of conditions involved. *)
				let is_unarchive_request = List.mem_assoc unarchive_query_key query in
				let is_master = Pool_role.is_master () in
				let is_owner_online owner = Db.is_valid_ref __context owner in
				let is_xapi_initialising = List.mem_assoc "dbsync" query in
				(* The logic. *)
				if is_unarchive_request then unarchive ()
				else (
					let localhost_uuid = Helpers.get_localhost_uuid () in
					let vm_ref = Db.VM.get_by_uuid ~__context ~uuid:vm_uuid in
					let owner = Db.VM.get_resident_on ~__context ~self:vm_ref in
					let owner_uuid = Ref.string_of owner in
					let is_owner_localhost = (owner_uuid = localhost_uuid) in
					if is_owner_localhost then (
						if is_master then unarchive () else unarchive_at_master ()
					) else (
						if is_owner_online owner && not is_xapi_initialising
						then read_at_owner owner
						else unarchive_at_master ()
					)
				)
			)
	)

(* Forward the request for host RRD data to the RRDD HTTP handler. If the host
 * is initialising, send the unarchive command to the host instead.
 *)
let get_host_rrd_forwarder (req: Http.Request.t) (s : Unix.file_descr) _ =
	debug "get_host_rrd_forwarder";
	let query = req.Http.Request.query in
	req.Http.Request.close <- true;
	Xapi_http.with_context ~dummy:true "Obtaining Host RRD metrics." req s
		(fun __context ->
			if List.mem_assoc "dbsync" query then ( (* Host initialising. *)
				if not (List.mem_assoc "uuid" query) then (
					error "HTTP request for RRD is missing the 'uuid' parameter.";
					Http_svr.headers s (Http.http_400_badrequest ())
				) else (
					let req = {req with Http.Request.uri = Constants.rrd_unarchive_uri} in
					ignore (Xapi_services.hand_over_connection req s Rrdd_interface.fd_path)
				)
			) else ( (* Normal request. *)
				ignore (Xapi_services.hand_over_connection req s Rrdd_interface.fd_path)
			)
		)

(* Forward the request for obtaining RRD data updates to the RRDD HTTP handler. *)
let get_rrd_updates_forwarder (req: Http.Request.t) (s : Unix.file_descr) _ =
	(* Do not log this event, since commonly called. *)
	let query = req.Http.Request.query in
	req.Http.Request.close <- true;
	Xapi_http.with_context ~dummy:true "Obtaining RRD updates." req s
		(fun __context ->
			if not (List.mem_assoc "start" query) then (
				error "HTTP request for RRD updates is missing the 'start' parameter.";
				Http_svr.headers s (Http.http_400_badrequest ())
			) else (
				ignore (Xapi_services.hand_over_connection req s Rrdd_interface.fd_path)
			)
		)

let put_rrd_forwarder (req : Http.Request.t) (s : Unix.file_descr) _ = ()
	(* Monitor_rrds.receieve_handler *)

let is_vm_on_localhost ~__context ~(vm_uuid : string) : bool =
  let localhost = Helpers.get_localhost ~__context in
	let vm = Db.VM.get_by_uuid ~__context ~uuid:vm_uuid in
	let vm_host = Db.VM.get_resident_on ~__context ~self:vm in
	localhost = vm_host

let push_rrd ~__context ~(vm_uuid : string) : unit =
	let master_address = Pool_role.get_master_address () in
	let is_on_localhost = is_vm_on_localhost ~__context ~vm_uuid in
	Rrdd.push_rrd ~master_address ~vm_uuid ~is_on_localhost

let migrate_rrd ~__context ?remote_address ?session_id ~vm_uuid ~host_uuid () =
	let remote_address = match remote_address with
		| None -> Db.Host.get_address ~__context ~self:(Ref.of_string host_uuid)
		| Some a -> a
	in Rrdd.migrate_rrd ~remote_address ?session_id ~vm_uuid ~host_uuid ()

let send_host_rrd_to_master () =
	let master_address = Pool_role.get_master_address () in
	Rrdd.send_host_rrd_to_master ~master_address

let backup_rrds ?(save_stats_locally : bool option) () =
	let master_address = Pool_role.get_master_address () in
	Rrdd.backup_rrds ~master_address ?save_stats_locally ()

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
