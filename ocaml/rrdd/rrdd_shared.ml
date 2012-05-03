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

module D = Debug.Debugger(struct let name="rrdd_shared" end)
open D

let mutex = Mutex.create ()

let localhost_uuid =
	Util_inventory.lookup Util_inventory._installation_uuid

type rrd_info = {
	rrd: Rrd.rrd;
	mutable dss: Ds.ds list;
	mutable domid: int;
}

(* RRDs *)
let vm_rrds : (string, rrd_info) Hashtbl.t = Hashtbl.create 32
let host_rrd : rrd_info option ref = ref None

let rrd_of_fd fd =
	let ic = Unix.in_channel_of_descr fd in
	let input = Xmlm.make_input (`Channel ic) in
	Rrd.from_xml input

(** Helper function - path is the path to the file _without the extension .gz_ *)
let rrd_of_gzip path =
	let gz_path = path ^ ".gz" in
	let gz_exists = try let (_: Unix.stats) = Unix.stat gz_path in true with _ -> false in
	if gz_exists then begin
		Unixext.with_file gz_path [ Unix.O_RDONLY ] 0o0
			(fun fd -> Gzip.decompress_passive fd rrd_of_fd)
	end else begin
		(* If this fails, let the exception propagate *)
		Unixext.with_file path [ Unix.O_RDONLY ] 0 rrd_of_fd
	end

(* Send rrds to a remote host. If the host is on another pool, you
 * must pass the session_id parameter, and optionally the __context. *)
let send_rrd ?(session_id : string option) ~(address : string)
		~(to_archive : bool) ~(uuid : string) ~(rrd : Rrd.rrd) () =
	debug "Sending RRD for object uuid=%s archiving=%b to address: %s"
		uuid to_archive address;
	let arch_query = if to_archive then ["archive", "true"] else [] in
	let sid_query = match session_id with
		None -> [] | Some id -> ["session_id", id] in
	let query = sid_query @ arch_query @ ["uuid", uuid] in
	let cookie =
		if sid_query = [] then ["pool_secret", !Xapi_globs.pool_secret] else [] in
	let request =
		Http.Request.make ~user_agent:Xapi_globs.xapi_user_agent
			~query ~cookie Http.Put Constants.put_rrd_uri in
	let open Xmlrpc_client in
	let transport = SSL(SSL.make (), address, !Xapi_globs.https_port) in
	with_transport transport (
		with_http request (fun (response, fd) ->
			try Rrd.to_fd rrd fd
			with e ->
				(*debug "Caught exception: %s" (ExnHelper.string_of_exn e);*)
				log_backtrace ()
		)
	)

let archive_rrd ~master_address ~save_stats_locally ~uuid ~rrd =
	debug "Archiving RRD for object uuid=%s %s" uuid
		(if save_stats_locally then "to local disk" else "to remote master");
	if save_stats_locally then begin
		try
			(* Stash away the rrd onto disk. *)
			let exists =
				try
					let (_: Unix.stats) = Unix.stat Xapi_globs.xapi_blob_location in
					true
				with _ -> false
			in
			if exists then begin
				Unixext.mkdir_safe Xapi_globs.xapi_rrd_location 0o755;
				let base_filename = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
				Unixext.atomic_write_to_file (base_filename ^ ".gz") 0o644
					(fun fd -> Gzip.compress fd (Rrd.to_fd rrd));
				(* If there's an uncompressed one hanging around, remove it. *)
				Unixext.unlink_safe base_filename
			end else begin
				debug "No local storage: not persisting RRDs"
			end
		with e ->
			(*debug "Caught exception: %s" (ExnHelper.string_of_exn e);*)
			log_backtrace();
	end else begin
		(* Stream it to the master to store, or maybe to a host in the migrate case *)
		debug "About to send to master.";
		send_rrd ~address:master_address ~to_archive:true ~uuid ~rrd ()
	end
