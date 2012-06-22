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

module D = Debug.Debugger(struct let name = "rrdd_shared" end)
open D

module StringSet = Set.Make(String)

(* Monitoring state. *)
let dirty_host_memory = ref false
let dirty_memory = ref StringSet.empty
let dirty_pifs = ref StringSet.empty
let pif_stats : Monitor_types.pif list ref = ref []

(** Cache memory/target values *)
let memory_targets : (int, int64) Hashtbl.t = Hashtbl.create 20
let memory_targets_m = Mutex.create ()

let cache_sr_uuid : string option ref = ref None
let cache_sr_lock = Mutex.create ()

(** Pool secret. *)
let get_pool_secret () =
	try
		Unix.access Constants.pool_secret_path [Unix.F_OK];
		Unixext.string_of_file Constants.pool_secret_path
	with _ ->
		failwith "Unable to read the pool secret."

(* Here is the only place where RRDs are created. The timescales are fixed. If
 * other timescales are required, this could be done externally. The types of
 * archives created are also fixed.  Currently, we're making 4 timescales of 3
 * types of archive. This adds up to a total of (120+120+168+366)*3 doubles per
 * field, and at 8 bytes per double this is a grand total of 18k per field. For
 * a VM with 2 VBDs, 2 VCPUs and 1 VIF, this adds up to 130k of data per VM.
 * This is the function where tuning could be done to change this. *)
let timescales =
	(* These are purely for xenrt testing. *)
	if Xapi_fist.reduce_rra_times then [
		(120, 1);
		(20, 12);
		(15, 24);
		(10, 36);
	] else [
		(120,     1); (* 120 values of interval 1 step (5 secs) = 10 mins  *)
		(120,    12); (* 120 values of interval 12 steps (1 min) = 2 hours *)
		(168,   720); (* 168 values of interval 720 steps (1 hr) = 1 week  *)
		(366, 17280); (* 366 values of interval 17280 steps (1 day) = 1 yr *)
	]

let use_min_max = ref false

let mutex = Mutex.create ()

let localhost_uuid =
	Util_inventory.lookup Util_inventory._installation_uuid

type rrd_info = {
	rrd : Rrd.rrd;
	mutable dss : Ds.ds list;
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
		if sid_query = [] then ["pool_secret", get_pool_secret ()] else [] in
	let request =
		Http.Request.make ~user_agent:Xapi_globs.xapi_user_agent
			~query ~cookie Http.Put Constants.put_rrd_uri in
	let open Xmlrpc_client in
	let transport = SSL(SSL.make (), address, !Xapi_globs.https_port) in
	with_transport transport (
		with_http request (fun (response, fd) ->
			try Rrd.to_fd rrd fd
			with e -> log_backtrace ()
		)
	);
	debug "Sending RRD complete."

let archive_rrd ?(save_stats_locally = Pool_role_shared.is_master ()) ~uuid
		~rrd () =
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
		let address = Pool_role_shared.get_master_address () in
		send_rrd ~address ~to_archive:true ~uuid ~rrd ()
	end

module Deprecated = struct
	let full_update : bool ref = ref false
	let full_update_last_rra_idx : int ref = ref (-1)
	let full_update_avg_rra_idx : int ref = ref (-1)

	(* DEPRECATED *)
	(* The condition variable no longer makes sense, since the trigger and the
	 * listener can no longer share its state. One way to get around this is to
	 * make the listener regularly check the value of full_update --- this is
	 * probably sufficient, since the functionality is deprecated. *)
	(* This is where we add the update hook that updates the metrics classes every
	 * so often. Called with the lock held. *)
	let add_update_hook ~rrd ~timescale =
		(* Clear any existing ones *)
		debug "clearing existing update hooks";
		Array.iter (fun rra -> rra.Rrd.rra_updatehook <- None) rrd.Rrd.rrd_rras;
		(* Only allow timescales 1 and 2 - that is 5 seconds and 60 seconds respectively *)
		if timescale > 0 && timescale < 3 then begin
			debug "Timescale OK";
			let (n,ns) = List.nth timescales (timescale-1) in
			debug "(n,ns)=(%d,%d)" n ns;
			let rras = List.filter
				(fun (_,rra) -> rra.Rrd.rra_pdp_cnt=ns)
				(Array.to_list (Array.mapi (fun i x -> (i,x)) rrd.Rrd.rrd_rras)) in
			try
				debug "Found some RRAs (%d)" (List.length rras);
				(* Add the update hook to the last RRA at this timescale to be updated. That way we know that all of the
				 * RRAs will have been updated when the hook is called. Last here means two things: the last RRA of this
				 * timescale, and also that it happens to be (coincidentally) the one with the CF_LAST consolidation function.
				 * We rely on this, as well as the first one being the CF_AVERAGE one *)
				let (new_last_rra_idx, last_rra) = List.hd (List.rev rras) in
				let (new_avg_rra_idx, avg_rra) = List.hd rras in
				debug "Got rra - cf=%s row_cnt=%d pdp_cnt=%d" (Rrd.cf_type_to_string last_rra.Rrd.rra_cf) last_rra.Rrd.rra_row_cnt last_rra.Rrd.rra_pdp_cnt;
				full_update_avg_rra_idx := new_avg_rra_idx;
				full_update_last_rra_idx := new_last_rra_idx;
				(* XXX FIXME TODO: temporarily disabled full_update and condition broadcast. *)
				last_rra.Rrd.rra_updatehook <-
					Some (fun _ _ ->
						full_update := true
						(*; Condition.broadcast condition*)
					);
			with _ -> ()
		end
end
