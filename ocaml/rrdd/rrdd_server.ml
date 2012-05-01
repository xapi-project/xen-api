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

(* The framework requires type 'context' to be defined. *)
type context = unit

open Threadext

module D = Debug.Debugger(struct let name="rrdd_server" end)
open D

let mutex = Mutex.create ()

type rrd_info = {
  rrd: Rrd.rrd;
  mutable dss: Ds.ds list;
}

(* RRDs *)
let vm_rrds : (string, rrd_info) Hashtbl.t = Hashtbl.create 32
let host_rrd : rrd_info option ref = ref None

(** Send rrds to a remote host. If the host is on another pool, you
    must pass the session_id parameter, and optionally the __context. *)
let send_rrd ?session_id address to_archive uuid rrd = ()
(* FIXME: Surely this should not be sending to xapi?! *)
(*
	debug "Sending RRD for object uuid=%s archiving=%b to address: %s"
		uuid to_archive address;
	let arch_query = if to_archive then ["archive", "true"] else [] in
	let sid_query = match session_id with
		| None -> [] | Some id -> ["session_id", Ref.string_of id] in
	let query = sid_query @ arch_query @ ["uuid", uuid] in
	let cookie =
		if sid_query = [] then ["pool_secret", !Xapi_globs.pool_secret] else [] in
	let request =
		Xapi_http.http_request ~query ~cookie Http.Put Constants.rrd_put_uri in
	let open Xmlrpc_client in
	let transport = SSL(SSL.make (), address, !Xapi_globs.https_port) in
	with_transport transport (
		with_http request (fun (response, fd) ->
			try Rrd.to_fd rrd fd
			with e ->
				debug "Caught exception: %s" (ExnHelper.string_of_exn e);
				log_backtrace ()
		)
	)
*)

let archive_rrd ~master_address ~save_stats_locally ~uuid rrd =
	debug "Archiving RRD for object uuid=%s %s" uuid (if save_stats_locally then "to local disk" else "to remote master");
	if save_stats_locally then begin
		try
			(* Stash away the rrd onto disk *)
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
				Unixext.unlink_safe base_filename (* If there's an uncompressed one hanging around, remove it *)
			end else begin
				debug "No local storage: not persisting RRDs"
			end
		with e ->
			(*debug "Caught exception: %s" (ExnHelper.string_of_exn e);*)
			log_backtrace();
	end else begin
		debug "About to get address";
		(* Stream it to the master to store, or maybe to a host in the migrate case *)
		(*let master_address = Pool_role.get_master_address () in*)
		debug "About to send";
		send_rrd master_address true uuid rrd
	end

let backup_rrds _ ~(master_address) ?(save_stats_locally = true)
		~(localhost_uuid : string) () : unit =
	debug "backup safe_stats_locally=%b" save_stats_locally;
	let total_cycles = 5 in
	let cycles_tried = ref 0 in
	while !cycles_tried < total_cycles do
		if Mutex.try_lock mutex then begin
			cycles_tried := total_cycles;
			let vrrds =
				try
					Hashtbl.fold (fun k v acc -> (k,v.rrd)::acc) vm_rrds []
				with exn ->
					Mutex.unlock mutex;
					raise exn
			in
			Mutex.unlock mutex;
			List.iter
				(fun (uuid, rrd) ->
					debug "Backup: saving RRD for VM uuid=%s to local disk" uuid;
					let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrd) in
					archive_rrd ~master_address ~save_stats_locally ~uuid rrd
				) vrrds;
			match !host_rrd with
			| Some rrdi ->
				debug "Backup: saving RRD for host to local disk";
				let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
				archive_rrd ~master_address ~save_stats_locally ~uuid:localhost_uuid rrd
			| None -> ()
		end else begin
			cycles_tried := 1 + !cycles_tried;
			if !cycles_tried >= total_cycles
			then debug "Could not acquire RRD lock, skipping RRD backup"
			else Thread.delay 1.
		end
	done

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

(* Load an RRD from the local filesystem. Will return an RRD or throw an exception. *)
let load_rrd_from_local_filesystem uuid =
	debug "Loading RRD from local filesystem for object uuid=%s" uuid;
	let path = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
	rrd_of_gzip path


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
	] else[
		(120,     1); (* 120 values of interval 1 step (5 secs) = 10 mins  *)
		(120,    12); (* 120 values of interval 12 steps (1 min) = 2 hours *)
		(168,   720); (* 168 values of interval 720 steps (1 hr) = 1 week  *)
		(366, 17280); (* 366 values of interval 17280 steps (1 day) = 1 yr *)
	]

module Deprecated = struct
	let full_update : bool ref = ref false
	let full_update_avg_rra_idx : int ref = ref (-1)
	let full_update_last_rra_idx : int ref = ref (-1)

	let get_full_update_avg_rra_idx _ () : int = !full_update_avg_rra_idx
	let get_full_update_last_rra_idx _ () : int = !full_update_last_rra_idx

	(* DEPRECATED *)
	(* Fetch an RRD from the master *)
	let pull_rrd_from_master ~master_address ~uuid ~is_host =
		let pool_secret = !Xapi_globs.pool_secret in
		let uri = if is_host then Constants.host_rrd_uri else Constants.vm_rrd_uri in
		(* Add in "dbsync = true" to the query to make sure the master
		 * doesn't try to redirect here! *)
		let uri = uri ^ "?uuid=" ^ uuid ^ "&dbsync=true" in
		let request =
			Http.Request.make ~user_agent:Xapi_globs.xapi_user_agent
			~cookie:["pool_secret", pool_secret] Http.Get uri in
		let open Xmlrpc_client in
		let transport = SSL(SSL.make (), master_address, !Xapi_globs.https_port) in
		with_transport transport (
			with_http request (fun (response, s) ->
				match response.Http.Response.content_length with
				| None -> failwith "pull_rrd_from_master needs a content-length"
				| Some l ->
					let body = Unixext.really_read_string s (Int64.to_int l) in
					let input = Xmlm.make_input (`String (0, body)) in
					debug "Pulled rrd for uuid=%s" uuid;
					Rrd.from_xml input
			)
		)

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

	(* DEPRECATED *)
	(** Only called from dbsync in two cases:
	    1. for the local host after a xapi restart or host restart
	    2. for running VMs after a xapi restart
	    Note we aren't called looking for running VMs after a host restart.
	    We assume that the RRDs were stored locally and fall back to asking the master if we can't find them. *)
	let load_rrd _ ~(master_address : string) ~(is_master : bool)
			~(uuid : string) ~(is_host : bool) ~(timescale : int) () : unit =
		try
			let rrd =
				try
					let rrd = load_rrd_from_local_filesystem uuid in
					debug "RRD loaded from local filesystem for object uuid=%s" uuid;
					rrd
				with e ->
					if is_master then begin
						info "Failed to load RRD from local filesystem: metrics not available for uuid=%s" uuid;
						raise e
					end else begin
						debug "Failed to load RRD from local filesystem for object uuid=%s; asking master" uuid;
						try
							let rrd = pull_rrd_from_master ~master_address ~uuid ~is_host in
							debug "RRD pulled from master for object uuid=%s" uuid;
							rrd
						with e ->
							info "Failed to fetch RRD from master: metrics not available for uuid=%s" uuid;
							raise e
					end
			in
			Mutex.execute mutex (fun () ->
				if is_host
				then begin
					add_update_hook ~rrd ~timescale;
					host_rrd := Some {rrd=rrd; dss=[]}
				end else Hashtbl.replace vm_rrds uuid {rrd=rrd; dss=[]}
			)
		with _ -> ()
end


let push_rrd _ ~(vm_uuid : string) : unit = ()
(* Monitor_rrds.push_rrd *)

let remove_rrd _ ~(vm_uuid : string) : unit = ()
(* Monitor_rrds.maybe_remove_rrd *)

let migrate_rrd _ ?(session_id : string option) ~(remote_address : string) ~(vm_uuid : string) ~(host_uuid : string) () : unit = ()
(* Monitor_rrds.migrate_push *)

let send_host_rrd_to_master _ () = ()
(* Monitor_rrds.send_host_rrd_to_master *)


let add_host_ds _ ~(ds_name : string) : unit =
	()
	(* Monitor_rrds.add_host_ds *)

let forget_host_ds _ ~(ds_name : string) : unit =
	()
	(* Monitor_rrds.forget_host_ds *)

let query_possible_host_dss _ () : Data_source.t list =
	[]
	(* Monitor_rrds.query_possible_host_dss *)

let query_host_ds _ ~(ds_name : string) : float =
	-1.0
	(* Monitor_rrds.query_host_dss *)


let add_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : unit =
	()
	(* Monitor_rrds.add_vm_ds *)

let forget_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : unit =
	()
	(* Monitor_rrds.forget_vm_ds *)

let query_possible_vm_dss _ ~(vm_uuid : string) : Data_source.t list =
	[]
	(* Monitor_rrds.query_possible_vm_dss *)

let query_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : float =
	-1.0
	(* Monitor_rrds.query_vm_dss *)


let update_use_min_max _ ~(value : bool) : unit =
	()
(*
    debug "Updating use_min_max: New value=%b" new_use_min_max;
    use_min_max := new_use_min_max;
*)

(* Handle uncooperative domains. *)
let uncooperative_domains: (int, unit) Hashtbl.t = Hashtbl.create 20
let uncooperative_domains_m = Mutex.create ()

let string_of_domain_handle dh =
  Uuid.string_of_uuid (Uuid.uuid_of_int_array dh.Xenctrl.handle)

let add_to_uncooperative_domains _ ~(domid : int) : unit =
	Mutex.execute uncooperative_domains_m
		(fun _ -> Hashtbl.replace uncooperative_domains domid ())

let remove_from_uncooperative_domains _ ~(domid : int) : unit =
	Mutex.execute uncooperative_domains_m
		(fun _ -> Hashtbl.remove uncooperative_domains domid)

let is_domain_cooperative _ ~(domid : int) : bool =
	Mutex.execute uncooperative_domains_m
		(fun _ -> not (Hashtbl.mem uncooperative_domains domid))

let get_uncooperative_domains _ () : string list =
	let domids = Mutex.execute uncooperative_domains_m
		(fun () -> Hashtbl.fold (fun domid _ acc -> domid::acc) uncooperative_domains []) in
	let dis = Xenctrl.with_intf (fun xc -> Xenctrl.domain_getinfolist xc 0) in
	let dis_uncoop = List.filter (fun di -> List.mem di.Xenctrl.domid domids) dis in
	List.map string_of_domain_handle dis_uncoop

(** Cache memory/target values *)
let memory_targets : (int, int64) Hashtbl.t = Hashtbl.create 20
let memory_targets_m = Mutex.create ()

let update_vm_memory_target _ ~(domid : int) ~(target : int64) : unit =
	Mutex.execute memory_targets_m
		(fun _ -> Hashtbl.replace memory_targets domid target)

let cache_sr_uuid = ref None
let cache_sr_lock = Mutex.create ()

let set_cache_sr _ ~(sr_uuid : string) : unit =
	Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := Some sr_uuid)

let unset_cache_sr _ () =
	Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := None)


