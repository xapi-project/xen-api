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

(* The framework requires type 'context' to be defined. *)
type context = unit

open Threadext
open Rrdd_shared

module D = Debug.Debugger(struct let name="rrdd_server" end)
open D

let has_vm_rrd _ ~(vm_uuid : string) =
	Mutex.execute mutex (fun _ -> Hashtbl.mem vm_rrds vm_uuid)

let backup_rrds _ ~(master_address) ?(save_stats_locally = true) () : unit =
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
					archive_rrd ~master_address ~save_stats_locally ~uuid ~rrd
				) vrrds;
			match !host_rrd with
			| Some rrdi ->
				debug "Backup: saving RRD for host to local disk";
				let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
				archive_rrd ~master_address ~save_stats_locally ~uuid:localhost_uuid ~rrd
			| None -> ()
		end else begin
			cycles_tried := 1 + !cycles_tried;
			if !cycles_tried >= total_cycles
			then debug "Could not acquire RRD lock, skipping RRD backup"
			else Thread.delay 1.
		end
	done

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
	] else [
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
		let uri = if is_host then Constants.get_host_rrd_uri else Constants.get_vm_rrd_uri in
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
	(* Only called from dbsync in two cases:
	 * 1. For the local host after a xapi restart or host restart.
	 * 2. For running VMs after a xapi restart.
	 * Note we aren't called looking for running VMs after a host restart. We
	 * assume that the RRDs were stored locally and fall back to asking the
	 * master if we can't find them. *)
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


(* Push function to push the archived RRD to the appropriate host
 * (which might be us, in which case, pop it into the hashtbl. *)
let push_rrd _ ~(master_address : string) ~(vm_uuid : string)
		~(is_on_localhost : bool) : unit =
	try
		let path = Xapi_globs.xapi_rrd_location ^ "/" ^ vm_uuid in
		let rrd = rrd_of_gzip path in
		debug "Pushing RRD for VM uuid=%s" vm_uuid;
		if is_on_localhost then
			Mutex.execute mutex (fun () -> Hashtbl.replace vm_rrds vm_uuid {rrd=rrd; dss=[]})
		else
			(* Host might be OpaqueRef:null, in which case we'll fail silently *)
			send_rrd ~address:master_address ~to_archive:false ~uuid:vm_uuid
				~rrd:(Rrd.copy_rrd rrd) ()
	with _ -> ()

(** Remove an RRD from the local filesystem, if it exists. *)
let remove_rrd _ ~(uuid : string) : unit =
	let path = Xapi_globs.xapi_rrd_location ^ "/" ^ uuid in
	let gz_path = path ^ ".gz" in
	(try Unix.unlink path with _ -> ());
	(try Unix.unlink gz_path with _ -> ())

(* Migrate_push - used by the migrate code to push an RRD directly to
 * a remote host without going via the master. If the host is on a
 * different pool, you must pass both the remote_address and
 * session_id parameters.
 * Remote address is assumed to be valid, since it is set by monitor_master.
 *)
let migrate_rrd _ ?(session_id : string option) ~(remote_address : string)
		~(vm_uuid : string) ~(host_uuid : string) () : unit =
	try
		let rrdi = Mutex.execute mutex (fun () ->
			let rrdi = Hashtbl.find vm_rrds vm_uuid in
			debug "Sending RRD for VM uuid=%s to remote host %s for migrate"
				host_uuid vm_uuid;
			Hashtbl.remove vm_rrds vm_uuid;
			rrdi
		) in
		send_rrd ?session_id ~address:remote_address ~to_archive:false
			~uuid:vm_uuid ~rrd:rrdi.rrd ()
	with
	| Not_found ->
		debug "VM %s RRDs not found on migrate! Continuing anyway..." vm_uuid;
		log_backtrace ()
	| e ->
		(*debug "Caught exception while trying to push VM %s RRDs: %s"
			vm_uuid (ExnHelper.string_of_exn e);*)
		log_backtrace ()

(* Called on host shutdown/reboot to send the Host RRD to the master for
 * backup. Note all VMs will have been shutdown by now. *)
let send_host_rrd_to_master _ ~(master_address : string) =
	match !host_rrd with
	| Some rrdi ->
		debug "sending host RRD to master";
		let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
		archive_rrd ~master_address ~save_stats_locally:false ~uuid:localhost_uuid ~rrd
	| None -> ()

let add_ds ~rrdi ~ds_name =
	let open Ds in
	let ds = List.find (fun ds -> ds.ds_name = ds_name) rrdi.dss in
	Rrd.rrd_add_ds rrdi.rrd
		(Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)

let add_host_ds _ ~(ds_name : string) : unit =
	Mutex.execute mutex (fun () ->
		match !host_rrd with None -> () | Some rrdi ->
		let rrd = add_ds ~rrdi ~ds_name in
		host_rrd := Some {rrdi with rrd = rrd}
	)

let forget_host_ds _ ~(ds_name : string) : unit =
	Mutex.execute mutex (fun () ->
		match !host_rrd with None -> () | Some rrdi ->
		host_rrd := Some {rrdi with rrd = Rrd.rrd_remove_ds rrdi.rrd ds_name}
	)

let query_possible_dss rrdi =
	let enabled_dss = Rrd.ds_names rrdi.rrd in
	let open Ds in
	let open Data_source in
	List.map (fun ds -> {
		name = ds.ds_name;
		description = ds.ds_description;
		enabled = List.mem ds.ds_name enabled_dss;
		standard = ds.ds_default;
		min = ds.ds_min;
		max = ds.ds_max;
		units = ds.ds_units;
	}) rrdi.dss

let query_possible_host_dss _ () : Data_source.t list =
	Mutex.execute mutex (fun () ->
		match !host_rrd with None -> [] | Some rrdi -> query_possible_dss rrdi
	)

let query_host_ds _ ~(ds_name : string) : float =
	Mutex.execute mutex (fun () ->
		match !host_rrd with
		| None -> failwith "No data source!"
		| Some rrdi -> Rrd.query_named_ds rrdi.rrd ds_name Rrd.CF_Average
	)

let add_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : unit =
	Mutex.execute mutex (fun () ->
		let rrdi = Hashtbl.find vm_rrds vm_uuid in
		let rrd = add_ds rrdi ds_name in
		Hashtbl.replace vm_rrds vm_uuid {rrd = rrd; dss = rrdi.dss}
	)

let forget_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : unit =
	Mutex.execute mutex (fun () ->
		let rrdi = Hashtbl.find vm_rrds vm_uuid in
		let rrd = rrdi.rrd in
		Hashtbl.replace vm_rrds vm_uuid {rrdi with rrd = Rrd.rrd_remove_ds rrd ds_name}
	)

let query_possible_vm_dss _ ~(vm_uuid : string) : Data_source.t list =
	Mutex.execute mutex (fun () ->
		let rrdi = Hashtbl.find vm_rrds vm_uuid in
		query_possible_dss rrdi
	)

let query_vm_ds _ ~(vm_uuid : string) ~(ds_name : string) : float =
	Mutex.execute mutex (fun () ->
		let rrdi = Hashtbl.find vm_rrds vm_uuid in
		Rrd.query_named_ds rrdi.rrd ds_name Rrd.CF_Average
	)

let use_min_max = ref false

let update_use_min_max _ ~(value : bool) : unit =
	debug "Updating use_min_max: New value=%b" value;
	use_min_max := value

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


