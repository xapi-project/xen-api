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

open Listext
open Pervasiveext
open Stringext
open Threadext
open Rrdd_shared

module D = Debug.Debugger(struct let name="rrdd_server" end)
open D

let has_vm_rrd _ ~(vm_uuid : string) =
	Mutex.execute mutex (fun _ -> Hashtbl.mem vm_rrds vm_uuid)

let set_master _ ~(is_master : bool) ~(master_address : string) : unit =
	Rrdd_shared.is_master := is_master;
	Rrdd_shared.master_address := master_address

let backup_rrds _ ?(save_stats_locally = true) () : unit =
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
					archive_rrd ~save_stats_locally ~uuid ~rrd ()
				) vrrds;
			match !host_rrd with
			| Some rrdi ->
				debug "Backup: saving RRD for host to local disk";
				let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
				archive_rrd ~save_stats_locally ~uuid:localhost_uuid ~rrd ()
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

module Deprecated = struct
	let get_full_update_avg_rra_idx _ () : int = !Deprecated.full_update_avg_rra_idx
	let get_full_update_last_rra_idx _ () : int = !Deprecated.full_update_last_rra_idx

	(* DEPRECATED *)
	(* Fetch an RRD from the master *)
	let pull_rrd_from_master ~uuid ~is_host =
		let pool_secret = !Xapi_globs.pool_secret in
		let uri = if is_host then Constants.get_host_rrd_uri else Constants.get_vm_rrd_uri in
		(* Add in "dbsync = true" to the query to make sure the master
		 * doesn't try to redirect here! *)
		let uri = uri ^ "?uuid=" ^ uuid ^ "&dbsync=true" in
		let request =
			Http.Request.make ~user_agent:Xapi_globs.xapi_user_agent
			~cookie:["pool_secret", pool_secret] Http.Get uri in
		let open Xmlrpc_client in
		let transport = SSL(SSL.make (), !master_address, !Xapi_globs.https_port) in
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
	(* Only called from dbsync in two cases:
	 * 1. For the local host after a xapi restart or host restart.
	 * 2. For running VMs after a xapi restart.
	 * Note we aren't called looking for running VMs after a host restart. We
	 * assume that the RRDs were stored locally and fall back to asking the
	 * master if we can't find them. *)
	let load_rrd _ ~(uuid : string) ~(is_host : bool) ~(timescale : int) () : unit =
		try
			let rrd =
				try
					let rrd = load_rrd_from_local_filesystem uuid in
					debug "RRD loaded from local filesystem for object uuid=%s" uuid;
					rrd
				with e ->
					if !is_master then begin
						info "Failed to load RRD from local filesystem: metrics not available for uuid=%s" uuid;
						raise e
					end else begin
						debug "Failed to load RRD from local filesystem for object uuid=%s; asking master" uuid;
						try
							let rrd = pull_rrd_from_master ~uuid ~is_host in
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
					Deprecated.add_update_hook ~rrd ~timescale;
					host_rrd := Some {rrd; dss = []}
				end else Hashtbl.replace vm_rrds uuid {rrd; dss = []}
			)
		with _ -> ()

	(* DEPRECATED *)
	(* let get_host_rrd _ () = None *)
	let i64_of_float f =
		match classify_float f with
		| FP_nan -> 0L
		| FP_infinite -> 0L
		| _ -> Int64.of_float f

	let get_host_stats _ () =
		let open Monitor_types in
		let avg_rra_idx = !Deprecated.full_update_avg_rra_idx in
		let last_rra_idx = !Deprecated.full_update_last_rra_idx in
		(* The following function constructs a bolus of data from the shared
		 * rrds in order to pass it over the wire to the master. There
		 * shouldn't be anything here that will take time to process, and by
		 * the end of the function all of the dirty bits should be marked as
		 * clean. Whatever happens, we need to unlock the mutex at the end of
		 * this! *)
		let host_stats =
			finally (fun _ ->
				(* We go through the vm rrds and regenerate a host_stats structure *)
				let rrd = match !host_rrd with Some r -> r.rrd | None -> failwith "No host rrd!" in
				let timestamp = rrd.Rrd.last_updated in
				let total_kib, free_kib, pcpus, pifs =
					match !host_rrd with
					| None -> (0L, 0L, {pcpus_usage = [||]}, [])
					| Some rrdi ->
						let rrd = rrdi.rrd in
						let values = Rrd.get_last_values rrd avg_rra_idx in
						let lastvalues = Rrd.get_last_values rrd last_rra_idx in
						let total_kib = try i64_of_float (List.assoc "memory_total_kib" lastvalues) with _ -> 0L in
						let free_kib = try i64_of_float (List.assoc "memory_free_kib" lastvalues) with _ -> 0L in
						let cpus = List.sort (fun (c1, _) (c2, _) ->
							let n1 = Scanf.sscanf c1 "cpu%d" (fun n -> n) in
							let n2 = Scanf.sscanf c2 "cpu%d" (fun n -> n) in
							compare n1 n2) (List.filter (fun (s, _) -> String.startswith "cpu" s) values)
						in
						let cpus = Array.of_list (List.map (fun (n, v) -> v) cpus) in
						let pifs = List.filter (fun (n1, _) -> String.startswith "pif_" n1) values in
						let pif_names = List.setify (List.map (fun (s, _) -> List.nth (String.split '_' s) 1) pifs) in
						let pifs = List.filter_map (fun name ->
							try
								let pif_stats = List.find (fun pif -> pif.pif_name = name) !pif_stats in
								Some {pif_stats with
									pif_tx = List.assoc ("pif_" ^ name ^ "_tx") values;
									pif_rx = List.assoc ("pif_" ^ name ^ "_rx") values;
								}
							with _ -> None) pif_names
						in
						(total_kib, free_kib, {pcpus_usage = cpus}, pifs)
				in
				let kvs = Hashtbl.fold (fun k v acc -> (k, Rrd.get_last_values v.rrd avg_rra_idx)::acc) vm_rrds [] in
				let vbdss = List.map (
					fun (uuid, values) ->
						let vbds = List.filter (fun (s, _) -> String.startswith "vbd_" s) values in
						let vbds_devices = List.setify (List.map (fun (s, _) -> List.nth (String.split '_' s) 1) vbds) in
						let vbds = List.filter_map (fun device ->
							try
								(* NB we only get stats from PV devices *)
								let device_id = Device_number.to_xenstore_key (Device_number.of_string false device) in
								let read = List.assoc ("vbd_" ^ device ^ "_read") values in
								let write = List.assoc ("vbd_" ^ device ^ "_write") values in
								Some (uuid, {
									vbd_device_id = device_id;
									vbd_io_read = read;
									vbd_io_write = write;
									vbd_raw_io_read = 0L;
									vbd_raw_io_write = 0L
								})
							with _ ->
								error "Bizarre error in monitor_rrds.ml - key dump: ";
								List.iter (fun (k, v) -> error "key: %s" k) vbds;
								None
						) vbds_devices in
						vbds
				) kvs in
				let vbds = List.flatten vbdss in
				let vifss = List.map (fun (uuid, values) ->
					let vifs = List.filter (fun (s, _) -> String.startswith "vif_" s) values in
					let vif_devices = List.setify (List.map (fun (s, _) -> List.nth (String.split '_' s) 1) vifs) in
					let vifs = List.map (fun device ->
						let dev = int_of_string device in
						let name="" in
						let tx = List.assoc ("vif_" ^ device ^ "_tx") values in
						let rx = List.assoc ("vif_" ^ device ^ "_rx") values in
						(uuid, {
							vif_n = dev;
							vif_name = name;
							vif_tx = tx;
							vif_rx = rx;
							vif_raw_tx = 0L;
							vif_raw_rx = 0L;
							vif_raw_tx_err = 0L;
							vif_raw_rx_err = 0L;
						})
					) vif_devices in
					vifs
				) kvs in
				let vifs = List.flatten vifss in
				let vcpus = List.map (fun (uuid, values) ->
					let vcpus = List.filter (fun (s, _) -> String.startswith "cpu" s) values in
					let cpunums = List.map (fun (s, v) -> (Scanf.sscanf s "cpu%d" (fun x -> x), v)) vcpus in
					(uuid, {
						vcpu_sumcpus = 0.0;
						vcpu_vcpus = Array.of_list (List.map snd (List.sort (fun a b -> compare (fst a) (fst b)) cpunums));
						vcpu_rawvcpus = [||];
						vcpu_cputime = 0L
					})
				) kvs in
				let mem = [] in (* This gets updated below instead *)
				let registered = List.map fst kvs in
				(* Now clear all the dirty bits *)
				dirty_memory := StringSet.empty;
				dirty_pifs := StringSet.empty;
				Deprecated.full_update := false;
				dirty_host_memory := false;
				{
					host_ref = Ref.null;
					timestamp;
					total_kib;
					free_kib;
					pifs;
					pcpus;
					vbds;
					vifs;
					vcpus;
					mem;
					registered;
				}
			) (fun _ -> Mutex.unlock mutex)
		in
		ignore (host_stats)
		(*
		(* This is the bit that might block for some time, but by now we've released the mutex *)
		Server_helpers.exec_with_new_task "updating host stats" (fun __context ->
			let host = Helpers.get_localhost ~__context in
			let host_stats = {host_stats with host_ref = host} in
			if Pool_role.is_master () then
				Monitor_master.update_all ~__context host_stats (* read stats locally and update locally *)
			else
				let host_stats_s = Xml.to_string_fmt (Monitor_transfer.marshall host_stats) in
				ignore (Master_connection.execute_remote_fn host_stats_s Constants.remote_stats_uri)
		)
		*)
end


(* Push function to push the archived RRD to the appropriate host
 * (which might be us, in which case, pop it into the hashtbl. *)
let push_rrd _ ~(vm_uuid : string) ~(is_on_localhost : bool) : unit =
	try
		let path = Xapi_globs.xapi_rrd_location ^ "/" ^ vm_uuid in
		let rrd = rrd_of_gzip path in
		debug "Pushing RRD for VM uuid=%s" vm_uuid;
		if is_on_localhost then
			Mutex.execute mutex (fun () -> Hashtbl.replace vm_rrds vm_uuid {rrd=rrd; dss=[]})
		else
			(* Host might be OpaqueRef:null, in which case we'll fail silently *)
			send_rrd ~address:!master_address ~to_archive:false ~uuid:vm_uuid
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
let send_host_rrd_to_master _ () =
	match !host_rrd with
	| Some rrdi ->
		debug "sending host RRD to master";
		let rrd = Mutex.execute mutex (fun () -> Rrd.copy_rrd rrdi.rrd) in
		archive_rrd ~save_stats_locally:false ~uuid:localhost_uuid ~rrd ()
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

let update_use_min_max _ ~(value : bool) : unit =
	debug "Updating use_min_max: New value=%b" value;
	use_min_max := value

let string_of_domain_handle dh =
	Uuid.string_of_uuid (Uuid.uuid_of_int_array dh.Xenctrl.Domain_info.handle)

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
	let dis_uncoop = List.filter (fun di -> List.mem di.Xenctrl.Domain_info.domid domids) dis in
	List.map string_of_domain_handle dis_uncoop

let update_vm_memory_target _ ~(domid : int) ~(target : int64) : unit =
	Mutex.execute memory_targets_m
		(fun _ -> Hashtbl.replace memory_targets domid target)

let set_cache_sr _ ~(sr_uuid : string) : unit =
	Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := Some sr_uuid)

let unset_cache_sr _ () =
	Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := None)



module HA = struct
	let enable_and_update _ ~(statefile_latencies : Rrd.Statefile_latency.t list)
			~(heartbeat_latency : float) ~(xapi_latency : float) =
		Mutex.execute Rrdd_ha_stats.m (fun _ ->
			Rrdd_ha_stats.enabled := true;
			Rrdd_ha_stats.Statefile_latency.all := statefile_latencies;
			Rrdd_ha_stats.Heartbeat_latency.raw := Some heartbeat_latency;
			Rrdd_ha_stats.Xapi_latency.raw      := Some xapi_latency
		)

	let disable _ () =
		Mutex.execute Rrdd_ha_stats.m (fun _ ->
			Rrdd_ha_stats.enabled := false
		)
end
