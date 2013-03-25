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

open Fun
open Hashtblext
open Listext
open Pervasiveext
open Stringext
open Threadext
open Rrdd_shared

module D = Debug.Debugger(struct let name="rrdd_server" end)
open D

let has_vm_rrd _ ~(vm_uuid : string) =
	Mutex.execute mutex (fun _ -> Hashtbl.mem vm_rrds vm_uuid)

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
	(* DEPRECATED *)
	(* Fetch an RRD from the master *)
	let pull_rrd_from_master ~uuid ~is_host =
		let pool_secret = get_pool_secret () in
		let uri = if is_host then Constants.get_host_rrd_uri else Constants.get_vm_rrd_uri in
		(* Add in "dbsync = true" to the query to make sure the master
		 * doesn't try to redirect here! *)
		let uri = uri ^ "?uuid=" ^ uuid ^ "&dbsync=true" in
		let request =
			Http.Request.make ~user_agent:Xapi_globs.xapi_user_agent
			~cookie:["pool_secret", pool_secret] Http.Get uri in
		let open Xmlrpc_client in
		let master_address = Pool_role_shared.get_master_address () in
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
	(* This used to be called from dbsync in two cases:
	 * 1. For the local host after a xapi restart or host restart.
	 * 2. For running VMs after a xapi restart.
	 * It is now only used to load the host's RRD after xapi restart. *)
	let load_rrd _ ~(uuid : string) ~(domid : int) ~(is_host : bool)
			~(timescale : int) () : unit =
		try
			let rrd =
				try
					let rrd = load_rrd_from_local_filesystem uuid in
					debug "RRD loaded from local filesystem for object uuid=%s" uuid;
					rrd
				with e ->
					if Pool_role_shared.is_master () then begin
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
					host_rrd := Some {rrd; dss = []; domid}
				end else
					Hashtbl.replace vm_rrds uuid {rrd; dss = []; domid}
			)
		with _ -> ()
end

(* Push function to push the archived RRD to the appropriate host
 * (which might be us, in which case, pop it into the hashtbl. *)
let push_rrd _ ~(vm_uuid : string) ~(domid : int) ~(is_on_localhost : bool) ()
		: unit =
	try
		let path = Xapi_globs.xapi_rrd_location ^ "/" ^ vm_uuid in
		let rrd = rrd_of_gzip path in
		debug "Pushing RRD for VM uuid=%s" vm_uuid;
		if is_on_localhost then
			Mutex.execute mutex (fun _ ->
				Hashtbl.replace vm_rrds vm_uuid {rrd; dss=[]; domid}
			)
		else
			(* Host might be OpaqueRef:null, in which case we'll fail silently *)
			let address = Pool_role_shared.get_master_address () in
			send_rrd ~address ~to_archive:false ~uuid:vm_uuid
				~rrd:(Rrd.copy_rrd rrd) ()
	with _ -> ()

(** Remove an RRD from the local filesystem, if it exists. *)
let remove_rrd _ ~(uuid : string) () : unit =
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

let add_vm_ds _ ~(vm_uuid : string) ~(domid : int) ~(ds_name : string) : unit =
	Mutex.execute mutex (fun () ->
		let rrdi = Hashtbl.find vm_rrds vm_uuid in
		let rrd = add_ds rrdi ds_name in
		Hashtbl.replace vm_rrds vm_uuid {rrd; dss = rrdi.dss; domid}
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

let update_use_min_max _ ~(value : bool) () : unit =
	debug "Updating use_min_max: New value=%b" value;
	use_min_max := value

let string_of_domain_handle dh =
	Uuid.string_of_uuid (Uuid.uuid_of_int_array dh.Xenctrl.Domain_info.handle)

let update_vm_memory_target _ ~(domid : int) ~(target : int64) : unit =
	Mutex.execute memory_targets_m
		(fun _ -> Hashtbl.replace memory_targets domid target)

let set_cache_sr _ ~(sr_uuid : string) () : unit =
	Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := Some sr_uuid)

let unset_cache_sr _ () =
	Mutex.execute cache_sr_lock (fun () -> cache_sr_uuid := None)

module Plugin = struct
	exception Invalid_header_string
	exception Invalid_length
	exception Invalid_checksum
	exception Invalid_payload
	exception No_update
	exception Read_error

	(* Static values. *)
	let base_path = "/dev/shm/metrics/"
	let header = "DATASOURCES\n"
	let header_bytes = String.length header
	let length_bytes = 8 (* hex length of payload *) + 1 (* newline char *)
	let checksum_bytes = 32 (* hex length of checksum *) + 1 (* newline char *)

	(* Cache of open file handles to files written by plugins. *)
	let open_files : (string, Unix.file_descr) Hashtbl.t = Hashtbl.create 10

	(* A function that opens files using the above cache. *)
	let open_file ~(path : string) : Unix.file_descr =
		try Hashtbl.find open_files path
		with Not_found ->
			let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
			Hashtbl.add open_files path fd;
			fd

	(* A function that reads using Unixext.really_read a string of specified
	 * length from the specified file. *)
	let read_string ~(fd : Unix.file_descr) ~(length : int) : string =
		try
			(* CA-92154: use Unixext.really_read since Unix.read will
			 * not read a string longer than 16384 bytes *)
			Unixext.really_read_string fd length
		with _ ->
			log_backtrace ();
			raise Read_error

	(* The payload type that corresponds to the plugin output file format. *)
	type payload = {
		timestamp : int;
		datasources : (Rrd.ds_owner * Ds.ds) list;
	}

	(* A helper function for extracting the dictionary out of the RPC type. *)
	let dict_of_rpc ~(rpc : Rpc.t) : (string * Rpc.t) list =
		match rpc with Rpc.Dict d -> d | _ -> raise Invalid_payload

	(* A helper function for extracting the enum/list out of the RPC type. *)
	let list_of_rpc ~(rpc : Rpc.t) : Rpc.t list =
		match rpc with Rpc.Enum l -> l | _ -> raise Invalid_payload

	(* [assoc_opt ~key ~default l] gets string value associated with [key] in
	 * [l], returning [default] if no mapping is found. *)
	let assoc_opt ~(key : string) ~(default : string)
			(l : (string * Rpc.t) list) : string =
		try Rpc.string_of_rpc (List.assoc key l) with
		| Not_found -> default
		| e -> error "Failed to obtain datasource key: %s" key; raise e

	(* Converts string to the corresponding datasource type. *)
	let ds_ty_of_string (s : string) : Rrd.ds_type =
		match String.lowercase s with
		| "absolute" -> Rrd.Gauge
		| "rate" -> Rrd.Absolute
		| "absolute_to_rate" -> Rrd.Derive
		| _ -> raise Invalid_payload

	(* Possible types for values in datasources. *)
	type value_type = Float | Int64

	(* Converts string to datasource value type. *)
	let val_ty_of_string (s : string) : value_type =
		match String.lowercase s with
		| "float" -> Float
		| "int64" -> Int64
		| _ -> raise Invalid_payload

	(* Converts an RPC value to a typed datasource value. *)
	let ds_value_of_rpc ~(ty : value_type) ~(rpc : Rpc.t) : Rrd.ds_value_type =
		match ty with
		| Float -> Rrd.VT_Float (Rpc.float_of_rpc rpc)
		| Int64 -> Rrd.VT_Int64 (Rpc.int64_of_rpc rpc)

	(* Converts a string to value of datasource owner type. *)
	let owner_of_string (s : string) : Rrd.ds_owner =
		match String.split ' ' (String.lowercase s) with
		| ["host"] -> Rrd.Host
		| ["vm"; uuid] -> Rrd.VM uuid
		| ["sr"; uuid] -> Rrd.SR uuid
		| _ -> raise Invalid_payload

	(* A function that converts a JSON type into a datasource type, assigning
	 * default values appropriately. *)
	let ds_of_rpc ((name, rpc) : (string * Rpc.t)) : (Rrd.ds_owner * Ds.ds) =
		try
			let open Rpc in
			let kvs = dict_of_rpc ~rpc in
			let description = assoc_opt ~key:"description" ~default:"" kvs in
			let units = assoc_opt ~key:"units" ~default:"" kvs in
			let ty =
				ds_ty_of_string (assoc_opt ~key:"type" ~default:"absolute" kvs) in
			let val_ty =
				val_ty_of_string (assoc_opt ~key:"value_type" ~default:"float" kvs) in
			let value =
				let value_rpc = List.assoc "value" kvs in
				ds_value_of_rpc ~ty:val_ty ~rpc:value_rpc
			in
			let min =
				float_of_string (assoc_opt ~key:"min" ~default:"-infinity" kvs) in
			let max =
				float_of_string (assoc_opt ~key:"max" ~default:"infinity" kvs) in
			let owner =
				owner_of_string (assoc_opt ~key:"owner" ~default:"host" kvs) in
			let ds = Ds.ds_make ~name ~description ~units ~ty ~value ~min ~max
				~default:true () in
			owner, ds
		with e ->
			error "Failed to process datasource: %s" name;
			log_backtrace (); raise e

	(* A function that parses the payload written by a plugin into the payload
	 * type. *)
	let parse_payload ~(json : string) : payload =
		try
			let open Rpc in
			let rpc = Jsonrpc.of_string json in
			let kvs = dict_of_rpc ~rpc in
			let timestamp = int_of_rpc (List.assoc "timestamp" kvs) in
			let datasource_rpcs = dict_of_rpc (List.assoc "datasources" kvs) in
			{timestamp; datasources = List.map ds_of_rpc datasource_rpcs}
		with _ -> log_backtrace (); raise Invalid_payload

	(* A map storing the last read checksum of the file written by each plugin. *)
	let last_read_checksum : (string, string) Hashtbl.t =
		Hashtbl.create 20

	(* Throw No_update exception if previous checksum is the same as the current
	 * one for this plugin. Otherwise, replace previous with current.*)
	let verify_checksum_freshness ~(uid : string) ~(checksum : string) : unit =
		try
			if checksum = Hashtbl.find last_read_checksum uid then raise No_update
		with Not_found -> ();
		Hashtbl.replace last_read_checksum uid checksum

	(* The function that reads the file that corresponds to the plugin with the
	 * specified uid, and returns the contents of the file in terms of the
	 * payload type, or throws an exception. *)
	let read_file (uid : string) : payload =
		try
			let path = Filename.concat base_path uid in
			let fd = open_file ~path in
			if Unix.lseek fd 0 Unix.SEEK_SET <> 0 then
				raise Read_error;
			if read_string ~fd ~length:header_bytes <> header then
				raise Invalid_header_string;
			let length =
				let length_str = String.rtrim (read_string ~fd ~length:length_bytes) in
				try int_of_string ("0x" ^ length_str) with _ -> raise Invalid_length
			in
			let checksum = String.rtrim (read_string ~fd ~length:checksum_bytes) in
			let payload = read_string ~fd ~length in
			if payload |> Digest.string |> Digest.to_hex <> checksum then
				raise Invalid_checksum;
			verify_checksum_freshness ~uid ~checksum;
			parse_payload ~json:payload
		with e ->
			error "Failed to process plugin: %s" uid;
			log_backtrace ();
			match e with
			| Invalid_header_string | Invalid_length | Invalid_checksum
			| No_update as e -> raise e
			| _ -> raise Read_error

	(* The function that tells the plugin what to write at the top of its output
	 * file. *)
	let get_header _ () : string = header

	(* The function that a plugin can use to determine which file to write to. *)
	let get_path _ ~(uid : string) : string =
		Filename.concat base_path uid

	(* A map storing currently registered plugins, and their sampling
	 * frequencies. *)
	let registered : (string, Rrd.sampling_frequency) Hashtbl.t =
		Hashtbl.create 20

	(* The mutex that protects the list of registered plugins against race
	 * conditions and data corruption. *)
	let registered_m : Mutex.t = Mutex.create ()

	(* Returns the number of seconds until the next reading phase for the
	 * sampling frequency given at registration by the plugin with the specified
	 * unique ID. If the plugin is not registered, -1 is returned. *)
	let next_reading _ ~(uid : string) : float =
		let open Rrdd_shared in
		if Mutex.execute registered_m (fun _ -> Hashtbl.mem registered uid)
		then Mutex.execute last_loop_end_time_m (fun _ ->
			!last_loop_end_time +. !timeslice -. (Unix.gettimeofday ())
		)
		else -1.

	(* The function registers a plugin, and returns the number of seconds until
	 * the next reading phase for the specified sampling frequency. *)
	let register _ ~(uid : string) ~(frequency : Rrd.sampling_frequency)
			: float =
		Mutex.execute registered_m (fun _ ->
			if not (Hashtbl.mem registered uid) then
				Hashtbl.add registered uid frequency
		);
		next_reading ~uid ()

	(* The function deregisters a plugin. After this call, the framework will
	 * process its output file at most once more. *)
	let deregister _ ~(uid : string) : unit =
		Mutex.execute registered_m (fun _ ->
			Hashtbl.remove registered uid
		)

	(* Read, parse, and combine metrics from all registered plugins. *)
	let read_stats () : (Rrd.ds_owner * Ds.ds) list =
		let uids =
			Mutex.execute registered_m (fun _ -> Hashtbl.fold_keys registered) in
		let process_plugin acc uid =
			try
				let payload = read_file uid in
				List.rev_append payload.datasources acc
			with _ -> acc
		in
		List.fold_left process_plugin [] uids
end

module HA = struct
	let enable_and_update _ ~(statefile_latencies : Rrd.Statefile_latency.t list)
			~(heartbeat_latency : float) ~(xapi_latency : float) () =
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
