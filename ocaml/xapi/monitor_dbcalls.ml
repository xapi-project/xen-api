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
(** Monitor DB calls
 * @group Performance Monitoring
 *)

(** Here is where all of the calls that touch the database live. There
 * is a thread that sits waiting for the monitor_rrd module (or
 * xapi_pif module) to wake it up so that it writes into the
 * database. We don't particularly care if the db calls block for a
 * while as if the master isn't responding, your problems are larger
 * than having an out-of-date value for the host's memory.
 *
 * The key thing in this module is to know when to push data from the
 * cache into the database. This is usually triggered by edge detection
 * code in monitor_rrds which notices when a value has changed between
 * the last call and the current one. This falls apart when e.g. we've
 * gathered PIF info for an interface that isn't in the database - we
 * try try to update the database with the information and the object
 * isn't there. We don't try to re-issue the db call, so if the PIF is
 * later introduced, it will never get updated with the cached
 * information. In this particular case, we fix this by forcing an
 * update from xapi_pif.ml - but it's important to keep this in mind
 * because it will be quite easy for this code to get out of sync.
 *)

module D = Debug.Debugger(struct let name = "monitor_dbcalls" end)
open D

open Monitor_types
open Stringext
open Listext
open Db_filter_types

let i64_of_float f =
	match classify_float f with
	| FP_nan -> 0L
	| FP_infinite -> 0L
	| _ -> Int64.of_float f

(** Called with the Rrd_shared mutex held *)
(* This is the legacy update function that constructs a bunch of xml and
 * copies it to the master to update all of the metrics objects. Not used
 * by default. *)
let full_update_fn () =
	()
(*
	let avg_rra_idx = Rrdd.Deprecated.get_full_update_avg_rra_idx () in
	let last_rra_idx = Rrdd.Deprecated.get_full_update_last_rra_idx () in
	(* The following function constructs a bolus of data from the shared
	 * rrds in order to pass it over the wire to the master. There
	 * shouldn't be anything here that will take time to process, and by
	 * the end of the function all of the dirty bits should be marked as
	 * clean. Whatever happens, we need to unlock the mutex at the end of
	 * this! *)
	let host_stats =
		Pervasiveext.finally (fun _ ->
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
							let pif_stats = List.find (fun pif -> pif.pif_name=name) !pif_stats in
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
			full_update := false;
			dirty_host_memory := false;
			{
				timestamp = timestamp;
				host_ref = Ref.null;
				total_kib = total_kib;
				free_kib = free_kib;
				pifs = pifs;
				pcpus = pcpus;
				vbds = vbds;
				vifs = vifs;
				vcpus = vcpus;
				mem = mem;
				registered = registered;
			}
		) (fun _ -> Mutex.unlock mutex)
	in
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
(* End of legacy function *)


module StringSet = Set.Make(String)

let pifs_cached : Monitor_types.pif list ref = ref []
let vm_memory_cached : (string * Int64.t) list ref = ref []
let host_memory_free_cached : Int64.t ref = ref Int64.zero
let host_memory_total_cached : Int64.t ref = ref Int64.zero

let changed_pifs_names = ref StringSet.empty
let changed_vm_memory : (string * Int64.t) list ref = ref []
let host_memory_changed : bool ref = ref false

let bonds_links_up : (string * int) list ref = ref []

let value_to_int64 = function
	| Rrd.VT_Int64 x -> x
	| Rrd.VT_Float x -> Int64.of_float x
	| Rrd.VT_Unknown -> failwith "No last value!"

let read_host_memory xc =
	let physinfo = Xenctrl.physinfo xc in
	let free_kib =
		Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.Phys_info.free_pages) in
	let total_kib =
		Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.Phys_info.total_pages) in
	host_memory_changed :=
		!host_memory_free_cached <> free_kib || !host_memory_total_cached <> total_kib;
	host_memory_free_cached := free_kib;
	host_memory_total_cached := total_kib

let read_vm_memory xc =
	let domains = Xenctrl.domain_getinfolist xc 0 in
	let process_vm dom =
		let open Xenctrl.Domain_info in
		let uuid = Uuid.string_of_uuid (Uuid.uuid_of_int_array dom.handle) in
		let kib = Xenctrl.pages_to_kib (Int64.of_nativeint dom.total_memory_pages) in
		let memory = Int64.mul kib 1024L in
		uuid, memory
	in
	let vm_memory = List.map process_vm domains in
	if vm_memory <> !vm_memory_cached then
		List.iter (fun (uuid, memory) ->
			let changed =
				try memory <> List.assoc uuid !vm_memory_cached
				with _ -> true
			in
			if changed then
				changed_vm_memory := (uuid, memory) :: !changed_vm_memory
		) vm_memory;
	vm_memory_cached := vm_memory

let read_pifs_and_bonds () =
	(* Read fresh PIF information from networkd. *)
	let open Network_monitor in
	let stats = read_stats () in
	let pifs =
		List.fold_left (fun acc (dev, stat) ->
			match String.startswith "vif" dev with
			| true -> acc
			| false ->
				if stat.nb_links > 1 then (* bond *)
					bonds_links_up := !bonds_links_up @ [(dev, stat.links_up)];
				let pif = {
					pif_name = dev;
					pif_tx = -1.0;
					pif_rx = -1.0;
					pif_raw_tx = 0L;
					pif_raw_rx = 0L;
					pif_carrier = stat.carrier;
					pif_speed = stat.speed;
					pif_duplex = stat.duplex;
					pif_pci_bus_path = stat.pci_bus_path;
					pif_vendor_id = stat.vendor_id;
					pif_device_id = stat.device_id;
				} in
				pif :: acc
		) [] stats
	in
	(* Check if any of the PIFs have changed since our last reading. *)
	if pifs <> !pifs_cached then
		List.iter (fun pif ->
			let changed =
				try pif <> List.find (fun p -> p.pif_name = pif.pif_name) !pifs_cached
				with _ -> true
			in
			if changed then
				changed_pifs_names := StringSet.add pif.pif_name !changed_pifs_names
		) pifs;
	(* Store PIFs just read into in-memory cache. *)
	pifs_cached := pifs

(* This function updates the database for all the slowly changing properties
 * of host memory, VM memory, PIFs, and bonds.
 *)
let pifs_and_memory_update_fn xc =
	let memories, host_memories, pifs, bonds =
		Pervasiveext.finally (fun _ ->
			(* VM memory. *)
			read_vm_memory xc;
			let memories = !changed_vm_memory in
			(* PIFs and bonds. *)
			read_pifs_and_bonds ();
			let has_pif_changed pif = StringSet.mem pif.pif_name !changed_pifs_names in
			let pifs = List.filter has_pif_changed !pifs_cached in
			let bonds = !bonds_links_up in
			(* Host memory. *)
			read_host_memory xc;
			let host_memories =
				let bytes_of_kib x = Int64.shift_left x 10 in
				match !host_memory_changed with
				| false -> None
				| true -> Some (bytes_of_kib !host_memory_free_cached, bytes_of_kib !host_memory_total_cached)
			in
			memories, host_memories, pifs, bonds
		) (fun _ ->
			changed_pifs_names := StringSet.empty;
			bonds_links_up := [];
			changed_vm_memory := [];
			host_memory_changed := false;
		) in
		(* This is the bit that might block for some time, but by now we've released the mutex *)
		Server_helpers.exec_with_new_task "updating VM_metrics.memory_actual fields and PIFs"
		(fun __context ->
			let host = Helpers.get_localhost ~__context in
			List.iter (fun (uuid, memory) ->
				let vm = Db.VM.get_by_uuid ~__context ~uuid in
				let vmm = Db.VM.get_metrics ~__context ~self:vm in
				Db.VM_metrics.set_memory_actual ~__context ~self:vmm ~value:memory
			) memories;
			Monitor_master.update_pifs ~__context host pifs;
			let localhost = Helpers.get_localhost ~__context in
			List.iter (fun (bond, links_up) ->
				let my_bond_pifs = Db.PIF.get_records_where ~__context
					~expr:(And (And (Eq (Field "host", Literal (Ref.string_of localhost)),
						Not (Eq (Field "bond_master_of", Literal "()"))),
						Eq(Field "device", Literal bond))) in
				let my_bonds = List.map (fun (_, pif) -> List.hd pif.API.pIF_bond_master_of) my_bond_pifs in
				if (List.length my_bonds) <> 1 then
					debug "Error: bond %s cannot be found" bond
				else
					Db.Bond.set_links_up ~__context ~self:(List.hd my_bonds)
						~value:(Int64.of_int links_up)
			) bonds;
			match host_memories with None -> () | Some (free, total) ->
			debug "monitor_dbcall_thread: updating host memory to %s and %s" (Int64.to_string free) (Int64.to_string total);
			let metrics = Db.Host.get_metrics ~__context ~self:localhost in
			Db.Host_metrics.set_memory_total ~__context ~self:metrics ~value:total;
			Db.Host_metrics.set_memory_free ~__context ~self:metrics ~value:free
		)

let monitor_dbcall_thread () =
	Xenctrl.with_intf (fun xc ->
		while true do
			try
				pifs_and_memory_update_fn xc;
				Thread.delay 5.
			with e ->
				debug "monitor_dbcall_thread would have died from: %s; restarting in 30s."
					(ExnHelper.string_of_exn e);
				Thread.delay 30.
		done
	)
