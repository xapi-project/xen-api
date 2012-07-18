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

open Db_filter_types
open Listext
open Monitor_types
open Stringext
open Threadext

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

(* Helper map functions. *)
let transfer_map ~source ~target =
	Hashtbl.clear target;
	Hashtbl.iter (fun k v -> Hashtbl.add target k v) source;
	Hashtbl.clear source

let get_updates ~before ~after ~f =
	Hashtbl.fold (fun k v acc ->
		if (try v <> Hashtbl.find before k with Not_found -> true)
		then (f k v acc)
		else acc
	) after []
let get_updates_map = get_updates ~f:(fun k v acc -> (k, v)::acc)
let get_updates_values = get_updates ~f:(fun _ v acc -> v::acc)

(* A cache mapping PIF names to PIF structures. *)
let pifs_cached_m : Mutex.t = Mutex.create ()
let pifs_cached : (string, Monitor_types.pif) Hashtbl.t = Hashtbl.create 10
let pifs_tmp    : (string, Monitor_types.pif) Hashtbl.t = Hashtbl.create 10
(* A cache mapping PIF names to bond.links_up. *)
let bonds_links_up_cached_m : Mutex.t = Mutex.create ()
let bonds_links_up_cached : (string, int) Hashtbl.t = Hashtbl.create 10
let bonds_links_up_tmp    : (string, int) Hashtbl.t = Hashtbl.create 10
(* A cache mapping vm_uuids to actual memory. *)
let vm_memory_cached_m : Mutex.t = Mutex.create ()
let vm_memory_cached : (string, Int64.t) Hashtbl.t = Hashtbl.create 100
let vm_memory_tmp    : (string, Int64.t) Hashtbl.t = Hashtbl.create 100
(* A cache for host's free/total memory. *)
let host_memory_m : Mutex.t = Mutex.create ()
let host_memory_free_cached : Int64.t ref = ref Int64.zero
let host_memory_total_cached : Int64.t ref = ref Int64.zero

(* This removes any current cache for the specified pif_name, which forces
 * fresh metrics for pif_name into xapi's database. *)
let clear_cache_for_pif ~pif_name =
	Mutex.execute pifs_cached_m (fun _ -> Hashtbl.remove pifs_cached pif_name)

(* Clear the whole cache. This forces fresh metrics to be written into xapi's
 * database. *)
let clear_cache () =
	let safe_clear ~cache ~lock =
		Mutex.execute lock (fun _ -> Hashtbl.clear cache) in
	safe_clear ~cache:pifs_cached ~lock:pifs_cached_m;
	safe_clear ~cache:bonds_links_up_cached ~lock:bonds_links_up_cached_m;
	safe_clear ~cache:vm_memory_cached ~lock:vm_memory_cached_m;
	Mutex.execute host_memory_m (fun _ ->
		host_memory_free_cached := Int64.zero;
		host_memory_total_cached := Int64.zero;
	)

let get_host_memory_changes xc =
	let physinfo = Xenctrl.physinfo xc in
	let bytes_of_pages pages =
		let kib = Xenctrl.pages_to_kib (Int64.of_nativeint pages) in
		Int64.shift_left kib 10
	in
	let free_bytes = bytes_of_pages physinfo.Xenctrl.Phys_info.free_pages in
	let total_bytes = bytes_of_pages physinfo.Xenctrl.Phys_info.total_pages in
	Mutex.execute host_memory_m (fun _ ->
		let host_memory_changed =
			!host_memory_free_cached <> free_bytes ||
			!host_memory_total_cached <> total_bytes
		in
		host_memory_free_cached := free_bytes;
		host_memory_total_cached := total_bytes;
		if host_memory_changed then Some (free_bytes, total_bytes) else None
	)

let get_vm_memory_changes xc =
	let domains = Xenctrl.domain_getinfolist xc 0 in
	let process_vm dom =
		let open Xenctrl.Domain_info in
		let uuid = Uuid.string_of_uuid (Uuid.uuid_of_int_array dom.handle) in
		let kib = Xenctrl.pages_to_kib (Int64.of_nativeint dom.total_memory_pages) in
		let memory = Int64.mul kib 1024L in
		Hashtbl.add vm_memory_tmp uuid memory
	in
	List.iter process_vm domains;
	Mutex.execute vm_memory_cached_m (fun _ ->
		let changed_vm_memory =
			get_updates_map ~before:vm_memory_cached ~after:vm_memory_tmp in
		transfer_map ~source:vm_memory_tmp ~target:vm_memory_cached;
		changed_vm_memory
	)

let get_pif_and_bond_changes () =
	(* Read fresh PIF information from networkd. *)
	let open Network_monitor in
	let stats = read_stats () in
	List.iter (fun (dev, stat) ->
		if not (String.startswith "vif" dev) then (
			if stat.nb_links > 1 then (* bond *)
				Hashtbl.add bonds_links_up_tmp dev stat.links_up;
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
			Hashtbl.add pifs_tmp pif.pif_name pif;
		)
	) stats;
	(* Check if any of the bonds have changed since our last reading. *)
	let bond_changes = Mutex.execute bonds_links_up_cached_m (fun _ ->
		let changes =
			get_updates_map ~before:bonds_links_up_cached ~after:bonds_links_up_tmp in
		transfer_map ~source:bonds_links_up_tmp ~target:bonds_links_up_cached;
		changes
	) in
	(* Check if any of the PIFs have changed since our last reading. *)
	let pif_changes = Mutex.execute pifs_cached_m (fun _ ->
		let changes = get_updates_values ~before:pifs_cached ~after:pifs_tmp in
		transfer_map ~source:pifs_tmp ~target:pifs_cached;
		changes
	) in
	(* Return lists of changes. *)
	pif_changes, bond_changes

(* This function updates the database for all the slowly changing properties
 * of host memory, VM memory, PIFs, and bonds.
 *)
let pifs_and_memory_update_fn xc =
	let host_memory_changes = get_host_memory_changes xc in
	let vm_memory_changes = get_vm_memory_changes xc in
	let pif_changes, bond_changes = get_pif_and_bond_changes () in
	Server_helpers.exec_with_new_task "updating VM_metrics.memory_actual fields and PIFs"
	(fun __context ->
		let host = Helpers.get_localhost ~__context in
		List.iter (fun (uuid, memory) ->
			let vm = Db.VM.get_by_uuid ~__context ~uuid in
			let vmm = Db.VM.get_metrics ~__context ~self:vm in
			Db.VM_metrics.set_memory_actual ~__context ~self:vmm ~value:memory
		) vm_memory_changes;
		Monitor_master.update_pifs ~__context host pif_changes;
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
		) bond_changes;
		match host_memory_changes with None -> () | Some (free, total) ->
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
