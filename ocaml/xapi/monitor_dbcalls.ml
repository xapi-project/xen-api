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

open Db_filter_types
open Listext
open Monitor_types
open Stringext
open Threadext

module D = Debug.Debugger(struct let name = "monitor_dbcalls" end)
open D

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

let clear_cache_for_pif ~pif_name =
	Mutex.execute pifs_cached_m (fun _ -> Hashtbl.remove pifs_cached pif_name)

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
		if not dom.dying then
			begin
				let uuid = Uuid.string_of_uuid (Uuid.uuid_of_int_array dom.handle) in
				let kib = Xenctrl.pages_to_kib (Int64.of_nativeint dom.total_memory_pages) in
				let memory = Int64.mul kib 1024L in
				Hashtbl.add vm_memory_tmp uuid memory
			end
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
