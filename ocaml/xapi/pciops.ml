(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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
module D=Debug.Debugger(struct let name="pciops" end)
open D

open Listext
open Stringext
open Threadext

let reservations : (API.ref_PCI, int64) Hashtbl.t = Hashtbl.create 5
let m = Mutex.create ()

let get_free_functions ~__context pci =
	let assignments = List.length (Db.PCI.get_attached_VMs ~__context ~self:pci) in
	let functions = Int64.to_int (Db.PCI.get_functions ~__context ~self:pci) in
	functions - assignments

let reserve ~__context pci =
	Mutex.execute m (fun () ->
		let pci_id = Db.PCI.get_pci_id ~__context ~self:pci in
		(* Only attempt to make a reservation if the PCI device is actually free *)
		if get_free_functions ~__context pci <= 0 then begin
			debug "PCI device %s is already in use by another VM" pci_id;
			false
		end else begin
			(* Get a timestamp in nano seconds *)
			let timestamp = Oclock.gettime Oclock.monotonic in
			let reserved =
				try
					let timestamp' = Hashtbl.find reservations pci in
					if Int64.sub timestamp timestamp' > Int64.of_float 3e11 then begin
						(* The previous reservation has expired, so remove it *)
						Hashtbl.remove reservations pci;
						false
					end else begin
						debug "PCI device %s was reserved by another VM just %Ldms ago"
							pci_id (Int64.div (Int64.sub timestamp timestamp') 1000000L);
						true
					end
				with Not_found ->
					false
			in
			if reserved then
				false
			else begin
				debug "Adding a temporary reservation for PCI device %s" pci_id;
				Hashtbl.add reservations pci timestamp;
				true
			end
		end
	)

let unreserve ~__context pci =
	Mutex.execute m (fun () ->
		let pci_id = Db.PCI.get_pci_id ~__context ~self:pci in
		debug "Removing any temporary reservations for PCI device %s" pci_id;
		Hashtbl.remove reservations pci
	)

let unassign_all_for_vm ~__context vm =
	(* Db.VM.set_attached_PCIs ~__context ~self:vm ~value:[] *)
	let pcis = Db.VM.get_attached_PCIs ~__context ~self:vm in
	List.iter (fun self -> Db.PCI.remove_attached_VMs ~__context ~self ~value:vm) pcis

let pcidev_of_pci ~__context pci =
	let pci_id = Db.PCI.get_pci_id ~__context ~self:pci in
	Scanf.sscanf pci_id "%04x:%02x:%02x.%01x" (fun a b c d -> (a, b, c, d))

(* Confusion: the n/xxxx:xx:xx.x syntax originally meant PCI device
   xxxx:xx:xx.x should be plugged into bus number n. HVM guests don't have
   multiple PCI buses anyway. We reinterpret the 'n' to be a hotplug ordering *)
let sort_pcidevs devs =
	let ids = List.sort compare (Listext.List.setify (List.map fst devs)) in
	List.map (fun id ->
		id, (List.map snd (List.filter (fun (x, _) -> x = id) devs))
	) ids

let of_string dev =
	Scanf.sscanf dev "%d/%04x:%02x:%02x.%01x" (fun id a b c d -> (id, (a, b, c, d)))

let to_string (id, (a, b, c, d)) =
	Printf.sprintf "%d/%04x:%02x:%02x.%01x" id a b c d

let other_pcidevs_of_vm ~__context other_config =
	let devs =
		try
			let oc = List.assoc "pci" other_config in
			String.split ',' oc
		with Not_found -> []
	in
	List.fold_left (fun acc dev ->
		try
			of_string dev :: acc
		with _ -> acc
	) [] devs

