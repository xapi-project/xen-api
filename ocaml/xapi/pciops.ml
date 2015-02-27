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

(* http://wiki.xen.org/wiki/Bus:Device.Function_%28BDF%29_Notation *)
(* It might be possible to refactor this but attempts so far have failed. *)
let bdf_fmt            = format_of_string    "%04x:%02x:%02x.%01x"
let slash_bdf_scan_fmt = format_of_string "%d/%04x:%02x:%02x.%01x"
let slash_bdf_prnt_fmt = format_of_string "%d/%04x:%02x:%02x.%01x"
let bdf_paren_prnt_fmt = format_of_string   "(%04x:%02x:%02x.%01x)"
let bdf_paren_scan_fmt = format_of_string   "(%04x:%02x:%02x.%01x)"

let pcidev_of_pci ~__context pci =
	let bdf_str = Db.PCI.get_pci_id ~__context ~self:pci in
	Scanf.sscanf bdf_str bdf_fmt (fun a b c d -> (a, b, c, d))

(* Confusion: the n/xxxx:xx:xx.x syntax originally meant PCI device
   xxxx:xx:xx.x should be plugged into bus number n. HVM guests don't have
   multiple PCI buses anyway. We reinterpret the 'n' to be a hotplug ordering *)
let sort_pcidevs devs =
	let ids = List.sort compare (Listext.List.setify (List.map fst devs)) in
	List.map (fun id ->
		id, (List.map snd (List.filter (fun (x, _) -> x = id) devs))
	) ids

let of_string dev =
	Scanf.sscanf dev slash_bdf_scan_fmt (fun id a b c d -> (id, (a, b, c, d)))

let to_string (id, (a, b, c, d)) =
	Printf.sprintf slash_bdf_prnt_fmt id a b c d

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

let pci_hiding_key = "xen-pciback.hide"
let pci_hiding_key_eq = pci_hiding_key ^ "="
let xen_cmdline_path = "/opt/xensource/libexec/xen-cmdline"

let get_pci_hidden_raw_value () =
	let cmd = xen_cmdline_path ^ " --get-dom0 " ^ pci_hiding_key in
	let raw_kv_string = Helpers.get_process_output cmd in
	(* E.g. "xen-pciback.hide=(0000:00:02.0)(0000:00:02.1)\n" or just "\n" *)
	if String.startswith pci_hiding_key_eq raw_kv_string then
		let keylen = String.length pci_hiding_key_eq in
		(* rtrim to remove trailing newline *)
		String.rtrim(String.sub_to_end raw_kv_string keylen)
	else
		""

let get_hidden_pcidevs () =
	let paren_len = String.length "(0000:00:00.0)" in
	let rec read_dev devs raw =
		match raw with
			| "" -> devs
			| _ -> (
				let dev = Scanf.sscanf
					raw bdf_paren_scan_fmt (fun a b c d -> (a, b, c, d)) in
				read_dev (dev::devs) (String.sub_to_end raw paren_len)
			)
	in
	read_dev [] (get_pci_hidden_raw_value ())

let _is_pci_hidden ~__context pci =
	let pcidev = pcidev_of_pci ~__context pci in
	List.mem pcidev (get_hidden_pcidevs ())

(** Check whether a PCI device will be hidden from the dom0 kernel on boot. *)
let is_pci_hidden ~__context pci =
	Mutex.execute m (fun () ->
		_is_pci_hidden ~__context pci
	)

let _hide_pci ~__context pci =
	if not (_is_pci_hidden ~__context pci) then (
		let paren_of (a, b, c, d) = (
			Printf.sprintf bdf_paren_prnt_fmt a b c d
		) in
		let p = pcidev_of_pci ~__context pci in
		let devs = p::(get_hidden_pcidevs ()) in
		let valstr = List.fold_left (fun acc d -> acc ^ (paren_of d)) "" devs in
		let cmd = Printf.sprintf "%s --set-dom0 %s%s"
			xen_cmdline_path pci_hiding_key_eq valstr in
		let _ = Helpers.get_process_output cmd in
		()
	)

(** Hide a PCI device from the dom0 kernel. (Takes effect after next boot.) *)
let hide_pci ~__context pci =
	Mutex.execute m (fun () ->
		_hide_pci ~__context pci
	)

let _unhide_pci ~__context pci =
	if (_is_pci_hidden ~__context pci) then (
		let raw_value = get_pci_hidden_raw_value () in
		let bdf_paren = Printf.sprintf "(%s)"
			(Db.PCI.get_pci_id ~__context ~self:pci) in
		let new_value = String.replace bdf_paren "" raw_value in
		let cmd = match new_value with
			| "" -> Printf.sprintf "%s --delete-dom0 %s"
				xen_cmdline_path pci_hiding_key
			| _ -> Printf.sprintf "%s --set-dom0 %s%s"
				xen_cmdline_path pci_hiding_key_eq new_value
		in
		let _ = Helpers.get_process_output cmd in
		()
	)

(** Unhide a PCI device from the dom0 kernel. (Takes effect after next boot.) *)
let unhide_pci ~__context pci =
	Mutex.execute m (fun () ->
		_unhide_pci ~__context pci
	)

(** Return the id of a PCI device *)
let id_of (id, (domain, bus, dev, fn)) = id

(** Return the domain of a PCI device *)
let domain_of (id, (domain, bus, dev, fn)) = domain

(** Return the bus of a PCI device *)
let bus_of (id, (domain, bus, dev, fn)) = bus

(** Return the device of a PCI device *)
let dev_of (id, (domain, bus, dev, fn)) = dev

(** Return the function of a PCI device *)
let fn_of (id, (domain, bus, dev, fn)) = fn
