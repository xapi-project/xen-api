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
module D=Debug.Make(struct let name="xapi" end)
open D

open Listext
open Stringext

type managed_class = Display_controller | Network_controller

let lookup_class_id = function
	| Display_controller -> 03L
	| Network_controller -> 02L

let managed_classes = [Display_controller]

let get_pcis_by_class pcis cls =
	List.filter (fun pci -> pci.Xapi_pci_helpers.class_id = lookup_class_id cls) pcis

let string_of_pci ~__context ~self =
	let pci = Db.PCI.get_record_internal ~__context ~self in
	String.concat "/" [pci.Db_actions.pCI_vendor_id; pci.Db_actions.pCI_device_id]

(* We use ints within code but schema uses hex strings _without_ leading '0x' *)
let int_of_id string_id =
	let int_of_hex_str = fun s -> Scanf.sscanf s "%Lx" (fun x -> x) in
	int_of_hex_str string_id
let id_of_int hex_id =
	Printf.sprintf "%04Lx" hex_id

let create ~__context ~class_id ~class_name ~vendor_id ~vendor_name ~device_id
		~device_name ~host ~pci_id ~functions ~dependencies ~other_config =
	let p = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.PCI.create ~__context ~ref:p ~uuid ~class_id ~class_name ~vendor_id ~vendor_name ~device_id
		~device_name ~host ~pci_id ~functions ~dependencies:[] ~other_config:[];
	debug "PCI %s, %s, %s created" pci_id vendor_name device_name;
	p

let update_pcis ~__context ~host =
	let existing = List.filter_map
		(fun pref ->
			let prec = Db.PCI.get_record_internal ~__context ~self:pref in
			if prec.Db_actions.pCI_host = host then
				Some (pref, prec)
			else
				None)
		(Db.PCI.get_all ~__context)
	in

	let open Xapi_pci_helpers in
	let pci_db = Pci_db.open_default () in
	let rec update_or_create cur = function
		| [] -> cur
		| pci :: remaining_pcis ->
			let obj =
				try
					let (rf, rc) = List.find (fun (rf, rc) ->
						rc.Db_actions.pCI_pci_id = pci.id &&
						rc.Db_actions.pCI_vendor_id = id_of_int pci.vendor_id &&
						rc.Db_actions.pCI_device_id = id_of_int pci.device_id)
						existing in
					if rc.Db_actions.pCI_vendor_name <> pci.vendor_name
					then Db.PCI.set_vendor_name ~__context ~self:rf ~value:pci.vendor_name;
					if rc.Db_actions.pCI_device_name <> pci.device_name
					then Db.PCI.set_device_name ~__context ~self:rf ~value:pci.device_name;
					let attached_VMs = List.filter (Db.is_valid_ref __context) rc.Db_actions.pCI_attached_VMs in
					if attached_VMs <> rc.Db_actions.pCI_attached_VMs then
						Db.PCI.set_attached_VMs ~__context ~self:rf ~value:attached_VMs;
					rf, rc
				with Not_found ->
					let self = create ~__context
						~class_id:(id_of_int pci.class_id)
						~class_name:pci.class_name
						~vendor_id:(id_of_int pci.vendor_id)
						~vendor_name:pci.vendor_name
						~device_id:(id_of_int pci.device_id)
						~device_name:pci.device_name ~host ~pci_id:pci.id
						~functions:1L ~dependencies:[] ~other_config:[] in
					self, Db.PCI.get_record_internal ~__context ~self
			in
			update_or_create ((obj, pci) :: cur) remaining_pcis
	in
	let host_pcis = Xapi_pci_helpers.get_host_pcis pci_db in
	let class_pcis = List.flatten (List.map (fun cls -> get_pcis_by_class host_pcis cls) managed_classes) in
	let deps = List.flatten (List.map (fun pci -> pci.related) class_pcis) in
	let deps = List.map (fun dep -> List.find (fun pci -> pci.id = dep) host_pcis) deps in
	let managed_pcis = List.setify (class_pcis @ deps) in
	let current = update_or_create [] managed_pcis in

	let update_dependencies current =
		let rec update = function
		| [] -> ()
		| ((pref, prec), pci) :: remaining ->
			let dependencies = List.map
				(fun pci_id ->
					let (r, _), _ = List.find (fun ((_, rc), _) -> rc.Db_actions.pCI_pci_id = pci_id) current
					in r)
				pci.related
			in
			Db.PCI.set_dependencies ~__context ~self:pref ~value:dependencies;
			update remaining
		in
		update current
	in
	update_dependencies current;

	let current = List.map (fun ((pref, prec), _) -> pref, prec) current in
	let obsolete = List.set_difference existing current in
	List.iter (fun (self, _) -> Db.PCI.destroy ~__context ~self) obsolete

let get_system_display_device () =
	let device = "/dev/vga_arbiter" in
	try
		let line =
			Unixext.with_input_channel
				device
				(fun chan -> input_line chan)
		in
		(* Example contents of line:
		 * count:7,PCI:0000:10:00.0,decodes=io+mem,owns=io+mem,locks=none(0:0) *)
		let items = String.split ',' line in
		List.fold_left
			(fun acc item ->
				if String.startswith "PCI" item
				then Some (Scanf.sscanf item "PCI:%s" (fun id -> id))
				else acc)
			None items
	with _ -> None
