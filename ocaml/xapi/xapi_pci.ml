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
module D=Debug.Debugger(struct let name="xapi" end)
open D

open Listext
open Stringext

type pci = {
	id: string;
	vendor_id: string;
	vendor_name: string;
	device_id: string;
	device_name: string;
	class_id: string;
	class_name: string;
	related: string list;
}

type pci_class = Display_controller | Network_controller

let prog = Filename.concat Fhs.libexecdir "pci-info"

let find_class_id = function
	| Display_controller -> "03"
	| Network_controller -> "02"

let managed_classes = [Display_controller]

let read_pcis () =
	let result, _ = Forkhelpers.execute_command_get_output prog [] in
	let result = String.split '\n' result in
	let rec read ac = function
		| [] -> ac
		| hd :: tl ->
			try
				let parse id vendor_id vendor_name device_id device_name class_id class_name related =
					if id = "" then
						failwith "empty record"
					else
						let related = if related = "" then [] else String.split ',' related in
						{id; vendor_id; vendor_name; device_id; device_name; class_id; class_name; related}
				in
				let pci = Scanf.sscanf hd "%s@\t %s@\t %s@\t %s@\t %s@\t %s@\t %s@\t %s@\t" parse in
				read (pci :: ac) tl
			with _ -> read ac tl
	in
	read [] result

let get_pcis_by_class pcis cls =
	List.filter (fun pci -> pci.class_id = find_class_id cls) pcis

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

	let rec update_or_create cur = function
		| [] -> cur
		| pci :: remaining_pcis ->
			let obj =
				try
					let (rf, rc) = List.find (fun (rf, rc) ->
						rc.Db_actions.pCI_pci_id = pci.id &&
						rc.Db_actions.pCI_vendor_id = pci.vendor_id &&
						rc.Db_actions.pCI_device_id = pci.device_id)
						existing in
					let attached_VMs = List.filter (Db.is_valid_ref __context) rc.Db_actions.pCI_attached_VMs in
					if attached_VMs <> rc.Db_actions.pCI_attached_VMs then
						Db.PCI.set_attached_VMs ~__context ~self:rf ~value:attached_VMs;
					rf, rc
				with Not_found ->
					let self = create ~__context ~class_id:pci.class_id ~class_name:pci.class_name
						~vendor_id:pci.vendor_id ~vendor_name:pci.vendor_name ~device_id:pci.device_id
						~device_name:pci.device_name ~host ~pci_id:pci.id ~functions:1L
						~dependencies:[] ~other_config:[] in
					self, Db.PCI.get_record_internal ~__context ~self
			in
			update_or_create ((obj, pci) :: cur) remaining_pcis
	in
	let all_pcis = read_pcis () in
	let class_pcis = List.flatten (List.map (fun cls -> get_pcis_by_class all_pcis cls) managed_classes) in
	let deps = List.flatten (List.map (fun pci -> pci.related) class_pcis) in
	let deps = List.map (fun dep -> List.find (fun pci -> pci.id = dep) all_pcis) deps in
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

