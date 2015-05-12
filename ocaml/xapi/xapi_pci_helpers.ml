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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type pci_property = {
	id: int64;
	name: string;
}

type pci = {
	pci_id: string;
	vendor: pci_property;
	device: pci_property;
	pci_class: pci_property;
	subsystem_vendor: pci_property option;
	subsystem_device: pci_property option;
	related: string list;
}

let get_host_pcis _ =
	let open Pci in
	let pci_access = alloc () in
	init pci_access;
	scan_bus pci_access;
	let devs = Pci_access.devices pci_access in
	List.map (fun d ->
		let (_: int) = fill_info d [ FILL_IDENT; FILL_BASES; FILL_CLASS ] in
		let open Pci_dev in
		let address_of_dev x = Printf.sprintf "%04x:%02x:%02x.%d" (domain x) (bus x) (dev x) (func x) in
		let vendor = { id = Int64.of_int @@ vendor_id d; name = lookup_vendor_name pci_access (vendor_id d) } in
		let device = { id = Int64.of_int @@ device_id d; name = lookup_device_name pci_access (vendor_id d) (device_id d) } in
		let (subsystem_vendor, subsystem_device) = match subsystem_id d with
		| None -> None, None
		| Some (sv_id, sd_id) ->
			let sv_name = lookup_subsystem_vendor_name pci_access sv_id in
			let sd_name = lookup_subsystem_device_name pci_access (vendor_id d) (device_id d) sv_id sd_id in
			Some { id = Int64.of_int @@ sv_id; name = sv_name }, Some { id = Int64.of_int @@ sd_id; name = sd_name }
		in
		let pci_class = { id = Int64.of_int @@ device_class d; name = lookup_class_name pci_access (device_class d) } in
		let related_devs =
			List.filter (fun d' ->
				func d' <> func d && List.for_all (fun f -> f d' = f d) [domain; bus; dev]
			) devs in
		{ pci_id = address_of_dev d;
			vendor; device; subsystem_vendor; subsystem_device; pci_class;
			related = List.map address_of_dev related_devs;
		}
	) devs

let igd_is_whitelisted ~__context pci =
	let vendor_id = Db.PCI.get_vendor_id ~__context ~self:pci in
	List.mem vendor_id !Xapi_globs.igd_passthru_vendor_whitelist

