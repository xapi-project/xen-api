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

let get_free_functions ~__context pci =
	let assignments = List.length (Db.PCI.get_attached_VMs ~__context ~self:pci) in
	let functions = Int64.to_int (Db.PCI.get_functions ~__context ~self:pci) in
	functions - assignments

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

let other_pcidevs_of_vm ~__context other_config =
	let devs =
		try
			let oc = List.assoc "pci" other_config in
			debug "PCI devices from other-config:pci to attach: %s" oc;
			String.split ',' oc
		with Not_found -> []
	in
	let devs = List.fold_left (fun acc dev ->
		try
			Scanf.sscanf dev "%d/%04x:%02x:%02x.%01x" (fun id a b c d -> (id, (a, b, c, d))) :: acc
		with _ -> acc
	) [] devs in
	if devs <> [] then begin
		Rbac.assert_permission ~__context ~permission:Rbac_static.permission_internal_vm_plug_pcidevs;
	end;
	devs

let attach_pcis ~__context ~xc ~xs ~hvm domid pcis =
	Helpers.log_exn_continue "attach_pcis" (fun () ->
		List.iter (fun (devid, devs) ->
			Device.PCI.bind devs;
			Device.PCI.add ~xc ~xs ~hvm ~msitranslate:0 ~pci_power_mgmt:0 devs domid devid
		) (sort_pcidevs pcis)
	) ()

let plug_pcidevs ~__context ~vm domid pcidevs =
	if List.length pcidevs > 0 then begin
		(* XXX: PCI passthrough needs a lot of work *)
		Vmopshelpers.with_xc_and_xs (fun xc xs ->
			if (Xenctrl.domain_getinfo xc domid).Xenctrl.hvm_guest then begin
				Device.PCI.bind pcidevs;
				List.iter (fun ((a, b, c, d) as device) ->
					debug "hotplugging PCI device %04x:%02x:%02x.%01x into domid: %d" a b c d domid;
					Device.PCI.plug ~xc ~xs device domid
				) pcidevs
			end
		)
	end

let plug_pcis ~__context ~vm domid managed_pcis other_pcidevs =
	(* First do the ones that xapi manages, plus dependencies. Plugs may generate exceptions here. *)
	let dependent_pcis = List.setify (List.flatten
		(List.map (fun pci -> Db.PCI.get_dependencies ~__context ~self:pci) managed_pcis)) in
	let pcis = managed_pcis @ dependent_pcis in
	debug "Managed and dependent PCI devices to attach: %s" (String.concat ", " (List.map Ref.string_of pcis));
	let pcidevs = List.sort compare (List.map (fun pci -> pcidev_of_pci ~__context pci) pcis) in
	plug_pcidevs ~__context ~vm domid pcidevs;

	(* Then do the ones specified in other-config:pci. Any exceptions are logged but ignored. *)
	let other_pcidevs = List.flatten (List.map (fun (_, dev) -> dev) (sort_pcidevs other_pcidevs)) in
	let other_pcidevs = List.set_difference other_pcidevs pcidevs in
	Helpers.log_exn_continue "plug_pcidevs" (fun () ->
		plug_pcidevs ~__context ~vm domid other_pcidevs
	) ()

let unplug_pcidevs_noexn ~__context ~vm domid pcidevs =
	Helpers.log_exn_continue "unplug_pcidevs" (fun () ->
		Vmopshelpers.with_xc_and_xs (fun xc xs ->
			if (Xenctrl.domain_getinfo xc domid).Xenctrl.hvm_guest then begin
				List.iter (fun (devid, devices) ->
					List.iter (fun device ->
						debug "requesting hotunplug of PCI device %s" (Device.PCI.to_string device);
						Device.PCI.unplug ~xc ~xs device domid;
					) devices
				) (sort_pcidevs pcidevs)
			end
		)
	) ()

let currently_attached_pcis ~__context domid =
	let host = Helpers.get_localhost ~__context in
	Vmopshelpers.with_xc_and_xs (fun xc xs ->
		if (Xenctrl.domain_getinfo xc domid).Xenctrl.hvm_guest then begin
			let online_devs = List.map (fun (_, (a, b, c, d)) -> Printf.sprintf "%04x:%02x:%02x.%01x" a b c d)
				(Device.PCI.list ~xc ~xs domid) in
			List.filter_map (fun (pref, prec) ->
				if (List.exists (fun dev -> dev = prec.API.pCI_pci_id) online_devs) &&
					(prec.API.pCI_host = host)
				then ((debug "PCI dev %s is currently attached" prec.API.pCI_pci_id); Some pref)
				else None)
				(Db.PCI.get_all_records ~__context)
		end else
			[]
	)

