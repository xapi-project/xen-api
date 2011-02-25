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

(* Confusion: the n/xxxx:xx:xx.x syntax originally meant PCI device
   xxxx:xx:xx.x should be plugged into bus number n. HVM guests don't have
   multiple PCI buses anyway. We reinterpret the 'n' to be a hotplug ordering *)
let sort_pcidevs devs =
	let ids = List.sort compare (Listext.List.setify (List.map fst devs)) in
	List.map (fun id ->
		id, (List.map snd (List.filter (fun (x, _) -> x = id) devs))
	) ids

let attach_pcis ~__context ~xc ~xs ~hvm domid pcis =
	Helpers.log_exn_continue "attach_pcis" (fun () ->
		List.iter (fun (devid, devs) ->
			Device.PCI.bind devs;
			Device.PCI.add ~xc ~xs ~hvm ~msitranslate:0 ~pci_power_mgmt:0 devs domid devid
		) (sort_pcidevs pcis)
	) ()

let pcidevs_of_vm ~__context ~vm =
	let other_config = Db.VM.get_other_config ~__context ~self:vm in
	let host = Helpers.get_localhost ~__context in
	let devs = try String.split ',' (List.assoc "pci" other_config) with Not_found -> [] in
	let devs = List.filter_map (fun self ->
		if Db.PCI.get_host ~__context ~self = host then
			let assignments = Db.PCI.get_assignments ~__context ~self in
			if List.mem_assoc vm assignments then
				Some ("0/" ^ (List.assoc vm assignments))
			else
				None
		else None
	) (Db.PCI.get_all ~__context) @ devs in
	let devs = List.fold_left (fun acc dev ->
		try
			Scanf.sscanf dev "%d/%04x:%02x:%02x.%01x" (fun id a b c d -> (id, (a, b, c, d))) :: acc
		with _ -> acc
	) [] devs in
	(* Preserve the configured order *)
	let devs = List.rev devs in
	if devs <> [] then
		Rbac.assert_permission ~__context ~permission:Rbac_static.permission_internal_vm_plug_pcidevs;
	devs

(** Hotplug the PCI devices into the domain (as opposed to 'attach_pcis') *)
let plug_pcidevs_noexn ~__context ~vm domid pcidevs =
	Helpers.log_exn_continue "plug_pcidevs" (fun () ->
		if List.length pcidevs > 0 then begin
			(* XXX: PCI passthrough needs a lot of work *)
			Vmopshelpers.with_xc_and_xs (fun xc xs ->
				if (Xc.domain_getinfo xc domid).Xc.hvm_guest then begin
					List.iter (fun (_, devices) ->
						Device.PCI.bind devices;
						List.iter (fun ((a, b, c, d) as device) ->
							debug "hotplugging PCI device %04x:%02x:%02x.%01x into domid: %d" a b c d domid;
							Device.PCI.plug ~xc ~xs device domid
						) devices
					) (sort_pcidevs pcidevs)
				end
			)
		end;
	) ()

(** Hot unplug the PCI devices from the domain. Note this is done serially due to a limitation of the
   xenstore protocol. *)
let unplug_pcidevs_noexn ~__context ~vm domid pcidevs = 
	Helpers.log_exn_continue "unplug_pcidevs" (fun () ->
		Vmopshelpers.with_xc_and_xs (fun xc xs ->
			if (Xc.domain_getinfo xc domid).Xc.hvm_guest then begin
				List.iter (fun (devid, devices) ->
					List.iter (fun device ->
						debug "requesting hotunplug of PCI device %s" (Device.PCI.to_string device);
						Device.PCI.unplug ~xc ~xs device domid;
					) devices
				) (sort_pcidevs pcidevs)
			end
		)
	) ()

