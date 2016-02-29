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

module D=Debug.Make(struct let name="agility" end)
open D

open Listext

module VMResources = struct
	(* Encapsulate the state of a VM (which may or may not be in the local
	 * database, along with the SRs and networks to which it needs access. *)
	type t = {
		status : [ `Local of API.ref_VM | `Incoming of API.ref_host ];
		vm_rec : API.vM_t;
		networks : API.ref_network list;
		srs : API.ref_SR list;
	}

	let of_ref_and_record ~__context vm_ref vm_rec =
		let status = `Local vm_ref in
		let networks =
			List.map
				(fun vif -> Db.VIF.get_network ~__context ~self:vif) vm_rec.API.vM_VIFs
		in
		let srs =
			List.filter_map
				(fun vbd ->
					let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
					if Db.is_valid_ref __context vdi
					then Some (Db.VDI.get_SR ~__context ~self:vdi)
					else None)
				vm_rec.API.vM_VBDs
		in {status; vm_rec; networks; srs}

	let of_ref ~__context vm_ref =
		let vm_rec = Db.VM.get_record ~__context ~self:vm_ref in
		of_ref_and_record ~__context vm_ref vm_rec

	let to_string {vm_rec} =
		Printf.sprintf "%s (%s)"
			vm_rec.API.vM_uuid vm_rec.API.vM_name_label

	let compare vm_res1 vm_res2 =
		compare vm_res1.vm_rec.API.vM_uuid vm_res2.vm_rec.API.vM_uuid

	let are_equal vm_res1 vm_res2 =
		vm_res1.vm_rec.API.vM_uuid = vm_res2.vm_rec.API.vM_uuid

	let mem vm_res vm_ress =
		List.exists (fun vm_res' -> are_equal vm_res vm_res') vm_ress

	let rec assoc vm_res pairs =
		match pairs with
		| [] -> raise Not_found
		| (vm_res', x)::rest ->
			if are_equal vm_res vm_res' then x else assoc vm_res rest

	let update_record vm_res vm_rec = {vm_res with vm_rec = vm_rec}
end

(* Only returns true if the SR is marked as shared, all hosts have PBDs and all PBDs are currently_attached.
   Is used to prevent a non-shared disk being added to a protected VM *)
let is_sr_properly_shared ~__context ~self =
	let shared = Db.SR.get_shared ~__context ~self in
	if not shared then begin
		false
	end else begin
		let pbds = Db.SR.get_PBDs ~__context ~self in
		let plugged_pbds = List.filter (fun pbd -> Db.PBD.get_currently_attached ~__context ~self:pbd) pbds in
		let plugged_hosts = List.setify (List.map (fun pbd -> Db.PBD.get_host ~__context ~self:pbd) plugged_pbds) in
		let all_hosts = Db.Host.get_all ~__context in
		let enabled_hosts = List.filter (fun host -> Db.Host.get_enabled ~__context ~self:host) all_hosts in
		if not(List.subset enabled_hosts plugged_hosts) then begin
			warn "SR %s not shared properly: Not all enabled hosts have a currently_attached PBD" (Ref.string_of self);
			false
		end else true
	end

(* Only returns true if the network is shared properly: all (enabled) hosts in the pool must have a PIF on
 * the network, and none of these PIFs may be bond slaves. This ensures that a VM with a VIF on this
 * network can run on (and be migrated to) any (enabled) host in the pool. *)
let is_network_properly_shared ~__context ~self =
	let pifs = Db.Network.get_PIFs ~__context ~self in
	let non_slave_pifs = List.filter (fun pif ->
		not (Db.is_valid_ref __context (Db.PIF.get_bond_slave_of ~__context ~self:pif))) pifs in
	let hosts_with_pif = List.setify (List.map (fun pif -> Db.PIF.get_host ~__context ~self:pif) non_slave_pifs) in
	let all_hosts = Db.Host.get_all ~__context in
	let enabled_hosts = List.filter (fun host -> Db.Host.get_enabled ~__context ~self:host) all_hosts in
	let properly_shared = List.subset enabled_hosts hosts_with_pif in
	if not properly_shared then
		warn "Network %s not shared properly: Not all hosts have PIFs" (Ref.string_of self);
	properly_shared

module SRSet = Set.Make(struct type t = API.ref_SR let compare = compare end)
module NetworkSet = Set.Make(struct type t = API.ref_network let compare = compare end)

let empty_cache = (SRSet.empty, NetworkSet.empty)

let caching_vm_res_assert_agile ~__context (ok_srs, ok_networks) vm_res =
	(* Any kind of vGPU means that the VM is not agile. *)
	if vm_res.VMResources.vm_rec.API.vM_VGPUs <> [] then begin
		let vm_string =
			match vm_res.VMResources.status with
			| `Local vm_ref -> Ref.string_of vm_ref
			| `Incoming _ -> Ref.string_of Ref.null
		in
		raise (Api_errors.Server_error
			(Api_errors.vm_has_vgpu, [vm_string]))
	end;
	(* All referenced SRs should be shared. *)
	let check_sr ok_srs sr =
		if SRSet.mem sr ok_srs
		then ok_srs
		else
			if not (is_sr_properly_shared ~__context ~self:sr)
			then raise (Api_errors.(Server_error(ha_constraint_violation_sr_not_shared, [Ref.string_of sr])))
			else SRSet.add sr ok_srs in
	(* All referenced networks should be shared. *)
	let check_network ok_networks network =
		if NetworkSet.mem network ok_networks
		then ok_networks
		else
			if not (is_network_properly_shared ~__context ~self:network)
			then raise (Api_errors.(Server_error(ha_constraint_violation_network_not_shared, [Ref.string_of network])))
			else NetworkSet.add network ok_networks in
	let ok_srs = List.fold_left check_sr ok_srs vm_res.VMResources.srs in
	let ok_networks = List.fold_left check_network ok_networks vm_res.VMResources.networks in
	(ok_srs, ok_networks)

let vm_assert_agile ~__context ~self =
	let vm_res = VMResources.of_ref ~__context self in
	let _ = caching_vm_res_assert_agile ~__context empty_cache vm_res in
	()

let partition_vm_ps_by_agile ~__context vm_ress =
	let distinguish_vm (agile_vm_ress, not_agile_vm_ress, cache) vm_res =
		try
			let cache = caching_vm_res_assert_agile ~__context cache vm_res in
			(vm_res :: agile_vm_ress, not_agile_vm_ress, cache)
		with _ ->
			(agile_vm_ress, vm_res :: not_agile_vm_ress, cache) in
	let agile_vm_ress, not_agile_vm_ress, _ = List.fold_left distinguish_vm ([], [], empty_cache) vm_ress in
	(List.rev agile_vm_ress, List.rev not_agile_vm_ress)
