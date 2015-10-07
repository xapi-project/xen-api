(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

open Fun
open Listext
open Xapi_globs

module D=Debug.Make(struct let name="xapi" end)
open D

let string_of_features features =
	Array.map (Printf.sprintf "%08Lx") features
		|> Array.to_list
		|> String.concat "-"

exception InvalidFeatureString of string
let features_of_string str =
	let scanf fmt s = Scanf.sscanf s fmt (fun x -> x) in
	try Stringext.split ~on:'-' str 
		|> Array.of_list
		|> Array.map (scanf "%08Lx%!")
	with _ -> raise (InvalidFeatureString str)

let zero_extend arr len =
	let new_arr = Array.make len 0L in 
	Array.blit arr 0 new_arr 0 (min len (Array.length arr));
	new_arr

let get_pool_feature_mask ~__context ~remote =
	try
		let other_config =
			match remote with
			| None ->
				let pool = Helpers.get_pool ~__context in
				Db.Pool.get_other_config ~__context ~self:pool
			| Some (rpc, session_id) ->
				let pool = List.hd (Client.Client.Pool.get_all rpc session_id) in
				Client.Client.Pool.get_other_config rpc session_id pool
		in
		let mask_string = List.assoc Xapi_globs.cpuid_feature_mask_key other_config in
		debug "Found pool feature mask: %s" mask_string;
		Some (Cpuid.string_to_features mask_string)
	with _ ->
		None

let maybe_apply_mask mask features =
	match mask with
	| Some mask' -> Cpuid.mask_features features mask'
	| None -> features

let get_host_compatibility_info ~__context ~host ~remote =
	let cpu_info =
		match remote with
		| None -> Db.Host.get_cpu_info ~__context ~self:host
		| Some (rpc, session_id) -> Client.Client.Host.get_cpu_info rpc session_id host
	in
	let vendor = List.assoc cpu_info_vendor_key cpu_info in
	let features = List.assoc cpu_info_features_key cpu_info in
	(vendor, features)

(* Populate last_boot_CPU_flags with the vendor and feature set.
 * On VM.start, the feature set is inherited from the pool level (PV or HVM).
 *)
let populate_cpu_flags ~__context ~vm ~host =
	let pool = Helpers.get_pool ~__context in
	let cpu_info = Db.Pool.get_cpu_info ~__context ~self:pool in
	let features_key =
		if Helpers.will_boot_hvm ~__context ~self:vm then
			cpu_info_features_hvm_key
		else
			cpu_info_features_pv_key
	in
	let cpu_vendor = List.assoc cpu_info_vendor_key cpu_info in
	let cpu_features = List.assoc features_key cpu_info in
	let vm_cpu_flags = [
		(cpu_info_vendor_key, cpu_vendor);
		(cpu_info_features_key, cpu_features);]
	in
	Db.VM.set_last_boot_CPU_flags ~__context ~self:vm ~value:vm_cpu_flags

(* Compare the CPU on which the given VM was last booted to the CPU of the given host. *)
let assert_vm_is_compatible ~__context ~vm ~host ?remote () =
	let fail msg =
		raise (Api_errors.Server_error(Api_errors.vm_incompatible_with_this_host,
			[Ref.string_of vm; Ref.string_of host; msg]))
	in
	if Db.VM.get_power_state ~__context ~self:vm <> `Halted then begin
		let host_cpu_vendor, host_cpu_features = get_host_compatibility_info ~__context ~host ~remote in
		let vm_cpu_info = Db.VM.get_last_boot_CPU_flags ~__context ~self:vm in
		if List.mem_assoc cpu_info_vendor_key vm_cpu_info then begin
			(* Check the VM was last booted on a CPU with the same vendor as this host's CPU. *)
			let vm_cpu_vendor = List.assoc cpu_info_vendor_key vm_cpu_info in
			debug "VM last booted on CPU of vendor %s; host CPUs are of vendor %s" vm_cpu_vendor host_cpu_vendor;
			if vm_cpu_vendor <> host_cpu_vendor then
				fail "VM last booted on a host which had a CPU from a different vendor."
		end;
		if List.mem_assoc cpu_info_features_key vm_cpu_info then begin
			(* Check the VM was last booted on a CPU whose features are a subset of the features of this host's CPU. *)
			let vm_cpu_features = List.assoc cpu_info_features_key vm_cpu_info in
			debug "VM last booted on CPU with features %s; host CPUs have features %s" vm_cpu_features host_cpu_features;
			let pool_mask = get_pool_feature_mask ~__context ~remote in
			let vm_cpu_features' = vm_cpu_features |> Cpuid.string_to_features |> maybe_apply_mask pool_mask in
			let host_cpu_features' = host_cpu_features |> Cpuid.string_to_features |> maybe_apply_mask pool_mask in
			if not((Cpuid.mask_features vm_cpu_features' host_cpu_features') = vm_cpu_features') then
				fail "VM last booted on a CPU with features this host's CPU does not have."
		end
	end

