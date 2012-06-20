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

open Listext
module D=Debug.Debugger(struct let name="xapi" end)
open D

(* Populate last_boot_CPU_flags with the vendor and feature set of the given host's CPU. *)
let populate_cpu_flags ~__context ~vm ~host =
	let add_or_replace (key, value) values =
		if List.mem_assoc key values then
			List.replace_assoc key value values
		else
			(key, value) :: values
	in
	let cpu_info = Db.Host.get_cpu_info ~__context ~self:host in
	let flags = ref (Db.VM.get_last_boot_CPU_flags ~__context ~self:vm) in
	if List.mem_assoc "vendor" cpu_info then
		flags := add_or_replace ("vendor", List.assoc "vendor" cpu_info) !flags;
	if List.mem_assoc "features" cpu_info then
		flags := add_or_replace ("features", List.assoc "features" cpu_info) !flags;
	Db.VM.set_last_boot_CPU_flags ~__context ~self:vm ~value:!flags

let assert_vm_is_compatible ~__context ~vm ~host =
	let host_cpu_info = Db.Host.get_cpu_info ~__context ~self:host in
	let vm_cpu_info = Db.VM.get_last_boot_CPU_flags ~__context ~self:vm in
	if List.mem_assoc "vendor" vm_cpu_info then begin
		(* Check the VM was last booted on a CPU with the same vendor as this host's CPU. *)
		debug "VM has vendor %s; host has vendor %s" (List.assoc "vendor" vm_cpu_info) (List.assoc "vendor" host_cpu_info);
		if (List.assoc "vendor" vm_cpu_info) <> (List.assoc "vendor" host_cpu_info) then
			raise (Api_errors.Server_error(Api_errors.vm_incompatible_with_this_host,
				[Ref.string_of vm; Ref.string_of host; "VM was last booted on a host which had a CPU from a different vendor."]))
	end;
	if List.mem_assoc "features" vm_cpu_info then begin
		(* Check the VM was last booted on a CPU whose features are a subset of the features of this host's CPU. *)
		let host_cpu_features = Cpuid.string_to_features (List.assoc "features" host_cpu_info) in
		let vm_cpu_features = Cpuid.string_to_features (List.assoc "features" vm_cpu_info) in
		debug "VM has features %s; host has features %s" (List.assoc "features" vm_cpu_info) (List.assoc "features" host_cpu_info);
		if not((Cpuid.mask_features host_cpu_features vm_cpu_features) = vm_cpu_features) then
			raise (Api_errors.Server_error(Api_errors.vm_incompatible_with_this_host,
				[Ref.string_of vm; Ref.string_of host; "VM was last booted on a CPU with features this host's CPU does not have."]))
	end
