(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

let ( +++ ) = Int64.add
let ( --- ) = Int64.sub
let ( *** ) = Int64.mul
let ( /// ) = Int64.div

(** Calculates the amounts of 'normal' and 'shadow' host memory needed *)
(** to run the given guest with the given amount of guest memory.      *)
let vm_compute_required_memory vm_record guest_memory_kib =
	let vcpu_count = Int64.to_int vm_record.API.vM_VCPUs_max in
	let multiplier =
		if Helpers.is_hvm vm_record
		then vm_record.API.vM_HVM_shadow_multiplier
		else Memory.Linux.shadow_multiplier_default in
	let target_mib = Memory.mib_of_kib_used guest_memory_kib in
	let max_mib = Memory.mib_of_bytes_used vm_record.API.vM_memory_static_max in
	let footprint_mib = (
		if Helpers.is_hvm vm_record
		then Memory.HVM.footprint_mib
		else Memory.Linux.footprint_mib)
			target_mib max_mib vcpu_count multiplier in
	let shadow_mib = (
		if Helpers.is_hvm vm_record
		then Memory.HVM.shadow_mib
		else Memory.Linux.shadow_mib)
			max_mib vcpu_count multiplier in
	let normal_mib =
		footprint_mib --- shadow_mib in
	let normal_bytes = Memory.bytes_of_mib normal_mib in
	let shadow_bytes = Memory.bytes_of_mib shadow_mib in
	(normal_bytes, shadow_bytes)

(** Different users will wish to use a different VM accounting policy, depending
on how conservative or liberal they are. *)
type accounting_policy =
	| Static_max
		(** use static_max: conservative: useful for HA. *)
	| Dynamic_max
		(** use dynamic_max: fairly conservative: useful for dom0 for HA. *)
	| Dynamic_min
		(** use dynamic_min: liberal: assumes that guests always co-operate. *)

(** Common logic of vm_compute_start_memory and vm_compute_used_memory *)
let choose_memory_required ~policy ~ballooning_enabled ~memory_dynamic_min ~memory_dynamic_max ~memory_static_max =
	match (ballooning_enabled, policy) with
		| (true, Dynamic_min) -> memory_dynamic_min
		| (true, Dynamic_max) -> memory_dynamic_max
		| (false, Dynamic_min)
		| (false, Dynamic_max)
		| (_, Static_max) -> memory_static_max

(** Calculates the amount of memory required in both 'normal' and 'shadow'
memory, to start a VM. If the given VM is a PV guest and if memory ballooning
is enabled, this function returns values derived from the VM's dynamic memory
target (since PV guests are able to start in a pre-ballooned state). If memory
ballooning is not enabled or if the VM is an HVM guest, this function returns
values derived from the VM's static memory maximum (since currently HVM guests
are not able to start in a pre-ballooned state). *)
let vm_compute_start_memory ~__context ?(policy=Dynamic_min) vm_record =
	if Xapi_fist.disable_memory_checks ()
	then (0L, 0L)
	else
		let memory_required = choose_memory_required
			~policy: policy
			~ballooning_enabled: (Helpers.ballooning_enabled_for_vm ~__context vm_record)
			~memory_dynamic_min: vm_record.API.vM_memory_dynamic_min
			~memory_dynamic_max: vm_record.API.vM_memory_dynamic_max
			~memory_static_max:  vm_record.API.vM_memory_static_max in
		vm_compute_required_memory vm_record
			(Memory.kib_of_bytes_used memory_required)

(** Calculates the amount of memory required in both 'normal' and 'shadow'
memory, for a running VM. If the VM is currently subject to a memory balloon
operation, this function returns the maximum amount of memory that the VM will
need between now, and the point in future time when the operation completes. *)
let vm_compute_used_memory ~__context policy vm_ref =
	if Xapi_fist.disable_memory_checks () then 0L else
	let vm_main_record = Db.VM.get_record ~__context ~self:vm_ref in
	let vm_boot_record = Helpers.get_boot_record ~__context ~self:vm_ref in

	let memory_required = choose_memory_required
		~policy: policy
		~ballooning_enabled: (Helpers.ballooning_enabled_for_vm ~__context vm_boot_record)
		~memory_dynamic_min: vm_main_record.API.vM_memory_dynamic_min
		(* ToDo: Is vm_main_record or vm_boot_record the right thing for dynamic_max? *)
		~memory_dynamic_max: vm_main_record.API.vM_memory_dynamic_max 
		~memory_static_max:  vm_boot_record.API.vM_memory_static_max in
	memory_required +++ vm_main_record.API.vM_memory_overhead

let vm_compute_resume_memory ~__context vm_ref =
	if Xapi_fist.disable_memory_checks () then 0L else
	let vm_boot_record = Helpers.get_boot_record ~__context ~self:vm_ref in
	let (_, shadow_bytes) = vm_compute_required_memory
		vm_boot_record vm_boot_record.API.vM_memory_static_max in
	(* CA-31759: use the live target field for this *)
	(* rather than the LBR to make upgrade easy.    *)
	let suspended_memory_usage_bytes =
		Db.VM.get_memory_target ~__context ~self:vm_ref in
	Int64.add suspended_memory_usage_bytes shadow_bytes

let vm_compute_migrate_memory ~__context vm_ref =
	if Xapi_fist.disable_memory_checks () then 0L else
	let vm_record = Db.VM.get_record ~__context ~self:vm_ref in
	let (_, shadow_bytes) = vm_compute_required_memory
		vm_record vm_record.API.vM_memory_static_max in
	(* Only used when in rolling upgrade mode (from a pre-ballooning product) *)
	let current_memory_usage_bytes = vm_record.API.vM_memory_static_max in
	Int64.add current_memory_usage_bytes shadow_bytes

(**
	The Pool master's view of the total memory and memory consumers on a host.
	This doesn't take into account dynamic changes i.e. those caused by
	ballooning. Therefore if we ask a question like, 'is there <x> amount of
	memory free to boot VM <y>' we will get one of 3 different answers:
	1. yes:
		the sum of the static_max's of all VMs with domains + the request
		is less than the total free.
	2. maybe:
		depending on the behaviour of the balloon drivers in the guest we
		may be able to free the memory.
	3. no:
		the sum of the dynamic_min's of all the VMs with domains + the
		request is more than the total free.
*)
type host_memory_summary = {
	(** The maximum amount of memory that guests can use on this host. *)
	host_maximum_guest_memory_bytes: int64;
	(** list of VMs which have a domain running here *)
	resident: API.ref_VM list;
	(** list of VMs which are in the process of having a domain created here *)
	scheduled: API.ref_VM list;
}

open Db_filter_types

(** Return a host's memory summary from live database contents. *)
let get_host_memory_summary ~__context ~host =
	let metrics = Db.Host.get_metrics ~__context ~self:host in
	let host_memory_total_bytes =
		Db.Host_metrics.get_memory_total ~__context ~self:metrics in
	let host_memory_overhead_bytes =
		Db.Host.get_memory_overhead ~__context ~self:host in
	let host_maximum_guest_memory_bytes =
		host_memory_total_bytes --- host_memory_overhead_bytes in
	let resident = Db.VM.get_refs_where ~__context
		~expr:(Eq (Field "resident_on", Literal (Ref.string_of host))) in
	let scheduled = Db.VM.get_refs_where ~__context
		~expr:(Eq (Field "scheduled_to_be_resident_on", Literal (
			Ref.string_of host))) in
	{
		host_maximum_guest_memory_bytes = host_maximum_guest_memory_bytes;
		resident = resident;
		scheduled = scheduled;
	}

(**
	Given a host's memory summary and a policy flag (i.e. whether to only
	consider static_max or to consider dynamic balloon data) it returns the
	amount of free memory on the host.
*)
let host_compute_free_memory_with_policy ~__context summary policy =
	let all_vms = summary.resident @ summary.scheduled in
	let all_vm_memories = List.map (vm_compute_used_memory ~__context policy)
		all_vms in
	let total_vm_memory = List.fold_left Int64.add 0L all_vm_memories in
	let host_mem_available = Int64.sub
		summary.host_maximum_guest_memory_bytes total_vm_memory in
	max 0L host_mem_available

(**
	Compute, from our managed data, how much memory is available on a host; this
	takes into account both VMs that are resident_on the host and also VMs that
	are scheduled_to_be_resident_on the host.

	If ignore_scheduled_vm is set then we do not consider this VM as having any
	resources allocated via the scheduled_to_be_resident_on mechanism. This is
	used to ensure that, when we're executing this function with a view to
	starting a VM, v, and further that v is scheduled_to_be_resident on the
	specified host, that we do not count the resources required for v twice.

	If 'dump_stats=true' then we write to the debug log where we think the
	memory is being used.
*)
let host_compute_free_memory_with_maximum_compression
		?(dump_stats=false) ~__context ~host
	ignore_scheduled_vm =
	(*
		Compute host free memory from what is actually running. Don't rely on
		reported free memory, since this is an asychronously-computed metric
		that's liable to change or be out of date.
	*)
	let summary = get_host_memory_summary ~__context ~host in
	(*
		When we're considering starting ourselves, and the host has reserved
		resources ready for us, then we need to make sure we don't count these
		reserved resources twice.
	*)
	let summary = { summary with scheduled = 
		match ignore_scheduled_vm with
		| None -> summary.scheduled (* no change *)
		| Some ignore_me ->
			List.filter (fun x -> x <> ignore_me) summary.scheduled
	} in
	let host_mem_available = host_compute_free_memory_with_policy
		~__context summary Dynamic_min (* consider ballooning *) in

	if dump_stats then begin
		let mib x = Int64.div (Int64.div x 1024L) 1024L in
		debug "Memory_check: total host memory: %Ld (%Ld MiB)"
			summary.host_maximum_guest_memory_bytes
			(mib summary.host_maximum_guest_memory_bytes);
		List.iter
			(fun v -> 
				let reqd = vm_compute_used_memory ~__context Static_max v in
				debug "Memory_check: VM %s (%s): memory %Ld (%Ld MiB)"
					(Db.VM.get_uuid ~__context ~self:v)
					(if List.mem v summary.resident
						then "resident here"
						else "scheduled to be resident here"
					)
					reqd (mib reqd)
			)
			(summary.scheduled @ summary.resident);
		debug "Memory_check: available memory: %Ld (%Ld MiB)"
			host_mem_available (mib host_mem_available)
	end;

	host_mem_available

let host_compute_memory_overhead ~__context ~host =
	(* We assume that the memory overhead of a host is constant with respect *)
	(* to time and simply fetch the existing cached value from the database. *)
	Db.Host.get_memory_overhead ~__context ~self:host

let vm_compute_memory_overhead snapshot =
	let static_max_bytes = snapshot.API.vM_memory_static_max in
	let static_max_mib = Memory.mib_of_bytes_used static_max_bytes in
	let multiplier = snapshot.API.vM_HVM_shadow_multiplier in
	let vcpu_count = Int64.to_int (snapshot.API.vM_VCPUs_max) in
	let memory_overhead_mib = (
		if Helpers.is_hvm snapshot
		then Memory.HVM.overhead_mib
		else Memory.Linux.overhead_mib)
			static_max_mib vcpu_count multiplier in
	Memory.bytes_of_mib memory_overhead_mib
