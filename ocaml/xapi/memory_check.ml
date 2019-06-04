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
module D=Debug.Make(struct let name="memory_check" end)
open D

let ( +++ ) = Int64.add
let ( --- ) = Int64.sub
let ( *** ) = Int64.mul
let ( /// ) = Int64.div

(** Calculates the amounts of 'normal' and 'shadow' host memory needed *)
(** to run the given guest with the given amount of guest memory.      *)
let vm_compute_required_memory vm_record guest_memory_kib =
  let vcpu_count = Int64.to_int vm_record.API.vM_VCPUs_max in
  let target_mib = Memory.mib_of_kib_used guest_memory_kib in
  let max_mib = Memory.mib_of_bytes_used vm_record.API.vM_memory_static_max in
  let video_mib = 0 in (* unused in this function *)
  let multiplier, full_config =
    match Helpers.check_domain_type vm_record.API.vM_domain_type with
    | `hvm       -> vm_record.API.vM_HVM_shadow_multiplier, Memory.HVM.full_config
    | `pv_in_pvh -> vm_record.API.vM_HVM_shadow_multiplier, Memory.PVinPVH.full_config
    | `pv        -> Memory.Linux.shadow_multiplier_default, Memory.Linux.full_config
  in
  let memory = full_config max_mib video_mib target_mib vcpu_count multiplier in
  let footprint_mib = memory.Memory.required_host_free_mib in
  let shadow_mib = memory.Memory.shadow_mib in
  let normal_mib = footprint_mib --- shadow_mib in
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
let choose_memory_required ~policy ~memory_dynamic_min ~memory_dynamic_max ~memory_static_max =
  match policy with
  | Dynamic_min -> memory_dynamic_min
  | Dynamic_max -> memory_dynamic_max
  | Static_max -> memory_static_max

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
    let vm_record = Db.VM.get_record ~__context ~self:vm_ref in

    let memory_required = choose_memory_required
        ~policy: policy
        ~memory_dynamic_min: vm_record.API.vM_memory_dynamic_min
        (* ToDo: Is vm_main_record or vm_boot_record the right thing for dynamic_max? *)
        ~memory_dynamic_max: vm_record.API.vM_memory_dynamic_max
        ~memory_static_max:  vm_record.API.vM_memory_static_max in
    memory_required +++ vm_record.API.vM_memory_overhead

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

let vm_compute_memory_overhead ~vm_record =
  let static_max_bytes = vm_record.API.vM_memory_static_max in
  let static_max_mib = Memory.mib_of_bytes_used static_max_bytes in
  let multiplier = vm_record.API.vM_HVM_shadow_multiplier in
  let vcpu_count = Int64.to_int (vm_record.API.vM_VCPUs_max) in
  let model =
    match Helpers.check_domain_type vm_record.API.vM_domain_type with
    | `hvm       -> Memory.HVM.overhead_mib
    | `pv_in_pvh -> Memory.PVinPVH.overhead_mib
    | `pv        -> Memory.Linux.overhead_mib
  in
  model static_max_mib vcpu_count multiplier |>
  Memory.bytes_of_mib
