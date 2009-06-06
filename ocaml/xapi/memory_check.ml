module D=Debug.Debugger(struct let name="xapi" end)
open D

(** Calculates the amounts of 'normal' and 'shadow' host memory needed *)
(** to run the given HVM guest with the given amount of guest memory.  *)
let vm_compute_required_memory_for_hvm vm_record guest_memory_kib =
	let vcpus = Int64.to_int vm_record.API.vM_VCPUs_max in
	let multiplier = vm_record.API.vM_HVM_shadow_multiplier in
	let normal = Memory.bytes_of_kib (Memory.HVM.required_available guest_memory_kib) in
	let shadow = Memory.bytes_of_kib (Memory.HVM.required_shadow vcpus guest_memory_kib multiplier) in
	(normal, shadow)

(** Calculates the amounts of 'normal' and 'shadow' host memory needed *)
(** to run the given PV guest with the given amount of guest memory.   *)
let vm_compute_required_memory_for_pv vm_record guest_memory_kib =
	let normal = Memory.bytes_of_kib (Memory.Linux.required_available guest_memory_kib) in
	let shadow = 0L in
	(normal, shadow)

(** Calculates the amounts of 'normal' and 'shadow' host memory needed *)
(** to run the given guest with the given amount of guest memory.      *)
let vm_compute_required_memory vm_record guest_memory_kib =
	if Helpers.is_hvm vm_record
		then vm_compute_required_memory_for_hvm vm_record guest_memory_kib
		else vm_compute_required_memory_for_pv  vm_record guest_memory_kib

(** Calculates the amount of system memory required in both 'normal' and 'shadow'  *)
(** memory, to start a VM. If the given VM is a PV guest and if memory ballooning  *)
(** is enabled, this function returns values derived from the VM's dynamic memory  *)
(** target (since PV guests are able to start in a pre-ballooned state). If memory *)
(** ballooning is not enabled or if the VM is an HVM guest, this function returns  *)
(** values derived from the VM's static memory maximum (since currently HVM guests *)
(** are not able to start in a pre-ballooned state).                               *)
let vm_compute_start_memory ~__context vm_record =
	if Xapi_fist.disable_memory_checks () then (0L, 0L) else
	let memory_static_max = vm_record.API.vM_memory_static_max in
	let ballooning_enabled = Helpers.ballooning_enabled_for_vm ~__context vm_record in
	let memory_target = vm_record.API.vM_memory_target in
	let memory_required = if Helpers.is_hvm vm_record
		then memory_static_max
		else if ballooning_enabled
			then memory_target
			else memory_static_max in
	vm_compute_required_memory vm_record (Memory.kib_of_bytes_used memory_required)

(** Calculates the amount of system memory required in both 'normal' and *)
(** 'shadow' memory, for a running VM. If the VM is currently subject to *)
(** a memory balloon operation, this function returns the maximum amount *)
(** of memory that the VM will need between now, and the point in future *)
(** time when the operation completes.                                   *)
let vm_compute_used_memory ~__context vm_ref =
	if Xapi_fist.disable_memory_checks () then (0L, 0L) else
	(* Fetch records relating to the current memory state of the given VM:                   *)
	(*   + the VM main record holds current values of memory_{dynamic_{min,max},target};     *)
	(*   + the VM boot record holds current values of memory_static_{min,max};               *)
	(*   + the VM metrics record holds the asynchronously-calculated value of memory_actual. *)
	let vm_main_record = Db.VM.get_record ~__context ~self:vm_ref in
	let vm_boot_record = Helpers.get_boot_record ~__context ~self:vm_ref in
	let vm_metrics_ref = vm_main_record.API.vM_metrics in
	let vm_metrics_record = Db.VM_metrics.get_record ~__context ~self:vm_metrics_ref in
	(* Extract the current memory state from the fetched records: *)
	let memory_static_max = vm_boot_record.API.vM_memory_static_max in
	let memory_target = vm_main_record.API.vM_memory_target in
	let memory_actual = vm_metrics_record.API.vM_metrics_memory_actual in
	(* Consider the case where ballooning is enabled, by taking the maximum  *)
	(* of the current target and the asynchronously calculated memory usage: *)
	let ballooning_enabled = Helpers.ballooning_enabled_for_vm ~__context vm_boot_record in
	let memory_required =
		if ballooning_enabled then max memory_target memory_actual
		else memory_static_max in
	vm_compute_required_memory vm_boot_record (Memory.kib_of_bytes_used memory_required)

(* Compute, from our managed data, how much memory is available on a host; this takes into account
   both VMs that are resident_on the host and also VMs that are scheduled_to_be_resident_on the host.

   If ignore_scheduled_vm is set then we do not consider this VM as having any resources allocated
   via the scheduled_to_be_resident_on mechanism. This is used to ensure that, when we're executing
   this function with a view to starting a VM, v, and further that v is scheduled_to_be_resident on
   the specified host, that we do not count the resources required for v twice..

   If 'dump_stats=true' then we write to the debug log where we think the memory is being used.
*)
let host_compute_free_memory ?(dump_stats=false) ~__context ~host ignore_scheduled_vm =
  (* Compute host free memory from what is actually running. Don't rely on reported
     free memory, since this is an asychronously computed metric that's liable to change or
     be out of date *)
  let host_mem_total =
    try Db.Host.get_boot_free_mem ~__context ~self:host
    with _ -> raise (Api_errors.Server_error (Api_errors.host_cannot_read_metrics, [])) in

  (* Get all the domains, including the control domain. *)
  let domains = (Db.VM.get_all ~__context) in

  (* consider resident_on and scheduled_to_be_resident_on independently: 
     a VM migrating to localhost will be in both sets and should be counted twice *)
  let resident_on =
    List.filter (fun self -> Db.VM.get_resident_on ~__context ~self = host)
      domains in
  let scheduled_to_be_resident_on =
    List.filter (fun self -> Db.VM.get_scheduled_to_be_resident_on ~__context ~self = host)
      domains in

  (* When we're considering starting ourselves, and the host has reserved resources ready for us,
     then we need to make sure we don't count these reserved resources twice.. *)
  let scheduled_to_be_resident_on =
    match ignore_scheduled_vm with
      None -> scheduled_to_be_resident_on (* no change *)
    | Some ignore_me ->
	List.filter (fun x -> x <> ignore_me) scheduled_to_be_resident_on in

  let all_vms = resident_on @ scheduled_to_be_resident_on in
  
  let total_running_vm_memory =
    List.fold_left Int64.add 0L ((List.map (fun (x, y) -> Int64.add x y) (List.map (vm_compute_used_memory ~__context) all_vms))) in
  let host_mem_available = Int64.sub host_mem_total total_running_vm_memory in

  if dump_stats then begin
    let mib x = Int64.div (Int64.div x 1024L) 1024L in
    debug "Memory_check: total host memory: %Ld (%Ld MiB)" host_mem_total (mib host_mem_total);
    List.iter
      (fun v -> 
	 let main, shadow = vm_compute_used_memory ~__context v in
	 debug "Memory_check: VM %s (%s): main memory %Ld (%Ld MiB); shadow memory %Ld (%Ld MiB)" 
	   (Db.VM.get_uuid ~__context ~self:v)
	   (if List.mem v resident_on then "resident here" else "scheduled to be resident here")
	   main (mib main) shadow (mib shadow)) all_vms;
    debug "Memory_check: total guest memory: %Ld (%Ld MiB)" 
      total_running_vm_memory (mib total_running_vm_memory);
    debug "Memory_check: available memory: %Ld (%Ld MiB)"
      host_mem_available (mib host_mem_available)
  end;
  
  host_mem_available

