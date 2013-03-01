(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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
open Printf
open Stringext
open Pervasiveext
open Xapi_vm_helpers
open Client
open Threadext
open Xmlrpc_sexpr
open Listext
open Xenstore

(* Notes re: VM.{start,resume}{on,}:
 * Until we support pools properly VM.start and VM.start_on both try
   to boot/resume the VM on this host.
 * If VM.{start,resume}_on is supplied another host reference, they will fail.
 *)

module D = Debug.Debugger(struct let name="xapi" end)
open D

exception InvalidOperation of string

let assert_operation_valid = Xapi_vm_lifecycle.assert_operation_valid

let update_allowed_operations ~__context ~self =
  Helpers.log_exn_continue "updating allowed operations of VBDs/VIFs/VDIs in VM.update_allowed_operations"
    (fun () ->
       List.iter
         (fun vbd ->
            Xapi_vbd_helpers.update_allowed_operations ~__context ~self:vbd;
            try
              if not(Db.VBD.get_empty ~__context ~self:vbd)
              then Xapi_vdi.update_allowed_operations ~__context ~self:(Db.VBD.get_VDI ~__context ~self:vbd)
            with _ -> ()) (Db.VM.get_VBDs ~__context ~self);
       List.iter
         (fun vif ->
            Xapi_vif_helpers.update_allowed_operations ~__context ~self:vif)
         (Db.VM.get_VIFs ~__context ~self)
    ) ();
  Xapi_vm_lifecycle.update_allowed_operations ~__context ~self

let assert_can_boot_here ~__context ~self ~host =
	let snapshot = Db.VM.get_record ~__context ~self in
	if Helpers.rolling_upgrade_in_progress ~__context then
		Helpers.assert_platform_version_is_same_on_master ~__context ~host ~self;
	assert_can_boot_here ~__context ~self ~host ~snapshot ()

let retrieve_wlb_recommendations ~__context ~vm =
  let snapshot = Db.VM.get_record ~__context ~self:vm in
  retrieve_wlb_recommendations ~__context ~vm ~snapshot

let assert_agile ~__context ~self = Helpers.vm_assert_agile ~__context ~self

(* helpers *)
let immediate_complete ~__context   =
	Helpers.progress ~__context  (0.0 -. 1.0)

(* API *)
let set_actions_after_shutdown ~__context ~self ~value =
	Db.VM.set_actions_after_shutdown ~__context ~self ~value
let set_actions_after_reboot ~__context ~self ~value =
	Db.VM.set_actions_after_reboot ~__context ~self ~value
let set_actions_after_crash ~__context ~self ~value =
	set_actions_after_crash ~__context ~self ~value
let set_is_a_template ~__context ~self ~value =
	set_is_a_template ~__context ~self ~value

let validate_restart_priority priority =
	if not(List.mem priority Constants.ha_valid_restart_priorities) then
		raise (Api_errors.Server_error(Api_errors.invalid_value, ["ha_restart_priority"; priority]))

let set_ha_restart_priority ~__context ~self ~value =
	validate_restart_priority value;
	let current = Db.VM.get_ha_restart_priority ~__context ~self in
	if true
		&& current <> Constants.ha_restart
		&& value = Constants.ha_restart then begin
			Xapi_ha_vm_failover.assert_new_vm_preserves_ha_plan ~__context self;
			let pool = Helpers.get_pool ~__context in
			if Db.Pool.get_ha_enabled ~__context ~self:pool then
				let (_: bool) = Xapi_ha_vm_failover.update_pool_status ~__context in ()
		end;

	if current <> value then begin
		Db.VM.set_ha_restart_priority ~__context ~self ~value;
		(* If the VM is running then immediately turn on or off "protection"
		   for the VM by setting ha_always_run *)
		if Db.VM.get_power_state ~__context ~self = `Running
		then Db.VM.set_ha_always_run ~__context ~self ~value:(value = Constants.ha_restart)
	end


(* Field deprecated since Boston - attempt to degrade gracefully if anything sets it. *)
let set_ha_always_run ~__context ~self ~value =
	if value then
		set_ha_restart_priority ~__context ~self ~value:Constants.ha_restart
	else
		set_ha_restart_priority ~__context ~self ~value:""

let compute_memory_overhead = compute_memory_overhead

open Xapi_vm_memory_constraints

let set_memory_static_range ~__context ~self ~min ~max =
	(* Called on the master only when the VM is offline *)
	if Db.VM.get_power_state ~__context ~self <> `Halted
	then failwith "assertion_failed: set_memory_static_range should only be \
		called when the VM is Halted";
	(* Check the range constraints *)
	let constraints = Vm_memory_constraints.get ~__context ~vm_ref:self in
	let constraints = {constraints with Vm_memory_constraints.
		static_min = min;
		static_max = max;
	} in
	Vm_memory_constraints.assert_valid_for_current_context
		~__context ~vm:self ~constraints;
	Db.VM.set_memory_static_min ~__context ~self ~value:min;
	Db.VM.set_memory_static_max ~__context ~self ~value:max;
	update_memory_overhead ~__context ~vm:self

(* These are always converted into set_memory_dynamic_range *)
(* by the message forwarding layer:                         *)
let set_memory_dynamic_min ~__context ~self ~value = assert false
let set_memory_dynamic_max ~__context ~self ~value = assert false
(* These are always converted into set_memory_static_range *)
(* by the message forwarding layer:                        *)
let set_memory_static_min ~__context ~self ~value = assert false
let set_memory_static_max ~__context ~self ~value = assert false

let set_memory_limits ~__context ~self
	~static_min ~static_max ~dynamic_min ~dynamic_max =
	(* Called on the master only when the VM is halted. *)
	if Db.VM.get_power_state ~__context ~self <> `Halted
	then failwith "assertion_failed: set_memory_limits should only be \
		called when the VM is Halted";
	(* Check that the new limits are in the correct order. *)
	let constraints = {Vm_memory_constraints.
		static_min  = static_min;
		dynamic_min = dynamic_min;
		target      = dynamic_min;
		dynamic_max = dynamic_max;
		static_max  = static_max;
	} in
	Vm_memory_constraints.assert_valid_for_current_context
		~__context ~vm:self ~constraints;
	Vm_memory_constraints.set ~__context ~vm_ref:self ~constraints;
	update_memory_overhead ~__context ~vm:self

(* CA-12940: sanity check to make sure this never happens again *)
let assert_power_state_is ~__context ~vm ~expected =
  let actual = Db.VM.get_power_state ~__context ~self:vm in
  if actual <> expected
  then raise (Api_errors.Server_error(Api_errors.vm_bad_power_state,
				      [ Ref.string_of vm;
					Record_util.power_to_string expected;
					Record_util.power_to_string actual ]))

(* If HA is enabled on the Pool and the VM is marked as always_run then block the action *)
let assert_not_ha_protected ~__context ~vm =
  let pool = Helpers.get_pool ~__context in
  let always_run = Db.VM.get_ha_always_run ~__context ~self:vm in
  let priority = Db.VM.get_ha_restart_priority ~__context ~self:vm in
  if Db.Pool.get_ha_enabled ~__context ~self:pool && (Helpers.vm_should_always_run always_run priority)
  then raise (Api_errors.Server_error(Api_errors.vm_is_protected, [ Ref.string_of vm ]))

let pause ~__context ~vm =
	Xapi_xenops.pause ~__context ~self:vm

let unpause ~__context ~vm =
  License_check.with_vm_license_check ~__context vm
    (fun () ->
		Xapi_xenops.unpause ~__context ~self:vm
	)

let set_xenstore_data ~__context ~self ~value =
	Xapi_xenops.set_xenstore_data ~__context ~self value

(* Note: it is important that we use the pool-internal API call, VM.atomic_set_resident_on, to set resident_on and clear
   scheduled_to_be_resident_on atomically. This prevents concurrent API calls on the master from accounting for the
   same VM twice during memory calculations to determine whether a given VM can start on a particular host..
*)

let start ~__context ~vm ~start_paused ~force =
	License_check.with_vm_license_check ~__context vm
		(fun () ->
			let vmr = Db.VM.get_record ~__context ~self:vm in
			Vgpuops.create_vgpus ~__context (vm, vmr) (Helpers.will_boot_hvm ~__context ~self:vm);

			if vmr.API.vM_ha_restart_priority = Constants.ha_restart
			then begin
				Xapi_ha_vm_failover.assert_new_vm_preserves_ha_plan ~__context vm;
				Db.VM.set_ha_always_run ~__context ~self:vm ~value:true
			end;
			Xapi_xenops.start ~__context ~self:vm start_paused
		)

(** For VM.start_on and VM.resume_on the message forwarding layer should only forward here
    if 'host' = localhost *)
let assert_host_is_localhost ~__context ~host =
	let localhost = Helpers.get_localhost ~__context in
	if host <> localhost then
	  let msg = "Error in message forwarding layer: host parameter was not localhost" in
	  raise (Api_errors.Server_error (Api_errors.internal_error, [ msg ]))

let start_on  ~__context ~vm ~host ~start_paused ~force =
	(* If we modify this to support start_on other-than-localhost,
	   insert a precheck to insure that we're starting on an
	   appropriately versioned host during an upgrade, as per
	   PR-1007. See the first lines of resume above *)
	assert_host_is_localhost ~__context ~host;
	start ~__context ~vm ~start_paused ~force

let hard_reboot ~__context ~vm =
	License_check.with_vm_license_check ~__context vm
		(fun () ->
			Xapi_xenops.reboot ~__context ~self:vm None
		)

let hard_shutdown ~__context ~vm =
	Db.VM.set_ha_always_run ~__context ~self:vm ~value:false;
    if Db.VM.get_power_state ~__context ~self:vm = `Suspended then begin
		debug "hard_shutdown: destroying any suspend VDI";
		let vdi = Db.VM.get_suspend_VDI ~__context ~self:vm in
		if vdi <> Ref.null (* avoid spurious but scary messages *)
		then Helpers.log_exn_continue
			(Printf.sprintf "destroying suspend VDI: %s" (Ref.string_of vdi))
			(Helpers.call_api_functions ~__context)
			(fun rpc session_id -> Client.VDI.destroy rpc session_id vdi);
		(* Whether or not that worked, forget about the VDI *)
		Db.VM.set_suspend_VDI ~__context ~self:vm ~value:Ref.null;
		Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted;
    end else 
	Xapi_xenops.shutdown ~__context ~self:vm None

let clean_reboot ~__context ~vm =
	License_check.with_vm_license_check ~__context vm
		(fun () ->
			Xapi_xenops.reboot ~__context ~self:vm (Some 1200.0)
		)

let clean_shutdown ~__context ~vm =
	Db.VM.set_ha_always_run ~__context ~self:vm ~value:false;
	Xapi_xenops.shutdown ~__context ~self:vm (Some 1200.0)

(***************************************************************************************)

(** @deprecated *)
let hard_reboot_internal ~__context ~vm = assert false

(***************************************************************************************)

let power_state_reset ~__context ~vm =
  (* CA-31428: Block if the VM is a control domain *)
  if Db.VM.get_is_control_domain ~__context ~self:vm then begin
    error "VM.power_state_reset vm=%s blocked because VM is a control domain" (Ref.string_of vm);
    raise (Api_errors.Server_error(Api_errors.cannot_reset_control_domain, [ Ref.string_of vm ]));
  end;
  (* Perform sanity checks if VM is Running or Paused since we don't want to
     lose track of running domains. *)
  let power_state = Db.VM.get_power_state ~__context ~self:vm in
  if power_state = `Running || power_state = `Paused then begin
    debug "VM.power_state_reset vm=%s power state is either running or paused: performing sanity checks" (Ref.string_of vm);
    let localhost = Helpers.get_localhost ~__context in
	let resident = Db.VM.get_resident_on ~__context ~self:vm in
	if resident = localhost then begin
		let open Xenops_interface in
		let open Xenops_client in
		let running =
			try
				let dbg = Context.string_of_task __context in
				let id = Db.VM.get_uuid ~__context ~self:vm in
				let _, s = Client.VM.stat dbg id in
				if s.Vm.power_state = Running then begin
					debug "VM.power_state_reset vm=%s xenopsd reports running;" (Ref.string_of vm);
					true
				end else begin
					(* Delete the metadata from xenopsd *)
					Xapi_xenops.Xenopsd_metadata.delete ~__context id;
					false
				end
			with _ -> false in
		if running then raise (Api_errors.Server_error(Api_errors.domain_exists, [ Ref.string_of vm ]))
    end else begin
      (* If resident on another host, check if that host is alive: if so
	 then refuse to perform the reset, since we have delegated state management
	 to this host and we trust it -- this call is intended for coping with
	 host failures and backup restores, not for working around agent bugs.
	 If the host agent software is malfunctioning, then it should be restarted
	 (via Host.restart_agent or 'service xapi restart') *)
      debug "VM.power_state_reset vm=%s resident_on<>localhost; checking liveness of remote host" (Ref.string_of vm);
      if Xapi_host.is_host_alive ~__context ~host:resident then begin
	error "VM.power_state_reset vm=%s resident_on=%s; host is alive so refusing to reset power-state"
	  (Ref.string_of vm) (Ref.string_of resident);
	raise (Api_errors.Server_error(Api_errors.host_is_live, [ Ref.string_of resident ]))
      end
    end
  end;

  Xapi_vm_lifecycle.force_state_reset ~__context ~value:`Halted ~self:vm

let suspend ~__context ~vm =
	Db.VM.set_ha_always_run ~__context ~self:vm ~value:false;
	Xapi_xenops.suspend ~__context ~self:vm

let resume ~__context ~vm ~start_paused ~force = 
	License_check.with_vm_license_check ~__context vm
		(fun () ->
			if Db.VM.get_ha_restart_priority ~__context ~self:vm = Constants.ha_restart
			then begin
				Xapi_ha_vm_failover.assert_new_vm_preserves_ha_plan ~__context vm;
				Db.VM.set_ha_always_run ~__context ~self:vm ~value:true
			end;

			let host = Helpers.get_localhost ~__context in
			if not force then Cpuid_helpers.assert_vm_is_compatible ~__context ~vm ~host ();

			Xapi_xenops.resume ~__context ~self:vm ~start_paused ~force
		)

let resume_on  ~__context ~vm ~host ~start_paused ~force =
	(* If we modify this to support resume_on other-than-localhost,
	   insert a precheck to insure that we're starting on an
	   appropriately versioned host during an upgrade, as per
	   PR-1007. See the first lines of resume above *)
	assert_host_is_localhost ~__context ~host;
	resume ~__context ~vm ~start_paused ~force

let create ~__context
		~name_label
		~name_description
		~user_version
		~is_a_template
		~affinity
		~memory_target
		~memory_static_max
		~memory_dynamic_max
		~memory_dynamic_min
		~memory_static_min
		~vCPUs_params
		~vCPUs_max
		~vCPUs_at_startup
		~actions_after_shutdown
		~actions_after_reboot
		~actions_after_crash
		~pV_bootloader
		~pV_kernel
		~pV_ramdisk
		~pV_args
		~pV_bootloader_args
		~pV_legacy_args
		~hVM_boot_policy
		~hVM_boot_params
		~hVM_shadow_multiplier
		~platform
		~pCI_bus
		~other_config
		~recommendations
		~xenstore_data
		~ha_always_run
		~ha_restart_priority
		~tags
		~blocked_operations
		~protection_policy
		~is_snapshot_from_vmpp
		~appliance
		~start_delay
		~shutdown_delay
		~order
		~suspend_SR
		~version
		: API.ref_VM =
	let gen_mac_seed () = Uuid.to_string (Uuid.make_uuid ()) in
	(* Add random mac_seed if there isn't one specified already *)
	let other_config =
		if not (List.mem_assoc Xapi_globs.mac_seed other_config)
		then (Xapi_globs.mac_seed, gen_mac_seed ()) :: other_config
		else other_config in
	create ~__context
		~name_label
		~name_description
		~user_version
		~is_a_template
		~affinity
		~memory_target
		~memory_static_max
		~memory_dynamic_max
		~memory_dynamic_min
		~memory_static_min
		~vCPUs_params
		~vCPUs_max
		~vCPUs_at_startup
		~actions_after_shutdown
		~actions_after_reboot
		~actions_after_crash
		~pV_bootloader
		~pV_kernel
		~pV_ramdisk
		~pV_args
		~pV_bootloader_args
		~pV_legacy_args
		~hVM_boot_policy
		~hVM_boot_params
		~hVM_shadow_multiplier
		~platform
		~pCI_bus
		~other_config
		~recommendations
		~xenstore_data
		~ha_always_run
		~ha_restart_priority
		~tags
		~blocked_operations
		~protection_policy
		~is_snapshot_from_vmpp
		~appliance
		~start_delay
		~shutdown_delay
		~order
		~suspend_SR
		~version

let destroy  ~__context ~self =
	let parent = Db.VM.get_parent ~__context ~self in

	(* rebase the children *)
	List.iter
		(fun child -> try Db.VM.set_parent ~__context ~self:child ~value:parent with _ -> ())
		(Db.VM.get_children ~__context ~self);

	log_and_ignore_exn (Rrdd.remove_rrd ~uuid:(Db.VM.get_uuid ~__context ~self));
	destroy ~__context ~self

(* Note: we don't need to call lock_vm around clone or copy. The lock_vm just takes the local
   lock on a specific pool host and is used to manage contention between API threads and the
   event monitoring thread on live VMs. Since clone does not deal with live VMs we ommit lock_vm. *)

let clone ~__context ~vm ~new_name =
  TaskHelper.set_cancellable ~__context;
  (* !!! Note - please do not be tempted to put this on the "long_running_queue", even though it may be long
     running.. XenRT relies on fast clones being parallelizable wrt other long-running ops such as
     suspend/resume/migrate etc. *)
  (* Now that clones are "fast", there's no need to put this operation in the "normal_vm_queue". Indeed,
     putting it in there would mean that clones are serialized on a host-basis whereas they may be able
     to proceed in parallel. *)
	let new_vm = Xapi_vm_clone.clone Xapi_vm_clone.Disk_op_clone ~__context ~vm ~new_name in
	if Db.VM.get_is_a_snapshot ~__context ~self:vm && Db.VM.get_power_state ~__context ~self:new_vm <> `Halted then
		hard_shutdown ~__context ~vm:new_vm;
	new_vm

(* We do call wait_in_line for snapshot and snapshot_with_quiesce because the locks are taken at *)
(* the VBD level (with pause/unpause mechanism                                                   *)
let snapshot ~__context ~vm ~new_name =
	TaskHelper.set_cancellable ~__context;
	Xapi_vm_snapshot.snapshot ~__context ~vm ~new_name

(* Snapshot_with_quiesce triggers the VSS plugin which will then calls the VM.snapshot API call.     *)
(* Thus, to avoid dead-locks, do not put snapshot and snapshot_with_quiesce on the same waiting line *)
let snapshot_with_quiesce ~__context ~vm ~new_name =
	TaskHelper.set_cancellable ~__context;
	Xapi_vm_snapshot.snapshot_with_quiesce ~__context ~vm ~new_name

(* As we will destroy the domain ourself, we grab the vm_lock here in order to tell the event thread to *)
(* do not look at this domain. The message forwarding layer already checked that the VM reference we    *)
(* revert too is still valid. *)
let revert ~__context ~snapshot =
	let vm = Db.VM.get_snapshot_of ~__context ~self:snapshot in
	let vm =
		if Db.is_valid_ref __context vm
		then vm
		else Xapi_vm_snapshot.create_vm_from_snapshot ~__context ~snapshot in
	Xapi_vm_snapshot.revert ~__context ~snapshot ~vm

(* As the checkpoint operation modify the domain state, we take the vm_lock to do not let the event *)
(* thread mess around with that. *)
let checkpoint ~__context ~vm ~new_name =
	if not (Pool_features.is_enabled ~__context Features.Checkpoint) then
		raise (Api_errors.Server_error(Api_errors.license_restriction, []))
	else begin
		Local_work_queue.wait_in_line Local_work_queue.long_running_queue
			(Printf.sprintf "VM.checkpoint %s" (Context.string_of_task __context))
			(fun () ->
				TaskHelper.set_cancellable ~__context;
				Xapi_vm_snapshot.checkpoint ~__context ~vm ~new_name
			)
	end

let copy ~__context ~vm ~new_name ~sr =
	(* See if the supplied SR is suitable: it must exist and be a non-ISO SR *)
	(* First the existence check. It's not an error to not exist at all. *)
	let sr = try ignore(Db.SR.get_uuid ~__context ~self:sr); Some sr with _ -> None in
	maybe (fun sr -> debug "Copying disks to SR: %s" (Db.SR.get_uuid ~__context ~self:sr)) sr;
	(* Second the non-iso check. It is an error to be an iso SR *)
	maybe (fun sr -> if Db.SR.get_content_type ~__context ~self:sr = "iso"
	       then raise (Api_errors.Server_error(Api_errors.operation_not_allowed,
						   [ "Cannot copy a VM's disks to an ISO SR" ]))) sr;
	Local_work_queue.wait_in_line Local_work_queue.long_running_queue
	  (Printf.sprintf "VM.copy %s" (Context.string_of_task __context))
	  (fun () ->
		let new_vm = Xapi_vm_clone.clone (Xapi_vm_clone.Disk_op_copy sr) ~__context ~vm ~new_name in
		if Db.VM.get_is_a_snapshot ~__context ~self:vm && Db.VM.get_power_state ~__context ~self:new_vm <> `Halted then
			hard_shutdown ~__context ~vm:new_vm;
		new_vm
	  )

let provision ~__context ~vm =
	Local_work_queue.wait_in_line Local_work_queue.long_running_queue
	  (Printf.sprintf "VM.provision %s" (Context.string_of_task __context))
	  (fun () ->
	(* This bit could be done in the guest: *)
	debug "start: checking to see whether VM needs 'installing'";
	Helpers.call_api_functions ~__context (fun rpc session_id ->
	     set_is_a_template ~__context ~self:vm ~value:false;
	     if Xapi_templates.needs_to_be_installed rpc session_id vm
	     then begin
	       TaskHelper.set_progress ~__context 0.1;
	       debug "install: phase 1/3: creating VBDs and VDIs";
	       let script, vbds = Xapi_templates.pre_install rpc session_id vm in
	       (* If an error occurs after this then delete the created VDIs, VBDs... *)
	       begin
		 try
		   debug "install: phase 2/3: running optional script (in domain 0)";
		   let dom0 = Helpers.get_domain_zero __context in
		   Xapi_templates_install.post_install_script rpc session_id __context dom0 vm (script, vbds);
		   debug "install: phase 3/3: removing install information from VM";
		   Xapi_templates.post_install rpc session_id vm;
		   debug "finished install";
		 with e ->
		   (* On error delete the VBDs and their associated VDIs *)
		   let vdis = List.map (fun self -> Client.VBD.get_VDI rpc session_id self) vbds in
		   List.iter (Helpers.log_exn_continue "deleting auto-provisioned VBD"
				 (fun self -> Client.VBD.destroy rpc session_id self)) vbds;
		   List.iter (Helpers.log_exn_continue "deleting auto-provisioned VDI"
				 (fun self -> Client.VDI.destroy rpc session_id self)) vdis;
		   raise e
	       end
	     end)
	  )

(** Sets the maximum number of VCPUs for a {b Halted} guest. *)
let set_VCPUs_max ~__context ~self ~value =
	if Db.VM.get_power_state ~__context ~self <> `Halted
	then failwith "assertion_failed: set_VCPUs_max should only be \
		called when the VM is Halted";
	let vcpus_at_startup = Db.VM.get_VCPUs_at_startup ~__context ~self in
	if value < 1L || value < vcpus_at_startup then invalid_value
		"VCPU values must satisfy: 0 < VCPUs_at_startup ≤ VCPUs_max"
		(Int64.to_string value);
	Db.VM.set_VCPUs_max ~__context ~self ~value;
	update_memory_overhead ~__context ~vm:self

(** Sets the number of startup VCPUs for a {b Halted} guest. *)
let set_VCPUs_at_startup ~__context ~self ~value =
	let vcpus_max = Db.VM.get_VCPUs_max ~__context ~self in
	if value < 1L || value > vcpus_max then invalid_value
		"VCPU values must satisfy: 0 < VCPUs_at_startup ≤ VCPUs_max"
		(Int64.to_string value);
	Db.VM.set_VCPUs_at_startup ~__context ~self ~value;
	update_memory_overhead ~__context ~vm:self

(** Sets the number of VCPUs for a {b Running} PV guest.
@raise Api_errors.operation_not_allowed if [self] is an HVM guest. *)
let set_VCPUs_number_live ~__context ~self ~nvcpu =
	Xapi_xenops.set_vcpus ~__context ~self nvcpu;
	(* Strictly speaking, PV guest memory overhead depends on the number of  *)
	(* vCPUs. Although our current overhead calculation uses a conservative  *)
	(* overestimate that ignores the real number of VCPUs, we still update   *)
	(* the overhead in case our level of conservativeness changes in future. *)
	update_memory_overhead ~__context ~vm:self

let add_to_VCPUs_params_live ~__context ~self ~key ~value =
	raise (Api_errors.Server_error (Api_errors.not_implemented, [ "add_to_VCPUs_params_live" ]))

(* Use set_memory_dynamic_range instead *)
let set_memory_target_live ~__context ~self ~target = ()

(** The default upper bound on the acceptable difference between *)
(** actual memory usage and target memory usage when waiting for *)
(** a running VM to reach its current memory target.             *)
let wait_memory_target_tolerance_bytes = Int64.(mul 1L (mul 1024L 1024L))

(** Returns true if (and only if) the   *)
(** specified argument is a power of 2. *)
let is_power_of_2 n =
	(n > 1) && (n land (0 - n) = n)

(** Waits for a running VM to reach its current memory target. *)
(** This function waits until the following condition is true: *)
(**                                                            *)
(**     abs (memory_actual - memory_target) <= tolerance       *)
(**                                                            *)
(** If the task associated with this function is cancelled or  *)
(** if the time-out counter exceeds its limit, this function   *)
(** raises a server error and terminates.                      *)
let wait_memory_target_live ~__context ~self =
	let timeout_seconds = int_of_float !Xapi_globs.wait_memory_target_timeout in
	let tolerance_bytes = wait_memory_target_tolerance_bytes in
	let raise_error error =
		raise (Api_errors.Server_error (error, [Ref.string_of (Context.get_task_id __context)])) in
	let open Xenops_client in
	let id = Xapi_xenops.id_of_vm ~__context ~self in
	let dbg = Context.string_of_task __context in
	let rec wait accumulated_wait_time_seconds =
		if accumulated_wait_time_seconds > timeout_seconds
			then raise_error Api_errors.vm_memory_target_wait_timeout;
		if TaskHelper.is_cancelling ~__context
			then raise_error Api_errors.task_cancelled;

		(* Fetch up-to-date value of memory_actual and memory_target *)
		let _, s = Client.VM.stat dbg id in
		let memory_target_bytes = s.Xenops_interface.Vm.memory_target in
		let memory_actual_bytes = s.Xenops_interface.Vm.memory_actual in

		let difference_bytes = Int64.abs (Int64.sub memory_actual_bytes memory_target_bytes) in
		debug "memory_actual = %Ld; memory_target = %Ld; difference = %Ld %s tolerance (%Ld)" memory_actual_bytes memory_target_bytes difference_bytes (if difference_bytes <= tolerance_bytes then "<=" else ">") tolerance_bytes;
		if difference_bytes <= tolerance_bytes then
			(* The memory target has been reached: use the most *)
			(* recent value of memory_actual to update the same *)
			(* field within the VM's metrics record, presenting *)
			(* a consistent view to the world.                  *)
			let vm_metrics_ref = Db.VM.get_metrics ~__context ~self in
			Db.VM_metrics.set_memory_actual ~__context ~self:vm_metrics_ref ~value:memory_actual_bytes
		else begin
			(* At exponentially increasing intervals, write  *)
			(* a debug message saying how long we've waited: *)
			if is_power_of_2 accumulated_wait_time_seconds then debug
				"Waited %i second(s) for VM %s to reach \
				its target = %Ld bytes; actual = %Ld bytes."
				accumulated_wait_time_seconds id
				memory_target_bytes memory_actual_bytes;
			(* The memory target has not yet been reached: *)
			(* wait for a while before repeating the test. *)
			Thread.delay 1.0;
			wait (accumulated_wait_time_seconds + 1)
		end
	in
	wait 0

(* Dummy implementation for a deprecated API method. *)
let get_cooperative ~__context ~self = true

let set_HVM_shadow_multiplier ~__context ~self ~value =
	set_HVM_shadow_multiplier ~__context ~self ~value

(** Sets the HVM shadow multiplier for a {b Running} VM. Runs on the slave. *)
let set_shadow_multiplier_live ~__context ~self ~multiplier =
	let power_state = Db.VM.get_power_state ~__context ~self in
	if power_state <> `Running
	then raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, [Ref.string_of self; "running"; (Record_util.power_to_string power_state)]));

	validate_HVM_shadow_multiplier multiplier;

	Xapi_xenops.set_shadow_multiplier ~__context ~self multiplier;
	update_memory_overhead ~__context ~vm:self

let set_memory_dynamic_range ~__context ~self ~min ~max = 
	(* NB called in either `Halted or `Running states *)
	let power_state = Db.VM.get_power_state ~__context ~self in
	(* Check the range constraints *)
	let constraints = 
		if power_state = `Running 
		then Vm_memory_constraints.get_live ~__context ~vm_ref:self 
		else Vm_memory_constraints.get ~__context ~vm_ref:self in
	let constraints = { constraints with Vm_memory_constraints.
		dynamic_min = min;
		target = min;
		dynamic_max = max } in
	Vm_memory_constraints.assert_valid_for_current_context
		~__context ~vm:self ~constraints;

	(* memory_target is now unused but setting it equal *)
	(* to dynamic_min avoids tripping validation code.  *)
	Db.VM.set_memory_target ~__context ~self ~value:min;
	Db.VM.set_memory_dynamic_min ~__context ~self ~value:min;
	Db.VM.set_memory_dynamic_max ~__context ~self ~value:max;

	if power_state = `Running
	then Xapi_xenops.set_memory_dynamic_range ~__context ~self min max

let send_sysrq ~__context ~vm ~key =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "send_sysrq" ]))

let send_trigger ~__context ~vm ~trigger =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "send_trigger" ]))

let get_boot_record ~__context ~self =
  Helpers.get_boot_record ~__context ~self

let get_data_sources ~__context ~self =
	List.map Data_source.to_API_data_source (Rrdd.query_possible_vm_dss ~vm_uuid:(Db.VM.get_uuid ~__context ~self))

let record_data_source ~__context ~self ~data_source =
	Rrdd.add_vm_ds ~vm_uuid:(Db.VM.get_uuid ~__context ~self)
		~domid:(Int64.to_int (Db.VM.get_domid ~__context ~self))
		~ds_name:data_source

let query_data_source ~__context ~self ~data_source = Rrdd.query_vm_ds ~vm_uuid:(Db.VM.get_uuid ~__context ~self) ~ds_name:data_source

let forget_data_source_archives ~__context ~self ~data_source = Rrdd.forget_vm_ds ~vm_uuid:(Db.VM.get_uuid ~__context ~self) ~ds_name:data_source

let get_possible_hosts ~__context ~vm =
  let snapshot = Db.VM.get_record ~__context ~self:vm in
  get_possible_hosts_for_vm ~__context ~vm ~snapshot

let get_allowed_VBD_devices ~__context ~vm = List.map (fun d -> string_of_int (Device_number.to_disk_number d)) (allowed_VBD_devices ~__context ~vm)
let get_allowed_VIF_devices = allowed_VIF_devices

(* Undocumented Rio message, deprecated in favour of standard VM.clone *)
let csvm ~__context ~vm =
  Xapi_vm_clone.clone ~__context  Xapi_vm_clone.Disk_op_clone ~vm
    ~new_name:(Db.VM.get_name_label ~__context ~self:vm ^ "-cloned-suspended")

(* XXX: NOT IN RIO *)
(** Return the largest possible static-max setting which will fit in a given amount of
    free physical memory. If 'approximate' is true then we return a more conservative value
    which allows for the number of vCPUs to be changed (for example).
    NB function is related to Vmops.check_enough_memory.
 *)
let maximise_memory ~__context ~self ~total ~approximate =
	let r = Db.VM.get_record ~__context ~self in
	let r = { r with API.vM_VCPUs_max = if approximate then 64L else r.API.vM_VCPUs_max } in

	(* Need to find the maximum input value to this function so that it still evaluates
       to true *)
	let will_fit static_max =
		let r = { r with API.vM_memory_static_max = static_max } in
		let normal, shadow = Memory_check.vm_compute_start_memory ~__context ~policy:Memory_check.Static_max r in
		Int64.add normal shadow <= total in

	let max = Helpers.bisect will_fit 0L total in
	(* Round down to the nearest MiB boundary... there's a slight mismatch between the
       boot_free_mem - sum(static_max) value and the results of querying the free pages in Xen.*)
	Int64.(mul (mul (div (div max 1024L) 1024L) 1024L) 1024L)

(* In the master's forwarding layer with the global forwarding lock *)
let atomic_set_resident_on ~__context ~vm ~host = assert false
let update_snapshot_metadata ~__context ~vm ~snapshot_of ~snapshot_time = assert false

let create_new_blob ~__context ~vm ~name ~mime_type ~public =
  let blob = Xapi_blob.create ~__context ~mime_type ~public in
  Db.VM.add_to_blobs ~__context ~self:vm ~key:name ~value:blob;
  blob

let s3_suspend ~__context ~vm = Xapi_xenops.s3suspend ~__context ~self:vm

let s3_resume ~__context ~vm = Xapi_xenops.s3resume ~__context ~self:vm

let copy_bios_strings = Xapi_vm_helpers.copy_bios_strings

let set_protection_policy ~__context ~self ~value =
	if value <> Ref.null then begin
		if Db.VM.get_is_control_domain ~__context ~self then
			(* do not assign vmpps to the dom0 vm of any host in the pool *)
			raise (Api_errors.Server_error(Api_errors.invalid_value, [Ref.string_of value]));
		if Db.VM.get_is_a_template ~__context ~self then
			(* Do not assign templates to a VMPP. *)
			raise (Api_errors.Server_error(Api_errors.vm_is_template, [Ref.string_of self]));
		(* if unlicensed, allow only to change to protection policy to null *)
		Xapi_vmpp.assert_licensed ~__context;
	end;
	Db.VM.set_protection_policy ~__context ~self ~value;
	update_allowed_operations ~__context ~self

let set_start_delay ~__context ~self ~value =
	if value < 0L then invalid_value
		"start_delay must be non-negative"
		(Int64.to_string value);
	Db.VM.set_start_delay ~__context ~self ~value

let set_shutdown_delay ~__context ~self ~value =
	if value < 0L then invalid_value
		"shutdown_delay must be non-negative"
		(Int64.to_string value);
	Db.VM.set_shutdown_delay ~__context ~self ~value

let set_order ~__context ~self ~value =
	if value < 0L then invalid_value
		"order must be non-negative"
		(Int64.to_string value);
	Db.VM.set_order ~__context ~self ~value

let assert_can_be_recovered ~__context ~self ~session_to =
	Xapi_vm_helpers.assert_can_be_recovered ~__context ~self ~session_to

let recover ~__context ~self ~session_to ~force =
	Xapi_dr.assert_session_allows_dr ~session_id:session_to ~action:"VM.recover";
	(* Check the VM SRs are available. *)
	assert_can_be_recovered ~__context ~self ~session_to;
	(* Attempt to recover the VM. *)
	ignore (Xapi_dr.recover_vms ~__context ~vms:[self] ~session_to ~force)

let set_suspend_VDI ~__context ~self ~value =
	let vm_state =  Db.VM.get_power_state ~__context ~self in
	if vm_state <> `Suspended then
		raise (Api_errors.Server_error(Api_errors.vm_bad_power_state,
		                               [Ref.string_of self; "suspended";
		                                Record_util.power_to_string vm_state]));
	let src_vdi = Db.VM.get_suspend_VDI ~__context ~self in
	let dst_vdi = value in
	if src_vdi <> dst_vdi then
	(*
	 * We don't care if the future host can see current suspend VDI or not, but
	 * we want to make sure there's at least a host can see all the VDIs of the
	 * VM + the new suspend VDI. We raise an exception if there's no suitable
	 * host.
	 *)
		let vbds = Db.VM.get_VBDs ~__context ~self in
		let vbds = List.filter (fun self -> not (Db.VBD.get_empty ~__context ~self)) vbds in
		let vdis = List.map (fun self -> Db.VBD.get_VDI ~__context ~self) vbds in
		let vdis = value :: vdis in
		let reqd_srs = List.map (fun self -> Db.VDI.get_SR ~__context ~self) vdis in
		let choose_fn = Xapi_vm_helpers.assert_can_see_specified_SRs ~__context ~reqd_srs in
		let _ = Xapi_vm_helpers.choose_host ~__context ~choose_fn () in
		let do_checksum vdi result =
			try
				let r = Helpers.call_api_functions ~__context
					(fun rpc session_id ->
						Client.VDI.checksum ~rpc ~session_id ~self:vdi) in
				result := `Succ r
			with e ->
				result := `Fail e in
		let src_result = ref `Pending in
		let src_thread = Thread.create (do_checksum src_vdi) src_result in
		let dst_result = ref `Pending in
		let dst_thread = Thread.create (do_checksum dst_vdi) dst_result in
		let get_result t r =
			Thread.join(t);
			match !r with
			| `Succ cs -> cs
			| `Fail e -> raise e
			| `Pending -> assert false in
		let src_checksum = get_result src_thread src_result in
		let dst_checksum = get_result dst_thread dst_result in
		debug "source suspend_VDI checksum: %s" src_checksum;
		debug "destination suspend VDI checksum: %s" dst_checksum;
		if src_checksum = dst_checksum then
			Db.VM.set_suspend_VDI ~__context ~self ~value
		else
			raise
				(Api_errors.Server_error
					 (Api_errors.suspend_vdi_replacement_is_not_identical,
					  [(Db.VDI.get_uuid ~__context ~self:src_vdi ^ " : " ^ src_checksum);
					   (Db.VDI.get_uuid ~__context ~self:dst_vdi ^ " : " ^ dst_checksum)]))

let set_appliance ~__context ~self ~value =
	if
		Db.VM.get_is_control_domain ~__context ~self ||
		Db.VM.get_is_a_template ~__context ~self ||
		Db.VM.get_is_a_snapshot ~__context ~self
	then
		raise (Api_errors.Server_error(Api_errors.operation_not_allowed, ["Control domains, templates and snapshots cannot be assigned to appliances."]));
	let previous_value = Db.VM.get_appliance ~__context ~self in
	Db.VM.set_appliance ~__context ~self ~value;
	(* Update allowed operations of the old appliance, if valid. *)
	if Db.is_valid_ref __context previous_value then
		Xapi_vm_appliance.update_allowed_operations ~__context ~self:previous_value;
	(* Update the VM's allowed operations - this will update the new appliance's operations, if valid. *)
	update_allowed_operations __context self

let import_convert ~__context ~_type ~username ~password ~sr ~remote_config =
	let open Vpx in
	let print_jobInstance (j : Vpx.jobInstance) =
		debug "import_convert %Ld%% %s -> %s!\n" j.percentComplete (string_of_jobState j.state) (j.stateDesc) in
	let rec loop call vpx_ip =
		let response = vpxrpc vpx_ip call in
		let jobInstance = Vpx.jobInstance_of_rpc response.Rpc.contents in
		print_jobInstance jobInstance;
		(match jobInstance.state with
			| Created
			| Queued
			| Running -> Thread.delay 1.; loop call vpx_ip
			| Completed
			| Aborted
			| UserAborted -> ()) in
	debug "import_convert %s" (String.concat "; " (List.map (fun (k,v) -> (k ^ "," ^ v)) remote_config));
	let vpx_ip = Xapi_plugins.call_plugin (Context.get_session_id __context) "conversion" "main" [] in
	debug "import_convert %s" vpx_ip;
	let xen_servicecred = { username = username; password = password } in
	let r_cred = rpc_of_serviceCred xen_servicecred in
	let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
	debug "import_convert sr-uuid:%s" sr_uuid;
	let importInfo = { Vpx.sRuuid = sr_uuid } in
	let vmware_serverinfo = {
		serverType = serverType_of_string _type;
		hostname = (List.assoc "hostname" remote_config);
		cred = {username = (List.assoc "username" remote_config); password = (List.assoc "password" remote_config)}} in
	let jobInfo = {source = vmware_serverinfo; sourceVmUUID = "";
			sourceVmName = (List.assoc "vm-name" remote_config); importInfo = importInfo } in
	let r_jobInfo = rpc_of_jobInfo jobInfo in
	let call = Rpc.call "job.create" [ r_cred; r_jobInfo ] in
	let response = vpxrpc vpx_ip call in
	let jobInstance = jobInstance_of_rpc response.Rpc.contents in
	let r_jobId = Rpc.rpc_of_string jobInstance.id in
	let call = Rpc.call "job.get" [ r_cred; r_jobId ] in
	loop call vpx_ip

let query_services ~__context ~self =
	raise (Api_errors.Server_error(Api_errors.not_implemented, [ "query_services" ]))
