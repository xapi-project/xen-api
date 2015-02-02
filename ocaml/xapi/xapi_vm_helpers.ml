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
(** Common code between the fake and real servers for dealing with VMs.
 * @group Virtual-Machine Management
 *)

open Stringext
open Printf
open Xapi_vm_memory_constraints
open Listext
open Xenstore
open Fun

module D=Debug.Debugger(struct let name="xapi" end)
open D
open Workload_balancing

let compute_memory_overhead ~__context ~vm =
  let snapshot = match Db.VM.get_power_state ~__context ~self:vm with
    | `Paused | `Running | `Suspended -> Helpers.get_boot_record ~__context ~self:vm
    | `Halted | _ -> Db.VM.get_record ~__context ~self:vm in
  Memory_check.vm_compute_memory_overhead snapshot 

let update_memory_overhead ~__context ~vm = Db.VM.set_memory_overhead ~__context ~self:vm ~value:(compute_memory_overhead ~__context ~vm)

(* Overrides for database set functions: ************************************************)
let set_actions_after_crash ~__context ~self ~value = 
	Db.VM.set_actions_after_crash ~__context ~self ~value
let set_is_a_template ~__context ~self ~value =
	(* We define a 'set_is_a_template false' as 'install time' *)
	info "VM.set_is_a_template('%b')" value;
	let m = Db.VM.get_metrics ~__context ~self in
	if not(value) then begin
	  (try Db.VM_metrics.set_install_time ~__context ~self:m ~value:(Date.of_float (Unix.gettimeofday ()))
	   with _ -> warn "Could not update VM install time because metrics object was missing")
	end else (
	  (* delete the vm metrics associated with the vm if it exists, when we templat'ize it *)
	  (try Db.VM_metrics.destroy ~__context ~self:m with _ -> ())
	);
	Db.VM.set_is_a_template ~__context ~self ~value

let create ~__context ~name_label ~name_description
		~user_version ~is_a_template
		~affinity
		~memory_target
		~memory_static_max
		~memory_dynamic_max
		~memory_dynamic_min
		~memory_static_min
		~vCPUs_params
		~vCPUs_max ~vCPUs_at_startup
		~actions_after_shutdown ~actions_after_reboot
		~actions_after_crash
		~pV_bootloader
		~pV_kernel ~pV_ramdisk ~pV_args ~pV_bootloader_args ~pV_legacy_args
		~hVM_boot_policy ~hVM_boot_params ~hVM_shadow_multiplier
		~platform
		~pCI_bus ~other_config ~xenstore_data ~recommendations
		~ha_always_run ~ha_restart_priority ~tags
		~blocked_operations ~protection_policy
		~is_snapshot_from_vmpp
		~appliance
		~start_delay
		~shutdown_delay
		~order
		~suspend_SR
		~version
		~generation_id
		~virt_hw_vn
		: API.ref_VM =

	(* NB parameter validation is delayed until VM.start *)

	let uuid = Uuid.make_uuid () in
	let vm_ref = Ref.make () in
	let resident_on = Ref.null in
	let scheduled_to_be_resident_on = Ref.null in

	let metrics = Ref.make () and metrics_uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let vCPUs_utilisation = [(0L, 0.)] in
	Db.VM_metrics.create ~__context ~ref:metrics ~uuid:metrics_uuid
		~memory_actual:0L ~vCPUs_number:0L
		~vCPUs_utilisation
		~vCPUs_CPU:[]
		~vCPUs_params:[]
		~vCPUs_flags:[]
		~state:[]
		~start_time:Date.never
		~install_time:Date.never
		~last_updated:Date.never
		~other_config:[];
	Db.VM.create ~__context ~ref:vm_ref ~uuid:(Uuid.to_string uuid)
		~power_state:(`Halted) ~allowed_operations:[]
		~current_operations:[]
		~blocked_operations:[]
		~name_label ~name_description
		~user_version ~is_a_template 
		~transportable_snapshot_id:""
		~is_a_snapshot:false ~snapshot_time:Date.never ~snapshot_of:Ref.null
		~parent:Ref.null
		~snapshot_info:[] ~snapshot_metadata:""
		~resident_on ~scheduled_to_be_resident_on ~affinity
		~memory_overhead:0L
		~memory_static_max
		~memory_dynamic_max
		~memory_target
		~memory_dynamic_min 
		~memory_static_min
		~vCPUs_params
		~vCPUs_at_startup ~vCPUs_max
		~actions_after_shutdown ~actions_after_reboot
		~actions_after_crash
		~hVM_boot_policy ~hVM_boot_params ~hVM_shadow_multiplier
		~suspend_VDI:Ref.null
		~platform
		~pV_kernel ~pV_ramdisk ~pV_args ~pV_bootloader ~pV_bootloader_args
		~pV_legacy_args
		~pCI_bus ~other_config ~domid:(-1L) ~domarch:""
		~last_boot_CPU_flags:[]
		~is_control_domain:false
		~metrics ~guest_metrics:Ref.null
		~last_booted_record:"" ~xenstore_data ~recommendations
		~blobs:[]
		~ha_restart_priority
		~ha_always_run ~tags
		~bios_strings:[]
		~protection_policy:Ref.null
		~is_snapshot_from_vmpp:false 
		~appliance
		~start_delay
		~shutdown_delay
		~order
		~suspend_SR
		~version
		~generation_id
		~virt_hw_vn
		;
	Db.VM.set_power_state ~__context ~self:vm_ref ~value:`Halted;
	Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm_ref;
	update_memory_overhead ~__context ~vm:vm_ref;
	vm_ref

let destroy  ~__context ~self =
  (* Used to be a call to hard shutdown here, but this will be redundant *)
  (* given the call to 'assert_operation_valid' *)
  debug "VM.destroy: deleting DB records";  

  (* Should we be destroying blobs? It's possible to create a blob and then
	 add its reference to multiple objects. Perhaps we want to just leave the
	 blob? Or only delete it if there is no other reference to it? Is that
	 even possible to know? *)
  let blobs = Db.VM.get_blobs ~__context ~self in
  List.iter (fun (_,self) -> try Xapi_blob.destroy ~__context ~self with _ -> ()) blobs;

  let other_config = Db.VM.get_other_config ~__context ~self in
    if ((List.mem_assoc Xapi_globs.default_template_key other_config) &&
	  (List.assoc Xapi_globs.default_template_key other_config)="true") then
      raise (Api_errors.Server_error (Api_errors.vm_cannot_delete_default_template, []));
	let appliance = Db.VM.get_appliance ~__context ~self in
	if Db.is_valid_ref __context appliance then begin
		Db.VM.set_appliance ~__context ~self ~value:Ref.null;
		Xapi_vm_appliance_lifecycle.update_allowed_operations ~__context ~self:appliance
	end;
  let vbds = Db.VM.get_VBDs ~__context ~self in
    List.iter (fun vbd -> 
		 (try 
		    let metrics = Db.VBD.get_metrics ~__context ~self:vbd in
		    Db.VBD_metrics.destroy ~__context ~self:metrics with _ -> ());
		 (try Db.VBD.destroy ~__context ~self:vbd with _ -> ())) vbds;
  let vifs = Db.VM.get_VIFs ~__context ~self in
    List.iter (fun vif -> 
		 (try
		    let metrics = Db.VIF.get_metrics ~__context ~self:vif in
		    Db.VIF_metrics.destroy ~__context ~self:metrics with _ -> ());
		 (try Db.VIF.destroy ~__context ~self:vif with _ -> ())) vifs;
  let vgpus = Db.VM.get_VGPUs ~__context ~self in
  List.iter (fun vgpu -> try Db.VGPU.destroy ~__context ~self:vgpu with _ -> ()) vgpus;
  let pcis = Db.VM.get_attached_PCIs ~__context ~self in
  List.iter (fun pci -> try Db.PCI.remove_attached_VMs ~__context ~self:pci ~value:self with _ -> ()) pcis;
   let vm_metrics = Db.VM.get_metrics ~__context ~self in
     (try Db.VM_metrics.destroy ~__context ~self:vm_metrics with _ -> ());
   let vm_guest_metrics = Db.VM.get_guest_metrics ~__context ~self in
     (try Db.VM_guest_metrics.destroy ~__context ~self:vm_guest_metrics with _ -> ());

     Db.VM.destroy ~__context ~self

(* Validation and assertion functions *)

let invalid_value x y = raise (Api_errors.Server_error (Api_errors.invalid_value, [ x; y ]))
let value_not_supported fld v reason =
	raise (Api_errors.Server_error (Api_errors.value_not_supported, [ fld; v; reason ]))

let validate_vcpus ~__context ~vCPUs_max ~vCPUs_at_startup = 
	if vCPUs_max < 1L then invalid_value "VCPUs_max" (Int64.to_string vCPUs_max);
	if vCPUs_at_startup < 1L
	then invalid_value "VCPUs-at-startup" (Int64.to_string vCPUs_at_startup);
	if vCPUs_at_startup > vCPUs_max 
	then value_not_supported "VCPUs-at-startup" (Int64.to_string vCPUs_at_startup) "value greater than VCPUs-max"

let validate_memory ~__context ~snapshot:vm_record =
	let constraints = Vm_memory_constraints.extract ~vm_record in
	(* For now, we simply check that the given snapshot record has  *)
	(* memory constraints that can be coerced to valid constraints. *)
	(* In future, we can be more rigorous and require the snapshot  *)
	(* to have valid constraints without allowing coercion.         *)
	match Vm_memory_constraints.transform constraints with
		| Some constraints -> ()
			(* Do nothing. *)
		| None ->
			(* The constraints could not be coerced. *)
			raise (Api_errors.Server_error (Api_errors.memory_constraint_violation, []))

let validate_shadow_multiplier ~hVM_shadow_multiplier =
	if hVM_shadow_multiplier < 1.
	then invalid_value "HVM_shadow_multiplier" (string_of_float hVM_shadow_multiplier)

let validate_actions_after_crash ~__context ~self ~value = 
	let fld = "VM.actions_after_crash" in 
	let hvm_cannot_coredump v = 
	      if Helpers.will_boot_hvm ~__context ~self
	      then value_not_supported fld v "cannot invoke a coredump of an HVM domain" in
	match value with
	  | `rename_restart -> value_not_supported fld "rename_restart" 
	      "option would leak a domain; VMs and not domains are managed by this API"
	  | `coredump_and_destroy -> hvm_cannot_coredump "coredump_and_destroy"
	  | `coredump_and_restart -> hvm_cannot_coredump "coredump_and_restart"
	  | `destroy | `restart | `preserve -> ()

(* Used to sanity-check parameters before VM start *)
let validate_basic_parameters ~__context ~self ~snapshot:x =
	validate_vcpus ~__context 
	  ~vCPUs_max:x.API.vM_VCPUs_max 
	  ~vCPUs_at_startup:x.API.vM_VCPUs_at_startup;
	validate_memory ~__context ~snapshot:x;
	validate_shadow_multiplier
	  ~hVM_shadow_multiplier:x.API.vM_HVM_shadow_multiplier;
	validate_actions_after_crash ~__context ~self ~value:x.API.vM_actions_after_crash


let assert_host_is_enabled ~__context ~host =
	(* Check the host is enabled first *)
	if not (Db.Host.get_enabled ~__context ~self:host) then
		raise (Api_errors.Server_error (
			Api_errors.host_disabled, [Ref.string_of host]))

let is_host_live ~__context host =
	try
		Db.Host_metrics.get_live
			~__context ~self:(Db.Host.get_metrics ~__context ~self:host)
	with _ -> false

let assert_host_is_live ~__context ~host =
	let host_is_live = is_host_live ~__context host in
	if not host_is_live then
		raise (Api_errors.Server_error (Api_errors.host_not_live, []))

let which_specified_SRs_not_available_on_host ~__context ~reqd_srs ~host =
  let pbds = Db.Host.get_PBDs ~__context ~self:host in
    (* filter for those currently_attached *)
  let pbds = List.filter (fun self -> Db.PBD.get_currently_attached ~__context ~self) pbds in
  let avail_srs = List.map (fun self -> Db.PBD.get_SR ~__context ~self) pbds in
  let not_available = List.set_difference reqd_srs avail_srs in
    List.iter (fun sr -> warn "Host %s cannot see SR %s"
		 (Helpers.checknull (fun () -> Db.Host.get_name_label ~__context ~self:host))
		 (Helpers.checknull (fun () -> Db.SR.get_name_label ~__context ~self:sr))) 
      not_available;
    not_available

exception Host_cannot_see_all_SRs
let assert_can_see_specified_SRs ~__context ~reqd_srs ~host =
  let not_available = which_specified_SRs_not_available_on_host ~__context ~reqd_srs ~host in
    if not_available <> []
    then raise Host_cannot_see_all_SRs

let assert_can_see_SRs ~__context ~self ~host =
	let vbds = Db.VM.get_VBDs ~__context ~self in
	(* Skip empty VBDs *)
	let vbds = List.filter (fun self -> not(Db.VBD.get_empty ~__context ~self)) vbds in
	let vdis = List.map (fun self -> Db.VBD.get_VDI ~__context ~self) vbds in
	(* If VM is currently suspended then consider the suspend_VDI. Note both power_state and the suspend VDI
	   are stored in R/O fields, not the last_boot_record *)
	let suspend_vdi = if Db.VM.get_power_state ~__context ~self =`Suspended then [ Db.VM.get_suspend_VDI ~__context ~self ] else [] in
	let reqd_srs = List.map (fun self -> Db.VDI.get_SR ~__context ~self) (vdis @ suspend_vdi) in
	let not_available = which_specified_SRs_not_available_on_host ~__context ~reqd_srs ~host in
	  if not_available <> []
	  then raise (Api_errors.Server_error (Api_errors.vm_requires_sr, [ Ref.string_of self; Ref.string_of (List.hd not_available) ]))

let assert_can_see_networks ~__context ~self ~host =
	let vifs = Db.VM.get_VIFs ~__context ~self in
	let reqd_nets =
		List.map (fun self -> Db.VIF.get_network ~__context ~self) vifs in

	let is_network_available_on host net =
		(* has the network been actualised by one or more PIFs? *)
		let pifs = Db.Network.get_PIFs ~__context ~self:net in
		if pifs <> [] then begin
			(* network is only available if one of  *)
			(* the PIFs connects to the target host *)
			let hosts =
				List.map (fun self -> Db.PIF.get_host ~__context ~self) pifs in
			List.mem host hosts
		end else begin
			let other_config = Db.Network.get_other_config ~__context ~self:net in
			if List.mem_assoc Xapi_globs.assume_network_is_shared other_config && (List.assoc Xapi_globs.assume_network_is_shared other_config = "true") then begin
				debug "other_config:%s is set on Network %s" Xapi_globs.assume_network_is_shared (Ref.string_of net);
				true
			end else begin
				(* find all the VIFs on this network and whose VM's are running. *)
				(* XXX: in many environments this will perform O (Vms) calls to  *)
				(* VM.getRecord. *)
				let vifs = Db.Network.get_VIFs ~__context ~self:net in
				let vms = List.map (fun self -> Db.VIF.get_VM ~__context ~self) vifs in
				let vms = List.map (fun self -> Db.VM.get_record ~__context ~self) vms in
				let vms = List.filter (fun vm -> vm.API.vM_power_state = `Running) vms in
				let hosts = List.map (fun vm -> vm.API.vM_resident_on) vms in
				(* either not pinned to any host OR pinned to this host already *)
				hosts = [] || (List.mem host hosts)
			end
		end
	in

	let avail_nets = List.filter (is_network_available_on host) reqd_nets in
	let not_available = List.set_difference reqd_nets avail_nets in

	List.iter
		(fun net -> warn "Host %s cannot see Network %s"
			(Helpers.checknull
				(fun () -> Db.Host.get_name_label ~__context ~self:host))
			(Helpers.checknull
				(fun () -> Db.Network.get_name_label ~__context ~self:net)))
		not_available;
	if not_available <> [] then
		raise (Api_errors.Server_error (Api_errors.vm_requires_net, [
			Ref.string_of self;
			Ref.string_of (List.hd not_available)
		]));

	(* Also, for each of the available networks, we need to ensure that we can bring it
	 * up on the specified host; i.e. it doesn't need an enslaved PIF. *)
	List.iter
		(fun network->
			try
				Xapi_network_attach_helpers.assert_can_attach_network_on_host
					~__context
					~self:network
					~host
			(* throw exception more appropriate to this context: *)
			with exn ->
				debug
					"Caught exception while checking if network %s could be attached on host %s:%s"
					(Ref.string_of network)
					(Ref.string_of host)
					(ExnHelper.string_of_exn exn);
				raise (Api_errors.Server_error (
					Api_errors.host_cannot_attach_network, [
						Ref.string_of host; Ref.string_of network ]))
		)
		avail_nets

(* IOMMU (VT-d) is required iff the VM has any vGPUs which require PCI
 * passthrough. *)
let vm_needs_iommu ~__context ~self =
	List.exists
		(fun vgpu -> Xapi_vgpu.requires_passthrough ~__context ~self:vgpu)
		(Db.VM.get_VGPUs ~__context ~self)

let assert_host_has_iommu ~__context ~host =
	let chipset_info = Db.Host.get_chipset_info ~__context ~self:host in
	if List.assoc "iommu" chipset_info <> "true" then
		raise (Api_errors.Server_error (Api_errors.vm_requires_iommu, [Ref.string_of host]))

let assert_gpus_available ~__context ~self ~host =
	let vgpus = Db.VM.get_VGPUs ~__context ~self in
	let reqd_groups =
		List.map (fun self -> Db.VGPU.get_GPU_group ~__context ~self) vgpus in
	let is_pgpu_available pgpu vgpu =
		try Xapi_pgpu.assert_can_run_VGPU ~__context ~self:pgpu ~vgpu; true
		with _ -> false
	in
	let can_run_vgpu_on host vgpu =
		let group = Db.VGPU.get_GPU_group ~__context ~self:vgpu in
		let pgpus = Db.GPU_group.get_PGPUs ~__context ~self:group in
		let avail_pgpus =
			List.filter
				(fun pgpu -> is_pgpu_available pgpu vgpu)
				pgpus
		in
		let hosts = List.map (fun self -> Db.PGPU.get_host ~__context ~self) avail_pgpus in
		List.mem host hosts
	in
	let runnable_vgpus = List.filter (can_run_vgpu_on host) vgpus in
	let avail_groups =
		List.map
			(fun self -> Db.VGPU.get_GPU_group ~__context ~self)
			runnable_vgpus
	in
	let not_available = List.set_difference reqd_groups avail_groups in

	List.iter
		(fun group -> warn "Host %s does not have a pGPU from group %s available"
			(Helpers.checknull
				(fun () -> Db.Host.get_name_label ~__context ~self:host))
			(Helpers.checknull
				(fun () -> Db.GPU_group.get_name_label ~__context ~self:group)))
		not_available;
	if not_available <> [] then
		raise (Api_errors.Server_error (Api_errors.vm_requires_gpu, [
			Ref.string_of self;
			Ref.string_of (List.hd not_available)
		]))

let assert_host_supports_hvm ~__context ~self ~host =
	(* For now we say that a host supports HVM if any of    *)
	(* the capability strings contains the substring "hvm". *)
	let capabilities = Db.Host.get_capabilities ~__context ~self:host in
	let host_supports_hvm = List.fold_left (||) false
		(List.map (fun x -> String.has_substr x "hvm") capabilities) in
	if not(host_supports_hvm) then
		raise (Api_errors.Server_error (Api_errors.vm_hvm_required, [Ref.string_of self]))

let assert_enough_memory_available ~__context ~self ~host ~snapshot =
	let host_mem_available =
		Memory_check.host_compute_free_memory_with_maximum_compression
			~__context ~host (Some self) in
	let main, shadow =
		Memory_check.vm_compute_start_memory ~__context snapshot in
	let mem_reqd_for_vm = Int64.add main shadow in
	debug "host %s; available_memory = %Ld; memory_required = %Ld"
		(Db.Host.get_name_label ~self:host ~__context)
		host_mem_available
		mem_reqd_for_vm;
	if host_mem_available < mem_reqd_for_vm then
		raise (Api_errors.Server_error (
			Api_errors.host_not_enough_free_memory,
			[
				Int64.to_string mem_reqd_for_vm;
				Int64.to_string host_mem_available;
			]))

(** Checks to see if a VM can boot on a particular host, throws an error if not.
 * Criteria:
 - The vCPU, memory, shadow multiplier, and actions-after-crash values must be valid.
 - For each VBD, corresponding VDI's SR must be attached on the target host.
 - For each VIF, either the Network has a PIF connecting to the target host,
   OR if no PIF is connected to the Network then the host must be the same one
   all running VMs with VIFs on the Network are running on.
 - If the VM need PCI passthrough, check the host supports IOMMU/VT-d.
 - For each vGPU, check whether a pGPU from the required GPU group is available.
 - If the VM would boot HVM, check the host supports it.
 - If the VM would boot PV, check the bootloader is supported.

 * I.e. we share storage but not (internal/PIF-less) networks: the first VIF on a
 * network pins it to the host the VM is running on.

 * We only check if a VM can boot here w.r.t. the configuration snapshot. If
 * the database is modified in parallel then this check will be inaccurate.
 * We must use the snapshot to boot the VM.

 * XXX: we ought to lock this otherwise we may violate our constraints under load
 *)
let assert_can_boot_here ~__context ~self ~host ~snapshot ?(do_sr_check=true) ?(do_memory_check=true) () =
	debug "Checking whether VM %s can run on host %s" (Ref.string_of self) (Ref.string_of host);
	validate_basic_parameters ~__context ~self ~snapshot;
	assert_host_is_live ~__context ~host;
	assert_host_is_enabled ~__context ~host;
	if do_sr_check then
		assert_can_see_SRs ~__context ~self ~host;
	assert_can_see_networks ~__context ~self ~host;
	if vm_needs_iommu ~__context ~self then
		assert_host_has_iommu ~__context ~host;
	assert_gpus_available ~__context ~self ~host;
	if Helpers.will_boot_hvm ~__context ~self then
		assert_host_supports_hvm ~__context ~self ~host;
	if do_memory_check then
		assert_enough_memory_available ~__context ~self ~host ~snapshot;
	debug "All fine, VM %s can run on host %s!" (Ref.string_of self) (Ref.string_of host)

let retrieve_wlb_recommendations ~__context ~vm ~snapshot =
	(* we have already checked the number of returned entries is correct in retrieve_vm_recommendations
	   But checking that there are no duplicates is also quite cheap, put them in a hash and overwrite duplicates *)
	let recs = Hashtbl.create 12 in
	List.iter
		(fun (h, r) ->
			try
				assert_can_boot_here ~__context ~self:vm ~host:h ~snapshot ();
				Hashtbl.replace recs h r;
			with
			| Api_errors.Server_error(x, y) -> Hashtbl.replace recs h (x :: y))
		(retrieve_vm_recommendations ~__context ~vm);
	if ((Hashtbl.length recs) <> (List.length (Helpers.get_live_hosts ~__context)))
	then
		raise_malformed_response' "VMGetRecommendations"
			"Number of unique recommendations does not match number of potential hosts" "Unknown"
	else
		Hashtbl.fold (fun k v tl -> (k,v) :: tl) recs []

(** Returns the subset of all hosts to which the given function [choose_fn]
can be applied without raising an exception. If the optional [vm] argument is
present, this function additionally prints a debug message that includes the
names of the given VM and each of the possible hosts. *)
let possible_hosts ~__context ?vm ~choose_fn () =
	(* XXXX: This function uses exceptions to control the flow of execution.  *)
	(* XXXX: This function mixes business logic with debugging functionality. *)
	let all_hosts = Db.Host.get_all ~__context in
	let choices = List.filter
		(fun host ->
			try choose_fn ~host; assert_host_is_live ~__context ~host; true
			with _ -> false
		)
		all_hosts in
	begin
		match vm with
		| Some vm ->
			warn "VM %s could run on any of these hosts: [ %s ]"
				(Helpers.checknull
					(fun () -> Db.VM.get_name_label ~__context ~self:vm))
				(String.concat "; "
					(List.map
						(fun self ->
							Helpers.checknull
								(fun () ->
									Db.Host.get_name_label ~__context ~self)
						)
						choices
					)
				);
		| None -> ()
	end;
	choices

(** Returns a single host (from the set of all hosts) to which the given
function [choose_fn] can be applied without raising an exception. Raises
[Api_errors.no_hosts_available] if no such host exists. If the optional [vm]
argument is present, then this function additionally prints a debug message
that includes the names of the given VM and the subset of all hosts that
satisfy the given function [choose_fn]. *)
let choose_host ~__context ?vm ~choose_fn ?(prefer_slaves=false) () =
	let choices = possible_hosts ~__context ?vm ~choose_fn () in
	match choices with
	| [] -> raise (Api_errors.Server_error (Api_errors.no_hosts_available, []))
	| [h] -> h
	| _ ->
		let choices =
			if prefer_slaves then
				let master = Db.Pool.get_master ~__context ~self:(Helpers.get_pool ~__context) in
				List.filter ((<>) master) choices
			else choices in
		List.nth choices (Random.int (List.length choices))

(* Compute all SRs required for shutting down suspended domains *)
let compute_required_SRs_for_shutting_down_suspended_domains ~__context ~vm =
	let all_vm_vdis =
		List.map
			(fun vbd->
				if Db.VBD.get_empty ~__context ~self:vbd then
					None
				else
					Some (Db.VBD.get_VDI ~__context ~self:vbd))
			(Db.VM.get_VBDs ~__context ~self:vm) in
	let all_vm_vdis = List.unbox_list all_vm_vdis in
	List.map (fun vdi -> Db.VDI.get_SR ~self:vdi ~__context) all_vm_vdis

(** Returns the subset of all hosts on which the given [vm] can boot. This
function also prints a debug message identifying the given [vm] and hosts. *)
let get_possible_hosts_for_vm ~__context ~vm ~snapshot = 
	possible_hosts ~__context ~vm
		~choose_fn:(assert_can_boot_here ~__context ~self:vm ~snapshot) ()

(** Performs an expensive and comprehensive check to determine whether the
given [guest] can run on the given [host]. Returns true if and only if the
guest can run on the host. *)
let vm_can_run_on_host __context vm snapshot do_memory_check host =
	let host_has_proper_version () =
		if Helpers.rolling_upgrade_in_progress ~__context
		then
			Helpers.host_has_highest_version_in_pool
				~__context ~host:(Helpers.LocalObject host)
		else true in
	let host_enabled () = Db.Host.get_enabled ~__context ~self:host in
	let host_live () =
		let host_metrics = Db.Host.get_metrics ~__context ~self:host in
		Db.Host_metrics.get_live ~__context ~self:host_metrics in
	let host_can_run_vm () =
		assert_can_boot_here ~__context ~self:vm ~host ~snapshot ~do_memory_check ();
		true in
	let host_evacuate_in_progress =
		try let _ = List.find (fun s -> snd s = `evacuate) (Db.Host.get_current_operations ~__context ~self:host) in false with _ -> true
	in
	try host_has_proper_version () && host_enabled () && host_live () && host_can_run_vm () && host_evacuate_in_progress
	with _ -> false


(* Group the hosts into lists of hosts with equal best capacity *)
let group_hosts_by_best_pgpu_in_group ~__context gpu_group vgpu_type =
	let pgpus = Db.GPU_group.get_PGPUs ~__context ~self:gpu_group in
	let can_accomodate_vgpu pgpu =
		Xapi_pgpu_helpers.get_remaining_capacity ~__context ~self:pgpu
			~vgpu_type > 0L
	in
	let viable_pgpus = List.filter can_accomodate_vgpu pgpus in
	let viable_hosts = List.setify
		(List.map (fun pgpu -> Db.PGPU.get_host ~__context ~self:pgpu)
			viable_pgpus)
	in
	let ordering =
		match Db.GPU_group.get_allocation_algorithm ~__context ~self:gpu_group with
		| `depth_first -> `ascending | `breadth_first -> `descending
	in
	Helpers.group_by ~ordering
		(fun host ->
			let group_by_capacity pgpus = Helpers.group_by ~ordering
				(fun pgpu -> Xapi_pgpu_helpers.get_remaining_capacity ~__context ~self:pgpu ~vgpu_type)
				pgpus
			in
			let viable_resident_pgpus = List.filter
				(fun self -> Db.PGPU.get_host ~__context ~self = host)
				viable_pgpus
			in
			snd (List.hd (List.hd (group_by_capacity viable_resident_pgpus)))
		) viable_hosts

(** Selects a single host from the set of all hosts on which the given [vm]
can boot. Raises [Api_errors.no_hosts_available] if no such host exists. *)
let choose_host_for_vm_no_wlb ~__context ~vm ~snapshot =
	let validate_host = vm_can_run_on_host __context vm snapshot true in
	let all_hosts = Db.Host.get_all ~__context in
	try
		match Db.VM.get_VGPUs ~__context ~self:vm with
		| [] -> Xapi_vm_placement.select_host __context vm validate_host all_hosts
		| vgpu :: _ -> (* just considering first vgpu *)
			let vgpu_type = Db.VGPU.get_type ~__context ~self:vgpu in
			let gpu_group = Db.VGPU.get_GPU_group ~__context ~self:vgpu in
			match
				Xapi_gpu_group.get_remaining_capacity_internal ~__context
					~self:gpu_group ~vgpu_type
			with
			| Either.Left e -> raise e
			| Either.Right _ -> ();
			let host_lists =
				group_hosts_by_best_pgpu_in_group ~__context gpu_group vgpu_type in
			let rec select_host_from = function
				| [] -> raise (Api_errors.Server_error (Api_errors.no_hosts_available, []))
				| (hosts :: less_optimal_groups_of_hosts) ->
					let hosts = List.map (fun (h, c) -> h) hosts in
					debug "Attempting to start VM (%s) on one of equally optimal hosts [ %s ]"
						(Ref.string_of vm) (String.concat ";" (List.map Ref.string_of hosts));
					try Xapi_vm_placement.select_host __context vm validate_host hosts
					with _ ->
						info "Failed to start VM (%s) on any of [ %s ]"
							(Ref.string_of vm) (String.concat ";" (List.map Ref.string_of hosts));
						select_host_from less_optimal_groups_of_hosts
			in
			select_host_from host_lists
	with Api_errors.Server_error(x,[]) when x=Api_errors.no_hosts_available ->
		debug "No hosts guaranteed to satisfy VM constraints. Trying again ignoring memory checks";
		let validate_host = vm_can_run_on_host __context vm snapshot false in
		Xapi_vm_placement.select_host __context vm validate_host all_hosts

(* choose_host_for_vm will use WLB as long as it is enabled and there *)
(* is no pool.other_config["wlb_choose_host_disable"] = "true".       *)
let choose_host_uses_wlb ~__context =
	Workload_balancing.check_wlb_enabled ~__context &&
		not (
			List.exists
				(fun (k,v) ->
					k = "wlb_choose_host_disable"
					&& (String.lowercase v = "true"))
				(Db.Pool.get_other_config ~__context
					~self:(Helpers.get_pool ~__context)))


(** Given a virtual machine, returns a host it can boot on, giving   *)
(** priority to an affinity host if one is present. WARNING: called  *)
(** while holding the global lock from the message forwarding layer. *)
let choose_host_for_vm ~__context ~vm ~snapshot =
	if choose_host_uses_wlb ~__context then
		try
			let rec filter_and_convert recs =
				match recs with
				| (h, recom) :: tl ->
					begin
						debug "\n%s\n" (String.concat ";" recom);
						match recom with
						| ["WLB"; "0.0"; rec_id; zero_reason] ->
							filter_and_convert tl
						| ["WLB"; stars; rec_id] ->
							(h, float_of_string stars, rec_id)
								:: filter_and_convert tl
						| _ -> filter_and_convert tl
					end
				| [] -> []
			in
			begin
				let all_hosts =
					(List.sort
						(fun (h, s, r) (h', s', r') ->
							if s < s' then 1 else if s > s' then -1 else 0)
						(filter_and_convert (retrieve_wlb_recommendations
							~__context ~vm ~snapshot))
					)
				in
				debug "Hosts sorted in priority: %s"
					(List.fold_left
						(fun a (h,s,r) ->
							a ^ (Printf.sprintf "%s %f,"
								(Db.Host.get_name_label ~__context ~self:h) s)
						) "" all_hosts
					);
				match all_hosts with
				| (h,s,r)::_ ->
					debug "Wlb has recommended host %s"
						(Db.Host.get_name_label ~__context ~self:h);
					let action = Db.Task.get_name_label ~__context
						~self:(Context.get_task_id __context) in
					let oc = Db.Pool.get_other_config ~__context
						~self:(Helpers.get_pool ~__context) in
					Db.Task.set_other_config ~__context
						~self:(Context.get_task_id __context)
						~value:([
							("wlb_advised", r);
							("wlb_action", action);
							("wlb_action_obj_type", "VM");
							("wlb_action_obj_ref", (Ref.string_of vm))
						] @ oc);
					h
				| _ ->
					debug "Wlb has no recommendations. \
						Using original algorithm";
					choose_host_for_vm_no_wlb ~__context ~vm ~snapshot
			end
		with
		| Api_errors.Server_error(error_type, error_detail) ->
			debug "Encountered error when using wlb for choosing host \
				\"%s: %s\". Using original algorithm"
				error_type
				(String.concat "" error_detail);
			begin
				try
					let uuid = Db.VM.get_uuid ~__context ~self:vm in
					let message_body =
						Printf.sprintf
							"Wlb consultation for VM '%s' failed (pool uuid: %s)"
							(Db.VM.get_name_label ~__context ~self:vm)
							(Db.Pool.get_uuid ~__context
								~self:(Helpers.get_pool ~__context))
					in
					let (name, priority) = Api_messages.wlb_failed in
					ignore (Xapi_message.create ~__context ~name ~priority
						~cls:`VM ~obj_uuid:uuid ~body:message_body)
				with _ -> ()
			end;
			choose_host_for_vm_no_wlb ~__context ~vm ~snapshot
		| Failure "float_of_string" ->
			debug "Star ratings from wlb could not be parsed to floats. \
				Using original algorithm";
			choose_host_for_vm_no_wlb ~__context ~vm ~snapshot
		| _ ->
			debug "Encountered an unknown error when using wlb for \
				choosing host. Using original algorithm";
			choose_host_for_vm_no_wlb ~__context ~vm ~snapshot
	else
		begin
			debug "Using wlb recommendations for choosing a host has been \
				disabled or wlb is not available. Using original algorithm";
			choose_host_for_vm_no_wlb ~__context ~vm ~snapshot
		end

type set_cpus_number_fn = __context:Context.t -> self:API.ref_VM -> int -> API.vM_t -> int64 -> unit

let validate_HVM_shadow_multiplier multiplier =
	if multiplier < 1.
	then invalid_value "multiplier" (string_of_float multiplier)

(** Sets the HVM shadow multiplier for a {b Halted} VM. Runs on the master. *)
let set_HVM_shadow_multiplier ~__context ~self ~value =
	if Db.VM.get_power_state ~__context ~self <> `Halted
	then failwith "assertion_failed: set_HVM_shadow_multiplier should only be \
		called when the VM is Halted";
	validate_HVM_shadow_multiplier value;
	Db.VM.set_HVM_shadow_multiplier ~__context ~self ~value;
	update_memory_overhead ~__context ~vm:self


let inclusive_range a b = Range.to_list (Range.make a (b + 1))
let vbd_inclusive_range hvm a b =
	List.map (Device_number.of_disk_number hvm) (inclusive_range a b)
let vif_inclusive_range a b = 
	List.map string_of_int (inclusive_range a b)

(* These are high-watermark limits as documented in CA-6525. Individual guest types
   may be further restricted. *)

let allowed_VBD_devices_HVM            = vbd_inclusive_range true 0 3
let allowed_VBD_devices_HVM_PP         = vbd_inclusive_range true 0 15
let allowed_VBD_devices_PV             = vbd_inclusive_range false 0 15
let allowed_VBD_devices_control_domain = vbd_inclusive_range false 0 255

let allowed_VIF_devices_HVM    = vif_inclusive_range 0 3
let allowed_VIF_devices_HVM_PP = vif_inclusive_range 0 6
let allowed_VIF_devices_PV     = vif_inclusive_range 0 6

(** [possible_VBD_devices_of_string s] returns a list of Device_number.t which 
	represent possible interpretations of [s]. *)
let possible_VBD_devices_of_string s = 
	(* NB userdevice fields are arbitrary strings and device fields may be "" *)
	let parse hvm x = try Some (Device_number.of_string hvm x) with _ -> None in
	Listext.List.unbox_list [ parse true s; parse false s ]

(** [all_used_VBD_devices __context self] returns a list of Device_number.t
	which are considered to be already in-use in the VM *)
let all_used_VBD_devices ~__context ~self =
	let all = Db.VM.get_VBDs ~__context ~self in

	let existing_devices = 
		let all_devices = List.map (fun self -> Db.VBD.get_device ~__context ~self) all in
		let all_devices2 = List.map (fun self -> Db.VBD.get_userdevice ~__context ~self) all in
		all_devices @ all_devices2 in

	List.concat (List.map possible_VBD_devices_of_string existing_devices)

let allowed_VBD_devices ~__context ~vm =
	let is_hvm = Helpers.will_boot_hvm ~__context ~self:vm in
	let is_control_domain = Db.VM.get_is_control_domain ~__context ~self:vm in
	let is_pp =
		try
			let guest_metrics = Db.VM.get_guest_metrics ~__context ~self:vm in
			(Db.VM_guest_metrics.get_PV_drivers_version ~__context ~self:guest_metrics) <> []
		with _ -> false in
	let all_devices = match is_hvm,is_pp,is_control_domain with
		| false, _, true  -> allowed_VBD_devices_control_domain
		| false, _, false -> allowed_VBD_devices_PV
		| true, false, _  -> allowed_VBD_devices_HVM
		| true, true, _   -> allowed_VBD_devices_HVM_PP
	in
	(* Filter out those we've already got VBDs for *)
	let used_devices = all_used_VBD_devices ~__context ~self:vm in
	List.filter (fun dev -> not (List.mem dev used_devices)) all_devices

let allowed_VIF_devices ~__context ~vm =
  let is_hvm = Helpers.will_boot_hvm ~__context ~self:vm in
  let guest_metrics = Db.VM.get_guest_metrics ~__context ~self:vm in
  let is_pp =
    try
      (Db.VM_guest_metrics.get_PV_drivers_version ~__context ~self:guest_metrics) <> []
    with
	_ -> false
  in
  let all_devices = 
    match is_hvm,is_pp with
	false,_ -> allowed_VIF_devices_PV
      | true,false -> allowed_VIF_devices_HVM
      | true,true -> allowed_VIF_devices_HVM_PP
  in
  (* Filter out those we've already got VIFs for *)
  let all_vifs = Db.VM.get_VIFs ~__context ~self:vm in
  let used_devices = List.map (fun vif -> Db.VIF.get_device ~__context ~self:vif) all_vifs in
  List.filter (fun dev -> not (List.mem dev used_devices)) all_devices


let delete_guest_metrics ~__context ~self:vm =
	(* Delete potentially stale guest metrics object *)
	let guest_metrics = Db.VM.get_guest_metrics ~__context ~self:vm in
	Db.VM.set_guest_metrics ~__context ~self:vm ~value:Ref.null;
	(try Db.VM_guest_metrics.destroy ~__context ~self:guest_metrics with _ -> ())

let copy_guest_metrics ~__context ~vm =
	try
		let gm = Db.VM.get_guest_metrics ~__context ~self:vm in
		let all = Db.VM_guest_metrics.get_record ~__context ~self:gm in
		let ref = Ref.make () in
		Db.VM_guest_metrics.create ~__context 
			~ref 
			~uuid:(Uuid.to_string (Uuid.make_uuid ()))
			~os_version:all.API.vM_guest_metrics_os_version
			~pV_drivers_version:all.API.vM_guest_metrics_PV_drivers_version
			~pV_drivers_up_to_date:all.API.vM_guest_metrics_PV_drivers_up_to_date
			~memory:all.API.vM_guest_metrics_memory
			~disks:all.API.vM_guest_metrics_disks
			~networks:all.API.vM_guest_metrics_networks
			~other:all.API.vM_guest_metrics_other
			~last_updated:all.API.vM_guest_metrics_last_updated
			~other_config:all.API.vM_guest_metrics_other_config
			~live:all.API.vM_guest_metrics_live;
		ref
	with _ ->
		Ref.null

let start_delay ~__context ~vm =
	let start_delay = Db.VM.get_start_delay ~__context ~self:vm in
	Thread.delay (Int64.to_float start_delay)

let shutdown_delay ~__context ~vm =
	let shutdown_delay = Db.VM.get_shutdown_delay ~__context ~self:vm in
	Thread.delay (Int64.to_float shutdown_delay)

let list_required_vdis ~__context ~self =
	let vbds = Db.VM.get_VBDs ~__context ~self in
	let vbds_excluding_cd =
		List.filter (fun vbd -> Db.VBD.get_type ~__context ~self:vbd <> `CD) vbds
	in
	List.map (fun vbd -> Db.VBD.get_VDI ~__context ~self:vbd) vbds_excluding_cd

(* Find the SRs of all VDIs which have VBDs attached to the VM. *)
let list_required_SRs ~__context ~self =
	let vdis = list_required_vdis ~__context ~self in
	let srs = List.map (fun vdi -> Db.VDI.get_SR ~__context ~self:vdi) vdis in
	let srs = List.filter (fun sr -> Db.SR.get_content_type ~__context ~self:sr <> "iso") srs in
	List.setify srs

(* Check if the database referenced by session_to *)
(* contains the SRs required to recover the VM. *)
let assert_can_be_recovered ~__context ~self ~session_to =
	(* Get the required SR uuids from the foreign database. *)
	let required_SRs = list_required_SRs ~__context ~self in
	let required_SR_uuids = List.map (fun sr -> Db.SR.get_uuid ~__context ~self:sr)
		required_SRs
	in
	(* Try to look up the SRs by uuid in the local database. *)
	try
		Server_helpers.exec_with_new_task ~session_id:session_to
			"Looking for required SRs"
			(fun __context_to -> List.iter
				(fun sr_uuid ->
					let sr = Db.SR.get_by_uuid ~__context:__context_to ~uuid:sr_uuid in
					(* Check if SR has any attached PBDs. *)
					let pbds = Db.SR.get_PBDs ~__context:__context_to ~self:sr in
					let attached_pbds = List.filter
						(fun pbd -> Db.PBD.get_currently_attached ~__context:__context_to ~self:pbd)
						pbds
					in
					if attached_pbds = [] then
						raise (Api_errors.Server_error(Api_errors.vm_requires_sr,
							[Ref.string_of self; Ref.string_of sr]))
				)
				required_SR_uuids)
	with Db_exn.Read_missing_uuid(_, _, sr_uuid) ->
		(* Throw exception containing the ref of the first SR which wasn't found. *)
		let sr = Db.SR.get_by_uuid ~__context ~uuid:sr_uuid in
		raise (Api_errors.Server_error(Api_errors.vm_requires_sr,
			[Ref.string_of self; Ref.string_of sr]))

let get_SRs_required_for_recovery ~__context ~self ~session_to =
	let required_SR_list = list_required_SRs ~__context ~self in
		Server_helpers.exec_with_new_task ~session_id:session_to
			"Looking for the required SRs"
				(fun __context_to ->  List.filter
					( fun sr_ref ->
						let sr_uuid = Db.SR.get_uuid ~__context ~self:sr_ref in
						try
							let sr = Db.SR.get_by_uuid ~__context:__context_to ~uuid:sr_uuid in
							let pbds = Db.SR.get_PBDs ~__context:__context_to ~self:sr in
							let attached_pbds = List.filter
								(fun pbd -> Db.PBD.get_currently_attached ~__context:__context_to ~self:pbd)
								pbds
							in
							 if attached_pbds = [] then true else false
						with Db_exn.Read_missing_uuid(_ , _ , sr_uuid) -> true
					)
					required_SR_list)


(* BIOS strings *)

let copy_bios_strings ~__context ~vm ~host =
	(* only allow to fill in BIOS strings if they are not yet set *)
	let current_strings = Db.VM.get_bios_strings ~__context ~self:vm in
	if List.length current_strings > 0 then
		raise (Api_errors.Server_error(Api_errors.vm_bios_strings_already_set, []))
	else begin
		let bios_strings = Db.Host.get_bios_strings ~__context ~self:host in
		Db.VM.set_bios_strings ~__context ~self:vm ~value:bios_strings;
		(* also set the affinity field to push the VM to start on this host *)
		Db.VM.set_affinity ~__context ~self:vm ~value:host
	end

let consider_generic_bios_strings ~__context ~vm =
	(* check BIOS strings: set to generic values if empty *)
	let bios_strings = Db.VM.get_bios_strings ~__context ~self:vm in
	if bios_strings = [] then begin
		info "The VM's BIOS strings were not yet filled in. The VM is now made BIOS-generic.";
		Db.VM.set_bios_strings ~__context ~self:vm ~value:Xapi_globs.generic_bios_strings
	end

(* Windows VM Generation ID *)

let fresh_genid ?(current_genid="0:0") () =
	if current_genid = "" then "" else
		Printf.sprintf "%Ld:%Ld"
			(Random.int64 Int64.max_int)
			(Random.int64 Int64.max_int)

let vm_fresh_genid ~__context ~self =
	let current_genid = Db.VM.get_generation_id ~__context ~self in
	let new_genid = fresh_genid ~current_genid ()
	and uuid = Db.VM.get_uuid ~__context ~self in
	debug "Refreshing GenID for VM %s to %s" uuid new_genid;
	Db.VM.set_generation_id ~__context ~self ~value:new_genid ;
	new_genid

(** CA-104674: we accidentally gave genids to all guests. This
    function removes them from all VMs that aren't Win8 or Win2012. We
    can tell that they are Win8/12 because they have vga=std in the
    platform flag map. A bit of a hack, but it works. *)
let remove_superfluous_genids ~__context =
	Db.VM.get_all_records ~__context
	|> List.filter (fun (_,vm) ->
		try List.assoc "vga" vm.API.vM_platform <> "std" with _ -> true)
	|> List.iter (fun (self,vm) ->
		if vm.API.vM_generation_id <> "" then
			begin
				debug "Removing superfluous Generation ID (%s) from VM %s"
					vm.API.vM_generation_id vm.API.vM_uuid ;
				Db.VM.set_generation_id ~__context ~self ~value:""
			end)

let assert_virt_hw_support ~__context ~vm ~host_to =
	let vm_virt_hw_vn = Db.VM.get_virt_hw_vn ~__context ~self:vm in
	if not (vm_virt_hw_vn = 0L)
	then begin			
		let host_virt_hw_vns = 
		try
			match host_to with
	        | Helpers.LocalObject host_ref ->
	        	Db.Host.get_virt_hw_vns ~__context ~self:host_ref
	        | Helpers.RemoteObject (rpc, session_id, host_ref) ->
	        	Client.Client.Host.get_virt_hw_vns ~rpc ~session_id ~self:host_ref
	    with Not_found ->
	    	[] in
		
		if not (List.mem vm_virt_hw_vn host_virt_hw_vns) then
			raise (Api_errors.Server_error (Api_errors.hardware_platform_version_6_5_SP1_required, []))
	end
