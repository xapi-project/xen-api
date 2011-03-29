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

module D=Debug.Debugger(struct let name="xapi" end)
open D
open Workload_balancing

let invalid_value x y = raise (Api_errors.Server_error (Api_errors.invalid_value, [ x; y ]))
let value_not_supported fld v reason = 
	raise (Api_errors.Server_error (Api_errors.value_not_supported, [ fld; v; reason ]))

let compute_memory_overhead ~__context ~vm =
  let snapshot = match Db.VM.get_power_state ~__context ~self:vm with
    | `Paused | `Running | `Suspended -> Helpers.get_boot_record ~__context ~self:vm
    | `Halted | _ -> Db.VM.get_record ~__context ~self:vm in
  Memory_check.vm_compute_memory_overhead snapshot 

let update_memory_overhead ~__context ~vm = Db.VM.set_memory_overhead ~__context ~self:vm ~value:(compute_memory_overhead ~__context ~vm)

(* Simple validation functions for fields ************************************************)
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
	   : API.ref_VM =

	(* NB parameter validation is delayed until VM.start *)

	let uuid = Uuid.make_uuid () in
	let vm_ref = Ref.make () in
	let resident_on = Ref.null in
	let scheduled_to_be_resident_on = Ref.null in
	let tools_version = [ (sprintf "%d" Xapi_globs.version_major),
	                      (sprintf "%d" Xapi_globs.version_minor); ] in

	let metrics = Ref.make () and metrics_uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let vCPUs_number = vCPUs_at_startup in
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
		;
	Db.VM.set_power_state ~__context ~self:vm_ref ~value:`Halted;
	Xapi_vm_lifecycle.update_allowed_operations ~__context ~self:vm_ref;
	update_memory_overhead ~__context ~vm:vm_ref;
	vm_ref

let destroy  ~__context ~self =
  (* Used to be a call to hard shutdown here, but this will be redundant *)
  (* given the call to 'assert_operation_valid' *)
  debug "VM.destroy: deleting DB records";  

  let blobs = Db.VM.get_blobs ~__context ~self in
  List.iter (fun (name,_ref) -> Xapi_blob.destroy ~__context ~self:_ref) blobs;

  let other_config = Db.VM.get_other_config ~__context ~self in
    if ((List.mem_assoc Xapi_globs.default_template_key other_config) &&
	  (List.assoc Xapi_globs.default_template_key other_config)="true") then
      raise (Api_errors.Server_error (Api_errors.vm_cannot_delete_default_template, []));
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

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

(** Checks to see if a VM can boot on a particular host, throws an error if not.
    Criteria: 
    * for each VBD, corresponding VDI's SR must be attached on the target host
    * for each VIF, either the Network has a PIF connecting to the target host
                    OR if no PIF is connected to the Network
                       then the host must be the same one all running VMs with
                       VIFs on the Network are running on.
    * if the VM would boot HVM, check the host supports it
    * if the VM would boot PV, check the bootloader is supported
    (ie we share storage but not networks; the first VIF on a network pins it to
    the host the VM is running on)
    XXX: we ought to lock this otherwise we may violate our constraints under load
    XXX: this constraint can be undermined by booting a VM and then downing the PIF
*)

let which_specified_SRs_not_available_on_host ~__context ~reqd_srs ~host =
  let pbds = Db.Host.get_PBDs ~__context ~self:host in
    (* filter for those currently_attached *)
  let pbds = List.filter (fun self -> Db.PBD.get_currently_attached ~__context ~self) pbds in
  let avail_srs = List.map (fun self -> Db.PBD.get_SR ~__context ~self) pbds in
  let not_available = set_difference reqd_srs avail_srs in
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

(* Currently, this check assumes that IOMMU (VT-d) is required iff the host
 * has a vGPU. This will likely need to be modified in future. *)
let vm_needs_iommu ~__context ~self =
	Db.VM.get_VGPUs ~__context ~self <> []

let assert_host_has_iommu ~__context ~host =
	let chipset_info = Db.Host.get_chipset_info ~__context ~self:host in
	if List.assoc "iommu" chipset_info <> "true" then
		raise (Api_errors.Server_error (Api_errors.vm_requires_iommu, [Ref.string_of host]))

let assert_gpus_available ~__context ~self ~host =
	let vgpus = Db.VM.get_VGPUs ~__context ~self in
	let reqd_groups =
		List.map (fun self -> Db.VGPU.get_GPU_group ~__context ~self) vgpus in
	let is_pgpu_available pgpu =
		let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
		let attached = List.length (Db.PCI.get_attached_VMs ~__context ~self:pci) in
		let functions = Int64.to_int (Db.PCI.get_functions ~__context ~self:pci) in
		attached < functions
	in
	let is_group_available_on host group =
		let pgpus = Db.GPU_group.get_PGPUs ~__context ~self:group in
		let avail_pgpus = List.filter is_pgpu_available pgpus in
		let hosts = List.map (fun self -> Db.PGPU.get_host ~__context ~self) avail_pgpus in
		List.mem host hosts
	in
	let avail_groups = List.filter (is_group_available_on host) reqd_groups in
	let not_available = set_difference reqd_groups avail_groups in

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

(* We only check if a VM can boot here w.r.t. the configuration snapshot. If
 * the database is modified in parallel then this check will be inaccurate.
 * We must use the snapshot to boot the VM.
 *)
let assert_can_boot_here_common
		~__context ~self ~host ~snapshot do_memory_check =

	(* First check to see if this is valid during upgrade *)
	if Helpers.rolling_upgrade_in_progress ~__context
		then Helpers.assert_host_has_highest_version_in_pool
			~__context ~host ;
	(* Check to see if the VM is obviously malformed *)
	validate_basic_parameters ~__context ~self ~snapshot;
	(* Check host is live *)
	assert_host_is_live ~__context ~host;
	(* Check host is enabled *)
	assert_host_is_enabled ~__context ~host;
	(* Check SRs *)
	assert_can_see_SRs ~__context ~self ~host;
	(* Check Networks *)
	let vifs = Db.VM.get_VIFs ~__context ~self in
	let reqd_nets =
		List.map (fun self -> Db.VIF.get_network ~__context ~self) vifs in

	let assert_enough_memory_available () =
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
	in

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
	in

	let avail_nets = List.filter (is_network_available_on host) reqd_nets in
	let not_available = set_difference reqd_nets avail_nets in

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
				ignore
					(Xapi_network_attach_helpers.assert_can_attach_network_on_host
						~__context
						~self:network
						~host)
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
		avail_nets;

	if vm_needs_iommu ~__context ~self then
		assert_host_has_iommu ~__context ~host;

	assert_gpus_available ~__context ~self ~host;

	(* Check if the VM would boot HVM and the target machine is HVM-capable *)
	let hvm = Helpers.will_boot_hvm ~__context ~self in
	let capabilities = Db.Host.get_capabilities ~__context ~self:host in
	(* For now we say that a host supports HVM if any of    *)
	(* the capability strings contains the substring "hvm". *)
	let host_supports_hvm = List.fold_left (||) false 
		(List.map (fun x -> String.has_substr x "hvm") capabilities) in
	if hvm && not(host_supports_hvm)
	then raise (Api_errors.Server_error (
		Api_errors.vm_hvm_required, [Ref.string_of self]));
	if do_memory_check then assert_enough_memory_available()

let assert_can_boot_here ~__context ~self ~host ~snapshot = 
	assert_can_boot_here_common ~__context ~self ~host ~snapshot true 

let assert_can_boot_here_no_memcheck ~__context ~self ~host ~snapshot = 
	assert_can_boot_here_common ~__context ~self ~host ~snapshot false 

let retrieve_wlb_recommendations ~__context ~vm ~snapshot =
  (* we have already checked the number of returned entries is correct in retrieve_vm_recommendations
  But checking that there are no duplicates is also quite cheap, put them in a hash and overwrite duplicates *)
  let recs = Hashtbl.create 12 in 
  List.iter (
    fun (h, r) -> 
      try 
        assert_can_boot_here ~__context ~self:vm ~host:h ~snapshot;
        Hashtbl.replace recs h r;
      with
        | Api_errors.Server_error(x, y) -> Hashtbl.replace recs h (x :: y)
  ) (retrieve_vm_recommendations ~__context ~vm);
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
let choose_host ~__context ?vm ~choose_fn () =
	let choices = possible_hosts ~__context ?vm ~choose_fn () in
	if List.length choices = 0
	then raise (Api_errors.Server_error (Api_errors.no_hosts_available, []));
	List.nth choices (Random.int (List.length choices))

(** Returns the subset of all hosts on which the given [vm] can boot. This
function also prints a debug message identifying the given [vm] and hosts. *)
let get_possible_hosts_for_vm ~__context ~vm ~snapshot = 
	possible_hosts ~__context ~vm
		~choose_fn:(assert_can_boot_here ~__context ~self:vm ~snapshot) ()

(** Performs an expensive and comprehensive check to determine whether the
given [guest] can run on the given [host]. Returns true if and only if the
guest can run on the host. *)
let vm_can_run_on_host __context vm snapshot host =
	let host_enabled () = Db.Host.get_enabled ~__context ~self:host in
	let host_live () =
		let host_metrics = Db.Host.get_metrics ~__context ~self:host in
		Db.Host_metrics.get_live ~__context ~self:host_metrics in
	let host_can_run_vm () =
		assert_can_boot_here_no_memcheck ~__context ~self:vm ~host ~snapshot;
		true in
	try host_enabled () && host_live () && host_can_run_vm ()
	with _ -> false

(** Selects a single host from the set of all hosts on which the given [vm]
can boot. Raises [Api_errors.no_hosts_available] if no such host exists. *)
let choose_host_for_vm_no_wlb ~__context ~vm ~snapshot =
	let validate_host = vm_can_run_on_host __context vm snapshot in
	Xapi_vm_placement.select_host __context vm validate_host

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
					ignore (Xapi_message.create ~__context
						~name:Api_messages.wlb_failed ~priority:3L
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

let add_to_VCPUs_params_live ~__context ~self ~key ~value =
	raise (Api_errors.Server_error (Api_errors.not_implemented, [ "add_to_VCPUs_params_live" ]))

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

	if power_state = `Running then begin
		let domid = Helpers.domid_of_vm ~__context ~self in
		Vmopshelpers.with_xc_and_xs
			(fun xc xs ->
				Domain.set_memory_dynamic_range ~xs
					~min:(Int64.to_int (Int64.div min 1024L))
					~max:(Int64.to_int (Int64.div max 1024L))
					domid;
				At_least_once_more.again Memory_control.async_balance_memory)
	end

(** Sets the current memory target for a running VM, to the given *)
(** value (in bytes), rounded down to the nearest page boundary.  *)
(** Writes the new target to the database and also to XenStore.   *)
let set_memory_target_live ~__context ~self ~target = () (*
	let bootrec = Helpers.get_boot_record ~__context ~self in
	if not (Helpers.ballooning_enabled_for_vm ~__context bootrec) then
		raise (Api_errors.Server_error (Api_errors.ballooning_disabled, []));
	(* Round down the given target to the nearest page boundary. *)
	let target = Memory.round_bytes_down_to_nearest_page_boundary target in
	(* Make sure the new target is within the acceptable range. *)
	let constraints = Vm_memory_constraints.get_live ~__context ~vm_ref:self in
	let constraints = {constraints with Vm_memory_constraints.target = target} in
	if not (Vm_memory_constraints.are_valid ~constraints) then raise (
		Api_errors.Server_error (
		Api_errors.memory_constraint_violation, ["invalid target"]));
	(* We've been forwarded, so the VM is local to this machine. *)
	let domid = Helpers.domid_of_vm ~__context ~self in
	Db.VM.set_memory_target ~__context ~self ~value:target;
	let target = Memory.kib_of_bytes_used target in
	Vmopshelpers.with_xs (fun xs -> Balloon.set_memory_target ~xs domid target)
*)

(** The default upper bound on the length of time to wait *)
(** for a running VM to reach its current memory target.  *)
let wait_memory_target_timeout_seconds = 256

(** The default upper bound on the acceptable difference between *)
(** actual memory usage and target memory usage when waiting for *)
(** a running VM to reach its current memory target.             *)
let wait_memory_target_tolerance_bytes = Memory.bytes_of_mib 1L

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
let wait_memory_target_live ~__context ~self
	?(timeout_seconds = wait_memory_target_timeout_seconds)
	?(tolerance_bytes = wait_memory_target_tolerance_bytes)
	() =
	let raise_error error =
		raise (Api_errors.Server_error (error, [Ref.string_of (Context.get_task_id __context)])) in
	let rec wait accumulated_wait_time_seconds =
		if accumulated_wait_time_seconds > wait_memory_target_timeout_seconds
			then raise_error Api_errors.vm_memory_target_wait_timeout;
		if TaskHelper.is_cancelling ~__context
			then raise_error Api_errors.task_cancelled;
		(* Fetch up-to-date value of memory_actual via a hypercall to Xen. *)
		let domain_id = Helpers.domid_of_vm ~__context ~self in
		let domain_info = Vmopshelpers.with_xc (fun xc -> Xc.domain_getinfo xc domain_id) in
		let memory_actual_pages = Int64.of_nativeint domain_info.Xc.total_memory_pages in
		let memory_actual_kib = Xc.pages_to_kib memory_actual_pages in 
		let memory_actual_bytes = Memory.bytes_of_kib memory_actual_kib in
		(* Fetch up-to-date value of target from xenstore. *)
		let memory_target_kib = Int64.of_string (Vmopshelpers.with_xs (fun xs -> xs.Xs.read (xs.Xs.getdomainpath domain_id ^ "/memory/target"))) in
		let memory_target_bytes = Memory.bytes_of_kib memory_target_kib in
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
				"Waited %i second(s) for domain %i to reach \
				its target = %Ld bytes; actual = %Ld bytes."
				accumulated_wait_time_seconds domain_id
				memory_target_bytes memory_actual_bytes;
			(* The memory target has not yet been reached: *)
			(* wait for a while before repeating the test. *)
			Thread.delay 1.0;
			wait (accumulated_wait_time_seconds + 1)
		end
	in
	wait 0

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

(** Sets the HVM shadow multiplier for a {b Running} VM. Runs on the slave. *)
let set_shadow_multiplier_live ~__context ~self ~multiplier =
	if Db.VM.get_power_state ~__context ~self <> `Running
	then failwith "assertion_failed: set_shadow_multiplier_live should only be \
		called when the VM is Running";
	validate_HVM_shadow_multiplier multiplier;
	if Helpers.has_booted_hvm ~__context ~self then (
		let bootrec = Helpers.get_boot_record ~__context ~self in
		let domid = Helpers.domid_of_vm ~__context ~self in
		let vcpus = Int64.to_int bootrec.API.vM_VCPUs_max in
		let static_max_mib = Memory.mib_of_bytes_used (bootrec.API.vM_memory_static_max) in
		let newshadow = Int64.to_int (Memory.HVM.shadow_mib static_max_mib vcpus multiplier) in

		(* Make sure enough free memory exists *)
		let host = Db.VM.get_resident_on ~__context ~self in
		let free_mem_b =
			Memory_check.host_compute_free_memory_with_maximum_compression
				~__context ~host None in
		let free_mem_mib = Int64.to_int (Int64.div (Int64.div free_mem_b 1024L) 1024L) in
		let multiplier_to_record = Xc.with_intf
		  (fun xc ->
		     let curshadow = Xc.shadow_allocation_get xc domid in
		     let needed_mib = newshadow - curshadow in
		     debug "Domid %d has %d MiB shadow; an increase of %d MiB requested; host has %d MiB free"
		       domid curshadow needed_mib free_mem_mib;
		     if free_mem_mib < needed_mib
		     then raise (Api_errors.Server_error(Api_errors.host_not_enough_free_memory, [ Int64.to_string (Memory.bytes_of_mib (Int64.of_int needed_mib)); Int64.to_string free_mem_b ]));
		     if not(Memory.wait_xen_free_mem xc (Int64.mul (Int64.of_int needed_mib) 1024L)) then begin
		       warn "Failed waiting for Xen to free %d MiB: some memory is not properly accounted" needed_mib;
		       (* Dump stats here: *)
		       let (_ : int64) =
		         Memory_check.host_compute_free_memory_with_maximum_compression
		           ~dump_stats:true ~__context ~host None in
		       raise (Api_errors.Server_error(Api_errors.host_not_enough_free_memory, [ Int64.to_string (Memory.bytes_of_mib (Int64.of_int needed_mib)); Int64.to_string free_mem_b ]));
		     end;
		     debug "Setting domid %d's shadow memory to %d MiB" domid newshadow;
		     Xc.shadow_allocation_set xc domid newshadow;
		     Memory.HVM.round_shadow_multiplier static_max_mib vcpus multiplier domid) in
		Db.VM.set_HVM_shadow_multiplier ~__context ~self ~value:multiplier_to_record;
		let newbootrec = { bootrec with API.vM_HVM_shadow_multiplier = multiplier_to_record } in
		Helpers.set_boot_record ~__context ~self newbootrec;
		update_memory_overhead ~__context ~vm:self;
		()
	) else
		()

let send_sysrq ~__context ~vm ~key =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "send_sysrq" ]))

let send_trigger ~__context ~vm ~trigger =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ "send_trigger" ]))

let range_inclusive a b = 
  let rec f a b = if a = b then [ a ] else a :: (f (a + 1) b) in
  List.map string_of_int (f a b)

(* These are high-watermark limits as documented in CA-6525. Individual guest types
   may be further restricted. *)

let allowed_VBD_devices_HVM    = range_inclusive 0 3

let allowed_VBD_devices_HVM_PP = range_inclusive 0 15

let allowed_VBD_devices_PV     = range_inclusive 0 15

let allowed_VIF_devices_HVM    = range_inclusive 0 3

let allowed_VIF_devices_HVM_PP = range_inclusive 0 6

let allowed_VIF_devices_PV     = range_inclusive 0 6

(* If a user has requested a userdevice be set to 'autodetect' and it isn't currently
   attached, then ignore it. If a userdevice is set to 'autodetect' then we assume the
   device field contains the actual device in use. *)

(** Given a VBD returns either Some devicename or None in the case where a device is set to
    autodetect and it hasn't been plugged in. *)
let get_device_name_in_use ~__context ~self = 
  try
	let vbd_r = Db.VBD.get_record ~__context ~self in
	match vbd_r.API.vBD_userdevice with
	| "autodetect" -> Some vbd_r.API.vBD_device
	| x -> Some x
  with _ -> (* someone just destroyed the VBD *)
	  None

exception Invalid_device of string
let translate_vbd_device_to_number name =
  try 
    let number = 
      match String.explode name with
	| 's' :: 'd' :: ('a'..'p' as n) :: rest -> (int_of_char n) - (int_of_char 'a')
	| 'x' :: 'v' :: 'd' :: ('a'..'p' as n) :: rest -> (int_of_char n) - (int_of_char 'a')
	| 'h' :: 'd' :: ('a'..'p' as n) :: rest -> (int_of_char n) - (int_of_char 'a')
	| _ -> int_of_string name 
    in
    string_of_int number 
  with _ -> raise (Invalid_device name)

let allowed_VBD_devices ~__context ~vm =
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
	false,_ -> allowed_VBD_devices_PV
      | true,false -> allowed_VBD_devices_HVM
      | true,true -> allowed_VBD_devices_HVM_PP
  in
  (* Filter out those we've already got VBDs for *)
  let all_vbds = Db.VM.get_VBDs ~__context ~self:vm in
  let used_devices = List.map (fun self -> get_device_name_in_use ~__context ~self) all_vbds in
  (* Remove all the Nones *)
  let used_devices = List.concat (List.map Opt.to_list used_devices) in
  let used_devices = List.map (fun name -> try translate_vbd_device_to_number name with _ -> name)  used_devices in
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
		if (List.assoc "vendor" vm_cpu_info) <> (List.assoc "vendor" host_cpu_info) then
			raise (Api_errors.Server_error(Api_errors.vm_incompatible_with_this_host,
				[Ref.string_of vm; Ref.string_of host; "VM was last booted on a host which had a CPU from a different vendor."]))
	end;
	if List.mem_assoc "features" vm_cpu_info then begin
		(* Check the VM was last booted on a CPU whose features are a subset of the features of this host's CPU. *)
		let host_cpu_features = Cpuid.string_to_features (List.assoc "features" host_cpu_info) in
		let vm_cpu_features = Cpuid.string_to_features (List.assoc "features" vm_cpu_info) in
		if not((Cpuid.mask_features host_cpu_features vm_cpu_features) = vm_cpu_features) then
			raise (Api_errors.Server_error(Api_errors.vm_incompatible_with_this_host,
				[Ref.string_of vm; Ref.string_of host; "VM was last booted on a CPU with features this host's CPU does not have."]))
	end

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
			(fun __context -> List.iter
				(fun sr_uuid -> ignore (Db.SR.get_by_uuid ~__context ~uuid:sr_uuid))
				required_SR_uuids)
	with Db_exn.Read_missing_uuid(_, _, sr_uuid) ->
		(* Throw exception containing the uuid of the first SR which wasn't found. *)
		let sr_ref = Db.SR.get_by_uuid ~__context ~uuid:sr_uuid in
		raise (Api_errors.Server_error(Api_errors.vm_requires_sr,
			[Ref.string_of self; Ref.string_of sr_ref]))
