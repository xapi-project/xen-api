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
(**
 * @group Virtual-Machine Management
 *)
open Printf
open Threadext
open Stringext
open Listext
open Pervasiveext
open Vmopshelpers
open Client
open Vbdops
open Listext
open Fun
open Xenstore

let ( +++ ) = Int64.add
let ( --- ) = Int64.sub
let ( *** ) = Int64.mul
let ( /// ) = Int64.div

module D = Debug.Debugger(struct let name="xapi" end)
open D

module L = Debug.Debugger(struct let name="license" end)

type api_access = InternalNetwork | FirstNetwork

(* This is only used to block the 'present multiple physical cores as one big hyperthreaded core' feature *)
let filtered_platform_flags = ["acpi"; "apic"; "nx"; "pae"; "viridian";
                               "acpi_s3";"acpi_s4"]

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

let clear_all_device_status_fields ~__context ~self =
	List.iter (fun self ->
		Db.VBD.set_currently_attached ~__context ~self ~value:false;
		Db.VBD.set_status_code ~__context ~self ~value:0L;
		Db.VBD.set_status_detail ~__context ~self ~value:""
	) (Db.VM.get_VBDs ~__context ~self);
	List.iter (fun self ->
		Db.VIF.set_currently_attached ~__context ~self ~value:false;
		Db.VIF.set_status_code ~__context ~self ~value:0L;
		Db.VIF.set_status_detail ~__context ~self ~value:""
	) (Db.VM.get_VIFs ~__context ~self);
	Pciops.unassign_all_for_vm ~__context self;
	Vgpuops.clear_vgpus ~__context ~vm:self

(* Called on VM.start codepath only to validate current VM parameters *)
let check_vm_parameters ~__context ~self ~snapshot =
	let refself = Ref.string_of self in
	let vcpus = Int64.to_int snapshot.API.vM_VCPUs_max in
	let mem = snapshot.API.vM_memory_static_max in
	if vcpus <= 0 then
		raise (Api_errors.Server_error (Api_errors.vm_no_vcpus, [ refself ]));
	if vcpus >= 64 then
		raise (Api_errors.Server_error (Api_errors.vm_toomany_vcpus, [ refself ]));
	if mem < Xapi_globs.vm_minimum_memory then
		raise (Api_errors.Server_error (Api_errors.vm_memory_size_too_low, [ refself ]));
	()

let add_vif ~__context ~xs vif_device = 
  if vif_device.Vm_config.bridge = "" then failwith "Don't know how to connect a VIF to this type of Network";
  let vif_uuid = Db.VIF.get_uuid ~__context ~self:vif_device.Vm_config.vif_ref in
  let extra_private_keys = ["ref", Ref.string_of vif_device.Vm_config.vif_ref;
                            "vif-uuid", vif_uuid;
                            "network-uuid", Db.Network.get_uuid ~__context ~self:vif_device.Vm_config.network_ref] in
  Xapi_network.register_vif ~__context vif_device.Vm_config.vif_ref;
  Xapi_network.attach_internal ~__context ~self:vif_device.Vm_config.network_ref ();
  Xapi_udhcpd.maybe_add_lease ~__context vif_device.Vm_config.vif_ref;

  Xapi_xenops_errors.handle_xenops_error
    (fun () ->
       let netty = Netman.netty_of_bridge vif_device.Vm_config.bridge in
	   (* If there are any non-virtual interfaces on the bridge with a valid
		  carrier then reflect the status *)
	   let ifs = 
		   let all = Netdev.network.Netdev.intf_list vif_device.Vm_config.bridge in
		   List.filter (fun x -> not(String.startswith "vif" x) && not(String.startswith "tap" x)) all in
	   let carrier = 
		   let all = List.map (fun x -> try Netdev.get_carrier x with _ -> false) ifs in
		   (all = []) (* internal network *)
		   || 
		   (List.fold_left (||) false all) (* any interface on external network *)
	   in

       let (_: Device_common.device) = Device.Vif.add ~xs ~devid:vif_device.Vm_config.devid ~netty 
	 ~mac:vif_device.Vm_config.mac ~carrier:(not(!Monitor_rrds.pass_through_pif_carrier) || carrier) ~mtu:vif_device.Vm_config.mtu ~rate:vif_device.Vm_config.rate ~protocol:vif_device.Vm_config.protocol 
	 ~other_config:vif_device.Vm_config.other_config ~extra_private_keys
	 vif_device.Vm_config.domid in
       ()
    );
  Db.VIF.set_currently_attached ~__context ~self:vif_device.Vm_config.vif_ref ~value:true;
  (* sync MTU *)
  try
    let device = "vif" ^ (string_of_int vif_device.Vm_config.domid) ^ "." ^ (string_of_int vif_device.Vm_config.devid) in
    let mtu = Int64.of_string (Netdev.get_mtu device) in
    Db.VIF.set_MTU ~__context ~self:vif_device.Vm_config.vif_ref ~value:mtu
  with _ ->
    debug "could not update MTU field on VIF %s" vif_uuid


(* take a list of vif devices and attach them to the domid *)
let create_vifs ~__context ~xs vifs =
  (* Sort VIFs by devid. Note that it is the order the devices appear in 'xenstore-ls' that many PV guests use to 
     determine initial device number. *)
  let vifs = List.stable_sort (fun a b -> compare a.Vm_config.devid b.Vm_config.devid) vifs in
  List.iter
    (fun vif ->
       try
	 add_vif ~__context ~xs vif
       with e ->
	 (* Note that failures are nolonger suppressed. See CA-12487 *)
	 warn "Error attaching VIF (%d) to domid %d: %s; throwing %s" 
	   vif.Vm_config.devid vif.Vm_config.domid (ExnHelper.string_of_exn e) Api_errors.cannot_plug_vif;
	 raise (Api_errors.Server_error (Api_errors.cannot_plug_vif, [ Ref.string_of vif.Vm_config.vif_ref ]))
    ) vifs

(* Called on both VM.start and VM.resume codepaths to create vcpus in xenstore *)
let create_cpus ~xs snapshot domid =
	for i = 0 to (Int64.to_int snapshot.API.vM_VCPUs_max) - 1
	do
		Device.Vcpu.add ~xs ~devid:i domid
	done

(* Called on both VM.start and VM.resume codepaths to set the correct number of
   vcpus as online *)
let set_cpus_number ~__context ~xs ~self domid snapshot =
	let metrics = Helpers.get_vm_metrics ~__context ~self in
	let target = Int64.to_int snapshot.API.vM_VCPUs_at_startup in
	let max_vcpus = Int64.to_int snapshot.API.vM_VCPUs_max in

	if target > max_vcpus
	then warn "VCPUs at startup (%d) is greater than VCPUs max (%d)" target max_vcpus;
	let target = min target max_vcpus in

	(* Returns the instantaneous CPU number from xenstore *)
	let current =
		let n = ref (-1) in
		for i = 0 to max_vcpus - 1
		do if Device.Vcpu.status ~xs ~devid:i domid then n := i
		done;
		!n + 1 in

	if current > target then (
		(* need to deplug cpus *)
		for i = current - 1 downto target
		do
			Device.Vcpu.set ~xs ~devid:i domid false
		done
	) else if current < target then (
		(* need to plug cpus *)
		for i = current to (target - 1)
		do
			Device.Vcpu.set ~xs ~devid:i domid true
		done
	);
	Db.VM_metrics.set_VCPUs_number ~__context ~self:metrics ~value:(Int64.of_int target)

let write_memory_policy ~xs snapshot domid = 
	Domain.set_memory_dynamic_range ~xs 
		~min:(Int64.to_int (Int64.div snapshot.API.vM_memory_dynamic_min 1024L))
		~max:(Int64.to_int (Int64.div snapshot.API.vM_memory_dynamic_max 1024L))
		domid

let update_console ~__context ty port vm =
	let protocol = match ty with
		| Xal.VNC -> `rfb
		| Xal.Text -> `vt100 in
	(* Delete and re-create the console record *)
	List.iter
		(fun self ->
			if Db.Console.get_protocol ~__context ~self = protocol
			then Db.Console.destroy ~__context ~self)
		(Db.VM.get_consoles ~__context ~self:vm);
	let localhost = Helpers.get_localhost ~__context in
	let address = Db.Host.get_address ~__context ~self:localhost in
	let ref = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let location = Printf.sprintf "https://%s%s?uuid=%s" address Constants.console_uri uuid in
	Db.Console.create ~__context ~ref ~uuid ~protocol ~location ~vM:vm ~other_config:[] ~port:(Int64.of_int port)

(* Detach all consoles from a guest, used when shutting down/pausing *)
let destroy_consoles ~__context ~vM =
	let all = Db.VM.get_consoles ~__context ~self:vM in
	List.iter (fun console -> Db.Console.destroy ~__context ~self:console) all

(* /boot/ contains potentially sensitive files like xen-initrd, so we will only*)
(* allow directly booting guests from the subfolder /boot/guest/ *) 
let allowed_dom0_directory_for_kernels = "/boot/guest/"
let is_kernel_whitelisted kernel =
  let safe_str str = not (Stringext.String.has_substr str "..") in
  (* make sure the script prefix is the allowed dom0 directory *)
  (Stringext.String.startswith allowed_dom0_directory_for_kernels kernel)
  (* avoid ..-style attacks and other weird things *)
  &&(safe_str kernel)
let assert_kernel_is_whitelisted kernel =
  if not (is_kernel_whitelisted kernel) then
    raise (Api_errors.Server_error (Api_errors.permission_denied, [
      (Printf.sprintf "illegal kernel path %s" kernel)]))

let hvm_video_mib snapshot =
	let platform = snapshot.API.vM_platform in
	let default = 4 in (* MiB *)
	if not(List.mem_assoc "videoram" platform)
	then default
	else
		let v = List.assoc "videoram" platform in
		try
			int_of_string v
		with _ ->
			error "Unknown videoram option: %s" v;
			default

(* Called on VM.start to populate kernel part of domain *)
let create_kernel ~__context ~xc ~xs ~self domid snapshot =
	(* get common parameters *)
	let ballooning_enabled = Helpers.ballooning_enabled_for_vm ~__context snapshot in
	let vcpus = Int64.to_int snapshot.API.vM_VCPUs_max in
	let mem_max_kib = Int64.div snapshot.API.vM_memory_static_max 1024L in
	let mem_target_kib = 
	  if ballooning_enabled
	  then Int64.div snapshot.API.vM_memory_target 1024L
	  else mem_max_kib in
	try
	let domarch = (match Helpers.boot_method_of_vm ~__context ~vm:snapshot with
	| Helpers.HVM { Helpers.timeoffset = timeoffset; } ->
		let kernel_path = Domain.hvmloader in
		let shadow_multiplier = snapshot.API.vM_HVM_shadow_multiplier in
		let video_mib = hvm_video_mib snapshot in
		debug "build hvm \"%s\" vcpus:%d mem_max:%Ld mem_target:%Ld video_mib:%d MiB timeoffset:%s"
		      kernel_path vcpus mem_max_kib mem_target_kib video_mib timeoffset;
		let arch = Domain.build_hvm xc xs mem_max_kib mem_target_kib shadow_multiplier vcpus kernel_path
		  timeoffset video_mib domid in
		(* Since our shadow allocation might have been increased by Xen we need to 
		   update the shadow_multiplier now. Nb. the last_boot_record wont 
		   necessarily have the right value in! *)
		let multiplier = Memory.HVM.round_shadow_multiplier
			(Memory.mib_of_kib_used mem_max_kib)
			vcpus shadow_multiplier domid in
		Db.VM.set_HVM_shadow_multiplier ~__context ~self ~value:multiplier;
		arch
	| Helpers.DirectPV { Helpers.kernel = pv_kernel;
			     kernel_args = pv_args;
			     ramdisk = pv_ramdisk } ->
		(* No bootloader; get the kernel, initrd from dom0 *)

		assert_kernel_is_whitelisted pv_kernel;
		maybe assert_kernel_is_whitelisted pv_ramdisk;
		debug "build linux \"%s\" \"%s\" vcpus:%d mem_max:%Ld mem_target:%Ld"
		      pv_kernel pv_args vcpus mem_max_kib mem_target_kib;
		Domain.build_linux xc xs mem_max_kib mem_target_kib pv_kernel pv_args
		     pv_ramdisk vcpus domid
	| Helpers.IndirectPV { Helpers.bootloader = bootloader;
			       extra_args = extra_args;
			       legacy_args = legacy_args;
			       pv_bootloader_args = pv_bootloader_args;
			       vdis = boot_vdis } ->
	        if not (List.mem_assoc bootloader Xapi_globs.supported_bootloaders) then
		  raise (Api_errors.Server_error(Api_errors.unknown_bootloader, [ Ref.string_of self; bootloader ]));

		let kernel =
		  Helpers.call_api_functions ~__context
		    (fun rpc session_id ->
		       Sm_fs_ops.with_block_attached_devices __context rpc session_id boot_vdis `RO
			 (fun devices ->
			    let vm_uuid = snapshot.API.vM_uuid in
			    Bootloader.extract_default_kernel bootloader devices legacy_args extra_args pv_bootloader_args vm_uuid)) in

		debug "build linux \"%s\" \"%s\" vcpus:%d mem_max:%Ld mem_target:%Ld"
		      kernel.Bootloader.kernel_path
		      kernel.Bootloader.kernel_args
		      vcpus mem_max_kib mem_target_kib;
		let arch = Domain.build_linux xc xs mem_max_kib mem_target_kib
		                   kernel.Bootloader.kernel_path
		                   kernel.Bootloader.kernel_args
		                   kernel.Bootloader.initrd_path
		                   vcpus domid in
		(* XXX: aim to use pygrub to detect when to HVM boot a guest *)
		Bootloader.delete_extracted_kernel kernel;
		arch
	) in
	Db.VM.set_domarch ~__context ~self ~value:(Domain.string_of_domarch domarch)
	with
	| Domain.Could_not_read_file filename ->
		raise (Api_errors.Server_error(Api_errors.bootloader_failed, [ Ref.string_of self; Printf.sprintf "Could not read file: %s" filename ]))
	| Bootloader.Error x ->
		raise (Api_errors.Server_error(Api_errors.bootloader_failed, [ Ref.string_of self; Printf.sprintf "Error from bootloader: %s" x ]))

let general_domain_create_check ~__context ~vm ~snapshot =
    (* If guest will boot HVM check that this host has HVM capabilities *)
    let hvm = Helpers.is_hvm snapshot in
      if hvm then (
	  let caps = with_xc (fun xc -> Xenctrl.version_capabilities xc) in
	  if not (String.has_substr caps "hvm") then (raise (Api_errors.Server_error (Api_errors.vm_hvm_required,[]))))

(** [vcpu_configuration snapshot] transforms a vM_t into a list of 
    key/value pairs representing the VM's vCPU configuration. *)
let vcpu_configuration snapshot = 
  let vcpus = Int64.to_int snapshot.API.vM_VCPUs_max in
  let vcpus_current = Int64.to_int snapshot.API.vM_VCPUs_at_startup in
  let pcpus = with_xc (fun xc -> (Xenctrl.physinfo xc).Xenctrl.max_nr_cpus) in
  debug "xen reports max %d pCPUs" pcpus;

  (* vcpu <-> pcpu affinity settings are stored here. Format is either:
	 1,2,3         ::  all vCPUs receive this mask
     1,2,3; 4,5,6  ::  vCPU n receives mask n. Unlisted vCPUs receive first mask
	 *)
  let masks = try String.split ';' (List.assoc "mask" snapshot.API.vM_VCPUs_params) with _ -> [] in
  (* If the mask has fewer elements than vCPUs, expand it *)
  let masks = match masks with
	  | [] -> []
	  | m :: ms ->
		  let defaults = Range.fold_right (fun _ acc -> m :: acc) 
			  (Range.make 0 vcpus) [] in
		  List.take vcpus (masks @ defaults) in
  (* convert a mask into a binary string, one character per pCPU *)
  let bitmap string: string = 
    let cpus = List.map int_of_string (String.split ',' string) in
    let cpus = List.filter (fun x -> x >= 0 && x < pcpus) cpus in
	let result = String.make pcpus '0' in
	List.iter (fun cpu -> result.[cpu] <- '1') cpus;
	result in
  (* Generate key/value pairs for affinity *)
  let affinity = 
	  try
		  List.mapi (fun idx mask -> 
			  Printf.sprintf "vcpu/%d/affinity" idx, bitmap mask
		  ) masks 
	  with e -> 
		  warn "Failed to parse vCPU masks: %s" (List.assoc "mask" snapshot.API.vM_VCPUs_params);
		  [] in

  (* scheduler parameters: weight and cap *)
  let weight = try Some (List.assoc "weight" snapshot.API.vM_VCPUs_params) with _ -> None in
  let cap = try Some (List.assoc "cap" snapshot.API.vM_VCPUs_params) with _ -> None in
  let weight = try Opt.map int_of_string weight with _ -> warn "Failed to parse vCPU weight"; None in
  let cap = try Opt.map int_of_string cap with _ -> warn "Failed to parse vCPU cap"; None in
  let weight = Opt.map (fun x -> [ "vcpu/weight", string_of_int x ]) weight in
  let cap = Opt.map (fun x -> [ "vcpu/cap", string_of_int x ]) cap in

  [ "vcpu/number", string_of_int vcpus;
    "vcpu/current", string_of_int vcpus_current ]
  @ affinity
  @ (Opt.default [] weight)
  @ (Opt.default [] cap)


(* create an empty domain *)
let create ~__context ~xc ~xs ~self (snapshot: API.vM_t) ~reservation_id () =
  finally
    (fun () ->
        general_domain_create_check ~__context ~vm:self ~snapshot;
	(* We always create at_startup vCPUs and modify this field to reflect the
	   current value when we suspend so it's safe to always use at_startup.
	   NB Rio used a different convention here. *)
	let num_cpus = Int64.to_int snapshot.API.vM_VCPUs_at_startup in
	debug "VM will have %Ld hotplugged vcpus out of %Ld" 
	  snapshot.API.vM_VCPUs_at_startup snapshot.API.vM_VCPUs_max;

	(* Set all the CPU utilisation numbers to -1 *)
	let range = Range.fold_right (fun x y -> x :: y) (Range.make 0 num_cpus) [] in
	let utilisations = List.map (fun x -> Int64.of_int x, (-1.0)) range in
	let uuid_str = Db.VM.get_uuid ~__context ~self in
	let uuid = Uuid.of_string uuid_str in
	let xsdata = Db.VM.get_xenstore_data ~__context ~self in
	(* disallowed by default; allowed only if it has one of a set of prefixes *)
	let allowed_xsdata (x, _) = List.fold_left (||) false (List.map (fun p -> String.startswith p x) [ "vm-data/"; "FIST/" ]) in
	let xsdata = List.filter allowed_xsdata xsdata in

	let platformdata =
		let p = Db.VM.get_platform ~__context ~self in
		if not (Pool_features.is_enabled ~__context Features.No_platform_filter) then
			List.filter (fun (k, v) -> List.mem k filtered_platform_flags) p
		else p
	in
	(* XXX: add extra configuration info to the platform/ map for now.
	   Eventually we'll put this somewhere where the guest can't see it. *)
	let platformdata = platformdata @ ( vcpu_configuration snapshot ) in


	(* If we don't have a metrics object, recreate one now *)
	let m = Db.VM.get_metrics ~__context ~self in
	if try ignore(Db.VM_metrics.get_uuid ~__context ~self:m); false with _ -> true then begin
	  let metrics = Ref.make () in
	  Db.VM_metrics.create ~__context ~ref:metrics ~uuid:(Uuid.to_string (Uuid.make_uuid()))
	    ~vCPUs_number:snapshot.API.vM_VCPUs_at_startup 
	    ~vCPUs_utilisation:utilisations ~memory_actual:0L
	    ~vCPUs_CPU:[]
	    ~vCPUs_params:[]
	    ~vCPUs_flags:[]
	    ~start_time:Date.never
	    ~install_time:Date.never
	    ~state: []
	    ~last_updated:(Date.of_float 0.)
	    ~other_config:[];
	  Db.VM.set_metrics ~__context ~self ~value:metrics
	end;
	
	let bios_strings = Db.VM.get_bios_strings ~__context ~self in

 	let hvm = Helpers.will_boot_hvm ~__context ~self in
	Xapi_xenops_errors.handle_xenops_error
	  (fun () ->
	     info "Memory free = %Ld; scrub = %Ld" (Memory.get_free_memory_kib ~xc) (Memory.get_scrub_memory_kib ~xc);
	     let domid = (try 
			     let info = {
 				     Domain.ssidref = 0l;
 				     Domain.hvm = hvm;
 				     Domain.hap = hvm;
 				     Domain.name = snapshot.API.vM_name_label;
 				     Domain.platformdata = platformdata;
 				     Domain.xsdata = xsdata;
				     Domain.bios_strings = bios_strings;
 			     } in
			     Domain.make ~xc ~xs info uuid
		     with e ->
			     info "Domain.make threw %s" (ExnHelper.string_of_exn e);
			     info "Memory free = %Ld; scrub = %Ld" (Memory.get_free_memory_kib ~xc) (Memory.get_scrub_memory_kib ~xc);
			     raise e
	     ) in

	     debug "Created domain with domid: %d" domid;
	     Memory_control.transfer_reservation_to_domain ~__context ~xs ~reservation_id ~domid;

	     begin try let sspf = (List.assoc "suppress-spurious-page-faults" snapshot.API.vM_other_config) in
	     if sspf = "true" then Domain.suppress_spurious_page_faults ~xc domid;
             with _ -> () end;

	     domid
	  )
    ) (fun () -> Memory_control.delete_reservation ~__context ~xs ~reservation_id)

(* Destroy a domain associated with this VM. Note the VM's power_state and domid might
   remain valid if the VM has a domain on another host (ie in the migrate case)
   NB to prevent resource leaks, log errors and continue on attempting to clean up as
   much as possible.
 *)
let destroy_domain ?(preserve_xs_vm=false) ?(unplug_frontends=true) ?(clear_currently_attached=true)  ~__context ~xc ~xs ~self domid =
	Helpers.log_exn_continue (Printf.sprintf "Vmops.destroy_domain: Destroying xen domain domid %d" domid)
		(fun () -> 
			Domain.destroy ~preserve_xs_vm ~xc ~xs domid
		) ();

	let all_vbds = Db.VM.get_VBDs ~__context ~self in
	List.iter
		(fun vbd ->
			if not(Db.VBD.get_empty ~__context ~self:vbd)
			then Helpers.log_exn_continue (Printf.sprintf "Vmops.destroy_domain: detaching associated with VBD %s" (Ref.string_of vbd))
				(fun () ->
					Storage_access.deactivate_and_detach ~__context ~vbd ~domid ~unplug_frontends
				) ()
		) (Storage_access.vbd_detach_order ~__context all_vbds);

	if clear_currently_attached
	then List.iter
		(fun vbd ->
			Helpers.log_exn_continue (Printf.sprintf "Vmops.destroy_domain: clearing currently_attached for VBD %s" (Ref.string_of vbd))
				(fun () ->
					Db.VBD.set_currently_attached ~__context ~self:vbd ~value:false
				) ()
		) all_vbds

(* Destroy a VM's domain and all runtime state (domid etc).
   If release_devices is true, unlock all VDIs and clear device_status_flags.
   In the restore case, set release_devices to false so we remember which 
   devices should be attached on resume. *)
let destroy ?(unplug_frontends=true) ?(clear_currently_attached=true) ~__context ~xc ~xs ~self domid state =
	destroy_domain ~unplug_frontends ~clear_currently_attached ~__context ~xc ~xs ~self domid;
	Db.VM.set_power_state ~__context ~self ~value:state;
	Db.VM.set_domid ~__context ~self ~value:(-1L);

	(* If the new power state is Running then we're killing the domain but /rebooting/
	   the VM-- keep the VM marked as resident_on to keep the memory allocated.
	   In any other case (Halted/Suspended) the VM's memory is nolonger needed so
	   we can blank the resident_on. 
	*)
	if state <> `Running
	then Db.VM.set_resident_on ~__context ~self ~value:Ref.null;

	Storage_access.reset ~__context ~vm:self;

	destroy_consoles ~__context ~vM:self;
	(* to make debugging easier, set currently_attached to false for all
	   VBDs and VIFs*)
	if clear_currently_attached
	then clear_all_device_status_fields ~__context ~self

let has_platform_flag platform feature =
  try bool_of_string (List.assoc feature platform) with _ -> false

(* Create the qemu-dm device emulator process. Has to be done after the
   disks and vifs have already been added.
   Returns the port number of the default VNC console. *)
let create_device_emulator ~__context ~xc ~xs ~self ?(restore=false) ?vnc_statefile ?pci_passthrough domid vifs snapshot =
	let vcpus = Int64.to_int snapshot.API.vM_VCPUs_max
	and mem_max_kib = Int64.div snapshot.API.vM_memory_static_max 1024L in
	let other_config = snapshot.API.vM_other_config in

	(* XXX: we need some locking here & some better place to put the bridge name *)
	let nics = List.map (fun vif -> vif.Vm_config.mac, vif.Vm_config.bridge, vif.Vm_config.devid) vifs in

	let platform = snapshot.API.vM_platform in

	let disks =
		(* XXX: this is an experimental way of attaching disks to VMs *)
		if has_platform_flag platform "qemu_disk_cmdline"
		then List.unbox_list (List.map (fun vbd ->
			Opt.map (fun filename ->
				let userdevice = Db.VBD.get_userdevice ~__context ~self:vbd in
				let device_number = Device_number.to_disk_number (Vbdops.translate_vbd_device self userdevice true) in
				let media = if Db.VBD.get_type ~__context ~self:vbd = `CD then Device.Dm.Cdrom else Device.Dm.Disk in
				device_number, filename, media
			) (Storage_access.Qemu_blkfront.path_opt ~__context ~self:vbd)
		) snapshot.API.vM_VBDs)
		else [] in

	let hvm = Helpers.is_hvm snapshot in

	let pvfb = has_platform_flag platform "pvfb" in
	
	(* Examine the boot method if the guest is HVM and do something about it *)
	if hvm || pvfb then begin
		let policy = snapshot.API.vM_HVM_boot_policy in

		if (policy <> Constants.hvm_boot_policy_bios_order) && (not pvfb) then
			failwith (sprintf "Unknown HVM boot policy: %s" policy);

		let params = snapshot.API.vM_HVM_boot_params in
		let boot = if List.mem_assoc Constants.hvm_boot_params_order params then
			List.assoc Constants.hvm_boot_params_order params
		else (
			warn "No boot order found. Defaulting to 'cd'";
			"cd"
		) in

		let platform = snapshot.API.vM_platform in
		let acpi = has_platform_flag platform "acpi" in
		let serial = try List.assoc "hvm_serial" other_config with _ -> "pty" in
		let vnc_keymap = try List.assoc "keymap" platform with _ -> "en-us" in
		let vga =
			let default = Device.Dm.Cirrus in
			if not(List.mem_assoc "vga" platform)
			then default
			else match (List.assoc "vga" platform) with
				| "std" -> Device.Dm.Std_vga
				| "cirrus" -> Device.Dm.Cirrus
				| x ->
					error "Unknown platform/vga option: %s (expected 'std' or 'cirrus')" x;
					default in
		let video_mib = hvm_video_mib snapshot in
		(* Xenclient specific *)
		let dom0_input = try if Xapi_globs.xenclient_enabled then Some (int_of_string (List.assoc "dom0_input" platform)) else None with _ -> 
		  warn "failed to parse dom0_input platform flag.";
		  None
		in

		let pci_emulations =
			let s = try Some (List.assoc "mtc_pci_emulations" other_config) with _ -> None in
			match s with
			| None -> []
			| Some x ->
				try
					let l = String.split ',' x in
					List.map (String.strip String.isspace) l
				with _ -> []
			in
		let dmpath = Filename.concat Fhs.libexecdir "qemu-dm-wrapper" in
		let dmstart = if restore then Device.Dm.restore else Device.Dm.start in

		(* Display and input devices are usually conflated *)
		let disp,usb = 
		  let default_disp_usb = (Device.Dm.VNC (vga, true, 0, vnc_keymap), ["tablet"]) in
		  if Xapi_globs.xenclient_enabled then begin 
		    try 
		      match List.assoc "vga_mode" platform with
			| "passthrough" -> 
			    (Device.Dm.Passthrough dom0_input,[])
			| "intel" ->
			    (Device.Dm.Intel (Device.Dm.Std_vga,dom0_input),[])
			| "none" -> 
			    (Device.Dm.NONE,[])
			| _ -> failwith "Unknown vga_mode"
		    with _ -> 
		      warn "Failed to parse 'vga_mode' parameter - expecting 'passthrough' or 'intel'. Defaulting to VNC mode";
		      default_disp_usb
		  end else default_disp_usb		    
		in
		let pci_passthrough = match pci_passthrough with
			| None -> List.mem_assoc "pci" other_config
			| Some p -> p
		in
		let info = {
			Device.Dm.memory = mem_max_kib;
			Device.Dm.boot = boot;
			Device.Dm.serial = serial;
			Device.Dm.vcpus = vcpus;
			Device.Dm.nics = nics;
			Device.Dm.disks = disks;
			Device.Dm.pci_emulations = pci_emulations;
			Device.Dm.usb = usb;
			Device.Dm.acpi = acpi;
			Device.Dm.disp = disp;
			Device.Dm.pci_passthrough = pci_passthrough;
			Device.Dm.xenclient_enabled=Xapi_globs.xenclient_enabled;
			Device.Dm.hvm=hvm;
			Device.Dm.sound=None;
			Device.Dm.power_mgmt=None;
			Device.Dm.oem_features=None;
			Device.Dm.inject_sci = None;
			Device.Dm.video_mib=video_mib;

			Device.Dm.extras = [];
		} in
		dmstart ~xs ~dmpath info domid;
		(* We can now attempt to unplug the qemu frontends, safe in the
		   knowledge that qemu won't close its filehandles until it has
		   finished with them. *)
		List.iter (fun vbd -> Storage_access.Qemu_blkfront.unplug_nowait ~__context ~self:vbd) snapshot.API.vM_VBDs
	end else begin
	        (* if we have a "disable_pv_vnc" entry in other_config we disable
		   VNC for PV *)
		let disable_pv_vnc = List.mem_assoc "disable_pv_vnc" other_config in
		if not disable_pv_vnc then Device.PV_Vnc.start ~xs domid ?statefile:vnc_statefile
	end

let create_vfb_vkbd ~xc ~xs ?protocol domid =
  Device.Vfb.add ~xc ~xs ?protocol domid;
  Device.Vkbd.add ~xc ~xs ?protocol domid

(* get CD VBDs required to resume *)
let get_required_CD_VBDs ~__context ~vm =
  List.filter (fun self -> (Db.VBD.get_currently_attached ~__context ~self) 
      && (Db.VBD.get_type ~__context ~self = `CD))
    (Db.VM.get_VBDs ~__context ~self:vm)
   
(* get non-CD VBDs required to resume *)
let get_required_nonCD_VBDs ~__context ~vm =
  List.filter (fun self -> (Db.VBD.get_currently_attached ~__context ~self) 
      && (Db.VBD.get_type ~__context ~self <> `CD))
    (Db.VM.get_VBDs ~__context ~self:vm)
   
(* restore CD drives. This needs to happen earlier in the restore sequence. 
 * See CA-17925
 *)
let _restore_CD_devices ~__context ~xc ~xs ~self at_boot_time fd domid vifs =
  	let hvm = Helpers.will_boot_hvm ~__context ~self in
	let protocol = Helpers.device_protocol_of_string (Db.VM.get_domarch ~__context ~self) in
    let needed_vbds = Storage_access.vbd_attach_order ~__context (get_required_CD_VBDs ~__context ~vm:self) in
	let string_of_vbd_list vbds = String.concat "; " 
	  (List.map (fun vbd -> string_of_vbd ~__context ~vbd) vbds) in
    debug "CD VBDs: [ %s ]" (string_of_vbd_list needed_vbds);

  	(* If any VBDs cannot be attached, let the exn propagate *)
	List.iter (fun self -> create_vbd ~__context ~xs ~hvm ~protocol domid self) needed_vbds
  
(* restore the devices, but not the domain; the vdis must all be attached/activated by the
   time this function is executed
*)
let _restore_devices ~__context ~xc ~xs ~self at_boot_time fd domid vifs includeCDs =
	let hvm = Helpers.will_boot_hvm ~__context ~self in
	(* CA-25297: domarch is r/o and not currently stored in the LBR *)
	let protocol = Helpers.device_protocol_of_string (Db.VM.get_domarch ~__context ~self) in

	let string_of_vbd_list vbds = String.concat "; " 
	  (List.map (fun vbd -> string_of_vbd ~__context ~vbd) vbds) in

	(* We /must/ be able to re-attach all the VBDs the guest had when it suspended. *)
	let needed_vbds = 
      (if includeCDs then get_required_CD_VBDs ~__context ~vm:self else [])
		@ (get_required_nonCD_VBDs ~__context ~vm:self) in
	let needed_vbds = Storage_access.vbd_attach_order ~__context needed_vbds in
	debug "To restore this domain we need VBDs: [ %s ]" (string_of_vbd_list needed_vbds);
	
	(* If any VBDs cannot be attached, let the exn propagate *)
	List.iter (fun self -> create_vbd ~__context ~xs ~hvm ~protocol domid self) needed_vbds;
	
	create_cpus ~xs at_boot_time domid;
	create_vifs ~__context ~xs vifs;
	
	debug "setting current number of vcpus to %Ld (out of %Ld)" 
	  at_boot_time.API.vM_VCPUs_at_startup at_boot_time.API.vM_VCPUs_max;
	set_cpus_number ~__context ~xs ~self domid at_boot_time

(* restore the domain, assumes the devices are already set up *)
let _restore_domain ~__context ~xc ~xs ~self at_boot_time fd ?vnc_statefile domid vifs =
	let ballooning_enabled = Helpers.ballooning_enabled_for_vm ~__context at_boot_time in

	(* NB the last-known timeoffset is used, not the at boot time one *)
	let timeoffset = try List.assoc "timeoffset" (Db.VM.get_platform ~__context ~self) with _ -> "0" in
	(* MTC: If it is a protected VM, the destination VM may not yet be booted, so calling
	 * Helpers.has_booted_hvm here would fail to find a boot record.  Instead, we have
         * changed the code to use the source VM's boot record (the argument at_boot_time)
         * and used that to determine if we're dealing with a VM.  This change should not 
         * break the intent of the original XenSource code.
         *)
	let hvm = Helpers.is_hvm at_boot_time in
	let vcpus = Int64.to_int (at_boot_time.API.vM_VCPUs_max) in
	let static_max_kib = Int64.div (at_boot_time.API.vM_memory_static_max) 1024L in
	let target_kib = if ballooning_enabled 
	  then Int64.div at_boot_time.API.vM_memory_target 1024L (* suspend copies this number from total_pages *)
	  else static_max_kib in 

	if hvm then (
		let shadow_multiplier = at_boot_time.API.vM_HVM_shadow_multiplier in
		Domain.hvm_restore ~xc ~xs domid ~static_max_kib ~target_kib ~shadow_multiplier ~vcpus
		                   ~timeoffset fd;
	) else (
		Domain.pv_restore ~xc ~xs domid ~static_max_kib ~target_kib ~vcpus fd;
	);
	create_device_emulator ~__context ~xc ~xs ~self ~restore:true ?vnc_statefile
		domid vifs at_boot_time;
	(* write in the current policy info *)
	write_memory_policy ~xs { at_boot_time with API.
		vM_memory_dynamic_min = Db.VM.get_memory_dynamic_min ~__context ~self;
		vM_memory_dynamic_max = Db.VM.get_memory_dynamic_max ~__context ~self;
	} domid

(** In Rio suspend images had filenames of the form: 
      suspend-image-<VM UUID>
    but in Miami this was changed to:
      suspend-image
    Return the actual filename in use by this suspend VDI *)
let find_suspend_image mountpoint = 
  match List.filter (String.startswith "suspend-image") (Array.to_list (Sys.readdir mountpoint)) with
  | [] ->
      error "Suspend VDI contained no files starting with 'suspend-image': suspend VDI is broken";
      failwith "Suspend VDI contained no 'suspend-image'* file"
  | x :: [] -> 
      mountpoint ^ "/" ^ x
  | x :: _ ->
      warn "Suspend VDI contains multiple files starting with 'suspend-image': how did this happen?";
      mountpoint ^ "/" ^ x

let find_vnc_statefile mountpoint =
	match List.filter (String.startswith "vncterm.statefile") (Array.to_list (Sys.readdir mountpoint)) with
	| [] -> 
		  debug "restore: no vncterm.statefile found"; None
	| h::_ -> 
		  let filename = mountpoint ^ "/" ^ h in
		  debug "restore: a vncterm statefile (%s) has been found" filename;
		  Some filename

(** Get the suspend VDI, mount the disk, open the file and call _restore*. Guarantees to
    always unmount the VDI and, only if the restore succeeds, deletes the VDI (otherwise
    an exception from lower-level code propagates out *)
let restore ~__context ~xc ~xs ~self start_paused =
  let memory_required_kib = Memory.kib_of_bytes_used (Memory_check.vm_compute_resume_memory ~__context self) in
  let snapshot = Helpers.get_boot_record ~__context ~self in
  (* CA-31759: we always use the live memory_target *)
  let snapshot = { snapshot with API.vM_memory_target = Db.VM.get_memory_target ~__context ~self } in

  let reservation_id = Memory_control.reserve_memory ~__context ~xc ~xs ~kib:memory_required_kib in
  let domid = create ~__context ~xc ~xs ~self ~reservation_id snapshot () in
  let other_pcidevs = Pciops.other_pcidevs_of_vm ~__context ~vm:self in

  (* Ensure no old consoles survive *)
  destroy_consoles ~__context ~vM:self;

  Xapi_xenops_errors.handle_xenops_error
    (fun () ->
       let suspend_vdi = Db.VM.get_suspend_VDI ~__context ~self in


       Sm_fs_ops.with_fs_vdi __context suspend_vdi
	 (fun mount_point ->
	    let filename = find_suspend_image mount_point in
		let vnc_statefile = find_vnc_statefile mount_point in
	    debug "Using suspend image: %s" filename;
	    let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o400 in
	    finally 
			(fun () ->
				try
					let vifs = Vm_config.vifs_of_vm ~__context ~vm:self domid in
					_restore_devices ~__context ~xc ~xs ~self snapshot fd domid vifs true;
					_restore_domain ~__context ~xc ~xs ~self snapshot fd ?vnc_statefile domid vifs;
				with exn ->
					error "Vmops.restore caught: %s" (ExnHelper.string_of_exn exn);
					destroy ~__context ~xc ~xs ~self ~clear_currently_attached:false domid `Suspended;
					raise exn (* re-raise *)
		    )
			(fun () -> Helpers.log_exn_continue "restore" (fun () -> Unix.close fd) ())
	 );
       
       (* No exception must have happened: safe to destroy the VDI *)
	   begin 
		 try
		   Helpers.call_api_functions ~__context
			   (fun rpc session_id ->
					Client.VDI.destroy rpc session_id suspend_vdi)
		 with _ ->
			 (* This should never happen but just in case, we log prominently and continue *)
			 error "Failed to delete suspend image VDI: %s" (Db.VDI.get_uuid ~__context ~self:suspend_vdi);
	   end;
       Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null;

	   Db.VM.set_domid ~__context ~self ~value:(Int64.of_int domid);

	   debug "Vmops.restore: %s unpausing domain" (if start_paused then "not" else "");
	   if not start_paused then Domain.unpause ~xc domid;
	   Pciops.plug_pcis ~__context ~vm:self domid [] other_pcidevs;
    )


(** Thrown if clean_shutdown_with_reason exits for the wrong reason: eg the domain
    crashed or rebooted *)
exception Domain_shutdown_for_wrong_reason of Xal.died_reason

(** Tells a VM to shutdown with a specific reason (reboot/halt/poweroff), waits for
    it to shutdown (or vanish) and then return the reason.
	Note this is not always called with the per-VM mutex. *)
let clean_shutdown_with_reason ?(at = fun _ -> ()) ~xal ~__context ~self ?(rel_timeout = 5.) domid reason =
  (* Set the task allowed_operations to include cancel *)
  if reason <> Domain.Suspend then TaskHelper.set_cancellable ~__context;

  at 0.25;
  let xs = Xal.xs_of_ctx xal in
  let xc = Xal.xc_of_ctx xal in
  begin
	(* Wait for up to 60s for the VM to acknowledge the shutdown request. In case the guest
	   misses our original request, keep making additional ones. *)
	let finished = ref false in
	let start = Unix.gettimeofday () in
	while Unix.gettimeofday () -. start < !Xapi_globs.domain_shutdown_ack_timeout && not !finished do
	  try
		(* Make the shutdown request: this will fail if the domain has vanished. *)
		Domain.shutdown ~xs domid reason;
		(* Wait for any necessary acknowledgement. If we get a Watch.Timeout _ then
		   we abort early; otherwise we continue in Xal.wait_release below. *)
		Domain.shutdown_wait_for_ack ~timeout:10. ~xc ~xs domid reason;
		finished := true
	  with 
	  | Watch.Timeout _ -> () (* try again *)
	  | e ->
			debug "Caught and ignoring exception: %s" (ExnHelper.string_of_exn e);
			log_backtrace ();
			finished := true
	done;
	if not !finished then raise (Api_errors.Server_error (Api_errors.vm_failed_shutdown_ack, []))
  end;
  at 0.50;
  (* Block for 5s at a time, in between check to see whether we've been cancelled
     and update our progress if not *)
  let start = Unix.gettimeofday () in
  let result = ref None in
  while (Unix.gettimeofday () -. start < !Xapi_globs.domain_shutdown_total_timeout) && (!result = None) do
    try
		debug "MTC: calling xal.wait_release timeout=%f" rel_timeout;
		Xs.monitor_paths xs [ "@releaseDomain","X" ] rel_timeout
			(fun _ ->
				 try (Xenctrl.domain_getinfo xc domid).Xenctrl.shutdown with Xenctrl.Error _ -> true);
		result := Some (try Domain.shutdown_reason_of_int (Xenctrl.domain_getinfo xc domid).Xenctrl.shutdown_code with _ -> Domain.Unknown (-1));
    with Xs.Timeout -> 
      if reason <> Domain.Suspend && TaskHelper.is_cancelling ~__context
      then raise (Api_errors.Server_error(Api_errors.task_cancelled, [ Ref.string_of (Context.get_task_id __context) ]));
      (* Update progress and repeat *)
      let progress = min ((Unix.gettimeofday () -. start) /. !Xapi_globs.domain_shutdown_total_timeout) 1. in
      at (0.50 +. 0.25 *. progress)
  done;
  match !result with
  | None -> raise (Api_errors.Server_error(Api_errors.vm_shutdown_timeout, [ Ref.string_of self; string_of_float !Xapi_globs.domain_shutdown_total_timeout ]))
  | Some x ->
		at 1.0;
		x

(* !!! FIX ME  - This allows a 10% overhead on static_max for size of suspend image !!! *)
let get_suspend_space __context vm =
  let at_boot_time = Helpers.get_boot_record ~__context ~self:vm in
  let mem_max = at_boot_time.API.vM_memory_static_max in
  Int64.add (Int64.of_float ((Int64.to_float mem_max) *. 1.2)) 104857600L

exception Domain_architecture_not_supported_in_suspend

let suspend ~live ~progress_cb ~__context ~xc ~xs ~vm =
	let uuid = Db.VM.get_uuid ~__context ~self:vm in
	let domid = Helpers.domid_of_vm ~__context ~self:vm in
	let is_paused = Db.VM.get_power_state ~__context ~self:vm = `Paused in
	let min = Db.VM.get_memory_dynamic_min ~__context ~self:vm in
	let max = Db.VM.get_memory_dynamic_max ~__context ~self:vm in
	let min = Int64.to_int (Int64.div min 1024L) in
	let max = Int64.to_int (Int64.div max 1024L) in
	let suspend_SR = Helpers.choose_suspend_sr ~__context ~vm in
	let required_space = get_suspend_space __context vm in
	let handle_death = function
		| Domain.Suspend ->
			() (* good *)
		| Domain.Crash ->
			raise (Api_errors.Server_error(Api_errors.vm_crashed, [ Ref.string_of vm ]))
		| Domain.Reboot ->
			raise (Api_errors.Server_error(Api_errors.vm_rebooted, [ Ref.string_of vm ]))
		| Domain.Halt | Domain.PowerOff | Domain.S3Suspend | Domain.Unknown _ ->
			raise (Api_errors.Server_error(Api_errors.vm_halted, [ Ref.string_of vm ]))
	in
	let suspend_domain ~fd ~hvm () = with_xal (fun xal ->
		Domain.suspend ~xc ~xs ~hvm domid fd [] ~progress_callback:progress_cb
			(fun () ->
				handle_death
					(clean_shutdown_with_reason
						~xal ~__context ~self:vm domid Domain.Suspend)))
	in
	let do_suspend () =
		(* Balloon down the guest as far as we can to force it to clear unnecessary caches etc. *)
		debug "suspend phase 0/4: asking guest to balloon down";
		Domain.set_memory_dynamic_range ~xs ~min ~max:min domid;
		Memory_control.balance_memory ~__context ~xc ~xs;
		debug "suspend phase 1/4: hot-unplugging any PCI devices";
		let hvm = (Xenctrl.domain_getinfo xc domid).Xenctrl.hvm_guest in
		if hvm then Pciops.unplug_pcidevs_noexn ~__context ~vm domid (Device.PCI.list xc xs domid);
		Sm_fs_ops.with_new_fs_vdi __context
			~name_label:"Suspend image" ~name_description:"Suspend image"
			~sR:suspend_SR ~_type:`suspend ~required_space
			~sm_config:[Xapi_globs._sm_vm_hint, uuid]
			(fun vdi_ref mount_point ->
				let filename = sprintf "%s/suspend-image" mount_point in
				debug "suspend: phase 2/4: opening suspend image file (%s)"
					filename;
				(* NB if the suspend file already exists it will be *)
				(* overwritten. *)
				let fd = Unix.openfile filename
					[ Unix.O_WRONLY; Unix.O_CREAT ] 0o600 in
				finally
					(fun () ->
						debug "suspend: phase 3/4: suspending to disk";
						suspend_domain ~fd ~hvm ();
						(* If the suspend succeeds, set the suspend_VDI *)
						Db.VM.set_suspend_VDI ~__context ~self:vm ~value:vdi_ref;)
						(fun () ->
						   try
						     Unixext.fsync fd;
						     Unix.close fd
						   with Unix.Unix_error(Unix.EIO, _, _) ->
						     raise (Api_errors.Server_error (Api_errors.vdi_io_error, ["I/O error saving VM suspend image"]))
						);
				debug "suspend: complete");
		debug "suspend phase 4/4: recording memory usage";
		(* Record the final memory usage of the VM, so that we know how much *)
		(* memory to free before attempting to resume this VM in future.     *)
		let di = with_xc (fun xc -> Xenctrl.domain_getinfo xc domid) in
		let final_memory_bytes = Memory.bytes_of_pages
			(Int64.of_nativeint di.Xenctrl.total_memory_pages) in
		debug "total_memory_pages=%Ld; storing target=%Ld"
			(Int64.of_nativeint di.Xenctrl.total_memory_pages) final_memory_bytes;
		(* CA-31759: avoid using the LBR to simplify upgrade *)
		Db.VM.set_memory_target ~__context ~self:vm ~value:final_memory_bytes
	in
	let do_final_actions_after_suspend () =
		Domain.set_memory_dynamic_range ~xs ~min ~max domid;
		Memory_control.balance_memory ~__context ~xc ~xs;
		if is_paused then (try Domain.pause ~xc domid with _ -> ())
	in
	Xapi_xenops_errors.handle_xenops_error (fun () ->
		with_xc_and_xs (fun xc xs ->
			if is_paused then Domain.unpause ~xc domid;
			finally
				(do_suspend)
				(do_final_actions_after_suspend)))

let resume ~__context ~xc ~xs ~vm =
	let domid = Helpers.domid_of_vm ~__context ~self:vm in
	let hvm = Helpers.has_booted_hvm ~__context ~self:vm in
	Xapi_xenops_errors.handle_xenops_error (fun () ->
		(* TTT: check if the domain is really cooperative *)
		Domain.resume ~xs ~xc ~hvm ~cooperative:true domid;
		Domain.cpuid_apply ~xc ~hvm domid;
		Domain.unpause ~xc domid) 

(** Starts up a VM, leaving it in the paused state *)
let start_paused ?(progress_cb = fun _ -> ()) ~pcidevs ~__context ~vm ~snapshot =
	(* Ensure no old consoles survive *)
	destroy_consoles ~__context ~vM:vm;

	check_vm_parameters ~__context ~self:vm ~snapshot;
	(* Take the subset of locked VBDs *)
	let other_config = Db.VM.get_other_config ~__context ~self:vm in
	let vbds = Vbdops.vbds_to_attach ~__context ~vm in
	let overhead_bytes = Memory_check.vm_compute_memory_overhead snapshot in
	(* NB the database value isn't authoritative because (eg) over upgrade it will be 0L *)
	Db.VM.set_memory_overhead ~__context ~self:vm ~value:overhead_bytes;
	with_xc_and_xs
		(fun xc xs ->
			let target_plus_overhead_kib, reservation_id =
				Memory_control.reserve_memory_range
					~__context ~xc ~xs
					~min:(Memory.kib_of_bytes_used
						(snapshot.API.vM_memory_dynamic_min +++ overhead_bytes))
					~max:(Memory.kib_of_bytes_used
						(snapshot.API.vM_memory_dynamic_max +++ overhead_bytes)) in
			let target_plus_overhead_bytes =
				Memory.bytes_of_kib target_plus_overhead_kib in
			let target_bytes =
				target_plus_overhead_bytes --- overhead_bytes in
			(* NB due to rounding up on the bytes to kib conversion
			   we might end up with more memory free than we need:
			   this is ok; we don't have to use it all. *)
			let target_bytes = min snapshot.API.vM_memory_dynamic_max target_bytes in
			if target_bytes < snapshot.API.vM_memory_dynamic_min then begin
				error
					"target = %Ld; dynamic_min = %Ld; overhead = %Ld; target+overhead = %Ld"
					target_bytes
					snapshot.API.vM_memory_dynamic_min
					overhead_bytes
					target_plus_overhead_bytes;
			end;
			assert (target_bytes >= snapshot.API.vM_memory_dynamic_min);
			assert (target_bytes <= snapshot.API.vM_memory_dynamic_max);
			let snapshot = { snapshot with API.vM_memory_target = target_bytes } in

			let hvm = Helpers.is_hvm snapshot in

			let (domid: Domain.domid) =
				create ~__context ~xc ~xs ~self:vm snapshot ~reservation_id () in
			try
					Db.VM.set_domid ~__context ~self:vm ~value:(Int64.of_int domid);
					progress_cb 0.25;
					clear_all_device_status_fields ~__context ~self:vm;
					progress_cb 0.30;
					debug "Verifying VDI records exist";
					List.iter (fun vbd -> check_vdi_exists ~__context ~vbd) vbds;
					let width =
						try Some (int_of_string (List.assoc "machine-address-size" other_config))
						with _ -> None in
					Domain.set_machine_address_size ~xc domid width;

					let non_empty_vbds = List.filter (fun self -> not(Db.VBD.get_empty ~__context ~self)) vbds in
					let vdis = List.map (fun self -> Db.VBD.get_VDI ~__context ~self) non_empty_vbds in
					let vdis_with_timeoffset_to_be_reset_on_boot =
						vdis
							|> List.map (fun self -> (self, Db.VDI.get_record ~__context ~self))
							|> List.filter (fun (_, record) -> record.API.vDI_on_boot = `reset)
							|> List.filter_map (fun (reference, record) ->
								Opt.of_exception (fun () ->
									reference,
									List.assoc "timeoffset"
										record.API.vDI_other_config)) in
					let snapshot = match vdis_with_timeoffset_to_be_reset_on_boot with
						| [] ->
							snapshot
						| [(reference, timeoffset)] ->
							{snapshot with
								API.vM_platform =
									("timeoffset", timeoffset) ::
										(List.filter
											(fun (key, _) -> key <> "timeoffset")
											(snapshot.API.vM_platform))}
						| reference_timeoffset_pairs ->
							raise (Api_errors.Server_error (
								(Api_errors.vm_attached_to_more_than_one_vdi_with_timeoffset_marked_as_reset_on_boot),
									(Ref.string_of vm) ::
										(reference_timeoffset_pairs
											|> List.map fst
											|> List.map Ref.string_of))) in
					(* This might involve a block-attach for bootloader:
					   do this early before locking the VDIs *)
					debug "creating kernel";
					create_kernel ~__context ~xc ~xs ~self:vm domid snapshot;
					progress_cb 0.35;

					Domain.cpuid_apply ~xc ~hvm domid;

						debug "creating VCPU devices and attaching to domain";
								create_cpus ~xs snapshot domid;
								progress_cb 0.40;
								debug "creating VBD devices and attaching to domain";
								(* CA-25297: domarch is r/o and not currently stored in the LBR *)
								let protocol =
									Helpers.device_protocol_of_string
										(Db.VM.get_domarch ~__context ~self:vm) in
								(* If any VBDs cannot be attached, let the exn propagate *)
								List.iter
									(fun self ->
										create_vbd ~__context ~xs ~hvm ~protocol domid self)
									vbds;
								progress_cb 0.60;
								debug "creating VIF devices and attaching to domain";
								let vifs = Vm_config.vifs_of_vm ~__context ~vm domid in
								create_vifs ~__context ~xs vifs;
								progress_cb 0.70;
								let pcis = Vgpuops.create_vgpus ~__context ~vm domid hvm in
								(* WORKAROUND FOR CA-55754: temporarily disable msitranslate when GPU is passed through. *)
								(* other-config:msitranslate can be used the override the default *)
								let msitranslate =
									let oc = Db.VM.get_other_config ~__context ~self:vm in
									if List.mem_assoc "msitranslate" oc then
										Some (List.assoc "msitranslate" oc)
									else
										if pcis <> [] then Some "0" (* pcis only contains GPUs here *)
										else None
								in
								begin
									match msitranslate with
									| Some msitranslate ->
										debug "Setting msitranslate = %s" msitranslate;
										let msitranslate_path = "/local/domain/0/backend/pci/" ^ (string_of_int domid) ^ "/0/msitranslate" in
										Vmopshelpers.with_xs (fun xs -> xs.Xs.write msitranslate_path msitranslate)
									| None -> ()
								end;
								let other_pcidevs =
									match pcidevs with Some x -> x | None -> Pciops.other_pcidevs_of_vm ~__context ~vm in
								let pci_passthrough = pcis <> [] || other_pcidevs <> [] in
								if not hvm then
									Pciops.attach_pcis ~__context ~xc ~xs ~hvm domid other_pcidevs;
								if true
									&& (not hvm)
									&& (has_platform_flag snapshot.API.vM_platform "pvfb")
								then create_vfb_vkbd ~xc ~xs domid; 
								progress_cb 0.75;
								debug "adjusting CPU number against startup-number";
								set_cpus_number ~__context ~xs ~self:vm domid snapshot;
								progress_cb 0.80;
								debug "creating device emulator";
								create_device_emulator ~__context ~xc ~xs ~self:vm
									~pci_passthrough domid vifs snapshot;
								if hvm then
									Pciops.plug_pcis ~__context ~vm domid pcis other_pcidevs;
								debug "writing memory policy";
								write_memory_policy ~xs snapshot domid;
								Db.VM_metrics.set_start_time
									~__context
									~self:snapshot.API.vM_metrics
									~value:(Date.of_float (Unix.gettimeofday ()));
								Helpers.set_boot_record ~__context ~self:vm snapshot
			with exn ->
				error "Vmops.start_paused caught: %s" (ExnHelper.string_of_exn exn);
				info "Memory F %Ld KiB S %Ld KiB T %Ld MiB"
					(Memory.get_free_memory_kib xc)
					(Memory.get_scrub_memory_kib xc)
					(Memory.get_total_memory_mib xc);
				destroy ~__context ~xc ~xs ~self:vm domid `Halted;
				raise (Xapi_xenops_errors.to_api_error exn) (* re-raise *)
		)
