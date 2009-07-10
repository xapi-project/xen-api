open Printf
open Threadext
open Stringext
open Pervasiveext
open Vmopshelpers
open Client
open Vbdops

module D = Debug.Debugger(struct let name="xapi" end)
open D

module L = Debug.Debugger(struct let name="license" end)

type api_access = InternalNetwork | FirstNetwork

(* This is only used to block the 'present multiple physical cores as one big hyperthreaded core' feature *)
let filtered_platform_flags = ["acpi"; "apic"; "nx"; "pae"; "viridian"] 

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
	) (Db.VM.get_VIFs ~__context ~self)

(* Called on VM.start codepath only to validate current VM parameters *)
let check_vm_parameters ~__context ~self ~snapshot =
	let hvm = Helpers.is_hvm snapshot in
	let refself = Ref.string_of self in
	let check_vm_vbds () =
		let vbds = Db.VM.get_VBDs ~__context ~self in
		let h = Hashtbl.create (List.length vbds) in
		let hashtbl_exists h e = try ignore (Hashtbl.find h e); true with Not_found -> false in
		List.iter (fun vbd ->
			let userdevice = Db.VBD.get_userdevice ~__context ~self:vbd in
			let realdevice = translate_vbd_device userdevice hvm in
			if hashtbl_exists h realdevice then
				raise (Api_errors.Server_error(Api_errors.vm_duplicate_vbd_device,
				                               [ refself; Ref.string_of vbd; realdevice ]))
			else
				Hashtbl.add h realdevice 0
		) vbds
		in
	let vcpus = Int64.to_int snapshot.API.vM_VCPUs_max in
	let mem = snapshot.API.vM_memory_static_max in
	if vcpus <= 0 then
		raise (Api_errors.Server_error (Api_errors.vm_no_vcpus, [ refself ]));
	if vcpus >= 64 then
		raise (Api_errors.Server_error (Api_errors.vm_toomany_vcpus, [ refself ]));
	if mem < Xapi_globs.vm_minimum_memory then
		raise (Api_errors.Server_error (Api_errors.vm_memory_size_too_low, [ refself ]));
	check_vm_vbds ();
	()

let add_vif ~__context ~xs vif_device = 
  if vif_device.Vm_config.bridge = "" then failwith "Don't know how to connect a VIF to this type of Network";
  Xapi_network.attach_internal ~__context ~self:vif_device.Vm_config.network_ref ();
  Xapi_udhcpd.maybe_add_lease ~__context vif_device.Vm_config.vif_ref;

  Xapi_xenops_errors.handle_xenops_error
    (fun () ->
       let (_: Device_common.device) = Device.Vif.add ~xs ~devid:vif_device.Vm_config.devid ~netty:(Netman.Bridge vif_device.Vm_config.bridge) 
	 ~mac:vif_device.Vm_config.mac ~mtu:vif_device.Vm_config.mtu ~rate:vif_device.Vm_config.rate ~protocol:vif_device.Vm_config.protocol 
	 ~other_config:vif_device.Vm_config.other_config ~extra_private_keys:[ "ref", Ref.string_of vif_device.Vm_config.vif_ref ] 
	 vif_device.Vm_config.domid in
       ()
    );
  Db.VIF.set_currently_attached ~__context ~self:vif_device.Vm_config.vif_ref ~value:true

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

let attach_pcis ~__context ~xc ~xs ~hvm domid pcis =
	List.iter (fun (devid, devs) ->
		Device.PCI.bind devs;
		Device.PCI.add ~xc ~xs ~hvm ~msitranslate:0 ~pci_power_mgmt:0 devs domid devid
	) pcis

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

(* Called on both VM.start and VM.resume codepaths to set cpus weight and cap *)
let set_cpus_qos ~__context ~xc ~self domid snapshot =
        let weight_param = "weight" in
        let cap_param = "cap" in
        let params = snapshot.API.vM_VCPUs_params in
	let alert reason = warn "cpu qos failed: %s" reason in

	let rstr = Restrictions.get () in
        if rstr.Restrictions.enable_qos then begin
	  let set_cap cap =
		let ctrl = Xc.sched_credit_domain_get xc domid in
		let newctrl = { ctrl with Xc.cap = cap } in
		Xc.sched_credit_domain_set xc domid newctrl
		in
	  let set_weight weight =
		let ctrl = Xc.sched_credit_domain_get xc domid in
		let newctrl = { ctrl with Xc.weight = weight } in
		Xc.sched_credit_domain_set xc domid newctrl
		in
	  let set_control_full weight cap =
		let ctrl = { Xc.weight = weight; Xc.cap = cap } in
		Xc.sched_credit_domain_set xc domid ctrl
		in
	  let set_credit_policy weight cap =
		match weight, cap with
		| None, None     -> alert "no parameter set, ignoring"
		| Some w, None   -> set_weight w
		| None, Some c   -> set_cap c
		| Some w, Some c -> set_control_full w c
		in
	  match Xc.sched_id xc with
	  | 5 -> (
		try
			let weight =
				try Some (int_of_string (List.assoc weight_param params))
				with _ -> None
			and cap =
				try Some (int_of_string (List.assoc cap_param params))
				with _ -> None in
			set_credit_policy weight cap
		with exn ->
			alert (ExnHelper.string_of_exn exn);
	      )
	  | _           ->
	  	alert "scheduler and policy mismatch";
	end
	else begin
	  (* Here we log the fact that we're ignoring this QoS stuff *)
	  if (List.mem_assoc weight_param params) || (List.mem_assoc cap_param params) then (
	    L.info "Ignoring CPU QoS params due to license restrictions";
	    alert "license restrictions";
	  )
	end

(* Called on both VM.start and VM.resume codepaths to pin vcpus *)
let set_cpus_mask_norestrictions ~__context ~xc ~self domid snapshot mask =
	let max_vcpus = Int64.to_int snapshot.API.vM_VCPUs_max in
	let all_cpu_affinity_set ~xc domid cpumap =
		for cpu = 0 to max_vcpus - 1
		do
			try
				Domain.vcpu_affinity_set ~xc domid cpu cpumap
			with exn ->
				warn "setting cpu mask: setting vcpu on cpu %d failed: %s"
				     cpu (ExnHelper.string_of_exn exn)
		done
		in

	try
		let cpus = List.map int_of_string (String.split ',' mask) in
		let cpumap = Array.make 64 false in
		List.iter (fun i ->
			if i >= 64 || i < 0 then
				warn "setting cpu mask: ignoring cpu %d -- valid value is between 0 and 64" i
			else
				cpumap.(i) <- true) cpus;
		all_cpu_affinity_set ~xc domid cpumap
	with exn ->
		warn "setting cpu mask failed: %s" (ExnHelper.string_of_exn exn)

(* Called on both VM.start and VM.resume codepaths to pin vcpus *)
let set_cpus_mask ~__context ~xc ~self domid snapshot =
	let params = snapshot.API.vM_VCPUs_params in
	let mask = try Some (List.assoc "mask" params) with _ -> None in

	let rstr = Restrictions.get () in
	if rstr.Restrictions.enable_qos then (
		match mask with
		| None      -> ()
		| Some mask ->
			set_cpus_mask_norestrictions ~__context ~xc ~self
			                             domid snapshot mask
	) else (
		match mask with
		| Some _ -> L.info "Ignoring CPU mask setting due to license restriction"
		| None -> ()
	)

let update_vm_last_booted ~__context ~self =
	let metrics = Db.VM.get_metrics ~__context ~self in
	if metrics <> Ref.null then (
		let value = Date.of_float (Unix.gettimeofday ()) in
		Db.VM_metrics.set_start_time ~__context ~self:metrics ~value
	)

(* Called on both VM.start and VM.resume to create and attach a single console using RFB to a VM *)
let create_console ?(vncport=(-1)) ~__context ~vM () =
	let port = Int64.of_int vncport in
	let localhost = Helpers.get_localhost ~__context in
	let address = Db.Host.get_address ~__context ~self:localhost in
	let location = Printf.sprintf "https://%s%s?ref=%s" address Constants.console_uri (Ref.string_of vM) in
	(* Ensure we have at most one console with the correct port *)
	List.iter (fun self -> Db.Console.destroy ~__context ~self) (Db.VM.get_consoles ~__context ~self:vM);
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let ref = Ref.make () in
	Db.Console.create ~__context ~ref ~uuid ~protocol:`rfb ~location ~vM ~other_config:[] ~port

(* Detach all consoles from a guest, used when shutting down/pausing *)
let destroy_consoles ~__context ~vM =
	let all = Db.VM.get_consoles ~__context ~self:vM in
	List.iter (fun console -> Db.Console.destroy ~__context ~self:console) all

(* Called on VM.start to make sure enough memory is available in theory and to wait for it
   to become available in practice *)
let check_enough_memory ~__context ~snapshot ~xc ~restore =
  let main, shadow = Memory_check.vm_compute_start_memory ~__context snapshot in
  let needed_b = Int64.add main shadow in
  let needed_kib = Int64.div needed_b 1024L in

  if not (Memory.wait_xen_free_mem xc needed_kib) then begin
    let free_mem = Memory.get_free_memory_kib ~xc in
    error "not enough memory available: %Ld available %Ld required" free_mem needed_kib;
    (* Get some debug messages printed: *)
    let host = Helpers.get_localhost () in
    ignore(Memory_check.host_compute_free_memory ~dump_stats:false ~__context ~host None);

    raise (Api_errors.Server_error(Api_errors.host_not_enough_free_memory, [ Int64.to_string needed_kib; Int64.to_string free_mem ]))
  end


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
	| Helpers.HVM { Helpers.pae = pae;
	                apic = apic; acpi = acpi;
	                nx = nx; viridian = viridian; timeoffset = timeoffset; } ->
		let kernel_path = Domain.hvmloader in
		let shadow_multiplier = snapshot.API.vM_HVM_shadow_multiplier in
		debug "build hvm \"%s\" vcpus:%d mem_max:%Ld mem_target:%Ld pae:%b apic:%b acpi:%b nx:%b viridian:%b timeoffset:%s"
		      kernel_path vcpus mem_max_kib mem_target_kib pae apic acpi nx viridian timeoffset;
		let arch = Domain.build_hvm xc xs mem_max_kib mem_target_kib shadow_multiplier vcpus kernel_path
		  pae apic acpi nx viridian timeoffset domid in
		(* Since our shadow allocation might have been increased by Xen we need to 
		   update the shadow_multiplier now. Nb. the last_boot_record wont 
		   necessarily have the right value in! *)
		let multiplier = Memory.HVM.round_shadow_multiplier vcpus mem_max_kib shadow_multiplier domid in
		Db.VM.set_HVM_shadow_multiplier ~__context ~self ~value:multiplier;
		arch
	| Helpers.DirectPV { Helpers.kernel = pv_kernel;
			     kernel_args = pv_args;
			     ramdisk = pv_ramdisk } ->
		(* No bootloader; get the kernel, initrd from dom0 *)

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

let grant_guest_api_access ~xs ~self domid access_type =
	(* Here we write into the store a URL the guest can use to invoke functions in the API.
	   We only do this if: 
	   (1) The guest has the key 'grant_api_access' in its other-config (this is removed when it's checked)
	   (2) If grant_api_access is 'internal' and the guest has a VIF on the guest installer network, or grant_api_access is the empty string.
	   In future we might want to:
	   (3) limit the access rights associated with this session *)
        try
	  let port = !Xapi_globs.https_port in
	  let ip = ref "" in
	  let mac = ref "" in
          let session_id = ref (Ref.of_string "") in
	  Server_helpers.exec_with_new_task "guest" ~task_in_database:true
	    (fun __context ->
              session_id := Xapi_session.login_no_password ~__context ~uname:(Some "root") ~pool:false ~is_local_superuser:true ~subject:(Ref.null) ~auth_user_sid:""
                                  ~host:(Helpers.get_localhost ~__context);
              if access_type = InternalNetwork then begin
                let gi_network = Helpers.get_guest_installer_network ~__context in
                (* Next line could fail if someone has screwed with the network *)
                ip := List.assoc "ip_begin" (Db.Network.get_other_config ~__context ~self:gi_network);
                (* Note that xapi creates a proxy on this ip address that communicates directly with the master *)
                let vifs = Db.VM.get_VIFs ~__context ~self in
                (* Next line will fail if there's no VIF on the guest installer network *)
                let vif = List.find (fun vif -> Db.VIF.get_network ~__context ~self:vif = gi_network) vifs in
	        mac := Db.VIF.get_MAC ~__context ~self:vif;
              end else if access_type = FirstNetwork then begin
                ip := Helpers.get_main_ip_address ~__context
              end;
	    );
          let session_id_ref = Ref.string_of !session_id in
	  let vmref = Ref.string_of self in
	  Domain.grant_api_access ~xs domid !mac !ip port session_id_ref vmref
	with e -> error "Caught exception '%s': not granting guest access" (ExnHelper.string_of_exn e); ()

let general_domain_create_check ~__context ~vm ~snapshot =
    (* If guest will boot HVM check that this host has HVM capabilities *)
    let hvm = Helpers.is_hvm snapshot in
      if hvm then (
	  let caps = with_xc (fun xc -> Xc.version_capabilities xc) in
	  if not (String.has_substr caps "hvm") then (raise (Api_errors.Server_error (Api_errors.vm_hvm_required,[]))))

(* create an empty domain *)
let create ~__context ~xc ~xs ~self (snapshot: API.vM_t) () =
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
	let allowed_xsdata_prefix =
		try
			let prefixes = ref [] in
			Unixext.readfile_line (fun line -> prefixes := line :: !prefixes)
			                      Xapi_globs.allowed_xsdata_file;
			!prefixes
		with _ -> []
		in
	let xsdata =
		if allowed_xsdata_prefix = [] then 
			[]
		else (
			List.filter (fun (k,v) ->
				let found = ref false in
				List.iter (fun p ->
					if String.startswith p k then
						found := true
				) allowed_xsdata_prefix;
				!found
			) xsdata
		)
		in
	let rstr = Restrictions.get () in
	let platformdata =
		let p = Db.VM.get_platform ~__context ~self in
		if rstr.Restrictions.platform_filter then List.filter (fun (k, v) -> List.mem k filtered_platform_flags) p else p
	in

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

 	let hvm = Helpers.will_boot_hvm ~__context ~self in
	Xapi_xenops_errors.handle_xenops_error
	  (fun () ->
	     let domid = Domain.make ~xc ~xs ~hvm ~xsdata ~platformdata ~name:snapshot.API.vM_name_label uuid in
	     debug "Created domain with domid: %d" domid;

	     begin try let sspf = (List.assoc "suppress-spurious-page-faults" snapshot.API.vM_other_config) in
	     if sspf = "true" then Domain.suppress_spurious_page_faults ~xc domid;
             with _ -> () end;
	     
	     let other_config = Db.VM.get_other_config ~__context ~self in
	     if List.mem_assoc Xapi_globs.grant_api_access other_config then (
           let access_type = match List.assoc "grant_api_access" other_config with
             | "internal" -> InternalNetwork
             | _ -> FirstNetwork
           in
	       Db.VM.remove_from_other_config ~__context ~self ~key:Xapi_globs.grant_api_access;
	       grant_guest_api_access ~xs ~self domid access_type;
	     );

	     domid
	  )

(* Destroy a domain associated with this VM. Note the VM's power_state and domid might
   remain valid if the VM has a domain on another host (ie in the migrate case)
   NB to prevent resource leaks, log errors and continue on attempting to clean up as
   much as possible.
 *)
let destroy_domain ?(preserve_xs_vm=false) ?(clear_currently_attached=true) ?(detach_devices=true) ?(deactivate_devices=true) ~__context ~xc ~xs ~self domid =
  (* destroy the session *)
  Helpers.log_exn_continue (Printf.sprintf "Vmops.destroy_domain: Destroying domid %d guest session" domid)
    (fun () ->
       let (ip,port,session_id,vm_ref) = Domain.get_api_access ~xs domid in
       if vm_ref=Ref.string_of (self) then begin
	 debug "Destroying guest session"; 
	 Server_helpers.exec_with_new_task ~session_id:(Ref.of_string session_id) "guest" (fun __context -> Xapi_session.logout ~__context)
       end) ();

  Helpers.log_exn_continue (Printf.sprintf "Vmops.destroy_domain: Destroying xen domain domid %d" domid)
    (fun () -> Domain.destroy ~preserve_xs_vm ~xc ~xs domid) ();
  
  Helpers.log_exn_continue "Vmops.destroy_domain: clearing VBD currently_attached fields"
    (fun () ->
       (* Finish using any VDIs whose VBDs were possibly still attached. Note we may
	  attempt to double 'finish' with VDIs -- this must be idempotent *)
       let vbds = Db.VM.get_VBDs ~__context ~self in
	    
       (* clear currently_attached if clear_currently_attached=true.
	  detach VDIs if detach_devices is true
	  deactivate VDIs if deactivate_devices is true *)
       List.iter 
	 (fun vbd ->
	    (* Best effort destroy of each disk *)
	    Helpers.log_exn_continue (Printf.sprintf "Vmops.destroy_domain: clearing currently_attached field of VBD: %s" (Ref.string_of vbd))
	      (fun () ->
		 let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
		 let is_currently_attached = Db.VBD.get_currently_attached ~__context ~self:vbd in
		 (* if vbd is not empty and the vbd is marked as 'currently attached' then call detach and maybe
		    deactivate vdi *)
		 if not(Db.VBD.get_empty ~__context ~self:vbd) && is_currently_attached
		 then Helpers.log_exn_continue (Printf.sprintf "finishing with VDI %s" (Ref.string_of vdi))
		   (fun vdi ->
		      if deactivate_devices then Storage_access.VDI.deactivate ~__context ~self:vdi;
		      if detach_devices then Storage_access.VDI.detach ~__context ~self:vdi) vdi;
		 (* if we're releasing devices then go for it: *)
		 if clear_currently_attached then
		   Db.VBD.set_currently_attached ~__context ~self:vbd ~value:false;
	      ) ();
	    (* We unpause every VBD which allows any pending VBD.pause thread to unblock, acquire the VM lock in turn and check the state *)
	    Helpers.log_exn_continue (Printf.sprintf "Vmops.destroy_domain: pre-emptively unpausing VBD: %s" (Ref.string_of vbd))
	      (fun () ->
		 Xapi_vbd.clean_up_on_domain_destroy vbd (* effect is to unblock threads, not actually unpause *)
	      ) ();
	 ) vbds
    ) ();
  (* Remove any static lease we might have *)
  Helpers.log_exn_continue "Vmops.destroy_domain: attempting to remove DHCP lease"
    (fun () ->
       Xapi_udhcpd.maybe_remove_lease ~__context self) ()

(* Destroy a VM's domain and all runtime state (domid etc).
   If release_devices is true, unlock all VDIs and clear device_status_flags.
   In the restore case, set release_devices to false so we remember which 
   devices should be attached on resume. *)
let destroy ?(clear_currently_attached=true) ?(detach_devices=true) ?(deactivate_devices=true) ~__context ~xc ~xs ~self domid state =
	destroy_domain ~clear_currently_attached ~detach_devices ~deactivate_devices ~__context ~xc ~xs ~self domid;
	Db.VM.set_power_state ~__context ~self ~value:state;
	Db.VM.set_domid ~__context ~self ~value:(-1L);

	(* If the new power state is Running then we're killing the domain but /rebooting/
	   the VM-- keep the VM marked as resident_on to keep the memory allocated.
	   In any other case (Halted/Suspended) the VM's memory is nolonger needed so
	   we can blank the resident_on. 
	*)
	if state <> `Running 
	then 
	  (Db.VM.set_resident_on ~__context ~self ~value:Ref.null);
	
	destroy_consoles ~__context ~vM:self;
	(* to make debugging easier, set currently_attached to false for all
	   VBDs and VIFs*)
	if clear_currently_attached
	then clear_all_device_status_fields ~__context ~self


let pcidevs_of_vm ~__context ~vm =
	let other_config = Db.VM.get_other_config ~__context ~self:vm in
	let devs = try String.split ',' (List.assoc "pci" other_config) with Not_found -> [] in
	let devs = List.fold_left (fun acc dev ->
		try
			Scanf.sscanf dev "%d/%04x:%02x:%02x.%01x"
			             (fun id a b c d -> (id, (a, b, c, d))) :: acc
		with _ -> acc
	) [] devs in
	let ids = ref [] in
	List.iter (fun (id, _) ->
		if not (List.mem id !ids) then
			ids := id :: !ids
	) devs;
	List.map (fun id ->
		id, (List.map snd (List.filter (fun (x, _) -> x = id) devs))
	) !ids

(* Create the qemu-dm device emulator process. Has to be done after the
   disks and vifs have already been added.
   Returns the port number of the default VNC console. *)
let create_device_emulator ~__context ~xc ~xs ~self ?(restore=false) ?vnc_statefile domid vifs snapshot =
	let vcpus = Int64.to_int snapshot.API.vM_VCPUs_max
	and mem_max_kib = Int64.div snapshot.API.vM_memory_static_max 1024L in
	let other_config = snapshot.API.vM_other_config in

	(* XXX: we need some locking here & some better place to put the bridge name *)

	(* Sort the VIF devices by devid *)
	let vifs = List.stable_sort (fun a b -> compare a.Vm_config.devid b.Vm_config.devid) vifs in
	let nics = List.map (fun vif -> vif.Vm_config.mac, vif.Vm_config.bridge) vifs in

	(* Examine the boot method if the guest is HVM and do something about it *)
	if Helpers.is_hvm snapshot then begin
	        let policy = snapshot.API.vM_HVM_boot_policy in

		if policy <> Constants.hvm_boot_policy_bios_order then
			failwith (sprintf "Unknown HVM boot policy: %s" policy);

		let params = snapshot.API.vM_HVM_boot_params in
		let boot = if List.mem_assoc Constants.hvm_boot_params_order params then
			List.assoc Constants.hvm_boot_params_order params
		else (
			warn "No boot order found. Defaulting to 'cd'";
			"cd"
		) in

		let platform = snapshot.API.vM_platform in
		let map_to_bool feature =
			try bool_of_string (List.assoc feature platform)
			with _ -> false in
		let acpi = map_to_bool "acpi" in
		let serial = try List.assoc "hvm_serial" other_config with _ -> "pty" in
		let vnc_keymap = try List.assoc "keymap" platform with _ -> "en-us" in
		let pci_emulations =
			let rstr = Restrictions.get () in
			let s = try Some (List.assoc "mtc_pci_emulations" other_config) with _ -> None in
			match s with
			| None -> []
			| Some x ->
				if rstr.Restrictions.enable_mtc_pci then (
					try
						let l = String.split ',' x in
						List.map (String.strip String.isspace) l
					with _ -> []
				) else (
					L.warn "ignoring MTC pci emulation due to license restrictions";
					[]
				)
			in
		let dmpath = "/opt/xensource/libexec/qemu-dm-wrapper" in
		let dmstart = if restore then Device.Dm.restore else Device.Dm.start in
		let disp = Device.Dm.VNC (true, 0, vnc_keymap) in
		dmstart ~xs ~dmpath ~memory:mem_max_kib
		        ~boot ~serial ~vcpus
		        ~usb:["tablet"] ~nics:nics ~acpi
		        ~disp ~pci_emulations domid;
	end else begin
	        (* if we have a "disable_pv_vnc" entry in other_config we disable
		   VNC for PV *)
		let disable_pv_vnc = List.mem_assoc "disable_pv_vnc" other_config in
		if not disable_pv_vnc then Device.PV_Vnc.start ~xs domid ?statefile:vnc_statefile else 0
	end


(* get VBDs required to resume *)
let get_VBDs_required_on_resume ~__context ~vm =
  List.filter (fun self -> Db.VBD.get_currently_attached ~__context ~self)
    (Db.VM.get_VBDs ~__context ~self:vm)
(* get real VDIs required to resume -- i.e. the things we have to attach and maybe activate *)
let get_VDIs_required_on_resume ~__context ~vm =
  let needed_vbds = get_VBDs_required_on_resume ~__context ~vm in
  let needed_vdis =
    List.map (fun vbd -> Db.VBD.get_VDI ~__context ~self:vbd, Db.VBD.get_mode ~__context ~self:vbd)
      (List.filter (fun self -> not (Db.VBD.get_empty ~__context ~self)) needed_vbds) in
  needed_vdis

(* restore the devices, but not the domain; the vdis must all be attached/activated by the
   time this function is executed
*)
let _restore_devices ~__context ~xc ~xs ~self at_boot_time fd domid vifs =
	let hvm = Helpers.will_boot_hvm ~__context ~self in
	(* CA-25297: domarch is r/o and not currently stored in the LBR *)
	let protocol = Helpers.device_protocol_of_string (Db.VM.get_domarch ~__context ~self) in

	debug "For each disk which was attached to the VM when suspended, reattach SRs, VDIs and use VDIs";
	let string_of_vbd_list vbds = String.concat "; " 
	  (List.map (fun vbd -> string_of_vbd ~__context ~vbd) vbds) in

	(* We /must/ be able to re-attach all the VBDs the guest had when it suspended. *)
	let needed_vbds = get_VBDs_required_on_resume ~__context ~vm:self in
	debug "To restore this domain we need VBDs: [ %s ]" (string_of_vbd_list needed_vbds);
	
	(* If any VBDs cannot be attached, let the exn propagate *)
	List.iter (fun self -> create_vbd ~__context ~xs ~hvm ~protocol domid self) needed_vbds;
	
	
	create_cpus ~xs at_boot_time domid;
	create_vifs ~__context ~xs vifs;
	
	debug "setting current number of vcpus to %Ld (out of %Ld)" 
	  at_boot_time.API.vM_VCPUs_at_startup at_boot_time.API.vM_VCPUs_max;
	set_cpus_number ~__context ~xs ~self domid at_boot_time;
	set_cpus_qos ~__context ~xc ~self domid at_boot_time;
	set_cpus_mask ~__context ~xc ~self domid at_boot_time

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
	let mem_max_kib = Int64.div (at_boot_time.API.vM_memory_static_max) 1024L in
	let mem_target_kib = if ballooning_enabled 
	  then Int64.div (Db.VM.get_memory_target ~__context ~self) 1024L (* Nb, current val, not boot-time *)
	  else mem_max_kib in 

	if hvm then (
		let platform = at_boot_time.API.vM_platform in
		let map_to_bool feature =
			try bool_of_string (List.assoc feature platform)
			with _ -> false in
		let shadow_multiplier = at_boot_time.API.vM_HVM_shadow_multiplier in
		let pae = map_to_bool "pae" in
		let viridian = map_to_bool "viridian" in
		Domain.hvm_restore ~xc ~xs domid ~mem_max_kib ~mem_target_kib ~shadow_multiplier ~vcpus
		                   ~pae ~viridian ~timeoffset fd;
	) else (
		Domain.restore ~xc ~xs domid ~mem_max_kib ~mem_target_kib ~vcpus fd;
	);
	let vncport = create_device_emulator ~__context ~xc ~xs ~self ~restore:true ?vnc_statefile domid vifs at_boot_time in
	create_console ~__context ~vM:self ~vncport ()

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
let restore ~__context ~xc ~xs ~self domid =
  Xapi_xenops_errors.handle_xenops_error
    (fun () ->
       let suspend_vdi = Db.VM.get_suspend_VDI ~__context ~self in
       let snapshot = Helpers.get_boot_record ~__context ~self in
       Sm_fs_ops.with_fs_vdi __context suspend_vdi
	 (fun mount_point ->
	    let filename = find_suspend_image mount_point in
		let vnc_statefile = find_vnc_statefile mount_point in
	    debug "Using suspend image: %s" filename;
	    let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o400 in
	    finally 
	      (fun () ->
		 (* Attach and activate VDIs required for resume *)
		 let needed_vdis = get_VDIs_required_on_resume ~__context ~vm:self in
		 try
		   Storage_access.with_careful_attach_and_activate ~__context ~vdis:needed_vdis ~leave_activated:true
		     (fun () ->
			try
			  let vifs = Vm_config.vifs_of_vm ~__context ~vm:self domid in
			  _restore_devices ~__context ~xc ~xs ~self snapshot fd domid vifs;
			  _restore_domain ~__context ~xc ~xs ~self snapshot fd ?vnc_statefile domid vifs;

			with exn ->
			  begin
			    (* Destroy domain in inner-exn handler because otherwise the storage_access handler won't be able to detach/deactivate the
			       devices -- the backends will still be accessing them. *** If you ever read this in future and think that this inner exn handler makes
			       the outer one redundant (or vice-versa) then you're wrong.

			       The double domain destroy that you'll get (since the outer handler will also call destroy) is harmless since domids are
			       not re-used until we loop round the whole range.
			    *)
			    debug "Vmops.restore (inner-handler) caught: %s." (ExnHelper.string_of_exn exn);
			    debug "Vmops.restore (inner-handler): calling domain_destroy";
			    destroy ~__context ~xc ~xs ~self ~clear_currently_attached:false ~detach_devices:false ~deactivate_devices:false domid `Suspended;
			    raise exn (* re-raise *)
			  end
		     )
		 with exn ->
		   begin
		     debug "Vmops.restore caught: %s." (ExnHelper.string_of_exn exn);
		     debug "Vmops.restore: calling domain_destroy";
		     (* We do not detach/deactivate here because -- either devices were attached (in which case the storage_access
			handler has already detached them by this point); or devices were _never_ attached because exn was thrown
			before storage_access handler *)
		     destroy ~__context ~xc ~xs ~self ~clear_currently_attached:false ~detach_devices:false ~deactivate_devices:false domid `Suspended;
		     raise exn (* re-raise exn *)
		   end
	      )
	      (fun () -> Helpers.log_exn_continue "restore" (fun () -> Unix.close fd) ()));
       
       (* No exception must have happened: safe to destroy the VDI *)
       Helpers.call_api_functions ~__context
	   (fun rpc session_id ->
	      Client.VDI.destroy rpc session_id suspend_vdi);
       Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null
    )



let match_xal_and_shutdown xalreason reason =
	debug "Comparing XAL %s with Domain %s"
	      (Xal.string_of_died_reason xalreason)
	      (Domain.string_of_shutdown_reason reason);
	match xalreason, reason with
	| Xal.Crashed, _ -> false
	| Xal.Vanished, _ -> false
	| Xal.Halted, (Domain.Halt | Domain.PowerOff) -> true
	| Xal.Rebooted, Domain.Reboot -> true
	| Xal.Suspended, Domain.Suspend -> true
	| Xal.Shutdown i, Domain.Unknown i2 -> i = i2
	| _, _ -> false

(** Thrown if clean_shutdown_with_reason exits for the wrong reason: eg the domain
    crashed or rebooted *)
exception Domain_shutdown_for_wrong_reason of Xal.died_reason

(** Tells a VM to shutdown with a specific reason (reboot/halt/poweroff). *)
let clean_shutdown_with_reason ?(at = fun _ -> ()) ~xal ~__context ~self domid reason =
  (* Set the task allowed_operations to include cancel *)
  TaskHelper.set_cancellable ~__context;

  at 0.25;
  (* Windows PV drivers will respond within 10s according to ssmith and
     improving this is likely to happen in a Rio timeframe (CA-3964). It's
     still possible (although unlikely) for us to timeout just before the
     drivers activate but the worst we'll suffer is a shutdown failure
     followed by a spontaneous shutdown (which can happen anyway). Having
     this check in here allows us to bail out quickly in the common case
     of the PV drivers being missing. *)
  with_xs (fun xs ->
	     let xc = Xal.xc_of_ctx xal in
	     if not (Domain.shutdown_ack ~timeout:60. ~xc ~xs domid reason) then
	       raise (Api_errors.Server_error (Api_errors.vm_failed_shutdown_ack, []))
	  );
  at 0.50;
  let total_timeout = 20. *. 60. in (* 20 minutes *)
  (* Block for 5s at a time, in between check to see whether we've been cancelled
     and update our progress if not *)
  let start = Unix.gettimeofday () in
  let finished = ref false in
  while (Unix.gettimeofday () -. start < total_timeout) && not(!finished) do
    try
      let r = Xal.wait_release xal ~timeout:5. domid in
      if not (match_xal_and_shutdown r reason) then begin
	let errmsg = Printf.sprintf 
	  "Domain died with reason: %s when it should have been %s" 
	  (Xal.string_of_died_reason r) (Domain.string_of_shutdown_reason reason) in
	debug "%s" errmsg;
	raise (Domain_shutdown_for_wrong_reason r)
      end;
      finished := true;
    with Xal.Timeout -> 
      if TaskHelper.is_cancelling ~__context
      then raise (Api_errors.Server_error(Api_errors.task_cancelled, [ Ref.string_of (Context.get_task_id __context) ]));
      (* Update progress and repeat *)
      let progress = min ((Unix.gettimeofday () -. start) /. total_timeout) 1. in
      at (0.50 +. 0.25 *. progress)
  done;
  if not(!finished)
  then raise (Api_errors.Server_error(Api_errors.vm_shutdown_timeout, [ Ref.string_of self; string_of_float total_timeout ]));
  at 1.0

(* !!! FIX ME  - This allows a 10% overhead on static_max for size of suspend image !!! *)
let get_suspend_space __context vm =
  let at_boot_time = Helpers.get_boot_record ~__context ~self:vm in
  let mem_max = at_boot_time.API.vM_memory_static_max in
  Int64.add (Int64.of_float ((Int64.to_float mem_max) *. 1.2)) 104857600L

exception Domain_architecture_not_supported_in_suspend

let suspend ~live ~progress_cb ~__context ~xc ~xs ~vm =
  Xapi_xenops_errors.handle_xenops_error
    (fun () ->
       let uuid = Db.VM.get_uuid ~__context ~self:vm in
       let hvm = Helpers.has_booted_hvm ~__context ~self:vm in
       let suspend_SR = Helpers.choose_suspend_sr ~__context ~vm in
       let required_space = get_suspend_space __context vm in
       let params = if live then [Domain.Live] else [] in
       Sm_fs_ops.with_new_fs_vdi __context
	 ~name_label:"Suspend image" ~name_description:"Suspend image"
	 ~sR:suspend_SR ~_type:`suspend ~required_space
	 ~sm_config:[Xapi_globs._sm_vm_hint, uuid]
	 (fun vdi_ref mount_point ->

	    let filename = sprintf "%s/suspend-image" mount_point in
		let vnc_statefile = sprintf "%s/vncterm.statefile" mount_point in

	    debug "suspend: phase 1/2: opening suspend image file (%s)" filename;
	    (* NB if the suspend file already exists it will be overwritten *)
	    let fd = Unix.openfile filename [ Unix.O_WRONLY; Unix.O_CREAT ] 0o600 in
	    finally
	      (fun () ->
		 let domid = Helpers.domid_of_vm ~__context ~self:vm in
		 debug "suspend: phase 2/2: suspending to disk";

		 Device.PV_Vnc.save ~xs domid;
		 begin match Device.PV_Vnc.get_statefile ~xs domid with
		 | None -> debug "suspend: no vncterm.statefile found"
		 | Some f -> 
			   debug "suspend: a vncterm statefile (%s) has been found, saving into %s" f vnc_statefile;
			   let fd_src = Unix.openfile f [Unix.O_RDONLY] 0o600 in
			   let fd_dst = Unix.openfile vnc_statefile [Unix.O_WRONLY; Unix.O_CREAT] 0o600 in
			   finally
				   (fun () -> let (_:int64) = Unixext.copy_file fd_src fd_dst in ())
				   (fun () ->
						Unix.close fd_src;
						Unix.close fd_dst;
						debug "suspend: deleting '%s'" f;
						Unix.unlink f)
		 end;

		 with_xal
		   (fun xal ->
		      Domain.suspend ~xc ~xs ~hvm domid fd params ~progress_callback:progress_cb
			(fun () -> clean_shutdown_with_reason ~xal ~__context ~self:vm domid Domain.Suspend));

		 (* If the suspend succeeds, set the suspend_VDI *)
		 Db.VM.set_suspend_VDI ~__context ~self:vm ~value:vdi_ref;
	      )
	      (fun () -> Unix.close fd);
	    debug "suspend: complete")
    )

let resume ~__context ~xc ~xs ~vm =
	let domid = Helpers.domid_of_vm ~__context ~self:vm in
	let hvm = Helpers.has_booted_hvm ~__context ~self:vm in
	Xapi_xenops_errors.handle_xenops_error (fun () ->
		(* TTT: check if the domain is really cooperative *)
		Domain.resume ~xs ~xc ~hvm ~cooperative:true domid;
		Domain.unpause ~xc domid) 

(** Starts up a VM, leaving it in the paused state *)
let start_paused ?(progress_cb = fun _ -> ()) ~__context ~vm ~snapshot =
  check_vm_parameters ~__context ~self:vm ~snapshot;
  (* Take the subset of locked VBDs *)
  let other_config = Db.VM.get_other_config ~__context ~self:vm in
  let vbds = Vbdops.vbds_to_attach ~__context ~vm in
  with_xc_and_xs (fun xc xs ->		    
		    check_enough_memory ~__context ~xc ~snapshot ~restore:false;

		    let domid = create ~__context ~xc ~xs ~self:vm snapshot () in
		    begin try
		      Db.VM.set_domid ~__context ~self:vm ~value:(Int64.of_int domid);

		      progress_cb 0.25;

		      clear_all_device_status_fields ~__context ~self:vm;
		      progress_cb 0.30;

		      debug "Verifying VDI records exist";
		      List.iter (fun vbd -> check_vdi_exists ~__context ~vbd) vbds;

		      let width = try Some (int_of_string (List.assoc "machine-address-size" other_config))
		                  with _ -> None in
		      Domain.set_machine_address_size ~xc domid width;

		      (* This might involve a block-attach for bootloader: do this early before
			 locking the VDIs *)
		      debug "creating kernel";
		      create_kernel ~__context ~xc ~xs ~self:vm domid snapshot;
		      progress_cb 0.35;

		      let hvm = Helpers.is_hvm snapshot in			
		      (* Don't attempt to attach empty VBDs to PV guests: they can't handle them *)
		      let vbds = 
			if hvm then vbds
			else List.filter (fun self -> not(Db.VBD.get_empty ~__context ~self)) vbds in
		      (* If any VDIs cannot be attached, let the exn propagate *)
		      let vdis =
			List.map (fun vbd -> Db.VBD.get_VDI ~__context ~self:vbd, Db.VBD.get_mode ~__context ~self:vbd)
			  (List.filter (fun self -> not (Db.VBD.get_empty ~__context ~self)) vbds) in
		      
		      (* Attach and activate reqd vdis: if exn occurs then we do best effort cleanup
			 -- that is detach and deactivate -- and then propogate original exception.
			 We need an exn handler around the whole thing as with_careful_attach_and_activate may throw an exn
			 whilst trying to attach/activate the
			 vdis; in this case the inner-fn won't have been able to install its exn handler, so we have to catch
			 the exn externally and destroy the domain.
		      *)
		      Storage_access.with_careful_attach_and_activate ~__context ~vdis ~leave_activated:true
			(fun () ->
			   try
			     debug "creating VCPU devices and attaching to domain";
			     create_cpus ~xs snapshot domid;
			     progress_cb 0.40;
			     debug "creating VBD devices and attaching to domain";
			     (* CA-25297: domarch is r/o and not currently stored in the LBR *)
			     let protocol = Helpers.device_protocol_of_string (Db.VM.get_domarch ~__context ~self:vm) in
			     (* If any VBDs cannot be attached, let the exn propagate *)
			     List.iter (fun self -> create_vbd ~__context ~xs ~hvm ~protocol domid self) vbds;
			     progress_cb 0.60;
			     debug "creating VIF devices and attaching to domain";
			     let vifs = Vm_config.vifs_of_vm ~__context ~vm domid in
			     create_vifs ~__context ~xs vifs;
			     progress_cb 0.70;
			     debug "attaching PCI devices to domain";
			     let pcis = pcidevs_of_vm ~__context ~vm in
			     attach_pcis ~__context ~xc ~xs ~hvm domid pcis;
			     progress_cb 0.75;
			     debug "adjusting CPU number against startup-number";
			     set_cpus_number ~__context ~xs ~self:vm domid snapshot;
			     progress_cb 0.80;
			     debug "settings CPUs qos";
			     set_cpus_qos ~__context ~xc ~self:vm domid snapshot;
			     progress_cb 0.90;
			     debug "settings CPUs mask";
			     set_cpus_mask ~__context ~xc ~self:vm domid snapshot;
			     progress_cb 0.95;
			     debug "creating device emulator";
			     let vncport = create_device_emulator ~__context ~xc ~xs ~self:vm domid vifs snapshot in
			     create_console ~__context ~vM:vm ~vncport ();
			     update_vm_last_booted ~__context ~self:vm
			   with exn ->
			     (* [Comment copied from similar pattern in "restore" fn above]:

				Destroy domain in inner-exn handler because otherwise the storage_access handler won't be able to detach/deactivate the
				devices -- the backends will still be accessing them. *** If you ever read this in future and think that this inner exn handler makes
				the outer one redundant (or vice-versa) then you're wrong.
				
				The double domain destroy that you'll get (since the outer handler will also call destroy) is harmless since domids are
				not re-used until we loop round the whole range.
			     *)
			     begin
			       debug "Vmops.start_paused (inner-handler) caught: %s." (ExnHelper.string_of_exn exn);
			       debug "Vmops.start_paused (inner-handler): calling domain_destroy";
			       destroy ~__context ~xc ~xs ~self:vm ~detach_devices:false ~deactivate_devices:false domid `Halted;
			       raise exn (* re-raise *)
			     end
			)
		    with exn ->
		      debug "Vmops.start_paused caught: %s."
			(ExnHelper.string_of_exn exn);
		      debug "Vmops.start_paused: calling domain_destroy";
		      (* We do not detach/deactivate here because -- either devices were attached (in which case the storage_access
			 handler has already detached them by this point); or devices were _never_ attached because exn was thrown
			 before storage_access handler *)
		      destroy ~__context ~xc ~xs ~self:vm ~detach_devices:false ~deactivate_devices:false domid `Halted;
		      (* Return a nice exception if we can *)
		      raise (Xapi_xenops_errors.to_api_error exn)
		    end;
		    Helpers.set_boot_record ~__context ~self:vm snapshot
		 )
