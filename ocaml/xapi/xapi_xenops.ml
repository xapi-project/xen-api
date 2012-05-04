(*
 * Copyright (C) Citrix Systems Inc.
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

module D=Debug.Debugger(struct let name="xenops" end)
open D

open Stringext
open Listext
open Threadext
open Pervasiveext
open Fun
module XenAPI = Client.Client
open Xenops_interface

let xenapi_of_xenops_power_state = function
	| Some Running -> `Running
	| Some Halted -> `Halted
	| Some Suspended -> `Suspended
	| Some Paused -> `Paused
	| None -> `Halted

(* This is only used to block the 'present multiple physical cores as one big hyperthreaded core' feature *)
let filtered_platform_flags = ["acpi"; "apic"; "nx"; "pae"; "viridian";
                               "acpi_s3";"acpi_s4"; "mmio_size_mib"; "revision"; "device_id"]

let xenops_vdi_locator_of_strings sr_uuid vdi_location =
	Printf.sprintf "%s/%s" sr_uuid vdi_location

let xenops_vdi_locator ~__context ~self =
	let sr = Db.VDI.get_SR ~__context ~self in
	let sr_uuid = Db.SR.get_uuid ~__context ~self:sr in
	let vdi_location = Db.VDI.get_location ~__context ~self in
	xenops_vdi_locator_of_strings sr_uuid vdi_location

let disk_of_vdi ~__context ~self =
	try Some (VDI (xenops_vdi_locator ~__context ~self)) with _ -> None

let vdi_of_disk ~__context x = match String.split ~limit:2 '/' x with
		| [ sr_uuid; location ] ->
			let open Db_filter_types in
			let sr = Db.SR.get_by_uuid ~__context ~uuid:sr_uuid in
			begin match Db.VDI.get_records_where ~__context ~expr:(And((Eq (Field "location", Literal location)),Eq (Field "SR", Literal (Ref.string_of sr)))) with
				| x :: _ -> Some x
				| _ ->
					error "Failed to find VDI: %s" x;
					None
			end
		| _ ->
			error "Failed to parse VDI name: %s" x;
			None

let backend_of_network net =
	Network.Local net.API.network_bridge (* PR-1255 *)

let find f map default feature =
	try f (List.assoc feature map)
	with _ -> default
let string = find (fun x -> x)
let int = find int_of_string
let bool = find bool_of_string

let rtc_timeoffset_of_vm ~__context (vm, vm_t) vbds =
	let timeoffset = string vm_t.API.vM_platform "0" "timeoffset" in
	(* If any VDI has on_boot = reset AND has a VDI.other_config:timeoffset
	   then we override the platform/timeoffset. This is needed because windows
	   stores the local time in timeoffset (the BIOS clock) but records whether
	   it has adjusted it for daylight savings in the system disk. If we reset
	   the system disk to an earlier snapshot then the BIOS clock needs to be
	   reset too. *)
	let non_empty_vbds = List.filter (fun vbd -> not vbd.API.vBD_empty) vbds in
	let vdis = List.map (fun vbd -> vbd.API.vBD_VDI) non_empty_vbds in
	let vdis_with_timeoffset_to_be_reset_on_boot =
		vdis
		|> List.map (fun self -> (self, Db.VDI.get_record ~__context ~self))
		|> List.filter (fun (_, record) -> record.API.vDI_on_boot = `reset)
		|> List.filter_map (fun (reference, record) ->
			Opt.of_exception (fun () ->
				reference,
				List.assoc "timeoffset"
					record.API.vDI_other_config)) in
	match vdis_with_timeoffset_to_be_reset_on_boot with
		| [] ->
			timeoffset
		| [(reference, timeoffset)] ->
			timeoffset
		| reference_timeoffset_pairs ->
			raise (Api_errors.Server_error (
				(Api_errors.vm_attached_to_more_than_one_vdi_with_timeoffset_marked_as_reset_on_boot),
				(Ref.string_of vm) ::
					(reference_timeoffset_pairs
					|> List.map fst
					|> List.map Ref.string_of)))

(* /boot/ contains potentially sensitive files like xen-initrd, so we will only*)
(* allow directly booting guests from the subfolder /boot/guest/ *) 
let allowed_dom0_directory_for_boot_files = "/boot/guest/"
let is_boot_file_whitelisted filename =
	let safe_str str = not (String.has_substr str "..") in
	(* make sure the script prefix is the allowed dom0 directory *)
	(String.startswith allowed_dom0_directory_for_boot_files filename)
		(* avoid ..-style attacks and other weird things *)
	&&(safe_str filename)

let builder_of_vm ~__context ~vm timeoffset pci_passthrough =
	let open Vm in

    let pci_emulations =
        let s = try Some (List.assoc "mtc_pci_emulations" vm.API.vM_other_config) with _ -> None in
        match s with
            | None -> []
            | Some x ->
                try
                    let l = String.split ',' x in
                    List.map (String.strip String.isspace) l
                with _ -> []
    in

	match Helpers.boot_method_of_vm ~__context ~vm with
		| Helpers.HVM { Helpers.timeoffset = t } -> HVM {
			hap = true;
			shadow_multiplier = vm.API.vM_HVM_shadow_multiplier;
			timeoffset = timeoffset;
			video_mib = int vm.API.vM_platform 4 "videoram";
			video = begin match string vm.API.vM_platform "cirrus" "vga" with
				| "std" -> Standard_VGA
				| "cirrus" -> Cirrus
				| x ->
					error "Unknown platform/vga option: %s (expected 'std' or 'cirrus')" x;
					Cirrus
			end;
			acpi = bool vm.API.vM_platform true "acpi";
			serial = Some (string vm.API.vM_platform "pty" "hvm_serial");
			keymap = Some (string vm.API.vM_platform "en-us" "keymap");
			vnc_ip = Some "0.0.0.0" (*None PR-1255*);
			pci_emulations = pci_emulations;
			pci_passthrough = pci_passthrough;
			boot_order = string vm.API.vM_HVM_boot_params "cd" "order";
			qemu_disk_cmdline = bool vm.API.vM_platform false "qemu_disk_cmdline";
			qemu_stubdom = bool vm.API.vM_platform false "qemu_stubdom";
		}
		| Helpers.DirectPV { Helpers.kernel = k; kernel_args = ka; ramdisk = initrd } ->
			let k = if is_boot_file_whitelisted k then k else begin
				debug "kernel %s is not in the whitelist: ignoring" k;
				""
			end in
			let initrd = Opt.map (fun x ->
				if is_boot_file_whitelisted x then x else begin
					debug "initrd %s is not in the whitelist: ignoring" k;
					""
				end
			) initrd in
			PV {
				boot = Direct { kernel = k; cmdline = ka; ramdisk = initrd };
				framebuffer = false;
				vncterm = true;
				vncterm_ip = Some "0.0.0.0" (*None PR-1255*);
			}
		| Helpers.IndirectPV { Helpers.bootloader = b; extra_args = e; legacy_args = l; pv_bootloader_args = p; vdis = vdis } ->
			PV {
				boot = Indirect { bootloader = b; extra_args = e; legacy_args = l; bootloader_args = p; devices = List.filter_map (fun x -> disk_of_vdi ~__context ~self:x) vdis };
				framebuffer = false;
				vncterm = true;
				vncterm_ip = Some "0.0.0.0" (*None PR-1255*);
			}

module MD = struct
	(** Convert between xapi DB records and xenopsd records *)

	let of_vbd ~__context ~vm ~vbd =
		let hvm = vm.API.vM_HVM_boot_policy <> "" in
		let device_number = Device_number.of_string hvm vbd.API.vBD_userdevice in
		let open Vbd in
		let ty = vbd.API.vBD_qos_algorithm_type in
		let params = vbd.API.vBD_qos_algorithm_params in

		let qos_class params =
			if List.mem_assoc "class" params then
				match List.assoc "class" params with
					| "highest" -> Highest
					| "high"    -> High
					| "normal"  -> Normal
					| "low"     -> Low
					| "lowest"  -> Lowest
					| s         ->
						try Other (int_of_string s) 
						with _ ->
							warn "Unknown VBD QoS scheduler class (try 'high' 'low' 'normal')";
							Normal
			else
				Normal in
		let qos_scheduler params =
			try
				match List.assoc "sched" params with
				| "rt" | "real-time" -> RealTime (qos_class params)
				| "idle"             -> Idle
				| "best-effort"      -> BestEffort (qos_class params)
				| _                  ->
					warn "Unknown VBD QoS scheduler (try 'real-time' 'idle' 'best-effort')";
					BestEffort (qos_class params)
			with Not_found ->
				BestEffort (qos_class params) in
		let qos = function
			| "ionice" -> Some (Ionice (qos_scheduler params))
			| "" -> None
			| x ->
				warn "Unknown VBD QoS type: %s (try 'ionice')" x;
				None in

		{
			id = (vm.API.vM_uuid, Device_number.to_linux_device device_number);
			position = Some device_number;
			mode = if vbd.API.vBD_mode = `RO then ReadOnly else ReadWrite;
			backend = disk_of_vdi ~__context ~self:vbd.API.vBD_VDI;
			ty = if vbd.API.vBD_type = `Disk then Disk else CDROM;
			unpluggable = true;
			extra_backend_keys = [];
			extra_private_keys = [];
			qos = qos ty;
		}

	let of_vif ~__context ~vm ~vif =
		let mtu =
			try
				if List.mem_assoc "mtu" vif.API.vIF_other_config
				then List.assoc "mtu" vif.API.vIF_other_config |> int_of_string
				else 1500
			with _ ->
				error "Failed to parse VIF.other_config:mtu; defaulting to 1500";
				1500 in
		let net = Db.Network.get_record ~__context ~self:vif.API.vIF_network in
		let locking_mode = match vif.API.vIF_locking_mode, net.API.network_default_locking_mode with
			| `network_default, `disabled -> Vif.Disabled
			| `network_default, `unlocked -> Vif.Unlocked
			| `locked, _ -> Vif.Locked { Vif.ipv4 = vif.API.vIF_ipv4_allowed; ipv6 = vif.API.vIF_ipv6_allowed }
			| `unlocked, _ -> Vif.Unlocked
			| `disabled, _ -> Vif.Disabled in
		let open Vif in {
			id = (vm.API.vM_uuid, vif.API.vIF_device);
			position = int_of_string vif.API.vIF_device;
			mac = vif.API.vIF_MAC;
			carrier = true;
			mtu = mtu;
			rate = None;
			backend = backend_of_network net;
			other_config = vif.API.vIF_other_config;
			locking_mode = locking_mode;
			extra_private_keys = [
                "vif-uuid", vif.API.vIF_uuid;
				"network-uuid", net.API.network_uuid
			]
		}

	let pcis_of_vm ~__context (vmref, vm) =
		let vgpu_pcidevs = Vgpuops.list_vgpus ~__context ~vm:vmref in
		let devs = List.flatten (List.map (fun (_, dev) -> dev) (Pciops.sort_pcidevs vgpu_pcidevs)) in

		(* The 'unmanaged' PCI devices are in the other_config key: *)
		let other_pcidevs =
			try
				Pciops.other_pcidevs_of_vm ~__context vm.API.vM_other_config
			with Api_errors.Server_error(code, _) when code = Api_errors.rbac_permission_denied ->
				error "No PCI devices will be passed-through: RBAC_PERMISSION_DENIED";
				[] in
		let unmanaged = List.flatten (List.map (fun (_, dev) -> dev) (Pciops.sort_pcidevs other_pcidevs)) in

		let devs = devs @ unmanaged in

		let open Pci in
		List.map
			(fun (idx, (domain, bus, dev, fn)) -> {
				id = (vm.API.vM_uuid, Printf.sprintf "%04x:%02x:%02x.%01x" domain bus dev fn);
				position = idx;
				domain = domain;
				bus = bus;
				dev = dev;
				fn = fn;
				msitranslate = None;
				power_mgmt = None;
			})
			(List.combine (Range.to_list (Range.make 0 (List.length devs))) devs)

	let of_vm ~__context (vmref, vm) vbds pci_passthrough =
		let on_crash_behaviour = function
			| `preserve -> []
			| `coredump_and_restart -> [ Vm.Coredump; Vm.Start ]
			| `coredump_and_destroy -> [ Vm.Coredump; Vm.Shutdown ]
			| `restart
			| `rename_restart -> [ Vm.Start ]
			| `destroy -> [ Vm.Shutdown ] in
		let on_normal_exit_behaviour = function
			| `restart -> [ Vm.Start ]
			| `destroy -> [ Vm.Shutdown ] in
		let open Vm in
		let scheduler_params =
			(* vcpu <-> pcpu affinity settings are stored here.
			   Format is either:
			   1,2,3         ::  all vCPUs receive this mask
			   1,2,3; 4,5,6  ::  vCPU n receives mask n. Unlisted vCPUs 
			                     receive first mask *)
			let affinity =
				try
					List.map
						(fun x -> List.map int_of_string (String.split ',' x))
						(String.split ';' (List.assoc "mask" vm.API.vM_VCPUs_params))
				with _ -> [] in
			let priority =
				try
					let weight = List.assoc "weight" vm.API.vM_VCPUs_params in
					let cap = List.assoc "cap" vm.API.vM_VCPUs_params in
					Some (int_of_string weight, int_of_string cap)
				with _ -> None in
			{ priority = priority; affinity = affinity } in

		let platformdata =
			let p = vm.API.vM_platform in
			if not (Pool_features.is_enabled ~__context Features.No_platform_filter) then
				List.filter (fun (k, v) -> List.mem k filtered_platform_flags) p
			else p in
		let timeoffset = rtc_timeoffset_of_vm ~__context (vmref, vm) vbds in
		(* Replace the timeoffset in the platform data too, to avoid confusion *)
		let platformdata =
			("timeoffset", timeoffset) ::
				(List.filter (fun (key, _) -> key <> "timeoffset") platformdata) in

		let pci_msitranslate = true in (* default setting *)
		(* CA-55754: allow VM.other_config:msitranslate to override the bus-wide setting *)
		let pci_msitranslate =
			if List.mem_assoc "msitranslate" vm.API.vM_other_config
			then List.assoc "msitranslate" vm.API.vM_other_config = "1"
			else pci_msitranslate in
		(* CA-55754: temporarily disable msitranslate when GPU is passed through. *)
		let pci_msitranslate =
			if vm.API.vM_VGPUs <> [] then false else pci_msitranslate in
		{
			id = vm.API.vM_uuid;
			name = vm.API.vM_name_label;
			ssidref = 0l;
			xsdata = vm.API.vM_xenstore_data;
			platformdata = platformdata;
			bios_strings = vm.API.vM_bios_strings;
			ty = builder_of_vm ~__context ~vm timeoffset pci_passthrough;
			suppress_spurious_page_faults = (try List.assoc "suppress-spurious-page-faults" vm.API.vM_other_config = "true" with _ -> false);
			machine_address_size = (try Some(int_of_string (List.assoc "machine-address-size" vm.API.vM_other_config)) with _ -> None);
			memory_static_max = vm.API.vM_memory_static_max;
			memory_dynamic_max = vm.API.vM_memory_dynamic_max;
			memory_dynamic_min = vm.API.vM_memory_dynamic_min;
			vcpu_max = Int64.to_int vm.API.vM_VCPUs_max;
			vcpus = Int64.to_int vm.API.vM_VCPUs_at_startup;
			scheduler_params = scheduler_params;
			on_crash = on_crash_behaviour vm.API.vM_actions_after_crash;
			on_shutdown = on_normal_exit_behaviour vm.API.vM_actions_after_shutdown;
			on_reboot = on_normal_exit_behaviour vm.API.vM_actions_after_reboot;
			pci_msitranslate = pci_msitranslate;
			pci_power_mgmt = false;
		}		


end

open Xenops_interface
open Xenops_client
open Fun

(* If a VM was suspended pre-xenopsd it won't have a last_booted_record of the format understood by xenopsd. *)
(* If we can parse the last_booted_record according to the old syntax, update it before attempting to resume. *)
let generate_xenops_state ~__context ~self ~vm ~vbds ~pcis =
	try
		let vm_to_resume = {
			(Helpers.parse_boot_record vm.API.vM_last_booted_record) with
			API.vM_VBDs = vm.API.vM_VBDs
		} in
		debug "Successfully parsed old last_booted_record format - translating to new format so that xenopsd can resume the VM.";
		let md = MD.of_vm ~__context (self, vm_to_resume) vbds (pcis <> []) in
		let dbg = Context.string_of_task __context in
		Client.VM.generate_state_string dbg md
	with Xml.Error _ ->
		debug "last_booted_record is not of the old format, so we should be able to resume the VM.";
		vm.API.vM_last_booted_record

(* Create an instance of Metadata.t, suitable for uploading to the xenops service *)
let create_metadata ~__context ~upgrade ~self =
	let vm = Db.VM.get_record ~__context ~self in
	let vbds = List.map (fun self -> Db.VBD.get_record ~__context ~self) vm.API.vM_VBDs in
	let vifs = List.map (fun self -> Db.VIF.get_record ~__context ~self) vm.API.vM_VIFs in
	let pcis = MD.pcis_of_vm ~__context (self, vm) in
	let domains =
		(* For suspended VMs, we may need to translate the last_booted_record from the old format. *)
		if vm.API.vM_power_state = `Suspended || upgrade
		then Some (generate_xenops_state ~__context ~self ~vm ~vbds ~pcis)
		else None in
	let open Metadata in {
		vm = MD.of_vm ~__context (self, vm) vbds (pcis <> []);
		vbds = List.map (fun vbd -> MD.of_vbd ~__context ~vm ~vbd) vbds;
		vifs = List.map (fun vif -> MD.of_vif ~__context ~vm ~vif) vifs;
		pcis = pcis;
		domains = domains
	}

let id_of_vm ~__context ~self = Db.VM.get_uuid ~__context ~self
let vm_of_id ~__context uuid = Db.VM.get_by_uuid ~__context ~uuid

let vm_exists_in_xenopsd dbg id =
	try Client.VM.stat dbg id |> ignore; true with Does_not_exist(_, _) -> false

let string_of_exn = function
	| Api_errors.Server_error(code, params) -> Printf.sprintf "%s [ %s ]" code (String.concat "; " params)
	| e -> Printexc.to_string e

(* Serialise updates to the xenopsd metadata *)
let metadata_m = Mutex.create ()

(* Remember the last events received from xapi so we can suppress
   non-updates (the xapi objects are bigger and will change more often
   than the xenopsd ones) *)
let metadata_cache = Hashtbl.create 10

module Xenops_cache = struct
	(** Remember the last events received from xenopsd so we can compute
		field-level differences. This allows us to minimise the number of
		database writes we issue upwards. *)

	type t = {
		vm: Vm.state option;
		vbds: (Vbd.id * Vbd.state) list;
		vifs: (Vif.id * Vif.state) list;
		pcis: (Pci.id * Pci.state) list;
	}
	let empty = {
		vm = None;
		vbds = [];
		vifs = [];
		pcis = [];
	}
			
	let cache = Hashtbl.create 10 (* indexed by Vm.id *)

	let register_nolock id =
		debug "xenopsd events: creating empty cache for %s" id;
		Hashtbl.replace cache id empty

	let unregister_nolock id =
		debug "xenopsd events: deleting cache for %s" id;
		Hashtbl.remove cache id

	let find id : t option =
		Mutex.execute metadata_m
			(fun () ->
				if Hashtbl.mem cache id
				then Some (Hashtbl.find cache id)
				else None
			)

	let find_vm id : Vm.state option =
		match find id with
			| Some { vm = Some vm } -> Some vm
			| _ -> None

	let find_vbd id : Vbd.state option =
		match find (fst id) with
			| Some { vbds = vbds } ->
				if List.mem_assoc id vbds
				then Some (List.assoc id vbds)
				else None
			| _ -> None

	let find_vif id : Vif.state option =
		match find (fst id) with
			| Some { vifs = vifs } ->
				if List.mem_assoc id vifs
				then Some (List.assoc id vifs)
				else None
			| _ -> None

	let find_pci id : Pci.state option =
		match find (fst id) with
			| Some { pcis = pcis } ->
				if List.mem_assoc id pcis
				then Some (List.assoc id pcis)
				else None
			| _ -> None

	let update id t =
		Mutex.execute metadata_m
			(fun () ->
				if Hashtbl.mem cache id
				then Hashtbl.replace cache id t
				else debug "xenopsd event: Not updating cache for unregistered VM %s" id
			)

	let update_vbd id info =
		let existing = Opt.default empty (find (fst id)) in
		let vbds' = List.filter (fun (vbd_id, _) -> vbd_id <> id) existing.vbds in
		update (fst id) { existing with vbds = Opt.default vbds' (Opt.map (fun info -> (id, info) :: vbds') info) }

	let update_vif id info =
		let existing = Opt.default empty (find (fst id)) in
		let vifs' = List.filter (fun (vif_id, _) -> vif_id <> id) existing.vifs in
		update (fst id) { existing with vifs = Opt.default vifs' (Opt.map (fun info -> (id, info) :: vifs') info) }

	let update_pci id info =
		let existing = Opt.default empty (find (fst id)) in
		let pcis' = List.filter (fun (pci_id, _) -> pci_id <> id) existing.pcis in
		update (fst id) { existing with pcis = Opt.default pcis' (Opt.map (fun info -> (id, info) :: pcis') info) }

	let update_vm id info =
		let existing = Opt.default empty (find id) in
		update id { existing with vm = info }

	let unregister_nolock id =
		Hashtbl.remove cache id
end

(* Set up event caches for a VM *)
let add_caches id =
	Mutex.execute metadata_m
		(fun () ->
			Hashtbl.replace metadata_cache id None;
			Xenops_cache.register_nolock id;
			debug "VM %s: registering with cache (xenops cache size = %d)" id (Hashtbl.length Xenops_cache.cache)
		)

(* Remove event caches for a VM *)
let remove_caches id =
	Mutex.execute metadata_m
		(fun () ->
			Hashtbl.remove metadata_cache id;
			Xenops_cache.unregister_nolock id;
			debug "VM %s: unregistering with cache (xenops cache size = %d; xapi cache size = %d)" id (Hashtbl.length Xenops_cache.cache) (Hashtbl.length metadata_cache);
		)

let push_metadata_to_xenopsd ~__context ~upgrade ~self =
	Mutex.execute metadata_m (fun () ->
		let txt = create_metadata ~__context ~upgrade ~self |> Metadata.rpc_of_t |> Jsonrpc.to_string in
		info "xenops: VM.import_metadata %s" txt;
		let dbg = Context.string_of_task __context in
		Client.VM.import_metadata dbg txt;
	)

(* Unregisters a VM with xenopsd, and cleans up metadata and caches *)
let pull_metadata_from_xenopsd ~__context id =
	Mutex.execute metadata_m
		(fun () ->
			info "xenops: VM.export_metadata %s" id;
			let dbg = Context.string_of_task __context in
			let md = Client.VM.export_metadata dbg id |> Jsonrpc.of_string |> Metadata.t_of_rpc in
			info "xenops: VM.remove %s" id;
			Client.VM.remove dbg id;
			md)

let delete_metadata_from_xenopsd ~__context id =
	Mutex.execute metadata_m
		(fun () ->
			let dbg = Context.string_of_task __context in
			info "xenops: VM.remove %s" id;
			Client.VM.remove dbg id
		)

let update_metadata_in_xenopsd ~__context ~self =
	let id = id_of_vm ~__context ~self in
	Mutex.execute metadata_m
		(fun () ->
			let dbg = Context.string_of_task __context in
			if vm_exists_in_xenopsd dbg id
			then
				let txt = create_metadata ~__context ~upgrade:false ~self |> Metadata.rpc_of_t |> Jsonrpc.to_string in
				if Hashtbl.mem metadata_cache id && (Hashtbl.find metadata_cache id = Some txt)
				then ()
				else begin
					debug "VM %s metadata has changed: updating xenopsd" id;
					info "xenops: VM.import_metadata %s" txt;
					let (_: Vm.id) = Client.VM.import_metadata dbg txt in
					if Hashtbl.mem metadata_cache id
					then Hashtbl.replace metadata_cache id (Some txt)
				end
		)

let to_xenops_console_protocol = let open Vm in function
	| `rfb -> Rfb
	| `vt100 -> Vt100
	| `rdp -> Rfb (* RDP was never used in the XenAPI so this never happens *)
let to_xenapi_console_protocol = let open Vm in function
	| Rfb -> `rfb
	| Vt100 -> `vt100

let attach_networks ~__context ~self =
	List.iter
		(fun vif ->
			Xapi_network.register_vif ~__context vif;
			let network = Db.VIF.get_network ~__context ~self:vif in
			Xapi_network.attach_internal ~__context ~self:network ();
			Xapi_udhcpd.maybe_add_lease ~__context vif
		) (Db.VM.get_VIFs ~__context ~self)

let detach_networks ~__context ~self =
	try
		List.iter
			(fun vif ->
				Xapi_network.deregister_vif ~__context vif
			) (Db.VM.get_VIFs ~__context ~self)
	with e ->
		error "Caught %s while detaching networks" (string_of_exn e)

let with_networks_attached ~__context ~self f =
	attach_networks ~__context ~self;
	try
		f ()
	with e ->
		info "Caught %s: detaching networks" (string_of_exn e);
		begin
			try
				detach_networks ~__context ~self;
			with e ->
				error "Caught %s while detaching networks" (string_of_exn e)
		end;
		raise e

(* Event handling:
   When we tell the xenopsd to start a VM, we wait for the task to complete.
   We also wait for an iteration of the xenops event loop to ensure that
   the states of modified objects are properly set. For example: the VM
   power_state is modified by the event thread *only* and must take its
   final value when the XenAPI VM.start returns. It will not be set when
   the xenops VM.start returns since the event is asynchronous. *)

module Event = struct
	type t = {
		mutable finished: bool;
		m: Mutex.t;
		c: Condition.t;
	}
	let make () = {
		finished = false;
		m = Mutex.create ();
		c = Condition.create ();
	}
	let active = Hashtbl.create 10
	let active_m = Mutex.create ()
	let register =
		let counter = ref 0 in
		fun t ->
			Mutex.execute active_m
				(fun () ->
					let id = !counter in
					incr counter;
					Hashtbl.replace active id t;
					id
				)
	let wait dbg () =
		let t = make () in
		let id = register t in
		debug "Client.UPDATES.inject_barrier %d" id;
		Client.UPDATES.inject_barrier dbg id;
		Mutex.execute t.m
			(fun () ->
				while not t.finished do Condition.wait t.c t.m done
			)
	let wakeup dbg id =
		Client.UPDATES.remove_barrier dbg id;
		let t = Mutex.execute active_m
			(fun () ->
				if not(Hashtbl.mem active id)
				then (warn "Event.wakeup: unknown id %d" id; None)
				else
					let t = Hashtbl.find active id in
					Hashtbl.remove active id;
					Some t
			) in
		Opt.iter
			(fun t ->
				Mutex.execute t.m
					(fun () ->
						t.finished <- true;
						Condition.signal t.c
					)
			) t
end

let with_caches dbg id f =
	add_caches id;
	try
		f ()
	with e ->
		error "Caught %s: removing caches" (string_of_exn e);
		Event.wait dbg ();
		begin
			try
				remove_caches id;
			with e ->
				error "Caught %s: while removing caches" (string_of_exn e)
		end;
		raise e

(* Ignore events on VMs which are migrating away *)
let migrating_away = Hashtbl.create 10
let migrating_away_m = Mutex.create ()

let with_migrating_away uuid f =
	Mutex.execute migrating_away_m (fun () -> Hashtbl.replace migrating_away uuid ());
	finally f
		(fun () -> Mutex.execute migrating_away_m (fun () -> Hashtbl.remove migrating_away uuid))

let is_migrating_away uuid =
	Mutex.execute migrating_away_m (fun () -> Hashtbl.mem migrating_away uuid)

let update_vm ~__context id =
	try
		let open Vm in
		if is_migrating_away id
		then debug "xenopsd event: ignoring event for VM (VM %s migrating away)" id
		else
			let self = Db.VM.get_by_uuid ~__context ~uuid:id in
			let localhost = Helpers.get_localhost ~__context in
			if Db.VM.get_resident_on ~__context ~self <> localhost
			then debug "xenopsd event: ignoring event for VM (VM %s not resident)" id
			else
				let previous = Xenops_cache.find_vm id in
				let dbg = Context.string_of_task __context in
				let info = try Some (Client.VM.stat dbg id) with _ -> None in
				if Opt.map snd info = previous
				then debug "xenopsd event: ignoring event for VM %s: metadata has not changed" id
				else begin
					debug "xenopsd event: processing event for VM %s" id;
					if info = None then debug "xenopsd event: VM state missing: assuming VM has shut down";
					let different f =
						let a = Opt.map (fun x -> f (snd x)) info in
						let b = Opt.map f previous in
						a <> b in
					if different (fun x -> x.power_state) then begin
						let power_state = xenapi_of_xenops_power_state (Opt.map (fun x -> (snd x).power_state) info) in
						debug "xenopsd event: Updating VM %s power_state <- %s" id (Record_util.power_state_to_string power_state);
						if power_state = `Suspended || power_state = `Halted then begin
							detach_networks ~__context ~self;
							Db.VM.set_ha_always_run ~__context ~self ~value:false;
							Storage_access.reset ~__context ~vm:self;
						end;
						if power_state = `Halted
						then delete_metadata_from_xenopsd ~__context id;
						(* This will mark VBDs, VIFs as detached and clear resident_on
						   if the VM has permenantly shutdown. *)
						Xapi_vm_lifecycle.force_state_reset ~__context ~self ~value:power_state;
					end;
					if different (fun x -> x.domids) then begin
						debug "xenopsd event: Updating VM %s domid" id;
						Opt.iter
							(fun (_, state) ->
								Db.VM.set_domid ~__context ~self ~value:(List.hd state.domids |> Int64.of_int)
							) info;
						(* If this is a storage domain, attempt to plug the PBD *)
						Opt.iter (fun pbd ->
							let (_: Thread.t) = Thread.create (fun () ->
								(* Don't block the database update thread *)
								Xapi_pbd.plug ~__context ~self:pbd
							) () in
							()
						) (System_domains.pbd_of_vm ~__context ~vm:self)

					end;
					(* consoles *)
					if different (fun x -> x.consoles) then begin
						debug "xenopsd event: Updating VM %s consoles" id;
						Opt.iter
							(fun (_, state) ->
								let localhost = Helpers.get_localhost ~__context in
								let address = Db.Host.get_address ~__context ~self:localhost in
								let uri = Printf.sprintf "https://%s%s" address Constants.console_uri in
								let get_uri_from_location loc =
									try
										let n = String.index loc '?' in
										String.sub loc 0 n
									with Not_found -> loc
								in
								let current_protocols = List.map
									(fun self ->
										(Db.Console.get_protocol ~__context ~self |> to_xenops_console_protocol,
										Db.Console.get_location ~__context ~self |> get_uri_from_location),
										self)
									(Db.VM.get_consoles ~__context ~self) in
								let new_protocols = List.map (fun c -> (c.protocol, uri), c) state.consoles in
								(* Destroy consoles that have gone away *)
								List.iter
									(fun protocol ->
										let self = List.assoc protocol current_protocols in
										Db.Console.destroy ~__context ~self
									) (List.set_difference (List.map fst current_protocols) (List.map fst new_protocols));
								(* Create consoles that have appeared *)
								List.iter
									(fun (protocol, _) ->
										let ref = Ref.make () in
										let uuid = Uuid.to_string (Uuid.make_uuid ()) in
										let location = Printf.sprintf "%s?uuid=%s" uri uuid in
										let port =
											try Int64.of_int ((List.find (fun c -> c.protocol = protocol) state.consoles).port)
											with Not_found -> -1L
										in
										Db.Console.create ~__context ~ref ~uuid
											~protocol:(to_xenapi_console_protocol protocol) ~location ~vM:self
											~other_config:[] ~port
									) (List.set_difference (List.map fst new_protocols) (List.map fst current_protocols));
							) info;
					end;
					if different (fun x -> x.memory_target) then begin
						Opt.iter
							(fun (_, state) ->
								debug "xenopsd event: Updating VM %s memory_target <- %Ld" id state.memory_target;
								Db.VM.set_memory_target ~__context ~self ~value:state.memory_target
							) info
					end;
					if different (fun x -> x.rtc_timeoffset) then begin
						Opt.iter
							(fun (_, state) ->
								debug "xenopsd event: Updating VM %s platform:timeoffset <- %s" id state.rtc_timeoffset;
								let key = "timeoffset" in
								(try Db.VM.remove_from_platform ~__context ~self ~key with _ -> ());
								Db.VM.add_to_platform ~__context ~self ~key ~value:state.rtc_timeoffset;
							) info
					end;
					Opt.iter
						(fun (_, state) ->
							List.iter
								(fun domid ->
									if different (fun x -> x.uncooperative_balloon_driver) then begin
										debug "xenopsd event: Updating VM %s domid %d uncooperative_balloon_driver <- %b" id domid state.uncooperative_balloon_driver;
										Mutex.execute Monitor.uncooperative_domains_m
											(fun () ->
												if state.uncooperative_balloon_driver
												then Hashtbl.replace Monitor.uncooperative_domains domid ()
												else Hashtbl.remove Monitor.uncooperative_domains domid
											)
									end;
									let lookup key =
										if List.mem_assoc key state.guest_agent then Some (List.assoc key state.guest_agent) else None in
									let list dir =
										let dir = if dir.[0] = '/' then String.sub dir 1 (String.length dir - 1) else dir in
										let results = Listext.List.filter_map (fun (path, value) ->
											if String.startswith dir path then begin
												let rest = String.sub path (String.length dir) (String.length path - (String.length dir)) in
												match List.filter (fun x -> x <> "") (String.split '/' rest) with
													| x :: _ -> Some x
													| _ -> None
											end else None
										) state.guest_agent |> Listext.List.setify in
										results in
									if different (fun x -> x.guest_agent) then begin
										debug "xenopsd event: Updating VM %s domid %d guest_agent" id domid;
										Xapi_guest_agent.all lookup list ~__context ~domid ~uuid:id
									end;
									if different (fun x -> x.xsdata_state) then begin
										debug "xenopsd event: Updating VM %s domid %d xsdata" id domid;
										Db.VM.set_xenstore_data ~__context ~self ~value:state.xsdata_state
									end;
									if different (fun x -> x.memory_target) then begin
										debug "xenopsd event: Updating VM %s domid %d memory target" id domid;
										Mutex.execute Monitor.memory_targets_m (fun () -> 
											Hashtbl.replace Monitor.memory_targets domid state.memory_target)
									end;
								) state.domids;
						) info;
					if different (fun x -> x.vcpu_target) then begin
						Opt.iter
							(fun (_, state) ->
								debug "xenopsd event: Updating VM %s vcpu_target <- %d" id state.Vm.vcpu_target;
								let metrics = Db.VM.get_metrics ~__context ~self in
								Db.VM_metrics.set_VCPUs_number ~__context ~self:metrics ~value:(Int64.of_int state.Vm.vcpu_target);
							) info
					end;
					if different (fun x -> x.last_start_time) then begin
						Opt.iter
							(fun (_, state) ->
								debug "xenopsd event: Updating VM %s last_start_time <- %s" id (Date.to_string (Date.of_float state.last_start_time));
								let metrics = Db.VM.get_metrics ~__context ~self in
								let start_time = Date.of_float state.last_start_time in
								Db.VM_metrics.set_start_time ~__context ~self:metrics ~value:start_time;
								begin
									try
										let gm = Db.VM.get_guest_metrics ~__context ~self in
										let update_time = Db.VM_guest_metrics.get_last_updated ~__context ~self:gm in
										if update_time < start_time then begin
											debug "VM %s guest metrics update time (%s) < VM start time (%s): deleting"
												id (Date.to_string update_time) (Date.to_string start_time);
											Xapi_vm_helpers.delete_guest_metrics ~__context ~self
										end
									with _ -> () (* The guest metrics didn't exist *)
								end
							) info
					end;
					if different (fun x -> x.shadow_multiplier_target) then begin
						Opt.iter
							(fun (_, state) ->
								debug "xenopsd event: Updating VM %s shadow_multiplier <- %.2f" id state.shadow_multiplier_target;
								Db.VM.set_HVM_shadow_multiplier ~__context ~self ~value:state.shadow_multiplier_target
							) info
					end;
					Xenops_cache.update_vm id (Opt.map snd info);
					Xapi_vm_lifecycle.update_allowed_operations ~__context ~self;
				end
	with e ->
		error "xenopsd event: Caught %s while updating VM: has this VM been removed while this host is offline?" (string_of_exn e)

let update_vbd ~__context (id: (string * string)) =
	try
		let open Vbd in
		if is_migrating_away (fst id)
		then debug "xenopsd event: ignoring event for VM (VM %s migrating away)" (fst id)
		else
			let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
			let localhost = Helpers.get_localhost ~__context in
			if Db.VM.get_resident_on ~__context ~self:vm <> localhost
			then debug "xenopsd event: ignoring event for VBD (VM %s not resident)" (fst id)
			else
				let previous = Xenops_cache.find_vbd id in
				let dbg = Context.string_of_task __context in
				let info = try Some(Client.VBD.stat dbg id) with _ -> None in
				if Opt.map snd info = previous
				then debug "xenopsd event: ignoring event for VBD %s.%s: metadata has not changed" (fst id) (snd id)
				else begin
					let vbds = Db.VM.get_VBDs ~__context ~self:vm in
					let vbdrs = List.map (fun self -> self, Db.VBD.get_record ~__context ~self) vbds in
					let linux_device = snd id in
					let disk_number = Device_number.of_linux_device (snd id) |> Device_number.to_disk_number |> string_of_int in
					debug "VM %s VBD userdevices = [ %s ]" (fst id) (String.concat "; " (List.map (fun (_,r) -> r.API.vBD_userdevice) vbdrs));
					let vbd, vbd_r = List.find (fun (_, vbdr) -> vbdr.API.vBD_userdevice = linux_device || vbdr.API.vBD_userdevice = disk_number) vbdrs in
					Opt.iter
						(fun (x, state) ->
							debug "xenopsd event: Updating VBD %s.%s device <- %s; currently_attached <- %b" (fst id) (snd id) linux_device state.plugged;
							Db.VBD.set_device ~__context ~self:vbd ~value:linux_device;
							Db.VBD.set_currently_attached ~__context ~self:vbd ~value:state.plugged;
							debug "state.media_present = %b" state.media_present;
							if state.plugged then begin
								if state.media_present then begin
									match x.backend with
										| Some (VDI x) ->
											Opt.iter
												(fun (vdi, _) ->
													Db.VBD.set_VDI ~__context ~self:vbd ~value:vdi;
													Db.VBD.set_empty ~__context ~self:vbd ~value:false;
													Xapi_vdi.update_allowed_operations ~__context ~self:vdi;
												) (vdi_of_disk ~__context x)
										| _ ->
											error "I don't know what to do with this kind of VDI backend"
								end else if vbd_r.API.vBD_type = `CD then begin
									Db.VBD.set_empty ~__context ~self:vbd ~value:true;
									Db.VBD.set_VDI ~__context ~self:vbd ~value:Ref.null
								end
							end
						) info;
					Xenops_cache.update_vbd id (Opt.map snd info);
					Xapi_vbd_helpers.update_allowed_operations ~__context ~self:vbd
				end
	with e ->
		error "xenopsd event: Caught %s while updating VBD" (string_of_exn e)

let update_vif ~__context id =
	try
		if is_migrating_away (fst id)
		then debug "xenopsd event: ignoring event for VIF (VM %s migrating away)" (fst id)
		else
			let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
			let localhost = Helpers.get_localhost ~__context in
			if Db.VM.get_resident_on ~__context ~self:vm <> localhost
			then debug "xenopsd event: ignoring event for VIF (VM %s not resident)" (fst id)
			else
				let open Vif in
				let previous = Xenops_cache.find_vif id in
				let dbg = Context.string_of_task __context in
				let info = try Some (Client.VIF.stat dbg id) with _ -> None in
				if Opt.map snd info = previous
				then debug "xenopsd event: ignoring event for VIF %s.%s: metadata has not changed" (fst id) (snd id)
				else begin
					let vifs = Db.VM.get_VIFs ~__context ~self:vm in
					let vifrs = List.map (fun self -> self, Db.VIF.get_record ~__context ~self) vifs in
					let vif, _ = List.find (fun (_, vifr) -> vifr.API.vIF_device = (snd id)) vifrs in
					Opt.iter
						(fun (_, state) ->
							if not state.plugged
							then Xapi_network.deregister_vif ~__context vif;
							debug "xenopsd event: Updating VIF %s.%s currently_attached <- %b" (fst id) (snd id) state.plugged;
							Db.VIF.set_currently_attached ~__context ~self:vif ~value:state.plugged
						) info;
					Xenops_cache.update_vif id (Opt.map snd info);
					Xapi_vif_helpers.update_allowed_operations ~__context ~self:vif
				end
	with e ->
		error "xenopsd event: Caught %s while updating VIF" (string_of_exn e)

let update_pci ~__context id =
	try
		if is_migrating_away (fst id)
		then debug "xenopsd event: ignoring event for PCI (VM %s migrating away)" (fst id)
		else
			let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
			let localhost = Helpers.get_localhost ~__context in
			if Db.VM.get_resident_on ~__context ~self:vm <> localhost
			then debug "xenopsd event: ignoring event for PCI (VM %s not resident)" (fst id)
			else
				let open Pci in
				let previous = Xenops_cache.find_pci id in
				let dbg = Context.string_of_task __context in
				let info = try Some (Client.PCI.stat dbg id) with _ -> None in
				if Opt.map snd info = previous
				then debug "xenopsd event: ignoring event for PCI %s.%s: metadata has not changed" (fst id) (snd id)
				else begin
					let pcis = Db.Host.get_PCIs ~__context ~self:localhost in
					let pcirs = List.map (fun self -> self, Db.PCI.get_record ~__context ~self) pcis in

					let pci, _ = List.find (fun (_, pcir) -> pcir.API.pCI_pci_id = (snd id)) pcirs in

					(* Assumption: a VM can have only one vGPU *)
					let gpu =
						let gpu_class_id = Xapi_pci.find_class_id (Xapi_pci.Display_controller) in
						if Db.PCI.get_class_id ~__context ~self:pci = gpu_class_id
						then
							match Db.VM.get_VGPUs ~__context ~self:vm with
								| x :: _ -> Some x
								| _ -> None
						else None in
					let attached_in_db = List.mem vm (Db.PCI.get_attached_VMs ~__context ~self:pci) in
					Opt.iter
						(fun (_, state) ->
							debug "xenopsd event: Updating PCI %s.%s currently_attached <- %b" (fst id) (snd id) state.plugged;
							if attached_in_db && (not state.plugged)
							then Db.PCI.remove_attached_VMs ~__context ~self:pci ~value:vm
							else if (not attached_in_db) && state.plugged
							then Db.PCI.add_attached_VMs ~__context ~self:pci ~value:vm;

							Opt.iter
								(fun gpu ->
									debug "xenopsd event: Update VGPU %s.%s currently_attached <- %b" (fst id) (snd id) state.plugged;
									Db.VGPU.set_currently_attached ~__context ~self:gpu ~value:state.plugged
								) gpu
						) info;
					Xenops_cache.update_pci id (Opt.map snd info);
				end
	with e ->
		error "xenopsd event: Caught %s while updating PCI" (string_of_exn e)

let id_to_task_tbl = Hashtbl.create 10
let task_to_id_tbl = Hashtbl.create 10
let task_tbl_m = Mutex.create ()

let id_to_task_exn id =
	Mutex.execute task_tbl_m
		(fun () ->
			Hashtbl.find id_to_task_tbl id
		)

let task_to_id_exn task =
	Mutex.execute task_tbl_m
		(fun () ->
			Hashtbl.find task_to_id_tbl task
		)

let register_task __context id =
	let task = Context.get_task_id __context in
	Mutex.execute task_tbl_m
		(fun () ->
			Hashtbl.replace id_to_task_tbl id task;
			Hashtbl.replace task_to_id_tbl task id;
		);
	(* Since we've bound the XenAPI Task to the xenopsd Task, and the xenopsd Task
	   is cancellable, mark the XenAPI Task as cancellable too. *)
	TaskHelper.set_cancellable ~__context;
	id

let unregister_task __context id =
	(* The rest of the XenAPI Task won't be cancellable *)
	TaskHelper.set_not_cancellable ~__context;
	Mutex.execute task_tbl_m
		(fun () ->
			let task = Hashtbl.find id_to_task_tbl id in
			Hashtbl.remove id_to_task_tbl id;
			Hashtbl.remove task_to_id_tbl task;
		);
	id

let update_task ~__context id =
	try
		let self = id_to_task_exn id in (* throws Not_found *)
		let dbg = Context.string_of_task __context in
		let task_t = Client.TASK.stat dbg id in
		match task_t.Task.result with
			| Task.Pending x ->
				Db.Task.set_progress ~__context ~self ~value:x
			| _ -> ()
	with Not_found ->
		(* Since this is called on all tasks, possibly after the task has been
		   destroyed, it's safe to ignore a Not_found exception here. *)
		()
	| e ->
		error "xenopsd event: Caught %s while updating task" (string_of_exn e)

let rec events_watch ~__context from =
	let dbg = Context.string_of_task __context in
	let events, next = Client.UPDATES.get dbg from None in
	let open Dynamic in
	List.iter
		(function
			| Vm id ->
				debug "xenops event on VM %s" id;
				update_vm ~__context id;
			| Vbd id ->
				debug "xenops event on VBD %s.%s" (fst id) (snd id);
				update_vbd ~__context id
			| Vif id ->
				debug "xenops event on VIF %s.%s" (fst id) (snd id);
				update_vif ~__context id
			| Pci id ->
				debug "xenops event on PCI %s.%s" (fst id) (snd id);
				update_pci ~__context id
			| Task id ->
				debug "xenops event on Task %s" id;
				update_task ~__context id
			| Barrier id ->
				debug "barrier %d" id;
				Event.wakeup dbg id
		) events;
	events_watch ~__context next

let manage_dom0 dbg =
	(* Tell xenopsd to manage domain 0 *)
	let uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
	let open Vm in
	if not(vm_exists_in_xenopsd dbg uuid)
	then begin
		info "Client.VM.add %s" uuid;
		Client.VM.add dbg {
			id = uuid;
			name = "Domain-0";
			ssidref = 0l;
			xsdata = [];
			platformdata = [];
			bios_strings = [];
			ty = PV {
				boot = Direct { kernel = ""; cmdline = ""; ramdisk = None };
				framebuffer = false; vncterm = false; vncterm_ip = None 
			};
			suppress_spurious_page_faults = false;
			machine_address_size = None;
			memory_static_max = 0L;
			memory_dynamic_max = 0L;
			memory_dynamic_min = 0L;
			vcpu_max = 0;
			vcpus = 0;
			scheduler_params = { priority = None; affinity = [] };
			on_crash = [];
			on_shutdown = [];
			on_reboot = [];
			pci_msitranslate = true;
			pci_power_mgmt = false;
		} |> ignore;
	end

let on_xapi_restart ~__context =
	let dbg = Context.string_of_task __context in
	manage_dom0 dbg;
	(* Destroy each active task in xenopsd, since the previous xapi
	   is not able to do it. *)
	let tasks = Client.TASK.list dbg in
	List.iter
		(fun t ->
			info "Deleting leaked xenopsd task %s (%s) (%s)" t.Task.id t.Task.debug_info (t.Task.result |> Task.rpc_of_result |> Jsonrpc.to_string);
			Client.TASK.destroy dbg t.Task.id
		) tasks;
	(* For each VM resident on this host, check if the xenopsd
	   has forgotten about it: this means it has shut down *)
	let localhost = Helpers.get_localhost ~__context in
	let vms = Db.Host.get_resident_VMs ~__context ~self:localhost in
	let in_db = List.map (fun self -> id_of_vm ~__context ~self) vms in
	let in_xenopsd = Client.VM.list dbg () |> List.map (fun (vm, _) -> vm.Vm.id) in
	List.iter add_caches in_xenopsd;
	List.iter
		(fun id ->
			info "VM %s is not running here: setting power_state to Halted" id;
			let vm = Db.VM.get_by_uuid ~__context ~uuid:id in
			Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted
		) (List.set_difference in_db in_xenopsd);
	List.iter
		(fun id ->
			try
				info "VM %s is running here: setting power_state to Running" id;
				let vm = Db.VM.get_by_uuid ~__context ~uuid:id in
				Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Running;
				Db.VM.set_resident_on ~__context ~self:vm ~value:localhost
			with _ ->
				begin
					try
						info "VM %s is running here but isn't in the database: terminating" id;
						let dbg = Context.string_of_task __context in
						Client.VM.shutdown dbg id None |> wait_for_task dbg |> ignore;
						Client.VM.remove dbg id
					with e ->
						error "Failed to remove VM %s: %s" id (string_of_exn e)
				end
		) (List.set_difference in_xenopsd in_db)

let events_from_xenopsd () =
    Server_helpers.exec_with_new_task "xapi_xenops"
		(fun __context ->
			while true do
				try
					events_watch ~__context None;
				with e ->
					error "event thread caught: %s" (string_of_exn e);
					Thread.delay 10.
			done
		)

(* Ignore events on VMs which are shutting down *)
let shutting_down = Hashtbl.create 10
let shutting_down_m = Mutex.create ()

let with_shutting_down uuid f =
	Mutex.execute shutting_down_m (fun () -> Hashtbl.replace shutting_down uuid ());
	finally f
		(fun () -> Mutex.execute shutting_down_m (fun () -> Hashtbl.remove shutting_down uuid))

(* XXX; PR-1255: this needs to be combined with the migration lock *)
let is_shutting_down uuid = Hashtbl.mem shutting_down uuid

(* If a xapi event thread is blocked, wake it up and cause it to re-register. This should be
   called after updating Host.resident_VMs *)
let trigger_xenapi_reregister =
	ref (fun () ->
		debug "No xapi event thread to wake up"
	)

(* XXX: PR-1255: this will be receiving too many events and we may wish to synchronise
   updates to the VM metadata and resident_on fields *)
(* XXX: PR-1255: we also want to only listen for events on VMs and fields we care about *)
let events_from_xapi () =
	let open Event_types in
    Server_helpers.exec_with_new_task "xapi_xenops"
		(fun __context ->
			let localhost = Helpers.get_localhost ~__context in
			let token = ref "" in
			while true do
				try
					Helpers.call_api_functions ~__context
						(fun rpc session_id ->
							trigger_xenapi_reregister :=
								(fun () ->
									try
										(* This causes Event.next () and Event.from () to return SESSION_INVALID *)
										XenAPI.Session.logout ~rpc ~session_id
									with
										| Api_errors.Server_error(code, _) when code = Api_errors.session_invalid ->
											debug "Event thead has already woken up"
										| e ->
											error "Waking up the xapi event thread: %s" (string_of_exn e)
								);
							(* We register for events on resident_VMs only *)
							let classes = List.map (fun x -> Printf.sprintf "VM/%s" (Ref.string_of x)) (Db.Host.get_resident_VMs ~__context ~self:localhost) in
							(* NB we re-use the old token so we don't get events we've already
							   received BUT we will not necessarily receive events for the new VMs *)

							while true do
								let from = XenAPI.Event.from ~rpc ~session_id ~classes ~token:!token ~timeout:60. |> event_from_of_xmlrpc in
								List.iter
									(function
										| { ty = "vm"; reference = vm' } ->
											let vm = Ref.of_string vm' in
											Mutex.execute shutting_down_m
												(fun () ->
													let id = id_of_vm ~__context ~self:vm in
													let resident_here = Db.VM.get_resident_on ~__context ~self:vm = localhost in
													let shutting_down = is_shutting_down id in
													debug "Event on VM %s; resident_here = %b; shutting_down = %b" id resident_here shutting_down;
													if resident_here && not shutting_down then begin
														update_metadata_in_xenopsd ~__context ~self:vm |> ignore
													end
												)
										| _ -> ()
									) from.events;
								token := string_of_token from.token;
							done
						)
				with 
					| Api_errors.Server_error(code, _) when code = Api_errors.session_invalid ->
						debug "Woken event thread: updating list of event subscriptions"
					| e ->
						debug "Caught %s listening to events from xapi" (string_of_exn e);
						(* Start from scratch *)
						token := "";
						Thread.delay 15.
			done
		)

let success_task f dbg id =
	finally
		(fun () ->
			let t = Client.TASK.stat dbg id in
			match t.Task.result with
				| Task.Completed _ -> f t
				| Task.Failed x -> 
					let exn = exn_of_exnty (Exception.exnty_of_rpc x) in
					begin match exn with 
						| Failed_to_contact_remote_service x ->
							failwith (Printf.sprintf "Failed to contact remote service on: %s\n" x)
						| e -> 
							debug "%s: caught xenops exception: %s" dbg (Jsonrpc.to_string x);
							raise e
					end
				| Task.Pending _ -> failwith "task pending"
		) (fun () -> Client.TASK.destroy dbg id)

(* Catch any uncaught xenops exceptions and transform into the most relevant XenAPI error.
   We do not want a XenAPI client to see a raw xenopsd error. *)
let transform_xenops_exn ~__context f =
	let reraise code params =
		error "Re-raising as %s [ %s ]" code (String.concat "; " params);
		raise (Api_errors.Server_error(code, params)) in
	let internal fmt = Printf.kprintf
		(fun x ->
			reraise Api_errors.internal_error [ x ]
		) fmt in
	try
		f ()
	with
		| Internal_error msg -> internal "xenopsd internal error: %s" msg
		| Already_exists(thing, id) -> internal "Object with type %s and id %s already exists in xenopsd" thing id
		| Does_not_exist(thing, id) -> internal "Object with type %s and id %s does not exist in xenopsd" thing id
		| Unimplemented(fn) -> reraise Api_errors.not_implemented [ fn ]
		| Domain_not_built -> internal "domain has not been built"
		| Maximum_vcpus n -> internal "the maximum number of vcpus configured for this VM is currently: %d" n
		| Bad_power_state(found, expected) ->
			let f x = x |> (fun x -> Some x) |> xenapi_of_xenops_power_state |> Record_util.power_state_to_string in
			let found = f found and expected = f expected in
			reraise Api_errors.vm_bad_power_state [ expected; found ]
		| Failed_to_acknowledge_shutdown_request ->
			reraise Api_errors.vm_failed_shutdown_ack []
		| Failed_to_shutdown(id, timeout) ->
			reraise Api_errors.vm_shutdown_timeout [ vm_of_id ~__context id |> Ref.string_of; string_of_float timeout ]
		| Device_is_connected ->
			internal "Cannot remove device because it is connected to a VM"
		| Device_not_connected ->
			internal "Device is not connected"
		| Device_detach_rejected(cls, id, msg) ->
			reraise Api_errors.device_detach_rejected [ cls; id; msg ]
		| Media_not_ejectable -> internal "the media in this drive cannot be ejected"
		| Media_present -> internal "there is already media in this drive"
		| Media_not_present -> internal "there is no media in this drive"
		| No_bootable_device -> internal "there is no bootable device"
		| Bootloader_error(code, params) -> reraise code params
		| Cannot_free_this_much_memory(needed, free) ->
			reraise Api_errors.host_not_enough_free_memory [ Int64.to_string needed; Int64.to_string free ]
		| Vms_failed_to_cooperate vms ->
			let vms' = List.map (fun uuid -> Db.VM.get_by_uuid ~__context ~uuid |> Ref.string_of) vms in
			reraise Api_errors.vms_failed_to_cooperate vms'
		| Ballooning_error(code, descr) -> internal "ballooning error: %s %s" code descr
		| IO_error -> reraise Api_errors.vdi_io_error ["I/O error saving VM suspend image"]
		| Failed_to_contact_remote_service x -> internal "failed to contact: %s" x
		| Hook_failed(script, reason, stdout, i) -> reraise Api_errors.xapi_hook_failed [ script; reason; stdout; i ]
		| Not_enough_memory needed -> internal "there was not enough memory (needed %Ld bytes)" needed
		| Cancelled id ->
			let task =
				try id_to_task_exn id
				with _ ->
					debug "xenopsd task id %s is not associated with a XenAPI task" id;
					Ref.null in
			reraise Api_errors.task_cancelled [ Ref.string_of task ]
		| Storage_backend_error(code, params) -> reraise code params


let refresh_vm ~__context ~self =
	let id = id_of_vm ~__context ~self in
	info "xenops: UPDATES.refresh_vm %s" id;
	let dbg = Context.string_of_task __context in
	Client.UPDATES.refresh_vm dbg id;
	Event.wait dbg ()

(* After this function is called, locally-generated events will be reflected
   in the xapi pool metadata. When this function returns we believe that the
   VM state is in 'sync' with xenopsd and the pool master where by 'sync'
   we mean that all changes will eventually be propagated or 'no events lost' *)
let set_resident_on ~__context ~self =
	let id = id_of_vm ~__context ~self in
	debug "VM %s set_resident_on" id;
	let localhost = Helpers.get_localhost ~__context in
	Helpers.call_api_functions ~__context
		(fun rpc session_id -> XenAPI.VM.atomic_set_resident_on rpc session_id self localhost);
	refresh_vm ~__context ~self;
	debug "Signalling xenapi event thread to re-register";
	!trigger_xenapi_reregister ();
	(* Any future XenAPI updates will trigger events, but we might have missed one so: *)
	update_metadata_in_xenopsd ~__context ~self |> ignore

let sync_with_task __context x =
	let dbg = Context.string_of_task __context in
	x |> register_task __context |> wait_for_task dbg |> unregister_task __context |> success_task ignore_task dbg

let sync __context x =
	let dbg = Context.string_of_task __context in
	x |> wait_for_task dbg |> success_task ignore_task dbg

let assert_resident_on ~__context ~self =
	let localhost = Helpers.get_localhost ~__context in
	assert (Db.VM.get_resident_on ~__context ~self = localhost)

let pause ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			let id = id_of_vm ~__context ~self in
			debug "xenops: VM.pause %s" id;
			let dbg = Context.string_of_task __context in
			Client.VM.pause dbg id |> sync_with_task __context;
			Event.wait dbg ();
			assert (Db.VM.get_power_state ~__context ~self = `Paused)
		)

let unpause ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			let id = id_of_vm ~__context ~self in
			debug "xenops: VM.unpause %s" id;
			let dbg = Context.string_of_task __context in
			Client.VM.unpause dbg id |> sync_with_task __context;
			Event.wait dbg ();
			assert (Db.VM.get_power_state ~__context ~self = `Running)
		)

let set_xenstore_data ~__context ~self xsdata =
	transform_xenops_exn ~__context
		(fun () ->
			let id = id_of_vm ~__context ~self in
			debug "xenops: VM.unpause %s" id;
			let dbg = Context.string_of_task __context in
			Client.VM.set_xsdata dbg id xsdata |> sync_with_task __context;
			Event.wait dbg ();
		)

let set_vcpus ~__context ~self n =
	transform_xenops_exn ~__context
		(fun () ->
			let id = id_of_vm ~__context ~self in
			debug "xenops: VM.unpause %s" id;
			let dbg = Context.string_of_task __context in
			try
				Client.VM.set_vcpus dbg id (Int64.to_int n) |> sync_with_task __context;
				Db.VM.set_VCPUs_at_startup ~__context ~self ~value:n;
				Event.wait dbg ();
			with
				| Maximum_vcpus n ->
					raise (Api_errors.Server_error(Api_errors.invalid_value, [
						"VCPU values must satisfy: 0 < VCPUs ≤ VCPUs_max";
						string_of_int n
					]))
				| Unimplemented _ ->
					error "VM.set_VCPUs_number_live: HVM VMs cannot hotplug cpus";
					raise (Api_errors.Server_error (Api_errors.operation_not_allowed,
					["HVM VMs cannot hotplug CPUs"]))
		)

let set_shadow_multiplier ~__context ~self target =
	transform_xenops_exn ~__context
		(fun () ->
			let id = id_of_vm ~__context ~self in
			debug "xenops: VM.set_shadow_multiplier %s" id;
			let dbg = Context.string_of_task __context in
			try
				Client.VM.set_shadow_multiplier dbg id target |> sync_with_task __context;
				Event.wait dbg ();
			with
				| Not_enough_memory needed ->
					let host = Db.VM.get_resident_on ~__context ~self in
					let free_mem_b = Memory_check.host_compute_free_memory_with_maximum_compression ~__context ~host None in
					raise (Api_errors.Server_error(Api_errors.host_not_enough_free_memory, [ Int64.to_string needed; Int64.to_string free_mem_b ]))
				| Unimplemented _ ->
					(* The existing behaviour is to ignore this failure *)
					error "VM.set_shadow_multiplier: not supported for PV domains"
		)

let set_memory_dynamic_range ~__context ~self min max =
	transform_xenops_exn ~__context
		(fun () ->
			let id = id_of_vm ~__context ~self in
			debug "xenops: VM.set_memory_dynamic_range %s" id;
			let dbg = Context.string_of_task __context in
			Client.VM.set_memory_dynamic_range dbg id min max |> sync_with_task __context;
			Event.wait dbg ()
		)

let with_metadata_pushed_to_xenopsd ?(upgrade=false) ~__context ~self f =
	let id = push_metadata_to_xenopsd ~__context ~upgrade ~self in
	try
		f id;
		set_resident_on ~__context ~self;
	with e ->
		error "Caught %s: removing VM %s from xenopsd" (string_of_exn e) id;
		begin
			try
				delete_metadata_from_xenopsd ~__context id;
			with e ->
				error "Caught %s while removing VM %s from metadata" (string_of_exn e) id
		end;
		raise e

let start ~__context ~self paused =
	let dbg = Context.string_of_task __context in
	transform_xenops_exn ~__context
		(fun () ->
			with_metadata_pushed_to_xenopsd ~__context ~self
				(fun id ->
					with_caches dbg id
						(fun () ->
							with_networks_attached ~__context ~self
								(fun () ->
									info "xenops: VM.start %s" id;
									Client.VM.start dbg id |> sync_with_task __context;
									if not paused then begin
										info "xenops: VM.unpause %s" id;
										Client.VM.unpause dbg id |> sync __context;
									end;
								)
						)
				);
			(* XXX: if the guest crashed or shutdown immediately then it may be offline now *)
			assert (Db.VM.get_power_state ~__context ~self = (if paused then `Paused else `Running))
		)

let start ~__context ~self paused =
	transform_xenops_exn ~__context
		(fun () ->
			try
				start ~__context ~self paused
			with Bad_power_state(a, b) ->
				let power_state = function
					| Running -> "Running"
					| Halted -> "Halted"
					| Suspended -> "Suspended"
					| Paused -> "Paused" in
				raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, [ Ref.string_of self; power_state a; power_state b ]))
		)

let reboot ~__context ~self timeout =
	transform_xenops_exn ~__context
		(fun () ->
			assert_resident_on ~__context ~self;
			let id = id_of_vm ~__context ~self in
			info "xenops: VM.reboot %s" id;
			let dbg = Context.string_of_task __context in
			(* Ensure we have the latest version of the VM metadata before the reboot *)
			update_metadata_in_xenopsd ~__context ~self;
			Client.VM.reboot dbg id timeout |> sync_with_task __context;
			Event.wait dbg ();
			assert (Db.VM.get_power_state ~__context ~self = `Running)
		)

let shutdown ~__context ~self timeout =
	transform_xenops_exn ~__context
		(fun () ->
			assert_resident_on ~__context ~self;
			let id = id_of_vm ~__context ~self in
			with_shutting_down id
				(fun () ->
					info "xenops: VM.shutdown %s" id;
					let dbg = Context.string_of_task __context in
					Client.VM.shutdown dbg id timeout |> sync_with_task __context;
					Event.wait dbg ();
					remove_caches id;
					assert (Db.VM.get_power_state ~__context ~self = `Halted);
					(* force_state_reset called from the xenopsd event loop above *)
					assert (Db.VM.get_resident_on ~__context ~self = Ref.null);
					List.iter
						(fun vbd ->
							assert (not(Db.VBD.get_currently_attached ~__context ~self:vbd))
						) (Db.VM.get_VBDs ~__context ~self)
				)
		)

let suspend ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			assert_resident_on ~__context ~self;
			let id = id_of_vm ~__context ~self in
			let dbg = Context.string_of_task __context in
			let vm_t, state = Client.VM.stat dbg id in
			(* XXX: this needs to be at boot time *)
			let space_needed = Int64.(add (of_float (to_float vm_t.Vm.memory_static_max *. 1.2 *. 1.05)) 104857600L) in
			let suspend_SR = Helpers.choose_suspend_sr ~__context ~vm:self in
			let sm_config = [Xapi_globs._sm_vm_hint, id] in
			Helpers.call_api_functions ~__context
				(fun rpc session_id ->
					let vdi =
						XenAPI.VDI.create ~rpc ~session_id
							~name_label:"Suspend image"
							~name_description:"Suspend image"
							~sR:suspend_SR ~virtual_size:space_needed
							~sharable:false ~read_only:false ~_type:`suspend
							~other_config:[] ~xenstore_data:[] ~sm_config ~tags:[] in
					let disk = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in
					Db.VM.set_suspend_VDI ~__context ~self ~value:vdi;
					try
						info "xenops: VM.suspend %s to %s" id (disk |> rpc_of_disk |> Jsonrpc.to_string);
						let dbg = Context.string_of_task __context in
						Client.VM.suspend dbg id disk |> sync_with_task __context;
						Event.wait dbg ();
						remove_caches id;
						assert (Db.VM.get_power_state ~__context ~self = `Suspended);
						assert (Db.VM.get_resident_on ~__context ~self = Ref.null);
						(* XXX PR-1255: need to convert between 'domains' and lbr *)
						let md = pull_metadata_from_xenopsd ~__context id in
						match md.Metadata.domains with
							| None ->
								failwith "Suspended VM has no domain-specific metadata"
							| Some x ->
								Db.VM.set_last_booted_record ~__context ~self ~value:x;
								debug "VM %s last_booted_record set to %s" (Ref.string_of self) x
					with e ->
						debug "Caught exception suspending VM: %s" (string_of_exn e);
						XenAPI.VDI.destroy ~rpc ~session_id ~self:vdi;
						Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null;
						raise e
				)
		)

let resume ~__context ~self ~start_paused ~force =
	let dbg = Context.string_of_task __context in
	transform_xenops_exn ~__context
		(fun () ->
			let vdi = Db.VM.get_suspend_VDI ~__context ~self in
			let disk = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in	
			with_metadata_pushed_to_xenopsd ~__context ~self
				(fun id ->
					with_caches dbg id
						(fun () ->
							with_networks_attached ~__context ~self
								(fun () ->
									info "xenops: VM.resume %s from %s" id (disk |> rpc_of_disk |> Jsonrpc.to_string);
									Client.VM.resume dbg id disk |> sync_with_task __context;
									if not start_paused then begin
										info "xenops: VM.unpause %s" id;
										Client.VM.unpause dbg id |> sync_with_task __context;
									end;
								)
						)
				);
			assert (Db.VM.get_power_state ~__context ~self = if start_paused then `Paused else `Running);
			Helpers.call_api_functions ~__context
				(fun rpc session_id ->
					XenAPI.VDI.destroy rpc session_id vdi
				);
			Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null
		)

let s3suspend ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			let id = id_of_vm ~__context ~self in
			let dbg = Context.string_of_task __context in
			debug "xenops: VM.s3suspend %s" id;
			Client.VM.s3suspend dbg id |> sync_with_task __context;
			Event.wait dbg ()
		)

let s3resume ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			let id = id_of_vm ~__context ~self in
			let dbg = Context.string_of_task __context in
			debug "xenops: VM.s3resume %s" id;
			Client.VM.s3resume dbg id |> sync_with_task __context;
			Event.wait dbg ()
		)

let is_vm_running ~__context ~self =
	let id = id_of_vm ~__context ~self in
	let dbg = Context.string_of_task __context in
	debug "xenops: VM.stat %s" id;
	(* If the metadata is still present, VM is "Running" *)
	try Client.VM.stat dbg id |> ignore; true with _ -> false

let md_of_vbd ~__context ~self =
	let vm = Db.VBD.get_VM ~__context ~self in
	MD.of_vbd ~__context ~vm:(Db.VM.get_record ~__context ~self:vm) ~vbd:(Db.VBD.get_record ~__context ~self)

let vbd_eject ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			let vm = Db.VBD.get_VM ~__context ~self in
			assert_resident_on ~__context ~self:vm;
			(* XXX: PR-1255: move the offline stuff to the master/message_forwarding? *)
			if Db.VM.get_power_state ~__context ~self:vm = `Halted then begin
				Db.VBD.set_empty ~__context ~self ~value:true;
				Db.VBD.set_VDI ~__context ~self ~value:Ref.null
			end else begin
				let vbd = md_of_vbd ~__context ~self in
				info "xenops: VBD.eject %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
				let dbg = Context.string_of_task __context in
				Client.VBD.eject dbg vbd.Vbd.id |> sync_with_task __context;
				Event.wait dbg ();
				(* XXX: PR-1255: this is because a PV eject is an unplug, so the
				   event is different *)
				Db.VBD.set_empty ~__context ~self ~value:true;
				Db.VBD.set_VDI ~__context ~self ~value:Ref.null		
			end;
			assert (Db.VBD.get_empty ~__context ~self);
			assert (Db.VBD.get_VDI ~__context ~self = Ref.null)
		)

let vbd_insert ~__context ~self ~vdi =
	transform_xenops_exn ~__context
		(fun () ->
			let vm = Db.VBD.get_VM ~__context ~self in
			assert_resident_on ~__context ~self:vm;
			(* XXX: PR-1255: move the offline stuff to the master/message_forwarding? *)
			if Db.VM.get_power_state ~__context ~self:vm = `Halted then begin
				Db.VBD.set_VDI ~__context ~self ~value:vdi;
				Db.VBD.set_empty ~__context ~self ~value:false
			end else begin
				let vbd = md_of_vbd ~__context ~self in
				let disk = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in
				info "xenops: VBD.insert %s.%s %s" (fst vbd.Vbd.id) (snd vbd.Vbd.id) (disk |> rpc_of_disk |> Jsonrpc.to_string);
				let dbg = Context.string_of_task __context in
				Client.VBD.insert dbg vbd.Vbd.id disk |> sync_with_task __context;
				Event.wait dbg ()
			end;
			assert (not(Db.VBD.get_empty ~__context ~self));
			assert (Db.VBD.get_VDI ~__context ~self = vdi)
		)

let vbd_plug ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			let vm = Db.VBD.get_VM ~__context ~self in
			assert_resident_on ~__context ~self:vm;
			let vbd = md_of_vbd ~__context ~self in
			info "xenops: VBD.remove %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
			let dbg = Context.string_of_task __context in
			(try Client.VBD.remove dbg vbd.Vbd.id with Does_not_exist _ -> ());
			info "xenops: VBD.add %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
			let id = Client.VBD.add dbg vbd in
			info "xenops: VBD.plug %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
			Client.VBD.plug dbg id |> sync_with_task __context;
			Event.wait dbg ();
			assert (Db.VBD.get_currently_attached ~__context ~self)
		)

let vbd_unplug ~__context ~self force =
	transform_xenops_exn ~__context
		(fun () ->
			let vm = Db.VBD.get_VM ~__context ~self in
			assert_resident_on ~__context ~self:vm;
			let vbd = md_of_vbd ~__context ~self in
			info "xenops: VBD.unplug %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
			let dbg = Context.string_of_task __context in
			begin
				try
					Client.VBD.unplug dbg vbd.Vbd.id force |> sync_with_task __context;
				with Device_detach_rejected(_, _, _) ->
					raise (Api_errors.Server_error(Api_errors.device_detach_rejected, [ "VBD"; Ref.string_of self; "" ]))
			end;
			(* We need to make sure VBD.stat still works so: wait before calling VBD.remove *)
			Event.wait dbg ();
			info "xenops: VBD.remove %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
			Client.VBD.remove dbg vbd.Vbd.id;
			assert (not(Db.VBD.get_currently_attached ~__context ~self))
		)

let md_of_vif ~__context ~self =
	let vm = Db.VIF.get_VM ~__context ~self in
	MD.of_vif ~__context ~vm:(Db.VM.get_record ~__context ~self:vm) ~vif:(Db.VIF.get_record ~__context ~self)

let vif_plug ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			let vm = Db.VIF.get_VM ~__context ~self in
			assert_resident_on ~__context ~self:vm;
			let vif = md_of_vif ~__context ~self in
			info "xenops: VIF.eject %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
			let dbg = Context.string_of_task __context in
			(try Client.VIF.remove dbg vif.Vif.id with Does_not_exist _ -> ());
			info "xenops: VIF.add %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
			with_networks_attached ~__context ~self:vm (fun () ->
				let id = Client.VIF.add dbg vif in
				info "xenops: VIF.plug %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
				Client.VIF.plug dbg id |> sync_with_task __context;
				Event.wait dbg ();
				assert (Db.VIF.get_currently_attached ~__context ~self))
		)

let vm_set_vm_data ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			()
		)

let vif_set_locking_mode ~__context ~self =
	transform_xenops_exn ~__context
		(fun () ->
			let vm = Db.VIF.get_VM ~__context ~self in
			assert_resident_on ~__context ~self:vm;
			let vif = md_of_vif ~__context ~self in
			info "xenops: VIF.set_locking_mode %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
			let dbg = Context.string_of_task __context in
			Client.VIF.set_locking_mode dbg vif.Vif.id vif.Vif.locking_mode |> sync_with_task __context;
			Event.wait dbg ();
		)

let vif_unplug ~__context ~self force =
	transform_xenops_exn ~__context
		(fun () ->
			let vm = Db.VIF.get_VM ~__context ~self in
			assert_resident_on ~__context ~self:vm;
			let vif = md_of_vif ~__context ~self in
			info "xenops: VIF.unplug %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
			let dbg = Context.string_of_task __context in
			Client.VIF.unplug dbg vif.Vif.id force |> sync_with_task __context;
			(* We need to make sure VIF.stat still works so: wait before calling VIF.remove *)
			Event.wait dbg ();
			info "xenops: VIF.remove %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
			Client.VIF.remove dbg vif.Vif.id;
			assert (not(Db.VIF.get_currently_attached ~__context ~self))
		)

let vif_move ~__context ~self network =
	transform_xenops_exn ~__context
		(fun () ->
			let vm = Db.VIF.get_VM ~__context ~self in
			assert_resident_on ~__context ~self:vm;
			let vif = md_of_vif ~__context ~self in
			info "xenops: VIF.move %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
			let network = Db.Network.get_record ~__context ~self:network in
			let backend = backend_of_network network in
			let dbg = Context.string_of_task __context in
			(* Nb., at this point, the database shows the vif on the new network *)
			with_networks_attached ~__context ~self:vm (fun () -> 
				Client.VIF.move dbg vif.Vif.id backend |> sync_with_task __context;
				Event.wait dbg ();
				assert (Db.VIF.get_currently_attached ~__context ~self))
		)

let task_cancel ~__context ~self =
	try
		let id = task_to_id_exn self in
		let dbg = Context.string_of_task __context in
		info "xenops: TASK.cancel %s" id;
		Client.TASK.cancel dbg id |> ignore; (* it might actually have completed, we don't care *)
		true
	with Not_found ->
		false
