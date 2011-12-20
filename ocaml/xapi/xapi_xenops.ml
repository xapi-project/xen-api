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
open Fun
module XenAPI = Client.Client
open Xenops_interface

(* This is only used to block the 'present multiple physical cores as one big hyperthreaded core' feature *)
let filtered_platform_flags = ["acpi"; "apic"; "nx"; "pae"; "viridian";
                               "acpi_s3";"acpi_s4"; "mmio_size_mib"]

let disk_of_vdi ~__context ~self =
	try
		let vdi = Db.VDI.get_record ~__context ~self in
		let content_id =
			if List.mem_assoc "content_id" vdi.API.vDI_other_config
			then List.assoc "content_id" vdi.API.vDI_other_config
			else vdi.API.vDI_location (* PR-1255 *) in
		Some (VDI content_id)
	with _ -> None

let backend_of_network ~__context ~self =
	let bridge = Db.Network.get_bridge ~__context ~self in
	VSwitch bridge (* PR-1255 *)

let builder_of_vm ~__context ~vm =
	let open Vm in
	let find f map default feature =
		try f (List.assoc feature map)
		with _ -> default in
	let string = find (fun x -> x) in
	let int = find int_of_string in
	let bool = find bool_of_string in

	match Helpers.boot_method_of_vm ~__context ~vm with
		| Helpers.HVM { Helpers.timeoffset = t } -> HVM {
			hap = true;
			shadow_multiplier = vm.API.vM_HVM_shadow_multiplier;
			timeoffset = string vm.API.vM_platform "0" "timeoffset";
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
			pci_emulations = [];
			pci_passthrough = false;
			boot_order = string vm.API.vM_HVM_boot_params "cd" "order";
			qemu_disk_cmdline = false;
		}
		| Helpers.DirectPV { Helpers.kernel = k; kernel_args = ka; ramdisk = initrd } ->
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
	let of_vbd ~__context ~vm ~vbd =
		let hvm = vm.API.vM_HVM_boot_policy <> "" in
		let device_number = Device_number.of_string hvm vbd.API.vBD_userdevice in
		let open Vbd in {
			id = (vm.API.vM_uuid, Device_number.to_linux_device device_number);
			position = Some device_number;
			mode = if vbd.API.vBD_mode = `RO then ReadOnly else ReadWrite;
			backend = disk_of_vdi ~__context ~self:vbd.API.vBD_VDI;
			ty = if vbd.API.vBD_type = `Disk then Disk else CDROM;
			unpluggable = true;
			extra_backend_keys = [];
			extra_private_keys = [];
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
		let open Vif in {
			id = (vm.API.vM_uuid, vif.API.vIF_device);
			position = int_of_string vif.API.vIF_device;
			mac = vif.API.vIF_MAC;
			carrier = true;
			mtu = mtu;
			rate = None;
			backend = backend_of_network ~__context ~self:vif.API.vIF_network;
			other_config = vif.API.vIF_other_config;
			extra_private_keys = []
		}

	let of_vm ~__context ~vm =
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
		let xsdata = vm.API.vM_xenstore_data in
		(* disallowed by default; allowed only if it has one of a set of prefixes *)
		let allowed_xsdata (x, _) = List.fold_left (||) false (List.map (fun p -> String.startswith p x) [ "vm-data/"; "FIST/" ]) in
		let xsdata = List.filter allowed_xsdata xsdata in

		let platformdata =
			let p = vm.API.vM_platform in
			if not (Pool_features.is_enabled ~__context Features.No_platform_filter) then
				List.filter (fun (k, v) -> List.mem k filtered_platform_flags) p
			else p in
		{
			id = vm.API.vM_uuid;
			name = vm.API.vM_name_label;
			ssidref = 0l;
			xsdata = xsdata;
			platformdata = platformdata;
			bios_strings = vm.API.vM_bios_strings;
			ty = builder_of_vm ~__context ~vm;
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
			transient = true;
		}		


end

open Xenops_interface
open Xenops_client
open Fun

(* Create an instance of Metadata.t, suitable for uploading to the xenops service *)
let create_metadata ~__context ~self =
	let vm = Db.VM.get_record ~__context ~self in
	let vbds = List.map (fun self -> Db.VBD.get_record ~__context ~self) vm.API.vM_VBDs in
	let vifs = List.map (fun self -> Db.VIF.get_record ~__context ~self) vm.API.vM_VIFs in
	(* XXX PR-1255: need to convert between 'domains' and last_boot_record *)
	let domains =
		if vm.API.vM_power_state = `Suspended
		then Some vm.API.vM_last_booted_record
		else None in
	let open Metadata in {
		vm = MD.of_vm ~__context ~vm;
		vbds = List.map (fun vbd -> MD.of_vbd ~__context ~vm ~vbd) vbds;
		vifs = List.map (fun vif -> MD.of_vif ~__context ~vm ~vif) vifs;
		domains = domains
	}

let id_of_vm ~__context ~self = Db.VM.get_uuid ~__context ~self

(* Serialise updates to the xenopsd metadata *)
let metadata_m = Mutex.create ()

let push_metadata_to_xenopsd_locked ~__context ~self =
	let txt = create_metadata ~__context ~self |> Metadata.rpc_of_t |> Jsonrpc.to_string in
	Client.VM.import_metadata txt |> success

let push_metadata_to_xenopsd ~__context ~self =
	Mutex.execute metadata_m (fun () -> push_metadata_to_xenopsd_locked ~__context ~self)

let pull_metadata_from_xenopsd id =
	Mutex.execute metadata_m
		(fun () ->
			let md = Client.VM.export_metadata id |> success |> Jsonrpc.of_string |> Metadata.t_of_rpc in
			Client.VM.remove id |> success;
			md)

let update_metadata_in_xenopsd ~__context ~self =
	let id = id_of_vm ~__context ~self in
	Mutex.execute metadata_m
		(fun () ->
			let all = Client.VM.list () |> success |> List.map (fun (x, _) -> x.Vm.id) in
			if List.mem id all
			then push_metadata_to_xenopsd_locked ~__context ~self |> ignore)

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
		error "Caught %s while detaching networks" (Printexc.to_string e)

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
	let wait () =
		let t = make () in
		let id = register t in
		Client.UPDATES.inject_barrier id |> success;
		Mutex.execute t.m
			(fun () ->
				while not t.finished do Condition.wait t.c t.m done
			)
	let wakeup id =
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

let update_vm ~__context id info =
	try
		let open Vm in
		let self = Db.VM.get_by_uuid ~__context ~uuid:id in
		if info = None then debug "VM state missing: assuming VM has shut down";
		let power_state = match (Opt.map (fun x -> (snd x).power_state) info) with
			| Some Running -> `Running
			| Some Halted -> `Running (* reboot transient *)
			| Some Suspended -> `Suspended
			| Some Paused -> `Paused
			| None -> `Halted in
		if power_state = `Suspended || power_state = `Halted
		then detach_networks ~__context ~self;
		Db.VM.set_power_state ~__context ~self ~value:power_state;
		(* consoles *)
		Opt.iter
			(fun (_, state) ->
				if state.domids <> [] then Db.VM.set_domid ~__context ~self ~value:(List.hd state.domids |> Int64.of_int);
				let current_protocols = List.map
					(fun self -> Db.Console.get_protocol ~__context ~self |> to_xenops_console_protocol, self)
					(Db.VM.get_consoles ~__context ~self) in
				let new_protocols = List.map (fun c -> c.protocol, c) state.consoles in
				(* Destroy consoles that have gone away *)
				List.iter
					(fun protocol ->
						let self = List.assoc protocol current_protocols in
						Db.Console.destroy ~__context ~self
					) (List.set_difference (List.map fst current_protocols) (List.map fst new_protocols));
				(* Create consoles that have appeared *)
				List.iter
					(fun protocol ->
						let localhost = Helpers.get_localhost ~__context in
						let address = Db.Host.get_address ~__context ~self:localhost in
						let ref = Ref.make () in
						let uuid = Uuid.to_string (Uuid.make_uuid ()) in
						let location = Printf.sprintf "https://%s%s?uuid=%s" address Constants.console_uri uuid in
						Db.Console.create ~__context ~ref ~uuid
							~protocol:(to_xenapi_console_protocol protocol) ~location ~vM:self
							~other_config:[] ~port:(Int64.of_int (List.assoc protocol new_protocols).port)
					) (List.set_difference (List.map fst new_protocols) (List.map fst current_protocols));

				Db.VM.set_memory_target ~__context ~self ~value:state.memory_target;
				let key = "timeoffset" in
				(try Db.VM.remove_from_platform ~__context ~self ~key with _ -> ());
				Db.VM.add_to_platform ~__context ~self ~key ~value:state.rtc_timeoffset;
				List.iter
					(fun domid ->
						Mutex.execute Monitor.uncooperative_domains_m
							(fun () ->
								if state.uncooperative_balloon_driver
								then Hashtbl.replace Monitor.uncooperative_domains domid ()
								else Hashtbl.remove Monitor.uncooperative_domains domid
							);
						let lookup key =
							if List.mem_assoc key state.guest_agent then Some (List.assoc key state.guest_agent) else None in
						let list dir =
							List.map snd (List.filter (fun x -> String.startswith dir (fst x)) state.guest_agent) in
						Xapi_guest_agent.all lookup list ~__context ~domid ~uuid:id
					) state.domids;
				let metrics = Db.VM.get_metrics ~__context ~self in
				Db.VM_metrics.set_start_time ~__context ~self:metrics ~value:(Date.of_float state.last_start_time)
			) info;
		Xapi_vm_lifecycle.update_allowed_operations ~__context ~self;
	with e ->
		error "Caught %s while updating VM: has this VM been removed while this host is offline?" (Printexc.to_string e)

let update_vbd ~__context id info =
	try
		let open Vbd in
		let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
		let vbds = Db.VM.get_VBDs ~__context ~self:vm in
		let vbdrs = List.map (fun self -> self, Db.VBD.get_record ~__context ~self) vbds in
		let linux_device = snd id in
		let disk_number = Device_number.of_linux_device (snd id) |> Device_number.to_disk_number |> string_of_int in
		debug "VM %s VBD userdevices = [ %s ]" (fst id) (String.concat "; " (List.map (fun (_,r) -> r.API.vBD_userdevice) vbdrs));
		let vbd, vbd_r = List.find (fun (_, vbdr) -> vbdr.API.vBD_userdevice = linux_device || vbdr.API.vBD_userdevice = disk_number) vbdrs in
		Opt.iter
			(fun (x, state) ->
				Db.VBD.set_device ~__context ~self:vbd ~value:linux_device;
				Db.VBD.set_currently_attached ~__context ~self:vbd ~value:state.plugged;
				debug "state.media_present = %b" state.media_present;
				if state.plugged then begin
					if state.media_present then begin
						(* XXX PR-1255: I need to know the actual SR and VDI in use, not the content requested *)
						match x.backend with
							| Some (VDI x) ->
								let vdi, _ = Storage_access.find_content ~__context x in
								Db.VBD.set_VDI ~__context ~self:vbd ~value:vdi;
								Db.VBD.set_empty ~__context ~self:vbd ~value:false
							| _ ->
								error "I don't know what to do with this kind of VDI backend"
					end else if vbd_r.API.vBD_type = `CD then begin
						Db.VBD.set_empty ~__context ~self:vbd ~value:true;
						Db.VBD.set_VDI ~__context ~self:vbd ~value:Ref.null
					end
				end
			) info;
		Xapi_vbd_helpers.update_allowed_operations ~__context ~self:vbd
	with e ->
		error "Caught %s while updating VBD" (Printexc.to_string e)

let update_vif ~__context id info =
	try
		let open Vif in
		let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst id) in
		let vifs = Db.VM.get_VIFs ~__context ~self:vm in
		let vifrs = List.map (fun self -> self, Db.VIF.get_record ~__context ~self) vifs in
		let vif, _ = List.find (fun (_, vifr) -> vifr.API.vIF_device = (snd id)) vifrs in
		Opt.iter
			(fun (_, state) ->
				if not state.plugged
				then Xapi_network.deregister_vif ~__context vif;
				Db.VIF.set_currently_attached ~__context ~self:vif ~value:state.plugged
			) info;
		Xapi_vif_helpers.update_allowed_operations ~__context ~self:vif
	with e ->
		error "Caught %s while updating VIF" (Printexc.to_string e)

let rec events_watch ~__context from =
	let events, next = Client.UPDATES.get from None |> success in
	let open Dynamic in
	List.iter
		(function
			| Vm_t(id, info) ->
				debug "xenops event on VM %s" id;
				update_vm ~__context id info;
			| Vbd_t(id, info) ->
				debug "xenops event on VBD %s.%s" (fst id) (snd id);
				update_vbd ~__context id info
			| Vif_t(id, info) ->
				debug "xenops event on VIF %s.%s" (fst id) (snd id);
				update_vif ~__context id info
			| Task_t(id, info) ->
				debug "xenops event on Task %s" id
			| Barrier_t id ->
				debug "barrier %d" id;
				Event.wakeup id
		) events;
	events_watch ~__context next

let events_from_xenopsd () =
    Server_helpers.exec_with_new_task "xapi_xenops"
		(fun __context ->
			(* For each VM resident on this host, check if the xenopsd
			   has forgotten about it: this means it has shut down *)
			let localhost = Helpers.get_localhost ~__context in
			let vms = Db.Host.get_resident_VMs ~__context ~self:localhost in
			let vms = List.filter (fun vm -> not(Db.VM.get_is_control_domain ~__context ~self:vm)) vms in
			let in_db = List.map (fun self -> Db.VM.get_uuid ~__context ~self) vms in
			let in_xenopsd = Client.VM.list () |> success |> List.map (fun (vm, _) -> vm.Vm.id) in
			List.iter
				(fun id ->
					info "VM %s is not running here: setting power_state to Halted" id;
					let vm = Db.VM.get_by_uuid ~__context ~uuid:id in
					Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Halted
				) (List.set_difference in_db in_xenopsd);
			List.iter
				(fun id ->
					info "VM %s is running here: setting power_state to Running" id;
					let vm = Db.VM.get_by_uuid ~__context ~uuid:id in
					Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value:`Running;
					Db.VM.set_resident_on ~__context ~self:vm ~value:localhost
				) (List.set_difference in_db in_xenopsd);
			(* Tell xenopsd to manage domain 0 *)
			let uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
			let open Vm in
			if not(List.mem uuid in_xenopsd)
			then Client.VM.add {
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
				transient = false
			} |> ignore;
			while true do
				try
					events_watch ~__context None;
				with e ->
					error "event thread caught: %s" (Printexc.to_string e);
					Thread.delay 10.
			done
		)


(* XXX: PR-1255: this will be receiving too many events and we may wish to synchronise
   updates to the VM metadata and resident_on fields *)
let events_from_xapi () =
	let open Event_types in
    Server_helpers.exec_with_new_task "xapi_xenops"
		(fun __context ->
			let localhost = Helpers.get_localhost ~__context in
			while true do
				try
					Helpers.call_api_functions ~__context
						(fun rpc session_id ->
							XenAPI.Event.register ~rpc ~session_id ~classes:["VM"];
							(* In case we miss events, update all VM metadata *)
							List.iter
								(fun vm ->
									info "VM %s might have changed: updating xenopsd metadata" (Ref.string_of vm);
									update_metadata_in_xenopsd ~__context ~self:vm |> ignore
								) (Db.Host.get_resident_VMs ~__context ~self:localhost);
							while true do
								let events = events_of_xmlrpc (XenAPI.Event.next ~rpc ~session_id) in
								List.iter
									(function
										| { ty = "vm"; reference = vm' } ->
											let vm = Ref.of_string vm' in
											if Db.VM.get_resident_on ~__context ~self:vm = localhost then begin
												info "VM %s has changed: updating xenopsd metadata" vm';
												update_metadata_in_xenopsd ~__context ~self:vm |> ignore
											end
										| _ -> ()
									) events
							done
						)
				with e ->
					debug "Caught %s listening to events from xapi" (Printexc.to_string e);
					Thread.delay 15.
			done
		)

let success_task id =
	let t = Client.TASK.stat id |> success in
	match t.Task.result with
	| Task.Completed _ -> t
	| Task.Failed (Failed_to_contact_remote_service x) ->
		failwith (Printf.sprintf "Failed to contact remote service on: %s\n" x)
	| Task.Failed x -> failwith (Jsonrpc.to_string (rpc_of_error x))
	| Task.Pending _ -> failwith "task pending"

let start ~__context ~self paused =
	let id = push_metadata_to_xenopsd ~__context ~self in
	attach_networks ~__context ~self;
	Client.VM.start id |> success |> wait_for_task |> success_task |> ignore_task;
	if not paused
	then Client.VM.unpause id |> success |> wait_for_task |> success_task |> ignore_task;
	Event.wait ();
	assert (Db.VM.get_power_state ~__context ~self = (if paused then `Paused else `Running))

let reboot ~__context ~self timeout =
	let id = id_of_vm ~__context ~self in
	Client.VM.reboot id timeout |> success |> wait_for_task |> success_task |> ignore_task;
	Event.wait ();
	assert (Db.VM.get_power_state ~__context ~self = `Running)

let shutdown ~__context ~self timeout =
	let id = id_of_vm ~__context ~self in
	Client.VM.shutdown id timeout |> success |> wait_for_task |> success_task |> ignore_task;
	Event.wait ();
	assert (Db.VM.get_power_state ~__context ~self = `Halted)

let suspend ~__context ~self =
	let id = id_of_vm ~__context ~self in
	let vm_t, state = Client.VM.stat id |> success in
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
				Client.VM.suspend id disk |> success |> wait_for_task |> success_task |> ignore_task;
				Event.wait ();
				assert (Db.VM.get_power_state ~__context ~self = `Suspended);
				(* XXX PR-1255: need to convert between 'domains' and lbr *)
				let md = pull_metadata_from_xenopsd id in
				match md.Metadata.domains with
					| None ->
						failwith "Suspended VM has no domain-specific metadata"
					| Some x ->
						Db.VM.set_last_booted_record ~__context ~self ~value:x;
						debug "VM %s last_booted_record set to %s" (Ref.string_of self) x
			with e ->
				debug "Caught exception suspending VM: %s" (Printexc.to_string e);
				XenAPI.VDI.destroy ~rpc ~session_id ~self:vdi;
				Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null;
				raise e
		)

let resume ~__context ~self ~start_paused ~force =
	let vdi = Db.VM.get_suspend_VDI ~__context ~self in
	let disk = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in	
	let id = push_metadata_to_xenopsd ~__context ~self in
	attach_networks ~__context ~self;
	Client.VM.resume id disk |> success |> wait_for_task |> success_task |> ignore_task;
	if not start_paused
	then Client.VM.unpause id |> success |> wait_for_task |> success_task |> ignore_task;
	Event.wait ();
	assert (Db.VM.get_power_state ~__context ~self = if start_paused then `Paused else `Running);
	Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			XenAPI.VDI.destroy rpc session_id vdi
		);
	Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null

let md_of_vbd ~__context ~self =
	let vm = Db.VBD.get_VM ~__context ~self in
	MD.of_vbd ~__context ~vm:(Db.VM.get_record ~__context ~self:vm) ~vbd:(Db.VBD.get_record ~__context ~self)

let vbd_eject ~__context ~self =
	let vbd = md_of_vbd ~__context ~self in
	Client.VBD.eject vbd.Vbd.id |> success |> wait_for_task |> success_task |> ignore_task;
	Event.wait ();
	assert (Db.VBD.get_empty ~__context ~self);
	assert (Db.VBD.get_VDI ~__context ~self = Ref.null)


let vbd_insert ~__context ~self ~vdi =
	let vbd = md_of_vbd ~__context ~self in
	let disk = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in
	Client.VBD.insert vbd.Vbd.id disk |> success |> wait_for_task |> success_task |> ignore_task;
	Event.wait ();
	assert (not(Db.VBD.get_empty ~__context ~self));
	assert (Db.VBD.get_VDI ~__context ~self = vdi)

let vbd_plug ~__context ~self =
	let vbd = md_of_vbd ~__context ~self in
	Client.VBD.remove vbd.Vbd.id |> might_not_exist;
	let id = Client.VBD.add vbd |> success in
	Client.VBD.plug id |> success |> wait_for_task |> success_task |> ignore_task;
	Event.wait ();
	assert (Db.VBD.get_currently_attached ~__context ~self)

let vbd_unplug ~__context ~self force =
	let vbd = md_of_vbd ~__context ~self in
	Client.VBD.unplug vbd.Vbd.id force |> success |> wait_for_task |> success_task |> ignore_task;
	Client.VBD.remove vbd.Vbd.id |> success;
	Event.wait ();
	assert (not(Db.VBD.get_currently_attached ~__context ~self))

let md_of_vif ~__context ~self =
	let vm = Db.VIF.get_VM ~__context ~self in
	MD.of_vif ~__context ~vm:(Db.VM.get_record ~__context ~self:vm) ~vif:(Db.VIF.get_record ~__context ~self)

let vif_plug ~__context ~self =
	let vif = md_of_vif ~__context ~self in
	Client.VIF.remove vif.Vif.id |> might_not_exist;
	let id = Client.VIF.add vif |> success in
	Client.VIF.plug id |> success |> wait_for_task |> success_task |> ignore_task;
	Event.wait ();
	assert (Db.VIF.get_currently_attached ~__context ~self)

let vif_unplug ~__context ~self force =
	let vif = md_of_vif ~__context ~self in
	Client.VIF.unplug vif.Vif.id force |> success |> wait_for_task |> success_task |> ignore_task;
	Client.VIF.remove vif.Vif.id |> success;
	Event.wait ();
	assert (not(Db.VIF.get_currently_attached ~__context ~self))


