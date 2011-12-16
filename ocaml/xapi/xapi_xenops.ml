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
module XenAPI = Client.Client
open Xenops_interface

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
		let open Vif in {
			id = (vm.API.vM_uuid, vif.API.vIF_device);
			position = int_of_string vif.API.vIF_device;
			mac = vif.API.vIF_MAC;
			carrier = true;
			mtu = Int64.to_int vif.API.vIF_MTU;
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
		let open Vm in {
			id = vm.API.vM_uuid;
			name = vm.API.vM_name_label;
			ssidref = 0l;
			xsdata = [];
			platformdata = vm.API.vM_platform;
			bios_strings = [];
			ty = builder_of_vm ~__context ~vm;
			suppress_spurious_page_faults = (try List.assoc "suppress-spurious-page-faults" vm.API.vM_other_config = "true" with _ -> false);
			machine_address_size = (try Some(int_of_string (List.assoc "machine-address-size" vm.API.vM_other_config)) with _ -> None);
			memory_static_max = vm.API.vM_memory_static_max;
			memory_dynamic_max = vm.API.vM_memory_dynamic_max;
			memory_dynamic_min = vm.API.vM_memory_dynamic_min;
			vcpus = Int64.to_int vm.API.vM_VCPUs_max;
			on_crash = on_crash_behaviour vm.API.vM_actions_after_crash;
			on_shutdown = on_normal_exit_behaviour vm.API.vM_actions_after_shutdown;
			on_reboot = on_normal_exit_behaviour vm.API.vM_actions_after_reboot;
			transient = true;
		}		
end

(* Create an instance of Metadata.t, suitable for uploading to the xenops service *)
let create_metadata ~__context ~self =
	let vm = Db.VM.get_record ~__context ~self in
	let vbds = List.map (fun self -> Db.VBD.get_record ~__context ~self) vm.API.vM_VBDs in
	let vifs = List.map (fun self -> Db.VIF.get_record ~__context ~self) vm.API.vM_VIFs in
	let open Metadata in {
		vm = MD.of_vm ~__context ~vm;
		vbds = List.map (fun vbd -> MD.of_vbd ~__context ~vm ~vbd) vbds;
		vifs = List.map (fun vif -> MD.of_vif ~__context ~vm ~vif) vifs;
		domains = None
	}

open Xenops_interface
open Xenops_client
open Fun

let to_xenops_console_protocol = let open Vm in function
	| `rfb -> Rfb
	| `vt100 -> Vt100
	| `rdp -> Rfb (* RDP was never used in the XenAPI so this never happens *)
let to_xenapi_console_protocol = let open Vm in function
	| Rfb -> `rfb
	| Vt100 -> `vt100

let update_vm ~__context id info =
	try
		let open Vm in
		let self = Db.VM.get_by_uuid ~__context ~uuid:id in
		if info = None then debug "VM state missing: assuming VM has shut down";
		let power_state = Opt.default Halted (Opt.map (fun x -> (snd x).power_state) info) in
		Db.VM.set_power_state ~__context ~self ~value:(match power_state with
			| Running -> `Running
			| Halted -> `Halted
			| Suspended -> `Suspended
			| Paused -> `Paused
		);
		(* consoles *)
		Opt.iter
			(fun (_, state) ->
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
		) events;
	events_watch ~__context next

let events_thread () =
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
			while true do
				try
					events_watch ~__context None;
				with e ->
					error "event thread caught: %s" (Printexc.to_string e);
					Thread.delay 10.
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
	let txt = create_metadata ~__context ~self |> Metadata.rpc_of_t |> Jsonrpc.to_string in
	let id = Client.VM.import_metadata txt |> success in
	Client.VM.start id |> success |> wait_for_task |> success_task |> ignore_task;
	if not paused
	then Client.VM.unpause id |> success |> wait_for_task |> success_task |> ignore_task

let id_of_vm ~__context ~self = Db.VM.get_uuid ~__context ~self

let reboot ~__context ~self timeout =
	let id = id_of_vm ~__context ~self in
	Client.VM.reboot id timeout |> success |> wait_for_task |> success_task |> ignore_task

let shutdown ~__context ~self timeout =
	let id = id_of_vm ~__context ~self in
	Client.VM.shutdown id timeout |> success |> wait_for_task |> success_task |> ignore_task

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
				Client.VM.suspend id disk |> success |> wait_for_task |> success_task |> ignore_task
			with e ->
				debug "Caught exception suspending VM: %s" (Printexc.to_string e);
				XenAPI.VDI.destroy ~rpc ~session_id ~self:vdi;
				Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null;
				raise e
		)

let resume ~__context ~self ~start_paused ~force =
	let vdi = Db.VM.get_suspend_VDI ~__context ~self in
	let disk = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in	
	let id = id_of_vm ~__context ~self in
	Client.VM.resume id disk |> success |> wait_for_task |> success_task |> ignore_task;
	if not start_paused
	then Client.VM.unpause id |> success |> wait_for_task |> success_task |> ignore_task;
	Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			XenAPI.VDI.destroy rpc session_id vdi
		);
	Db.VM.set_suspend_VDI ~__context ~self ~value:Ref.null

let id_of_vbd ~__context ~self =
	let vm = Db.VBD.get_VM ~__context ~self in
	let vbd = MD.of_vbd ~__context ~vm:(Db.VM.get_record ~__context ~self:vm) ~vbd:(Db.VBD.get_record ~__context ~self) in
	vbd.Vbd.id

let vbd_eject ~__context ~self =
	let id = id_of_vbd ~__context ~self in
	Client.VBD.eject id |> success |> wait_for_task |> success_task |> ignore_task

let vbd_insert ~__context ~self ~vdi =
	let id = id_of_vbd ~__context ~self in
	let disk = disk_of_vdi ~__context ~self:vdi |> Opt.unbox in
	Client.VBD.insert id disk |> success |> wait_for_task |> success_task |> ignore_task
