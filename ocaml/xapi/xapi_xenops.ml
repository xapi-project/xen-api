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
				framebuffer = false
			}
		| Helpers.IndirectPV { Helpers.bootloader = b; extra_args = e; legacy_args = l; pv_bootloader_args = p; vdis = vdis } ->
			PV {
				boot = Indirect { bootloader = b; extra_args = e; legacy_args = l; bootloader_args = p; devices = List.filter_map (fun x -> disk_of_vdi ~__context ~self:x) vdis };
				framebuffer = false
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

let update_vm ~__context x state =
	try
		let open Vm in
		let self = Db.VM.get_by_uuid ~__context ~uuid:x.id in
		Db.VM.set_power_state ~__context ~self ~value:(match state.power_state with
			| Running -> `Running
			| Halted -> `Halted
			| Suspended -> `Suspended
			| Paused -> `Paused
		);
		(* consoles *)
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
				Xapi_guest_agent.all lookup list ~__context ~domid ~uuid:x.id
			) state.domids;
		let metrics = Db.VM.get_metrics ~__context ~self in
		Db.VM_metrics.set_start_time ~__context ~self:metrics ~value:(Date.of_float state.last_start_time);
	with e ->
		error "Caught %s while updating VM: has this VM been removed while this host is offline?" (Printexc.to_string e)

let update_vbd ~__context x state =
	try
		let open Vbd in
		let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst x.id) in
		let vbds = Db.VM.get_VBDs ~__context ~self:vm in
		let vbdrs = List.map (fun self -> self, Db.VBD.get_record ~__context ~self) vbds in
		let linux_device = snd x.id in
		let disk_number = Device_number.of_linux_device (snd x.id) |> Device_number.to_disk_number |> string_of_int in
		debug "VM %s VBD userdevices = [ %s ]" (fst x.id) (String.concat "; " (List.map (fun (_,r) -> r.API.vBD_userdevice) vbdrs));
		let vbd, vbd_r = List.find (fun (_, vbdr) -> vbdr.API.vBD_userdevice = linux_device || vbdr.API.vBD_userdevice = disk_number) vbdrs in
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
	with e ->
		error "Caught %s while updating VBD" (Printexc.to_string e)

let update_vif ~__context x state =
	try
		let open Vif in
		let vm = Db.VM.get_by_uuid ~__context ~uuid:(fst x.id) in
		let vifs = Db.VM.get_VIFs ~__context ~self:vm in
		let vifrs = List.map (fun self -> self, Db.VIF.get_record ~__context ~self) vifs in
		let vif, _ = List.find (fun (_, vifr) -> vifr.API.vIF_device = (snd x.id)) vifrs in
		Db.VIF.set_currently_attached ~__context ~self:vif ~value:state.plugged
	with e ->
		error "Caught %s while updating VIF" (Printexc.to_string e)

let rec events_watch ~__context from =
	let events, next = Client.UPDATES.get from None |> success in
	let open Dynamic in
	List.iter
		(function
			| Vm_t(x, state) ->
				debug "xenops event on VM %s" x.Vm.name;
				update_vm ~__context x state;
			| Vbd_t(x, state) ->
				debug "xenops event on VBD %s.%s" (fst x.Vbd.id) (snd x.Vbd.id);
				update_vbd ~__context x state
			| Vif_t(x, state) ->
				debug "xenops event on VIF %s.%s" (fst x.Vif.id) (snd x.Vif.id);
				update_vif ~__context x state
			| Task_t x ->
				debug "xenops event on Task %s %s" x.Task.id (x.Task.result |> Task.rpc_of_result |> Jsonrpc.to_string)
		) events;
	events_watch ~__context next

let events_thread () =
    Server_helpers.exec_with_new_task "xapi_xenops"
		(fun __context ->
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
