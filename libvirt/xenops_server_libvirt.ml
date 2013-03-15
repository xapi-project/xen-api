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
open Xenops_interface
open Xenops_server_plugin
open Xenops_utils
open Xenops_task

module D = Debug.Make(struct let name = "xenops_server_libvirt" end)
open D

module Domain = struct
	type t = {
		vm: Vm.t;
		vbds: Vbd.t list; (* maintained in reverse-plug order *)
		attach_infos: (Vbd.id * Storage_interface.attach_info option) list;
		vifs: Vif.t list;
		active_vbds: Vbd.id list;
		active_vifs: Vif.id list;
		pcis: Pci.t list;
		last_create_time: float;
	} with rpc

        module To_xml = struct
		let tag_start ?(attr=[]) key output = Xmlm.output output (`El_start (("", key), List.map (fun (x, y) -> ("", x), y) attr))
		let tag_end output = Xmlm.output output `El_end
		let data x output = Xmlm.output output (`Data x)
		let string key v output =
			tag_start key output;
			data v output;
			tag_end output
		let empty ?(attr=[]) key output =
			tag_start ~attr key output;
			tag_end output

		let name = string "name"
		let uuid = string "uuid"
		let memory bytes = string "memory" (Int64.(to_string (div bytes 1024L)))
		let vcpu n = string "vcpu" (string_of_int n)
		let emulator = string "emulator"
		let bootloader = string "bootloader"

		open Vm
		let action x = match x with
		| [ Shutdown ] -> "destroy"
		| [ Shutdown; Start ] -> "restart"
		| [ Pause ] -> "preserve"
		| _ -> "destroy"
		let platform key x =
			let x = x.platformdata in
			List.mem_assoc key x && (let x = List.assoc key x in x = "1" || String.lowercase x = "true")

		let os bi output = match bi with
		| HVM hvm_info ->
			tag_start "os" output;
			string "type" "hvm" output;
			string "loader" !Path.hvmloader output;
			tag_start ~attr:["dev", "hd"] "boot" output;
			tag_end output;
			tag_end output
		| PV { boot = Indirect { bootloader = b } } ->
			bootloader b output;
			tag_start "os" output;
			string "type" "linux" output;
			tag_end output
		| _ -> failwith "boot type"

		let xen output x =
			let open Vm in
			tag_start ~attr:["type", "xen"] "domain" output;
			name x.vm.name output;
			uuid x.vm.id output;
			os x.vm.ty output;
			memory x.vm.memory_static_max output;
			vcpu x.vm.vcpus output;
			string "on_shutdown" (action x.vm.on_shutdown) output;
			string "on_reboot" (action x.vm.on_reboot) output;
			string "on_crash" (action x.vm.on_crash) output;
			tag_start "features" output;
			if platform "pae" x.vm then empty "pae" output;
			if platform "acpi" x.vm then empty "acpi" output;
			if platform "apic" x.vm then empty "apic" output;	
			tag_end output;
			empty ~attr:["sync", "localtime"] "clock" output;
			tag_start "devices" output;
			emulator !Path.qemu_dm_wrapper output;
			List.iter
				(fun vif ->
					let bridge = match vif.Vif.backend with
					| Network.Local x -> x
					| Network.Remote _ -> failwith "Network.Remote" in
					tag_start ~attr:["type", "bridge"] "interface" output;
					empty ~attr:["bridge", bridge] "source" output;
					empty ~attr:["address", vif.Vif.mac] "mac" output;
					empty ~attr:["path", !Path.vif_script] "script" output;
					tag_end output;
				) x.vifs;
			List.iter
				(fun vbd ->
					let disk_opt =
						if List.mem_assoc vbd.Vbd.id x.attach_infos
						then List.assoc vbd.Vbd.id x.attach_infos
						else None in
					let virtual_media_type = match vbd.Vbd.ty with
					| Vbd.CDROM -> ["device", "cdrom"]
					| Vbd.Disk  -> [] in
					let physical_media_type = [ "type", "file" ] in
					let attr = virtual_media_type @ physical_media_type in
					tag_start ~attr "disk" output;
					begin match disk_opt with
					| Some x -> empty ~attr:["file", x.Storage_interface.params] "source" output
					| None -> ()
					end;
					let linux = match vbd.Vbd.position with
					| None -> failwith "unresolved disk position"
					| Some p -> Device_number.to_linux_device p in
					let attr = [ "dev", linux ] in
					empty ~attr "target" output;
					if vbd.Vbd.mode = Vbd.ReadOnly then empty "readonly" output;

					tag_end output;
				) x.vbds;
			let attr = [ "type", "vnc"; "port", "5904" ] in
			empty ~attr "graphics" output;
			tag_end output;
			tag_end output

	end

	let to_xml (x: t) = assert false

end

module DB = TypedTable(struct
	include Domain
	let namespace = "domain"
	type key = string
	let key x = [ x ]
end)

let updates = Updates.empty ()

let mib = Int64.mul 1024L 1024L

module HOST = struct
	include Xenops_server_skeleton.HOST

	let stat () = {
		Host.nr_cpus = 0;
		cpu_info = {
			Host.vendor = "unknown";
			speed = "";
			modelname = "";
			family = "";
			model = "";
			stepping = "";
			flags = "";
			features = "";
			features_after_reboot = "";
			physical_features = "";
			maskable = "";
		};
		hypervisor = {
			Host.name = "unknown";
			version = "";
			capabilities = "hvm";
		}
	}


	let get_console_data () = "should run 'dmesg' here"
	let get_total_memory_mib () = Int64.mul 1024L 1024L
	let send_debug_keys _ = ()
end

module VM = struct
	include Xenops_server_skeleton.VM

	let create _ memory_limit vm =
		debug "Domain.create vm=%s" vm.Vm.id;
		(* Idempotent *)
		if DB.exists vm.Vm.id then DB.delete vm.Vm.id;
		let open Domain in
		let domain = {
			vm = vm;
			vifs = [];
			vbds = [];
			active_vifs = [];
			active_vbds = [];
			attach_infos = [];
			pcis = [];
			last_create_time = Unix.gettimeofday ();
		} in
		DB.write vm.Vm.id domain

	let destroy _ vm =
		debug "Domain.destroy vm=%s" vm.Vm.id;
		(* Idempotent *)
		match DB.read vm.Vm.id with
		| Some d ->
			DB.delete vm.Vm.id
		| None -> ()

	let unpause _ vm =
		Updates.add (Dynamic.Vm vm.Vm.id) updates

	let build _ vm vbds vifs = ()
	let create_device_model _ vm vbds vifs _ = ()
	let destroy_device_model _ vm = ()
	let request_shutdown task vm reason ack_delay =
		debug "pushing ACPI power button";
		true
	let wait_shutdown task vm reason timeout =
		false

	let get_state vm =
		halted_vm

	let set_domain_action_request vm request = ()
	let get_domain_action_request vm =
		None	
	let minimum_reboot_delay = 0.
end

module PCI = struct
	include Xenops_server_skeleton.PCI

	let plug _ (vm: Vm.id) (pci: Pci.t) =
		debug "add_pci";
		let d = DB.read_exn vm in
		let existing_positions = List.map (fun pci -> pci.Pci.position) d.Domain.pcis in
		if List.mem pci.Pci.position existing_positions then begin
			debug "PCI.plug %s.%s: Already exists" (fst pci.Pci.id) (snd pci.Pci.id);
			raise (Already_exists("pci", string_of_int pci.Pci.position))
		end else DB.write vm { d with Domain.pcis = pci :: d.Domain.pcis }

	let unplug _ vm pci =
		let d = DB.read_exn vm in
		let this_one x = x.Pci.id = pci.Pci.id in
		if List.filter this_one d.Domain.pcis = []
		then raise (Does_not_exist("PCI", Printf.sprintf "%s.%s" (fst pci.Pci.id) (snd pci.Pci.id)))
		else DB.write vm { d with Domain.pcis = List.filter (fun x -> not (this_one x)) d.Domain.pcis }

	let get_state vm pci = unplugged_pci

	let get_device_action_request vm pci = None
end

module VBD = struct
	include Xenops_server_skeleton.VBD

	let dp_of domain vbd = Storage.id_of domain.Domain.vm.Vm.id vbd.Vbd.id

	let attach_and_activate task dp vbd = match vbd.Vbd.backend with
	| None ->
		None
	| Some (Local path) ->
		Some { Storage_interface.params=path; xenstore_data=[]; }
	| Some (VDI path) ->
		let sr, vdi = Storage.get_disk_by_name task path in
		let vm = fst vbd.Vbd.id in
		Some (Storage.attach_and_activate task vm dp sr vdi (vbd.Vbd.mode = Vbd.ReadWrite))

	let set_active task (vm: Vm.id) (vbd: Vbd.t) setting =
		info "VBD.set_active %s.%s %b" (fst vbd.Vbd.id) (snd vbd.Vbd.id) setting;
		match DB.read vm with
		| Some d ->
			let d = DB.read_exn vm in
			let active_vbds = List.filter (fun x -> x <> vbd.Vbd.id) d.Domain.active_vbds in
			let active_vbds' = if setting then vbd.Vbd.id :: active_vbds else active_vbds in
			DB.write vm { d with Domain.active_vbds = active_vbds' }
		| None ->
			info "ignoring because domain has been destroyed"

	let plug task (vm: Vm.id) (vbd: Vbd.t) =
		info "VBD.plug %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
		let d = DB.read_exn vm in
		(* there shouldn't be any None values in here anyway *)
		let ps = List.map (fun vbd -> vbd.Vbd.position) d.Domain.vbds in
		assert (not (List.mem None ps));
		let dns = List.map (Opt.unbox) ps in
		let indices = List.map Device_number.to_disk_number dns in
		let next_index = List.fold_left max (-1) indices + 1 in
		let next_dn = Device_number.of_disk_number true next_index in
		let this_dn = Opt.default next_dn vbd.Vbd.position in
		if List.mem this_dn dns then begin
			debug "VBD.plug %s.%s: Already exists" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
			raise (Already_exists("vbd", Device_number.to_debug_string this_dn))
		end else begin
			let dp = dp_of d vbd in
			let attach_info = attach_and_activate task dp vbd in
			DB.write vm { d with
				Domain.vbds = { vbd with Vbd.position = Some this_dn } :: d.Domain.vbds;
				attach_infos = (vbd.Vbd.id, attach_info) :: d.Domain.attach_infos;
			}
		end
	let unplug task vm vbd _ =
		info "VBD.unplug %s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
		let this_one x = x.Vbd.id = vbd.Vbd.id in
		let d = DB.read_exn vm in
		let dp = dp_of d vbd in
		Storage.dp_destroy task dp;
		DB.write vm { d with
			Domain.vbds = List.filter (fun x -> not (this_one x)) d.Domain.vbds;
			attach_infos = List.filter (fun (x, _) -> x <> vbd.Vbd.id) d.Domain.attach_infos;
		}

	let insert _ vm vbd disk = ()
	let eject _ vm vbd = ()

	let get_state vm vbd = {
		Vbd.active = true;
		plugged = true;
		backend_present = vbd.Vbd.backend;
		qos_target = None
	}

	let get_device_action_request vm vbd = None
end

module VIF = struct
	include Xenops_server_skeleton.VIF

	let set_active task (vm: Vm.id) (vif: Vif.t) setting =
		info "VIF.set_active %s.%s %b" (fst vif.Vif.id) (snd vif.Vif.id) setting;
		match DB.read vm with
		| Some d ->
			let active_vifs = List.filter (fun x -> x <> vif.Vif.id) d.Domain.active_vifs in
			let active_vifs' = if setting then vif.Vif.id :: active_vifs else active_vifs in
			DB.write vm { d with Domain.active_vifs = active_vifs' }
		| None ->
			info "ignoring because domain has been destroyed"

	let plug _ vm vif =
		info "VIF.plug %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
		let d = DB.read_exn vm in
		let existing_positions = List.map (fun vif -> vif.Vif.position) d.Domain.vifs in
		if List.mem vif.Vif.position existing_positions then begin
			debug "VIF.plug %s.%s: Already exists" (fst vif.Vif.id) (snd vif.Vif.id);
			raise (Already_exists("vif", string_of_int vif.Vif.position))
		end else DB.write vm { d with Domain.vifs = vif :: d.Domain.vifs }

	let unplug _ vm vif _ =
		info "VIF.unplug %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
		let d = DB.read_exn vm in
		let this_one x = x.Vif.id = vif.Vif.id in
		DB.write vm { d with Domain.vifs = List.filter (fun x -> not (this_one x)) d.Domain.vifs }

	let get_state vm vif = {
		Vif.active = true;
		plugged = true;
		media_present = true;
		kthread_pid = 0;
	}

	let get_device_action_request vm vif = None
end

module UPDATES = struct
	let get last timeout = Updates.get "UPDATES.get" last timeout updates
end

module DEBUG = struct
	include Xenops_server_skeleton.DEBUG
end

let init () =
	()
