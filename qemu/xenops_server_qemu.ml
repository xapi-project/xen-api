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

module D = Debug.Make(struct let name = "xenops_server_qemu" end)
open D

let simplified = false

module Domain = struct
	type t = {
		domid: int;
		qemu_pid: int option;
		uuid: string;
		vcpus: int;
		memory: int64;
		vbds: Vbd.t list; (* maintained in reverse-plug order *)
		attach_infos: (Vbd.id * Storage_interface.attach_info option) list;
		vifs: Vif.t list;
		active_vbds: Vbd.id list;
		active_vifs: Vif.id list;
		pcis: Pci.t list;
		last_create_time: float;
	} with rpc
end

module DB = TypedTable(struct
	include Domain
	let namespace = "domain"
	type key = string
	let key x = [ x ]
end)

let updates = Updates.empty ()

let mib = Int64.mul 1024L 1024L

module Qemu = struct
		let qmp_dir = Filename.concat (Xenops_utils.get_root ()) "qmp"
		let vnc_dir = Filename.concat (Xenops_utils.get_root ()) "vnc"

		let commas = String.concat ","
		let kv = List.map (fun (k, v) -> k ^ "=" ^ v)

		let of_vbd attach_infos x =
			let open Vbd in
			let file =
				if List.mem_assoc x.id attach_infos
				then match List.assoc x.id attach_infos with
					| Some x -> [ "file", x.Storage_interface.params ]
					| None -> []
				else [] in
			let intfs, media = match x.ty with
				| CDROM -> [ "ide" ], "cdrom"
				| Disk  -> [ (* "scsi"; "virtio"*) "ide" ], "disk" in
			let intfs = List.map (fun intf -> [ "if", intf ]) intfs in
			let media = [ "media", media ] in
			let format = [ "format", "raw" ] in
			let index = match x.position with
				| None -> failwith "unresolved disk position"
				| Some p -> [ "index", string_of_int (Device_number.to_disk_number p) ] in
			List.concat (List.map (fun intf ->
				let params = file @ intf @ media @ format @ index in
				[ "-drive"; commas (kv params) ]) intfs)

		let of_vif domid x =
			let open Vif in
			let vlan = [ "vlan", string_of_int x.position ] in
			let mac = [ "macaddr", x.mac ] in
			let model = [ "model", "virtio" (* "rtl8139" *) ] in
			let ifname = [ "ifname", Printf.sprintf "tap%d.%d" domid x.position ] in
			(* XXX: we need to make our vif script compatible *)
			let script = [ "script", !Qemu_path.qemu_vif_script ] in
			[ "-net"; commas ("nic" :: (kv (vlan @ mac @ model)));
			  "-net"; commas ("tap" :: (kv (vlan @ ifname @ script))) ]

		let interface_of_vif domid x =
			let name = Printf.sprintf "tap%d.%d" domid x.Vif.position in
			{ Interface.Interface.vif = x.Vif.id; name = name }

		type t = {
			cmd: string;
			args: string list;
			vnc: string; (* where to find the display *)
			qmp: string; (* where to find the QMP socket *)
			interfaces: Interface.Interface.t list;
		}

		let of_domain d =
			let open Domain in
			let memory = [ "-m"; Int64.to_string (Int64.div d.memory mib) ] in
			let vga = [ "-vga"; "std" ] in
			let vbds = List.concat (List.map (of_vbd d.attach_infos) d.vbds) in
			let vifs = List.concat (List.map (of_vif d.domid) d.vifs) in
			let interfaces = List.map (interface_of_vif d.domid) d.vifs in
			let arch = "x86_64" in
			let kvm = [ "-enable-kvm" ] in
			let boot = [ "-boot"; "cd" ] in
			let usb = [ "-usb"; "-usbdevice"; "tablet" ] in
			let vnc_path = Filename.concat vnc_dir d.uuid in
			let vnc = [ "-vnc"; Printf.sprintf "unix:%s" vnc_path ] in
			let qmp_path = Filename.concat qmp_dir d.uuid in
			let qmp = [ "-qmp"; Printf.sprintf "unix:%s,server" qmp_path ] in
			{ cmd = Printf.sprintf "/usr/bin/qemu-system-%s" arch;
			  args = memory @ vga @ vbds @ vifs @ kvm @ boot @ vnc @ qmp @ usb;
			  vnc = ":0";
			  qmp = qmp_path;
			  interfaces;
			}

		let qmp_to_uuid = Hashtbl.create 128
		let uuid_to_qmp = Hashtbl.create 128

		let monitor () =
			(* Close any currently-open QMP connections *)
			Hashtbl.iter (fun c _ -> Qmp_protocol.close c) qmp_to_uuid;
			Hashtbl.clear qmp_to_uuid;
			Hashtbl.clear uuid_to_qmp;

			let remove uuid =
				(* XXX: shutdown a qemu too? *)
				let path = Filename.concat qmp_dir uuid in
				if Hashtbl.mem uuid_to_qmp uuid then begin
					let c = Hashtbl.find uuid_to_qmp uuid in
					Hashtbl.remove qmp_to_uuid c;
					Hashtbl.remove uuid_to_qmp uuid;
					Qmp_protocol.close c
				end;
				debug "QMP %s: removing socket" uuid;
				Sys.remove path in

			let add uuid =
				debug "QMP %s: performing negotiation" uuid;
				let path = Filename.concat qmp_dir uuid in
				try
					let c = Qmp_protocol.connect path in
					Qmp_protocol.negotiate c;
					Hashtbl.replace qmp_to_uuid c uuid;
					Hashtbl.replace uuid_to_qmp uuid c;
					debug "QMP %s: negotiation complete" uuid
				with e ->
					info "QMP %s: negotiation failed (%s): removing socket" uuid (Printexc.to_string e);
					remove uuid in

			(* Monitor the QMP sockets for events *)
			while true do
				(* Look for any new sockets and negotiate *)
				let uuids = Sys.readdir qmp_dir in
				Array.iter
					(fun uuid ->
						match DB.read uuid with
						| None ->
							debug "QMP %s: no corresponding VM, removing socket" uuid;
							remove uuid
						| Some d ->
							if not(Hashtbl.mem uuid_to_qmp uuid)
							then add uuid
				) uuids;

				let cs = Hashtbl.fold (fun c _ acc -> c :: acc) qmp_to_uuid [] in
				let fds = List.map (fun x -> Qmp_protocol.to_fd x, x) cs in
				(* Re-select every 1s as a cheap way to re-scan *)
				let rs, _, _ = Unix.select (List.map fst fds) [] [] 1. in
				List.iter
					(fun fd ->
						let c = List.assoc fd fds in
						let uuid = Hashtbl.find qmp_to_uuid c in
						try
							let m = Qmp_protocol.read c in
							debug "QMP %s: %s" uuid (Qmp.string_of_message m)
						with End_of_file ->
							(* End_of_file means the VM has shutdown *)
							debug "QMP %s: End_of_file" uuid;
							remove uuid;
							Updates.add (Dynamic.Vm uuid) updates
					) rs
			done

		let send_async uuid cmd =
			try
				let c = Hashtbl.find uuid_to_qmp uuid in
				Qmp_protocol.write c (Qmp.Command(None, cmd));
				true
			with Not_found ->
				error "Race with qmp monitor thread: type more slowly, try again";
				false

end

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

let next_domid = ref 1

module VM = struct
	include Xenops_server_skeleton.VM

	let create _ memory_limit vm =
		debug "Domain.create vm=%s" vm.Vm.id;
		(* Idempotent *)
		if DB.exists vm.Vm.id then DB.delete vm.Vm.id;
		let open Domain in
		let domain = {
			domid = !next_domid;
			qemu_pid = None;
			uuid = vm.Vm.id;
			vcpus = vm.Vm.vcpus;
			memory = vm.Vm.memory_dynamic_max;
			vifs = [];
			vbds = [];
			active_vifs = [];
			active_vbds = [];
			attach_infos = [];
			pcis = [];
			last_create_time = Unix.gettimeofday ();
		} in
		incr next_domid;
		DB.write vm.Vm.id domain

	let destroy _ vm =
		debug "Domain.destroy vm=%s" vm.Vm.id;
		(* Idempotent *)
		match DB.read vm.Vm.id with
		| Some d ->
			begin match d.Domain.qemu_pid with
			| Some x ->
				info "Sending SIGTERM to qemu pid %d" x;
				(try Unix.kill x Sys.sigterm with Unix.Unix_error(Unix.ESRCH, _, _) -> ())
			| None -> ()
			end;
			DB.delete vm.Vm.id
		| None -> ()

	let unpause _ vm =
		let d = DB.read_exn vm.Vm.id in
		let qemu = Qemu.of_domain d in
		List.iter
			(fun interface ->
				let open Interface in
				DB.write interface.Interface.name interface
			) qemu.Qemu.interfaces;
		let syslog_stdout = Forkhelpers.Syslog_WithKey (Printf.sprintf "qemu-%s" vm.Vm.id) in
		let t = Forkhelpers.safe_close_and_exec None None None [] ~syslog_stdout qemu.Qemu.cmd qemu.Qemu.args in
		let pid = Forkhelpers.getpid t in
		debug "%s %s" qemu.Qemu.cmd (String.concat " " qemu.Qemu.args);
		debug "<- listening on %s" qemu.Qemu.qmp;
		debug "<- with PID %d" pid;
		(* wait for the QMP path to exist *)
		let path = Filename.concat Qemu.qmp_dir vm.Vm.id in
		while not(Sys.file_exists path); do
			debug "waiting for %s" path;
			Thread.delay 0.2
		done;
		(* wait for the VNC path to exist *)
		let path = Filename.concat Qemu.vnc_dir vm.Vm.id in
		while not(Sys.file_exists path); do
			debug "waiting for %s" path;
			Thread.delay 0.2
		done;
		DB.write vm.Vm.id { d with Domain.qemu_pid = Some pid };
		(* At this point we expect qemu to outlive us; we will never call waitpid *)	
		Forkhelpers.dontwaitpid t;
		Updates.add (Dynamic.Vm vm.Vm.id) updates

	let build ?restore_fd _ vm vbds vifs = ()
	let create_device_model _ vm vbds vifs _ = ()
	let destroy_device_model _ vm = ()
	let request_shutdown task vm reason ack_delay =
		debug "pushing ACPI power button";
		Qemu.send_async vm.Vm.id Qmp.System_powerdown
	let wait_shutdown task vm reason timeout =
		(* wait for the QMP path to be removed *)
		let now = Oclock.gettime Oclock.monotonic in
		let path = Filename.concat Qemu.qmp_dir vm.Vm.id in
		let counter = ref 0 in
		while Sys.file_exists path && (Int64.(sub (Oclock.gettime Oclock.monotonic) now < (mul (of_float timeout) 1_000_000_000L))) do
			debug "wait_shutdown: %s still exists" path;
			Thread.delay 0.2;
			incr counter;
			if !counter mod (5 * 10) = 0 then ignore(request_shutdown task vm reason 0.)
		done;
		not(Sys.file_exists path)

	let get_state vm =
		if DB.exists vm.Vm.id then begin
			let d = DB.read_exn vm.Vm.id in
			let power_state = match d.Domain.qemu_pid with
			| None -> Halted
			| Some pid ->
				let path = Filename.concat Qemu.qmp_dir vm.Vm.id in
				if Sys.file_exists path then Running else Halted in
			if power_state = Running then begin
				(* XXX: stat the file first *)
				let path = Filename.concat Qemu.vnc_dir vm.Vm.id in
				debug "%s %s %s" !Path.chgrp !Qemu_path.sockets_group path;
				ignore(Forkhelpers.execute_command_get_output !Path.chgrp [!Qemu_path.sockets_group; path]);
				Unix.chmod path 0o0770;
			end;
			let domids =
				if power_state = Halted
				then []
				else [ d.Domain.domid ] in
			let consoles =
				if power_state = Halted
				then []
				else [ { Vm.protocol = Vm.Rfb; port = 0; path = Filename.concat Qemu.vnc_dir d.Domain.uuid } ] in
			{ halted_vm with
				Vm.power_state = power_state;
				domids = domids;
				consoles = consoles;
				vcpu_target = d.Domain.vcpus;
				last_start_time = d.Domain.last_create_time;
			}
		end else halted_vm

	let set_domain_action_request vm request = ()
	let get_domain_action_request vm =
		if DB.exists vm.Vm.id then begin
			(* If the QMP socket has been deleted then the VM needs shutdown *)
			let path = Filename.concat Qemu.qmp_dir vm.Vm.id in
			if Sys.file_exists path then None else Some Needs_poweroff
		end else None
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

	let dp_of domain vbd = Storage.id_of (string_of_int domain.Domain.domid) vbd.Vbd.id

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
		let interface = Qemu.interface_of_vif d.Domain.domid vif in
		Interface.DB.write interface.Interface.Interface.name interface;
		let existing_positions = List.map (fun vif -> vif.Vif.position) d.Domain.vifs in
		if List.mem vif.Vif.position existing_positions then begin
			debug "VIF.plug %s.%s: Already exists" (fst vif.Vif.id) (snd vif.Vif.id);
			raise (Already_exists("vif", string_of_int vif.Vif.position))
		end else DB.write vm { d with Domain.vifs = vif :: d.Domain.vifs }

	let unplug _ vm vif _ =
		info "VIF.unplug %s.%s" (fst vif.Vif.id) (snd vif.Vif.id);
		let d = DB.read_exn vm in
		let this_one x = x.Vif.id = vif.Vif.id in
		DB.write vm { d with Domain.vifs = List.filter (fun x -> not (this_one x)) d.Domain.vifs };
		let interface = Qemu.interface_of_vif d.Domain.domid vif in
		let id = interface.Interface.Interface.name in
		if Interface.DB.exists id then Interface.DB.delete id

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
	List.iter
		(fun id ->
			let d = DB.read_exn id in
			next_domid := max !next_domid (d.Domain.domid + 1)
		) (DB.list []);

	debug "Creating QMP directory: %s" Qemu.qmp_dir;
	Unixext.mkdir_rec Qemu.qmp_dir 0o0755;
	debug "Creating VNC directory: %s" Qemu.vnc_dir;
	Unixext.mkdir_rec Qemu.vnc_dir 0o0755;
	let (_: Thread.t) = Thread.create
		(fun () ->
			while true do
				finally
				(fun () ->
					debug "(re)starting QMP thread";
					Qemu.monitor ())
				(fun () ->
					Thread.delay 5.)
			done
		) () in
	()
