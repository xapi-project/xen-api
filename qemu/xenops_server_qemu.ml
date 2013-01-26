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

module Domain = struct
	type t = {
		uuid: string;
		vcpus: int;
		memory: int64;
		vbds: Vbd.t list; (* maintained in reverse-plug order *)
		vifs: Vif.t list;
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

module HOST = struct
	let get_console_data () = "should run 'dmesg' here"
	let get_total_memory_mib () = 0L
	let send_debug_keys _ = ()
end
module VM = struct
	include Xenops_server_skeleton.VM

	let create _ memory_limit vm =
		debug "Domain.create vm=%s" vm.Vm.id;
		if DB.exists vm.Vm.id then begin
			debug "VM.create_nolock %s: Already_exists" vm.Vm.id;
			raise (Already_exists("domain", vm.Vm.id))
		end else begin
			let open Domain in
			let domain = {
				uuid = vm.Vm.id;
				vcpus = vm.Vm.vcpus;
				memory = vm.Vm.memory_dynamic_max;
				vifs = [];
				vbds = [];
				pcis = [];
				last_create_time = Unix.gettimeofday ();
			} in
			DB.write vm.Vm.id domain
		end

	let destroy _ vm =
		debug "Domain.destroy vm=%s" vm.Vm.id;
		(* Idempotent *)
		if DB.exists vm.Vm.id then DB.delete vm.Vm.id

	let unpause _ vm = ()
	let build _ vm vbds vifs = ()
	let create_device_model _ vm vbds vifs _ = ()
	let destroy_device_model _ vm = ()
	let request_shutdown _ vm reason ack_delay = false
	let wait_shutdown _ vm reason timeout = true

	let get_state vm =
		if DB.exists vm.Vm.id then begin
			let d = DB.read_exn vm.Vm.id in
			{ halted_vm with
				Vm.power_state = Running;
				domids = [ ];
				vcpu_target = d.Domain.vcpus;
				last_start_time = d.Domain.last_create_time;
			}
		end else halted_vm

	let set_domain_action_request vm request = ()
	let get_domain_action_request vm = None

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

	let plug _ (vm: Vm.id) (vbd: Vbd.t) =
		debug "add_vbd";
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
		end else DB.write vm { d with Domain.vbds = { vbd with Vbd.position = Some this_dn } :: d.Domain.vbds }

	let unplug _ vm vbd _ =
		let d = DB.read_exn vm in
		let this_one x = x.Vbd.id = vbd.Vbd.id in
		if List.filter this_one d.Domain.vbds = []
		then raise (Does_not_exist("VBD", Printf.sprintf "%s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id)))
		else DB.write vm { d with Domain.vbds = List.filter (fun x -> not (this_one x)) d.Domain.vbds }

	let insert _ vm vbd disk = ()
	let eject _ vm vbd = ()

	let get_state vm vbd = unplugged_vbd

	let get_device_action_request vm vbd = None
end

module VIF = struct
	include Xenops_server_skeleton.VIF

	let plug _ vm vif =
		let d = DB.read_exn vm in
		let existing_positions = List.map (fun vif -> vif.Vif.position) d.Domain.vifs in
		if List.mem vif.Vif.position existing_positions then begin
			debug "VIF.plug %s.%s: Already exists" (fst vif.Vif.id) (snd vif.Vif.id);
			raise (Already_exists("vif", string_of_int vif.Vif.position))
		end else DB.write vm { d with Domain.vifs = vif :: d.Domain.vifs }

	let unplug _ vm vif _ =
		let d = DB.read_exn vm in
		let this_one x = x.Vif.id = vif.Vif.id in
		if List.filter this_one d.Domain.vifs = []
		then raise (Does_not_exist("VIF", Printf.sprintf "%s.%s" (fst vif.Vif.id) (snd vif.Vif.id)))
		else DB.write vm { d with Domain.vifs = List.filter (fun x -> not (this_one x)) d.Domain.vifs }

	let get_state vm vif = unplugged_vif

	let get_device_action_request vm vif = None
end

module UPDATES = struct
	let get last timeout = Updates.get "UPDATES.get" last timeout updates
end

module DEBUG = struct
	include Xenops_server_skeleton.DEBUG
end

let init () = ()
