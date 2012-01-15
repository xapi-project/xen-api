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
open Threadext
open Listext
open Xenops_interface
open Xenops_server_plugin
open Xenops_utils

let ( |> ) a b = b a

module Domain = struct
	type t = {
		domid: int;
		uuid: string;
		hvm: bool;
		domain_action_request: domain_action_request option;
		paused: bool;
		built: bool;
		vcpus: int;
		shadow_multiplier: float;
		qemu_created: bool;
		suspended: bool;
		vbds: Vbd.t list;
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

let next_domid =
	let next = ref None in (* unknown *)
	let get_next =
		fun () -> match !next with
			| None ->
				let domains = List.filter_map DB.read (DB.list []) in
				let domids = List.map (fun x -> x.Domain.domid) domains in
				let highest = List.fold_left max 0 domids in
				next := Some (highest + 1);
				highest + 1
			| Some x -> x in
	let incr_next () = next := Opt.map (fun x -> x + 1) !next in
	fun () ->
		let result = get_next () in
		incr_next ();
		debug "using domid %d" result;
		result

let m = Mutex.create ()

let create_nolock vm () =
	debug "Domain.create vm=%s" vm.Vm.id;
	if DB.exists vm.Vm.id then begin
		debug "VM.create_nolock %s: Already_exists" vm.Vm.id;
		raise (Exception Already_exists)
	end else begin
		let open Domain in
		let domain = {
			domid = next_domid ();
			uuid = vm.Vm.id;
			hvm = (match vm.Vm.ty with Vm.HVM _ -> true | _ -> false);
			domain_action_request = None;
			paused = true;
			built = false;
			vcpus = vm.Vm.vcpus;
			shadow_multiplier = (match vm.Vm.ty with Vm.HVM { Vm.shadow_multiplier = x } -> x | _ -> 1.);
			qemu_created = false;
			suspended = false;
			vifs = [];
			vbds = [];
			pcis = [];
			last_create_time = Unix.gettimeofday ();
		} in
		DB.write vm.Vm.id domain
	end

let get_state_nolock vm () =
	if DB.exists vm.Vm.id then begin
		let d = DB.read_exn vm.Vm.id in
		{ halted_vm with
			Vm.power_state = Running;
			domids = [ d.Domain.domid ];
			vcpu_target = d.Domain.vcpus;
			last_start_time = d.Domain.last_create_time;
		}
	end else halted_vm

let get_domain_action_request_nolock vm () =
	if DB.exists vm.Vm.id then begin
		let d = DB.read_exn vm.Vm.id in
		d.Domain.domain_action_request
	end else Some Needs_poweroff

let destroy_nolock vm () =
	debug "Domain.destroy vm=%s" vm.Vm.id;
	(* Idempotent *)
	if DB.exists vm.Vm.id then DB.delete vm.Vm.id

let build_nolock vm vbds vifs () =
	debug "Domain.build vm=%s" vm.Vm.id;
	debug "setting built <- true";
	DB.write vm.Vm.id { DB.read_exn vm.Vm.id with Domain.built = true }

let create_device_model_nolock vm () =
	debug "Domain.create_device_model vm=%s" vm.Vm.id;
	DB.write vm.Vm.id { DB.read_exn vm.Vm.id with Domain.qemu_created = true }

let destroy_device_model_nolock vm () =
	debug "Domain.destroy_device_model vm=%s" vm.Vm.id;
	DB.write vm.Vm.id { DB.read_exn vm.Vm.id with Domain.qemu_created = false }

let request_shutdown_nolock vm reason () =
	DB.write vm.Vm.id { DB.read_exn vm.Vm.id with Domain.domain_action_request =
			Some (match reason with
				| Halt | PowerOff -> Needs_poweroff
				| Reboot -> Needs_reboot
				| Suspend | S3Suspend -> Needs_suspend)
	};
	Updates.add (Dynamic.Vm vm.Vm.id) updates;
	true

let save_nolock vm _ data () =
	DB.write vm.Vm.id { DB.read_exn vm.Vm.id with Domain.suspended = true }

let restore_nolock vm data () =
	DB.write vm.Vm.id { DB.read_exn vm.Vm.id with Domain.built = true }

let do_pause_unpause_nolock vm paused () =
	let d = DB.read_exn vm.Vm.id in
	if not d.Domain.built || (d.Domain.hvm && not(d.Domain.qemu_created))
	then raise (Exception Domain_not_built)
	else DB.write vm.Vm.id { d with Domain.paused = paused }

let do_set_vcpus_nolock vm n () =
	let d = DB.read_exn vm.Vm.id in
	if not d.Domain.built || (d.Domain.hvm && not(d.Domain.qemu_created))
	then raise (Exception Domain_not_built)	
	else DB.write vm.Vm.id { d with Domain.vcpus = n }

let do_set_shadow_multiplier_nolock vm m () =
	let d = DB.read_exn vm.Vm.id in
	if not d.Domain.built || (d.Domain.hvm && not(d.Domain.qemu_created))
	then raise (Exception Domain_not_built)	
	else DB.write vm.Vm.id { d with Domain.shadow_multiplier = m }

let add_vif vm vif () =
	let d = DB.read_exn vm in
	let existing_positions = List.map (fun vif -> vif.Vif.position) d.Domain.vifs in
	if List.mem vif.Vif.position existing_positions then begin
		debug "VIF.plug %s.%s: Already exists" (fst vif.Vif.id) (snd vif.Vif.id);
		raise (Exception Already_exists)
	end else DB.write vm { d with Domain.vifs = vif :: d.Domain.vifs }

let add_vbd (vm: Vm.id) (vbd: Vbd.t) () =
	debug "add_vbd";
	let d = DB.read_exn vm in
	(* there shouldn't be any None values in here anyway *)
	let ps = List.map (fun vbd -> vbd.Vbd.position) d.Domain.vbds in
	assert (not (List.mem None ps));
	let dns = List.map (Opt.unbox) ps in
	let indices = List.map Device_number.to_disk_number dns in
	let next_index = List.fold_left max (-1) indices + 1 in
	let next_dn = Device_number.of_disk_number d.Domain.hvm next_index in
	let this_dn = Opt.default next_dn vbd.Vbd.position in
	if List.mem this_dn dns then begin
		debug "VBD.plug %s.%s: Already exists" (fst vbd.Vbd.id) (snd vbd.Vbd.id);
		raise (Exception Already_exists)
	end else DB.write vm { d with Domain.vbds = { vbd with Vbd.position = Some this_dn } :: d.Domain.vbds }

let add_pci (vm: Vm.id) (pci: Pci.t) () =
	debug "add_pci";
	let d = DB.read_exn vm in
	let existing_positions = List.map (fun pci -> pci.Pci.position) d.Domain.pcis in
	if List.mem pci.Pci.position existing_positions then begin
		debug "PCI.plug %s.%s: Already exists" (fst pci.Pci.id) (snd pci.Pci.id);
		raise (Exception Already_exists)
	end else DB.write vm { d with Domain.pcis = pci :: d.Domain.pcis }

let pci_state vm pci () =
	if not (DB.exists vm)
	then unplugged_pci
	else
		let d = DB.read_exn vm in
		let this_one x = x.Pci.id = pci.Pci.id in
		match List.filter this_one d.Domain.pcis with
			| [ pci ] ->
				{
					Pci.plugged = true
				}
			| [] -> unplugged_pci
			| _ -> assert false (* at most one *)

let vbd_state vm vbd () =
	if not (DB.exists vm)
	then unplugged_vbd
	else
		let d = DB.read_exn vm in
		let this_one x = x.Vbd.id = vbd.Vbd.id in
		match List.filter this_one d.Domain.vbds with
			| [ vbd ] ->
				{
					unplugged_vbd with
						Vbd.plugged = true;
						media_present = vbd.Vbd.backend <> None
				}
			| [] -> unplugged_vbd
			| _ -> assert false (* at most one *)
				

let vif_state vm vif () =
	if not (DB.exists vm)
	then unplugged_vif
	else
		let d = DB.read_exn vm in
		let this_one x = x.Vif.id = vif.Vif.id in
		match List.filter this_one d.Domain.vifs with
			| [ domain ] ->
				{
					unplugged_vif with
						Vif.plugged = true;
				}
			| [] -> unplugged_vif
			| _ -> assert false (* at most one *)

let remove_vif vm vif () =
	let d = DB.read_exn vm in
	let this_one x = x.Vif.id = vif.Vif.id in
	if List.filter this_one d.Domain.vifs = []
	then raise (Exception(Does_not_exist("VIF", Printf.sprintf "%s.%s" (fst vif.Vif.id) (snd vif.Vif.id))))
	else DB.write vm { d with Domain.vifs = List.filter (fun x -> not (this_one x)) d.Domain.vifs }

let remove_pci vm pci () =
	let d = DB.read_exn vm in
	let this_one x = x.Pci.id = pci.Pci.id in
	if List.filter this_one d.Domain.pcis = []
	then raise (Exception(Does_not_exist("PCI", Printf.sprintf "%s.%s" (fst pci.Pci.id) (snd pci.Pci.id))))
	else DB.write vm { d with Domain.pcis = List.filter (fun x -> not (this_one x)) d.Domain.pcis }

let remove_vbd vm vbd () =
	let d = DB.read_exn vm in
	let this_one x = x.Vbd.id = vbd.Vbd.id in
	if List.filter this_one d.Domain.vbds = []
	then raise (Exception(Does_not_exist("VBD", Printf.sprintf "%s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id))))
	else DB.write vm { d with Domain.vbds = List.filter (fun x -> not (this_one x)) d.Domain.vbds }

let set_qos_vbd vm vbd () =
	let d = DB.read_exn vm in
	let this_one x = x.Vbd.id = vbd.Vbd.id in
	if List.filter this_one d.Domain.vbds = []
	then raise (Exception(Does_not_exist("VBD", Printf.sprintf "%s.%s" (fst vbd.Vbd.id) (snd vbd.Vbd.id))));
	(* XXX *)
	()

	
module VM = struct
	let create _ vm = Mutex.execute m (create_nolock vm)
	let destroy _ vm = Mutex.execute m (destroy_nolock vm)
	let pause _ vm = Mutex.execute m (do_pause_unpause_nolock vm true)
	let unpause _ vm = Mutex.execute m (do_pause_unpause_nolock vm false)
	let set_vcpus _ vm n = Mutex.execute m (do_set_vcpus_nolock vm n)
	let set_shadow_multiplier _ vm n = Mutex.execute m (do_set_shadow_multiplier_nolock vm n)
	let build _ vm vbds vifs = Mutex.execute m (build_nolock vm vbds vifs)
	let create_device_model _ vm _ = Mutex.execute m (create_device_model_nolock vm)
	let destroy_device_model _ vm = Mutex.execute m (destroy_device_model_nolock vm)
	let request_shutdown _ vm reason ack_delay = Mutex.execute m (request_shutdown_nolock vm reason)
	let wait_shutdown _ vm reason timeout = true

	let save _ vm flags data = Mutex.execute m (save_nolock vm flags data)
	let restore _ vm data = Mutex.execute m (restore_nolock vm data)

	let s3suspend _ vm = ()
	let s3resume _ vm = ()

	let get_state vm = Mutex.execute m (get_state_nolock vm)
	let get_domain_action_request vm = Mutex.execute m (get_domain_action_request_nolock vm)

	let get_internal_state vm =
		vm.Vm.id |> DB.read |> Opt.unbox |> Domain.rpc_of_t |> Jsonrpc.to_string
	let set_internal_state vm s =
		DB.write vm.Vm.id (s |> Jsonrpc.of_string |> Domain.t_of_rpc)

	let minimum_reboot_delay = 0.
end

module PCI = struct
	let plug _ (vm: Vm.id) (pci: Pci.t) = Mutex.execute m (add_pci vm pci)
	let unplug _ vm pci = Mutex.execute m (remove_pci vm pci)

	let get_state vm pci = Mutex.execute m (pci_state vm pci)
end

module VBD = struct
	let plug _ (vm: Vm.id) (vbd: Vbd.t) = Mutex.execute m (add_vbd vm vbd)
	let unplug _ vm vbd _ = Mutex.execute m (remove_vbd vm vbd)

	let insert _ vm vbd disk = ()
	let eject _ vm vbd = ()

	let set_qos _ vm vbd = Mutex.execute m (set_qos_vbd vm vbd)

	let get_state vm vbd = Mutex.execute m (vbd_state vm vbd)

	let get_device_action_request vm vbd = None
end

module VIF = struct
	let plug _ vm vif = Mutex.execute m (add_vif vm vif)
	let unplug _ vm vif _ = Mutex.execute m (remove_vif vm vif)

	let get_state vm vif = Mutex.execute m (vif_state vm vif)

	let get_device_action_request vm vif = None
end

module UPDATES = struct
	let get last timeout = Updates.get last timeout updates
end

module DEBUG = struct
	let trigger cmd args = match cmd, args with
		| "reboot", [ k ] ->
			let d = DB.read_exn k in
			DB.write k { d with Domain.domain_action_request = Some Needs_reboot };
			Updates.add (Dynamic.Vm k) updates
		| "halt", [ k ] ->
			let d = DB.read_exn k in
			DB.write k { d with Domain.domain_action_request = Some Needs_poweroff };
			Updates.add (Dynamic.Vm k) updates
		| _ ->
			debug "DEBUG.trigger cmd=%s Not_supported" cmd;
			raise (Exception Not_supported)
end

let init () = ()
