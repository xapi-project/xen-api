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

open OUnit
open Test_common
open Test_highlevel

let ( *** ) = Int64.mul
let kib x = 1024L *** x
let mib x = x |> kib |> kib
let gib x = x |> kib |> kib |> kib

type vbd = {
	agile : bool;
}

type vif = {
	agile : bool;
}

type vm = {
	ha_always_run : bool;
	ha_restart_priority : string;
	memory : int64;
	name_label : string;
	vbds : vbd list;
	vifs : vif list;
}

let basic_vm = {
	ha_always_run = true;
	ha_restart_priority = "restart";
	memory = gib 1L;
	name_label = "vm";
	vbds = [{agile = true}];
	vifs = [{agile = true}];
}

type host = {
	memory_total : int64;
	name_label : string;
	vms : vm list;
}

type pool = {
	master: host;
	slaves: host list;
}

let string_of_vm {memory; name_label} =
	Printf.sprintf "{memory = %Ld; name_label = %S}" memory name_label

let string_of_host {memory_total; name_label; vms} =
	Printf.sprintf "{memory_total = %Ld; name_label = %S; vms = [%s]}"
		memory_total name_label
		(Test_printers.list string_of_vm vms)

let string_of_pool {master; slaves} =
	Printf.sprintf
		"{master = %s; slaves = %s}"
		(string_of_host master)
		(Test_printers.list string_of_host slaves)

let load_vm ~__context ~(vm:vm) ~local_sr ~shared_sr ~local_net ~shared_net =
	let vm_ref = make_vm ~__context
		~ha_always_run:vm.ha_always_run
		~ha_restart_priority:vm.ha_restart_priority
		~memory_static_min:vm.memory
		~memory_dynamic_min:vm.memory
		~memory_dynamic_max:vm.memory
		~memory_static_max:vm.memory
		~name_label:vm.name_label ()
	in
	let (_ : API.ref_VIF list) =
		List.mapi
			(fun index (vif:vif) ->
				make_vif ~__context ~device:(string_of_int index) ~vM:vm_ref
					~network:(if vif.agile then shared_net else local_net) ())
			vm.vifs
	in
	let (_ : API.ref_VBD list) =
		List.mapi
			(fun index (vbd:vbd) ->
				let vdi_ref =
					make_vdi ~__context ~sR:(if vbd.agile then shared_sr else local_sr) ()
				in
				make_vbd ~__context ~device:(string_of_int index) ~vM:vm_ref
					~vDI:vdi_ref ())
			vm.vbds
	in
	vm_ref

let load_host ~__context ~host ~local_sr ~shared_sr ~local_net ~shared_net =
	let host_ref = make_host ~__context ~name_label:host.name_label () in
	Db.Host.set_enabled ~__context ~self:host_ref ~value:true;
	let metrics = Db.Host.get_metrics ~__context ~self:host_ref in
	Db.Host_metrics.set_live ~__context ~self:metrics ~value:true;
	Db.Host_metrics.set_memory_total ~__context
		~self:metrics ~value:host.memory_total;

	let (_ : API.ref_VM list) =
		List.map
			(fun vm ->
				load_vm ~__context ~vm ~local_sr ~shared_sr ~local_net ~shared_net)
			host.vms
	in
	host_ref

let setup ~__context {master; slaves} =
	let shared_sr = make_sr ~__context ~shared:true () in
	let shared_net = make_network ~__context ~bridge:"xenbr0" () in

	let load_host_and_local_resources host =
		let local_sr = make_sr ~__context ~shared:false () in
		let local_net = make_network ~__context ~bridge:"xapi0" () in

		let host_ref =
			load_host ~__context ~host ~local_sr ~shared_sr ~local_net ~shared_net in

		let (_ : API.ref_PBD) =
			make_pbd ~__context ~host:host_ref ~sR:local_sr () in
		let (_ : API.ref_PBD) =
			make_pbd ~__context ~host:host_ref ~sR:shared_sr () in

		let (_ : API.ref_PIF) =
			make_pif ~__context ~host:host_ref ~network:local_net () in
		let (_ : API.ref_PIF) =
			make_pif ~__context ~host:host_ref ~network:shared_net () in
		host_ref
	in

	let master_ref = load_host_and_local_resources master in
	let (_ : API.ref_host list) = List.map load_host_and_local_resources slaves in

	let (_ : API.ref_pool) = make_pool ~__context ~master:master_ref () in
	()

module AllProtectedVms = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = pool
		type output_t = string list

		let string_of_input_t = string_of_pool
		let string_of_output_t = Test_printers.(list string)
	end

	module State = XapiDb

	let load_input __context input = setup ~__context input

	let extract_output __context _ =
		Xapi_ha_vm_failover.all_protected_vms ~__context
			|> List.map (fun (_, vm_rec) -> vm_rec.API.vM_name_label)
			|> List.sort compare

	let tests = [
		(* No VMs and a single host. *)
		{
			master = {memory_total = gib 256L; name_label = "master"; vms = []};
			slaves = [];
		},
		[];
		(* One unprotected VM. *)
		{
			master = {
				memory_total = gib 256L; name_label = "master";
				vms = [{basic_vm with
					ha_always_run = false;
					ha_restart_priority = "";
				}];
			};
			slaves = [];
		},
		[];
		(* One VM which would be protected if it was running. *)
		{
			master = {
				memory_total = gib 256L; name_label = "master";
				vms = [{basic_vm with ha_always_run = false}];
			};
			slaves = [];
		},
		[];
		(* One protected VM. *)
		{
			master = {
				memory_total = gib 256L; name_label = "master";
				vms = [basic_vm];
			};
			slaves = [];
		},
		["vm"];
		(* One protected VM and one unprotected VM. *)
		{
			master = {
				memory_total = gib 256L; name_label = "master";
				vms = [
					{basic_vm with name_label = "vm1"};
					{basic_vm with
						ha_always_run = false;
						ha_restart_priority = "";
						name_label = "vm2"
					}
				];
			};
			slaves = [];
		},
		["vm1"];
	]
end))

let test =
	"test_ha_vm_failover" >:::
		[
			"test_all_protected_vms" >::: AllProtectedVms.tests;
		]
