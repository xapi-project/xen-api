(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

type pif = {device: string; management: bool; other_config: (string * string) list}

module DetermineGateway = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		(* The type of inputs to a system being tested. *)
		type input_t = pif list * string option
		(* The type of outputs from a system being tested. *)
		type output_t = string option * string option

		(* Helper functions for printing error messages on test failure. *)
		let string_of_pif pif =
			Printf.sprintf "[device = %s; management = %b; other_config = %s]"
				pif.device pif.management
				(Test_printers.(assoc_list string string) pif.other_config)
		
		let string_of_input_t =
			Test_printers.(assoc_pair
				(list string_of_pif)
				(option string))
				
		let string_of_output_t =
			Test_printers.(assoc_pair
				(option string)
				(option string))
	end
	module State = XapiDb
	
	let load_input __context (pifs, _) =
		make_localhost ~__context;
		List.iter (fun pif ->
			let network = make_network ~__context () in
			let _ = make_pif ~__context ~network ~host:!Xapi_globs.localhost_ref
				~ip_configuration_mode:`DHCP ~device:pif.device
				~management:pif.management ~other_config:pif.other_config () in
			()
		) pifs
			
	let extract_output __context (_, mgmt) =
		let management_interface = Opt.map (fun device ->
			let open Db_filter_types in
			let pifs = Db.PIF.get_refs_where ~__context ~expr:(Eq (Field "device", Literal device)) in
			List.hd pifs
		) mgmt in
		let gateway, dns = Helpers.determine_gateway_and_dns_ifs ~__context ?management_interface () in
		let get_device = Opt.map (fun (self, _) -> Db.PIF.get_device ~__context ~self) in
		get_device gateway,
		get_device dns
		
	let tests = [
		([
			{device="eth0"; management=true; other_config=[]};
			{device="eth1"; management=false; other_config=[]}],
			None
		),
		(Some "eth0", Some "eth0");
		
		([
			{device="eth0"; management=true; other_config=[]};
			{device="eth1"; management=false; other_config=[]}],
			Some "eth1"
		),
		(Some "eth1", Some "eth1");
		
		([
			{device="eth0"; management=true; other_config=[]};
			{device="eth1"; management=false; other_config=["defaultroute","true"]}],
			None
		),
		(Some "eth1", Some "eth0");
		
		([
			{device="eth0"; management=true; other_config=[]};
			{device="eth1"; management=false; other_config=["peerdns","true"]}],
			None
		),
		(Some "eth0", Some "eth1");
		
		([
			{device="eth0"; management=false; other_config=[]};
			{device="eth1"; management=false; other_config=["defaultroute","true"]}],
			Some "eth0"
		),
		(Some "eth1", Some "eth0");
		
		([
			{device="eth0"; management=false; other_config=[]};
			{device="eth1"; management=false; other_config=["peerdns","true"]}],
			Some "eth0"
		),
		(Some "eth0", Some "eth1");
	]
end))

let test_vm_agility_with_vgpu () =
	let __context = make_test_database () in
	let vm = make_vm ~__context () in
	(* VM has no VIFs, VBDs or VGPUs, so should be agile. *)
	Helpers.vm_assert_agile ~__context ~self:vm;
	(* Create a VGPU - VM should no longer be agile. *)
	let (_: API.ref_VGPU) = make_vgpu ~__context ~vM:vm () in
	assert_raises_api_error
		~args:[Ref.string_of vm]
		Api_errors.vm_has_vgpu
		(fun () -> Helpers.vm_assert_agile ~__context ~self:vm)

let test =
	"test_helpers" >:::
		[
			"test_vm_agility_with_vgpu" >:: test_vm_agility_with_vgpu;
			"test_determine_gateway" >::: DetermineGateway.tests;
		]
