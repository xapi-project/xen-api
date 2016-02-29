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

let test_vm_resources_of_ref () =
	let open Agility.VMResources in
	let __context = make_test_database () in
	let name_label = "my_vm" in
	let vm = make_vm ~__context ~name_label () in
	(* Set up storage. *)
	let sr = make_sr ~__context () in
	let vdi = make_vdi ~__context ~sR:sr () in
	let (_ : API.ref_VBD) = make_vbd ~__context ~vDI:vdi ~vM:vm () in
	(* Set up networking. *)
	let network = make_network ~__context () in
	let (_ : API.ref_VIF) = make_vif ~__context ~network ~vM:vm () in
	(* Make a VMResources.t *)
	let vm_resources = of_ref ~__context vm in
	(* Test the fields are correct. *)
	assert_equal vm_resources.status (`Local vm);
	assert_equal vm_resources.vm_rec.API.vM_name_label name_label;
	assert_equal vm_resources.networks [network];
	assert_equal vm_resources.srs [sr]

let test_vm_resources_compare () =
	let open Agility.VMResources in
	let __context = make_test_database () in
	let vm_resources1 = make_vm ~__context () |> of_ref ~__context in
	let vm_resources2 = make_vm ~__context () |> of_ref ~__context in
	assert_equal (compare vm_resources1 vm_resources1 = 0) true;
	assert_equal (compare vm_resources1 vm_resources2 = 0) false

let test_vm_resources_are_equal () =
	let open Agility.VMResources in
	let __context = make_test_database () in
	let vm_resources1 = make_vm ~__context () |> of_ref ~__context in
	let vm_resources2 = make_vm ~__context () |> of_ref ~__context in
	assert_equal (are_equal vm_resources1 vm_resources1) true;
	assert_equal (are_equal vm_resources1 vm_resources2) false

let test_vm_resources_mem () =
	let open Agility.VMResources in
	let __context = make_test_database () in
	let vm_resources1 = make_vm ~__context () |> of_ref ~__context in
	let vm_resources2 = make_vm ~__context () |> of_ref ~__context in
	let vm_resources3 = make_vm ~__context () |> of_ref ~__context in
	let items = [vm_resources1; vm_resources2] in
	assert_equal (mem vm_resources1 items) true;
	assert_equal (mem vm_resources2 items) true;
	assert_equal (mem vm_resources3 items) false

let test_vm_resources_assoc () =
	let open Agility.VMResources in
	let __context = make_test_database () in
	let vm_resources1 = make_vm ~__context () |> of_ref ~__context in
	let vm_resources2 = make_vm ~__context () |> of_ref ~__context in
	let vm_resources3 = make_vm ~__context () |> of_ref ~__context in
	let map = [
		vm_resources1, "foo";
		vm_resources2, "bar";
	] in
	assert_equal (assoc vm_resources1 map) "foo";
	assert_equal (assoc vm_resources2 map) "bar";
	assert_raises Not_found (fun () -> assoc vm_resources3 map)

let test_vm_resources_update_record () =
	let open Agility.VMResources in
	let __context = make_test_database () in
	let vm_resources = make_vm ~__context ~name_label:"foo" () |> of_ref ~__context in
	let vm_resources' =
		update_record vm_resources
			{vm_resources.vm_rec with API.vM_name_label = "bar"}
	in
	assert_equal vm_resources'.vm_rec.API.vM_name_label "bar"

let test_vm_agility_with_vgpu () =
	let __context = make_test_database () in
	let vm = make_vm ~__context () in
	(* VM has no VIFs, VBDs or VGPUs, so should be agile. *)
	Agility.vm_assert_agile ~__context ~self:vm;
	(* Create a VGPU - VM should no longer be agile. *)
	let (_: API.ref_VGPU) = make_vgpu ~__context ~vM:vm () in
	assert_raises_api_error
		~args:[Ref.string_of vm]
		Api_errors.vm_has_vgpu
		(fun () -> Agility.vm_assert_agile ~__context ~self:vm)

let test_vm_agility_with_network () =
	(* Set up two hosts, an network and a VM with a VIF on the network. *)
	let __context = make_test_database () in
	let vm = make_vm ~__context () in
	let host1 = make_host ~__context () in
	let host2 = make_host ~__context () in
	Db.Host.set_enabled ~__context ~self:host1 ~value:true;
	Db.Host.set_enabled ~__context ~self:host2 ~value:true;
	let network = make_network ~__context () in
	let (_: API.ref_PIF) = make_pif ~__context ~network ~host:host1 () in
	let (_: API.ref_VIF) = make_vif ~__context ~network ~vM:vm () in
	(* Only one host has a PIF, so the VM is not agile. *)
	assert_raises_api_error
		~args:[Ref.string_of network]
		Api_errors.ha_constraint_violation_network_not_shared
		(fun () -> Agility.vm_assert_agile ~__context ~self:vm);
	(* Making a PIF for the second host should make the VM agile. *)
	let (_: API.ref_PIF) = make_pif ~__context ~network ~host:host2 () in
	Agility.vm_assert_agile ~__context ~self:vm

let test_vm_agility_with_sr () =
	(* Set up two hosts, an SR and a VM with a VDI in the SR. *)
	let __context = make_test_database () in
	let vm = make_vm ~__context () in
	let host1 = make_host ~__context () in
	let host2 = make_host ~__context () in
	Db.Host.set_enabled ~__context ~self:host1 ~value:true;
	Db.Host.set_enabled ~__context ~self:host2 ~value:true;
	let sr = make_sr ~__context () in
	let (_: API.ref_PBD) = make_pbd ~__context ~sR:sr ~host:host1 () in
	let vdi = make_vdi ~__context ~sR:sr () in
	let (_: API.ref_VBD) = make_vbd ~__context ~vDI:vdi ~vM:vm () in
	(* Only one host has a PBD, so the VM is not agile. *)
	assert_raises_api_error
		~args:[Ref.string_of sr]
		Api_errors.ha_constraint_violation_sr_not_shared
		(fun () -> Agility.vm_assert_agile ~__context ~self:vm);
	(* Making a PBD for the second host should make the VM agile. *)
	let (_: API.ref_PBD) = make_pbd ~__context ~sR:sr ~host:host2 () in
	Agility.vm_assert_agile ~__context ~self:vm

let test =
	"test_agility" >:::
		[
			"test_vm_resources_of_ref" >:: test_vm_resources_of_ref;
			"test_vm_resources_compare" >:: test_vm_resources_compare;
			"test_vm_resources_are_equal" >:: test_vm_resources_are_equal;
			"test_vm_resources_mem" >:: test_vm_resources_mem;
			"test_vm_resources_assoc" >:: test_vm_resources_assoc;
			"test_vm_resources_update_record" >:: test_vm_resources_update_record;
			"test_vm_agility_with_vgpu" >:: test_vm_agility_with_vgpu;
			"test_vm_agility_with_network" >:: test_vm_agility_with_network;
			"test_vm_agility_with_sr" >:: test_vm_agility_with_sr;
		]
