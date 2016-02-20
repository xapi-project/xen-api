(*
 * Copyright (C) 2014 Citrix Systems Inc.
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

let make_guest_metrics ~__context ~self =
	let existing = Db.VM.get_guest_metrics ~__context ~self in
	if (try ignore(Db.VM_guest_metrics.get_uuid ~__context ~self:existing); true with _ -> false)
	then existing
	else
	  let new_ref = Ref.make () and new_uuid = Uuid.to_string (Uuid.make_uuid ()) in
	  Db.VM_guest_metrics.create ~__context ~ref:new_ref ~uuid:new_uuid
		~os_version:[] ~pV_drivers_version:[] ~pV_drivers_up_to_date:false ~memory:[] ~disks:[] ~networks:[] ~other:[]
		~storage_paths_optimized:false ~network_paths_optimized:false ~last_updated:(Date.of_float 0.) ~other_config:[] ~live:false ~can_use_hotplug_vbd:`unspecified ~can_use_hotplug_vif:`unspecified;
	  Db.VM.set_guest_metrics ~__context ~self ~value:new_ref;
	  new_ref

let setup_test ~__context ~vif_run =
	let vm_ref = make_vm ~__context () in
	let gm_ref = make_guest_metrics ~__context ~self:vm_ref in
	let vif_ref = make_vif ~__context ~device:"0" ~vM:vm_ref () in
	vif_run vm_ref gm_ref vif_ref

let test_add_to_static_ip_setting_success () =
	let __context = make_test_database () in
	setup_test ~__context
		~vif_run:(fun vm_ref gm_ref vif_ref ->
			Db.VM_guest_metrics.set_network_paths_optimized ~__context ~self:gm_ref ~value:true;
			Db.VM_guest_metrics.set_storage_paths_optimized ~__context ~self:gm_ref ~value:true;
			Xapi_vif.add_to_static_ip_setting ~__context ~self:vif_ref ~key:"address" ~value:"192.168.1.10/24")

let test_add_to_static_ip_setting_failure () =
	let __context = make_test_database () in
	assert_raises_api_error Api_errors.vm_missing_pv_drivers
	(fun () ->
		setup_test ~__context ~vif_run:(fun vm_ref gm_ref vif_ref ->
			Xapi_vif.add_to_static_ip_setting ~__context ~self:vif_ref ~key:"address" ~value:"192.168.1.10/24"))	

let test_remove_from_static_ip_setting_success () =
	let __context = make_test_database () in
	setup_test ~__context
		~vif_run:(fun vm_ref gm_ref vif_ref ->
			Db.VM_guest_metrics.set_network_paths_optimized ~__context ~self:gm_ref ~value:true;
			Db.VM_guest_metrics.set_storage_paths_optimized ~__context ~self:gm_ref ~value:true;
			Xapi_vif.add_to_static_ip_setting ~__context ~self:vif_ref ~key:"address" ~value:"192.168.1.10/24";
			Xapi_vif.remove_from_static_ip_setting ~__context ~self:vif_ref ~key:"address")

let test =
	"test_xapi_vif" >:::
		[
			"test_add_to_static_ip_setting_success" >:: test_add_to_static_ip_setting_success;
			"test_add_to_static_ip_setting_failure" >:: test_add_to_static_ip_setting_failure;
			"test_remove_from_static_ip_setting_success" >:: test_remove_from_static_ip_setting_success;
		]
