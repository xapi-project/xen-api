(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

open API
open Fun
open OUnit

(** Utility functions *)
let id (x : 'a) : 'a = x

let string_of_string_list strings =
	Printf.sprintf "[%s]"
		(String.concat "; " strings)

let string_of_string_map map =
	Printf.sprintf "[%s]"
		(List.map (fun (k, v) -> k ^ ": " ^ v) map |> String.concat "; ")

let skip str = skip_if true str
let make_uuid () = Uuid.string_of_uuid (Uuid.make_uuid ())

let assert_raises_api_error (code : string) ?(args : string list option) (f : unit -> 'a) : unit =
	try
		f ();
		assert_failure (Printf.sprintf "Function didn't raise expected API error %s" code)
	with Api_errors.Server_error (c, a) ->
		assert_equal ~printer:id ~msg:"Function raised unexpected API error" code c;
		match args with
		| None -> ()
		| Some args ->
			assert_equal ~printer:string_of_string_list ~msg:"Function raised API error with unexpected args" args a

(** Make a simple in-memory database containing a single host and dom0 VM record. *)
let make_test_database () = 
	let __context = Mock.make_context_with_new_db "Mock context" in

	let host_info = {
		Create_misc.name_label = "test host";
		xen_verstring = "unknown";
		linux_verstring = "something";
		hostname = "localhost";
		uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid;
		dom0_uuid = "dom0-uuid";
		oem_manufacturer = None;
		oem_model = None;
		oem_build_number = None;
		machine_serial_number = None;
		machine_serial_name = None;
		total_memory_mib = 1024L;
		dom0_static_max = Memory.bytes_of_mib 512L;
	} in
	Dbsync_slave.create_localhost ~__context host_info;
	Create_misc.ensure_domain_zero_records ~__context host_info;
	Dbsync_master.create_pool_record ~__context;
	__context

let make_vm ~__context ?(name_label="name_label") ?(name_description="description")
		?(user_version=1L) ?(is_a_template=false) ?(affinity=Ref.null)
		?(memory_target=500L) ?(memory_static_max=1000L) ?(memory_dynamic_max=500L)
		?(memory_dynamic_min=500L) ?(memory_static_min=0L) ?(vCPUs_params=[])
		?(vCPUs_max=1L) ?(vCPUs_at_startup=1L) ?(actions_after_shutdown=`destroy)
		?(actions_after_reboot=`restart) ?(actions_after_crash=`destroy)
		?(pV_bootloader="") ?(pV_kernel="") ?(pV_ramdisk="") ?(pV_args="") 
		?(pV_bootloader_args="") ?(pV_legacy_args="") ?(hVM_boot_policy="BIOS order")
		?(hVM_boot_params=[]) ?(hVM_shadow_multiplier=1.) ?(platform=[]) ?(pCI_bus="")
		?(other_config=[]) ?(xenstore_data=[]) ?(recommendations="") ?(ha_always_run=false)
		?(ha_restart_priority="") ?(tags=[]) ?(blocked_operations=[]) ?(protection_policy=Ref.null)
		?(is_snapshot_from_vmpp=false) ?(appliance=Ref.null) ?(start_delay=0L)
		?(shutdown_delay=0L) ?(order=0L) ?(suspend_SR=Ref.null) ?(version=0L) ?(generation_id="0:0") () =
	Xapi_vm.create ~__context ~name_label ~name_description ~user_version ~is_a_template
		~affinity ~memory_target ~memory_static_max ~memory_dynamic_max ~memory_dynamic_min
        ~memory_static_min ~vCPUs_params ~vCPUs_max ~vCPUs_at_startup ~actions_after_shutdown 
		~actions_after_reboot ~actions_after_crash ~pV_bootloader ~pV_kernel ~pV_ramdisk 
		~pV_args ~pV_bootloader_args ~pV_legacy_args ~hVM_boot_policy ~hVM_boot_params 
		~hVM_shadow_multiplier ~platform ~pCI_bus ~other_config ~xenstore_data ~recommendations
		~ha_always_run ~ha_restart_priority ~tags ~blocked_operations ~protection_policy
		~is_snapshot_from_vmpp ~appliance ~start_delay ~shutdown_delay ~order ~suspend_SR
		~version ~generation_id

let make_host ~__context ?(uuid=make_uuid ()) ?(name_label="host")
		?(name_description="description") ?(hostname="localhost") ?(address="127.0.0.1")
		?(external_auth_type="") ?(external_auth_service_name="") ?(external_auth_configuration=[])
		?(license_params=[]) ?(edition="free") ?(license_server=[]) ?(local_cache_sr=Ref.null) ?(chipset_info=[]) () = 

	Xapi_host.create ~__context ~uuid ~name_label ~name_description ~hostname ~address ~external_auth_type ~external_auth_service_name ~external_auth_configuration ~license_params ~edition ~license_server ~local_cache_sr ~chipset_info

let make_pif ~__context ~network ~host ?(device="eth0") ?(mAC="C0:FF:EE:C0:FF:EE") ?(mTU=1500L)
		?(vLAN=(-1L)) ?(physical=true) ?(ip_configuration_mode=`None) ?(iP="") ?(netmask="")
		?(gateway="") ?(dNS="") ?(bond_slave_of=Ref.null) ?(vLAN_master_of=Ref.null) 
		?(management=false) ?(other_config=[]) ?(disallow_unplug=false) 
		?(ipv6_configuration_mode=`None) ?(iPv6=[""]) ?(ipv6_gateway="") ?(primary_address_type=`IPv4) ?(managed=true)
		?(properties=["gro", "on"]) () =
	Xapi_pif.pool_introduce ~__context
		~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode
		~iP ~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug
                ~ipv6_configuration_mode ~iPv6 ~ipv6_gateway ~primary_address_type ~managed ~properties

let make_network ~__context ?(name_label="net") ?(name_description="description") ?(mTU=1500L)
		?(other_config=[]) ?(bridge="xenbr0") () = 
	Xapi_network.pool_introduce ~__context ~name_label ~name_description ~mTU ~other_config ~bridge

let make_pool ~__context ~master ?(name_label="") ?(name_description="")
		?(default_SR=Ref.null) ?(suspend_image_SR=Ref.null) ?(crash_dump_SR=Ref.null)
		?(ha_enabled=false) ?(ha_configuration=[]) ?(ha_statefiles=[])
		?(ha_host_failures_to_tolerate=0L) ?(ha_plan_exists_for=0L)
		?(ha_allow_overcommit=false) ?(ha_overcommitted=false) ?(blobs=[]) ?(tags=[])
		?(gui_config=[]) ?(wlb_url="") ?(wlb_username="") ?(wlb_password=Ref.null)
		?(wlb_enabled=false) ?(wlb_verify_cert=false) ?(redo_log_enabled=false)
		?(redo_log_vdi=Ref.null) ?(vswitch_controller="") ?(restrictions=[])
		?(other_config=[Xapi_globs.memory_ratio_hvm; Xapi_globs.memory_ratio_pv]) () =
	let pool_ref = Ref.make () in
	Db.Pool.create ~__context ~ref:pool_ref
		~uuid:(make_uuid ()) ~name_label ~name_description
		~master ~default_SR ~suspend_image_SR ~crash_dump_SR ~ha_enabled
		~ha_configuration ~ha_statefiles ~ha_host_failures_to_tolerate
		~ha_plan_exists_for ~ha_allow_overcommit ~ha_overcommitted ~blobs ~tags
		~gui_config ~wlb_url ~wlb_username ~wlb_password ~wlb_enabled
		~wlb_verify_cert ~redo_log_enabled ~redo_log_vdi ~vswitch_controller
		~restrictions ~other_config;
	pool_ref

let make_sr ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(name_label="") ?(name_description="") ?(allowed_operations=[])
		?(current_operations=[]) ?(virtual_allocation=0L) ?(physical_utilisation=0L) ?(physical_size=0L) ?(_type="")
		?(content_type="") ?(shared=true) ?(other_config=[]) ?(tags=[]) ?(default_vdi_visibility=true)
		?(sm_config=[]) ?(blobs=[]) ?(local_cache_enabled=false) ?(introduced_by=Ref.make ()) () =
	Db.SR.create ~__context ~ref ~uuid ~name_label ~name_description ~allowed_operations
		~current_operations ~virtual_allocation ~physical_utilisation ~physical_size ~_type
		~content_type ~shared ~other_config ~tags ~default_vdi_visibility ~sm_config ~blobs
		~local_cache_enabled ~introduced_by;
	ref

let make_pbd ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(host=Ref.make ()) ?(sR=Ref.make ())
		?(device_config=[]) ?(currently_attached=true) ?(other_config=[]) () =
	Db.PBD.create ~__context ~ref ~uuid ~host ~sR ~device_config ~currently_attached ~other_config;
	ref

let make_vbd ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(allowed_operations=[])
		?(current_operations=[]) ?(vM=Ref.make ()) ?(vDI=Ref.make ()) ?(device="")
		?(userdevice="") ?(bootable=true) ?(mode=`RW) ?(_type=`Disk)
		?(unpluggable=false) ?(storage_lock=false) ?(empty=false)
		?(reserved=false) ?(other_config=[]) ?(currently_attached=false)
		?(status_code=0L) ?(status_detail="") ?(runtime_properties=[])
		?(qos_algorithm_type="") ?(qos_algorithm_params=[]) ?(qos_supported_algorithms=[])
		?(metrics = Ref.make ()) () =
	Db.VBD.create ~__context ~ref ~uuid ~allowed_operations ~current_operations ~vM ~vDI ~device
		~userdevice ~bootable ~mode ~_type ~unpluggable ~storage_lock ~empty ~reserved ~other_config
		~currently_attached ~status_code ~status_detail ~runtime_properties ~qos_algorithm_type
		~qos_algorithm_params ~qos_supported_algorithms ~metrics;
	ref

let make_vdi ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(name_label="")
		?(name_description="") ?(allowed_operations=[]) ?(current_operations=[]) ?(sR=Ref.make ())
		?(virtual_size=0L) ?(physical_utilisation=0L) ?(_type=`user) ?(sharable=false) ?(read_only=false)
		?(other_config=[]) ?(storage_lock=false) ?(location="") ?(managed=false) ?(missing=false)
		?(parent=Ref.null) ?(xenstore_data=[]) ?(sm_config=[]) ?(is_a_snapshot=false)
		?(snapshot_of=Ref.null) ?(snapshot_time=Date.never) ?(tags=[]) ?(allow_caching=true)
		?(on_boot=`persist) ?(metadata_of_pool=Ref.make ()) ?(metadata_latest=true) () =
	Db.VDI.create ~__context ~ref ~uuid ~name_label ~name_description ~allowed_operations
		~current_operations ~sR ~virtual_size ~physical_utilisation ~_type ~sharable ~read_only ~other_config
		~storage_lock ~location ~managed ~missing ~parent ~xenstore_data ~sm_config ~is_a_snapshot ~snapshot_of
		~snapshot_time ~tags ~allow_caching ~on_boot ~metadata_of_pool ~metadata_latest;
	ref

let make_pci ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(class_id="")
		?(class_name="") ?(vendor_id="") ?(vendor_name="") ?(device_id="")
		?(device_name="") ?(host=Ref.null) ?(pci_id="") ?(functions=0L)
		?(dependencies=[]) ?(other_config=[]) () =
	Db.PCI.create ~__context ~ref ~uuid ~class_id ~class_name ~vendor_id
		~vendor_name ~device_id ~device_name ~host ~pci_id ~functions ~dependencies
		~other_config;
	ref

let make_pgpu ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(pCI=Ref.null)
		?(gPU_group=Ref.null) ?(host=Ref.null) ?(other_config=[])
		?(size=Constants.pgpu_default_size)
		?(supported_VGPU_types=[]) ?(enabled_VGPU_types=[])
		?(supported_VGPU_max_capacities=[]) () =
	Db.PGPU.create ~__context ~ref ~uuid ~pCI ~gPU_group
		~host ~other_config ~size ~supported_VGPU_max_capacities;
	Db.PGPU.set_supported_VGPU_types ~__context ~self:ref
		~value:supported_VGPU_types;
	Db.PGPU.set_enabled_VGPU_types ~__context ~self:ref
		~value:enabled_VGPU_types;
	ref

let make_gpu_group ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ())
		?(name_label="") ?(name_description="") ?(gPU_types=[]) ?(other_config=[])
		?(allocation_algorithm=`depth_first) () =
	Db.GPU_group.create ~__context ~ref ~uuid ~name_label ~name_description
		~gPU_types ~other_config ~allocation_algorithm;
	ref

let make_vgpu ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(vM=Ref.null)
		?(gPU_group=Ref.null) ?(device="0") ?(currently_attached=false)
		?(other_config=[]) ?(_type=Ref.null) ?(resident_on=Ref.null) () =
	Db.VGPU.create ~__context ~ref ~uuid ~vM ~gPU_group ~device ~currently_attached
		~other_config ~_type ~resident_on;
	ref

let make_vgpu_type ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ())
		?(vendor_name="") ?(model_name="") ?(framebuffer_size=0L) ?(max_heads=0L)
		?(max_resolution_x=0L) ?(max_resolution_y=0L) ?(size=0L)
		?(internal_config=[]) () =
	Db.VGPU_type.create ~__context ~ref ~uuid ~vendor_name ~model_name
		~framebuffer_size ~max_heads ~max_resolution_x ~max_resolution_y ~size
		~internal_config;
	ref
