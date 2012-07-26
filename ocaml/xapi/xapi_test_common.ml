(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

(** Make a simple in-memory database containing a single host and dom0 VM record. *)
let make_test_database () = 
	let db = Db_upgrade.generic_database_upgrade (Db_cache_types.Database.make (Datamodel_schema.of_datamodel ())) in
	let db_ref = Db_ref.in_memory (ref (ref db)) in
	let __context = Context.make ~database:db_ref "upgrade_vm_memory_for_dmc" in

	(* Db_xml.To.file "/tmp/new3.db" (Db_ref.get_database (Context.database_of __context)); *)

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
		?(shutdown_delay=0L) ?(order=0L) ?(suspend_SR=Ref.null) ?(version=0L) () = 
	Xapi_vm.create ~__context ~name_label ~name_description ~user_version ~is_a_template
		~affinity ~memory_target ~memory_static_max ~memory_dynamic_max ~memory_dynamic_min
        ~memory_static_min ~vCPUs_params ~vCPUs_max ~vCPUs_at_startup ~actions_after_shutdown 
		~actions_after_reboot ~actions_after_crash ~pV_bootloader ~pV_kernel ~pV_ramdisk 
		~pV_args ~pV_bootloader_args ~pV_legacy_args ~hVM_boot_policy ~hVM_boot_params 
		~hVM_shadow_multiplier ~platform ~pCI_bus ~other_config ~xenstore_data ~recommendations
		~ha_always_run ~ha_restart_priority ~tags ~blocked_operations ~protection_policy
		~is_snapshot_from_vmpp ~appliance ~start_delay ~shutdown_delay ~order ~suspend_SR ~version

let make_host ~__context ?(uuid=Uuid.string_of_uuid (Uuid.insecure ())) ?(name_label="host")
		?(name_description="description") ?(hostname="localhost") ?(address="127.0.0.1")
		?(external_auth_type="") ?(external_auth_service_name="") ?(external_auth_configuration=[])
		?(license_params=[]) ?(edition="free") ?(license_server=[]) ?(local_cache_sr=Ref.null) ?(chipset_info=[]) () = 

	Xapi_host.create ~__context ~uuid ~name_label ~name_description ~hostname ~address ~external_auth_type ~external_auth_service_name ~external_auth_configuration ~license_params ~edition ~license_server ~local_cache_sr ~chipset_info

let make_pif ~__context ~network ~host ?(device="eth0") ?(mAC="C0:FF:EE:C0:FF:EE") ?(mTU=1500L)
		?(vLAN=(-1L)) ?(physical=true) ?(ip_configuration_mode=`None) ?(iP="") ?(netmask="")
		?(gateway="") ?(dNS="") ?(bond_slave_of=Ref.null) ?(vLAN_master_of=Ref.null) 
		?(management=false) ?(other_config=[]) ?(disallow_unplug=false) 
                ?(ipv6_configuration_mode=`None) ?(iPv6=[""]) ?(ipv6_gateway="") ?(primary_address_type=`IPv4) () =
	Xapi_pif.pool_introduce ~__context
		~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode
		~iP ~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug
                ~ipv6_configuration_mode ~iPv6 ~ipv6_gateway ~primary_address_type

let make_network ~__context ?(name_label="net") ?(name_description="description") ?(mTU=1500L)
		?(other_config=[]) ?(bridge="xenbr0") () = 
	Xapi_network.pool_introduce ~__context ~name_label ~name_description ~mTU ~other_config ~bridge


