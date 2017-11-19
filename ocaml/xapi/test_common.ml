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
open Stdext
open Fun
open OUnit

(* A directory to use for temporary files. *)
let working_area = "/tmp/xapi-test"

(** Utility functions *)
let id (x : 'a) : 'a = x

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
      assert_equal ~printer:Test_printers.(list string) ~msg:"Function raised API error with unexpected args" args a

let make_localhost ~__context ?(features=Features.all_features) () =
  let host_info = {
    Create_misc.name_label = "test host";
    xen_verstring = "unknown";
    linux_verstring = "something";
    hostname = "localhost";
    uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid;
    dom0_uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid;
    oem_manufacturer = None;
    oem_model = None;
    oem_build_number = None;
    machine_serial_number = None;
    machine_serial_name = None;
    total_memory_mib = 1024L;
    dom0_static_max = XenopsMemory.bytes_of_mib 512L;
    ssl_legacy = false;
  } in

  Dbsync_slave.create_localhost ~__context host_info;
  (* We'd like to be able to call refresh_localhost_info, but
     	   create_misc is giving me too many headaches right now. Do the
     	   simple thing first and just set localhost_ref instead. *)
  (* Dbsync_slave.refresh_localhost_info ~__context host_info; *)
  Xapi_globs.localhost_ref := Helpers.get_localhost ~__context;
  Db.Host.remove_from_software_version ~__context ~self:!Xapi_globs.localhost_ref ~key:"network_backend";
  Db.Host.add_to_software_version ~__context ~self:!Xapi_globs.localhost_ref ~key:"network_backend"
    ~value:(Network_interface.(string_of_kind Openvswitch));
  Create_misc.ensure_domain_zero_records ~__context ~host:!Xapi_globs.localhost_ref host_info;
  Dbsync_master.create_pool_record ~__context;
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_restrictions ~__context
    ~self:pool
    ~value:(Features.to_assoc_list features)

(** Make a simple in-memory database containing a single host and dom0 VM record. *)
let make_test_database ?(conn=Mock.Database.conn) ?(reuse=false) ?features () =
  let __context = Mock.make_context_with_new_db ~conn ~reuse "mock" in
  Helpers.domain_zero_ref_cache := None;
  make_localhost ~__context ?features ();
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
    ?(snapshot_schedule=Ref.null) ?(is_vmss_snapshot=false)
    ?(shutdown_delay=0L) ?(order=0L) ?(suspend_SR=Ref.null) ?(version=0L)
    ?(generation_id="0:0") ?(hardware_platform_version=0L)
    ?(has_vendor_device=false) ?(has_vendor_device=false) ?(reference_label="")
    ?(domain_type=`hvm) () =
  Xapi_vm.create ~__context ~name_label ~name_description ~user_version ~is_a_template
    ~affinity ~memory_target ~memory_static_max ~memory_dynamic_max ~memory_dynamic_min
    ~memory_static_min ~vCPUs_params ~vCPUs_max ~vCPUs_at_startup ~actions_after_shutdown
    ~actions_after_reboot ~actions_after_crash ~pV_bootloader ~pV_kernel ~pV_ramdisk
    ~pV_args ~pV_bootloader_args ~pV_legacy_args ~hVM_boot_policy ~hVM_boot_params
    ~hVM_shadow_multiplier ~platform ~pCI_bus ~other_config ~xenstore_data ~recommendations
    ~ha_always_run ~ha_restart_priority ~tags ~blocked_operations ~protection_policy
    ~is_snapshot_from_vmpp ~appliance ~start_delay ~shutdown_delay ~order ~suspend_SR
    ~snapshot_schedule ~is_vmss_snapshot
    ~version ~generation_id ~hardware_platform_version ~has_vendor_device ~reference_label
    ~domain_type

let make_host ~__context ?(uuid=make_uuid ()) ?(name_label="host")
    ?(name_description="description") ?(hostname="localhost") ?(address="127.0.0.1")
    ?(external_auth_type="") ?(external_auth_service_name="") ?(external_auth_configuration=[])
    ?(license_params=[]) ?(edition="free") ?(license_server=[]) ?(local_cache_sr=Ref.null) ?(chipset_info=[]) ?(ssl_legacy=false) () =
  Xapi_host.create ~__context ~uuid ~name_label ~name_description ~hostname ~address ~external_auth_type ~external_auth_service_name ~external_auth_configuration ~license_params ~edition ~license_server ~local_cache_sr ~chipset_info ~ssl_legacy

let make_host2 ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(name_label="host")
    ?(name_description="description") ?(hostname="localhost") ?(address="127.0.0.1")
    ?(external_auth_type="") ?(external_auth_service_name="") ?(external_auth_configuration=[])
    ?(license_params=[]) ?(edition="free") ?(license_server=[]) ?(local_cache_sr=Ref.null)
    ?(chipset_info=[]) ?(ssl_legacy=false) () =
  Db.Host.create ~__context ~ref
    ~current_operations:[] ~allowed_operations:[]
    ~software_version:(Xapi_globs.software_version ())
    ~enabled:false
    ~aPI_version_major:Datamodel_common.api_version_major
    ~aPI_version_minor:Datamodel_common.api_version_minor
    ~aPI_version_vendor:Datamodel_common.api_version_vendor
    ~aPI_version_vendor_implementation:Datamodel_common.api_version_vendor_implementation
    ~name_description ~name_label ~uuid ~other_config:[]
    ~capabilities:[]
    ~cpu_configuration:[]
    ~cpu_info:[]
    ~chipset_info
    ~memory_overhead:0L
    ~sched_policy:"credit"
    ~supported_bootloaders:[]
    ~suspend_image_sr:Ref.null ~crash_dump_sr:Ref.null
    ~logging:[] ~hostname ~address ~metrics:Ref.null
    ~license_params ~boot_free_mem:0L
    ~ha_statefiles:[] ~ha_network_peers:[] ~blobs:[] ~tags:[]
    ~external_auth_type
    ~external_auth_service_name
    ~external_auth_configuration
    ~edition ~license_server
    ~bios_strings:[]
    ~power_on_mode:""
    ~power_on_config:[]
    ~local_cache_sr
    ~ssl_legacy
    ~guest_VCPUs_params:[]
    ~display:`enabled
    ~virtual_hardware_platform_versions:[]
    ~control_domain:Ref.null
    ~updates_requiring_reboot:[];
  ref

let make_pif ~__context ~network ~host ?(device="eth0") ?(mAC="C0:FF:EE:C0:FF:EE") ?(mTU=1500L)
    ?(vLAN=(-1L)) ?(physical=true) ?(ip_configuration_mode=`None) ?(iP="") ?(netmask="")
    ?(gateway="") ?(dNS="") ?(bond_slave_of=Ref.null) ?(vLAN_master_of=Ref.null)
    ?(management=false) ?(other_config=[]) ?(disallow_unplug=false)
    ?(ipv6_configuration_mode=`None) ?(iPv6=[]) ?(ipv6_gateway="") ?(primary_address_type=`IPv4) ?(managed=true)
    ?(properties=["gro", "on"]) () =
  Xapi_pif.pool_introduce ~__context
    ~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode
    ~iP ~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug
    ~ipv6_configuration_mode ~iPv6 ~ipv6_gateway ~primary_address_type ~managed ~properties

let make_network ~__context ?(name_label="net") ?(name_description="description") ?(mTU=1500L)
    ?(other_config=[]) ?(bridge="xenbr0") ?(managed=true) ?(purpose=[]) () =
  Xapi_network.pool_introduce ~__context ~name_label ~name_description ~mTU ~other_config ~bridge ~managed ~purpose

let make_vif ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ())
    ?(current_operations=[]) ?(allowed_operations=[]) ?(reserved=false)
    ?(device="") ?(network=Ref.null) ?(vM=Ref.null) ?(mAC="00:00:00:00:00:00")
    ?(mAC_autogenerated=false) ?(mTU=1500L) ?(qos_algorithm_type="")
    ?(qos_algorithm_params=[]) ?(qos_supported_algorithms=[])
    ?(currently_attached=false) ?(status_code=0L) ?(status_detail="")
    ?(runtime_properties=[]) ?(other_config=[]) ?(metrics=Ref.null)
    ?(locking_mode=`unlocked) ?(ipv4_allowed=[]) ?(ipv6_allowed=[])
    ?(ipv4_configuration_mode=`None) ?(ipv4_addresses=[]) ?(ipv4_gateway="")
    ?(ipv6_configuration_mode=`None) ?(ipv6_addresses=[]) ?(ipv6_gateway="") () =
  Db.VIF.create ~__context ~ref ~uuid ~current_operations ~allowed_operations
    ~reserved ~device ~network ~vM ~mAC ~mAC_autogenerated ~mTU
    ~qos_algorithm_type ~qos_algorithm_params ~qos_supported_algorithms
    ~currently_attached ~status_code ~status_detail ~runtime_properties
    ~other_config ~metrics ~locking_mode ~ipv4_allowed ~ipv6_allowed
    ~ipv4_configuration_mode ~ipv4_addresses ~ipv4_gateway
    ~ipv6_configuration_mode ~ipv6_addresses ~ipv6_gateway;
  ref

let make_pool ~__context ~master ?(name_label="") ?(name_description="")
    ?(default_SR=Ref.null) ?(suspend_image_SR=Ref.null) ?(crash_dump_SR=Ref.null)
    ?(ha_enabled=false) ?(ha_configuration=[]) ?(ha_statefiles=[])
    ?(ha_host_failures_to_tolerate=0L) ?(ha_plan_exists_for=0L)
    ?(ha_allow_overcommit=false) ?(ha_overcommitted=false) ?(blobs=[]) ?(tags=[])
    ?(gui_config=[]) ?(health_check_config=[]) ?(wlb_url="") ?(wlb_username="") ?(wlb_password=Ref.null)
    ?(wlb_enabled=false) ?(wlb_verify_cert=false) ?(redo_log_enabled=false)
    ?(redo_log_vdi=Ref.null) ?(vswitch_controller="") ?(igmp_snooping_enabled=false) ?(restrictions=[])
    ?(current_operations=[]) ?(allowed_operations=[])
    ?(other_config=[Xapi_globs.memory_ratio_hvm; Xapi_globs.memory_ratio_pv])
    ?(ha_cluster_stack="xhad") ?(guest_agent_config=[]) ?(cpu_info=[]) ?(policy_no_vendor_device=false) ?(live_patching_disabled=false)() =
  let pool_ref = Ref.make () in
  Db.Pool.create ~__context ~ref:pool_ref
    ~uuid:(make_uuid ()) ~name_label ~name_description
    ~master ~default_SR ~suspend_image_SR ~crash_dump_SR ~ha_enabled
    ~ha_configuration ~ha_statefiles ~ha_host_failures_to_tolerate
    ~ha_plan_exists_for ~ha_allow_overcommit ~ha_overcommitted ~blobs ~tags
    ~gui_config ~health_check_config ~wlb_url ~wlb_username ~wlb_password ~wlb_enabled
    ~wlb_verify_cert ~redo_log_enabled ~redo_log_vdi ~vswitch_controller ~igmp_snooping_enabled
    ~current_operations ~allowed_operations
    ~restrictions ~other_config ~ha_cluster_stack ~guest_agent_config ~cpu_info ~policy_no_vendor_device ~live_patching_disabled;
  pool_ref

let default_sm_features = [
  "SR_PROBE", 1L;
  "SR_UPDATE", 1L;
  "VDI_CREATE", 1L;
  "VDI_DELETE", 1L;
  "VDI_ATTACH", 1L;
  "VDI_DETACH", 1L;
  "VDI_UPDATE", 1L;
  "VDI_CLONE", 1L;
  "VDI_SNAPSHOT", 1L;
  "VDI_RESIZE", 1L;
  "VDI_GENERATE_CONFIG", 1L;
  "VDI_RESET_ON_BOOT", 2L;
  "VDI_CONFIG_CBT", 1L;
]

let make_sm ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(_type="sm")
    ?(name_label="") ?(name_description="") ?(vendor="") ?(copyright="")
    ?(version="") ?(required_api_version="") ?(capabilities=[]) ?(features=default_sm_features)
    ?(configuration=[]) ?(other_config=[]) ?(driver_filename="/dev/null")
    ?(required_cluster_stack=[]) () =
  Db.SM.create ~__context ~ref:ref ~uuid ~_type ~name_label ~name_description
    ~vendor ~copyright ~version ~required_api_version ~capabilities ~features
    ~configuration ~other_config ~driver_filename ~required_cluster_stack;
  ref

let make_sr ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(name_label="") ?(name_description="") ?(allowed_operations=[])
    ?(current_operations=[]) ?(virtual_allocation=0L) ?(physical_utilisation=0L) ?(physical_size=0L) ?(_type="sm")
    ?(content_type="") ?(shared=true) ?(other_config=[]) ?(tags=[]) ?(default_vdi_visibility=true)
    ?(sm_config=[]) ?(blobs=[]) ?(local_cache_enabled=false) ?(introduced_by=Ref.make ()) ?(clustered=false)
    ?(is_tools_sr=false)() =
  Db.SR.create ~__context ~ref ~uuid ~name_label ~name_description ~allowed_operations
    ~current_operations ~virtual_allocation ~physical_utilisation ~physical_size ~_type
    ~content_type ~shared ~other_config ~tags ~default_vdi_visibility ~sm_config ~blobs
    ~local_cache_enabled ~introduced_by ~clustered ~is_tools_sr;
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
    ?(snapshot_of=Ref.null) ?(snapshot_time=API.Date.never) ?(tags=[]) ?(allow_caching=true)
    ?(on_boot=`persist) ?(metadata_of_pool=Ref.make ()) ?(metadata_latest=true) ?(is_tools_iso=false) ?(cbt_enabled=false) () =
  Db.VDI.create ~__context ~ref ~uuid ~name_label ~name_description ~allowed_operations
    ~current_operations ~sR ~virtual_size ~physical_utilisation ~_type ~sharable ~read_only ~other_config
    ~storage_lock ~location ~managed ~missing ~parent ~xenstore_data ~sm_config ~is_a_snapshot ~snapshot_of
    ~snapshot_time ~tags ~allow_caching ~on_boot ~metadata_of_pool ~metadata_latest ~is_tools_iso ~cbt_enabled;
  ref

let make_pci ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(class_id="")
    ?(class_name="") ?(vendor_id="") ?(vendor_name="") ?(device_id="")
    ?(device_name="") ?(host=Ref.null) ?(pci_id="0000:00:00.0") ?(functions=0L)
    ?(physical_function=Ref.null) ?(dependencies=[]) ?(other_config=[]) ?(subsystem_vendor_id="")
    ?(subsystem_vendor_name="") ?(subsystem_device_id="")
    ?(subsystem_device_name="") ?(scheduled_to_be_attached_to=Ref.null) () =
  Db.PCI.create ~__context ~ref ~uuid ~class_id ~class_name ~vendor_id
    ~vendor_name ~device_id ~device_name ~host ~pci_id ~functions ~physical_function
    ~dependencies ~other_config ~subsystem_vendor_id ~subsystem_vendor_name
    ~subsystem_device_id ~subsystem_device_name ~scheduled_to_be_attached_to;
  ref

let make_pgpu ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(pCI=Ref.null)
    ?(gPU_group=Ref.null) ?(host=Ref.null) ?(other_config=[])
    ?(size=Constants.pgpu_default_size)
    ?(supported_VGPU_types=[]) ?(enabled_VGPU_types=[])
    ?(supported_VGPU_max_capacities=[]) ?(dom0_access=`enabled)
    ?(is_system_display_device=false) () =
  Db.PGPU.create ~__context ~ref ~uuid ~pCI ~gPU_group
    ~host ~other_config ~size ~supported_VGPU_max_capacities ~dom0_access
    ~is_system_display_device ~compatibility_metadata:[];
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

let make_vgpu ~__context
    ?(ref=Ref.make ())
    ?(uuid=make_uuid ())
    ?(vM=Ref.null)
    ?(gPU_group=Ref.null)
    ?(device="0")
    ?(currently_attached=false)
    ?(other_config=[])
    ?(_type=Ref.null)
    ?(resident_on=Ref.null)
    ?(scheduled_to_be_resident_on=Ref.null)
    ?(compatibility_metadata=[])
    () =
  Db.VGPU.create ~__context
    ~ref ~uuid ~vM ~gPU_group ~device ~currently_attached
    ~other_config ~_type ~resident_on ~scheduled_to_be_resident_on
    ~compatibility_metadata
    ;
  ref

let make_vgpu_type ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ())
    ?(vendor_name="") ?(model_name="") ?(framebuffer_size=0L) ?(max_heads=0L)
    ?(max_resolution_x=0L) ?(max_resolution_y=0L) ?(size=0L)
    ?(internal_config=[]) ?(implementation=`passthrough)
    ?(identifier="") ?(experimental=false) () =
  Db.VGPU_type.create ~__context ~ref ~uuid ~vendor_name ~model_name
    ~framebuffer_size ~max_heads ~max_resolution_x ~max_resolution_y ~size
    ~internal_config ~implementation ~identifier ~experimental;
  ref

let make_pvs_site ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ())
    ?(name_label="") ?(name_description="") ?(pVS_uuid="") ?(cache_storage=[]) () =
  Db.PVS_site.create ~__context ~ref ~uuid ~name_label ~name_description
    ~pVS_uuid ~cache_storage;
  ref

let make_pvs_proxy ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ())
    ?(site=Ref.null) ?(vIF=Ref.null)
    ?(currently_attached=false) ?(status=`stopped) () =
  Db.PVS_proxy.create ~__context
    ~ref ~uuid ~site ~vIF ~currently_attached ~status;
  ref

let make_pvs_server ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ())
    ?(addresses=[]) ?(first_port=1L) ?(last_port=65535L) ?(site=Ref.null) () =
  Db.PVS_server.create ~__context
    ~addresses ~ref ~uuid ~first_port ~last_port ~site;
  ref

let make_pvs_cache_storage ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ())
    ?(host=Ref.null) ?(sR=Ref.null) ?(site=Ref.null) ?(size=0L) ?(vDI=Ref.null) () =
  Db.PVS_cache_storage.create ~__context
    ~ref ~uuid ~host ~sR ~site ~size ~vDI;
  ref

let make_pool_update ~__context
    ?(ref=Ref.make ())
    ?(uuid=make_uuid ())
    ?(name_label="")
    ?(name_description="")
    ?(version="")
    ?(installation_size=0L)
    ?(key="")
    ?(after_apply_guidance=[])
    ?(enforce_homogeneity=false)
    ?(other_config=[])
    ?(vdi=Ref.null) () =
  let update_info = Xapi_pool_update.
    { uuid
    ; name_label
    ; name_description
    ; version
    ; key
    ; installation_size
    ; after_apply_guidance
    ; other_config
    ; enforce_homogeneity
    } in
  Xapi_pool_update.create_update_record ~__context ~update:ref ~update_info ~vdi;
  ref

let make_session ~__context ?(ref=Ref.make ()) ?(uuid=make_uuid ()) ?(this_host=Ref.null) ?(this_user=Ref.null) ?(last_active=API.Date.never) ?(pool=false) ?(other_config=[]) ?(is_local_superuser=false) ?(subject=Ref.null) ?(validation_time=API.Date.never) ?(auth_user_sid="") ?(auth_user_name="") ?(rbac_permissions=[]) ?(parent=Ref.null) ?(originator="test") () =
  Db.Session.create ~__context ~ref ~uuid ~this_host ~this_user ~last_active ~pool ~other_config ~is_local_superuser ~subject ~validation_time ~auth_user_sid ~auth_user_name ~rbac_permissions ~parent ~originator;
  ref

(** Returns a [(rpc, session_id)] pair that can be passed to the
    functions within the [Client] module to make XenAPI calls. The
    calls can only succeed if they get forwarded to the local host
    by the message forwarding layer. Forwarding to slaves does not
    work in unit tests. *)
let make_client_params ~__context =
  let req = Xmlrpc_client.xmlrpc ~version:"1.1" "/" in
  let rpc = Api_server.Server.dispatch_call req Unix.stdout in
  let session_id =
    let session_id = Ref.make () in
    let now = Stdext.Date.of_float (Unix.time ()) in
    let _: _ API.Ref.t = make_session ~__context ~ref:session_id ~this_host:(Helpers.get_localhost ~__context) ~last_active:now ~is_local_superuser:true ~validation_time:now ~auth_user_name:"root" ~originator:"test" () in
    session_id
  in
  (rpc, session_id)
