(* Datamodel errors *)

(* Errors and messages *)


let errors : (string, Datamodel_types.error) Hashtbl.t = Hashtbl.create 10
let messages : (string, Datamodel_types.mess) Hashtbl.t = Hashtbl.create 10

let error name params ?(doc="") () =
  Hashtbl.add errors name {
    err_name = name;
    err_params = params;
    err_doc = doc;
  }

let message name ?(doc="") () =
  Hashtbl.add messages name {
    mess_name = name;
    mess_doc = doc;
  }

let _ =
  (* Internal *)
  error Api_errors.internal_error ["message"]
    ~doc:"The server failed to handle your request, due to an internal error.  The given message may give details useful for debugging the problem." ();

  error Api_errors.message_deprecated []
    ~doc:"This message has been deprecated." ();
  error Api_errors.message_removed []
    ~doc:"This function is no longer available." ();

  error Api_errors.permission_denied ["message"]
    ~doc:"Caller not allowed to perform this operation." ();

  (* Generic errors *)
  (* licensed expired - copied from geneva *)
  error Api_errors.license_expired []
    ~doc:"Your license has expired.  Please contact your support representative." ();
  error Api_errors.license_processing_error []
    ~doc:"There was an error processing your license.  Please contact your support representative." ();
  error Api_errors.license_restriction ["feature"]
    ~doc:"This operation is not allowed because your license lacks a needed feature.  Please contact your support representative." ();
  error Api_errors.license_cannot_downgrade_in_pool []
    ~doc:"Cannot downgrade license while in pool. Please disband the pool first, then downgrade licenses on hosts separately." ();
  error Api_errors.license_does_not_support_pooling []
    ~doc:"This host cannot join a pool because its license does not support pooling." ();
  error Api_errors.license_host_pool_mismatch []
    ~doc:"Host and pool have incompatible licenses (editions)." ();
  error Api_errors.license_does_not_support_xha []
    ~doc:"XHA cannot be enabled because this host's license does not allow it." ();

  error Api_errors.v6d_failure []
    ~doc:"There was a problem with the license daemon (v6d)." ();
  error Api_errors.invalid_edition ["edition"]
    ~doc:"The edition you supplied is invalid." ();
  error Api_errors.missing_connection_details []
    ~doc:"The license-server connection details (address or port) were missing or incomplete." ();
  error Api_errors.license_checkout_error ["reason"]
    ~doc:"The license for the edition you requested is not available." ();
  error Api_errors.license_file_deprecated []
    ~doc:"This license file is no longer accepted. Please upgrade to the new licensing system." ();
  error Api_errors.activation_while_not_free []
    ~doc:"An activation key can only be applied when the edition is set to 'free'." ();

  error Api_errors.feature_restricted []
    ~doc:"The use of this feature is restricted." ();

  error Api_errors.cannot_contact_host ["host"]
    ~doc:"Cannot forward messages because the host cannot be contacted.  The host may be switched off or there may be network connectivity problems." ();

  error Api_errors.tls_connection_failed ["address"; "port"]
    ~doc:"Cannot contact the other host using TLS on the specified address and port" ();

  error Api_errors.uuid_invalid [ "type"; "uuid" ]
    ~doc:"The uuid you supplied was invalid." ();
  error Api_errors.object_nolonger_exists []
    ~doc:"The specified object no longer exists." ();
  error Api_errors.map_duplicate_key ["type"; "param_name"; "uuid"; "key"]
    ~doc:"You tried to add a key-value pair to a map, but that key is already there." ();
  error Api_errors.xmlrpc_unmarshal_failure [ "expected"; "received" ]
    ~doc:"The server failed to unmarshal the XMLRPC message; it was expecting one element and received something else." ();

  error Api_errors.message_method_unknown ["method"]
    ~doc:"You tried to call a method that does not exist.  The method name that you used is echoed." ();
  error Api_errors.message_parameter_count_mismatch ["method"; "expected"; "received"]
    ~doc:"You tried to call a method with the incorrect number of parameters.  The fully-qualified method name that you used, and the number of received and expected parameters are returned." ();
  error Api_errors.value_not_supported ["field"; "value"; "reason"]
    ~doc:"You attempted to set a value that is not supported by this implementation.  The fully-qualified field name and the value that you tried to set are returned.  Also returned is a developer-only diagnostic reason." ();
  error Api_errors.invalid_value ["field"; "value"]
    ~doc:"The value given is invalid" ();
  error Api_errors.field_type_error ["field"]
    ~doc:"The value specified is of the wrong type" ();
  error Api_errors.operation_not_allowed ["reason"]
    ~doc:"You attempted an operation that was not allowed." ();
  error Api_errors.operation_blocked ["ref"; "code"]
    ~doc:"You attempted an operation that was explicitly blocked (see the blocked_operations field of the given object)." ();
  error Api_errors.not_implemented ["function"]
    ~doc:"The function is not implemented" ();
  error Api_errors.unimplemented_in_sm_backend ["message"]
    ~doc:"You have attempted a function which is not implemented" ();
  (* DB errors *)
  error Api_errors.handle_invalid ["class"; "handle"]
    ~doc:"You gave an invalid object reference.  The object may have recently been deleted.  The class parameter gives the type of reference given, and the handle parameter echoes the bad value given." ();
  error Api_errors.db_uniqueness_constraint_violation ["table";"field";"value"]
    ~doc:"You attempted an operation which would have resulted in duplicate keys in the database." ();
  error Api_errors.location_not_unique ["SR"; "location"]
    ~doc:"A VDI with the specified location already exists within the SR" ();
  error Api_errors.memory_constraint_violation ["constraint"]
    ~doc:"The dynamic memory range does not satisfy the following constraint." ();

  (* Session errors *)
  error Api_errors.session_authentication_failed []
    ~doc:"The credentials given by the user are incorrect, so access has been denied, and you have not been issued a session handle." ();
  error Api_errors.session_invalid ["handle"]
    ~doc:"You gave an invalid session reference.  It may have been invalidated by a server restart, or timed out.  You should get a new session handle, using one of the session.login_ calls.  This error does not invalidate the current connection.  The handle parameter echoes the bad value given." ();
  error Api_errors.change_password_rejected [ "msg" ]
    ~doc:"The system rejected the password change request; perhaps the new password was too short?" ();
  error Api_errors.user_is_not_local_superuser [ "msg" ]
    ~doc:"Only the local superuser can execute this operation" ();

  (* SR-IOV errors *)
  error Api_errors.network_sriov_insufficient_capacity [ "network" ]
    ~doc:"There is insufficient capacity for VF reservation" ();
  error Api_errors.network_sriov_already_enabled ["PIF"]
    ~doc:"The PIF selected for the SR-IOV network is already enabled" ();
  error Api_errors.network_sriov_enable_failed ["PIF"; "msg"]
    ~doc:"Failed to enable SR-IOV on PIF" ();
  error Api_errors.network_sriov_disable_failed ["PIF"; "msg"]
    ~doc:"Failed to disable SR-IOV on PIF" ();
  error Api_errors.network_has_incompatible_sriov_pifs ["PIF"; "network"]
    ~doc:"The PIF is not compatible with the selected SR-IOV network" ();
  error Api_errors.network_has_incompatible_vlan_on_sriov_pifs ["PIF"; "network"]
    ~doc:"VLAN on the PIF is not compatible with the selected SR-IOV VLAN network" ();
  error Api_errors.network_incompatible_with_sriov ["network"]
    ~doc:"The network is incompatible with sriov" ();
  error Api_errors.network_incompatible_with_vlan_on_bridge ["network"]
    ~doc:"The network is incompatible with vlan on bridge" ();
  error Api_errors.network_incompatible_with_vlan_on_sriov ["network"]
    ~doc:"The network is incompatible with vlan on sriov" ();
  error Api_errors.network_incompatible_with_bond ["network"]
    ~doc:"The network is incompatible with bond" ();
  error Api_errors.network_incompatible_with_tunnel ["network"]
    ~doc:"The network is incompatible with tunnel" ();
  error Api_errors.pool_joining_host_has_network_sriovs []
    ~doc:"The host joining the pool must not have any network SR-IOVs." ();

  (* PIF/VIF/Network errors *)
  error Api_errors.network_unmanaged [ "network" ]
    ~doc:"The network is not managed by xapi." ();
  error Api_errors.network_already_connected ["network"; "connected PIF"]
    ~doc:"You tried to create a PIF, but the network you tried to attach it to is already attached to some other PIF, and so the creation failed." ();
  error Api_errors.cannot_destroy_system_network [ "network" ]
    ~doc:"You tried to destroy a system network: these cannot be destroyed." ();
  error Api_errors.network_incompatible_purposes ["new_purpose"; "conflicting_purpose"]
    ~doc:"You tried to add a purpose to a network but the new purpose is not compatible with an existing purpose of the network or other networks." ();
  error Api_errors.pif_is_physical ["PIF"]
    ~doc:"You tried to destroy a PIF, but it represents an aspect of the physical host configuration, and so cannot be destroyed.  The parameter echoes the PIF handle you gave." ();
  error Api_errors.pif_is_not_physical ["PIF"]
    ~doc:"You tried to perform an operation which is only available on physical PIF" ();
  error Api_errors.pif_is_vlan ["PIF"]
    ~doc:"You tried to create a VLAN on top of another VLAN - use the underlying physical PIF/bond instead" ();
  error Api_errors.pif_is_sriov_logical ["PIF"]
    ~doc:"You tried to create a bond on top of a network SR-IOV logical PIF - use the underlying physical PIF instead" ();
  error Api_errors.pif_vlan_exists ["PIF"]
    ~doc:"You tried to create a PIF, but it already exists." ();
  error Api_errors.pif_vlan_still_exists [ "PIF" ]
    ~doc:"Operation cannot proceed while a VLAN exists on this interface." ();
  error Api_errors.vlan_in_use ["device"; "vlan"]
    ~doc:"Operation cannot be performed because this VLAN is already in use. Please check your network configuration." ();
  error Api_errors.pif_already_bonded [ "PIF" ]
    ~doc:"This operation cannot be performed because the pif is bonded." ();
  error Api_errors.pif_cannot_bond_cross_host []
    ~doc:"You cannot bond interfaces across different hosts." ();
  error Api_errors.pif_bond_needs_more_members []
    ~doc:"A bond must consist of at least two member interfaces" ();
  error Api_errors.pif_bond_more_than_one_ip []
    ~doc:"Only one PIF on a bond is allowed to have an IP configuration." ();
  error Api_errors.pif_configuration_error [ "PIF"; "msg" ]
    ~doc:"An unknown error occurred while attempting to configure an interface." ();
  error Api_errors.invalid_ip_address_specified [ "parameter" ]
    ~doc:"A required parameter contained an invalid IP address" ();
  error Api_errors.invalid_cidr_address_specified [ "parameter" ]
    ~doc:"A required parameter contained an invalid CIDR address (<addr>/<prefix length>)" ();
  error Api_errors.address_violates_locking_constraint [ "address" ]
    ~doc:"The specified IP address violates the VIF locking configuration." ();
  error Api_errors.pif_is_management_iface [ "PIF" ]
    ~doc:"The operation you requested cannot be performed because the specified PIF is the management interface." ();
  error Api_errors.pif_not_present ["host"; "network"]
    ~doc:"This host has no PIF on the given network." ();
  error Api_errors.pif_does_not_allow_unplug [ "PIF" ]
    ~doc:"The operation you requested cannot be performed because the specified PIF does not allow unplug." ();
  error Api_errors.pif_allows_unplug [ "PIF" ]
    ~doc:"The operation you requested cannot be performed because the specified PIF allows unplug." ();
  error Api_errors.required_pif_is_unplugged [ "PIF" ]
    ~doc:"The operation you requested cannot be performed because the specified PIF is currently unplugged." ();
  error Api_errors.pif_has_fcoe_sr_in_use ["PIF"; "SR"]
    ~doc:"The operation you requested cannot be performed because the specified PIF has FCoE SR in use." ();
  error Api_errors.pif_unmanaged [ "PIF" ]
    ~doc:"The operation you requested cannot be performed because the specified PIF is not managed by xapi." ();
  error Api_errors.pif_is_not_sriov_capable [ "PIF" ]
    ~doc:"The selected PIF is not capable of network SR-IOV" ();
  error Api_errors.pif_has_no_network_configuration [ "PIF" ]
    ~doc:"PIF has no IP configuration (mode currently set to 'none')" ();
  error Api_errors.pif_has_no_v6_network_configuration [ "PIF" ]
    ~doc:"PIF has no IPv6 configuration (mode currently set to 'none')" ();
  error Api_errors.pif_incompatible_primary_address_type [ "PIF" ]
    ~doc:"The primary address types are not compatible" ();
  error Api_errors.pif_sriov_still_exists [ "PIF" ]
    ~doc:"The PIF is still related with a network SR-IOV" ();
  error Api_errors.cannot_plug_bond_slave ["PIF"]
    ~doc:"This PIF is a bond slave and cannot be plugged." ();
  error Api_errors.cannot_add_vlan_to_bond_slave ["PIF"]
    ~doc:"This PIF is a bond slave and cannot have a VLAN on it." ();
  error Api_errors.cannot_add_tunnel_to_bond_slave ["PIF"]
    ~doc:"This PIF is a bond slave and cannot have a tunnel on it." ();
  error Api_errors.cannot_add_tunnel_to_sriov_logical ["PIF"]
    ~doc:"This is a network SR-IOV logical PIF and cannot have a tunnel on it." ();
  error Api_errors.cannot_add_tunnel_to_vlan_on_sriov_logical ["PIF"]
    ~doc:"This is a vlan PIF on network SR-IOV and cannot have a tunnel on it." ();
  error Api_errors.cannot_change_pif_properties ["PIF"]
    ~doc:"This properties of this PIF cannot be changed. Only the properties of non-bonded physical PIFs, or bond masters can be changed." ();
  error Api_errors.cannot_forget_sriov_logical [ "PIF" ]
    ~doc:"This is a network SR-IOV logical PIF and cannot do forget on it" ();
  error Api_errors.incompatible_pif_properties []
    ~doc:"These PIFs cannot be bonded, because their properties are different." ();
  error Api_errors.slave_requires_management_iface []
    ~doc:"The management interface on a slave cannot be disabled because the slave would enter emergency mode." ();
  error Api_errors.vif_in_use [ "network"; "VIF" ]
    ~doc:"Network has active VIFs" ();
  error Api_errors.cannot_plug_vif [ "VIF" ]
    ~doc:"Cannot plug VIF" ();

  error Api_errors.mac_does_not_exist [ "MAC" ]
    ~doc:"The MAC address specified doesn't exist on this host." ();
  error Api_errors.mac_still_exists [ "MAC" ]
    ~doc:"The MAC address specified still exists on this host." ();
  error Api_errors.mac_invalid [ "MAC" ]
    ~doc:"The MAC address specified is not valid." ();
  error Api_errors.duplicate_pif_device_name [ "device" ]
    ~doc:"A PIF with this specified device name already exists." ();
  error Api_errors.could_not_find_network_interface_with_specified_device_name_and_mac_address [ "device"; "mac" ]
    ~doc:"Could not find a network interface with the specified device name and MAC address." ();

  error Api_errors.vlan_tag_invalid ["VLAN"]
    ~doc:"You tried to create a VLAN, but the tag you gave was invalid -- it must be between 0 and 4094.  The parameter echoes the VLAN tag you gave." ();
  error Api_errors.network_contains_vif ["vifs"]
    ~doc:"The network contains active VIFs and cannot be deleted." ();
  error Api_errors.network_contains_pif ["pifs"]
    ~doc:"The network contains active PIFs and cannot be deleted." ();
  error Api_errors.gpu_group_contains_vgpu ["vgpus"]
    ~doc:"The GPU group contains active VGPUs and cannot be deleted." ();
  error Api_errors.gpu_group_contains_pgpu ["pgpus"]
    ~doc:"The GPU group contains active PGPUs and cannot be deleted." ();
  error Api_errors.gpu_group_contains_no_pgpus ["gpu_group"]
    ~doc:"The GPU group does not contain any PGPUs." ();
  error Api_errors.device_attach_timeout [ "type"; "ref" ]
    ~doc:"A timeout happened while attempting to attach a device to a VM." ();
  error Api_errors.device_detach_timeout [ "type"; "ref" ]
    ~doc:"A timeout happened while attempting to detach a device from a VM." ();
  error Api_errors.device_detach_rejected [ "type"; "ref"; "msg" ]
    ~doc:"The VM rejected the attempt to detach the device." ();
  error Api_errors.device_not_attached [ "VBD" ]
    ~doc:"The operation could not be performed because the VBD was not connected to the VM." ();
  error Api_errors.pif_device_not_found []
    ~doc:"The specified device was not found." ();

  error Api_errors.pgpu_in_use_by_vm ["VMs"]
    ~doc:"This PGPU is currently in use by running VMs." ();
  error Api_errors.pgpu_not_compatible_with_gpu_group ["type"; "group_types"]
    ~doc:"PGPU type not compatible with destination group." ();
  error Api_errors.pgpu_insufficient_capacity_for_vgpu ["pgpu"; "vgpu_type"]
    ~doc:"There is insufficient capacity on this PGPU to run the VGPU." ();
  error Api_errors.vgpu_type_not_enabled ["type"; "enabled_types"]
    ~doc:"VGPU type is not one of the PGPU's enabled types." ();
  error Api_errors.vgpu_type_not_supported ["type"; "supported_types"]
    ~doc:"VGPU type is not one of the PGPU's supported types." ();
  error Api_errors.vgpu_type_not_compatible_with_running_type ["pgpu"; "type"; "running_type"]
    ~doc:"VGPU type is not compatible with one or more of the VGPU types currently running on this PGPU" ();
  error Api_errors.vgpu_destination_incompatible ["reason"; "vgpu"; "host"]
    ~doc:"The VGPU is not compatible with any PGPU in the destination." ();
  error Api_errors.nvidia_tools_error ["host"]
    ~doc:"Nvidia tools error. Please ensure that the latest Nvidia tools are installed" ();

  error Api_errors.openvswitch_not_active []
    ~doc:"This operation needs the OpenVSwitch networking backend to be enabled on all hosts in the pool." ();
  error Api_errors.transport_pif_not_configured ["PIF"]
    ~doc:"The tunnel transport PIF has no IP configuration set." ();
  error Api_errors.is_tunnel_access_pif ["PIF"]
    ~doc:"You tried to create a VLAN or tunnel on top of a tunnel access PIF - use the underlying transport PIF instead." ();
  error Api_errors.pif_tunnel_still_exists ["PIF"]
    ~doc:"Operation cannot proceed while a tunnel exists on this interface." ();
  error Api_errors.bridge_not_available [ "bridge" ]
    ~doc:"Could not find bridge required by VM." ();
  error Api_errors.bridge_name_exists [ "bridge" ]
    ~doc:"The specified bridge already exists." ();

  (* VM specific errors *)
  error Api_errors.vm_is_protected [ "vm" ]
    ~doc:"This operation cannot be performed because the specified VM is protected by xHA" ();
  error Api_errors.vm_no_crashdump_sr ["vm"]
    ~doc:"This VM does not have a crashdump SR specified." ();
  error Api_errors.vm_no_suspend_sr ["vm"]
    ~doc:"This VM does not have a suspend SR specified." ();
  error Api_errors.vm_memory_size_too_low ["vm"]
    ~doc:"The specified VM has too little memory to be started." ();
  error Api_errors.vm_duplicate_vbd_device [ "vm"; "vbd"; "device" ]
    ~doc:"The specified VM has a duplicate VBD device and cannot be started." ();
  error Api_errors.illegal_vbd_device [ "vbd"; "device" ]
    ~doc:"The specified VBD device is not recognized: please use a non-negative integer" ();
  error Api_errors.vm_not_resident_here [ "vm"; "host" ]
    ~doc:"The specified VM is not currently resident on the specified host." ();
  error Api_errors.domain_exists [ "vm"; "domid" ]
    ~doc:"The operation could not be performed because a domain still exists for the specified VM." ();
  error Api_errors.cannot_reset_control_domain [ "vm" ]
    ~doc:"The power-state of a control domain cannot be reset." ();
  error Api_errors.not_system_domain [ "vm" ]
    ~doc:"The given VM is not registered as a system domain. This operation can only be performed on a registered system domain." ();
  error Api_errors.vm_cannot_delete_default_template ["vm"]
    ~doc:"You cannot delete the specified default template." ();
  error Api_errors.vm_bad_power_state ["vm"; "expected"; "actual"]
    ~doc:"You attempted an operation on a VM that was not in an appropriate power state at the time; for example, you attempted to start a VM that was already running.  The parameters returned are the VM's handle, and the expected and actual VM state at the time of the call." ();
  error Api_errors.vm_missing_pv_drivers [ "vm" ]
    ~doc:"You attempted an operation on a VM which requires PV drivers to be installed but the drivers were not detected." ();
  error Api_errors.vm_old_pv_drivers [ "vm"; "major"; "minor" ]
    ~doc:"You attempted an operation on a VM which requires a more recent version of the PV drivers. Please upgrade your PV drivers." ();
  error Api_errors.vm_lacks_feature_shutdown [ "vm" ]
    ~doc:"You attempted an operation which needs the cooperative shutdown feature on a VM which lacks it." ();
  error Api_errors.vm_lacks_feature_vcpu_hotplug [ "vm" ]
    ~doc:"You attempted an operation which needs the VM hotplug-vcpu feature on a VM which lacks it." ();
  error Api_errors.vm_lacks_feature_suspend [ "vm" ]
    ~doc:"You attempted an operation which needs the VM cooperative suspend feature on a VM which lacks it." ();
  error Api_errors.vm_lacks_feature_static_ip_setting [ "vm" ]
    ~doc:"You attempted an operation which needs the VM static-ip-setting feature on a VM which lacks it." ();
  error Api_errors.vm_lacks_feature [ "vm" ]
    ~doc:"You attempted an operation on a VM which lacks the feature." ();
  error Api_errors.vm_is_template ["vm"]
    ~doc:"The operation attempted is not valid for a template VM" ();
  error Api_errors.other_operation_in_progress ["class"; "object"]
    ~doc:"Another operation involving the object is currently in progress" ();
  error Api_errors.vbd_not_removable_media ["vbd"]
    ~doc:"Media could not be ejected because it is not removable" ();
  error Api_errors.vbd_not_unpluggable ["vbd"]
    ~doc:"Drive could not be hot-unplugged because it is not marked as unpluggable" ();
  error Api_errors.vbd_not_empty ["vbd"]
    ~doc:"Operation could not be performed because the drive is not empty" ();
  error Api_errors.vbd_is_empty ["vbd"]
    ~doc:"Operation could not be performed because the drive is empty" ();
  error Api_errors.vbd_tray_locked ["vbd"]
    ~doc:"This VM has locked the DVD drive tray, so the disk cannot be ejected" ();
  error Api_errors.vbd_cds_must_be_readonly [ ]
    ~doc:"Read/write CDs are not supported" ();
  (* CA-83260 *)
  error Api_errors.disk_vbd_must_be_readwrite_for_hvm ["vbd"]
    ~doc:"All VBDs of type 'disk' must be read/write for HVM guests" ();
  error Api_errors.vm_no_empty_cd_vbd ["vm"]
    ~doc:"The VM has no empty CD drive (VBD)." ();
  error Api_errors.vm_hvm_required ["vm"]
    ~doc:"HVM is required for this operation" ();
  error Api_errors.vm_no_vcpus ["vm"]
    ~doc:"You need at least 1 VCPU to start a VM" ();
  error Api_errors.vm_toomany_vcpus ["vm"]
    ~doc:"Too many VCPUs to start this VM" ();
  error Api_errors.host_not_enough_free_memory [ "needed"; "available" ]
    ~doc:"Not enough host memory is available to perform this operation" ();
  error Api_errors.host_not_enough_pcpus [ "vcpus"; "pcpus" ]
    ~doc:"The host does not have enough pCPUs to run the VM. It needs at least as many as the VM has vCPUs." ();
  error Api_errors.duplicate_vm [ "vm" ]
    ~doc:"Cannot restore this VM because it would create a duplicate" ();
  error Api_errors.duplicate_mac_seed [ "seed" ]
    ~doc:"This MAC seed is already in use by a VM in the pool" ();
  error Api_errors.vm_snapshot_with_quiesce_failed [ "vm" ]
    ~doc:"The quiesced-snapshot operation failed for an unexpected reason" ();
  error Api_errors.vm_snapshot_with_quiesce_timeout [ "vm" ]
    ~doc:"The VSS plug-in has timed out" ();
  error Api_errors.vm_snapshot_with_quiesce_plugin_does_not_respond [ "vm" ]
    ~doc:"The VSS plug-in cannot be contacted" ();
  error Api_errors.vm_snapshot_with_quiesce_not_supported [ "vm"; "error" ]
    ~doc:"The VSS plug-in is not installed on this virtual machine" ();
  error Api_errors.xen_vss_req_error_init_failed [ "vm"; "error_code" ]
    ~doc:"Initialization of the VSS requester failed" ();
  error Api_errors.xen_vss_req_error_prov_not_loaded [ "vm"; "error_code" ]
    ~doc:"The Vss Provider is not loaded" ();
  error Api_errors.xen_vss_req_error_no_volumes_supported [ "vm"; "error_code" ]
    ~doc:"Could not find any volumes supported by the Vss Provider" ();
  error Api_errors.xen_vss_req_error_start_snapshot_set_failed [ "vm"; "error_code" ]
    ~doc:"An attempt to start a new VSS snapshot failed" ();
  error Api_errors.xen_vss_req_error_adding_volume_to_snapset_failed [ "vm"; "error_code" ]
    ~doc:"Some volumes to be snapshot could not be added to the VSS snapshot set" ();
  error Api_errors.xen_vss_req_error_preparing_writers [ "vm"; "error_code" ]
    ~doc:"An attempt to prepare VSS writers for the snapshot failed" ();
  error Api_errors.xen_vss_req_error_creating_snapshot [ "vm"; "error_code" ]
    ~doc:"An attempt to create the snapshots failed" ();
  error Api_errors.xen_vss_req_error_creating_snapshot_xml_string [ "vm"; "error_code" ]
    ~doc:"Could not create the XML string generated by the transportable snapshot" ();
  error Api_errors.vm_revert_failed [ "vm"; "snapshot" ]
    ~doc:"An error occured while reverting the specified virtual machine to the specified snapshot" ();
  error Api_errors.vm_checkpoint_suspend_failed [ "vm" ]
    ~doc:"An error occured while saving the memory image of the specified virtual machine" ();
  error Api_errors.vm_checkpoint_resume_failed [ "vm" ]
    ~doc:"An error occured while restoring the memory image of the specified virtual machine" ();
  error Api_errors.vm_pv_drivers_in_use [ "vm" ]
    ~doc:"VM PV drivers still in use" ();
  (* VM appliance errors *)
  error Api_errors.operation_partially_failed [ "operation" ]
    ~doc:"Some VMs belonging to the appliance threw an exception while carrying out the specified operation" ();

  (* Host errors *)
  error Api_errors.host_offline [ "host" ]
    ~doc:"You attempted an operation which involves a host which could not be contacted." ();
  error Api_errors.host_disabled [ "host" ]
    ~doc:"The specified host is disabled." ();
  error Api_errors.host_disabled_until_reboot [ "host" ]
    ~doc:"The specified host is disabled and cannot be re-enabled until after it has rebooted." ();
  error Api_errors.no_hosts_available []
    ~doc:"There were no hosts available to complete the specified operation." ();
  error Api_errors.host_in_emergency_mode []
    ~doc:"Cannot perform operation as the host is running in emergency mode." ();
  error Api_errors.host_cannot_destroy_self [ "host" ]
    ~doc:"The pool master host cannot be removed." ();
  error Api_errors.host_cannot_read_metrics []
    ~doc:"The metrics of this host could not be read." ();
  error Api_errors.host_in_use [ "host"; "type"; "ref" ]
    ~doc:"This operation cannot be completed as the host is in use by (at least) the object of type and ref echoed below." ();
  error Api_errors.host_not_disabled []
    ~doc:"This operation cannot be performed because the host is not disabled. Please disable the host and then try again." ();
  error Api_errors.host_not_live []
    ~doc:"This operation cannot be completed as the host is not live." ();
  error Api_errors.host_is_live [ "host" ]
    ~doc:"This operation cannot be completed as the host is still live." ();
  error Api_errors.host_power_on_mode_disabled []
    ~doc:"This operation cannot be completed as the host power on mode is disabled." ();

  error Api_errors.host_its_own_slave []
    ~doc:"The host is its own slave. Please use pool-emergency-transition-to-master or pool-emergency-reset-master." ();
  error Api_errors.host_still_booting []
    ~doc:"The host toolstack is still initialising. Please wait." ();
  error Api_errors.host_has_no_management_ip []
    ~doc:"The host failed to acquire an IP address on its management interface and therefore cannot contact the master." ();
  error Api_errors.host_name_invalid [ "reason" ]
    ~doc:"The host name is invalid." ();
  error Api_errors.host_master_cannot_talk_back [ "ip" ]
    ~doc:"The master reports that it cannot talk back to the slave on the supplied management IP address." ();
  error Api_errors.host_unknown_to_master [ "host" ]
    ~doc:"The master says the host is not known to it. Perhaps the Host was deleted from the master's database? Perhaps the slave is pointing to the wrong master?" ();
  error Api_errors.host_broken []
    ~doc:"This host failed in the middle of an automatic failover operation and needs to retry the failover action" ();
  error Api_errors.host_has_resident_vms [ "host" ]
    ~doc:"This host cannot be forgotten because there are some user VMs still running" ();

  error Api_errors.not_supported_during_upgrade []
    ~doc:"This operation is not supported during an upgrade." ();

  error Api_errors.interface_has_no_ip [ "interface" ]
    ~doc:"The specified interface cannot be used because it has no IP address" ();
  error Api_errors.auth_already_enabled ["current auth_type";"current service_name"]
    ~doc:"External authentication for this host is already enabled." ();
  error Api_errors.auth_unknown_type ["type"]
    ~doc:"Unknown type of external authentication." ();
  error Api_errors.auth_is_disabled []
    ~doc:"External authentication is disabled, unable to resolve subject name." ();
  error Api_errors.auth_enable_failed ["message"]
    ~doc:"The host failed to enable external authentication." ();
  error Api_errors.auth_enable_failed_wrong_credentials ["message"]
    ~doc:"The host failed to enable external authentication." ();
  error Api_errors.auth_enable_failed_permission_denied ["message"]
    ~doc:"The host failed to enable external authentication." ();
  error Api_errors.auth_enable_failed_domain_lookup_failed ["message"]
    ~doc:"The host failed to enable external authentication." ();
  error Api_errors.auth_enable_failed_unavailable ["message"]
    ~doc:"The host failed to enable external authentication." ();
  error Api_errors.auth_enable_failed_invalid_ou ["message"]
    ~doc:"The host failed to enable external authentication." ();
  error Api_errors.auth_enable_failed_invalid_account ["message"]
    ~doc:"The host failed to enable external authentication." ();
  error Api_errors.auth_disable_failed ["message"]
    ~doc:"The host failed to disable external authentication." ();
  error Api_errors.auth_disable_failed_wrong_credentials ["message"]
    ~doc:"The host failed to disable external authentication." ();
  error Api_errors.auth_disable_failed_permission_denied ["message"]
    ~doc:"The host failed to disable external authentication." ();
  error Api_errors.host_evacuate_in_progress [ "host" ]
    ~doc:"This host is being evacuated." ();


  (* Pool errors *)
  error Api_errors.pool_joining_host_cannot_contain_shared_SRs []
    ~doc:"The host joining the pool cannot contain any shared storage." ();
  error Api_errors.pool_joining_host_cannot_have_running_or_suspended_VMs []
    ~doc:"The host joining the pool cannot have any running or suspended VMs." ();
  error Api_errors.pool_joining_host_cannot_have_running_VMs []
    ~doc:"The host joining the pool cannot have any running VMs." ();
  error Api_errors.pool_joining_host_cannot_have_vms_with_current_operations []
    ~doc:"The host joining the pool cannot have any VMs with active tasks." ();
  error Api_errors.pool_joining_host_cannot_be_master_of_other_hosts []
    ~doc:"The host joining the pool cannot already be a master of another pool." ();
  error Api_errors.pool_joining_host_connection_failed []
    ~doc:"There was an error connecting to the host while joining it in the pool." ();
  error Api_errors.pool_joining_host_service_failed []
    ~doc:"There was an error connecting to the host. the service contacted didn't reply properly." ();
  error Api_errors.pool_joining_host_must_have_physical_management_nic []
    ~doc:"The host joining the pool must have a physical management NIC (i.e. the management NIC must not be on a VLAN or bonded PIF)." ();
  error Api_errors.pool_joining_external_auth_mismatch []
    ~doc:"Cannot join pool whose external authentication configuration is different." ();
  error Api_errors.pool_joining_host_must_have_same_product_version []
    ~doc:"The host joining the pool must have the same product version as the pool master." ();
  error Api_errors.pool_joining_host_must_only_have_physical_pifs []
    ~doc:"The host joining the pool must not have any bonds, VLANs or tunnels." ();
  error Api_errors.pool_joining_host_management_vlan_does_not_match ["local"; "remote"]
    ~doc:"The host joining the pool must have the same management vlan." ();
  error Api_errors.pool_joining_host_has_non_management_vlans []
    ~doc:"The host joining the pool must not have any non-management vlans." ();
  error Api_errors.pool_joining_host_has_bonds []
    ~doc:"The host joining the pool must not have any bonds." ();
  error Api_errors.pool_joining_host_has_tunnels []
    ~doc:"The host joining the pool must not have any tunnels." ();
  error Api_errors.pool_hosts_not_compatible []
    ~doc:"The hosts in this pool are not compatible." ();
  error Api_errors.pool_hosts_not_homogeneous [ "reason" ]
    ~doc:"The hosts in this pool are not homogeneous." ();
  error Api_errors.pool_not_in_emergency_mode []
    ~doc:"This pool is not in emergency mode." ();
  error Api_errors.pool_auth_already_enabled ["host"]
    ~doc:"External authentication in this pool is already enabled for at least one host." ();
  error Api_errors.pool_auth_enable_failed ["host";"message"]
    ~doc:"The pool failed to enable external authentication." ();
  error Api_errors.pool_auth_enable_failed_wrong_credentials ["host";"message"]
    ~doc:"The pool failed to enable external authentication." ();
  error Api_errors.pool_auth_enable_failed_permission_denied ["host";"message"]
    ~doc:"The pool failed to enable external authentication." ();
  error Api_errors.pool_auth_enable_failed_domain_lookup_failed ["host";"message"]
    ~doc:"The pool failed to enable external authentication." ();
  error Api_errors.pool_auth_enable_failed_unavailable ["host";"message"]
    ~doc:"The pool failed to enable external authentication." ();
  error Api_errors.pool_auth_enable_failed_duplicate_hostname ["host";"message"]
    ~doc:"The pool failed to enable external authentication." ();
  error Api_errors.pool_auth_enable_failed_invalid_ou ["host";"message"]
    ~doc:"The pool failed to enable external authentication." ();
  error Api_errors.pool_auth_enable_failed_invalid_account ["host";"message"]
    ~doc:"The pool failed to enable external authentication." ();
  error Api_errors.pool_auth_disable_failed ["host";"message"]
    ~doc:"The pool failed to disable the external authentication of at least one host." ();
  error Api_errors.pool_auth_disable_failed_wrong_credentials ["host";"message"]
    ~doc:"External authentication has been disabled with errors: Some AD machine accounts were not disabled on the AD server due to invalid credentials." ();
  error Api_errors.pool_auth_disable_failed_permission_denied ["host";"message"]
    ~doc:"External authentication has been disabled with errors: Your AD machine account was not disabled on the AD server as permission was denied." ();
  error Api_errors.pool_auth_disable_failed_invalid_account ["host";"message"]
    ~doc:"External authentication has been disabled with errors: Some AD machine accounts were not disabled on the AD server due to invalid account." ();
  error Api_errors.pool_joining_host_must_have_same_api_version ["host_api_version";"master_api_version"]
    ~doc:"The host joining the pool must have the same API version as the pool master." ();
  error Api_errors.pool_joining_host_must_have_same_db_schema ["host_db_schema";"master_db_schema"]
    ~doc:"The host joining the pool must have the same database schema as the pool master." ();

  (* External directory service *)
  error Api_errors.subject_cannot_be_resolved []
    ~doc:"Subject cannot be resolved by the external directory service." ();
  error Api_errors.auth_service_error ["message"]
    ~doc:"Error querying the external directory service." ();
  error Api_errors.subject_already_exists []
    ~doc:"Subject already exists." ();

  (* RBAC *)
  error Api_errors.role_not_found []
    ~doc: "Role cannot be found." ();
  error Api_errors.role_already_exists []
    ~doc: "Role already exists." ();
  error Api_errors.rbac_permission_denied ["permission";"message"]
    ~doc: "RBAC permission denied." ();

  (* wlb errors, deprecated since clearwater *)
  error Api_errors.wlb_not_initialized []
    ~doc:"No WLB connection is configured." ();
  error Api_errors.wlb_disabled []
    ~doc:"This pool has wlb-enabled set to false." ();
  error Api_errors.wlb_connection_refused []
    ~doc:"WLB refused a connection to the server." ();
  error Api_errors.wlb_unknown_host []
    ~doc:"The configured WLB server name failed to resolve in DNS." ();
  error Api_errors.wlb_timeout ["configured_timeout"]
    ~doc:"The communication with the WLB server timed out." ();
  error Api_errors.wlb_authentication_failed []
    ~doc:"WLB rejected our configured authentication details." ();
  error Api_errors.wlb_malformed_request []
    ~doc:"WLB rejected the server's request as malformed." ();
  error Api_errors.wlb_malformed_response ["method"; "reason"; "response"]
    ~doc:"WLB said something that the server wasn't expecting or didn't understand.  The method called on WLB, a diagnostic reason, and the response from WLB are returned." ();
  error Api_errors.wlb_xenserver_connection_refused []
    ~doc:"WLB reported that the server refused it a connection (even though we're connecting perfectly fine in the other direction)." ();
  error Api_errors.wlb_xenserver_unknown_host []
    ~doc:"WLB reported that its configured server name for this server instance failed to resolve in DNS." ();
  error Api_errors.wlb_xenserver_timeout []
    ~doc:"WLB reported that communication with the server timed out." ();
  error Api_errors.wlb_xenserver_authentication_failed []
    ~doc:"WLB reported that the server rejected its configured authentication details." ();
  error Api_errors.wlb_xenserver_malformed_response []
    ~doc:"WLB reported that the server said something to it that WLB wasn't expecting or didn't understand." ();
  error Api_errors.wlb_internal_error []
    ~doc:"WLB reported an internal error." ();
  error Api_errors.wlb_connection_reset []
    ~doc:"The connection to the WLB server was reset." ();
  error Api_errors.wlb_url_invalid ["url"]
    ~doc:"The WLB URL is invalid. Ensure it is in format: <ipaddress>:<port>.  The configured/given URL is returned." ();


  (* Api_errors specific to running VMs on multiple hosts *)
  error Api_errors.vm_unsafe_boot ["vm"]
    ~doc:"You attempted an operation on a VM that was judged to be unsafe by the server. This can happen if the VM would run on a CPU that has a potentially incompatible set of feature flags to those the VM requires. If you want to override this warning then use the 'force' option." ();
  error Api_errors.vm_requires_sr [ "vm"; "sr" ]
    ~doc:"You attempted to run a VM on a host which doesn't have access to an SR needed by the VM. The VM has at least one VBD attached to a VDI in the SR." ();
  error Api_errors.vm_requires_net [ "vm"; "network" ]
    ~doc:"You attempted to run a VM on a host which doesn't have a PIF on a Network needed by the VM. The VM has at least one VIF attached to the Network." ();
  error Api_errors.vm_requires_gpu ["vm"; "GPU_group"]
    ~doc:"You attempted to run a VM on a host which doesn't have a pGPU available in the GPU group needed by the VM. The VM has a vGPU attached to this GPU group." ();
  error Api_errors.vm_requires_vgpu ["vm"; "GPU_group"; "vGPU_type"]
    ~doc:"You attempted to run a VM on a host on which the vGPU required by the VM cannot be allocated on any pGPUs in the GPU_group needed by the VM." ();
  error Api_errors.vm_requires_iommu ["host"]
    ~doc:"You attempted to run a VM on a host which doesn't have I/O virtualization (IOMMU/VT-d) enabled, which is needed by the VM." ();
  error Api_errors.vm_host_incompatible_version_migrate ["host"; "vm"]
    ~doc:"You attempted to migrate a VM to a destination host which is older than the source host." ();
  error Api_errors.vm_host_incompatible_version ["host"; "vm"]
    ~doc:"This VM operation cannot be performed on an older-versioned host during an upgrade." ();
  error Api_errors.vm_host_incompatible_virtual_hardware_platform_version ["host"; "host_versions"; "vm"; "vm_version"]
    ~doc:"You attempted to run a VM on a host that cannot provide the VM's required Virtual Hardware Platform version." ();
  error Api_errors.vm_has_pci_attached ["vm"]
    ~doc:"This operation could not be performed, because the VM has one or more PCI devices passed through." ();
  error Api_errors.vm_has_vgpu ["vm"]
    ~doc:"This operation could not be performed, because the VM has one or more virtual GPUs." ();
  error Api_errors.vm_has_sriov_vif ["vm"]
    ~doc:"This operation could not be performed, because the VM has one or more SR-IOV VIFs." ();
  error Api_errors.host_cannot_attach_network [ "host"; "network" ]
    ~doc:"Host cannot attach network (in the case of NIC bonding, this may be because attaching the network on this host would require other networks [that are currently active] to be taken down)." ();
  error Api_errors.vm_requires_vdi [ "vm"; "vdi" ]
    ~doc:"VM cannot be started because it requires a VDI which cannot be attached" ();
  error Api_errors.vm_has_no_suspend_vdi [ "vm" ]
    ~doc:"VM cannot be resumed because it has no suspend VDI" ();
  error Api_errors.vm_migrate_failed [ "vm"; "source"; "destination"; "msg" ]
    ~doc:"An error occurred during the migration process." ();
  error Api_errors.vm_migrate_contact_remote_service_failed []
    ~doc:"Failed to contact service on the destination host." ();
  error Api_errors.vm_has_too_many_snapshots [ "vm" ]
    ~doc:"You attempted to migrate a VM with more than one snapshot." ();
  error Api_errors.vm_has_checkpoint [ "vm" ]
    ~doc:"You attempted to migrate a VM which has a checkpoint." ();
  error Api_errors.vdi_needs_vm_for_migrate [ "vdi" ]
    ~doc:"You attempted to migrate a VDI which is not attached to a running VM." ();
  error Api_errors.mirror_failed [ "vdi" ]
    ~doc:"The VDI mirroring cannot be performed" ();
  error Api_errors.too_many_storage_migrates [ "number" ]
    ~doc:"You reached the maximal number of concurrently migrating VMs." ();
  error Api_errors.sr_does_not_support_migration [ "sr" ]
    ~doc:"You attempted to migrate a VDI to or from an SR which doesn't support migration" ();
  error Api_errors.vm_failed_shutdown_ack [ "vm" ]
    ~doc:"VM didn't acknowledge the need to shutdown." ();
  error Api_errors.vm_shutdown_timeout [ "vm"; "timeout" ]
    ~doc:"VM failed to shutdown before the timeout expired" ();
  error Api_errors.vm_crashed [ "vm" ]
    ~doc:"The VM crashed" ();
  error Api_errors.vm_rebooted [ "vm" ]
    ~doc:"The VM unexpectedly rebooted" ();
  error Api_errors.vm_halted [ "vm" ]
    ~doc:"The VM unexpectedly halted" ();
  error Api_errors.bootloader_failed [ "vm"; "msg" ]
    ~doc:"The bootloader returned an error" ();
  error Api_errors.unknown_bootloader [ "vm"; "bootloader" ]
    ~doc:"The requested bootloader is unknown" ();
  error Api_errors.failed_to_start_emulator [ "vm"; "name"; "msg" ]
    ~doc:"An emulator required to run this VM failed to start" ();
  error Api_errors.vm_attached_to_more_than_one_vdi_with_timeoffset_marked_as_reset_on_boot [ "vm" ]
    ~doc:"You attempted to start a VM that's attached to more than one VDI with a timeoffset marked as reset-on-boot." ();
  error Api_errors.vms_failed_to_cooperate [ ]
    ~doc:"The given VMs failed to release memory when instructed to do so" ();
  error Api_errors.ballooning_timeout_before_migration [ "vm" ]
    ~doc:"Timeout trying to balloon down memory before VM migration. If the error occurs repeatedly, consider increasing the memory-dynamic-min value." ();
  error Api_errors.vm_requires_vusb ["vm"; "USB_group"]
    ~doc:"You attempted to run a VM on a host on which the VUSB required by the VM cannot be allocated on any PUSBs in the USB_group needed by the VM." ();

  (* Storage errors *)
  error Api_errors.sr_not_attached ["sr"]
    ~doc:"The SR is not attached." ();
  error Api_errors.sr_attach_failed ["sr"]
    ~doc:"Attaching this SR failed." ();
  error Api_errors.sr_backend_failure ["status"; "stdout"; "stderr"]
    ~doc:"There was an SR backend failure." ();
  error Api_errors.sr_uuid_exists ["uuid"]
    ~doc:"An SR with that uuid already exists." ();
  error Api_errors.sr_no_pbds ["sr"]
    ~doc:"The SR has no attached PBDs" ();
  error Api_errors.sr_full ["requested";"maximum"]
    ~doc:"The SR is full. Requested new size exceeds the maximum size" ();
  error Api_errors.sr_source_space_insufficient ["sr"]
    ~doc:"The source SR does not have sufficient temporary space available to proceed the operation." ();
  error Api_errors.pbd_exists ["sr";"host";"pbd"]
    ~doc:"A PBD already exists connecting the SR to the host" ();
  error Api_errors.sr_has_pbd ["sr"]
    ~doc:"The SR is still connected to a host via a PBD. It cannot be destroyed or forgotten." ();
  error Api_errors.sr_has_multiple_pbds [ "PBD" ]
    ~doc:"The SR.shared flag cannot be set to false while the SR remains connected to multiple hosts" ();
  error Api_errors.sr_requires_upgrade [ "SR" ]
    ~doc:"The operation cannot be performed until the SR has been upgraded" ();
  error Api_errors.sr_unknown_driver [ "driver" ]
    ~doc:"The SR could not be connected because the driver was not recognised." ();
  error Api_errors.sr_vdi_locking_failed []
    ~doc:"The operation could not proceed because necessary VDIs were already locked at the storage level." ();
  error Api_errors.vdi_readonly [ "vdi" ]
    ~doc:"The operation required write access but this VDI is read-only" ();
  error Api_errors.vdi_has_rrds [ "vdi" ]
    ~doc:"The operation cannot be performed because this VDI has rrd stats" ();
  error Api_errors.vdi_too_small [ "vdi"; "minimum size" ]
    ~doc:"The VDI is too small. Please resize it to at least the minimum size." ();
  error Api_errors.vdi_too_large [ "vdi"; "maximum size" ]
    ~doc:"The VDI is too large." ();
  error Api_errors.vdi_not_sparse [ "vdi" ]
    ~doc:"The VDI is not stored using a sparse format. It is not possible to query and manipulate only the changed blocks (or 'block differences' or 'disk deltas') between two VDIs. Please select a VDI which uses a sparse-aware technology such as VHD." ();
  error Api_errors.vdi_is_a_physical_device [ "vdi" ]
    ~doc:"The operation cannot be performed on physical device" ();
  error Api_errors.vdi_is_not_iso [ "vdi"; "type" ]
    ~doc:"This operation can only be performed on CD VDIs (iso files or CDROM drives)" ();
  error Api_errors.host_cd_drive_empty [ ]
    ~doc:"The host CDROM drive does not contain a valid CD" ();
  error Api_errors.vdi_in_use [ "vdi"; "operation" ]
    ~doc:"This operation cannot be performed because this VDI is in use by some other operation" ();
  error Api_errors.vdi_not_available [ "vdi" ]
    ~doc:"This operation cannot be performed because this VDI could not be properly attached to the VM." ();
  error Api_errors.vdi_location_missing [ "sr"; "location" ]
    ~doc:"This operation cannot be performed because the specified VDI could not be found in the specified SR" ();
  error Api_errors.vdi_missing [ "sr"; "vdi" ]
    ~doc:"This operation cannot be performed because the specified VDI could not be found on the storage substrate" ();
  error Api_errors.vdi_incompatible_type [ "vdi"; "type" ]
    ~doc:"This operation cannot be performed because the specified VDI is of an incompatible type (eg: an HA statefile cannot be attached to a guest)" ();
  error Api_errors.vdi_not_managed [ "vdi" ]
    ~doc:"This operation cannot be performed because the system does not manage this VDI" ();
  error Api_errors.vdi_not_in_map [ "vdi" ]
    ~doc:"This VDI was not mapped to a destination SR in VM.migrate_send operation" () ;
  error Api_errors.vdi_cbt_enabled [ "vdi" ]
    ~doc:"The requested operation is not allowed for VDIs with CBT enabled or VMs having such VDIs, and CBT is enabled for the specified VDI." ();
  error Api_errors.vdi_no_cbt_metadata [ "vdi" ]
    ~doc:"The requested operation is not allowed because the specified VDI does not have changed block tracking metadata." ();
  error Api_errors.vdi_is_encrypted [ "vdi" ]
    ~doc:"The requested operation is not allowed because the specified VDI is encrypted." ();
  error Api_errors.vdi_copy_failed []
    ~doc:"The VDI copy action has failed" ();
  error Api_errors.vdi_on_boot_mode_incompatible_with_operation []
    ~doc:"This operation is not permitted on VDIs in the 'on-boot=reset' mode, or on VMs having such VDIs." ();
  error Api_errors.cannot_create_state_file []
    ~doc:"An HA statefile could not be created, perhaps because no SR with the appropriate capability was found." ();
  error Api_errors.vif_not_in_map [ "vif" ]
    ~doc:"This VIF was not mapped to a destination Network in VM.migrate_send operation" () ;

  error Api_errors.suspend_image_not_accessible [ "vdi" ]
    ~doc:"The suspend image of a checkpoint is not accessible from the host on which the VM is running" ();

  error Api_errors.sr_operation_not_supported [ "sr" ]
    ~doc:"The SR backend does not support the operation (check the SR's allowed operations)" ();
  error Api_errors.sr_not_empty [ ]
    ~doc:"The SR operation cannot be performed because the SR is not empty." ();
  error Api_errors.sr_device_in_use [ ]
    ~doc:"The SR operation cannot be performed because a device underlying the SR is in use by the host." ();
  error Api_errors.sr_not_sharable [ "sr"; "host" ]
    ~doc:"The PBD could not be plugged because the SR is in use by another host and is not marked as sharable." ();
  error Api_errors.sr_indestructible ["sr"]
    ~doc:"The SR could not be destroyed, as the 'indestructible' flag was set on it." ();
  error Api_errors.sr_is_cache_sr [ "host" ]
    ~doc:"The SR is currently being used as a local cache SR." ();
  error Api_errors.clustered_sr_degraded [ "sr" ]
    ~doc:"An SR is using clustered local storage. It is not safe to reboot a host at the moment." ();

  error Api_errors.sm_plugin_communication_failure ["sm"]
    ~doc:"The SM plugin did not respond to a query." ();

  error Api_errors.device_already_attached ["device"]
    ~doc:"The device is already attached to a VM" ();
  error Api_errors.device_already_detached ["device"]
    ~doc:"The device is not currently attached" ();
  error Api_errors.device_already_exists ["device"]
    ~doc:"A device with the name given already exists on the selected VM" ();
  error Api_errors.invalid_device ["device"]
    ~doc:"The device name is invalid" ();

  error Api_errors.default_sr_not_found [ "sr" ]
    ~doc:"The default SR reference does not point to a valid SR" ();

  error Api_errors.only_provision_template [ ]
    ~doc:"The provision call can only be invoked on templates, not regular VMs." ();
  error Api_errors.provision_failed_out_of_space [ ]
    ~doc:"The provision call failed because it ran out of space." ();

  (* Import export errors *)
  error Api_errors.import_incompatible_version [ ]
    ~doc:"The import failed because this export has been created by a different (incompatible) product version" ();
  error Api_errors.import_error_generic [ "msg" ]
    ~doc:"The VM could not be imported." ();
  error Api_errors.import_error_premature_eof []
    ~doc:"The VM could not be imported; the end of the file was reached prematurely." ();
  error Api_errors.import_error_some_checksums_failed []
    ~doc:"Some data checksums were incorrect; the VM may be corrupt." ();
  error Api_errors.import_error_cannot_handle_chunked []
    ~doc:"Cannot import VM using chunked encoding." ();
  error Api_errors.import_error_failed_to_find_object ["id"]
    ~doc:"The VM could not be imported because a required object could not be found." ();
  error Api_errors.import_error_attached_disks_not_found []
    ~doc:"The VM could not be imported because attached disks could not be found." ();
  error Api_errors.import_error_unexpected_file ["filename_expected";"filename_found"]
    ~doc:"The VM could not be imported because the XVA file is invalid: an unexpected file was encountered." ();

  (* Restore errors *)
  error Api_errors.restore_incompatible_version [ ]
    ~doc:"The restore could not be performed because this backup has been created by a different (incompatible) product version" ();
  error Api_errors.restore_target_missing_device [ "device" ]
    ~doc:"The restore could not be performed because a network interface is missing" ();
  error Api_errors.restore_target_mgmt_if_not_in_backup [ ]
    ~doc:"The restore could not be performed because the host's current management interface is not in the backup. The interfaces mentioned in the backup are:" ();

  error Api_errors.cannot_find_state_partition [ ]
    ~doc:"This operation could not be performed because the state partition could not be found" ();
  error Api_errors.backup_script_failed [ "log" ]
    ~doc:"The backup could not be performed because the backup script failed." ();
  error Api_errors.restore_script_failed [ "log" ]
    ~doc:"The restore could not be performed because the restore script failed.  Is the file corrupt?" ();



  (* Event errors *)
  error Api_errors.events_lost []
    ~doc:"Some events have been lost from the queue and cannot be retrieved." ();
  error Api_errors.event_subscription_parse_failure [ "subscription" ]
    ~doc:"The server failed to parse your event subscription. Valid values include: *, class-name, class-name/object-reference." ();
  error Api_errors.event_from_token_parse_failure [ "token" ]
    ~doc:"The event.from token could not be parsed. Valid values include: '', and a value returned from a previous event.from call." ();
  error Api_errors.session_not_registered ["handle"]
    ~doc:"This session is not registered to receive events.  You must call event.register before event.next.  The session handle you are using is echoed." ();

  error Api_errors.task_cancelled [ "task" ]
    ~doc:"The request was asynchronously cancelled." ();
  error Api_errors.too_many_pending_tasks [ ]
    ~doc:"The request was rejected because there are too many pending tasks on the server." ();
  error Api_errors.too_busy [ ]
    ~doc:"The request was rejected because the server is too busy." ();

  (* Patch errors *)
  error Api_errors.out_of_space ["location"]
    ~doc:"There is not enough space to upload the update" ();
  error Api_errors.invalid_patch []
    ~doc:"The uploaded patch file is invalid" ();
  error Api_errors.invalid_patch_with_log [ "log" ]
    ~doc:"The uploaded patch file is invalid.  See attached log for more details." ();
  error Api_errors.cannot_find_patch []
    ~doc:"The requested update could not be found.  This can occur when you designate a new master or xe patch-clean.  Please upload the update again" ();
  error Api_errors.cannot_fetch_patch ["uuid"]
    ~doc:"The requested update could to be obtained from the master." ();
  error Api_errors.patch_already_exists [ "uuid" ]
    ~doc:"The uploaded patch file already exists" ();
  error Api_errors.update_already_exists [ "uuid" ]
    ~doc:"The uploaded update already exists" ();
  error Api_errors.patch_is_applied [ ]
    ~doc:"The specified patch is applied and cannot be destroyed." ();
  error Api_errors.patch_already_applied [ "patch" ]
    ~doc:"This patch has already been applied" ();
  error Api_errors.patch_apply_failed [ "output" ]
    ~doc:"The patch apply failed.  Please see attached output." ();
  error Api_errors.patch_apply_failed_backup_files_exist [ "output" ]
    ~doc:"The patch apply failed: there are backup files created while applying patch. Please remove these backup files before applying patch again." ();
  error Api_errors.patch_precheck_failed_unknown_error [ "patch"; "info" ]
    ~doc:"The patch precheck stage failed with an unknown error.  See attached info for more details." ();
  error Api_errors.patch_precheck_failed_prerequisite_missing [ "patch"; "prerequisite_patch_uuid_list" ]
    ~doc:"The patch precheck stage failed: prerequisite patches are missing." ();
  error Api_errors.patch_precheck_failed_wrong_server_version [ "patch"; "found_version"; "required_version"]
    ~doc:"The patch precheck stage failed: the server is of an incorrect version." ();
  error Api_errors.patch_precheck_failed_wrong_server_build [ "patch"; "found_build"; "required_build"]
    ~doc:"The patch precheck stage failed: the server is of an incorrect build." ();
  error Api_errors.patch_precheck_failed_vm_running [ "patch" ]
    ~doc:"The patch precheck stage failed: there are one or more VMs still running on the server.  All VMs must be suspended before the patch can be applied." ();
  error Api_errors.patch_precheck_failed_out_of_space [ "patch"; "found_space"; "required_required"]
    ~doc:"The patch precheck stage failed: the server does not have enough space." ();
  error Api_errors.patch_precheck_tools_iso_mounted ["patch"]
    ~doc:"Tools ISO must be ejected from all running VMs." ();

  error Api_errors.cannot_find_oem_backup_partition []
    ~doc:"The backup partition to stream the updat to cannot be found" ();
  error Api_errors.only_allowed_on_oem_edition ["command"]
    ~doc:"This command is only allowed on the OEM edition." ();
  error Api_errors.not_allowed_on_oem_edition ["command"]
    ~doc:"This command is not allowed on the OEM edition." ();

  (* Update errors *)
  error Api_errors.invalid_update [ "info" ]
    ~doc:"The uploaded update package is invalid." ();
  error Api_errors.update_is_applied [ ]
    ~doc:"The specified update has been applied and cannot be destroyed." ();
  error Api_errors.cannot_find_update []
    ~doc:"The requested update could not be found. Please upload the update again. This can occur when you run xe update-pool-clean before xe update-apply. " ();
  error Api_errors.update_pool_apply_failed [ "hosts" ]
    ~doc:"The update cannot be applied for the following host(s)." ();
  error Api_errors.could_not_update_igmp_snooping_everywhere [ ]
    ~doc:"The IGMP Snooping setting cannot be applied for some of the host, network(s)." ();
  error Api_errors.update_apply_failed [ "output" ]
    ~doc:"The update failed to apply. Please see attached output." ();
  error Api_errors.update_already_applied [ "update" ]
    ~doc:"This update has already been applied." ();
  error Api_errors.update_already_applied_in_pool [ "update" ]
    ~doc:"This update has already been applied to all hosts in the pool." ();
  error Api_errors.update_precheck_failed_unknown_error [ "update"; "info" ]
    ~doc:"The update precheck stage failed with an unknown error." ();
  error Api_errors.update_precheck_failed_prerequisite_missing [ "update"; "prerequisite_update" ]
    ~doc:"The update precheck stage failed: prerequisite update(s) are missing." ();
  error Api_errors.update_precheck_failed_conflict_present ["update"; "conflict_update"]
    ~doc:"The update precheck stage failed: conflicting update(s) are present." ();
  error Api_errors.update_precheck_failed_wrong_server_version ["update"; "installed_version"; "required_version "]
    ~doc:"The update precheck stage failed: the server is of an incorrect version." ();
  error Api_errors.update_precheck_failed_out_of_space ["update"; "available_space"; "required_space "]
    ~doc:"The update precheck stage failed: the server does not have enough space." ();
  error Api_errors.update_precheck_failed_gpgkey_not_imported ["update"]
    ~doc:"The update precheck stage failed: RPM package validation requires a GPG key that is not present on the host." ();

  (* Pool errors *)

  error Api_errors.host_is_slave ["Master IP address"]
    ~doc:"You cannot make regular API calls directly on a slave. Please pass API calls via the master host." ();


  (* HA errors *)
  error Api_errors.ha_failed_to_form_liveset [ ]
    ~doc:"HA could not be enabled on the Pool because a liveset could not be formed: check storage and network heartbeat paths." ();
  error Api_errors.ha_heartbeat_daemon_startup_failed [ ]
    ~doc:"The host could not join the liveset because the HA daemon failed to start." ();
  error Api_errors.ha_host_cannot_access_statefile [ ]
    ~doc:"The host could not join the liveset because the HA daemon could not access the heartbeat disk." ();
  error Api_errors.ha_host_is_armed [ "host" ]
    ~doc:"The operation could not be performed while the host is still armed; it must be disarmed first" ();
  error Api_errors.ha_is_enabled [ ]
    ~doc:"The operation could not be performed because HA is enabled on the Pool" ();
  error Api_errors.ha_not_enabled [ ]
    ~doc:"The operation could not be performed because HA is not enabled on the Pool" ();
  error Api_errors.ha_enable_in_progress [ ]
    ~doc:"The operation could not be performed because HA enable is in progress" ();
  error Api_errors.ha_disable_in_progress [ ]
    ~doc:"The operation could not be performed because HA disable is in progress" ();
  error Api_errors.ha_not_installed [ "host" ]
    ~doc:"The operation could not be performed because the HA software is not installed on this host." ();
  error Api_errors.ha_host_cannot_see_peers [ "host"; "all"; "subset" ]
    ~doc:"The operation failed because the HA software on the specified host could not see a subset of other hosts. Check your network connectivity."
    ();
  error Api_errors.ha_too_few_hosts [ ]
    ~doc:"HA can only be enabled for 2 hosts or more. Note that 2 hosts requires a pre-configured quorum tiebreak script."
    ();
  error Api_errors.ha_should_be_fenced [ "host" ]
    ~doc:"Host cannot rejoin pool because it should have fenced (it is not in the master's partition)"
    ();
  error Api_errors.ha_abort_new_master [ "reason" ]
    ~doc:"This host cannot accept the proposed new master setting at this time."
    ();

  error Api_errors.ha_no_plan [ ]
    ~doc:"Cannot find a plan for placement of VMs as there are no other hosts available."
    ();
  error Api_errors.ha_lost_statefile [ ]
    ~doc:"This host lost access to the HA statefile."
    ();
  error Api_errors.ha_pool_is_enabled_but_host_is_disabled [ ]
    ~doc:"This host cannot join the pool because the pool has HA enabled but this host has HA disabled."
    ();
  error Api_errors.ha_constraint_violation_sr_not_shared [ "SR" ]
    ~doc:"This operation cannot be performed because the referenced SR is not properly shared. The SR must both be marked as shared and a currently_attached PBD must exist for each host."
    ();
  error Api_errors.ha_constraint_violation_network_not_shared [ "network" ]
    ~doc:"This operation cannot be performed because the referenced network is not properly shared. The network must either be entirely virtual or must be physically present via a currently_attached PIF on every host."
    ();

  error Api_errors.ha_operation_would_break_failover_plan [ ]
    ~doc:"This operation cannot be performed because it would invalidate VM failover planning such that the system would be unable to guarantee to restart protected VMs after a Host failure."
    ();

  error Api_errors.ha_cannot_change_bond_status_of_mgmt_iface [ ]
    ~doc:"This operation cannot be performed because creating or deleting a bond involving the management interface is not allowed while HA is on. In order to do that, disable HA, create or delete the bond then re-enable HA."
    ();

  error Api_errors.incompatible_statefile_sr ["SR type"]
    ~doc:"The specified SR is incompatible with the selected HA cluster stack."
    ();
  error Api_errors.incompatible_cluster_stack_active ["cluster_stack"]
    ~doc:"This operation cannot be performed, because it is incompatible with the currently active HA cluster stack."
    ();

  error Api_errors.cannot_evacuate_host ["errors"]
    ~doc:"This host cannot be evacuated."
    ();

  error Api_errors.system_status_retrieval_failed ["reason"]
    ~doc:"Retrieving system status from the host failed.  A diagnostic reason suitable for support organisations is also returned."
    ();

  error Api_errors.system_status_must_use_tar_on_oem []
    ~doc:"You must use tar output to retrieve system status from an OEM host." ();

  error Api_errors.xapi_hook_failed ["hook_name"; "reason"; "stdout"; "exit_code"]
    ~doc:"3rd party xapi hook failed" ();

  error Api_errors.xenapi_missing_plugin ["name"]
    ~doc:"The requested plugin could not be found." ();
  error Api_errors.xenapi_plugin_failure ["status"; "stdout"; "stderr"]
    ~doc:"There was a failure communicating with the plugin." ();

  error Api_errors.domain_builder_error [ "function"; "code"; "message" ]
    ~doc:"An internal error generated by the domain builder." ();

  error Api_errors.certificate_does_not_exist ["name"]
    ~doc:"The specified certificate does not exist." ();
  error Api_errors.certificate_already_exists ["name"]
    ~doc:"A certificate already exists with the specified name." ();
  error Api_errors.certificate_name_invalid ["name"]
    ~doc:"The specified certificate name is invalid." ();
  error Api_errors.certificate_corrupt ["name"]
    ~doc:"The specified certificate is corrupt or unreadable." ();
  error Api_errors.certificate_library_corrupt []
    ~doc:"The certificate library is corrupt or unreadable." ();
  error Api_errors.crl_does_not_exist ["name"]
    ~doc:"The specified CRL does not exist." ();
  error Api_errors.crl_already_exists ["name"]
    ~doc:"A CRL already exists with the specified name." ();
  error Api_errors.crl_name_invalid ["name"]
    ~doc:"The specified CRL name is invalid." ();
  error Api_errors.crl_corrupt ["name"]
    ~doc:"The specified CRL is corrupt or unreadable." ();

  error Api_errors.vmpp_has_vm []
    ~doc:"There is at least one VM assigned to this protection policy." ();
  error Api_errors.vmpp_archive_more_frequent_than_backup []
    ~doc:"Archive more frequent than backup." ();
  error Api_errors.vm_assigned_to_protection_policy ["vm"; "vmpp"]
    ~doc:"This VM is assigned to a protection policy." ();

  error Api_errors.vmss_has_vm []
    ~doc:"There is at least one VM assigned to snapshot schedule." ();
  error Api_errors.vm_assigned_to_snapshot_schedule ["vm"; "vmss"]
    ~doc:"This VM is assigned to a snapshot schedule." ();

  error Api_errors.ssl_verify_error ["reason"]
    ~doc:"The remote system's SSL certificate failed to verify against our certificate library." ();

  error Api_errors.cannot_enable_redo_log ["reason"]
    ~doc:"Could not enable redo log." ();

  error Api_errors.redo_log_is_enabled []
    ~doc:"The operation could not be performed because a redo log is enabled on the Pool." ();

  error Api_errors.vm_bios_strings_already_set []
    ~doc:"The BIOS strings for this VM have already been set and cannot be changed." ();

  (* CPU feature masking (a.k.a. Intel FlexMigration or AMD Extended Migration technology) *)

  error Api_errors.invalid_feature_string ["details"]
    ~doc:"The given feature string is not valid." ();

  error Api_errors.cpu_feature_masking_not_supported ["details"]
    ~doc:"The CPU does not support masking of features." ();

  error Api_errors.feature_requires_hvm ["details"]
    ~doc:"The VM is set up to use a feature that requires it to boot as HVM." ();

  (* Disaster recovery errors *)
  error Api_errors.vdi_contains_metadata_of_this_pool ["vdi"; "pool"]
    ~doc:"The VDI could not be opened for metadata recovery as it contains the current pool's metadata." ();

  error Api_errors.no_more_redo_logs_allowed []
    ~doc:"The upper limit of active redo log instances was reached." ();

  error Api_errors.could_not_import_database ["reason"]
    ~doc:"An error occurred while attempting to import a database from a metadata VDI" ();

  error Api_errors.vm_incompatible_with_this_host ["vm"; "host"; "reason"]
    ~doc:"The VM is incompatible with the CPU features of this host." ();

  error Api_errors.cannot_destroy_disaster_recovery_task ["reason"]
    ~doc:"The disaster recovery task could not be cleanly destroyed." ();

  error Api_errors.vm_is_part_of_an_appliance ["vm"; "appliance"]
    ~doc:"This operation is not allowed as the VM is part of an appliance." ();

  error Api_errors.vm_to_import_is_not_newer_version ["vm"; "existing_version"; "version_to_import"]
    ~doc:"The VM cannot be imported unforced because it is either the same version or an older version of an existing VM." ();

  error Api_errors.vm_call_plugin_rate_limit ["VM"; "interval"; "wait"]
    ~doc:"There is a minimal interval required between consecutive plugin calls made on the same VM, please wait before retry." ();

  error Api_errors.vm_is_immobile ["VM"]
    ~doc:"The VM is configured in a way that prevents it from being mobile." ();

  error Api_errors.vm_is_using_nested_virt ["VM"]
    ~doc:"This operation is illegal because the VM is using nested virtualisation." ();

  (* PVS errors *)
  error Api_errors.pvs_site_contains_running_proxies ["proxies"]
    ~doc:"The PVS site contains running proxies." ();

  error Api_errors.pvs_site_contains_servers ["servers"]
    ~doc:"The PVS site contains servers and cannot be forgotten."
    ();

  error Api_errors.pvs_cache_storage_already_present ["site"; "host"]
    ~doc:"The PVS site already has cache storage configured for the host."
    ();

  error Api_errors.pvs_cache_storage_is_in_use ["PVS_cache_storage"]
    ~doc:"The PVS cache storage is in use by the site and cannot be removed."
    ();

  error Api_errors.pvs_proxy_already_present ["proxies"]
    ~doc:"The VIF is already associated with a PVS proxy"
    ();

  error Api_errors.pvs_server_address_in_use ["address"]
    ~doc:"The address specified is already in use by an existing PVS_server object"
    ();

  error Api_errors.usb_group_contains_vusb ["vusbs"]
    ~doc:"The USB group contains active VUSBs and cannot be deleted." ();
  error Api_errors.usb_group_contains_pusb ["pusbs"]
    ~doc:"The USB group contains active PUSBs and cannot be deleted." ();
  error Api_errors.usb_group_contains_no_pusbs ["usb_group"]
    ~doc:"The USB group does not contain any PUSBs." ();
  error Api_errors.too_many_vusbs [ "number" ]
    ~doc:"The VM has too many VUSBs." ();
  error Api_errors.usb_group_conflict [ "USB_group" ]
    ~doc:"USB_groups are currently restricted to contain no more than one VUSB." ();
  error Api_errors.usb_already_attached [ "PUSB"; "VM" ]
    ~doc:"The USB device is currently attached to a VM." ();
  error Api_errors.passthrough_not_enabled [ "PUSB"]
    ~doc:"The passthrough_enabled must be true before passthrough usb to vm." ();
  error Api_errors.pusb_vdi_conflict [ "PUSB"; "VDI" ]
    ~doc:"The VDI corresponding to this PUSB has existing VBDs." ();
  error Api_errors.vm_has_vusbs ["VM"]
    ~doc:"The operation is not allowed when the VM has VUSBs." ();

  (* clustering errors *)
  error Api_errors.cluster_create_in_progress []
    ~doc:"The operation could not be performed because cluster creation is in progress." ();
  error Api_errors.cluster_already_exists []
    ~doc:"A cluster already exists in the pool." ();
  error Api_errors.clustering_enabled ["cluster_host"]
    ~doc:"An operation was attempted while clustering was enabled on the cluster_host." ();
  error Api_errors.clustering_disabled ["cluster_host"]
    ~doc:"An operation was attempted while clustering was disabled on the cluster_host." ();
  error Api_errors.cluster_does_not_have_one_node ["number_of_nodes"]
    ~doc:"An operation failed as it expected the cluster to have only one node but found multiple cluster_hosts." ();
  error Api_errors.no_compatible_cluster_host ["host"]
    ~doc:"The host does not have a Cluster_host with a compatible cluster stack." ();
  error Api_errors.cluster_force_destroy_failed ["cluster"]
    ~doc:"Force destroy failed on a Cluster_host while force destroying the cluster." ();
  error Api_errors.cluster_stack_in_use ["cluster_stack"]
    ~doc:"The cluster stack is already in use." ();
  error Api_errors.invalid_cluster_stack [ "cluster_stack" ]
    ~doc:"The cluster stack provided is not supported." ();
  error Api_errors.pif_not_attached_to_host [ "pif"; "host" ]
    ~doc:"Cluster_host creation failed as the PIF provided is not attached to the host." ();
  error Api_errors.cluster_host_not_joined [ "cluster_host" ]
    ~doc:"Cluster_host operation failed as the cluster_host has not joined the cluster." ();
  error Api_errors.cluster_host_is_last ["cluster_host"]
    ~doc:"The last cluster host cannot be destroyed. Destroy the cluster instead" ();
  error Api_errors.no_cluster_hosts_reachable ["cluster"]
    ~doc:"No other cluster host was reachable when joining" ()



let _ =
  message (fst Api_messages.ha_pool_overcommitted) ~doc:"Pool has become overcommitted: it can no longer guarantee to restart protected VMs if the configured number of hosts fail." ();
  message (fst Api_messages.ha_statefile_lost) ~doc:"Host lost access to HA storage heartbeat" ();
  message (fst Api_messages.ha_heartbeat_approaching_timeout) ~doc:"HA network heartbeat almost timed-out" ();
  message (fst Api_messages.ha_statefile_approaching_timeout) ~doc:"HA storage heartbeat almost timed-out" ();
  message (fst Api_messages.ha_xapi_healthcheck_approaching_timeout) ~doc:"HA xapi healthcheck almost timed-out" ();
  message (fst Api_messages.ha_network_bonding_error) ~doc:"HA network heartbeat interface bonding error" ();
  message (fst Api_messages.vif_qos_failed) ~doc:"Applying QoS to VIF failed." ();
  message (fst Api_messages.vbd_qos_failed) ~doc:"Applying QoS to VBD failed." ();
  message (fst Api_messages.vcpu_qos_failed) ~doc:"Applying QoS to VCPU failed." ();
  message (fst Api_messages.pool_master_transition) ~doc:"Host has become the new Pool master." ();
  message (fst Api_messages.pbd_plug_failed_on_server_start) ~doc:"Host failed to attach one or more Storage Repositories." ();
  ()

