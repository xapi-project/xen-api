(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** Data Model and Message Specification for Xen Management Tools *)

open Datamodel_types

(* IMPORTANT: Please bump schema vsn if you change/add/remove a _field_.
              You do not have to bump vsn if you change/add/remove a message *)
let schema_major_vsn = 5
let schema_minor_vsn = 80

(* Historical schema versions just in case this is useful later *)
let rio_schema_major_vsn = 5
let rio_schema_minor_vsn = 19

let miami_release_schema_major_vsn = 5
let miami_release_schema_minor_vsn = 35

let orlando_release_schema_major_vsn = 5
let orlando_release_schema_minor_vsn = 55

let george_release_schema_major_vsn = 5
let george_release_schema_minor_vsn = 57

let midnight_ride_release_schema_major_vsn = 5
let midnight_ride_release_schema_minor_vsn = 60

let cowley_release_schema_major_vsn = 5
let cowley_release_schema_minor_vsn = 61

let boston_release_schema_major_vsn = 5
let boston_release_schema_minor_vsn = 63

let tampa_release_schema_major_vsn = 5
let tampa_release_schema_minor_vsn = 66

let clearwater_release_schema_major_vsn = 5
let clearwater_release_schema_minor_vsn = 67

let augusta_release_schema_major_vsn = 5
let augusta_release_schema_minor_vsn = 80

(* the schema vsn of the last release: used to determine whether we can upgrade or not.. *)
let last_release_schema_major_vsn = clearwater_release_schema_major_vsn
let last_release_schema_minor_vsn = clearwater_release_schema_minor_vsn

(** Bindings for currently specified releases *)

(** Name of variable which refers to reference in the parameter list *)
let _self = "self"

(** All the various object names *)

let _session = "session"
let _task = "task"
let _user = "user"
let _host = "host"
let _host_metrics = "host_metrics"
let _host_crashdump = "host_crashdump"
let _host_patch = "host_patch"
let _hostcpu = "host_cpu"
let _sr = "SR"
let _sm = "SM"
let _vm = "VM"
let _vm_metrics = "VM_metrics"
let _vm_guest_metrics = "VM_guest_metrics"
let _vm_appliance = "VM_appliance"
let _dr_task = "DR_task"
let _vmpp = "VMPP"
let _network = "network"
let _vif = "VIF"
let _vif_metrics = "VIF_metrics"
let _pif = "PIF"
let _pif_metrics = "PIF_metrics"
let _bond = "Bond"
let _vlan = "VLAN"
let _pbd = "PBD"
let _vdi = "VDI"
let _vbd = "VBD"
let _vbd_metrics = "VBD_metrics"
let _vtpm = "VTPM"
let _console = "console"
let _event = "event"
let _alert = "alert"
let _crashdump = "crashdump"
let _pool = "pool"
let _pool_patch = "pool_patch"
let _data_source = "data_source"
let _blob = "blob"
let _message = "message"
let _auth = "auth"
let _subject = "subject"
let _role = "role"
let _secret = "secret"
let _tunnel = "tunnel"
let _pci = "PCI"
let _pgpu = "PGPU"
let _gpu_group = "GPU_group"
let _vgpu = "VGPU"

(** All the various static role names *)

let role_pool_admin = "pool-admin"
let role_pool_operator = "pool-operator"
let role_vm_power_admin = "vm-power-admin"
let role_vm_admin = "vm-admin"
let role_vm_operator = "vm-operator"
let role_read_only = "read-only"
let roles_all = 
	[ (* in decreasing total linear order of privileges *)
		role_pool_admin;
		role_pool_operator;
		role_vm_power_admin;
		role_vm_admin;
		role_vm_operator;
		role_read_only
	]
let role_description = [
	role_pool_admin,"The Pool Administrator role has full access to all features and settings, including accessing Dom0 and managing subjects, roles and external authentication";
	role_pool_operator,"The Pool Operator role manages host- and pool-wide resources, including setting up storage, creating resource pools and managing patches and high availability (HA)";
	role_vm_power_admin,"The VM Power Administrator role has full access to VM and template management and can choose where to start VMs and use the dynamic memory control and VM snapshot features";
	role_vm_admin,"The VM Administrator role can manage VMs and templates";
	role_vm_operator,"The VM Operator role can use VMs and interact with VM consoles";
	role_read_only,"The Read-Only role can log in with basic read-only access";
]
(* obtain all roles with at least the specified role privileges *)
let roles_gte role = 
	let rec gte = function []->failwith "invalid role"
		|x::xs->if x=role then x::[] else x::gte xs in
	gte roles_all
(* shortcuts to subsets of greater than or equal roles *)
let _R_LOCAL_ROOT_ONLY = Some([]) (* only local root, emergency and pool-secret *)
let _R_POOL_ADMIN = Some(roles_gte role_pool_admin)
let _R_POOL_OP = Some(roles_gte role_pool_operator)
let _R_VM_POWER_ADMIN = Some(roles_gte role_vm_power_admin)
let _R_VM_ADMIN = Some(roles_gte role_vm_admin)
let _R_VM_OP = Some(roles_gte role_vm_operator)
let _R_READ_ONLY = Some(roles_gte role_read_only) (* = all *)
let _R_ALL = _R_READ_ONLY

(******************************************************************************************************************)
(* Define additional RPCs for objects *)

let errors = Hashtbl.create 10
let messages = Hashtbl.create 10

exception UnspecifiedRelease

let get_oss_releases in_oss_since =
  match in_oss_since with
    None -> []
  | Some "3.0.3" -> ["3.0.3"]
  | _ -> raise UnspecifiedRelease

let get_product_releases in_product_since =
  let rec go_through_release_order rs =
    match rs with
      [] -> raise UnspecifiedRelease
    | x::xs -> if x=in_product_since then "closed"::x::xs else go_through_release_order xs
  in go_through_release_order release_order

let autusta_release =
	{ internal = get_product_releases rel_augusta
	; opensource=get_oss_releases None
	; internal_deprecated_since=None
	}

let clearwater_release =
	{ internal=get_product_releases rel_clearwater
	; opensource=get_oss_releases None
	; internal_deprecated_since=None
	}

let tampa_release =
	{ internal=get_product_releases rel_tampa
	; opensource=get_oss_releases None
	; internal_deprecated_since=None
	}

let boston_release =
	{ internal=get_product_releases rel_boston
	; opensource=get_oss_releases None
	; internal_deprecated_since=None
	}

let cowley_release =
  { internal=get_product_releases rel_cowley
  ; opensource=get_oss_releases None
  ; internal_deprecated_since=None
  }

let midnight_ride_release =
	{ internal=get_product_releases "midnight-ride"
	; opensource=get_oss_releases None
	; internal_deprecated_since=None
	}

let george_release =
	{ internal=get_product_releases "george"
	; opensource=get_oss_releases None
	; internal_deprecated_since=None
	}

let orlando_release =
	{ internal=get_product_releases "orlando"
	; opensource=get_oss_releases None
	; internal_deprecated_since=None
	}

let miami_symc_release =
	{ internal=get_product_releases "symc"
	; opensource=get_oss_releases None
	; internal_deprecated_since=None
	}

let miami_release =
	{ internal=get_product_releases "miami"
	; opensource=get_oss_releases None
	; internal_deprecated_since=None
	}

let rio_release =
	{ internal=get_product_releases "rio"
	; opensource=get_oss_releases (Some "3.0.3")
	; internal_deprecated_since=None
	}

let get_published lifecycle =
	try
		let _, published, _ = List.find (fun (t, _, _) -> t = Published) lifecycle in
		Some published
	with Not_found -> None

let get_deprecated lifecycle =
	try
		let _, deprecated, _ = List.find (fun (t, _, _) -> t = Deprecated) lifecycle in
		Some deprecated
	with Not_found -> None

let call ~name ?(doc="") ?(in_oss_since=Some "3.0.3") ?in_product_since ?internal_deprecated_since
	?result ?(flags=[`Session;`Async])
	?(effect=true) ?(tag=Custom) ?(errs=[]) ?(custom_marshaller=false) ?(db_only=false)
	?(no_current_operations=false) ?(secret=false) ?(hide_from_docs=false)
	?(pool_internal=false)
	~allowed_roles
	?(map_keys_roles=[])
	?(params=[]) ?versioned_params ?lifecycle () =
	(* if you specify versioned_params then these get put in the params field of the message record;
	 * otherwise params go in with no default values and param_release=call_release...
	 *)
	if lifecycle = None && in_product_since = None then
		failwith ("Lifecycle for message '" ^ name ^ "' not specified");
	let lifecycle = match lifecycle with
		| None ->
			let published = match in_product_since with
				| None -> []
				| Some rel -> [Published, rel, doc]
			in
			let deprecated = match internal_deprecated_since with
				| None -> []
				| Some rel -> [Deprecated, rel, ""]
			in
			published @ deprecated
		| Some l -> l
	in
	let call_release =
		{
			internal = (match get_published lifecycle with 
				| Some published -> get_product_releases published
				| None -> ["closed"]);
			opensource = get_oss_releases in_oss_since;
			internal_deprecated_since = get_deprecated lifecycle;
		}
	in
	{ 
		msg_name = name;
		msg_params =
			(match versioned_params with
			| None ->
				List.map (fun (ptype, pname, pdoc) -> {param_type=ptype; param_name=pname;
					param_doc=pdoc; param_release=call_release; param_default=None}) params
			| Some ps -> ps);
		msg_result = result; msg_doc = doc;
		msg_session = List.mem `Session flags; msg_async = List.mem `Async flags;
		msg_db_only = db_only;
		msg_release = call_release;
		msg_lifecycle = lifecycle;
		msg_has_effect = effect; msg_tag = tag; msg_obj_name="";
		msg_force_custom = None;
		msg_errors = List.map (Hashtbl.find errors) errs; msg_secret = secret;
		msg_custom_marshaller = custom_marshaller;
		msg_no_current_operations = no_current_operations;
		msg_hide_from_docs = hide_from_docs;
		msg_pool_internal = pool_internal;
		msg_allowed_roles = allowed_roles;
		msg_map_keys_roles = map_keys_roles
	}

let assert_operation_valid enum cls self = call 
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~name:"assert_operation_valid"
  ~doc:"Check to see whether this operation is acceptable in the current state of the system, raising an error if the operation is invalid for some reason"
  ~params:[Ref cls, self, "reference to the object";
	   enum, "op", "proposed operation" ]
  ~allowed_roles:_R_POOL_ADMIN
  ()

let update_allowed_operations enum cls self = call
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~name:"update_allowed_operations"
  ~doc:"Recomputes the list of acceptable operations"
  ~params:[Ref cls, self, "reference to the object"]
  ~allowed_roles:_R_POOL_ADMIN
  ()

(** Compute an enum constant corresponding to an operation, for current_operations,
    allowed_operations.*)
let operation_enum x = 
    x.msg_name, Printf.sprintf "refers to the operation \"%s\"" x.msg_name 

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
  error Api_errors.license_restriction []
    ~doc:"This operation is not allowed under your license.  Please contact your support representative." ();
  error Api_errors.license_cannot_downgrade_in_pool []
    ~doc:"Cannot downgrade license while in pool. Please disband the pool first, then downgrade licenses on hosts separately." ();
  error Api_errors.license_does_not_support_pooling []
    ~doc:"This host cannot join a pool because it's license does not support pooling" ();
  error Api_errors.license_does_not_support_xha []
    ~doc:"XHA cannot be enabled because this host's license does not allow it" ();

  error Api_errors.v6d_failure []
    ~doc:"There was a problem with the license daemon (v6d). Is it running?" ();
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

  (* Session errors *)
  error Api_errors.session_authentication_failed [] 
    ~doc:"The credentials given by the user are incorrect, so access has been denied, and you have not been issued a session handle." ();
  error Api_errors.session_invalid ["handle"] 
    ~doc:"You gave an invalid session reference.  It may have been invalidated by a server restart, or timed out.  You should get a new session handle, using one of the session.login_ calls.  This error does not invalidate the current connection.  The handle parameter echoes the bad value given." ();
  error Api_errors.change_password_rejected [ "msg" ]
    ~doc:"The system rejected the password change request; perhaps the new password was too short?" ();
  error Api_errors.user_is_not_local_superuser [ "msg" ]
    ~doc:"Only the local superuser can execute this operation" ();

  (* PIF/VIF/Network errors *)
  error Api_errors.network_already_connected ["network"; "connected PIF"]
    ~doc:"You tried to create a PIF, but the network you tried to attach it to is already attached to some other PIF, and so the creation failed." ();
  error Api_errors.cannot_destroy_system_network [ "network" ]
	  ~doc:"You tried to destroy a system network: these cannot be destroyed." ();
  error Api_errors.pif_is_physical ["PIF"]
    ~doc:"You tried to destroy a PIF, but it represents an aspect of the physical host configuration, and so cannot be destroyed.  The parameter echoes the PIF handle you gave." ();
  error Api_errors.pif_is_vlan ["PIF"]
    ~doc:"You tried to create a VLAN on top of another VLAN - use the underlying physical PIF/bond instead" ();

  error Api_errors.pif_vlan_exists ["PIF"]
    ~doc:"You tried to create a PIF, but it already exists." ();
  error Api_errors.pif_vlan_still_exists [ "PIF" ]
    ~doc:"Operation cannot proceed while a VLAN exists on this interface." ();
  error Api_errors.pif_already_bonded [ "PIF" ]
    ~doc:"This operation cannot be performed because the pif is bonded." ();
  error Api_errors.pif_cannot_bond_cross_host []
    ~doc:"You cannot bond interfaces across different hosts." ();
  error Api_errors.pif_bond_needs_more_members []
    ~doc:"A bond must consist of at least two member interfaces" ();
  error Api_errors.pif_configuration_error [ "PIF"; "msg" ]
    ~doc:"An unknown error occurred while attempting to configure an interface." ();
  error Api_errors.invalid_ip_address_specified [ "parameter" ]
    ~doc:"A required parameter contained an invalid IP address" ();
  error Api_errors.pif_is_management_iface [ "PIF" ]
    ~doc:"The operation you requested cannot be performed because the specified PIF is the management interface." ();
  error Api_errors.pif_does_not_allow_unplug [ "PIF" ]
    ~doc:"The operation you requested cannot be performed because the specified PIF does not allow unplug." ();
  error Api_errors.pif_has_no_network_configuration [ ]
    ~doc:"PIF has no IP configuration (mode currently set to 'none')" ();
  error Api_errors.pif_has_no_v6_network_configuration [ ]
    ~doc:"PIF has no IPv6 configuration (mode currently set to 'none')" ();
  error Api_errors.pif_incompatible_primary_address_type [ ]
    ~doc:"The primary address types are not compatible" ();
  error Api_errors.cannot_plug_bond_slave ["PIF"]
    ~doc:"This PIF is a bond slave and cannot be plugged." ();
  error Api_errors.cannot_add_vlan_to_bond_slave ["PIF"]
    ~doc:"This PIF is a bond slave and cannot have a VLAN on it." ();
  error Api_errors.cannot_add_tunnel_to_bond_slave ["PIF"]
    ~doc:"This PIF is a bond slave and cannot have a tunnel on it." ();
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

  error Api_errors.openvswitch_not_active []
    ~doc:"This operation needs the OpenVSwitch networking backend to be enabled on all hosts in the pool." ();
  error Api_errors.transport_pif_not_configured ["PIF"]
    ~doc:"The tunnel transport PIF has no IP configuration set." ();
  error Api_errors.is_tunnel_access_pif ["PIF"]
    ~doc:"You tried to create a VLAN or tunnel on top of a tunnel access PIF - use the underlying transport PIF instead." ();
  error Api_errors.pif_tunnel_still_exists ["PIF"]
    ~doc:"Operation cannot proceed while a tunnel exists on this interface." ();

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
  error Api_errors.vm_hvm_required ["vm"]
    ~doc:"HVM is required for this operation" ();
  error Api_errors.vm_no_vcpus ["vm"]
    ~doc:"You need at least 1 VCPU to start a VM" ();
  error Api_errors.vm_toomany_vcpus ["vm"]
    ~doc:"Too many VCPUs to start this VM" ();
  error Api_errors.host_not_enough_free_memory [ "needed"; "available" ]
    ~doc:"Not enough host memory is available to perform this operation" ();
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
    ~doc:"The Citrix XenServer Vss Provider is not loaded" ();
  error Api_errors.xen_vss_req_error_no_volumes_supported [ "vm"; "error_code" ]
    ~doc:"Could not find any volumes supported by the Citrix XenServer Vss Provider" ();
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
    ~doc:"This host can not be forgotten because there are some user VMs still running" ();

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
  error Api_errors.pool_joining_host_must_have_physical_managment_nic []
    ~doc:"The host joining the pool must have a physical management NIC (i.e. the management NIC must not be on a VLAN or bonded PIF)." ();
  error Api_errors.pool_joining_external_auth_mismatch []
    ~doc:"Cannot join pool whose external authentication configuration is different." ();
  error Api_errors.pool_joining_host_must_have_same_product_version []
    ~doc:"The host joining the pool must have the same product version as the pool master." ();
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
  error Api_errors.pool_auth_disable_failed ["host";"message"]
    ~doc:"The pool failed to disable the external authentication of at least one host." ();
  error Api_errors.pool_auth_disable_failed_wrong_credentials ["host";"message"]
    ~doc:"The pool failed to disable the external authentication of at least one host." ();
  error Api_errors.pool_auth_disable_failed_permission_denied ["host";"message"]
    ~doc:"The pool failed to disable the external authentication of at least one host." ();

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
    ~doc:"The WLB server refused a connection to XenServer." ();
  error Api_errors.wlb_unknown_host []
    ~doc:"The configured WLB server name failed to resolve in DNS." ();
  error Api_errors.wlb_timeout ["configured_timeout"]
    ~doc:"The communication with the WLB server timed out." ();
  error Api_errors.wlb_authentication_failed []
    ~doc:"The WLB server rejected our configured authentication details." ();
  error Api_errors.wlb_malformed_request []
    ~doc:"The WLB server rejected XenServer's request as malformed." ();
  error Api_errors.wlb_malformed_response ["method"; "reason"; "response"]
    ~doc:"The WLB server said something that XenServer wasn't expecting or didn't understand.  The method called on the WLB server, a diagnostic reason, and the response from WLB are returned." ();
  error Api_errors.wlb_xenserver_connection_refused []
    ~doc:"The WLB server reported that XenServer refused it a connection (even though we're connecting perfectly fine in the other direction)." ();
  error Api_errors.wlb_xenserver_unknown_host []
    ~doc:"The WLB server reported that its configured server name for this XenServer instance failed to resolve in DNS." ();
  error Api_errors.wlb_xenserver_timeout []
    ~doc:"The WLB server reported that communication with XenServer timed out." ();
  error Api_errors.wlb_xenserver_authentication_failed []
    ~doc:"The WLB server reported that XenServer rejected its configured authentication details." ();
  error Api_errors.wlb_xenserver_malformed_response []
    ~doc:"The WLB server reported that XenServer said something to it that WLB wasn't expecting or didn't understand." ();
  error Api_errors.wlb_internal_error []
    ~doc:"The WLB server reported an internal error." ();
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
  error Api_errors.vm_requires_iommu ["host"]
    ~doc:"You attempted to run a VM on a host which doesn't have I/O virtualization (IOMMU/VT-d) enabled, which is needed by the VM." ();
  error Api_errors.vm_host_incompatible_version ["host"; "vm"]
    ~doc:"This VM operation cannot be performed on an older-versioned host during an upgrade." ();
  error Api_errors.vm_has_pci_attached ["vm"]
    ~doc:"This operation could not be performed, because the VM has one or more PCI devices passed through." ();
  error Api_errors.host_cannot_attach_network [ "host"; "network" ]
    ~doc:"Host cannot attach network (in the case of NIC bonding, this may be because attaching the network on this host would require other networks [that are currently active] to be taken down)." ();
  error Api_errors.vm_requires_vdi [ "vm"; "vdi" ]
    ~doc:"VM cannot be started because it requires a VDI which cannot be attached" ();
  error Api_errors.vm_migrate_failed [ "vm"; "source"; "destination"; "msg" ]
    ~doc:"An error occurred during the migration process." ();
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
  error Api_errors.vm_failed_shutdown_ack []
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
	error Api_errors.vm_attached_to_more_than_one_vdi_with_timeoffset_marked_as_reset_on_boot [ "vm" ]
		~doc:"You attempted to start a VM that's attached to more than one VDI with a timeoffset marked as reset-on-boot." ();
  error Api_errors.vms_failed_to_cooperate [ ]
    ~doc:"The given VMs failed to release memory when instructed to do so" ();


  (* Storage errors *)
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
  error Api_errors.vdi_copy_failed []
    ~doc:"The VDI copy action has failed" (); 
  error Api_errors.cannot_create_state_file []
    ~doc:"An HA statefile could not be created, perhaps because no SR with the appropriate capability was found." ();

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
  error Api_errors.patch_is_applied [ ]
    ~doc:"The specified patch is applied and cannot be destroyed." ();
  error Api_errors.patch_already_applied [ "patch" ]
    ~doc:"This patch has already been applied" ();
  error Api_errors.patch_apply_failed [ "output" ]
    ~doc:"The patch apply failed.  Please see attached output." ();
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
  error Api_errors.patch_precheck_tools_iso_mounted ["patch"]
    ~doc:"XenServer Tools ISO must be ejected from all running VMs." ();

  error Api_errors.cannot_find_oem_backup_partition []
    ~doc:"The backup partition to stream the updat to cannot be found" ();
  error Api_errors.only_allowed_on_oem_edition ["command"]
    ~doc:"This command is only allowed on the OEM edition." ();
  error Api_errors.not_allowed_on_oem_edition ["command"]
    ~doc:"This command is not allowed on the OEM edition." ();

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

  error Api_errors.ssl_verify_error ["reason"]
    ~doc:"The remote system's SSL certificate failed to verify against our certificate library." ();
	
  error Api_errors.cannot_enable_redo_log ["reason"] 
	~doc:"Could not enable redo log." ();

  error Api_errors.redo_log_is_enabled [] 
	~doc:"The operation could not be performed because a redo log is enabled on the Pool." ();
	
  error Api_errors.vm_bios_strings_already_set []
    ~doc:"The BIOS strings for this VM have already been set and cannot be changed anymore." ();
	
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
		~doc:"The VM cannot be imported unforced because it is either the same version or an older version of an existing VM." ()


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

(* ------------------------------------------------------------------------------------------------------------
   Session Management
   ------------------------------------------------------------------------------------------------------------ *)

(* Session.Login *)

let session_login  = call ~flags:[]
  ~name:"login_with_password"
  ~in_product_since:rel_rio
  ~doc:"Attempt to authenticate the user, returning a session reference if successful"
  ~result:(Ref _session,"reference of newly created session")
  ~versioned_params:
  [{param_type=String; param_name="uname"; param_doc="Username for login."; param_release=rio_release; param_default=None};
   {param_type=String; param_name="pwd"; param_doc="Password for login."; param_release=rio_release; param_default=None};
   {param_type=String; param_name="version"; param_doc="Client API version."; param_release=miami_release; param_default=Some (VString "1.1")};
   {param_type=String; param_name="originator"; param_doc="Key string for distinguishing different API users sharing the same login name."; param_release=clearwater_release; param_default=Some (VString "")}
  ]
  ~errs:[Api_errors.session_authentication_failed]
  ~secret:true
  ~allowed_roles:_R_ALL (*any static role can try to create a user session*)
  ()

let slave_login  = call ~flags:[]
  ~name:"slave_login"
  ~doc:"Attempt to authenticate to the pool master by presenting the slave's host ref and pool secret"
  ~result:(Ref _session,"ID of newly created session")
  ~params:[
	    Ref _host, "host", "Host id of slave";
	    String, "psecret", "Pool secret"
	  ]
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~secret:true
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_ADMIN (*system can create a slave session !!! *)
  ()

let slave_local_login = call ~flags:[]
  ~in_product_since:rel_miami
  ~name:"slave_local_login"
  ~doc:"Authenticate locally against a slave in emergency mode. Note the resulting sessions are only good for use on this host."
  ~result:(Ref _session,"ID of newly created session")
  ~params:[
	    String, "psecret", "Pool secret"
	  ]
  ~in_oss_since:None
  ~secret:true
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_ADMIN (*system can create a slave session*)
  ()

let slave_local_login_with_password = call ~flags:[]
  ~in_product_since:rel_miami
  ~name:"slave_local_login_with_password"
  ~doc:"Authenticate locally against a slave in emergency mode. Note the resulting sessions are only good for use on this host."
  ~result:(Ref _session,"ID of newly created session")
  ~params:[
	    String, "uname", "Username for login.";
            String, "pwd", "Password for login.";
	  ]
  ~in_oss_since:None
  ~secret:true
  ~allowed_roles:_R_POOL_ADMIN (*only root can do an emergency slave login*)
  ()

let local_logout = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"local_logout"
  ~doc:"Log out of local session."
  ~params:[]
  ~in_oss_since:None
  ~allowed_roles:_R_POOL_ADMIN (*system can destroy a local session*)
  ()

(* Session.Logout *)
  
let session_logout = call ~flags:[`Session]
  ~in_product_since:rel_rio
  ~name:"logout"
  ~doc:"Log out of a session"
  ~params:[]
  ~allowed_roles:_R_ALL (*any role can destroy a known user session*)
  ()

let session_chpass = call ~flags:[`Session]
  ~name:"change_password"
  ~doc:"Change the account password; if your session is authenticated with root priviledges then the old_pwd is validated and the new_pwd is set regardless"
  ~params:[
	    String, "old_pwd", "Old password for account";
	    String, "new_pwd", "New password for account"
	  ]
  ~in_product_since:rel_rio
  ~in_oss_since:None
  ~allowed_roles:_R_LOCAL_ROOT_ONLY (*not even pool-admin can change passwords, only root*)
  ()

(* static function for class session *)
let session_get_all_subject_identifiers = call
  ~name:"get_all_subject_identifiers"
  ~doc:"Return a list of all the user subject-identifiers of all existing sessions"
  ~result:(Set (String), "The list of user subject-identifiers of all existing sessions")
  ~params:[]
  ~in_product_since:rel_george
  ~in_oss_since:None
  ~allowed_roles:_R_ALL
  ()

(* static function for class session *)
let session_logout_subject_identifier = call
  ~name:"logout_subject_identifier"
  ~doc:"Log out all sessions associated to a user subject-identifier, except the session associated with the context calling this function"
  ~params:[
	    String, "subject_identifier", "User subject-identifier of the sessions to be destroyed"
	  ]
  ~in_product_since:rel_george
  ~in_oss_since:None
  ~allowed_roles:_R_POOL_OP
  ()

(* ------------------------------------------------------------------------------------------------------------
   Asynchronous Task Management
   ------------------------------------------------------------------------------------------------------------ *)

let cancel_result = Enum ("cancel_result",
			  [ "OK", "OK";
			    "Failed", "Not OK" ])

(* ------------------------------------------------------------------------------------------------------------
   RRD Consolidation function specification 
   ------------------------------------------------------------------------------------------------------------ *)

let rrd_cf_type = Enum ("rrd_cf_type",
		       [ "Average", "Average";
			 "Min", "Minimum";
			 "Max", "Maximum";
			 "Last", "Last value" ])


(* ------------------------------------------------------------------------------------------------------------
   VM Management
   ------------------------------------------------------------------------------------------------------------ *)

(* Install and UnInstall correspond to autogenerate create/delete functions *)

let vm_get_boot_record = call
  ~name:"get_boot_record"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~doc:"Returns a record describing the VM's dynamic state, initialised when the VM boots and updated to reflect runtime configuration changes e.g. CPU hotplug"
  ~result:(Record _vm, "A record describing the VM")
  ~params:[Ref _vm, "self", "The VM whose boot-time state to return"]
  ~errs:[]
  ~flags:[`Session] (* no async *)
  ~allowed_roles:_R_READ_ONLY
  ()

let vm_get_data_sources = call
  ~name:"get_data_sources"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:""
  ~result:(Set (Record _data_source), "A set of data sources")
  ~params:[Ref _vm, "self", "The VM to interrogate"]
  ~errs:[]
  ~flags:[`Session] 
  ~allowed_roles:_R_READ_ONLY
  ()

let vm_record_data_source = call
  ~name:"record_data_source"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:"Start recording the specified data source"
  ~params:[Ref _vm, "self", "The VM";
	   String, "data_source", "The data source to record"]
  ~errs:[]
  ~flags:[`Session]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vm_query_data_source = call
  ~name:"query_data_source"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:"Query the latest value of the specified data source"
  ~params:[Ref _vm, "self", "The VM";
	   String, "data_source", "The data source to query"]
  ~result:(Float,"The latest value, averaged over the last 5 seconds")
  ~errs:[]
  ~flags:[`Session]
  ~allowed_roles:_R_READ_ONLY
  ()

let vm_forget_data_source_archives = call
  ~name:"forget_data_source_archives"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:"Forget the recorded statistics related to the specified data source"
  ~params:[Ref _vm, "self", "The VM";
	   String, "data_source", "The data source whose archives are to be forgotten"]
  ~flags:[`Session]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vm_set_ha_always_run = call
  ~name:"set_ha_always_run"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:"Set the value of the ha_always_run"
  ~params:[Ref _vm, "self", "The VM";
	   Bool, "value", "The value"]
  ~flags:[`Session]
  ~allowed_roles:_R_POOL_OP
  ~internal_deprecated_since:rel_boston
  ()

let vm_set_ha_restart_priority = call
  ~name:"set_ha_restart_priority"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:"Set the value of the ha_restart_priority field"
  ~params:[Ref _vm, "self", "The VM";
	   String, "value", "The value"]
  ~flags:[`Session]
  ~allowed_roles:_R_POOL_OP
  ()

(* VM.Clone *)

let vm_clone = call
  ~name:"clone"
  ~in_product_since:rel_rio
  ~doc:"Clones the specified VM, making a new VM. Clone automatically exploits the capabilities of the underlying storage repository in which the VM's disk images are stored (e.g. Copy on Write).   This function can only be called when the VM is in the Halted State."
  ~result:(Ref _vm, "The reference of the newly created VM.")
  ~params:[
	    Ref _vm, "vm", "The VM to be cloned";
	    String, "new_name", "The name of the cloned VM"
	  ]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.sr_full; Api_errors.operation_not_allowed]
  ~allowed_roles:_R_VM_ADMIN
  ()

(* VM.Copy *)
let vm_copy = call
  ~name:"copy"
  ~lifecycle:[
	Published, rel_rio, "Copies a VM to an SR. There must be a host that can see both the source and destination SRs simultaneously";
	Extended, rel_cowley, "The copy can now be performed between any two SRs." ]
  ~doc:"Copied the specified VM, making a new VM. Unlike clone, copy does not exploits the capabilities of the underlying storage repository in which the VM's disk images are stored. Instead, copy guarantees that the disk images of the newly created VM will be 'full disks' - i.e. not part of a CoW chain.  This function can only be called when the VM is in the Halted State."
  ~result:(Ref _vm, "The reference of the newly created VM.")
  ~params:[
	    Ref _vm, "vm", "The VM to be copied";
	    String, "new_name", "The name of the copied VM";
	    Ref _sr, "sr", "An SR to copy all the VM's disks into (if an invalid reference then it uses the existing SRs)";
	  ]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.sr_full; Api_errors.operation_not_allowed]
  ~allowed_roles:_R_VM_ADMIN
  ()

(* VM.snapshot *)
let vm_snapshot_with_quiesce = call
  ~name:"snapshot_with_quiesce"
  ~in_product_since: rel_orlando
  ~doc:"Snapshots the specified VM with quiesce, making a new VM. Snapshot automatically exploits the capabilities of the underlying storage repository in which the VM's disk images are stored (e.g. Copy on Write)."
  ~result: (Ref _vm, "The reference of the newly created VM.")
  ~params:[
    Ref _vm, "vm", "The VM to be snapshotted";
    String, "new_name", "The name of the snapshotted VM"
  ]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.sr_full; Api_errors.operation_not_allowed;
		Api_errors.vm_snapshot_with_quiesce_failed;
		Api_errors.vm_snapshot_with_quiesce_timeout;
		Api_errors.vm_snapshot_with_quiesce_plugin_does_not_respond;
		Api_errors.vm_snapshot_with_quiesce_not_supported ]
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let vm_update_snapshot_metadata = call
  ~name:"update_snapshot_metadata"
  ~in_product_since: rel_george
  ~internal_deprecated_since:rel_midnight_ride
  ~doc:""
  ~hide_from_docs:true
  ~params:[
    Ref _vm, "vm", "The VM to update";
    Ref _vm, "snapshot_of", "";
    DateTime, "snapshot_time", "";
    String, "transportable_snapshot_id", "" ]
  ~allowed_roles:_R_POOL_OP
  ()

let vm_snapshot = call
  ~name:"snapshot"
  ~in_product_since: rel_orlando
  ~doc:"Snapshots the specified VM, making a new VM. Snapshot automatically exploits the capabilities of the underlying storage repository in which the VM's disk images are stored (e.g. Copy on Write)."
  ~result: (Ref _vm, "The reference of the newly created VM.")
  ~params:[
    Ref _vm, "vm", "The VM to be snapshotted";
    String, "new_name", "The name of the snapshotted VM"
  ]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.sr_full; Api_errors.operation_not_allowed]
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let vm_revert = call
  ~name:"revert"
  ~in_product_since: rel_midnight_ride
  ~doc:"Reverts the specified VM to a previous state."
  ~params:[Ref _vm, "snapshot", "The snapshotted state that we revert to"]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.operation_not_allowed;
		Api_errors.sr_full; Api_errors.vm_revert_failed ]
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let vm_checkpoint = call
  ~name:"checkpoint"
  ~in_product_since: rel_midnight_ride
  ~doc:"Checkpoints the specified VM, making a new VM. Checkpoint automatically exploits the capabilities of the underlying storage repository in which the VM's disk images are stored (e.g. Copy on Write) and saves the memory image as well."
  ~result: (Ref _vm, "The reference of the newly created VM.")
  ~params:[
    Ref _vm, "vm", "The VM to be checkpointed";
    String, "new_name", "The name of the checkpointed VM"
  ]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.sr_full; Api_errors.operation_not_allowed;
		Api_errors.vm_checkpoint_suspend_failed; Api_errors.vm_checkpoint_resume_failed]
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let vm_create_template = call
  ~name:"create_template"
  ~hide_from_docs:true
  ~internal_deprecated_since:rel_midnight_ride
  ~in_product_since:rel_midnight_ride
  ~doc:"Deprecated: use VM.clone or VM.copy instead."
  ~result:(Ref _vm, "")
  ~params:[
	    Ref _vm, "vm", "";
	    String, "new_name", ""
	  ]
  ~errs:[]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vm_import_convert = call
	~name:"import_convert"
  ~in_product_since:rel_tampa
	~doc:"Import using a conversion service."
	~params:[
		String, "type", "Type of the conversion";
		String, "username", "Admin username on the host";
		String, "password", "Password on the host";
		Ref _sr, "sr", "The destination SR";
		Map(String, String), "remote_config", "Remote configuration options"
	]
	~errs:[]
	~allowed_roles:_R_VM_ADMIN
	()

(* VM.Provision -- causes the template's disks to be instantiated *)

let vm_provision = call
  ~name:"provision"
  ~doc:"Inspects the disk configuration contained within the VM's other_config, creates VDIs and VBDs and then executes any applicable post-install script."
  ~params:[
	    Ref _vm, "vm", "The VM to be provisioned";
	  ]
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.sr_full; Api_errors.operation_not_allowed]
  ~allowed_roles:_R_VM_ADMIN
  ()

(* VM.Start *)

let vm_start = call
	~name:"start"
	~in_product_since:rel_rio
	~doc:"Start the specified VM.  This function can only be called with the VM is in the Halted State."
	~params:[
		Ref _vm, "vm", "The VM to start";
		Bool, "start_paused", "Instantiate VM in paused state if set to true.";
		Bool, "force", "Attempt to force the VM to start. If this flag is false then the VM may fail pre-boot safety checks (e.g. if the CPU the VM last booted on looks substantially different to the current one)";
	]
	~errs:[
		Api_errors.vm_bad_power_state;
		Api_errors.vm_hvm_required;
		Api_errors.vm_is_template;
		Api_errors.other_operation_in_progress;
		Api_errors.operation_not_allowed;
		Api_errors.bootloader_failed;
		Api_errors.unknown_bootloader;
		Api_errors.no_hosts_available;
		Api_errors.license_restriction;
	]
	~allowed_roles:_R_VM_OP
	()

let vm_assert_can_boot_here = call
	~name:"assert_can_boot_here"
	~in_product_since:rel_rio
	~doc:"Returns an error if the VM could not boot on this host for some reason"
	~params:[
		Ref _vm, "self", "The VM";
		Ref _host, "host", "The host";
	]
	~allowed_roles:_R_READ_ONLY
	~errs:[
		Api_errors.host_not_enough_free_memory;
		Api_errors.vm_requires_sr;
		Api_errors.vm_host_incompatible_version;
	]
	()

let vm_assert_agile = call
	~name:"assert_agile"
	~in_product_since:rel_orlando
	~doc:"Returns an error if the VM is not considered agile e.g. because it is tied to a resource local to a host"
	~params:[Ref _vm, "self", "The VM"]
	~allowed_roles:_R_READ_ONLY
	()

let vm_get_possible_hosts = call
	~name:"get_possible_hosts"
	~in_product_since:rel_rio
	~doc:"Return the list of hosts on which this VM may run."
	~params:[Ref _vm, "vm", "The VM" ]
	~result:(Set (Ref _host), "The possible hosts")
	~allowed_roles:_R_READ_ONLY
	()

let wlb_removed =
	[ Published, rel_george, "";
	  Removed, rel_clearwater, "The WLB feature has been removed" ]

let vm_retrieve_wlb_recommendations = call
	~name:"retrieve_wlb_recommendations"
	~lifecycle:wlb_removed
	~doc:"Returns mapping of hosts to ratings, indicating the suitability of starting the VM at that location according to wlb. Rating is replaced with an error if the VM cannot boot there."
	~params:[Ref _vm, "vm", "The VM";]
	~result:(Map (Ref _host, Set(String)), "The potential hosts and their corresponding recommendations or errors")
	~allowed_roles:_R_READ_ONLY
	()
  

let vm_maximise_memory = call
  ~in_product_since:rel_miami
  ~name:"maximise_memory"
  ~doc:"Returns the maximum amount of guest memory which will fit, together with overheads, in the supplied amount of physical memory. If 'exact' is true then an exact calculation is performed using the VM's current settings. If 'exact' is false then a more conservative approximation is used"
  ~params:[Ref _vm, "self", "The VM";
	   Int, "total", "Total amount of physical RAM to fit within";
	   Bool, "approximate", "If false the limit is calculated with the guest's current exact configuration. Otherwise a more approximate calculation is performed";
	  ]
  ~result:(Int, "The maximum possible static-max")
  ~allowed_roles:_R_READ_ONLY
  ()

let vm_get_allowed_VBD_devices = call ~flags:[`Session] ~no_current_operations:true
  ~in_product_since:rel_rio
  ~name:"get_allowed_VBD_devices"
  ~doc:"Returns a list of the allowed values that a VBD device field can take"
  ~params:[Ref _vm,"vm","The VM to query"]
  ~result:(Set String, "The allowed values")
  ~allowed_roles:_R_READ_ONLY
  ()

let vm_get_allowed_VIF_devices = call ~flags:[`Session] ~no_current_operations:true
  ~in_product_since:rel_rio
  ~name:"get_allowed_VIF_devices"
  ~doc:"Returns a list of the allowed values that a VIF device field can take"
  ~params:[Ref _vm,"vm","The VM to query"]
  ~result:(Set String, "The allowed values")
  ~allowed_roles:_R_READ_ONLY
  ()

(* VM.atomic_set_resident_on *)
(* an internal call that sets resident_on and clears the scheduled_to_be_resident_on atomically *)

let vm_atomic_set_resident_on = call
  ~in_product_since:rel_rio
  ~pool_internal:true
  ~hide_from_docs:true
  ~name:"atomic_set_resident_on"
  ~doc:""
  ~params:[Ref _vm, "vm", "The VM to modify";
	   Ref _host, "host", "The host to set resident_on to"
          ]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let vm_compute_memory_overhead = call
	~in_product_since:rel_midnight_ride
	~name:"compute_memory_overhead"
	~doc:"Computes the virtualization memory overhead of a VM."
	~params:[Ref _vm, "vm", "The VM for which to compute the memory overhead"]
	~pool_internal:false
	~hide_from_docs:false
	~result:(Int, "the virtualization memory overhead of the VM.")
	~allowed_roles:_R_READ_ONLY
	()

let vm_set_memory_dynamic_max = call ~flags:[`Session]
	~in_product_since:rel_midnight_ride
	~name:"set_memory_dynamic_max"
	~doc:"Set the value of the memory_dynamic_max field"
	~params:[
		Ref _vm, "self", "The VM to modify";
		Int, "value", "The new value of memory_dynamic_max";
	]
	~allowed_roles:_R_VM_POWER_ADMIN
	~errs:[] ()

let vm_set_memory_dynamic_min = call ~flags:[`Session]
	~in_product_since:rel_midnight_ride
	~name:"set_memory_dynamic_min"
	~doc:"Set the value of the memory_dynamic_min field"
	~params:[
		Ref _vm, "self", "The VM to modify";
		Int, "value", "The new value of memory_dynamic_min";
	]
	~allowed_roles:_R_VM_POWER_ADMIN
	~errs:[] ()

let vm_set_memory_dynamic_range = call
	~name:"set_memory_dynamic_range"
	~in_product_since:rel_midnight_ride
	~doc:"Set the minimum and maximum amounts of physical memory the VM is \
		allowed to use."
	~allowed_roles:_R_VM_POWER_ADMIN
	~params:[
		Ref _vm, "self", "The VM";
		Int, "min", "The new minimum value";
		Int, "max", "The new maximum value";
	] ()

(* When HA is enabled we need to prevent memory *)
(* changes which will break the recovery plan.  *)
let vm_set_memory_static_max = call ~flags:[`Session]
	~in_product_since:rel_orlando
	~name:"set_memory_static_max"
	~doc:"Set the value of the memory_static_max field"
	~errs:[Api_errors.ha_operation_would_break_failover_plan]
	~allowed_roles:_R_VM_POWER_ADMIN
	~params:[
		Ref _vm, "self", "The VM to modify";
		Int, "value", "The new value of memory_static_max";
	] ()

let vm_set_memory_static_min = call ~flags:[`Session]
	~in_product_since:rel_midnight_ride
	~name:"set_memory_static_min"
	~doc:"Set the value of the memory_static_min field"
	~errs:[]
	~allowed_roles:_R_VM_POWER_ADMIN
	~params:[
		Ref _vm, "self", "The VM to modify";
		Int, "value", "The new value of memory_static_min";
	] ()

let vm_set_memory_static_range = call
	~name:"set_memory_static_range"
	~in_product_since:rel_midnight_ride
	~doc:"Set the static (ie boot-time) range of virtual memory that the VM is \
		allowed to use."
	~allowed_roles:_R_VM_POWER_ADMIN
	~params:[Ref _vm, "self", "The VM";
		Int, "min", "The new minimum value";
		Int, "max", "The new maximum value";
	] ()

let vm_set_memory_limits = call
	~name:"set_memory_limits"
	~in_product_since:rel_midnight_ride
	~doc:"Set the memory limits of this VM."
	~allowed_roles:_R_VM_POWER_ADMIN
	~params:[Ref _vm, "self", "The VM";
		Int, "static_min", "The new value of memory_static_min.";
		Int, "static_max", "The new value of memory_static_max.";
		Int, "dynamic_min", "The new value of memory_dynamic_min.";
		Int, "dynamic_max", "The new value of memory_dynamic_max.";
	] ()

let vm_set_memory_target_live = call
	~name:"set_memory_target_live"
	~in_product_since:rel_rio
	~internal_deprecated_since:rel_midnight_ride
	~doc:"Set the memory target for a running VM"
	~allowed_roles:_R_VM_POWER_ADMIN
	~params:[
		Ref _vm, "self", "The VM";
		Int, "target", "The target in bytes";
	] ()

let vm_wait_memory_target_live = call
	~name:"wait_memory_target_live"
	~in_product_since:rel_orlando
	~internal_deprecated_since:rel_midnight_ride
	~doc:"Wait for a running VM to reach its current memory target"
	~allowed_roles:_R_READ_ONLY
	~params:[
		Ref _vm, "self", "The VM";
	] ()

let vm_get_cooperative = call
	~name:"get_cooperative"
	~in_product_since:rel_midnight_ride
	~internal_deprecated_since:rel_tampa
	~doc:"Return true if the VM is currently 'co-operative' i.e. is expected to reach a balloon target and actually has done"
	~params:[
		Ref _vm, "self", "The VM";
	]
	~result:(Bool, "true if the VM is currently 'co-operative'; false otherwise")
	~allowed_roles:_R_READ_ONLY
	()

let vm_query_services = call
	~name:"query_services"
	~in_product_since:rel_tampa
	~doc:"Query the system services advertised by this VM and register them. This can only be applied to a system domain."
	~params:[
		Ref _vm, "self", "The VM";
	]
	~result:(Map(String, String), "map of service type to name")
	~allowed_roles:_R_POOL_ADMIN
	()

(* VM.StartOn *)

let vm_start_on = call
  ~in_product_since:rel_rio
  ~name:"start_on"
  ~doc:"Start the specified VM on a particular host.  This function can only be called with the VM is in the Halted State."
  ~in_oss_since:None
  ~params:[Ref _vm, "vm", "The VM to start";
	   Ref _host, "host", "The Host on which to start the VM";
           Bool, "start_paused", "Instantiate VM in paused state if set to true.";
	   Bool, "force", "Attempt to force the VM to start. If this flag is false then the VM may fail pre-boot safety checks (e.g. if the CPU the VM last booted on looks substantially different to the current one)";
	  ]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.vm_is_template; Api_errors.other_operation_in_progress;
         Api_errors.operation_not_allowed;
	 Api_errors.bootloader_failed;
	 Api_errors.unknown_bootloader;
]
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

(* VM.Pause *)

let vm_pause = call
  ~in_product_since:rel_rio
  ~name:"pause"
  ~doc:"Pause the specified VM. This can only be called when the specified VM is in the Running state."
  ~params:[Ref _vm, "vm", "The VM to pause"]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.other_operation_in_progress; Api_errors.operation_not_allowed;
         Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_OP
  ()  

(* VM.UnPause *)

let vm_unpause = call
  ~in_product_since:rel_rio
  ~name:"unpause"
  ~doc:"Resume the specified VM. This can only be called when the specified VM is in the Paused state."
  ~params:[Ref _vm, "vm", "The VM to unpause"]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.operation_not_allowed; Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_OP
  ()  


(* VM.CleanShutdown *)

let vm_cleanShutdown = call
  ~in_product_since:rel_rio
  ~name:"clean_shutdown"
  ~doc:"Attempt to cleanly shutdown the specified VM. (Note: this may not be supported---e.g. if a guest agent is not installed). This can only be called when the specified VM is in the Running state."
  ~params:[Ref _vm, "vm", "The VM to shutdown"]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.other_operation_in_progress; Api_errors.operation_not_allowed;
         Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_OP
  ()

(* VM.CleanReboot *)

let vm_cleanReboot = call
  ~in_product_since:rel_rio
  ~name:"clean_reboot"
  ~doc:"Attempt to cleanly shutdown the specified VM (Note: this may not be supported---e.g. if a guest agent is not installed). This can only be called when the specified VM is in the Running state."
  ~params:[Ref _vm, "vm", "The VM to shutdown"]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.other_operation_in_progress; Api_errors.operation_not_allowed;
         Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_OP
  ()

(* VM.HardShutdown *)

let vm_hardShutdown = call
  ~in_product_since:rel_rio
  ~name:"hard_shutdown"
  ~doc:"Stop executing the specified VM without attempting a clean shutdown."
  ~params:[Ref _vm, "vm", "The VM to destroy"]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.other_operation_in_progress; Api_errors.operation_not_allowed;
         Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_OP
  ()

(* VM.Shutdown *)

let vm_shutdown = call
  ~in_product_since:rel_clearwater
  ~name:"shutdown"
  ~doc:"Attempts to first clean shutdown a VM and if it should fail then perform a hard shutdown on it."
  ~params:[Ref _vm, "vm", "The VM to shutdown"]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.other_operation_in_progress; Api_errors.operation_not_allowed;
         Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_OP
  ()

(* VM.PowerStateReset *)

let vm_stateReset = call
  ~in_product_since:rel_rio
  ~name:"power_state_reset"
  ~doc:"Reset the power-state of the VM to halted in the database only. (Used to recover from slave failures in pooling scenarios by resetting the power-states of VMs running on dead slaves to halted.) This is a potentially dangerous operation; use with care."
  ~params:[Ref _vm, "vm", "The VM to reset"]
  ~errs:[]
  ~allowed_roles:_R_POOL_OP
  ()

(* VM.HardReboot *)

let vm_hardReboot = call
  ~in_product_since:rel_rio
  ~name:"hard_reboot"
  ~doc:"Stop executing the specified VM without attempting a clean shutdown and immediately restart the VM."
  ~params:[Ref _vm, "vm", "The VM to reboot"]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.other_operation_in_progress; Api_errors.operation_not_allowed;
         Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_OP
  ()

let vm_hardReboot_internal = call
  ~in_product_since:rel_orlando
  ~name:"hard_reboot_internal"
  ~doc:"Internal function which immediately restarts the specified VM."
  ~params:[Ref _vm, "vm", "The VM to reboot"]
  ~pool_internal:true
  ~hide_from_docs:true
  ~internal_deprecated_since:rel_midnight_ride
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()
  
(* VM.Hibernate *)
  
let vm_suspend = call
  ~in_product_since:rel_rio
  ~name:"suspend"
  ~doc:"Suspend the specified VM to disk.  This can only be called when the specified VM is in the Running state."
  ~params:[Ref _vm, "vm", "The VM to suspend"]
      (*	    Bool, "live", "If set to true, perform a live hibernate; otherwise suspend the VM before commencing hibernate" *)
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.other_operation_in_progress; Api_errors.operation_not_allowed;
	 Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_OP
  ()

(* VM.clsp -- clone suspended, undocumented API for VMLogix *)
let csvm = call
  ~name:"csvm"
  ~in_product_since:rel_rio
  ~doc:"undocumented. internal use only. This call is deprecated."
  ~params:[Ref _vm, "vm", ""]
  ~result:(Ref _vm, "")
  ~errs:[]
  ~hide_from_docs:true
  ~internal_deprecated_since:rel_miami
  ~allowed_roles:_R_VM_ADMIN
  ()
      
(* VM.UnHibernate *)
      
let vm_resume = call
  ~name:"resume"
  ~in_product_since:rel_rio
  ~doc:"Awaken the specified VM and resume it.  This can only be called when the specified VM is in the Suspended state."
  ~params:[Ref _vm, "vm", "The VM to resume";
           Bool, "start_paused", "Resume VM in paused state if set to true.";
           Bool, "force", "Attempt to force the VM to resume. If this flag is false then the VM may fail pre-resume safety checks (e.g. if the CPU the VM was running on looks substantially different to the current one)";
          ]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.operation_not_allowed; Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_OP
  ()

let vm_resume_on = call
  ~name:"resume_on"
  ~in_product_since:rel_rio
  ~doc:"Awaken the specified VM and resume it on a particular Host.  This can only be called when the specified VM is in the Suspended state."
  ~in_oss_since:None
  ~params:[Ref _vm, "vm", "The VM to resume";
	   Ref _host, "host", "The Host on which to resume the VM";
           Bool, "start_paused", "Resume VM in paused state if set to true.";
	   Bool, "force", "Attempt to force the VM to resume. If this flag is false then the VM may fail pre-resume safety checks (e.g. if the CPU the VM was running on looks substantially different to the current one)";
]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.operation_not_allowed; Api_errors.vm_is_template]
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let vm_pool_migrate = call
  ~in_oss_since:None 
  ~in_product_since:rel_rio
  ~name:"pool_migrate"
  ~doc:"Migrate a VM to another Host. This can only be called when the specified VM is in the Running state."
  ~params:[Ref _vm, "vm", "The VM to migrate";
	   Ref _host, "host", "The target host";
	   Map(String, String), "options", "Extra configuration operations" ]
  ~errs:[Api_errors.vm_bad_power_state; Api_errors.other_operation_in_progress; Api_errors.vm_is_template; Api_errors.operation_not_allowed; Api_errors.vm_migrate_failed; Api_errors.vm_missing_pv_drivers]
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let vm_pool_migrate_complete = call
  ~in_oss_since:None 
  ~in_product_since:rel_tampa
  ~name:"pool_migrate_complete"
  ~doc:"Tell a destination host that migration is complete."
  ~params:[Ref _vm, "vm", "The VM which has finished migrating";
	   Ref _host, "host", "The target host" ]
  ~hide_from_docs:true
  ~pool_internal:false (* needed for cross-pool migrate too *)
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let host_migrate_receive = call
  ~in_oss_since:None
  ~in_product_since:rel_tampa
  ~name:"migrate_receive"
  ~doc:"Prepare to receive a VM, returning a token which can be passed to VM.migrate."
  ~params:[Ref _host, "host", "The target host";
    Ref _network, "network", "The network through which migration traffic should be received.";
    Map(String, String), "options", "Extra configuration operations" ]
  ~result:(Map(String,String), "A value which should be passed to VM.migrate")
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let set_vcpus_number_live = call
	~name:"set_VCPUs_number_live"
	~in_product_since:rel_rio
	~doc:"Set the number of VCPUs for a running VM"
	~params:[Ref _vm, "self", "The VM";
		Int, "nvcpu", "The number of VCPUs"]
	~allowed_roles:_R_VM_ADMIN
	()

let vm_set_VCPUs_max = call ~flags:[`Session]
	~name:"set_VCPUs_max"
	~in_product_since:rel_midnight_ride
	~doc:"Set the maximum number of VCPUs for a halted VM"
	~params:[Ref _vm, "self", "The VM";
		Int, "value", "The new maximum number of VCPUs"]
	~allowed_roles:_R_VM_ADMIN
	()

let vm_set_VCPUs_at_startup = call ~flags:[`Session]
	~name:"set_VCPUs_at_startup"
	~in_product_since:rel_midnight_ride
	~doc:"Set the number of startup VCPUs for a halted VM"
	~params:[Ref _vm, "self", "The VM";
		Int, "value", "The new maximum number of VCPUs"]
	~allowed_roles:_R_VM_ADMIN
	()

let vm_set_HVM_shadow_multiplier = call ~flags:[`Session]
	~name:"set_HVM_shadow_multiplier"
	~in_product_since:rel_midnight_ride
	~doc:"Set the shadow memory multiplier on a halted VM"
	~params:[Ref _vm, "self", "The VM";
		Float, "value", "The new shadow memory multiplier to set"]
	~allowed_roles:_R_VM_POWER_ADMIN
	()

let vm_set_shadow_multiplier_live = call
	~name:"set_shadow_multiplier_live"
	~in_product_since:rel_rio
	~doc:"Set the shadow memory multiplier on a running VM"
	~params:[Ref _vm, "self", "The VM";
		Float, "multiplier", "The new shadow memory multiplier to set"]
	~allowed_roles:_R_VM_POWER_ADMIN
	()

let vm_add_to_VCPUs_params_live = call
  ~name:"add_to_VCPUs_params_live"
  ~in_product_since:rel_rio
  ~doc:"Add the given key-value pair to VM.VCPUs_params, and apply that value on the running VM"
  ~params:[Ref _vm, "self", "The VM";
           String, "key", "The key";
           String, "value", "The value"]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vm_send_sysrq = call
  ~name:"send_sysrq"
  ~in_product_since:rel_rio
  ~doc:"Send the given key as a sysrq to this VM.  The key is specified as a single character (a String of length 1).  This can only be called when the specified VM is in the Running state."
  ~params:[Ref _vm, "vm", "The VM";
           String, "key", "The key to send"]
  ~errs:[Api_errors.vm_bad_power_state]
  ~allowed_roles:_R_POOL_ADMIN
  ()

let vm_send_trigger = call
  ~name:"send_trigger"
  ~in_product_since:rel_rio
  ~doc:"Send the named trigger to this VM.  This can only be called when the specified VM is in the Running state."
  ~params:[Ref _vm, "vm", "The VM";
           String, "trigger", "The trigger to send"]
  ~errs:[Api_errors.vm_bad_power_state]
  ~allowed_roles:_R_POOL_ADMIN
  ()

let vm_migrate_send = call
  ~name: "migrate_send"
  ~in_product_since:rel_tampa
  ~doc: "Migrate the VM to another host.  This can only be called when the specified VM is in the Running state."
  ~params:[Ref _vm, "vm", "The VM";
           Map(String,String), "dest", "The result of a Host.migrate_receive call.";
           Bool, "live", "Live migration";
		   Map (Ref _vdi, Ref _sr), "vdi_map", "Map of source VDI to destination SR";
		   Map (Ref _vif, Ref _network), "vif_map", "Map of source VIF to destination network";
           Map (String, String), "options", "Other parameters"]
  ~errs:[Api_errors.vm_bad_power_state]
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let vm_assert_can_migrate = call
~name:"assert_can_migrate"
	~in_product_since:rel_tampa
	~doc:"Assert whether a VM can be migrated to the specified destination."
	~params:[
		Ref _vm, "vm", "The VM";
		Map(String,String), "dest", "The result of a VM.migrate_receive call.";
		Bool, "live", "Live migration";
		Map (Ref _vdi, Ref _sr), "vdi_map", "Map of source VDI to destination SR";
		Map (Ref _vif, Ref _network), "vif_map", "Map of source VIF to destination network";
		Map (String, String), "options", "Other parameters" ]
	~allowed_roles:_R_VM_POWER_ADMIN
	()

let vm_s3_suspend = call
  ~name: "s3_suspend"
  ~in_product_since:rel_midnight_ride
  ~doc:"Try to put the VM into ACPI S3 state"
  ~params:[Ref _vm, "vm", "The VM"]
  ~hide_from_docs:true
  ~allowed_roles:_R_VM_OP
  ()

let vm_s3_resume = call 
  ~name: "s3_resume"
  ~in_product_since:rel_midnight_ride
  ~doc:"Try to resume the VM from ACPI S3 state"
  ~params:[Ref _vm, "vm", "The VM"]
  ~hide_from_docs:true
  ~allowed_roles:_R_VM_OP
  ()
	   

let vm_create_new_blob = call
  ~name: "create_new_blob"
  ~in_product_since:rel_orlando
  ~doc:"Create a placeholder for a named binary blob of data that is associated with this VM"
  ~versioned_params:
  [{param_type=Ref _vm; param_name="vm"; param_doc="The VM"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
  {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}
  ]
  ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

let vm_copy_bios_strings = call
  ~name: "copy_bios_strings"
  ~in_product_since:rel_midnight_ride
  ~doc:"Copy the BIOS strings from the given host to this VM"
  ~params:[Ref _vm, "vm", "The VM to modify";
	   Ref _host, "host", "The host to copy the BIOS strings from";]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vm_set_protection_policy = call
  ~name:"set_protection_policy"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:"Set the value of the protection_policy field"
  ~params:[Ref _vm, "self", "The VM";
     Ref _vmpp, "value", "The value"]
  ~flags:[`Session]
  ~allowed_roles:_R_POOL_OP
  ()

let vm_set_start_delay = call
  ~name:"set_start_delay"
  ~in_product_since:rel_boston
  ~doc:"Set this VM's start delay in seconds"
  ~params:[Ref _vm, "self", "The VM";
    Int, "value", "This VM's start delay in seconds"]
  ~allowed_roles:_R_POOL_OP
  ()

let vm_set_shutdown_delay = call
  ~name:"set_shutdown_delay"
  ~in_product_since:rel_boston
  ~doc:"Set this VM's shutdown delay in seconds"
  ~params:[Ref _vm, "self", "The VM";
    Int, "value", "This VM's shutdown delay in seconds"]
  ~allowed_roles:_R_POOL_OP
  ()

let vm_set_order = call
  ~name:"set_order"
  ~in_product_since:rel_boston
  ~doc:"Set this VM's boot order"
  ~params:[Ref _vm, "self", "The VM";
    Int, "value", "This VM's boot order"]
  ~allowed_roles:_R_POOL_OP
  ()

let vm_set_suspend_VDI = call
	~name:"set_suspend_VDI"
	~in_product_since:rel_boston
	~doc:"Set this VM's suspend VDI, which must be indentical to its current one"
	~params:[Ref _vm, "self", "The VM";
	         Ref _vdi, "value", "The suspend VDI uuid"]
	~allowed_roles:_R_POOL_OP
	()
	
let vm_assert_can_be_recovered = call
  ~name:"assert_can_be_recovered"
  ~in_product_since:rel_boston
  ~doc:"Assert whether all SRs required to recover this VM are available."
  ~params:[Ref _vm, "self", "The VM to recover";
    Ref _session, "session_to", "The session to which the VM is to be recovered."]
  ~errs:[Api_errors.vm_is_part_of_an_appliance; Api_errors.vm_requires_sr]
  ~allowed_roles:_R_READ_ONLY
  ()

let vm_get_SRs_required_for_recovery = call
	~name:"get_SRs_required_for_recovery"
	~in_product_since:rel_clearwater
	~doc:"List all the SR's that are required for the VM to be recovered"
	~params:[Ref _vm , "self" , "The VM for which the SRs have to be recovered";
					Ref _session , "session_to" , "The session to which the SRs of the VM have to be recovered."]
	~result:(Set(Ref _sr),"refs for SRs required to recover the VM")
	~errs:[]
	~allowed_roles:_R_READ_ONLY
	()

let vm_recover = call
  ~name:"recover"
  ~in_product_since:rel_boston
  ~doc:"Recover the VM"
  ~params:[Ref _vm, "self", "The VM to recover";
    Ref _session, "session_to", "The session to which the VM is to be recovered.";
    Bool, "force", "Whether the VM should replace newer versions of itself."]
  ~allowed_roles:_R_READ_ONLY
  ()

let vm_set_appliance = call
	~name:"set_appliance"
	~in_product_since:rel_boston
	~doc:"Assign this VM to an appliance."
	~params:[Ref _vm, "self", "The VM to assign to an appliance.";
		Ref _vm_appliance, "value", "The appliance to which this VM should be assigned."]
	~allowed_roles:_R_POOL_OP
	()

(* ------------------------------------------------------------------------------------------------------------
   Host Management
   ------------------------------------------------------------------------------------------------------------ *)

let host_ha_disable_failover_decisions = call
  ~in_product_since:rel_orlando
  ~name:"ha_disable_failover_decisions"
  ~doc:"Prevents future failover decisions happening on this node. This function should only be used as part of a controlled shutdown of the HA system."
  ~params:[Ref _host, "host", "The Host to disable failover decisions for"]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_ha_disarm_fencing = call
  ~in_product_since:rel_orlando
  ~name:"ha_disarm_fencing"
  ~doc:"Disarms the fencing function of the HA subsystem. This function is extremely dangerous and should only be used as part of a controlled shutdown of the HA system."
  ~params:[Ref _host, "host", "The Host to disarm"]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_ha_stop_daemon = call
  ~in_product_since:rel_orlando
  ~name:"ha_stop_daemon"
  ~doc:"Stops the HA daemon. This function is extremely dangerous and should only be used as part of a controlled shutdown of the HA system."
  ~params:[Ref _host, "host", "The Host whose daemon should be stopped"]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_ha_release_resources = call
  ~in_product_since:rel_orlando
  ~name:"ha_release_resources"
  ~doc:"Cleans up any resources on the host associated with this HA instance."
  ~params:[Ref _host, "host", "The Host whose resources should be cleaned up"]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_local_assert_healthy = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"local_assert_healthy"
  ~doc:"Returns nothing if this host is healthy, otherwise it throws an error explaining why the host is unhealthy"
  ~params:[]
  ~pool_internal:true
  ~hide_from_docs:true
  ~errs:[ Api_errors.host_still_booting;
	  Api_errors.host_has_no_management_ip;
	  Api_errors.host_master_cannot_talk_back;
	  Api_errors.host_unknown_to_master;
	  Api_errors.host_broken;
	  Api_errors.license_restriction;
	  Api_errors.license_does_not_support_pooling;
	  Api_errors.ha_should_be_fenced;
	]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_preconfigure_ha = call
  ~in_product_since:rel_miami
  ~name:"preconfigure_ha"  
  ~doc:"Attach statefiles, generate config files but do not start the xHA daemon."
  ~params:[Ref _host, "host", "The Host to modify";
	   Set(Ref _vdi), "statefiles", "Set of statefile VDIs to use";
	   Ref _vdi, "metadata_vdi", "VDI to use for Pool metadata";
	   String, "generation", "UUID identifying this HA instance";
	  ]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()  

let host_ha_join_liveset = call
  ~in_product_since:rel_orlando
  ~name:"ha_join_liveset"
  ~doc:"Block until this host joins the liveset."
  ~params:[Ref _host, "host", "The Host whose HA datmon to start"]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_ha_wait_for_shutdown_via_statefile = call
  ~in_product_since:rel_orlando
  ~name:"ha_wait_for_shutdown_via_statefile"
  ~doc:"Block until this host xHA daemon exits after having seen the invalid statefile. If the host loses statefile access then throw an exception"
  ~params:[Ref _host, "host", "The Host whose HA subsystem to query"]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()
(*
let host_query_ha = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"query_ha"
  ~doc:"Return the local HA configuration as seen by this host"
  ~params:[]
  ~custom_marshaller:true  
  ~pool_internal:true
  ~hide_from_docs:true
  ()
*)
let host_request_backup = call ~flags:[`Session]
  ~name:"request_backup"
  ~in_product_since:rel_rio
  ~doc:"Request this host performs a database backup"
  ~params:[Ref _host, "host", "The Host to send the request to";
	   Int, "generation", "The generation count of the master's database";
	   Bool, "force", "If this is true then the client _has_ to take a backup, otherwise it's just an 'offer'"
	  ]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_request_config_file_sync = call ~flags:[`Session]
  ~name:"request_config_file_sync"
  ~in_product_since:rel_rio
  ~doc:"Request this host syncs dom0 config files"
  ~params:[Ref _host, "host", "The Host to send the request to";
	   String, "hash", "The hash of the master's dom0 config files package"
	  ]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()


(* Since there are no async versions, no tasks are generated (!) this is important
   otherwise the call would block doing a Db.Task.create *)
let host_propose_new_master = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"propose_new_master"
  ~doc:"First phase of a two-phase commit protocol to set the new master. If the host has already committed to another configuration or if the proposed new master is not in this node's membership set then the call will return an exception."
  ~params:[String, "address", "The address of the Host which is proposed as the new master";
	   Bool, "manual", "True if this call is being invoked by the user manually, false if automatic";
	  ]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_abort_new_master = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"abort_new_master"
  ~doc:"Causes the new master transaction to abort"
  ~params:[String, "address", "The address of the Host which is proposed as the new master"]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()  

let host_commit_new_master = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"commit_new_master"
  ~doc:"Second phase of a two-phase commit protocol to set the new master."
  ~params:[String, "address", "The address of the Host which should be committed as the new master"]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_compute_free_memory = call
	~in_product_since:rel_orlando
	~name:"compute_free_memory"
	~doc:"Computes the amount of free memory on the host."
	~params:[Ref _host, "host", "The host to send the request to"]
	~pool_internal:false
	~hide_from_docs:false
	~result:(Int, "the amount of free memory on the host.")
  ~allowed_roles:_R_READ_ONLY
	()

let host_compute_memory_overhead = call
	~in_product_since:rel_midnight_ride
	~name:"compute_memory_overhead"
	~doc:"Computes the virtualization memory overhead of a host."
	~params:[Ref _host, "host", "The host for which to compute the memory overhead"]
	~pool_internal:false
	~hide_from_docs:false
	~result:(Int, "the virtualization memory overhead of the host.")
	~allowed_roles:_R_READ_ONLY
	()

(* Diagnostics see if host is in emergency mode *)
let host_is_in_emergency_mode = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"is_in_emergency_mode"
  ~doc:"Diagnostics call to discover if host is in emergency mode"
  ~params:[]
  ~pool_internal:false
  ~hide_from_docs:true
  ~result:(Bool, "true if host is in emergency mode")
  ~allowed_roles:_R_READ_ONLY
  ()

(* Signal that the management IP address or hostname has been changed beneath us. *)
let host_signal_networking_change = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"signal_networking_change"
  ~doc:"Signals that the management IP address or hostname has been changed beneath us."
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_notify = call
  ~in_product_since:rel_miami
  ~name:"notify"
  ~doc:"Notify an event"
  ~params:[String, "ty", "type of the notification";
           String, "params", "arguments of the notification (can be empty)"; ]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_syslog_reconfigure = call
  ~in_product_since:rel_miami
  ~name:"syslog_reconfigure"
  ~doc:"Re-configure syslog logging"
  ~params:[Ref _host, "host", "Tell the host to reread its Host.logging parameters and reconfigure itself accordingly"]
  ~allowed_roles:_R_POOL_OP
  ()

let host_management_reconfigure = call
  ~in_product_since:rel_miami
  ~name:"management_reconfigure"
  ~doc:"Reconfigure the management network interface"
  ~params:[
    Ref _pif, "pif", "reference to a PIF object corresponding to the management interface";
	  ]
  ~allowed_roles:_R_POOL_OP
  ()

let host_local_management_reconfigure = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"local_management_reconfigure"
  ~doc:"Reconfigure the management network interface. Should only be used if Host.management_reconfigure is impossible because the network configuration is broken."
  ~params:[
    String, "interface", "name of the interface to use as a management interface";
	  ]
  ~allowed_roles:_R_POOL_OP
  ()

let host_ha_xapi_healthcheck = call ~flags:[`Session]
  ~in_product_since:rel_orlando
  ~name:"ha_xapi_healthcheck"
  ~doc:"Returns true if xapi appears to be functioning normally."
  ~result:(Bool, "true if xapi is functioning normally.")
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_ADMIN
  ()

let host_management_disable = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"management_disable"
  ~doc:"Disable the management network interface"
  ~params:[]
  ~allowed_roles:_R_POOL_OP
  ()

let host_get_management_interface = call
  ~lifecycle:[]
  ~name:"get_management_interface"
  ~doc:"Returns the management interface for the specified host"
  ~params:[Ref _host, "host", "Which host's management interface is required"]
  ~result:(Ref _pif, "The managment interface for the host")
  ~allowed_roles:_R_POOL_OP
  ()

(* Simple host evacuate message for Miami.
   Not intended for HA *)

let host_assert_can_evacuate = call
  ~in_product_since:rel_miami
  ~name:"assert_can_evacuate"
  ~doc:"Check this host can be evacuated."
  ~params:[Ref _host, "host", "The host to evacuate"]
  ~allowed_roles:_R_POOL_OP
  ()

(* New Orlando message which aims to make the GUI less brittle (unexpected errors will trigger a VM suspend)
   and sensitive to HA planning constraints *)
let host_get_vms_which_prevent_evacuation = call
  ~in_product_since:rel_orlando
  ~name:"get_vms_which_prevent_evacuation"
  ~doc:"Return a set of VMs which prevent the host being evacuated, with per-VM error codes"
  ~params:[Ref _host, "self", "The host to query"]
  ~result:(Map(Ref _vm, Set(String)), "VMs which block evacuation together with reasons")
  ~allowed_roles:_R_READ_ONLY
  ()

let host_evacuate = call
  ~in_product_since:rel_miami
  ~name:"evacuate"
  ~doc:"Migrate all VMs off of this host, where possible."
  ~params:[Ref _host, "host", "The host to evacuate"]
  ~allowed_roles:_R_POOL_OP
  ()

let host_get_uncooperative_resident_VMs = call
	~in_product_since:rel_midnight_ride
	~internal_deprecated_since:rel_tampa
	~name:"get_uncooperative_resident_VMs"
	~doc:"Return a set of VMs which are not co-operating with the host's memory control system"
	~params:[Ref _host, "self", "The host to query"]
	~result:((Set(Ref _vm)), "VMs which are not co-operating")
	~allowed_roles:_R_READ_ONLY
	()

let host_get_uncooperative_domains = call
	~in_product_since:rel_midnight_ride
	~internal_deprecated_since:rel_tampa
	~name:"get_uncooperative_domains"
	~doc:"Return the set of domain uuids which are not co-operating with the host's memory control system"
	~params:[Ref _host, "self", "The host to query"]
	~result:((Set(String)), "UUIDs of domains which are not co-operating")
	~pool_internal:true
	~hide_from_docs:true
	~allowed_roles:_R_LOCAL_ROOT_ONLY
	()

let host_retrieve_wlb_evacuate_recommendations = call
  ~name:"retrieve_wlb_evacuate_recommendations"
  ~lifecycle:wlb_removed
  ~doc:"Retrieves recommended host migrations to perform when evacuating the host from the wlb server. If a VM cannot be migrated from the host the reason is listed instead of a recommendation."
  ~params:[Ref _host, "self", "The host to query"]
  ~result:(Map(Ref _vm, Set(String)), "VMs and the reasons why they would block evacuation, or their target host recommended by the wlb server")
  ~allowed_roles:_R_READ_ONLY
  ()

(* Host.Disable *)

let host_disable = call
  ~in_product_since:rel_rio
  ~name:"disable"
  ~doc:"Puts the host into a state in which no new VMs can be started. Currently active VMs on the host continue to execute."
  ~params:[Ref _host, "host", "The Host to disable"]
  ~allowed_roles:_R_POOL_OP
  ()

(* Host.Enable *)

let host_enable = call
  ~name:"enable"
  ~in_product_since:rel_rio
  ~doc:"Puts the host into a state in which new VMs can be started."
  ~params:[Ref _host, "host", "The Host to enable"]
  ~allowed_roles:_R_POOL_OP
  ()

(* Host.Shutdown *)

let host_shutdown = call
  ~name:"shutdown"
  ~in_product_since:rel_rio
  ~doc:"Shutdown the host. (This function can only be called if there are no currently running VMs on the host and it is disabled.)"
  ~params:[Ref _host, "host", "The Host to shutdown"]
  ~allowed_roles:_R_POOL_OP
  ()

(* Host.reboot *)

let host_reboot = call
  ~name:"reboot"
  ~in_product_since:rel_rio
  ~doc:"Reboot the host. (This function can only be called if there are no currently running VMs on the host and it is disabled.)"
  ~params:[Ref _host, "host", "The Host to reboot"]
  ~allowed_roles:_R_POOL_OP
  ()

(* Host.power_on *)

let host_power_on = call
  ~name:"power_on"
  ~in_product_since:rel_orlando
  ~doc:"Attempt to power-on the host (if the capability exists)."
  ~params:[Ref _host, "host", "The Host to power on"]
  ~allowed_roles:_R_POOL_OP
  ()

let host_restart_agent = call
  ~name:"restart_agent"
  ~in_product_since:rel_rio
  ~doc:"Restarts the agent after a 10 second pause. WARNING: this is a dangerous operation. Any operations in progress will be aborted, and unrecoverable data loss may occur. The caller is responsible for ensuring that there are no operations in progress when this method is called."
  ~params:[Ref _host, "host", "The Host on which you want to restart the agent"]
  ~allowed_roles:_R_POOL_OP
  ()

let host_shutdown_agent = call
  ~name:"shutdown_agent"
  ~in_product_since:rel_orlando
  ~doc:"Shuts the agent down after a 10 second pause. WARNING: this is a dangerous operation. Any operations in progress will be aborted, and unrecoverable data loss may occur. The caller is responsible for ensuring that there are no operations in progress when this method is called."
  ~params:[]
  ~flags:[`Session] (* no async *)
  ~allowed_roles:_R_POOL_OP
  ()

let host_dmesg = call
  ~name:"dmesg"
  ~in_product_since:rel_rio
  ~doc:"Get the host xen dmesg."
  ~params:[Ref _host, "host", "The Host to query"]
  ~result:(String, "dmesg string")
  ~allowed_roles:_R_POOL_OP
  ()

let host_dmesg_clear = call
  ~name:"dmesg_clear"
  ~in_product_since:rel_rio
  ~doc:"Get the host xen dmesg, and clear the buffer."
  ~params:[Ref _host, "host", "The Host to query"]
  ~result:(String, "dmesg string")
  ~allowed_roles:_R_POOL_OP
  ()

let host_get_log = call
  ~name:"get_log"
  ~in_product_since:rel_rio
  ~doc:"Get the host's log file"
  ~params:[Ref _host, "host", "The Host to query"]
  ~result:(String, "The contents of the host's primary log file")
  ~allowed_roles:_R_READ_ONLY
  ()

let host_send_debug_keys = call
  ~name:"send_debug_keys"
  ~in_product_since:rel_rio
  ~doc:"Inject the given string as debugging keys into Xen"
  ~params:[Ref _host, "host", "The host";
           String, "keys", "The keys to send"]
  ~allowed_roles:_R_POOL_ADMIN
  ()

let host_get_data_sources = call
  ~name:"get_data_sources"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:""
  ~result:(Set (Record _data_source), "A set of data sources")
  ~params:[Ref _host, "host", "The host to interrogate"]
  ~errs:[]
  ~flags:[`Session] 
  ~allowed_roles:_R_READ_ONLY
  ()

let host_record_data_source = call
  ~name:"record_data_source"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:"Start recording the specified data source"
  ~params:[Ref _host, "host", "The host";
	   String, "data_source", "The data source to record"]
  ~errs:[]
  ~flags:[`Session]
  ~allowed_roles:_R_POOL_OP
  ()

let host_query_data_source = call
  ~name:"query_data_source"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:"Query the latest value of the specified data source"
  ~params:[Ref _host, "host", "The host";
	   String, "data_source", "The data source to query"]
  ~result:(Float,"The latest value, averaged over the last 5 seconds")
  ~errs:[]
  ~flags:[`Session]
  ~allowed_roles:_R_READ_ONLY
  ()

let host_attach_static_vdis = call
  ~name:"attach_static_vdis"
	~in_product_since:rel_midnight_ride
  ~doc:"Statically attach VDIs on a host."
  ~params:[Ref _host, "host", "The Host to modify";
    Map(Ref _vdi, String), "vdi_reason_map", "List of VDI+reason pairs to attach"
	  ]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  () 

let host_detach_static_vdis = call
  ~name:"detach_static_vdis"
	~in_product_since:rel_midnight_ride
  ~doc:"Detach static VDIs from a host."
  ~params:[Ref _host, "host", "The Host to modify";
	   Set(Ref _vdi), "vdis", "Set of VDIs to detach";
	  ]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_declare_dead = call
  ~name:"declare_dead"
	~in_product_since:rel_clearwater
  ~doc:"Declare that a host is dead. This is a dangerous operation, and should only be called if the administrator is absolutely sure the host is definitely dead"
  ~params:[Ref _host, "host", "The Host to declare is dead"]
  ~allowed_roles:_R_POOL_OP
  ()

let host_forget_data_source_archives = call
  ~name:"forget_data_source_archives"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~doc:"Forget the recorded statistics related to the specified data source"
  ~params:[Ref _host, "host", "The host";
	   String, "data_source", "The data source whose archives are to be forgotten"]
  ~flags:[`Session]
  ~allowed_roles:_R_POOL_OP
  ()

let host_get_diagnostic_timing_stats = call ~flags:[`Session]
  ~in_product_since:rel_miami
  ~name:"get_diagnostic_timing_stats"
  ~doc:"Return timing statistics for diagnostic purposes"
  ~params:[Ref _host, "host", "The host to interrogate"]
  ~result:(Map(String, String), "population name to summary map")
  ~hide_from_docs:true
  ~allowed_roles:_R_READ_ONLY
  ()

let host_create_new_blob = call
  ~name: "create_new_blob"
  ~in_product_since:rel_orlando
  ~doc:"Create a placeholder for a named binary blob of data that is associated with this host"
  ~versioned_params:
  [{param_type=Ref _host; param_name="host"; param_doc="The host"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
  {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}]
  ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
  ~allowed_roles:_R_POOL_OP
  ()

let host_call_plugin = call
  ~name:"call_plugin"
  ~in_product_since:rel_orlando
  ~doc:"Call a XenAPI plugin on this host"
  ~params:[Ref _host, "host", "The host";
	   String, "plugin", "The name of the plugin";
	   String, "fn", "The name of the function within the plugin";
	   Map(String, String), "args", "Arguments for the function";]
  ~result:(String, "Result from the plugin")
  ~allowed_roles:_R_POOL_ADMIN
  ()

let host_enable_binary_storage = call
  ~name:"enable_binary_storage"
  ~in_product_since:rel_orlando
  ~hide_from_docs:true
  ~pool_internal:true
  ~doc:"Enable binary storage on a particular host, for storing RRDs, messages and blobs"
  ~params:[Ref _host, "host", "The host"]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_disable_binary_storage = call
  ~name:"disable_binary_storage"
  ~in_product_since:rel_orlando
  ~hide_from_docs:true
  ~pool_internal:true
  ~doc:"Disable binary storage on a particular host, deleting stored RRDs, messages and blobs"
  ~params:[Ref _host, "host", "The host"]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_update_pool_secret = call
	~name:"update_pool_secret"
	~in_product_since:rel_midnight_ride
	~hide_from_docs:true
	~pool_internal:true
	~doc:""
	~params:[
		Ref _host, "host", "The host";
		String, "pool_secret", "The new pool secret" ]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
	()

let host_update_master = call
	~name:"update_master"
	~in_product_since:rel_midnight_ride
	~hide_from_docs:true
	~pool_internal:true
	~doc:""
	~params:[
		Ref _host, "host", "The host";
		String, "master_address", "The new master address" ]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
	()

let host_set_localdb_key = call
  ~name:"set_localdb_key"
  ~in_product_since:rel_midnight_ride
  ~doc:"Set a key in the local DB of the host."
  ~params:[Ref _host, "host", "The Host to modify";
    String, "key", "Key to change";
    String, "value", "Value to set"
	  ]
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  () 

let host_refresh_pack_info = call
  ~name:"refresh_pack_info"
  ~in_product_since:rel_midnight_ride
  ~doc:"Refresh the list of installed Supplemental Packs."
  ~params:[Ref _host, "host", "The Host to modify"]
  ~allowed_roles:_R_POOL_OP
  () 
    
(* ------------------------------------------------------------------------------------------------------------
   VDI Management
   ------------------------------------------------------------------------------------------------------------ *)

(* VDI.Snapshot *)

let vdi_snapshot = call
  ~name:"snapshot"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~versioned_params:
  [{param_type=Ref _vdi; param_name="vdi"; param_doc="The VDI to snapshot"; param_release=rio_release; param_default=None};
   {param_type=Map (String, String); param_name="driver_params"; param_doc="Optional parameters that can be passed through to backend driver in order to specify storage-type-specific snapshot options"; param_release=miami_release; param_default=Some (VMap [])}
  ]
  ~doc:"Take a read-only snapshot of the VDI, returning a reference to the snapshot. If any driver_params are specified then these are passed through to the storage-specific substrate driver that takes the snapshot. NB the snapshot lives in the same Storage Repository as its parent."
  ~result:(Ref _vdi, "The ID of the newly created VDI.")
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_clone = call
  ~name:"clone"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[Ref _vdi, "vdi", "The VDI to clone"]
  ~versioned_params:
  [{param_type=Ref _vdi; param_name="vdi"; param_doc="The VDI to clone"; param_release=rio_release; param_default=None};
   {param_type=Map (String, String); param_name="driver_params"; param_doc="Optional parameters that are passed through to the backend driver in order to specify storage-type-specific clone options"; param_release=miami_release; param_default=Some (VMap [])}
  ]
  ~doc:"Take an exact copy of the VDI and return a reference to the new disk. If any driver_params are specified then these are passed through to the storage-specific substrate driver that implements the clone operation. NB the clone lives in the same Storage Repository as its parent."
  ~result:(Ref _vdi, "The ID of the newly created VDI.")
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_resize = call
  ~name:"resize"
  ~in_product_since:rel_rio
  ~in_oss_since:None
  ~params:[Ref _vdi, "vdi", "The VDI to resize"; Int, "size", "The new size of the VDI" ]
  ~doc:"Resize the VDI."
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_resize_online = call
  ~name:"resize_online"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[Ref _vdi, "vdi", "The VDI to resize"; Int, "size", "The new size of the VDI" ]
  ~doc:"Resize the VDI which may or may not be attached to running guests."
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_copy = call
  ~name:"copy"
  ~lifecycle:[
	Published, rel_rio, "Copies a VDI to an SR. There must be a host that can see both the source and destination SRs simultaneously";
	Extended, rel_cowley, "The copy can now be performed between any two SRs." ]
  ~in_oss_since:None
  ~params:[Ref _vdi, "vdi", "The VDI to copy"; Ref _sr, "sr", "The destination SR" ]
  ~doc:"Make a fresh VDI in the specified SR and copy the supplied VDI's data to the new disk"
  ~result:(Ref _vdi, "The reference of the newly created VDI.")
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_pool_migrate = call
  ~name:"pool_migrate"
  ~in_oss_since:None
  ~in_product_since:rel_tampa
  ~params:[ Ref _vdi, "vdi", "The VDI to migrate"
    ; Ref _sr, "sr", "The destination SR"
    ; Map (String, String), "options", "Other parameters" ]
  ~result:(Ref _vdi, "The new reference of the migrated VDI.")
  ~doc:"Migrate a VDI, which may be attached to a running guest, to a different SR. The destination SR must be visible to the guest."
  ~allowed_roles:_R_VM_POWER_ADMIN
  ()

(* ------------------------------------------------------------------------------------------------------------
   VBDs
   ------------------------------------------------------------------------------------------------------------ *)

let vbd_eject = call
  ~name:"eject"
  ~in_product_since:rel_rio
  ~doc:"Remove the media from the device and leave it empty"
  ~params:[Ref _vbd, "vbd", "The vbd representing the CDROM-like device"]
  ~errs:[Api_errors.vbd_not_removable_media; Api_errors.vbd_is_empty]
  ~allowed_roles:_R_VM_OP
  ()

let vbd_insert = call
  ~name:"insert"
  ~in_product_since:rel_rio
  ~doc:"Insert new media into the device"
  ~params:[Ref _vbd, "vbd", "The vbd representing the CDROM-like device";
	   Ref _vdi, "vdi", "The new VDI to 'insert'"]
  ~errs:[Api_errors.vbd_not_removable_media; Api_errors.vbd_not_empty]
  ~allowed_roles:_R_VM_OP
  ()

let vbd_plug = call
  ~name:"plug"
  ~in_product_since:rel_rio
  ~doc:"Hotplug the specified VBD, dynamically attaching it to the running VM"
  ~params:[Ref _vbd, "self", "The VBD to hotplug"]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vbd_unplug = call
  ~name:"unplug"
  ~in_product_since:rel_rio
  ~doc:"Hot-unplug the specified VBD, dynamically unattaching it from the running VM"
  ~params:[Ref _vbd, "self", "The VBD to hot-unplug"]
  ~errs:[Api_errors.device_detach_rejected; Api_errors.device_already_detached]
  ~allowed_roles:_R_VM_ADMIN
  ()
 
let vbd_unplug_force = call
  ~name:"unplug_force"
  ~in_product_since:rel_rio
  ~doc:"Forcibly unplug the specified VBD"
  ~params:[Ref _vbd, "self", "The VBD to forcibly unplug"]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vbd_unplug_force_no_safety_check = call
  ~name:"unplug_force_no_safety_check"
  ~doc:"Forcibly unplug the specified VBD without any safety checks. This is an extremely dangerous operation in the general case that can cause guest crashes and data corruption; it should be called with extreme caution."
  ~params:[Ref _vbd, "self", "The VBD to forcibly unplug (no safety checks are applied to test if the device supports surprise-remove)"]
  ~hide_from_docs:true
  ~in_product_since:rel_symc
  ~allowed_roles:_R_VM_ADMIN
  ()

let vbd_pause = call
  ~name:"pause"
  ~doc:"Stop the backend device servicing requests so that an operation can be performed on the disk (eg live resize, snapshot)"
  ~params:[Ref _vbd, "self", "The VBD to pause"]
  ~hide_from_docs:true
  ~in_product_since:rel_symc
  ~result:(String, "Token to uniquely identify this pause instance, used to match the corresponding unpause") (* new in MR *)
  ~allowed_roles:_R_VM_ADMIN
  ()

let vbd_unpause = call
  ~name:"unpause"
  ~doc:"Restart the backend device after it was paused while an operation was performed on the disk (eg live resize, snapshot)"
  ~versioned_params:
  [{param_type=Ref _vbd; param_name="self"; param_doc="The VBD to unpause"; param_release=miami_symc_release; param_default=None};
   {param_type=String; param_name="token"; param_doc="The token from VBD.pause"; param_release=orlando_release; param_default=Some(VString "")}]
  ~hide_from_docs:true
  ~in_product_since:rel_symc
  ~allowed_roles:_R_VM_ADMIN
  ()

let vbd_assert_attachable = call
  ~name:"assert_attachable"
  ~in_product_since:rel_rio
  ~doc:"Throws an error if this VBD could not be attached to this VM if the VM were running. Intended for debugging."
  ~params:[Ref _vbd, "self", "The VBD to query"]
  ~in_oss_since:None
  ~allowed_roles:_R_VM_ADMIN
  ()

(******************************************************************************************************************)
(* Now define the objects themselves and their fields *)


(** Make an object field record *)
let field ?(in_oss_since = Some "3.0.3") ?in_product_since ?(internal_only = false)
	?internal_deprecated_since ?(ignore_foreign_key = false) ?(writer_roles=None) ?(reader_roles=None)
	?(qualifier = RW) ?(ty = String) ?(effect = false) ?(default_value = None) ?(persist = true)
	?(map_keys_roles=[]) (* list of (key_name,(writer_roles)) for a map field *)
	?lifecycle name desc =
	(* in_product_since currently defaults to 'Some rel_rio', for backwards compatibility.
	 * This should eventually become 'None'. *)
	let in_product_since = match in_product_since with None -> Some rel_rio | x -> x in
	if lifecycle = None && in_product_since = None then
		failwith ("Lifecycle for field '" ^ name ^ "' not specified");
	let lifecycle = match lifecycle with
		| None ->
			let published = match in_product_since with
				| None -> []
				| Some rel -> [Published, rel, desc]
			in
			let deprecated = match internal_deprecated_since with
				| None -> []
				| Some rel -> [Deprecated, rel, ""]
			in
			published @ deprecated
		| Some l -> l
	in
	let release =
		{
			internal = (match get_published lifecycle with 
				| Some published -> get_product_releases published
				| None -> ["closed"]);
			opensource = get_oss_releases in_oss_since;
			internal_deprecated_since = get_deprecated lifecycle;
		}
	in
	Field {
		release = release;
		lifecycle=lifecycle;
		qualifier=qualifier; ty=ty; internal_only = internal_only; default_value = default_value;
		field_name=name; 
		full_name=[ name ];
		field_description=desc;
		field_persist=persist;
		field_has_effect = effect;
		field_ignore_foreign_key = ignore_foreign_key;
		field_setter_roles = writer_roles;
		field_getter_roles = reader_roles;
		field_map_keys_roles = map_keys_roles;
	}

let uid ?(in_oss_since=Some "3.0.3") ?(reader_roles=None) ?lifecycle refname =
	field
		~in_oss_since
		?lifecycle
		~qualifier:DynamicRO
		~ty:(String)
		~writer_roles:_R_POOL_ADMIN (* only the system should be able to create/modify uuids *)
		~reader_roles
		"uuid"
		"Unique identifier/object reference"

let allowed_and_current_operations ?(writer_roles=None) ?(reader_roles=None) operations_type =
  [ 
    field ~writer_roles ~reader_roles ~persist:false ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set operations_type) "allowed_operations" "list of the operations allowed in this state. This list is advisory only and the server state may have changed by the time this field is read by a client.";
    field ~writer_roles ~reader_roles ~persist:false ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map(String, operations_type)) "current_operations" "links each of the running tasks using this object (by reference) to a current_operation enum which describes the nature of the task.";
  ]


(** Make a Namespace (note effect on enclosing field.full_names) *)
let namespace ?(get_field_writer_roles=fun x->x) ?(get_field_reader_roles=fun x->x) ?(idempotent=false) ~name ~contents () = 
  let rec prefix = function
    | Namespace(x, xs) -> Namespace(x, List.map prefix xs)
    | Field x -> Field { x with full_name = if idempotent then x.full_name else name :: x.full_name;
        field_setter_roles=get_field_writer_roles x.field_setter_roles;
        field_getter_roles=get_field_reader_roles x.field_getter_roles
       } in
  Namespace(name, List.map prefix contents)

(** Many of the objects have a set of names of various lengths: *)
let names ?(writer_roles=None) ?(reader_roles=None) ?lifecycle in_oss_since qual =
	let field x y =
		field x y ~in_oss_since ~qualifier:qual ~writer_roles ~reader_roles
			~default_value:(Some (VString "")) ?lifecycle in
	[
		field "label" "a human-readable name";
		field "description" "a notes field containing human-readable description"
	]

let default_field_reader_roles = _R_ALL (* by default, all can read fields *)
let default_field_writer_roles = _R_POOL_ADMIN (* by default, only root can write to them *)

(** Create an object and map the object name into the messages *)
let create_obj ?lifecycle ~in_oss_since ?in_product_since ?(internal_deprecated_since=None) ~gen_constructor_destructor ~gen_events ~persist ~name ~descr ~doccomments ~contents ~messages ~in_db
	?(contents_default_reader_roles=default_field_reader_roles) ?(contents_default_writer_roles=None)
	?(implicit_messages_allowed_roles=_R_ALL) (* used in implicit obj msgs (get_all, etc) *)
	?force_custom_actions:(force_custom_actions=None) (* None,Some(RW),Some(StaticRO) *)
	~messages_default_allowed_roles (* used in constructor, destructor and explicit obj msgs *)
	() =
	let contents_default_writer_roles = if contents_default_writer_roles=None then messages_default_allowed_roles else contents_default_writer_roles in
	let get_field_reader_roles = function None->contents_default_reader_roles|r->r in
	let get_field_writer_roles = function None->contents_default_writer_roles|r->r in
	let get_msg_allowed_roles = function None->messages_default_allowed_roles|r->r in
	let contents = List.map (function 
		| Namespace(n,cs)->namespace ~get_field_writer_roles ~get_field_reader_roles ~name:n ~contents:cs ~idempotent:true ()
		| Field f->Field{f with field_setter_roles=get_field_writer_roles f.field_setter_roles;
			field_getter_roles=get_field_reader_roles f.field_getter_roles}
		) contents in
	if lifecycle = None && in_product_since = None then
		failwith ("Lifecycle for class '" ^ name ^ "' not specified");
	let lifecycle = match lifecycle with
		| None ->
			let published = match in_product_since with
				| None -> []
				| Some rel -> [Published, rel, descr]
			in
			let deprecated = match internal_deprecated_since with
				| None -> []
				| Some rel -> [Deprecated, rel, ""]
			in
			published @ deprecated
		| Some l -> l
	in	
	let release =
		{
			internal = (match get_published lifecycle with 
				| Some published -> get_product_releases published
				| None -> ["closed"]);
			opensource = get_oss_releases in_oss_since;
			internal_deprecated_since = get_deprecated lifecycle;
		}
	in
	let msgs = List.map (fun m -> {m with msg_obj_name=name;msg_allowed_roles=get_msg_allowed_roles m.msg_allowed_roles}) messages in
	{ name = name; description = descr; obj_lifecycle = lifecycle; messages = msgs; contents = contents;
		doccomments = doccomments; gen_constructor_destructor = gen_constructor_destructor; force_custom_actions = force_custom_actions;
		persist = persist; gen_events = gen_events; obj_release = release;
		in_database=in_db; obj_allowed_roles = messages_default_allowed_roles; obj_implicit_msg_allowed_roles = implicit_messages_allowed_roles;
	}

(** Additional messages for srs *)
let dev_config_param =
  {param_type=Map(String,String); param_name="device_config"; param_doc="The device config string that will be passed to backend SR driver"; param_release=rio_release; param_default=None}

let sr_host_param =
  {param_type=Ref _host; param_name="host"; param_doc="The host to create/make the SR on"; param_release=rio_release; param_default=None}

let sr_physical_size_param =
  {param_type=Int; param_name="physical_size"; param_doc="The physical size of the new storage repository"; param_release=rio_release; param_default=None}

let sr_shared_param =
  {param_type=Bool; param_name="shared"; param_doc="True if the SR (is capable of) being shared by multiple hosts"; param_release=rio_release; param_default=None}

let sr_create_common =
  [
    {param_type=String; param_name="name_label"; param_doc="The name of the new storage repository"; param_release=rio_release; param_default=None};
    {param_type=String; param_name="name_description"; param_doc="The description of the new storage repository"; param_release=rio_release; param_default=None};
    {param_type=String; param_name="type"; param_doc="The type of the SR; used to specify the SR backend driver to use"; param_release=rio_release; param_default=None};
    {param_type=String; param_name="content_type"; param_doc="The type of the new SRs content, if required (e.g. ISOs)"; param_release=rio_release; param_default=None};
  ]

let sr_sm_config = 
  {param_type=Map(String,String); param_name="sm_config"; param_doc="Storage backend specific configuration options"; param_release=miami_release; param_default=Some (VMap [])}


let sr_create = call
  ~name:"create"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~versioned_params:(sr_host_param::dev_config_param::sr_physical_size_param::(sr_create_common @  [ sr_shared_param; sr_sm_config ] ))
  ~doc:"Create a new Storage Repository and introduce it into the managed system, creating both SR record and PBD record to attach it to current host (with specified device_config parameters)"
  ~result:(Ref _sr, "The reference of the newly created Storage Repository.")
  ~errs:[Api_errors.sr_unknown_driver]
  ~allowed_roles:_R_POOL_OP
    ()

let destroy_self_param =
  (Ref _sr, "sr", "The SR to destroy")

let sr_destroy = call
  ~name:"destroy"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~doc:"Destroy specified SR, removing SR-record from database and remove SR from disk. (In order to affect this operation the appropriate device_config is read from the specified SR's PBD on current host)"
  ~errs:[Api_errors.sr_has_pbd]
  ~params:[destroy_self_param]
  ~allowed_roles:_R_POOL_OP
  ()

let sr_forget = call
  ~name:"forget"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~doc:"Removing specified SR-record from database, without attempting to remove SR from disk"
  ~params:[destroy_self_param]
  ~errs:[Api_errors.sr_has_pbd]
  ~allowed_roles:_R_POOL_OP
  ()

let sr_introduce = 
  call
  ~name:"introduce"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~versioned_params:({param_type=String; param_name="uuid"; param_doc="The uuid assigned to the introduced SR"; param_release=rio_release; param_default=None}::(sr_create_common @ [sr_shared_param; sr_sm_config]))
  ~doc:"Introduce a new Storage Repository into the managed system"
  ~result:(Ref _sr, "The reference of the newly introduced Storage Repository.")
  ~allowed_roles:_R_POOL_OP
    ()

let sr_probe = call
  ~name:"probe"
  ~in_oss_since:None
  ~in_product_since:rel_miami
  ~versioned_params:[sr_host_param; dev_config_param; {param_type=String; param_name="type"; param_doc="The type of the SR; used to specify the SR backend driver to use"; param_release=miami_release; param_default=None}; sr_sm_config]
  ~doc:"Perform a backend-specific scan, using the given device_config.  If the device_config is complete, then this will return a list of the SRs present of this type on the device, if any.  If the device_config is partial, then a backend-specific scan will be performed, returning results that will guide the user in improving the device_config."
  ~result:(String, "An XML fragment containing the scan results.  These are specific to the scan being performed, and the backend.")
  ~allowed_roles:_R_POOL_OP
    ()

let sr_make = call
  ~name:"make"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~internal_deprecated_since:rel_miami
  ~lifecycle:[
    Published, rel_rio, "Create a new Storage Repository on disk";
    Deprecated, rel_miami, "Use SR.create instead"
  ]
  ~versioned_params:(sr_host_param::dev_config_param::sr_physical_size_param::(sr_create_common @ [sr_sm_config]))
  ~doc:"Create a new Storage Repository on disk. This call is deprecated: use SR.create instead."
  ~result:(String, "The uuid of the newly created Storage Repository.")
  ~allowed_roles:_R_POOL_OP
    ()

let sr_get_supported_types = call
  ~name:"get_supported_types"
  ~in_product_since:rel_rio
  ~flags:[`Session]
  ~doc:"Return a set of all the SR types supported by the system"
  ~params:[]
  ~result:(Set String, "the supported SR types")
  ~allowed_roles:_R_READ_ONLY
()

let sr_scan = call
  ~name:"scan"
  ~in_product_since:rel_rio
  ~doc:"Refreshes the list of VDIs associated with an SR"
  ~params:[Ref _sr, "sr", "The SR to scan" ]
  ~allowed_roles:_R_POOL_OP
  ()

(* Nb, although this is a new explicit call, it's actually been in the API since rio - just autogenerated. So no setting of rel_miami. *)
let sr_set_shared = call
  ~name:"set_shared"
  ~in_product_since:rel_rio
  ~doc:"Sets the shared flag on the SR"
  ~params:[Ref _sr, "sr", "The SR";
	   Bool, "value", "True if the SR is shared"]
  ~allowed_roles:_R_POOL_OP
  ()

let sr_set_name_label = call
	~name:"set_name_label"
	~in_product_since:rel_rio
	~doc:"Set the name label of the SR"
	~params:[Ref _sr, "sr", "The SR";
	         String, "value", "The name label for the SR"]
	~allowed_roles:_R_POOL_OP
	()

let sr_set_name_description = call
	~name:"set_name_description"
	~in_product_since:rel_rio
	~doc:"Set the name description of the SR"
	~params:[Ref _sr, "sr", "The SR";
	         String, "value", "The name description for the SR"]
	~allowed_roles:_R_POOL_OP
	()

let sr_create_new_blob = call
  ~name: "create_new_blob"
  ~in_product_since:rel_orlando
  ~doc:"Create a placeholder for a named binary blob of data that is associated with this SR"
  ~versioned_params:
  [{param_type=Ref _sr; param_name="sr"; param_doc="The SR"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
  {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}
  ]
  ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
  ~allowed_roles:_R_POOL_OP
  ()

let pbd_plug = call
  ~name:"plug"
  ~in_oss_since:None 
  ~in_product_since:rel_rio
  ~doc:"Activate the specified PBD, causing the referenced SR to be attached and scanned"
  ~params:[Ref _pbd, "self", "The PBD to activate"]
  ~errs:[Api_errors.sr_unknown_driver]
  ~allowed_roles:_R_POOL_OP
  ()

let pbd_unplug = call
  ~name:"unplug"
  ~in_oss_since:None 
  ~in_product_since:rel_rio
  ~doc:"Deactivate the specified PBD, causing the referenced SR to be detached and nolonger scanned"
  ~params:[Ref _pbd, "self", "The PBD to deactivate"]
  ~allowed_roles:_R_POOL_OP
  ()

(** Sessions *)
let session = 
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_session ~descr:"A session" ~gen_events:false
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~messages:[session_login; session_logout; session_chpass;
	       slave_login; 
	       slave_local_login; slave_local_login_with_password; local_logout;
	       session_get_all_subject_identifiers; session_logout_subject_identifier;
	      ] ~contents:[
		  uid _session;
		  field ~qualifier:DynamicRO ~ty:(Ref _host)
		    "this_host" "Currently connected host";
		  field ~qualifier:DynamicRO ~ty:(Ref _user)
		    "this_user" "Currently connected user";
		  field ~qualifier:DynamicRO ~ty:DateTime
		    "last_active" "Timestamp for last time session was active";
		  field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None
		    "pool" "True if this session relates to a intra-pool login, false otherwise";
		  field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
		  field ~in_product_since:rel_george ~qualifier:DynamicRO ~default_value:(Some (VBool false)) ~ty:Bool "is_local_superuser" "true iff this session was created using local superuser credentials";
		  field ~in_product_since:rel_george ~qualifier:DynamicRO ~default_value:(Some (VRef (Ref.string_of Ref.null))) ~ty:(Ref _subject) "subject" "references the subject instance that created the session. If a session instance has is_local_superuser set, then the value of this field is undefined.";
		  field ~in_product_since:rel_george ~qualifier:DynamicRO ~default_value:(Some(VDateTime(Date.of_float 0.))) ~ty:DateTime "validation_time" "time when session was last validated";
		  field ~in_product_since:rel_george ~qualifier:DynamicRO ~default_value:(Some(VString(""))) ~ty:String "auth_user_sid" "the subject identifier of the user that was externally authenticated. If a session instance has is_local_superuser set, then the value of this field is undefined.";
		  field ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~default_value:(Some(VString(""))) ~ty:String "auth_user_name" "the subject name of the user that was externally authenticated. If a session instance has is_local_superuser set, then the value of this field is undefined.";
		  field ~in_product_since:rel_midnight_ride ~qualifier:StaticRO ~default_value:(Some(VSet [])) ~ty:(Set(String)) "rbac_permissions" "list with all RBAC permissions for this session";
		  field ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:(Set(Ref _task)) "tasks" "list of tasks created using the current session";
		  field ~in_product_since:rel_midnight_ride ~qualifier:StaticRO ~default_value:(Some (VRef (Ref.string_of Ref.null))) ~ty:(Ref _session) "parent" "references the parent session that created this session"; 
		  field ~in_product_since:rel_clearwater ~qualifier:DynamicRO ~default_value:(Some(VString(""))) ~ty:String  "originator" "a key string provided by a API user to distinguish itself from other users sharing the same login name";
		]
	()


(** Tasks *)


(* NB: the status 'cancelling' is not being used, nor should it ever be used. It should be purged from here! *)
let status_type = Enum("task_status_type", [ "pending", "task is in progress";
					     "success", "task was completed successfully";
					     "failure", "task has failed";
					     "cancelling", "task is being cancelled";
					     "cancelled", "task has been cancelled" ])


let task_cancel = call
  
  ~name:"cancel"
  ~in_product_since:rel_rio
  ~doc:"Request that a task be cancelled. Note that a task may fail to be cancelled and may complete or fail normally and note that, even when a task does cancel, it might take an arbitrary amount of time."
  ~params:[Ref _task, "task", "The task"]
  ~errs:[Api_errors.operation_not_allowed]
  ~allowed_roles:_R_READ_ONLY (* POOL_OP can cancel any tasks, others can cancel only owned tasks *)
  ()


let task_create = call ~flags:[`Session]
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~name:"create"
  ~doc:"Create a new task object which must be manually destroyed."
  ~params:[String, "label", "short label for the new task";
   String, "description", "longer description for the new task"]
  ~result:(Ref _task, "The reference of the created task object")
  ~allowed_roles:_R_READ_ONLY (* any subject can create tasks *)
  ()

let task_destroy = call ~flags:[`Session]
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~name:"destroy"
  ~doc:"Destroy the task object"
  ~params:[Ref _task, "self", "Reference to the task object"]
  ~allowed_roles:_R_READ_ONLY (* POOL_OP can destroy any tasks, others can destroy only owned tasks *)
  ()
(* this permission allows to destroy any task, instead of only the owned ones *)
let extra_permission_task_destroy_any = "task.destroy/any"

let task_allowed_operations =
  Enum ("task_allowed_operations", List.map operation_enum [ task_cancel ])

let task = 
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_task ~descr:"A long-running asynchronous task" ~gen_events:true
    ~doccomments:[] 
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages: [ task_create; task_destroy; task_cancel ]
    ~contents: ([
      uid _task;
      namespace ~name:"name" ~contents:(names oss_since_303 DynamicRO) ();
    ] @ (allowed_and_current_operations task_allowed_operations) @ [
      field ~qualifier:DynamicRO ~ty:DateTime "created" "Time task was created";
      field ~qualifier:DynamicRO ~ty:DateTime "finished" "Time task finished (i.e. succeeded or failed). If task-status is pending, then the value of this field has no meaning";
      field ~qualifier:DynamicRO ~ty:status_type "status" "current status of the task";
      field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _session) "session" "the session that created the task";
      field ~qualifier:DynamicRO ~ty:(Ref _host) "resident_on" "the host on which the task is running";
      field ~qualifier:DynamicRO ~ty:Float "progress" "This field contains the estimated fraction of the task which is complete. This field should not be used to determine whether the task is complete - for this the status field of the task should be used.";
      field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Int "externalpid" "If the task has spawned a program, the field record the PID of the process that the task is waiting on. (-1 if no waiting completion of an external program )";
      field ~in_oss_since:None ~internal_deprecated_since:rel_boston ~internal_only:true ~qualifier:DynamicRO ~ty:Int "stunnelpid" "If the task has been forwarded, this field records the pid of the stunnel process spawned to manage the forwarding connection";
      field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Bool "forwarded" "True if this task has been forwarded to a slave";
      field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _host) "forwarded_to" "The host to which the task has been forwarded";
      field ~qualifier:DynamicRO ~ty:String "type" "if the task has completed successfully, this field contains the type of the encoded result (i.e. name of the class whose reference is in the result field). Undefined otherwise.";
      field ~qualifier:DynamicRO ~ty:String "result" "if the task has completed successfully, this field contains the result value (either Void or an object reference). Undefined otherwise.";
      field ~qualifier:DynamicRO ~ty:(Set String) "error_info" "if the task has failed, this field contains the set of associated error strings. Undefined otherwise.";
      field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("applies_to",(_R_VM_OP));("XenCenterUUID",(_R_VM_OP));("XenCenterMeddlingActionTitle",(_R_VM_OP))];
      (* field ~ty:(Set(Ref _alert)) ~in_product_since:rel_miami ~qualifier:DynamicRO "alerts" "all alerts related to this task"; *)
      field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VRef "")) ~ty:(Ref _task) "subtask_of" "Ref pointing to the task this is a substask of.";
      field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Set (Ref _task)) "subtasks"   "List pointing to all the substasks."; 
    ]) 
    ()

(** Many of the objects need to record IO bandwidth *)
let iobandwidth =
  [ field ~persist:false ~qualifier:DynamicRO ~ty:Float "read_kbs" "Read bandwidth (KiB/s)";
    field ~persist:false ~qualifier:DynamicRO ~ty:Float "write_kbs" "Write bandwidth (KiB/s)" ]

(** Human users *)
let user = (* DEPRECATED in favor of subject *)
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_user ~descr:"A user of the system" ~gen_events:false
    ~lifecycle:[
      Published, rel_rio, "A user of the system";
      Deprecated, rel_george, "Deprecated in favor of subject";
    ]
    ~doccomments:[] 
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~messages:[] ~contents:
      [ uid _user;
	field ~qualifier:StaticRO "short_name" "short name (e.g. userid)";
	field "fullname" "full name";
	  field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
    ()

(** Guest Memory *)
let guest_memory =
	let field = field ~ty:Int in
	[
		field "overhead" ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO "Virtualization memory overhead (bytes)." ~default_value:(Some (VInt 0L));
		field "target" ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:StaticRO "Dynamically-set memory target (bytes). The value of this field indicates the current target for memory available to this VM." ~default_value:(Some (VInt 0L)) ~internal_deprecated_since:rel_midnight_ride;
		field "static_max" ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:StaticRO "Statically-set (i.e. absolute) maximum (bytes). The value of this field at VM start time acts as a hard limit of the amount of memory a guest can use. New values only take effect on reboot.";
		field "dynamic_max" ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:StaticRO "Dynamic maximum (bytes)";
		field "dynamic_min" ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:StaticRO "Dynamic minimum (bytes)";
		field "static_min" ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:StaticRO "Statically-set (i.e. absolute) mininum (bytes). The value of this field indicates the least amount of memory this VM can boot with without crashing.";
	]

(** Host Memory *)
let host_memory =
	let field = field ~ty:Int in
	[
		field ~qualifier:DynamicRO "overhead" "Virtualization memory overhead (bytes)." ~default_value:(Some (VInt 0L));
	]

(** Host Metrics Memory *)
let host_metrics_memory = 
	let field = field ~ty:Int in
	[
		field ~qualifier:DynamicRO "total" "Total host memory (bytes)";
		field ~qualifier:DynamicRO ~internal_deprecated_since:rel_midnight_ride "free" "Free host memory (bytes)";
	]

let api_version = 
  let field' = field ~qualifier:DynamicRO in
  [
    field' ~ty:Int "major" "major version number";
    field' ~ty:Int "minor" "minor version number";
    field' ~ty:String "vendor" "identification of vendor";
    field' ~ty:(Map(String,String)) "vendor_implementation" "details of vendor implementation";
  ]

(* Management of host crash dumps. Note that this would be neater if crashes were stored in 
   VDIs like VM crashes, however the nature of a host crash dump is that the dom0 has crashed
   and has no access to any fancy storage drivers or tools. Plus a host is not guaranteed to 
   have any SRs at all. *)

let host_crashdump_destroy = call
  ~name:"destroy"
  ~doc:"Destroy specified host crash dump, removing it from the disk."
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[ Ref _host_crashdump, "self", "The host crashdump to destroy" ]
  ~allowed_roles:_R_POOL_OP
  ()

let host_crashdump_upload = call
  ~name:"upload"
  ~doc:"Upload the specified host crash dump to a specified URL"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[ Ref _host_crashdump, "self", "The host crashdump to upload";
	    String, "url", "The URL to upload to";
	    Map(String, String), "options", "Extra configuration operations" ]
  ~allowed_roles:_R_POOL_OP
  ()

let host_crashdump =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host_crashdump ~gen_events:true
    ~descr:"Represents a host crash dump"
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages: [host_crashdump_destroy; host_crashdump_upload]
    ~contents:
    [ uid ~in_oss_since:None _host_crashdump;
      field ~in_oss_since:None ~qualifier:StaticRO ~ty:(Ref _host) "host" "Host the crashdump relates to";
      field ~in_oss_since:None ~qualifier:DynamicRO ~ty:DateTime "timestamp" "Time the crash happened";
      field ~in_oss_since:None ~qualifier:DynamicRO ~ty:Int "size" "Size of the crashdump";
      field ~qualifier:StaticRO ~ty:String ~in_oss_since:None ~internal_only:true "filename" "filename of crash dir";
      field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
    ] 
    ()

(* New Miami pool patching mechanism *)

let pool_patch_after_apply_guidance =
  Enum ("after_apply_guidance", 
        [ "restartHVM",  "This patch requires HVM guests to be restarted once applied.";
          "restartPV",   "This patch requires PV guests to be restarted once applied.";
		  "restartHost", "This patch requires the host to be restarted once applied."; 
          "restartXAPI", "This patch requires XAPI to be restarted once applied.";
        ])

let pool_patch_apply = call
  ~name:"apply"
  ~doc:"Apply the selected patch to a host and return its output"
  ~in_oss_since:None
  ~in_product_since:rel_miami
  ~params:[ Ref _pool_patch, "self", "The patch to apply"; Ref _host, "host", "The host to apply the patch too" ]  
  ~result:(String, "the output of the patch application process")
  ~allowed_roles:_R_POOL_OP
  ()

let pool_patch_precheck = call
  ~name:"precheck"
  ~doc:"Execute the precheck stage of the selected patch on a host and return its output"
  ~in_oss_since:None
  ~in_product_since:rel_miami
  ~params:[ Ref _pool_patch, "self", "The patch whose prechecks will be run"; Ref _host, "host", "The host to run the prechecks on" ]  
  ~result:(String, "the output of the patch prechecks")
  ~allowed_roles:_R_POOL_OP
  ()

let pool_patch_clean = call
  ~name:"clean"
  ~doc:"Removes the patch's files from the server"
  ~in_oss_since:None
  ~in_product_since:rel_miami
  ~params:[ Ref _pool_patch, "self", "The patch to clean up" ]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_patch_clean_on_host = call
  ~name:"clean_on_host"
  ~doc:"Removes the patch's files from the specified host"
  ~in_oss_since:None
  ~in_product_since:rel_tampa
  ~params:[ Ref _pool_patch, "self", "The patch to clean up"; Ref _host, "host", "The host on which to clean the patch"  ]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_patch_pool_clean = call
  ~name:"pool_clean"
  ~doc:"Removes the patch's files from all hosts in the pool, but does not remove the database entries"
  ~in_oss_since:None
  ~in_product_since:rel_tampa
  ~params:[ Ref _pool_patch, "self", "The patch to clean up" ]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_patch_destroy = call
  ~name:"destroy"
  ~doc:"Removes the patch's files from all hosts in the pool, and removes the database entries.  Only works on unapplied patches."
  ~in_oss_since:None
  ~in_product_since:rel_miami
  ~params:[ Ref _pool_patch, "self", "The patch to destroy" ]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_patch_pool_apply = call
  ~name:"pool_apply"
  ~doc:"Apply the selected patch to all hosts in the pool and return a map of host_ref -> patch output"
  ~in_oss_since:None
  ~in_product_since:rel_miami
  ~params:[ Ref _pool_patch, "self", "The patch to apply"]  
  ~allowed_roles:_R_POOL_OP
  ()

let pool_patch =
  create_obj ~in_db:true 
    ~in_product_since:rel_miami 
    ~in_oss_since:None 
    ~internal_deprecated_since:None

    ~persist:PersistEverything 
    ~gen_constructor_destructor:false 
    ~gen_events:true

    ~name:_pool_patch 
    ~descr:"Pool-wide patches"
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[pool_patch_apply; pool_patch_pool_apply; pool_patch_precheck; pool_patch_clean; pool_patch_pool_clean; pool_patch_destroy; pool_patch_clean_on_host]
    ~contents:
    [ uid       ~in_oss_since:None _pool_patch;
      namespace ~name:"name" ~contents:(names None StaticRO) ();
      field     ~in_product_since:rel_miami ~default_value:(Some (VString "")) ~in_oss_since:None ~qualifier:StaticRO ~ty:String "version" "Patch version number";
      field     ~in_product_since:rel_miami ~default_value:(Some (VString "")) ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:String "filename" "Filename of the patch";
      field     ~in_product_since:rel_miami ~default_value:(Some (VInt Int64.zero)) ~in_oss_since:None ~qualifier:DynamicRO ~ty:Int "size" "Size of the patch";
      field     ~in_product_since:rel_miami ~default_value:(Some (VBool false)) ~in_oss_since:None ~qualifier:DynamicRO ~ty:Bool "pool_applied" "This patch should be applied across the entire pool";
      field     ~in_product_since:rel_miami ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set (Ref _host_patch)) "host_patches" "This hosts this patch is applied to.";
      field     ~in_product_since:rel_miami ~default_value:(Some (VSet [])) ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set pool_patch_after_apply_guidance) "after_apply_guidance" "What the client should do after this patch has been applied.";
      field     ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~in_oss_since:None  ~ty:(Map(String, String)) "other_config" "additional configuration";
    ] 
	()

(* Management of host patches. Just like the crash dumps it would be marginally neater if
   the patches were stored as VDIs. *)

let host_patch_destroy = call
  ~name:"destroy"
  ~doc:"Destroy the specified host patch, removing it from the disk. This does NOT reverse the patch"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[ Ref _host_patch, "self", "The patch to destroy" ]
  ~internal_deprecated_since: rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let host_patch_apply = call
  ~name:"apply"
  ~doc:"Apply the selected patch and return its output"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[ Ref _host_patch, "self", "The patch to apply" ]  
  ~result:(String, "the output of the patch application process")
  ~internal_deprecated_since: rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let host_patch = 
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host_patch ~gen_events:true 
    ~descr:"Represents a patch stored on a server"
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages: [host_patch_destroy; host_patch_apply]
    ~contents:
    [ uid ~in_oss_since:None _host_patch;
      namespace ~name:"name" ~contents:(names None StaticRO) ();
      field ~in_oss_since:None ~qualifier:StaticRO ~ty:String "version" "Patch version number";
      field ~in_oss_since:None ~qualifier:StaticRO ~ty:(Ref _host) "host" "Host the patch relates to";
      field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:String "filename" "Filename of the patch";
      field ~in_oss_since:None ~qualifier:DynamicRO ~ty:Bool "applied" "True if the patch has been applied";
      field ~in_oss_since:None ~qualifier:DynamicRO ~ty:DateTime "timestamp_applied" "Time the patch was applied";
      field ~in_oss_since:None ~qualifier:DynamicRO ~ty:Int "size" "Size of the patch";
      field ~in_product_since:rel_miami ~in_oss_since:None ~qualifier:StaticRO ~ty:(Ref _pool_patch) ~default_value:(Some (VRef "")) "pool_patch" "The patch applied";
      field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~in_oss_since:None  ~ty:(Map(String, String)) "other_config" "additional configuration";
    ]
    ()

let host_bugreport_upload = call
  ~name:"bugreport_upload"
  ~doc:"Run xen-bugtool --yestoall and upload the output to Citrix support"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[ Ref _host, "host", "The host on which to run xen-bugtool";
	    String, "url", "The URL to upload to";
	    Map(String, String), "options", "Extra configuration operations" ]
  ~allowed_roles:_R_POOL_OP
  ()

let host_list_methods = call
  ~name:"list_methods"
  ~in_product_since:rel_rio
  ~flags: [`Session]
  ~doc:"List all supported methods"
  ~params:[]
  ~result:(Set(String), "The name of every supported method.")
  ~allowed_roles:_R_READ_ONLY
  ()

let host_license_apply = call
  ~name:"license_apply"
  ~in_oss_since:None
  ~lifecycle:[
    Published, rel_rio, "Apply a new license to a host";
    Removed, rel_clearwater, "Free licenses no longer handled by xapi";
  ]
  ~params:[Ref _host, "host", "The host to upload the license to";
	   String, "contents", "The contents of the license file, base64 encoded"]
  ~doc:"Apply a new license to a host"
  ~errs: [Api_errors.license_processing_error]
  ~allowed_roles:_R_POOL_OP
  ()

let host_create_params =
  [
    {param_type=String; param_name="uuid"; param_doc="unique identifier/object reference"; param_release=rio_release; param_default=None};
    {param_type=String; param_name="name_label"; param_doc="The name of the new storage repository"; param_release=rio_release; param_default=None};
    {param_type=String; param_name="name_description"; param_doc="The description of the new storage repository"; param_release=rio_release; param_default=None};
    {param_type=String; param_name="hostname"; param_doc="Hostname"; param_release=rio_release; param_default=None};
    {param_type=String; param_name="address"; param_doc="An address by which this host can be contacted by other members in its pool"; param_release=rio_release; param_default=None};
    {param_type=String; param_name="external_auth_type"; param_doc="type of external authentication service configured; empty if none configured"; param_release=george_release; param_default=Some(VString "")};
    {param_type=String; param_name="external_auth_service_name"; param_doc="name of external authentication service configured; empty if none configured"; param_release=george_release; param_default=Some(VString "")};
    {param_type=Map(String,String); param_name="external_auth_configuration"; param_doc="configuration specific to external authentication service"; param_release=george_release; param_default=Some(VMap [])};
    {param_type=Map(String,String); param_name="license_params"; param_doc="State of the current license"; param_release=midnight_ride_release; param_default=Some(VMap [])};
    {param_type=String; param_name="edition"; param_doc="XenServer edition"; param_release=midnight_ride_release; param_default=Some(VString "")};
    {param_type=Map(String,String); param_name="license_server"; param_doc="Contact information of the license server"; param_release=midnight_ride_release; param_default=Some(VMap [VString "address", VString "localhost"; VString "port", VString "27000"])};
    {param_type=Ref _sr; param_name="local_cache_sr"; param_doc="The SR that is used as a local cache"; param_release=cowley_release; param_default=(Some (VRef (Ref.string_of Ref.null)))};
    {param_type=Map(String,String); param_name="chipset_info"; param_doc="Information about chipset features"; param_release=boston_release; param_default=Some(VMap [])};
  ]

let host_create = call
  ~name:"create"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~versioned_params:host_create_params
  ~doc:"Create a new host record"
  ~result:(Ref _host, "Reference to the newly created host object.")
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_OP
    ()

let host_destroy = call
  ~name:"destroy"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~doc:"Destroy specified host record in database"
  ~params:[(Ref _host, "self", "The host record to remove")]
  ~allowed_roles:_R_POOL_OP
  ()

let host_get_system_status_capabilities = call ~flags:[`Session]
  ~name:"get_system_status_capabilities"
  ~in_oss_since:None
  ~in_product_since:rel_miami
  ~params:[Ref _host, "host", "The host to interrogate"]
  ~doc:""
  ~result:(String, "An XML fragment containing the system status capabilities.")
  ~allowed_roles:_R_READ_ONLY
    ()

let host_set_hostname_live = call ~flags:[`Session]
  ~name:"set_hostname_live"
  ~in_oss_since:None
  ~in_product_since:rel_miami
  ~params:[Ref _host, "host", "The host whose host name to set";
           String, "hostname", "The new host name"]
  ~errs:[Api_errors.host_name_invalid]
  ~doc:"Sets the host name to the specified string.  Both the API and lower-level system hostname are changed immediately."
  ~allowed_roles:_R_POOL_OP
    ()

let host_tickle_heartbeat = call ~flags:[`Session]
  ~name:"tickle_heartbeat"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~params:[Ref _host, "host", "The host calling the function, and whose heartbeat to tickle";
	   Map(String, String), "stuff", "Anything else we want to let the master know";
	  ]
  ~result:(Map(String, String), "Anything the master wants to tell the slave")
  ~doc:"Needs to be called every 30 seconds for the master to believe the host is alive"
  ~pool_internal:true
  ~hide_from_docs:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_sync_data = call ~flags:[`Session]
  ~name:"sync_data"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~params:[Ref _host, "host", "The host to whom the data should be sent"]
  ~doc:"This causes the synchronisation of the non-database data (messages, RRDs and so on) stored on the master to be synchronised with the host"
  ~allowed_roles:_R_POOL_ADMIN
  ()

let host_backup_rrds = call ~flags:[`Session]
  ~name:"backup_rrds"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~params:[Ref _host, "host", "Schedule a backup of the RRDs of this host";
	   Float, "delay", "Delay in seconds from when the call is received to perform the backup"]
  ~doc:"This causes the RRDs to be backed up to the master"
  ~allowed_roles:_R_POOL_ADMIN
  ()

let host_get_servertime = call ~flags:[`Session]
  ~name:"get_servertime"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~params:[Ref _host, "host", "The host whose clock should be queried"]
  ~doc:"This call queries the host's clock for the current time"
  ~result:(DateTime, "The current time")
  ~allowed_roles:_R_READ_ONLY
  ()

let host_get_server_localtime = call ~flags:[`Session]
  ~name:"get_server_localtime"
  ~in_oss_since:None
  ~in_product_since:rel_cowley
  ~params:[Ref _host, "host", "The host whose clock should be queried"]
  ~doc:"This call queries the host's clock for the current time in the host's local timezone"
  ~result:(DateTime, "The current local time")
  ~allowed_roles:_R_READ_ONLY
  ()

let host_emergency_ha_disable = call ~flags:[`Session]
  ~name:"emergency_ha_disable"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~params:[]
  ~doc:"This call disables HA on the local host. This should only be used with extreme care."
  ~allowed_roles:_R_POOL_OP
  ()

let host_certificate_install = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~pool_internal:true
  ~hide_from_docs:true
  ~name:"certificate_install"
  ~doc:"Install an SSL certificate to this host."
  ~params:[Ref _host, "host", "The host";
           String, "name", "A name to give the certificate";
           String, "cert", "The certificate"]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_certificate_uninstall = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~pool_internal:true
  ~hide_from_docs:true
  ~name:"certificate_uninstall"
  ~doc:"Remove an SSL certificate from this host."
  ~params:[Ref _host, "host", "The host";
           String, "name", "The certificate name"]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_certificate_list = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~pool_internal:true
  ~hide_from_docs:true
  ~name:"certificate_list"
  ~doc:"List all installed SSL certificates."
  ~params:[Ref _host, "host", "The host"]
  ~result:(Set(String),"All installed certificates")
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_crl_install = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~pool_internal:true
  ~hide_from_docs:true
  ~name:"crl_install"
  ~doc:"Install an SSL certificate revocation list to this host."
  ~params:[Ref _host, "host", "The host";
           String, "name", "A name to give the CRL";
           String, "crl", "The CRL"]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_crl_uninstall = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~pool_internal:true
  ~hide_from_docs:true
  ~name:"crl_uninstall"
  ~doc:"Remove an SSL certificate revocation list from this host."
  ~params:[Ref _host, "host", "The host";
           String, "name", "The CRL name"]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_crl_list = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~pool_internal:true
  ~hide_from_docs:true
  ~name:"crl_list"
  ~doc:"List all installed SSL certificate revocation lists."
  ~params:[Ref _host, "host", "The host"]
  ~result:(Set(String),"All installed CRLs")
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_certificate_sync = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~pool_internal:true
  ~hide_from_docs:true
  ~name:"certificate_sync"
  ~doc:"Resync installed SSL certificates and CRLs."
  ~params:[Ref _host, "host", "The host"]
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_get_server_certificate = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~name:"get_server_certificate"
  ~doc:"Get the installed server SSL certificate."
  ~params:[Ref _host, "host", "The host"]
  ~result:(String,"The installed server SSL certificate, in PEM form.")
  ~allowed_roles:_R_POOL_OP
  ()

let host_operations =
  Enum ("host_allowed_operations", 
	[ "provision", "Indicates this host is able to provision another VM"; 
	  "evacuate", "Indicates this host is evacuating";
	  "shutdown", "Indicates this host is in the process of shutting itself down";
	  "reboot", "Indicates this host is in the process of rebooting";
	  "power_on", "Indicates this host is in the process of being powered on";
	  "vm_start", "This host is starting a VM";
	  "vm_resume", "This host is resuming a VM";
	  "vm_migrate", "This host is the migration target of a VM";
	])

let host_enable_external_auth = call ~flags:[`Session]
  ~name:"enable_external_auth"
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~params:[
    Ref _host, "host", "The host whose external authentication should be enabled"; 
    Map (String,String), "config", "A list of key-values containing the configuration data" ; 
    String, "service_name", "The name of the service" ; 
    String, "auth_type", "The type of authentication (e.g. AD for Active Directory)" 
    ]
  ~doc:"This call enables external authentication on a host"
  ~allowed_roles:_R_POOL_ADMIN
  ()

let host_disable_external_auth = call ~flags:[`Session]
  ~name:"disable_external_auth"
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~versioned_params:[
    {param_type=Ref _host; param_name="host"; param_doc="The host whose external authentication should be disabled"; param_release=george_release; param_default=None};
    {param_type=Map (String, String); param_name="config"; param_doc="Optional parameters as a list of key-values containing the configuration data"; param_release=george_release; param_default=Some (VMap [])}
    ]
  ~doc:"This call disables external authentication on the local host"
  ~allowed_roles:_R_POOL_ADMIN
  ()

let host_set_license_params = call
  ~name:"set_license_params"
  ~in_product_since:rel_orlando (* actually update 3 aka floodgate *)
  ~doc:"Set the new license details in the database, trigger a recomputation of the pool SKU"
  ~params:[ 
    Ref _host, "self", "The host";
    Map(String, String), "value", "The license_params"
  ]
  ~hide_from_docs:true
  ~pool_internal:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let host_apply_edition = call ~flags:[`Session]
  ~name:"apply_edition"
  ~in_product_since:rel_midnight_ride
  ~doc:"Change to another edition, or reactivate the current edition after a license has expired. This may be subject to the successful checkout of an appropriate license."
  ~versioned_params:[
    {param_type=Ref _host; param_name="host"; param_doc="The host"; param_release=midnight_ride_release; param_default=None};
    {param_type=String; param_name="edition"; param_doc="The requested edition"; param_release=midnight_ride_release; param_default=None};
    {param_type=Bool; param_name="force"; param_doc="Update the license params even if the apply call fails"; param_release=clearwater_release; param_default=Some (VBool false)};
  ]
  ~allowed_roles:_R_POOL_OP
  ()
 
let host_set_power_on_mode = call
  ~name:"set_power_on_mode"
  ~in_product_since:rel_midnight_ride
  ~doc:"Set the power-on-mode, host, user and password "
  ~params:[ 
    Ref _host, "self", "The host";
    String, "power_on_mode", "power-on-mode can be empty,iLO,wake-on-lan, DRAC or other";
    Map(String, String), "power_on_config", "Power on config";
          ]
  ~allowed_roles:_R_POOL_OP
  ()
  
let host_set_cpu_features = call ~flags:[`Session]
  ~name:"set_cpu_features"
  ~in_product_since:rel_midnight_ride
  ~doc:"Set the CPU features to be used after a reboot, if the given features string is valid."
  ~params:[ 
    Ref _host, "host", "The host";
    String, "features", "The features string (32 hexadecimal digits)"
  ]
  ~allowed_roles:_R_POOL_OP
  ()
  
let host_reset_cpu_features = call ~flags:[`Session]
  ~name:"reset_cpu_features"
  ~in_product_since:rel_midnight_ride
  ~doc:"Remove the feature mask, such that after a reboot all features of the CPU are enabled."
  ~params:[ 
    Ref _host, "host", "The host"
  ]
  ~allowed_roles:_R_POOL_OP
  ()

let host_reset_networking = call
	~name:"reset_networking"
	~lifecycle:[]
	~doc:"Purge all network-related metadata associated with the given host."
	~params:[Ref _host, "host", "The Host to modify"]
	~allowed_roles:_R_POOL_OP
	~hide_from_docs:true
	() 
  
let host_enable_local_storage_caching = call ~flags:[`Session]
	~name:"enable_local_storage_caching"
	~in_product_since:rel_cowley
	~doc:"Enable the use of a local SR for caching purposes"
	~params:[
		Ref _host, "host", "The host";
		Ref _sr, "sr", "The SR to use as a local cache"
	]
	~allowed_roles:_R_POOL_OP
	()

let host_disable_local_storage_caching = call ~flags:[`Session]
	~name:"disable_local_storage_caching"
	~in_product_since:rel_cowley
	~doc:"Disable the use of a local SR for caching purposes"
	~params:[
		Ref _host, "host", "The host"
	]
	~allowed_roles:_R_POOL_OP
	()

let host_get_sm_diagnostics = call ~flags:[`Session]
	~name:"get_sm_diagnostics"
	~in_product_since:rel_boston
	~doc:"Return live SM diagnostics"
	~params:[
		Ref _host, "host", "The host"
	]
	~result:(String, "Printable diagnostic data")
	~allowed_roles:_R_POOL_OP
	~hide_from_docs:true
	()

let host_get_thread_diagnostics = call ~flags:[`Session]
	~name:"get_thread_diagnostics"
	~in_product_since:rel_boston
	~doc:"Return live thread diagnostics"
	~params:[
		Ref _host, "host", "The host"
	]
	~result:(String, "Printable diagnostic data")
	~allowed_roles:_R_POOL_OP
	~hide_from_docs:true
	()

let host_sm_dp_destroy = call ~flags:[`Session]
	~name:"sm_dp_destroy"
	~in_product_since:rel_boston
	~doc:"Attempt to cleanup and destroy a named SM datapath"
	~params:[
		Ref _host, "host", "The host";
		String, "dp", "The datapath";
		Bool, "allow_leak", "If true, all records of the datapath will be removed even if the datapath could not be destroyed cleanly.";
	]
	~allowed_roles:_R_POOL_OP
	~hide_from_docs:true
	()

let host_sync_vlans = call ~flags:[`Session]
	~name:"sync_vlans"
	~lifecycle:[]
	~doc:"Synchronise VLANs on given host with the master's VLANs"
	~params:[
		Ref _host, "host", "The host";
	]
	~hide_from_docs:true
	~pool_internal:true
	~allowed_roles:_R_POOL_OP
	()

let host_sync_tunnels = call ~flags:[`Session]
	~name:"sync_tunnels"
	~lifecycle:[]
	~doc:"Synchronise tunnels on given host with the master's tunnels"
	~params:[
		Ref _host, "host", "The host";
	]
	~hide_from_docs:true
	~pool_internal:true
	~allowed_roles:_R_POOL_OP
	()

let host_sync_pif_currently_attached = call ~flags:[`Session]
	~name:"sync_pif_currently_attached"
	~lifecycle:[]
	~doc:"Synchronise tunnels on given host with the master's tunnels"
	~params:[
		Ref _host, "host", "The host";
		Set String, "bridges", "A list of bridges that are currently up";
	]
	~hide_from_docs:true
	~pool_internal:true
	~allowed_roles:_R_POOL_OP
	()

(** Hosts *)
let host =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host ~descr:"A physical host" ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages: [host_disable; host_enable; host_shutdown; host_reboot; host_dmesg; host_dmesg_clear; host_get_log; host_send_debug_keys; host_bugreport_upload; host_list_methods; host_license_apply; host_create; host_destroy; 
		  host_power_on;
		 host_set_license_params;
		 host_emergency_ha_disable;
		 host_ha_disarm_fencing; host_preconfigure_ha; host_ha_join_liveset; 
		 host_ha_disable_failover_decisions;
		 host_ha_wait_for_shutdown_via_statefile;
		 host_ha_stop_daemon;
		 host_ha_release_resources;
		 host_ha_xapi_healthcheck;
		 host_local_assert_healthy;
		 host_request_backup;
		 host_request_config_file_sync;
		 host_propose_new_master; host_commit_new_master; host_abort_new_master;
		 host_get_data_sources;
		 host_record_data_source;
		 host_query_data_source;
		 host_forget_data_source_archives;
		 host_assert_can_evacuate;
		 host_get_vms_which_prevent_evacuation;
		 host_get_uncooperative_resident_VMs;
		 host_get_uncooperative_domains;
		 host_evacuate;
		 host_signal_networking_change;
		 host_notify;
		 host_syslog_reconfigure;
		 host_management_reconfigure;
		 host_local_management_reconfigure;
		 host_management_disable;
		 host_get_management_interface;
		 host_get_system_status_capabilities;
		 host_get_diagnostic_timing_stats;
		 host_restart_agent;
		 host_shutdown_agent;
		 host_set_hostname_live;
		 host_is_in_emergency_mode;
		 host_compute_free_memory;
		 host_compute_memory_overhead;
		 host_tickle_heartbeat;
		 host_sync_data;
		 host_backup_rrds;
		 host_create_new_blob;
		 host_call_plugin;
		 host_get_servertime;
		 host_get_server_localtime;
		 host_enable_binary_storage;
		 host_disable_binary_storage;
		 host_enable_external_auth;
		 host_disable_external_auth;
		 host_retrieve_wlb_evacuate_recommendations;
		 host_certificate_install;
		 host_certificate_uninstall;
		 host_certificate_list;
		 host_crl_install;
		 host_crl_uninstall;
		 host_crl_list;
		 host_certificate_sync;
		 host_get_server_certificate;
		 host_update_pool_secret;
		 host_update_master;
		 host_attach_static_vdis;
		 host_detach_static_vdis;
		 host_set_localdb_key;
		 host_apply_edition;
		 host_refresh_pack_info;
		 host_set_power_on_mode;
		 host_set_cpu_features;
		 host_reset_cpu_features;
		 host_reset_networking;
		 host_enable_local_storage_caching;
		 host_disable_local_storage_caching;
		 host_get_sm_diagnostics;
		 host_get_thread_diagnostics;
		 host_sm_dp_destroy;
		 host_sync_vlans;
		 host_sync_tunnels;
		 host_sync_pif_currently_attached;
		 host_migrate_receive;
		 host_declare_dead;
		 ]
      ~contents:
        ([ uid _host;
	namespace ~name:"name" ~contents:(names None RW) ();
	namespace ~name:"memory" ~contents:host_memory ();
	] @ (allowed_and_current_operations host_operations) @ [
	namespace ~name:"API_version" ~contents:api_version ();
	field ~qualifier:DynamicRO ~ty:Bool "enabled" "True if the host is currently enabled";
	field ~qualifier:StaticRO ~ty:(Map(String, String)) "software_version" "version strings";
	field ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
	field ~qualifier:StaticRO ~ty:(Set(String)) "capabilities" "Xen capabilities";
	field ~qualifier:DynamicRO ~ty:(Map(String, String)) "cpu_configuration" "The CPU configuration on this host.  May contain keys such as \"nr_nodes\", \"sockets_per_node\", \"cores_per_socket\", or \"threads_per_core\"";
	field ~qualifier:DynamicRO ~ty:String "sched_policy" "Scheduler policy currently in force on this host";
	field ~qualifier:DynamicRO ~ty:(Set String) "supported_bootloaders" "a list of the bootloaders installed on the machine";
	field ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) "resident_VMs" "list of VMs currently resident on host";
	field ~qualifier:RW ~ty:(Map(String, String)) "logging" "logging configuration";
	field ~qualifier:DynamicRO ~ty:(Set (Ref _pif)) "PIFs" "physical network interfaces";
	field ~qualifier:RW ~ty:(Ref _sr) "suspend_image_sr" "The SR in which VDIs for suspend images are created";
	field ~qualifier:RW ~ty:(Ref _sr) "crash_dump_sr" "The SR in which VDIs for crash dumps are created";
	field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set (Ref _host_crashdump)) "crashdumps" "Set of host crash dumps";
	field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set (Ref _host_patch)) "patches" "Set of host patches";
	field ~qualifier:DynamicRO ~ty:(Set (Ref _pbd)) "PBDs" "physical blockdevices";
	field ~qualifier:DynamicRO ~ty:(Set (Ref _hostcpu)) "host_CPUs" "The physical CPUs on this host";
	field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "cpu_info" "Details about the physical CPUs on this host";
	field ~in_oss_since:None ~qualifier:RW ~ty:String "hostname" "The hostname of this host";
	field ~in_oss_since:None ~qualifier:RW ~ty:String "address" "The address by which this host can be contacted from any other host in the pool";
	field ~qualifier:DynamicRO ~ty:(Ref _host_metrics) "metrics" "metrics associated with this host";
	field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map (String,String)) "license_params" "State of the current license";
	field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Int "boot_free_mem" "Free memory on host at boot time";
	field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_statefiles" "The set of statefiles accessible from this host";
	field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_network_peers" "The set of hosts visible via the network from this host";
	field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String,Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this host";
	field ~writer_roles:_R_VM_OP ~qualifier:RW ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
	field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VString "")) ~ty:String "external_auth_type" "type of external authentication service configured; empty if none configured.";
	field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VString "")) ~ty:String "external_auth_service_name" "name of external authentication service configured; empty if none configured.";
	field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VMap [])) ~ty:(Map (String,String)) "external_auth_configuration" "configuration specific to external authentication service";
	field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~ty:String "edition" "XenServer edition";
	field ~qualifier:RW ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [VString "address", VString "localhost"; VString "port", VString "27000"])) ~ty:(Map (String, String)) "license_server" "Contact information of the license server";
    field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map (String,String)) "bios_strings" "BIOS strings";
	field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~ty:String "power_on_mode" "The power on mode";  
	field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "power_on_config" "The power on config";
	field ~qualifier:StaticRO ~in_product_since:rel_cowley ~default_value:(Some (VRef (Ref.string_of Ref.null))) ~ty:(Ref _sr) "local_cache_sr" "The SR that is used as a local cache";
	field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Map (String, String)) ~default_value:(Some (VMap []))
		"chipset_info" "Information about chipset features";
	field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _pci)) "PCIs" "List of PCI devices in the host";
	field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _pgpu)) "PGPUs" "List of physical GPUs in the host";
	field ~qualifier:RW ~in_product_since:rel_tampa ~default_value:(Some (VMap [])) ~ty:(Map (String, String)) "guest_VCPUs_params" "VCPUs params to apply to all resident guests";
 ])
	()

let host_metrics = 
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_host_metrics ~descr:"The metrics associated with a host" ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[] ~contents:
      [ uid _host_metrics;
	namespace ~name:"memory" ~contents:host_metrics_memory ();
	field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None "live" "Pool master thinks this host is live";
	field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
	field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
	()

(** HostCPU *)

let hostcpu =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_hostcpu ~descr:"A physical CPU" ~gen_events:true
      ~lifecycle:[
        Published, rel_rio, "A physical CPU";
        Deprecated, rel_midnight_ride, "Deprecated in favour of the Host.cpu_info field";
      ]
      ~doccomments:[] 
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[] ~contents:
      [ uid _hostcpu;
	field ~qualifier:DynamicRO ~ty:(Ref _host) "host" "the host the CPU is in";
	field ~qualifier:DynamicRO ~ty:Int "number" "the number of the physical CPU within the host";
	field ~qualifier:DynamicRO ~ty:String "vendor" "the vendor of the physical CPU";
	field ~qualifier:DynamicRO ~ty:Int "speed" "the speed of the physical CPU";
	field ~qualifier:DynamicRO ~ty:String "modelname" "the model name of the physical CPU";
	field ~qualifier:DynamicRO ~ty:Int "family" "the family (number) of the physical CPU";
	field ~qualifier:DynamicRO ~ty:Int "model" "the model number of the physical CPU";
	field ~qualifier:DynamicRO ~ty:String "stepping" "the stepping of the physical CPU";
	field ~qualifier:DynamicRO ~ty:String "flags" "the flags of the physical CPU (a decoded version of the features field)";
	field ~qualifier:DynamicRO ~ty:String "features" "the physical CPU feature bitmap";
	field ~qualifier:DynamicRO ~persist:false ~ty:Float "utilisation" "the current CPU utilisation";
	field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
]
	()

(** Disk and network interfaces are associated with QoS parameters: *)
let qos devtype =
  [ field  "algorithm_type" "QoS algorithm to use";
    field  ~ty:(Map(String,String)) "algorithm_params" 
      "parameters for chosen QoS algorithm";
    field ~qualifier:DynamicRO  ~ty:(Set String) "supported_algorithms"
      ("supported QoS algorithms for this " ^ devtype);
  ]

let network_operations =
  Enum ("network_operations", 
	[ "attaching", "Indicates this network is attaching to a VIF or PIF" ])

let network_default_locking_mode =
	Enum ("network_default_locking_mode", [
		"unlocked", "Treat all VIFs on this network with locking_mode = 'default' as if they have locking_mode = 'unlocked'";
		"disabled", "Treat all VIFs on this network with locking_mode = 'default' as if they have locking_mode = 'disabled'";
	])


let network_attach = call
  ~name:"attach"
  ~doc:"Makes the network immediately available on a particular host"
  ~params:[Ref _network, "network", "network to which this interface should be connected";
	   Ref _host, "host", "physical machine to which this PIF is connected"]
  ~in_product_since:rel_miami  
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_OP
  ()

let network_introduce_params first_rel =
  [
    {param_type=String; param_name="name_label"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="name_description"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Int; param_name="MTU"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Map(String,String); param_name="other_config"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="bridge"; param_doc=""; param_release=first_rel; param_default=None};
  ]

(* network pool introduce is used to copy network records on pool join -- it's the network analogue of VDI/PIF.pool_introduce *)
let network_pool_introduce = call
  ~name:"pool_introduce"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~versioned_params:(network_introduce_params miami_release)
  ~doc:"Create a new network record in the database only"
  ~result:(Ref _network, "The ref of the newly created network record.")
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_OP
    ()

let network_create_new_blob = call
  ~name: "create_new_blob"
  ~in_product_since:rel_orlando
  ~doc:"Create a placeholder for a named binary blob of data that is associated with this pool"
  ~versioned_params:
  [{param_type=Ref _network; param_name="network"; param_doc="The network"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
  {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}
  ]
  ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
  ~allowed_roles:_R_POOL_OP
  ()

let network_set_default_locking_mode = call
	~name:"set_default_locking_mode"
	~in_product_since:rel_tampa
	~doc:"Set the default locking mode for VIFs attached to this network"
	~params:[
		Ref _network, "network", "The network";
		network_default_locking_mode, "value", "The default locking mode for VIFs attached to this network.";
	]
	~allowed_roles:_R_POOL_OP
	()

let network_attach_for_vm = call
	~name:"attach_for_vm"
	~doc:"Attaches all networks needed by a given VM on a particular host"
	~params:[
		Ref _host, "host", "Physical machine to which the networks are to be attached";
		Ref _vm, "vm", "The virtual machine"
	]
	~in_product_since:rel_tampa
	~hide_from_docs:true
	~allowed_roles:_R_POOL_OP
	()

let network_detach_for_vm = call
	~name:"detach_for_vm"
	~doc:"Detaches all networks of a given VM from a particular host"
	~params:[
		Ref _host, "host", "Physical machine from which the networks are to be attached";
		Ref _vm, "vm", "The virtual machine"
	]
	~in_product_since:rel_tampa
	~hide_from_docs:true
	~allowed_roles:_R_POOL_OP
	()

(** A virtual network *)
let network =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_network ~descr:"A virtual network" ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN (* vm admins can create/destroy networks without PIFs *)
      ~messages:[network_attach; network_pool_introduce; network_create_new_blob; network_set_default_locking_mode;
        network_attach_for_vm; network_detach_for_vm]
      ~contents:
      ([
      uid _network;
      namespace ~name:"name" ~contents:(names ~writer_roles:_R_POOL_OP oss_since_303 RW) ();
    ] @ (allowed_and_current_operations ~writer_roles:_R_POOL_OP network_operations) @ [
      field ~qualifier:DynamicRO ~ty:(Set (Ref _vif)) "VIFs" "list of connected vifs";
      field ~qualifier:DynamicRO ~ty:(Set (Ref _pif)) "PIFs" "list of connected pifs";
      field ~qualifier:RW ~ty:Int ~default_value:(Some (VInt 1500L)) ~in_product_since:rel_midnight_ride "MTU" "MTU in octets";
      field ~writer_roles:_R_POOL_OP ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP));("XenCenterCreateInProgress",(_R_VM_OP))];
      field ~in_oss_since:None ~qualifier:DynamicRO "bridge" "name of the bridge corresponding to this network on the local host";
      field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this network";
      field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
      field ~qualifier:DynamicRO ~in_product_since:rel_tampa ~default_value:(Some (VEnum "unlocked")) ~ty:network_default_locking_mode "default_locking_mode" "The network will use this value to determine the behaviour of all VIFs where locking_mode = default";
       ])
     ()

let pif_create_VLAN = call
  ~name:"create_VLAN"
  ~in_product_since:rel_rio
  ~doc:"Create a VLAN interface from an existing physical interface. This call is deprecated: use VLAN.create instead"
  ~lifecycle:[
    Published, rel_rio, "Create a VLAN interface from an existing physical interface";
    Deprecated, rel_miami, "Replaced by VLAN.create";
  ]
  ~params:[String, "device", "physical interface on which to create the VLAN interface";
	   Ref _network, "network", "network to which this interface should be connected";
	   Ref _host, "host", "physical machine to which this PIF is connected";
	   Int, "VLAN", "VLAN tag for the new interface"]
  ~result:(Ref _pif, "The reference of the created PIF object")
  ~errs:[Api_errors.vlan_tag_invalid]
  ~internal_deprecated_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let pif_destroy = call
  ~name:"destroy"
  ~in_product_since:rel_rio
  ~doc:"Destroy the PIF object (provided it is a VLAN interface). This call is deprecated: use VLAN.destroy or Bond.destroy instead"
  ~lifecycle:[
    Published, rel_rio, "Destroy the PIF object (provided it is a VLAN interface)";
    Deprecated, rel_miami, "Replaced by VLAN.destroy and Bond.destroy";
  ]
  ~params:[Ref _pif, "self", "the PIF object to destroy"]
  ~errs:[Api_errors.pif_is_physical]
  ~internal_deprecated_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let pif_plug = call
  ~name:"plug"
  ~doc:"Attempt to bring up a physical interface"
  ~params:[Ref _pif, "self", "the PIF object to plug"]
  ~in_product_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ~errs:[Api_errors.transport_pif_not_configured]
  ()

let pif_unplug = call
  ~name:"unplug"
  ~doc:"Attempt to bring down a physical interface"
  ~params:[Ref _pif, "self", "the PIF object to unplug"]
  ~in_product_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let pif_ip_configuration_mode = Enum ("ip_configuration_mode",
				      [ "None", "Do not acquire an IP address";
					"DHCP", "Acquire an IP address by DHCP";
					"Static", "Static IP address configuration" ])

let pif_reconfigure_ip = call
  ~name:"reconfigure_ip"
  ~doc:"Reconfigure the IP address settings for this interface"
  ~params:[Ref _pif, "self", "the PIF object to reconfigure";
	   pif_ip_configuration_mode, "mode", "whether to use dynamic/static/no-assignment";
	   String, "IP", "the new IP address";
	   String, "netmask", "the new netmask";
	   String, "gateway", "the new gateway";
	   String, "DNS", "the new DNS settings";
	  ]
  ~in_product_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let pif_ipv6_configuration_mode = Enum ("ipv6_configuration_mode",
				      [ "None", "Do not acquire an IPv6 address";
					"DHCP", "Acquire an IPv6 address by DHCP";
					"Static", "Static IPv6 address configuration";
				        "Autoconf", "Router assigned prefix delegation IPv6 allocation" ])

let pif_reconfigure_ipv6 = call
  ~name:"reconfigure_ipv6"
  ~doc:"Reconfigure the IPv6 address settings for this interface"
  ~params:[Ref _pif, "self", "the PIF object to reconfigure";
	   pif_ipv6_configuration_mode, "mode", "whether to use dynamic/static/no-assignment";
	   String, "IPv6", "the new IPv6 address (in <addr>/<prefix length> format)";
	   String, "gateway", "the new gateway";
	   String, "DNS", "the new DNS settings";
	  ]
  ~lifecycle:[]
  ~allowed_roles:_R_POOL_OP
  ()

let pif_primary_address_type = Enum ("primary_address_type",
				      [ "IPv4", "Primary address is the IPv4 address";
					"IPv6", "Primary address is the IPv6 address" ])

let pif_set_primary_address_type = call
  ~name:"set_primary_address_type"
  ~doc:"Change the primary address type used by this PIF"
  ~params:[Ref _pif, "self", "the PIF object to reconfigure";
	   pif_primary_address_type, "primary_address_type", "Whether to prefer IPv4 or IPv6 connections";
	  ]
  ~lifecycle:[]
  ~allowed_roles:_R_POOL_OP
  ()

let pif_scan = call
  ~name:"scan"
  ~doc:"Scan for physical interfaces on a host and create PIF objects to represent them"
  ~params:[Ref _host, "host", "The host on which to scan"]
  ~in_product_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let pif_introduce = call
  ~name:"introduce"
  ~doc:"Create a PIF object matching a particular network interface"
  ~params:[Ref _host, "host", "The host on which the interface exists";
	   String, "MAC", "The MAC address of the interface";
	   String, "device", "The device name to use for the interface";
	  ]
  ~in_product_since:rel_miami
  ~result:(Ref _pif, "The reference of the created PIF object")
  ~allowed_roles:_R_POOL_OP
  ()

let pif_forget = call
  ~name:"forget"
  ~doc:"Destroy the PIF object matching a particular network interface"
  ~params:[Ref _pif, "self", "The PIF object to destroy"]
  ~in_product_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ~errs:[Api_errors.pif_tunnel_still_exists]
  ()

let pif_introduce_params first_rel =
  [
    {param_type=String; param_name="device"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Ref _network; param_name="network"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Ref _host; param_name="host"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="MAC"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Int; param_name="MTU"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Int; param_name="VLAN"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="physical"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=pif_ip_configuration_mode; param_name="ip_configuration_mode"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="IP"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="netmask"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="gateway"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=String; param_name="DNS"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Ref _bond; param_name="bond_slave_of"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Ref _vlan; param_name="VLAN_master_of"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="management"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Map(String, String); param_name="other_config"; param_doc=""; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="disallow_unplug"; param_doc=""; param_release=orlando_release; param_default=Some (VBool false)};
    {param_type=pif_ipv6_configuration_mode; param_name="ipv6_configuration_mode"; param_doc=""; param_release=boston_release; param_default=Some (VEnum "None")};
    {param_type=(Set(String)); param_name="IPv6"; param_doc=""; param_release=boston_release; param_default=Some (VSet [])};
    {param_type=String; param_name="ipv6_gateway"; param_doc=""; param_release=boston_release; param_default=Some (VString "")};
    {param_type=pif_primary_address_type; param_name="primary_address_type"; param_doc=""; param_release=boston_release; param_default=Some (VEnum "IPv4")}
  ]

(* PIF pool introduce is used to copy PIF records on pool join -- it's the PIF analogue of VDI.pool_introduce *)
let pif_pool_introduce = call
  ~name:"pool_introduce"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~versioned_params:(pif_introduce_params miami_release)
  ~doc:"Create a new PIF record in the database only"
  ~result:(Ref _pif, "The ref of the newly created PIF record.")
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_OP
    ()

let pif_db_introduce = call
  ~name:"db_introduce"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~versioned_params:(pif_introduce_params orlando_release)
  ~doc:"Create a new PIF record in the database only"
  ~result:(Ref _pif, "The ref of the newly created PIF record.")
  ~hide_from_docs:false
  ~allowed_roles:_R_POOL_OP
  ()

let pif_db_forget = call
  ~name:"db_forget"
  ~in_oss_since:None
  ~in_product_since:rel_orlando
  ~params:[ Ref _pif, "self", "The ref of the PIF whose database record should be destroyed" ]
  ~doc:"Destroy a PIF database record."
  ~hide_from_docs:false
  ~allowed_roles:_R_POOL_OP
  ()

let pif = 
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_pif ~descr:"A physical network interface (note separate VLANs are represented as several PIFs)"
      ~gen_events:true
      ~doccomments:[] 
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[pif_create_VLAN; pif_destroy; pif_reconfigure_ip; pif_reconfigure_ipv6; pif_set_primary_address_type; pif_scan; pif_introduce; pif_forget;
		pif_unplug; pif_plug; pif_pool_introduce;
		pif_db_introduce; pif_db_forget
		] ~contents:
      [ uid _pif;
	(* qualifier changed RW -> StaticRO in Miami *)
	field ~qualifier:StaticRO "device" "machine-readable name of the interface (e.g. eth0)";
	field ~qualifier:StaticRO ~ty:(Ref _network) "network" "virtual network to which this pif is connected";
	field ~qualifier:StaticRO ~ty:(Ref _host) "host" "physical machine to which this pif is connected";
	(* qualifier changed RW -> StaticRO in Miami *)
	field ~qualifier:StaticRO "MAC" "ethernet MAC address of physical interface";
	(* qualifier changed RW -> StaticRO in Miami *)
	field ~qualifier:StaticRO ~ty:Int "MTU" "MTU in octets";
	(* qualifier changed RW -> StaticRO in Miami *)
	field ~qualifier:StaticRO ~ty:Int "VLAN" "VLAN tag for all traffic passing through this interface";
	field ~in_oss_since:None ~internal_only:true "device_name" "actual dom0 device name";
	field ~qualifier:DynamicRO ~ty:(Ref _pif_metrics) "metrics" "metrics associated with this PIF";
	field ~in_oss_since:None ~ty:Bool ~in_product_since:rel_miami ~qualifier:DynamicRO "physical" "true if this represents a physical network interface" ~default_value:(Some (VBool false));
	field ~in_oss_since:None ~ty:Bool ~in_product_since:rel_miami ~qualifier:DynamicRO "currently_attached" "true if this interface is online" ~default_value:(Some (VBool true));
	field ~in_oss_since:None ~ty:pif_ip_configuration_mode ~in_product_since:rel_miami ~qualifier:DynamicRO "ip_configuration_mode" "Sets if and how this interface gets an IP address" ~default_value:(Some (VEnum "None"));
	field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "IP" "IP address" ~default_value:(Some (VString ""));
	field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "netmask" "IP netmask" ~default_value:(Some (VString ""));
	field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "gateway" "IP gateway" ~default_value:(Some (VString ""));
	field ~in_oss_since:None ~ty:String ~in_product_since:rel_miami ~qualifier:DynamicRO "DNS" "IP address of DNS servers to use" ~default_value:(Some (VString ""));
	field ~in_oss_since:None ~ty:(Ref _bond) ~in_product_since:rel_miami ~qualifier:DynamicRO "bond_slave_of" "Indicates which bond this interface is part of" ~default_value:(Some (VRef ""));
	field ~in_oss_since:None ~ty:(Set(Ref _bond)) ~in_product_since:rel_miami ~qualifier:DynamicRO "bond_master_of" "Indicates this PIF represents the results of a bond";	
	field ~in_oss_since:None ~ty:(Ref _vlan) ~in_product_since:rel_miami ~qualifier:DynamicRO "VLAN_master_of" "Indicates wich VLAN this interface receives untagged traffic from" ~default_value:(Some (VRef ""));
	field ~in_oss_since:None ~ty:(Set(Ref _vlan)) ~in_product_since:rel_miami ~qualifier:DynamicRO "VLAN_slave_of" "Indicates which VLANs this interface transmits tagged traffic to";
	field ~in_oss_since:None ~ty:Bool ~in_product_since:rel_miami ~qualifier:DynamicRO "management" "Indicates whether the control software is listening for connections on this interface" ~default_value:(Some (VBool false));
	field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "Additional configuration";
	field ~in_product_since:rel_orlando ~default_value:(Some (VBool false)) ~ty:Bool "disallow_unplug" "Prevent this PIF from being unplugged; set this to notify the management tool-stack that the PIF has a special use and should not be unplugged under any circumstances (e.g. because you're running storage traffic over it)";
	field ~in_oss_since:None ~ty:(Set(Ref _tunnel)) ~lifecycle:[Published, rel_cowley, "Indicates to which tunnel this PIF gives access"] ~qualifier:DynamicRO "tunnel_access_PIF_of" "Indicates to which tunnel this PIF gives access";
	field ~in_oss_since:None ~ty:(Set(Ref _tunnel)) ~lifecycle:[Published, rel_cowley, "Indicates to which tunnel this PIF provides transport"] ~qualifier:DynamicRO "tunnel_transport_PIF_of" "Indicates to which tunnel this PIF provides transport";
	field ~in_oss_since:None ~ty:pif_ipv6_configuration_mode ~lifecycle:[] ~qualifier:DynamicRO "ipv6_configuration_mode" "Sets if and how this interface gets an IPv6 address" ~default_value:(Some (VEnum "None"));
	field ~in_oss_since:None ~ty:(Set(String)) ~lifecycle:[] ~qualifier:DynamicRO "IPv6" "IPv6 address" ~default_value:(Some (VSet []));
	field ~in_oss_since:None ~ty:String ~lifecycle:[] ~qualifier:DynamicRO "ipv6_gateway" "IPv6 gateway" ~default_value:(Some (VString ""));
	field ~in_oss_since:None ~ty:pif_primary_address_type ~lifecycle:[] ~qualifier:DynamicRO "primary_address_type" "Which protocol should define the primary address of this interface" ~default_value:(Some (VEnum "IPv4"));
      ]
	()

let pif_metrics = 
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_pif_metrics ~descr:"The metrics associated with a physical network interface"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[] ~contents:
      [ uid _pif_metrics;
	namespace ~name:"io" ~contents:iobandwidth ();
	field ~qualifier:DynamicRO ~ty:Bool "carrier" "Report if the PIF got a carrier or not";
	field ~qualifier:DynamicRO ~ty:String "vendor_id" "Report vendor ID";
	field ~qualifier:DynamicRO ~ty:String "vendor_name" "Report vendor name";
	field ~qualifier:DynamicRO ~ty:String "device_id" "Report device ID";
	field ~qualifier:DynamicRO ~ty:String "device_name" "Report device name";
	field ~qualifier:DynamicRO ~ty:Int "speed" "Speed of the link (if available)";
	field ~qualifier:DynamicRO ~ty:Bool "duplex" "Full duplex capability of the link (if available)";
	field ~qualifier:DynamicRO ~ty:String "pci_bus_path" "PCI bus path of the pif (if available)";
	field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
	field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
	()


let bond_mode =
	Enum ("bond_mode", [
		"balance-slb", "Source-level balancing";
		"active-backup", "Active/passive bonding: only one NIC is carrying traffic";
		"lacp", "Link aggregation control protocol";
	])

let bond_create = call
  ~name:"create"
  ~doc:"Create an interface bond"
  ~versioned_params:[
    {param_type=Ref _network; param_name="network"; param_doc="Network to add the bonded PIF to"; param_release=miami_release; param_default=None};
    {param_type=Set (Ref _pif); param_name="members"; param_doc="PIFs to add to this bond"; param_release=miami_release; param_default=None};
    {param_type=String; param_name="MAC"; param_doc="The MAC address to use on the bond itself. If this parameter is the empty string then the bond will inherit its MAC address from the primary slave."; param_release=miami_release; param_default=None};
    {param_type=bond_mode; param_name="mode"; param_doc="Bonding mode to use for the new bond"; param_release=boston_release; param_default=Some (VEnum "balance-slb")};
    {param_type=Map (String, String); param_name="properties"; param_doc="Additional configuration parameters specific to the bond mode"; param_release=tampa_release; param_default=Some (VMap [])};
  ]
  ~result:(Ref _bond, "The reference of the created Bond object")
  ~in_product_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let bond_destroy = call
  ~name:"destroy"
  ~doc:"Destroy an interface bond"
  ~params:[Ref _bond, "self", "Bond to destroy"]
  ~in_product_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let bond_set_mode = call
	~name:"set_mode"
	~doc:"Change the bond mode"
	~params:[
		Ref _bond, "self", "The bond";
		bond_mode, "value", "The new bond mode";
	]
	~lifecycle:[Published, rel_boston, ""]
	~allowed_roles:_R_POOL_OP
	()

let bond_set_property = call
	~name:"set_property"
	~doc:"Set the value of a property of the bond"
	~params:[
		Ref _bond, "self", "The bond";
		String, "name", "The property name";
		String, "value", "The property value";
	]
	~in_product_since:rel_tampa
	~allowed_roles:_R_POOL_OP
	()

let bond = 
  create_obj ~in_db:true ~in_product_since:rel_miami ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_bond ~descr:"" ~gen_events:true ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[ bond_create; bond_destroy; bond_set_mode; bond_set_property ]
    ~contents:
    [ uid _bond;
      field ~in_oss_since:None ~in_product_since:rel_miami ~qualifier:StaticRO ~ty:(Ref _pif) "master" "The bonded interface" ~default_value:(Some (VRef ""));
      field ~in_oss_since:None ~in_product_since:rel_miami ~qualifier:DynamicRO ~ty:(Set(Ref _pif)) "slaves" "The interfaces which are part of this bond";
      field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      field ~lifecycle:[Published, rel_boston, ""] ~qualifier:DynamicRO ~default_value:(Some (VRef (Ref.string_of Ref.null))) ~ty:(Ref _pif) "primary_slave" "The PIF of which the IP configuration and MAC were copied to the bond, and which will receive all configuration/VLANs/VIFs on the bond if the bond is destroyed";
      field ~lifecycle:[Published, rel_boston, ""] ~qualifier:DynamicRO ~default_value:(Some (VEnum "balance-slb")) ~ty:bond_mode "mode" "The algorithm used to distribute traffic among the bonded NICs";
      field ~in_oss_since:None ~in_product_since:rel_tampa ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "properties" "Additional configuration properties specific to the bond mode.";
	  field ~in_oss_since:None ~in_product_since:rel_tampa ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L)) "links_up" "Number of links up in this bond";
    ]
    ()

let vlan_create = call
  ~name:"create"
  ~doc:"Create a VLAN mux/demuxer"
  ~params:[ Ref _pif, "tagged_PIF", "PIF which receives the tagged traffic";
	    Int, "tag", "VLAN tag to use";
	    Ref _network, "network", "Network to receive the untagged traffic" ]
  ~result:(Ref _vlan, "The reference of the created VLAN object")
  ~in_product_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let vlan_destroy = call
  ~name:"destroy"
  ~doc:"Destroy a VLAN mux/demuxer"
  ~params:[Ref _vlan, "self", "VLAN mux/demuxer to destroy"]
  ~in_product_since:rel_miami
  ~allowed_roles:_R_POOL_OP
  ()

let vlan = 
  create_obj ~in_db:true ~in_product_since:rel_miami ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_vlan ~descr:"A VLAN mux/demux" ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[ vlan_create; vlan_destroy ] ~contents:
    ([
       uid _vlan;
       field ~qualifier:StaticRO ~ty:(Ref _pif) ~in_product_since:rel_miami "tagged_PIF" "interface on which traffic is tagged" ~default_value:(Some (VRef ""));
       field ~qualifier:DynamicRO ~ty:(Ref _pif) ~in_product_since:rel_miami "untagged_PIF" "interface on which traffic is untagged" ~default_value:(Some (VRef ""));
       field ~qualifier:StaticRO ~ty:Int ~in_product_since:rel_miami "tag" "VLAN tag in use" ~default_value:(Some (VInt (-1L)));
       field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";	  
     ])
    ()
    
let tunnel_create = call
	~name:"create"
	~doc:"Create a tunnel"
	~params:[ Ref _pif, "transport_PIF", "PIF which receives the tagged traffic";
		Ref _network, "network", "Network to receive the tunnelled traffic" ]
	~result:(Ref _tunnel, "The reference of the created tunnel object")
	~lifecycle:[Published, rel_cowley, "Create a tunnel"]
	~allowed_roles:_R_POOL_OP
	~errs:[Api_errors.openvswitch_not_active; Api_errors.transport_pif_not_configured; Api_errors.is_tunnel_access_pif]
	()

let tunnel_destroy = call
	~name:"destroy"
	~doc:"Destroy a tunnel"
	~params:[Ref _tunnel, "self", "tunnel to destroy"]
	~lifecycle:[Published, rel_cowley, "Destroy a tunnel"]
	~allowed_roles:_R_POOL_OP
	()

let tunnel = 
	create_obj ~in_db:true ~lifecycle:[Published, rel_cowley, "A tunnel for network traffic"] ~in_oss_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_tunnel ~descr:"A tunnel for network traffic" ~gen_events:true
	~doccomments:[]
	~messages_default_allowed_roles:_R_POOL_OP
	~messages:[ tunnel_create; tunnel_destroy ]
	~contents:([
		uid _tunnel ~lifecycle:[Published, rel_cowley, "Unique identifier/object reference"];
		field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle:[Published, rel_cowley, "The interface through which the tunnel is accessed"] "access_PIF" "The interface through which the tunnel is accessed" ~default_value:(Some (VRef ""));
		field ~qualifier:StaticRO ~ty:(Ref _pif) ~lifecycle:[Published, rel_cowley, "The interface used by the tunnel"] "transport_PIF" "The interface used by the tunnel" ~default_value:(Some (VRef ""));
		field ~ty:(Map(String, String)) ~lifecycle:[Published, rel_cowley, "Status information about the tunnel"] "status" "Status information about the tunnel" ~default_value:(Some (VMap [VString "active", VString "false"]));
		field ~lifecycle:[Published, rel_cowley, "Additional configuration"] ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "Additional configuration";
	])
	()

let pbd_set_device_config = call
   ~name:"set_device_config"
   ~in_oss_since:None
   ~in_product_since:rel_miami
   ~params:[Ref _pbd, "self", "The PBD to modify";
	    Map(String, String), "value", "The new value of the PBD's device_config"]
   ~doc:"Sets the PBD's device_config field"
  ~allowed_roles:_R_POOL_OP
   ()

let pbd =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_pbd ~descr:"The physical block devices through which hosts access SRs"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[ 
      pbd_plug; pbd_unplug;
      pbd_set_device_config
	      ] ~contents:
    [ uid _pbd;
      field ~qualifier:StaticRO ~ty:(Ref _host) "host" "physical machine on which the pbd is available";
      field ~qualifier:StaticRO ~ty:(Ref _sr) "SR" "the storage repository that the pbd realises";
      field ~ty:(Map(String, String)) ~qualifier:StaticRO "device_config" "a config string to string map that is provided to the host's SR-backend-driver";
      field ~ty:Bool ~qualifier:DynamicRO "currently_attached" "is the SR currently attached on this host?";
      field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
    ]
    ()

(* These are included in vbds and vifs -- abstracted here to keep both these uses consistent *)
let device_status_fields =
  [
    field ~ty:Bool ~qualifier:DynamicRO "currently_attached" "is the device currently attached (erased on reboot)";
    field ~ty:Int ~qualifier:DynamicRO "status_code" "error/success code associated with last attach-operation (erased on reboot)";
    field ~ty:String ~qualifier:DynamicRO "status_detail" "error/success information associated with last attach-operation status (erased on reboot)";
    field ~ty:(Map(String, String)) ~qualifier:DynamicRO "runtime_properties" "Device runtime properties"
  ]

(* VIF messages *)

let vif_plug = call
  ~name:"plug"
  ~in_product_since:rel_rio
  ~doc:"Hotplug the specified VIF, dynamically attaching it to the running VM"
  ~params:[Ref _vif, "self", "The VIF to hotplug"]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vif_unplug = call
  ~name:"unplug"
  ~in_product_since:rel_rio
  ~doc:"Hot-unplug the specified VIF, dynamically unattaching it from the running VM"
  ~params:[Ref _vif, "self", "The VIF to hot-unplug"]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vif_unplug_force = call
  ~name:"unplug_force"
  ~in_product_since:rel_boston
  ~doc:"Forcibly unplug the specified VIF"
  ~params:[Ref _vif, "self", "The VIF to forcibly unplug"]
  ~allowed_roles:_R_VM_ADMIN
  ()

let vif_operations =
  Enum ("vif_operations", 
	[ "attach", "Attempting to attach this VIF to a VM";
	  "plug", "Attempting to hotplug this VIF";
	  "unplug", "Attempting to hot unplug this VIF";
	])

let vif_locking_mode =
	Enum ("vif_locking_mode", [
		"network_default", "No specific configuration set - default network policy applies";
		"locked", "Only traffic to a specific MAC and a list of IPv4 or IPv6 addresses is permitted";
		"unlocked", "All traffic is permitted";
		"disabled", "No traffic is permitted";
	])

let vif_set_locking_mode = call
	~name:"set_locking_mode"
	~in_product_since:rel_tampa
	~doc:"Set the locking mode for this VIF"
	~params:[
		Ref _vif, "self", "The VIF whose locking mode will be set";
		vif_locking_mode, "value", "The new locking mode for the VIF";
	]
	~allowed_roles:_R_POOL_OP
	()

let vif_set_ipv4_allowed = call
	~name:"set_ipv4_allowed"
	~in_product_since:rel_tampa
	~doc:"Set the IPv4 addresses to which traffic on this VIF can be restricted"
	~params:[
		Ref _vif, "self", "The VIF which the IP addresses will be associated with";
		Set String, "value", "The IP addresses which will be associated with the VIF";
	]
	~allowed_roles:_R_POOL_OP
	()

let vif_add_ipv4_allowed = call
	~name:"add_ipv4_allowed"
	~in_product_since:rel_tampa
	~doc:"Associates an IPv4 address with this VIF"
	~params:[
		Ref _vif, "self", "The VIF which the IP address will be associated with";
		String, "value", "The IP address which will be associated with the VIF";
	]
	~allowed_roles:_R_POOL_OP
	()

let vif_remove_ipv4_allowed = call
	~name:"remove_ipv4_allowed"
	~in_product_since:rel_tampa
	~doc:"Removes an IPv4 address from this VIF"
	~params:[
		Ref _vif, "self", "The VIF from which the IP address will be removed";
		String, "value", "The IP address which will be removed from the VIF";
	]
	~allowed_roles:_R_POOL_OP
	()

let vif_set_ipv6_allowed = call
	~name:"set_ipv6_allowed"
	~in_product_since:rel_tampa
	~doc:"Set the IPv6 addresses to which traffic on this VIF can be restricted"
	~params:[
		Ref _vif, "self", "The VIF which the IP addresses will be associated with";
		Set String, "value", "The IP addresses which will be associated with the VIF";
	]
	~allowed_roles:_R_POOL_OP
	()

let vif_add_ipv6_allowed = call
	~name:"add_ipv6_allowed"
	~in_product_since:rel_tampa
	~doc:"Associates an IPv6 address with this VIF"
	~params:[
		Ref _vif, "self", "The VIF which the IP address will be associated with";
		String, "value", "The IP address which will be associated with the VIF";
	]
	~allowed_roles:_R_POOL_OP
	()

let vif_remove_ipv6_allowed = call
	~name:"remove_ipv6_allowed"
	~in_product_since:rel_tampa
	~doc:"Removes an IPv6 address from this VIF"
	~params:[
		Ref _vif, "self", "The VIF from which the IP address will be removed";
		String, "value", "The IP address which will be removed from the VIF";
	]
	~allowed_roles:_R_POOL_OP
	()

(** A virtual network interface *)
let vif =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vif ~descr:"A virtual network interface"
      ~gen_events:true
      ~doccomments:[] 
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[vif_plug; vif_unplug; vif_unplug_force; vif_set_locking_mode;
        vif_set_ipv4_allowed; vif_add_ipv4_allowed; vif_remove_ipv4_allowed; vif_set_ipv6_allowed; vif_add_ipv6_allowed; vif_remove_ipv6_allowed]
      ~contents:
      ([ uid _vif;
       ] @ (allowed_and_current_operations vif_operations) @ [
	 field ~qualifier:StaticRO "device" "order in which VIF backends are created by xapi";
	 field ~qualifier:StaticRO ~ty:(Ref _network) "network" "virtual network to which this vif is connected";
	 field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "virtual machine to which this vif is connected";
	 field ~qualifier:StaticRO ~ty:String "MAC" "ethernet MAC address of virtual interface, as exposed to guest";
	 field ~qualifier:StaticRO ~ty:Int "MTU" "MTU in octets";
	 field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Bool "reserved" "true if the VIF is reserved pending a reboot/migrate";
	 field ~ty:(Map(String, String)) "other_config" "additional configuration";
       ] @ device_status_fields @
	 [ namespace ~name:"qos" ~contents:(qos "VIF") (); ] @
	 [ field ~qualifier:DynamicRO ~ty:(Ref _vif_metrics) "metrics" "metrics associated with this VIF";
		 field ~qualifier:DynamicRO ~in_product_since:rel_george ~default_value:(Some (VBool false)) ~ty:Bool "MAC_autogenerated" "true if the MAC was autogenerated; false indicates it was set manually";
		 field ~qualifier:StaticRO ~in_product_since:rel_tampa ~default_value:(Some (VEnum "network_default")) ~ty:vif_locking_mode "locking_mode" "current locking mode of the VIF";
		 field ~qualifier:StaticRO ~in_product_since:rel_tampa ~default_value:(Some (VSet [])) ~ty:(Set (String)) "ipv4_allowed" "A list of IPv4 addresses which can be used to filter traffic passing through this VIF";
		 field ~qualifier:StaticRO ~in_product_since:rel_tampa ~default_value:(Some (VSet [])) ~ty:(Set (String)) "ipv6_allowed" "A list of IPv6 addresses which can be used to filter traffic passing through this VIF";
	 ])
	()

let vif_metrics = 
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_vif_metrics ~descr:"The metrics associated with a virtual network device"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[] ~contents:
      [ uid _vif_metrics;
	namespace ~name:"io" ~contents:iobandwidth ();
	field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
	field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
	()

let data_source =
  create_obj ~in_db:false ~in_product_since:rel_orlando ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_data_source ~descr:"Data sources for logging in RRDs" 
    ~gen_events:false
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~messages:[] ~contents:
    [ namespace ~name:"name" ~contents:(names oss_since_303 DynamicRO) ();
      field ~qualifier:DynamicRO ~ty:Bool "enabled" "true if the data source is being logged";
      field ~qualifier:DynamicRO ~ty:Bool "standard" "true if the data source is enabled by default. Non-default data sources cannot be disabled";
      field ~qualifier:DynamicRO ~ty:String "units" "the units of the value";
      field ~qualifier:DynamicRO ~ty:Float "min" "the minimum value of the data source";
      field ~qualifier:DynamicRO ~ty:Float "max" "the maximum value of the data source";
      field ~qualifier:DynamicRO ~ty:Float "value" "current value of the data source" ]
      
    ()

let storage_operations =
  Enum ("storage_operations", 
	[ "scan", "Scanning backends for new or deleted VDIs";
	  "destroy", "Destroying the SR";
	  "forget", "Forgetting about SR";
	  "plug", "Plugging a PBD into this SR";
	  "unplug", "Unplugging a PBD from this SR";
	  "update", "Refresh the fields on the SR";
	  "vdi_create", "Creating a new VDI";
	  "vdi_introduce", "Introducing a new VDI";
	  "vdi_destroy", "Destroying a VDI";
	  "vdi_resize", "Resizing a VDI"; 
	  "vdi_clone", "Cloneing a VDI"; 
	  "vdi_snapshot", "Snapshotting a VDI";
	  "pbd_create", "Creating a PBD for this SR";
	  "pbd_destroy", "Destroying one of this SR's PBDs"; ])

let sr_set_virtual_allocation = call
   ~name:"set_virtual_allocation"
   ~in_oss_since:None
   ~in_product_since:rel_miami
   ~params:[Ref _sr, "self", "The SR to modify";
 	   Int, "value", "The new value of the SR's virtual_allocation"]
   ~flags:[`Session]
   ~doc:"Sets the SR's virtual_allocation field"
   ~allowed_roles:_R_POOL_OP
   ()

let sr_set_physical_size = call
   ~name:"set_physical_size"
   ~in_oss_since:None
   ~in_product_since:rel_miami
   ~params:[Ref _sr, "self", "The SR to modify";
 	   Int, "value", "The new value of the SR's physical_size"]
   ~flags:[`Session]
   ~doc:"Sets the SR's physical_size field"
   ~allowed_roles:_R_POOL_OP
   ()

let sr_set_physical_utilisation = call
   ~name:"set_physical_utilisation"
   ~in_oss_since:None
   ~in_product_since:rel_miami
   ~flags:[`Session]
   ~params:[Ref _sr, "self", "The SR to modify";
 	   Int, "value", "The new value of the SR's physical utilisation"]
   ~doc:"Sets the SR's physical_utilisation field"
   ~allowed_roles:_R_POOL_OP
   ()

let sr_update = call
   ~name:"update"
   ~in_oss_since:None
   ~in_product_since:rel_symc
   ~params:[Ref _sr, "sr", "The SR whose fields should be refreshed" ]
   ~doc:"Refresh the fields on the SR object"
   ~allowed_roles:_R_POOL_OP
   ()

let sr_assert_can_host_ha_statefile = call
   ~name:"assert_can_host_ha_statefile"
   ~in_oss_since:None
   ~in_product_since:rel_orlando
   ~params:[Ref _sr, "sr", "The SR to query" ]
   ~doc:"Returns successfully if the given SR can host an HA statefile. Otherwise returns an error to explain why not"
   ~allowed_roles:_R_POOL_OP
   ()

let sr_assert_supports_database_replication = call
	~name:"assert_supports_database_replication"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _sr, "sr", "The SR to query"]
	~doc:"Returns successfully if the given SR supports database replication. Otherwise returns an error to explain why not."
	~allowed_roles:_R_POOL_OP
	()

let sr_enable_database_replication = call
	~name:"enable_database_replication"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _sr, "sr", "The SR to which metadata should be replicated"]
	~allowed_roles:_R_POOL_OP
	()

let sr_disable_database_replication = call
	~name:"disable_database_replication"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _sr, "sr", "The SR to which metadata should be no longer replicated"]
	~allowed_roles:_R_POOL_OP
	()

(** A storage repository. Note we overide default create/destroy methods with our own here... *)
let storage_repository =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_sr ~descr:"A storage repository"
      ~gen_events:true
      ~doccomments:[] 
      ~messages_default_allowed_roles:_R_POOL_OP
      ~messages:[ sr_create; sr_introduce; sr_make; sr_destroy; sr_forget;
		  sr_update;
		  sr_get_supported_types; sr_scan; sr_probe; sr_set_shared;
		  sr_set_name_label; sr_set_name_description;
		  sr_create_new_blob;
		  sr_set_physical_size; sr_set_virtual_allocation; sr_set_physical_utilisation;
		  sr_assert_can_host_ha_statefile;
			sr_assert_supports_database_replication;
			sr_enable_database_replication;
			sr_disable_database_replication;

		]
      ~contents:
      ([ uid _sr;
	namespace ~name:"name" ~contents:(names oss_since_303 StaticRO) ();
      ] @ (allowed_and_current_operations storage_operations) @ [
	field ~ty:(Set(Ref _vdi)) ~qualifier:DynamicRO "VDIs" "all virtual disks known to this storage repository";
	field ~qualifier:DynamicRO ~ty:(Set (Ref _pbd)) "PBDs" "describes how particular hosts can see this storage repository";
	field ~ty:Int ~qualifier:DynamicRO "virtual_allocation" "sum of virtual_sizes of all VDIs in this storage repository (in bytes)";
	field ~ty:Int ~qualifier:DynamicRO "physical_utilisation" "physical space currently utilised on this storage repository (in bytes). Note that for sparse disk formats, physical_utilisation may be less than virtual_allocation";
	field ~ty:Int ~qualifier:StaticRO "physical_size" "total physical size of the repository (in bytes)";
	field ~qualifier:StaticRO "type" "type of the storage repository";
	field ~qualifier:StaticRO "content_type" "the type of the SR's content, if required (e.g. ISOs)";
	field ~qualifier:DynamicRO "shared" ~ty:Bool "true if this SR is (capable of being) shared between multiple hosts";
	field ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
	field  ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
 	field ~ty:Bool ~qualifier:DynamicRO ~in_oss_since:None ~internal_only:true "default_vdi_visibility" "";
	field ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "sm_config" "SM dependent data" ~default_value:(Some (VMap []));
	field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this SR";
	field ~qualifier:DynamicRO ~in_product_since:rel_cowley ~ty:Bool ~default_value:(Some (VBool false)) "local_cache_enabled" "True if this SR is assigned to be the local cache for its host";
	field ~qualifier:DynamicRO ~in_product_since:rel_boston ~ty:(Ref _dr_task) ~default_value:(Some (VRef (Ref.string_of Ref.null))) "introduced_by" "The disaster recovery task which introduced this SR";
      ])
	()

(** XXX: just make this a field and be done with it. Cowardly refusing to change the schema for now. *)
let sm_get_driver_filename = call
   ~name:"get_driver_filename"
   ~in_oss_since:None
   ~in_product_since:rel_orlando
   ~params:[Ref _sm, "self", "The SM to query" ]
   ~result:(String, "The SM's driver_filename field")
   ~doc:"Gets the SM's driver_filename field"
   ()  

let storage_plugin = 
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_sm ~descr:"A storage manager plugin"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[ ]
    ~contents:
    ([ uid _sm;
       namespace ~name:"name" ~contents:(names None DynamicRO) ();
       field ~in_oss_since:None ~qualifier:DynamicRO "type" "SR.type";
       field ~in_oss_since:None ~qualifier:DynamicRO "vendor" "Vendor who created this plugin";
       field ~in_oss_since:None ~qualifier:DynamicRO "copyright" "Entity which owns the copyright of this plugin";
       field ~in_oss_since:None ~qualifier:DynamicRO "version" "Version of the plugin";
       field ~in_oss_since:None ~qualifier:DynamicRO "required_api_version" "Minimum SM API version required on the server";
       field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map(String,String)) "configuration" "names and descriptions of device config keys";
       field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_miami ~lifecycle:[ Deprecated, rel_clearwater, "Use SM.features instead"; ] ~ty:(Set(String)) "capabilities" "capabilities of the SM plugin" ~default_value:(Some (VSet []));
       field ~in_oss_since:None ~qualifier:DynamicRO ~in_product_since:rel_clearwater ~ty:(Map(String, Int)) "features" "capabilities of the SM plugin, with capability version numbers" ~default_value:(Some (VMap []));
       field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
       field ~in_product_since:rel_orlando ~qualifier:DynamicRO ~default_value:(Some (VString "")) ~ty:String "driver_filename" "filename of the storage driver";
     ])
    ()

(* --- rws: removed this after talking to Andy and Julian
let filesystem =
  { name = _filesystem; description = "An on-disk filesystem";
    messages = [];
    contents =
      field "uuid" "globally-unique ID" ::
    let field ?(ty=Int) = field ~qualifier:DynamicRO ~ty in
    [ field "block_size" "block size";
      field "total_blocks" "total blocks on disk";
      field "available_blocks" "blocks available for allocation";
      field "used_blocks" "blocks already in use";
      field "percentage_free" "Percentage of free space left in filesystem";
      field ~ty:String "type" "filesystem type" ] }
*)


(** Each disk is associated with a vdi_type: (a 'style' of disk?) *)
let vdi_type = Enum ("vdi_type", [ "system",    "a disk that may be replaced on upgrade"; 
				   "user",      "a disk that is always preserved on upgrade"; 
				   "ephemeral", "a disk that may be reformatted on upgrade";
				   "suspend",   "a disk that stores a suspend image";
				   "crashdump", "a disk that stores VM crashdump information";
				   "ha_statefile", "a disk used for HA storage heartbeating";
				   "metadata", "a disk used for HA Pool metadata";
				   "redo_log", "a disk used for a general metadata redo-log";
				 ])

let vdi_introduce_params first_rel =
  [
    {param_type=String; param_name="uuid"; param_doc="The uuid of the disk to introduce"; param_release=first_rel; param_default=None};
    {param_type=String; param_name="name_label"; param_doc="The name of the disk record"; param_release=first_rel; param_default=None};
    {param_type=String; param_name="name_description"; param_doc="The description of the disk record"; param_release=first_rel; param_default=None};
    {param_type=Ref _sr; param_name="SR"; param_doc="The SR that the VDI is in"; param_release=first_rel; param_default=None};
    {param_type=vdi_type; param_name="type"; param_doc="The type of the VDI"; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="sharable"; param_doc="true if this disk may be shared"; param_release=first_rel; param_default=None};
    {param_type=Bool; param_name="read_only"; param_doc="true if this disk may ONLY be mounted read-only"; param_release=first_rel; param_default=None};
    {param_type=Map(String, String); param_name="other_config"; param_doc="additional configuration"; param_release=first_rel; param_default=None};
    {param_type=String; param_name="location"; param_doc="location information"; param_release=first_rel; param_default=None};
    {param_type=Map(String, String); param_name="xenstore_data"; param_doc="Data to insert into xenstore"; param_release=first_rel; param_default=Some (VMap [])};
    {param_type=Map(String, String); param_name="sm_config"; param_doc="Storage-specific config"; param_release=miami_release; param_default=Some (VMap [])};
	{param_type=Bool; param_name = "managed"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VBool true) };
	{param_type=Int; param_name="virtual_size"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VInt 0L) };
	{param_type=Int; param_name="physical_utilisation"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VInt 0L) };
	{param_type=Ref _pool; param_name="metadata_of_pool"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VRef "") };
	{param_type=Bool; param_name="is_a_snapshot"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VBool false) };
	{param_type=DateTime; param_name="snapshot_time"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VDateTime Date.never) };
	{param_type=Ref _vdi; param_name="snapshot_of"; param_doc = "Storage-specific config"; param_release=tampa_release; param_default = Some (VRef "") };


  ]

(* This used to be called VDI.introduce but it was always an internal call *)
let vdi_pool_introduce = call
  ~name:"pool_introduce"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~versioned_params:(vdi_introduce_params miami_release)
  ~doc:"Create a new VDI record in the database only"
  ~result:(Ref _vdi, "The ref of the newly created VDI record.")
  ~hide_from_docs:true
  ~allowed_roles:_R_VM_ADMIN
    ()

let vdi_db_introduce = { vdi_pool_introduce with msg_name = "db_introduce"; msg_hide_from_docs = false }

let vdi_db_forget = call
  ~name:"db_forget"
  ~in_oss_since:None
  ~params:[Ref _vdi, "vdi", "The VDI to forget about"]
  ~doc:"Removes a VDI record from the database"
  ~in_product_since:rel_miami
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_introduce = call
  ~name:"introduce"
  ~in_oss_since:None
  ~versioned_params:(vdi_introduce_params rio_release)
  ~doc:"Create a new VDI record in the database only"
  ~result:(Ref _vdi, "The ref of the newly created VDI record.")
  ~errs:[Api_errors.sr_operation_not_supported]
  ~in_product_since:rel_miami
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_forget = call
  ~name:"forget"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[Ref _vdi, "vdi", "The VDI to forget about"]
  ~doc:"Removes a VDI record from the database"
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_force_unlock = call
  ~name:"force_unlock"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~internal_deprecated_since:rel_miami
  ~params:[Ref _vdi, "vdi", "The VDI to forcibly unlock"]
  ~doc:"Steals the lock on this VDI and leaves it unlocked. This function is extremely dangerous. This call is deprecated."
  ~hide_from_docs:true
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_update = call
  ~name:"update"
  ~in_oss_since:None
  ~params:[Ref _vdi, "vdi", "The VDI whose stats (eg size) should be updated" ]
  ~doc:"Ask the storage backend to refresh the fields in the VDI object"
  ~errs:[Api_errors.sr_operation_not_supported]
  ~in_product_since:rel_symc
  ~allowed_roles:_R_VM_ADMIN
  ()

let vdi_operations =
  Enum ("vdi_operations", 
	[ "scan", "Scanning backends for new or deleted VDIs";
	  "clone", "Cloning the VDI";
	  "copy", "Copying the VDI";
	  "resize", "Resizing the VDI";
	  "resize_online", "Resizing the VDI which may or may not be online";
	  "snapshot", "Snapshotting the VDI";
	  "destroy", "Destroying the VDI";
	  "forget", "Forget about the VDI";
	  "update", "Refreshing the fields of the VDI";
	  "force_unlock", "Forcibly unlocking the VDI";
	  "generate_config", "Generating static configuration";
	  "blocked", "Operations on this VDI are temporarily blocked";
	])

let vdi_set_missing = call
  ~name:"set_missing"
  ~in_oss_since:None
  ~in_product_since:rel_miami
   ~params:[Ref _vdi, "self", "The VDI to modify";
 	   Bool, "value", "The new value of the VDI's missing field"]
   ~doc:"Sets the VDI's missing field"
  ~flags:[`Session]
  ~allowed_roles:_R_VM_ADMIN
   ()  

let vdi_set_read_only = call
   ~name:"set_read_only"
   ~in_oss_since:None
   ~in_product_since:rel_rio
   ~params:[Ref _vdi, "self", "The VDI to modify";
 	   Bool, "value", "The new value of the VDI's read_only field"]
  ~flags:[`Session]
   ~doc:"Sets the VDI's read_only field"
  ~allowed_roles:_R_VM_ADMIN
   ()

let vdi_set_sharable = call
   ~name:"set_sharable"
   ~in_oss_since:None
   ~in_product_since:rel_george
   ~params:[Ref _vdi, "self", "The VDI to modify";
 	   Bool, "value", "The new value of the VDI's sharable field"]
  ~flags:[`Session]
   ~doc:"Sets the VDI's sharable field"
  ~allowed_roles:_R_VM_ADMIN
   ()

let vdi_set_managed = call
   ~name:"set_managed"
   ~in_oss_since:None
	 ~in_product_since:rel_rio
   ~params:[Ref _vdi, "self", "The VDI to modify";
 	   Bool, "value", "The new value of the VDI's managed field"]
  ~flags:[`Session]
   ~doc:"Sets the VDI's managed field"
  ~allowed_roles:_R_VM_ADMIN
   ()
 
let vdi_set_virtual_size = call
   ~name:"set_virtual_size"
   ~in_oss_since:None
   ~in_product_since:rel_miami
   ~params:[Ref _vdi, "self", "The VDI to modify";
 	   Int, "value", "The new value of the VDI's virtual size"]
  ~flags:[`Session]
   ~doc:"Sets the VDI's virtual_size field"
  ~allowed_roles:_R_VM_ADMIN
   ()

let vdi_set_physical_utilisation = call
   ~name:"set_physical_utilisation"
   ~in_oss_since:None
   ~in_product_since:rel_miami
   ~params:[Ref _vdi, "self", "The VDI to modify";
 	   Int, "value", "The new value of the VDI's physical utilisation"]
  ~flags:[`Session]
   ~doc:"Sets the VDI's physical_utilisation field"
  ~allowed_roles:_R_VM_ADMIN
   ()

let vdi_set_is_a_snapshot = call
	~name:"set_is_a_snapshot"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _vdi, "self", "The VDI to modify";
		Bool, "value", "The new value indicating whether this VDI is a snapshot"]
	~flags:[`Session]
	~doc:"Sets whether this VDI is a snapshot"
	~allowed_roles:_R_VM_ADMIN
	()

let vdi_set_snapshot_of = call
	~name:"set_snapshot_of"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _vdi, "self", "The VDI to modify";
		Ref _vdi, "value", "The VDI of which this VDI is a snapshot"]
	~flags:[`Session]
	~doc:"Sets the VDI of which this VDI is a snapshot"
	~allowed_roles:_R_VM_ADMIN
	()

let vdi_set_snapshot_time = call
	~name:"set_snapshot_time"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _vdi, "self", "The VDI to modify";
		DateTime, "value", "The snapshot time of this VDI."]
	~flags:[`Session]
	~doc:"Sets the snapshot time of this VDI."
	~allowed_roles:_R_VM_ADMIN
	()

let vdi_set_metadata_of_pool = call
	~name:"set_metadata_of_pool"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _vdi, "self", "The VDI to modify";
		Ref _pool, "value", "The pool whose metadata is contained by this VDI"]
	~flags:[`Session]
	~doc:"Records the pool whose metadata is contained by this VDI."
	~allowed_roles:_R_VM_ADMIN
	()

(** An API call for debugging and testing only *)
let vdi_generate_config = call
   ~name:"generate_config"
   ~in_oss_since:None
   ~in_product_since:rel_orlando
   ~params:[Ref _host, "host", "The host on which to generate the configuration";
	    Ref _vdi, "vdi", "The VDI to generate the configuration for" ]
   ~result:(String, "The generated static configuration")
   ~doc:"Internal function for debugging only"
   ~hide_from_docs:true
   ~allowed_roles:_R_VM_ADMIN
   ()

let on_boot = Enum ("on_boot", [
   "reset", "When a VM containing this VDI is started, the contents of the VDI are reset to the state they were in when this flag was last set.";
   "persist", "Standard behaviour.";
   ])

let vdi_set_on_boot = call
	 ~name:"set_on_boot"
	 ~in_oss_since:None
	 ~in_product_since:rel_cowley
	 ~params:[Ref _vdi, "self", "The VDI to modify";
	          on_boot, "value", "The value to set"]
	 ~doc:"Set the value of the on_boot parameter. This value can only be changed when the VDI is not attached to a running VM."
	 ~allowed_roles:_R_VM_ADMIN
	 ()

let vdi_set_allow_caching = call
	~name:"set_allow_caching"
	~in_oss_since:None
	~in_product_since:rel_cowley
	~params:[Ref _vdi, "self", "The VDI to modify";
	         Bool, "value", "The value to set"]
	~doc:"Set the value of the allow_caching parameter. This value can only be changed when the VDI is not attached to a running VM. The caching behaviour is only affected by this flag for VHD-based VDIs that have one parent and no child VHDs. Moreover, caching only takes place when the host running the VM containing this VDI has a nominated SR for local caching."
	~allowed_roles:_R_VM_ADMIN
	()

let vdi_set_name_label = call
	~name:"set_name_label"
	~in_oss_since:None
	~in_product_since:rel_rio
	~params:[Ref _vdi, "self", "The VDI to modify";
	         String, "value", "The name lable for the VDI"]
	~doc:"Set the name label of the VDI. This can only happen when then its SR is currently attached."
	~allowed_roles:_R_VM_ADMIN
	()

let vdi_set_name_description = call
	~name:"set_name_description"
	~in_oss_since:None
	~in_product_since:rel_rio
	~params:[Ref _vdi, "self", "The VDI to modify";
	         String, "value", "The name description for the VDI"]
	~doc:"Set the name description of the VDI. This can only happen when its SR is currently attached."
	~allowed_roles:_R_VM_ADMIN
	()

let vdi_open_database = call
	~name:"open_database"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _vdi, "self", "The VDI which contains the database to open"]
	~result:(Ref _session, "A session which can be used to query the database")
	~doc:"Load the metadata found on the supplied VDI and return a session reference which can be used in XenAPI calls to query its contents."
	~allowed_roles:_R_POOL_OP
	()

let vdi_checksum = call
	~name:"checksum"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _vdi, "self", "The VDI to checksum"]
	~result:(String, "The md5sum of the vdi")
	~doc:"Internal function to calculate VDI checksum and return a string"
	~hide_from_docs:true
	~allowed_roles:_R_VM_ADMIN (* Conceptually, this is not correct. We do it
	                              this way only to follow the previous
	                              convention. It is supposed to fix by future
	                              version of RBAC *)
  ()

let vdi_read_database_pool_uuid = call
	~name:"read_database_pool_uuid"
	~in_oss_since:None
	~in_product_since:rel_boston
	~params:[Ref _vdi, "self", "The metadata VDI to look up in the cache."]
	~result:(String, "The cached pool UUID of the database on the VDI.")
	~doc:"Check the VDI cache for the pool UUID of the database on this VDI."
	~allowed_roles:_R_READ_ONLY
	()

(** A virtual disk *)
let vdi =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vdi ~descr:"A virtual disk image"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[vdi_snapshot; vdi_clone; vdi_resize; 
		 vdi_resize_online;
		 vdi_introduce; vdi_pool_introduce;
		 vdi_db_introduce; vdi_db_forget;
		 vdi_update;
		 vdi_copy;
		 vdi_force_unlock; vdi_set_managed;
		 vdi_forget;
		 vdi_set_sharable;
		 vdi_set_read_only;
		 vdi_set_missing;
		 vdi_set_virtual_size;
		 vdi_set_physical_utilisation;
		 vdi_set_is_a_snapshot;
		 vdi_set_snapshot_of;
		 vdi_set_snapshot_time;
		 vdi_set_metadata_of_pool;
		 vdi_set_name_label;
		 vdi_set_name_description;
		 vdi_generate_config;
		 vdi_set_on_boot;
		 vdi_set_allow_caching;
		 vdi_open_database;
		 vdi_checksum;
		 vdi_read_database_pool_uuid;
		 vdi_pool_migrate;
		]
      ~contents:
      ([ uid _vdi;
	namespace ~name:"name" ~contents:(names oss_since_303 StaticRO) ();
      ] @ (allowed_and_current_operations vdi_operations) @ [
	field ~qualifier:StaticRO ~ty:(Ref _sr) "SR" "storage repository in which the VDI resides";
  field ~qualifier:DynamicRO ~ty:(Set (Ref _vbd)) "VBDs" "list of vbds that refer to this disk";
	field ~qualifier:DynamicRO ~ty:(Set (Ref _crashdump)) "crash_dumps" "list of crash dumps that refer to this disk";
	field ~qualifier:StaticRO ~ty:Int "virtual_size" "size of disk as presented to the guest (in bytes). Note that, depending on storage backend type, requested size may not be respected exactly";
	field ~qualifier:DynamicRO ~ty:Int "physical_utilisation" "amount of physical space that the disk image is currently taking up on the storage repository (in bytes)";
	field ~qualifier:StaticRO ~ty:vdi_type "type" "type of the VDI";
	field ~qualifier:StaticRO ~ty:Bool "sharable" "true if this disk may be shared";
	field ~qualifier:StaticRO ~ty:Bool "read_only" "true if this disk may ONLY be mounted read-only";
	field ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
	field ~qualifier:DynamicRO ~ty:Bool "storage_lock" "true if this disk is locked at the storage level";
	(* XXX: location field was in the database in rio, now API in miami *)
	field ~in_oss_since:None ~in_product_since:rel_miami ~ty:String ~qualifier:DynamicRO ~default_value:(Some (VString "")) "location" "location information";
	field ~in_oss_since:None ~ty:Bool ~qualifier:DynamicRO "managed" "";
	field ~in_oss_since:None ~ty:Bool ~qualifier:DynamicRO "missing" "true if SR scan operation reported this VDI as not present on disk";
	field ~in_oss_since:None ~ty:(Ref _vdi) ~qualifier:DynamicRO "parent" "References the parent disk, if this VDI is part of a chain";
	field ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "xenstore_data" "data to be inserted into the xenstore tree (/local/domain/0/backend/vbd/<domid>/<device-id>/sm-data) after the VDI is attached. This is generally set by the SM backends on vdi_attach." ~default_value:(Some (VMap []));
	field ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "sm_config" "SM dependent data" ~default_value:(Some (VMap []));

	field ~in_product_since:rel_orlando ~default_value:(Some (VBool false))          ~qualifier:DynamicRO ~ty:Bool             "is_a_snapshot" "true if this is a snapshot.";
	field ~in_product_since:rel_orlando ~default_value:(Some (VRef ""))              ~qualifier:DynamicRO ~ty:(Ref _vdi)       "snapshot_of" "Ref pointing to the VDI this snapshot is of.";
	field ~in_product_since:rel_orlando                                              ~qualifier:DynamicRO ~ty:(Set (Ref _vdi)) "snapshots" "List pointing to all the VDIs snapshots.";
	field ~in_product_since:rel_orlando ~default_value:(Some (VDateTime Date.never)) ~qualifier:DynamicRO ~ty:DateTime         "snapshot_time" "Date/time when this snapshot was created.";
	field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
	field ~in_product_since:rel_cowley ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "allow_caching" "true if this VDI is to be cached in the local cache SR";
	field ~in_product_since:rel_cowley ~qualifier:DynamicRO ~ty:on_boot ~default_value:(Some (VEnum "persist")) "on_boot" "The behaviour of this VDI on a VM boot";
	field ~in_product_since:rel_boston ~qualifier:DynamicRO ~ty:(Ref _pool) ~default_value:(Some (VRef (Ref.string_of Ref.null))) "metadata_of_pool" "The pool whose metadata is contained in this VDI";
	field ~in_product_since:rel_boston ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "metadata_latest" "Whether this VDI contains the latest known accessible metadata for the pool";
      ])
	()

(** Virtual disk interfaces have a mode parameter: *)
let vbd_mode = Enum ("vbd_mode", [ "RO", "only read-only access will be allowed";
				   "RW", "read-write access will be allowed" ])

let vbd_type = Enum ("vbd_type",
		     [ "CD", "VBD will appear to guest as CD"; 
		       "Disk", "VBD will appear to guest as disk" ])

let vbd_operations =
  Enum ("vbd_operations", 
	[ "attach", "Attempting to attach this VBD to a VM";
	  "eject", "Attempting to eject the media from this VBD";
	  "insert", "Attempting to insert new media into this VBD";
	  "plug", "Attempting to hotplug this VBD";
	  "unplug", "Attempting to hot unplug this VBD";
	  "unplug_force", "Attempting to forcibly unplug this VBD";
	  "pause", "Attempting to pause a block device backend";
	  "unpause", "Attempting to unpause a block device backend";
	])

(** A virtual disk interface *)
let vbd =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vbd ~descr:"A virtual block device"
    ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages: [ vbd_eject; vbd_insert; vbd_plug; vbd_unplug; vbd_unplug_force; vbd_unplug_force_no_safety_check; vbd_assert_attachable;
		   vbd_pause; vbd_unpause;
		 ]
      ~contents:
    ([ uid _vbd;
     ] @ (allowed_and_current_operations vbd_operations) @ [
       field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine"; 
       field ~qualifier:StaticRO ~ty:(Ref _vdi) "VDI" "the virtual disk";

       field ~qualifier:DynamicRO "device" "device seen by the guest e.g. hda1";
       field "userdevice" "user-friendly device name e.g. 0,1,2,etc.";
       field ~ty:Bool "bootable" "true if this VBD is bootable";
       field ~effect:true ~ty:vbd_mode "mode" "the mode the VBD should be mounted with";
       field ~ty:vbd_type "type" "how the VBD will appear to the guest (e.g. disk or CD)";
       field ~in_oss_since:None ~in_product_since:rel_miami ~ty:Bool ~default_value:(Some (VBool true))
	 "unpluggable" "true if this VBD will support hot-unplug";
       field ~qualifier:DynamicRO ~ty:Bool "storage_lock" "true if a storage level lock was acquired";
       field ~qualifier:StaticRO ~ty:Bool "empty" "if true this represents an empty drive";
       field ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "reserved" "true if the VBD is reserved pending a reboot/migrate";
       field ~ty:(Map(String, String)) "other_config" "additional configuration";
 ]
     @ device_status_fields @
       [ namespace ~name:"qos" ~contents:(qos "VBD") (); ] @
       [ field ~qualifier:DynamicRO ~ty:(Ref _vbd_metrics) "metrics" "metrics associated with this VBD"; ])
	()

let vbd_metrics = 
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_vbd_metrics ~descr:"The metrics associated with a virtual block device"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[] ~contents:
      [ uid _vbd_metrics;
	namespace ~name:"io" ~contents:iobandwidth ();
	field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
	field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      ]
	()

let crashdump_destroy = call
  ~name:"destroy"
  ~in_product_since:rel_rio
  ~doc:"Destroy the specified crashdump"
  ~params:[Ref _crashdump, "self", "The crashdump to destroy"]
  ~allowed_roles:_R_POOL_OP
  ()


(** A crashdump for a particular VM, stored in a particular VDI *)
let crashdump =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_crashdump ~descr:"A VM crashdump"
    ~gen_events:true
    ~doccomments:[] 
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages: [crashdump_destroy]
    ~contents:
    ([ uid _crashdump;
       field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine";
       field ~qualifier:StaticRO ~ty:(Ref _vdi) "VDI" "the virtual disk";
       field ~in_product_since:rel_miami ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";      
     ])
	()

let pool_enable_ha = call
  ~in_product_since:rel_miami
  ~name:"enable_ha"
  ~in_oss_since:None
  ~params:[
    Set(Ref _sr), "heartbeat_srs", "Set of SRs to use for storage heartbeating.";
    Map(String, String), "configuration", "Detailed HA configuration to apply"]
  ~doc:"Turn on High Availability mode"
  ~allowed_roles:_R_POOL_OP
  ()

let pool_disable_ha = call
  ~in_product_since:rel_miami
  ~name:"disable_ha"
  ~in_oss_since:None
  ~params:[]
  ~doc:"Turn off High Availability mode"
  ~allowed_roles:_R_POOL_OP
  ()

let pool_sync_database = call
  ~name:"sync_database"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[]
  ~doc:"Forcibly synchronise the database now"
  ~allowed_roles:_R_POOL_OP
  ()

let pool_designate_new_master = call
  ~in_product_since:rel_miami
  ~name:"designate_new_master"
  ~in_oss_since:None
  ~params:[Ref _host, "host", "The host who should become the new master"]
  ~doc:"Perform an orderly handover of the role of master to the referenced host."
  ~allowed_roles:_R_POOL_OP
  ()

let pool_join = call
  ~name:"join"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[String, "master_address", "The hostname of the master of the pool to join";
	   String, "master_username", "The username of the master (for initial authentication)";
	   String, "master_password", "The password for the master (for initial authentication)";
	  ]
  ~errs:[Api_errors.pool_joining_host_cannot_contain_shared_SRs]
  ~doc:"Instruct host to join a new pool"
  ~allowed_roles:_R_POOL_OP
    ()

let pool_join_force = call
  ~name:"join_force"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[String, "master_address", "The hostname of the master of the pool to join";
	   String, "master_username", "The username of the master (for initial authentication)";
	   String, "master_password", "The password for the master (for initial authentication)";
	  ]
  ~doc:"Instruct host to join a new pool"
  ~allowed_roles:_R_POOL_OP
    ()


let pool_slave_reset_master = call ~flags:[`Session]
  ~name:"emergency_reset_master"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[
	    String, "master_address", "The hostname of the master";
	  ]
  ~doc:"Instruct a slave already in a pool that the master has changed"
  ~allowed_roles:_R_POOL_OP
    ()

let pool_transition_to_master = call ~flags:[`Session]
  ~name:"emergency_transition_to_master"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[]
  ~doc:"Instruct host that's currently a slave to transition to being master"
  ~allowed_roles:_R_POOL_OP
    ()

let pool_recover_slaves = call
  ~name:"recover_slaves"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[]
  ~result:(Set (Ref _host), "list of hosts whose master address were successfully reset")
  ~doc:"Instruct a pool master, M, to try and contact its slaves and, if slaves are in emergency mode, reset their master address to M."
  ~allowed_roles:_R_POOL_OP
  ()
  
let pool_eject = call
  ~name:"eject"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[Ref _host, "host", "The host to eject"]
  ~doc:"Instruct a pool master to eject a host from the pool"
  ~allowed_roles:_R_POOL_OP
    ()

let pool_initial_auth = call
  ~name:"initial_auth"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[]
  ~result:(String, "")
  ~doc:"Internal use only"
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_OP
    ()

let pool_create_VLAN_from_PIF = call
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~name:"create_VLAN_from_PIF"
  ~doc:"Create a pool-wide VLAN by taking the PIF."
  ~params:[Ref _pif, "pif", "physical interface on any particular host, that identifies the PIF on which to create the (pool-wide) VLAN interface";
	   Ref _network, "network", "network to which this interface should be connected";
	   Int, "VLAN", "VLAN tag for the new interface"]
  ~result:(Set (Ref _pif), "The references of the created PIF objects")
  ~errs:[Api_errors.vlan_tag_invalid]
  ~allowed_roles:_R_POOL_OP
  ()

(* !! THIS IS BROKEN; it takes a device name which in the case of a bond is not homogeneous across all pool hosts.
      See CA-22613. !! *)
let pool_create_VLAN = call
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~name:"create_VLAN"
  ~doc:"Create PIFs, mapping a network to the same physical interface/VLAN on each host. This call is deprecated: use Pool.create_VLAN_from_PIF instead."
  ~params:[String, "device", "physical interface on which to create the VLAN interface";
	   Ref _network, "network", "network to which this interface should be connected";
	   Int, "VLAN", "VLAN tag for the new interface"]
  ~result:(Set (Ref _pif), "The references of the created PIF objects")
  ~errs:[Api_errors.vlan_tag_invalid]
  ~allowed_roles:_R_POOL_OP
  ()


let hello_return = Enum("hello_return", [
			      "ok", "";
			      "unknown_host", "";
			      "cannot_talk_back", ""
			    ])

let pool_hello = call
  ~name:"hello"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[String, "host_uuid", "";
	   String, "host_address", ""
	  ]
  ~result:(hello_return, "")
  ~doc:"Internal use only"
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_OP
  ()

let pool_slave_network_report = call
  ~name:"slave_network_report"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~doc:"Internal use only"
  ~params:[Map (String, String), "phydevs", "(device,bridge) pairs of physical NICs on slave";
	   Map (String, String), "dev_to_mac", "(device,mac) pairs of physical NICs on slave";
	   Map (String, Int), "dev_to_mtu", "(device,mtu) pairs of physical NICs on slave";
	   Ref _host, "slave_host", "the host that the PIFs will be attached to when created"
	  ]
  ~result:(Set(Ref _pif), "refs for pifs corresponding to device list")
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_ADMIN
  ()

let pool_ping_slave = call ~flags:[`Session]
  ~name:"is_slave"
  ~in_oss_since:None
  ~in_product_since:rel_rio
  ~params:[Ref _host, "host", ""]
  ~doc:"Internal use only"
  ~result:(Bool, "returns false if pinged host is master [indicating critical error condition]; true if pinged host is slave")
  ~hide_from_docs:true
  ~allowed_roles:_R_POOL_ADMIN
  ()

let pool_ha_prevent_restarts_for = call ~flags:[`Session]
  ~name:"ha_prevent_restarts_for"
  ~in_product_since:rel_orlando_update_1
  ~doc:"When this call returns the VM restart logic will not run for the requested number of seconds. If the argument is zero then the restart thread is immediately unblocked"
  ~params:[Int, "seconds", "The number of seconds to block the restart thread for"]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_ha_failover_plan_exists = call ~flags:[`Session]
  ~name:"ha_failover_plan_exists"
  ~in_product_since:rel_orlando
  ~doc:"Returns true if a VM failover plan exists for up to 'n' host failures"
  ~params:[Int, "n", "The number of host failures to plan for" ]
  ~result:(Bool, "true if a failover plan exists for the supplied number of host failures")
  ~allowed_roles:_R_POOL_OP
  ()

let pool_ha_compute_max_host_failures_to_tolerate = call ~flags:[`Session]
  ~name:"ha_compute_max_host_failures_to_tolerate"
  ~in_product_since:rel_orlando
  ~doc:"Returns the maximum number of host failures we could tolerate before we would be unable to restart configured VMs"
  ~params:[]
  ~result:(Int, "maximum value for ha_host_failures_to_tolerate given current configuration")
  ~allowed_roles:_R_POOL_OP
  ()

let pool_ha_compute_hypothetical_max_host_failures_to_tolerate = call ~flags:[`Session]
  ~name:"ha_compute_hypothetical_max_host_failures_to_tolerate"
  ~in_product_since:rel_orlando
  ~doc:"Returns the maximum number of host failures we could tolerate before we would be unable to restart the provided VMs"
  ~params:[ Map(Ref _vm, String), "configuration", "Map of protected VM reference to restart priority" ]
  ~result:(Int, "maximum value for ha_host_failures_to_tolerate given provided configuration")
  ~allowed_roles:_R_READ_ONLY
  ()

let pool_ha_compute_vm_failover_plan = call ~flags:[`Session]
  ~name:"ha_compute_vm_failover_plan"
  ~in_product_since:rel_orlando
  ~doc:"Return a VM failover plan assuming a given subset of hosts fail"
  ~params:[Set(Ref _host), "failed_hosts", "The set of hosts to assume have failed";
	   Set(Ref _vm), "failed_vms", "The set of VMs to restart" ]
  ~result:(Map(Ref _vm, Map(String, String)), "VM failover plan: a map of VM to host to restart the host on")
  ~allowed_roles:_R_POOL_OP
  ()

let pool_create_new_blob = call
  ~name: "create_new_blob"
  ~in_product_since:rel_orlando
  ~doc:"Create a placeholder for a named binary blob of data that is associated with this pool"
  ~versioned_params:
  [{param_type=Ref _pool; param_name="pool"; param_doc="The pool"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="name"; param_doc="The name associated with the blob"; param_release=orlando_release; param_default=None};
  {param_type=String; param_name="mime_type"; param_doc="The mime type for the data. Empty string translates to application/octet-stream"; param_release=orlando_release; param_default=None};
  {param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}
  ]
  ~result:(Ref _blob, "The reference of the blob, needed for populating its data")
  ~allowed_roles:_R_POOL_OP
  ()

let pool_set_ha_host_failures_to_tolerate = call
  ~name:"set_ha_host_failures_to_tolerate"
  ~in_product_since:rel_orlando
  ~doc:"Set the maximum number of host failures to consider in the HA VM restart planner"
  ~params:[Ref _pool, "self", "The pool";
	   Int, "value", "New number of host failures to consider"]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_ha_schedule_plan_recomputation = call
  ~name:"ha_schedule_plan_recomputation"
  ~in_product_since:rel_orlando
  ~doc:"Signal that the plan should be recomputed (eg a host has come online)"
  ~params:[]
  ~hide_from_docs:true
  ~pool_internal:true
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ()

let pool_enable_binary_storage = call
  ~name:"enable_binary_storage"
  ~in_product_since:rel_orlando
  ~hide_from_docs:true
  ~doc:"Enable the storage of larger objects, such as RRDs, messages and binary blobs across all hosts in the pool"
  ~params:[]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_disable_binary_storage = call
  ~name:"disable_binary_storage"
  ~in_product_since:rel_orlando
  ~hide_from_docs:true
  ~doc:"Disable the storage of larger objects, such as RRDs, messages and binary blobs across all hosts in the pool. This will destroy all of these objects where they exist."
  ~params:[]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_enable_external_auth = call ~flags:[`Session]
  ~name:"enable_external_auth"
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~params:[
    Ref _pool, "pool", "The pool whose external authentication should be enabled"; 
    Map (String,String), "config", "A list of key-values containing the configuration data" ; 
    String, "service_name", "The name of the service" ; 
    String, "auth_type", "The type of authentication (e.g. AD for Active Directory)" 
    ]
  ~doc:"This call enables external authentication on all the hosts of the pool"
  ~allowed_roles:_R_POOL_ADMIN
  ()

let pool_disable_external_auth = call ~flags:[`Session]
  ~name:"disable_external_auth"
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~versioned_params:[
    {param_type=Ref _pool; param_name="pool"; param_doc="The pool whose external authentication should be disabled"; param_release=george_release; param_default=None};
    {param_type=Map (String, String); param_name="config"; param_doc="Optional parameters as a list of key-values containing the configuration data"; param_release=george_release; param_default=Some (VMap [])}
    ]
  ~doc:"This call disables external authentication on all the hosts of the pool"
  ~allowed_roles:_R_POOL_ADMIN
  ()

let pool_detect_nonhomogeneous_external_auth = call ~flags:[`Session]
  ~name:"detect_nonhomogeneous_external_auth"
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~params:[
    Ref _pool, "pool", "The pool where to detect non-homogeneous external authentication configuration"; 
    ]
  ~doc:"This call asynchronously detects if the external authentication configuration in any slave is different from that in the master and raises appropriate alerts"
  ~allowed_roles:_R_POOL_OP
  ()

let pool_initialize_wlb = call
	~name:"initialize_wlb"
	~lifecycle:wlb_removed
  ~doc:"Initializes workload balancing monitoring on this pool with the specified wlb server"
  ~params:[String, "wlb_url", "The ip address and port to use when accessing the wlb server";
    String, "wlb_username", "The username used to authenticate with the wlb server";
    String, "wlb_password", "The password used to authenticate with the wlb server";
    String, "xenserver_username", "The usernamed used by the wlb server to authenticate with the xenserver";
    String, "xenserver_password", "The password used by the wlb server to authenticate with the xenserver"]
  ~allowed_roles:_R_POOL_OP
   ()

let pool_deconfigure_wlb = call
	~name:"deconfigure_wlb"
	~lifecycle:wlb_removed
  ~doc:"Permanently deconfigures workload balancing monitoring on this pool"
  ~params:[]
  ~allowed_roles:_R_POOL_OP
   ()

let pool_send_wlb_configuration = call
  ~name:"send_wlb_configuration"
  ~lifecycle:wlb_removed
  ~doc:"Sets the pool optimization criteria for the workload balancing server"
  ~params:[Map(String, String), "config", "The configuration to use in optimizing this pool"]
  ~allowed_roles:_R_POOL_OP
   ()
 
let pool_retrieve_wlb_configuration = call
  ~name:"retrieve_wlb_configuration"
  ~lifecycle:wlb_removed
  ~doc:"Retrieves the pool optimization criteria from the workload balancing server"
  ~params:[]
  ~result:(Map(String,String), "The configuration used in optimizing this pool")
  ~allowed_roles:_R_READ_ONLY
   ()
   
let pool_retrieve_wlb_recommendations = call
  ~name:"retrieve_wlb_recommendations"
  ~lifecycle:wlb_removed
  ~doc:"Retrieves vm migrate recommendations for the pool from the workload balancing server"
  ~params:[]
  ~result:(Map(Ref _vm,Set(String)), "The list of vm migration recommendations")
  ~allowed_roles:_R_READ_ONLY
   ()
   
let pool_send_test_post = call
  ~name:"send_test_post"
  ~in_product_since:rel_george
  ~doc:"Send the given body to the given host and port, using HTTPS, and print the response.  This is used for debugging the SSL layer."
  ~params:[(String, "host", ""); (Int, "port", ""); (String, "body", "")]
  ~result:(String, "The response")
  ~allowed_roles:_R_POOL_ADMIN
   ()
   
let pool_certificate_install = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~name:"certificate_install"
  ~doc:"Install an SSL certificate pool-wide."
  ~params:[String, "name", "A name to give the certificate";
	   String, "cert", "The certificate"]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_certificate_uninstall = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~name:"certificate_uninstall"
  ~doc:"Remove an SSL certificate."
  ~params:[String, "name", "The certificate name"]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_certificate_list = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~name:"certificate_list"
  ~doc:"List all installed SSL certificates."
  ~result:(Set(String),"All installed certificates")
  ~allowed_roles:_R_POOL_OP
  ()

let pool_crl_install = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~name:"crl_install"
  ~doc:"Install an SSL certificate revocation list, pool-wide."
  ~params:[String, "name", "A name to give the CRL";
	   String, "cert", "The CRL"]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_crl_uninstall = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~name:"crl_uninstall"
  ~doc:"Remove an SSL certificate revocation list."
  ~params:[String, "name", "The CRL name"]
  ~allowed_roles:_R_POOL_OP
  ()

let pool_crl_list = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~name:"crl_list"
  ~doc:"List all installed SSL certificate revocation lists."
  ~result:(Set(String), "All installed CRLs")
  ~allowed_roles:_R_POOL_OP
  ()

let pool_certificate_sync = call
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~name:"certificate_sync"
  ~doc:"Sync SSL certificates from master to slaves."
  ~allowed_roles:_R_POOL_OP
  ()
  
let pool_enable_redo_log = call
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~name:"enable_redo_log"
  ~params:[Ref _sr, "sr", "SR to hold the redo log."]
  ~doc:"Enable the redo log on the given SR and start using it, unless HA is enabled."
  ~allowed_roles:_R_POOL_OP
  ()
  
let pool_disable_redo_log = call
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~name:"disable_redo_log"
  ~doc:"Disable the redo log if in use, unless HA is enabled."
  ~allowed_roles:_R_POOL_OP
  ()

let pool_audit_log_append = call
	~in_oss_since:None
  ~pool_internal:true
  ~hide_from_docs:true
	~in_product_since:rel_midnight_ride
	~name:"audit_log_append"
	~params:[String, "line", "line to be appended to the audit log"]
	~doc:"Append a line to the audit log on the master."
	~allowed_roles:_R_POOL_ADMIN
	()

let pool_set_vswitch_controller = call
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~lifecycle:[
    Published, rel_midnight_ride, "Set the IP address of the vswitch controller.";
    Extended, rel_cowley, "Allow to be set to the empty string (no controller is used)."]
  ~name:"set_vswitch_controller"
  ~params:[String, "address", "IP address of the vswitch controller."]
  ~doc:"Set the IP address of the vswitch controller."
  ~allowed_roles:_R_POOL_OP
  ()

let pool_test_archive_target = call ~flags:[`Session]
  ~name:"test_archive_target"
  ~in_oss_since:None
  ~in_product_since:rel_cowley
  ~params:[Ref _pool, "self", "Reference to the pool";
    Map(String,String), "config", "Location config settings to test";
  ]
  ~doc:"This call tests if a location is valid"
  ~allowed_roles:_R_POOL_OP
  ~result:(String, "An XMLRPC result")
  ()

let pool_enable_local_storage_caching = call 
	~name:"enable_local_storage_caching"
	~in_oss_since:None
	~in_product_since:rel_cowley
	~params:[Ref _pool, "self", "Reference to the pool"]
	~doc:"This call attempts to enable pool-wide local storage caching"
	~allowed_roles:_R_POOL_OP
	()

let pool_disable_local_storage_caching = call 
	~name:"disable_local_storage_caching"
	~in_oss_since:None
	~in_product_since:rel_cowley
	~params:[Ref _pool, "self", "Reference to the pool"]
	~doc:"This call disables pool-wide local storage caching"
	~allowed_roles:_R_POOL_OP
	()

let pool_get_license_state = call
	~name:"get_license_state"
	~in_oss_since:None
	~in_product_since:rel_clearwater
	~params:[Ref _pool, "self", "Reference to the pool"]
	~doc:"This call returns the license state for the pool"
	~allowed_roles:_R_READ_ONLY
	~result:(Map(String,String), "The pool's license state")
	()

let pool_apply_edition = call
	~name:"apply_edition"
	~in_oss_since:None
	~in_product_since:rel_clearwater
	~params:[
		Ref _pool, "self", "Reference to the pool";
		String, "edition", "The requested edition";
	]
	~doc:"Apply an edition to all hosts in the pool"
	~allowed_roles:_R_POOL_OP
	()

(** A pool class *)
let pool =
	create_obj
		~in_db:true
		~in_product_since:rel_rio
		~in_oss_since:None
		~internal_deprecated_since:None
		~persist:PersistEverything
		~gen_constructor_destructor:false
		~name:_pool
		~descr:"Pool-wide information"
		~gen_events:true
		~doccomments:[]
		~messages_default_allowed_roles:_R_POOL_OP
		~messages:
			[ pool_join
			; pool_join_force
			; pool_eject
			; pool_initial_auth
			; pool_transition_to_master
			; pool_slave_reset_master
			; pool_recover_slaves
			; pool_hello
			; pool_ping_slave
			; pool_create_VLAN
			; pool_create_VLAN_from_PIF
			; pool_slave_network_report
			; pool_enable_ha
			; pool_disable_ha
			; pool_sync_database
			; pool_designate_new_master
			; pool_ha_prevent_restarts_for
			; pool_ha_failover_plan_exists
			; pool_ha_compute_max_host_failures_to_tolerate
			; pool_ha_compute_hypothetical_max_host_failures_to_tolerate
			; pool_ha_compute_vm_failover_plan
			; pool_set_ha_host_failures_to_tolerate
			; pool_create_new_blob
			; pool_ha_schedule_plan_recomputation
			; pool_enable_binary_storage
			; pool_disable_binary_storage
			; pool_enable_external_auth
			; pool_disable_external_auth
			; pool_detect_nonhomogeneous_external_auth
			; pool_initialize_wlb
			; pool_deconfigure_wlb
			; pool_send_wlb_configuration
			; pool_retrieve_wlb_configuration
			; pool_retrieve_wlb_recommendations
			; pool_send_test_post
			; pool_certificate_install
			; pool_certificate_uninstall
			; pool_certificate_list
			; pool_crl_install
			; pool_crl_uninstall
			; pool_crl_list
			; pool_certificate_sync
			; pool_enable_redo_log
			; pool_disable_redo_log
			; pool_audit_log_append
			; pool_set_vswitch_controller
			; pool_test_archive_target
			; pool_enable_local_storage_caching
			; pool_disable_local_storage_caching
			; pool_get_license_state
			; pool_apply_edition
			]
		~contents:
			[uid ~in_oss_since:None _pool
			; field ~in_oss_since:None ~qualifier:RW ~ty:String "name_label" "Short name"
			; field ~in_oss_since:None ~qualifier:RW ~ty:String "name_description" "Description"
			; field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Ref _host) "master" "The host that is pool master"
			; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "default_SR" "Default SR for VDIs"
			; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "suspend_image_SR" "The SR in which VDIs for suspend images are created"
			; field ~in_oss_since:None ~qualifier:RW ~ty:(Ref _sr) "crash_dump_SR" "The SR in which VDIs for crash dumps are created"
			; field ~in_oss_since:None ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:[("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP));("EMPTY_FOLDERS",(_R_VM_OP))]
			; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "ha_enabled" "true if HA is enabled on the pool, false otherwise"
			; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "ha_configuration" "The current HA configuration"
			; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:(Set String) ~default_value:(Some (VSet [])) "ha_statefiles" "HA statefile VDIs in use"
			; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L)) "ha_host_failures_to_tolerate" "Number of host failures to tolerate before the Pool is declared to be overcommitted"
			; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Int ~default_value:(Some (VInt 0L)) "ha_plan_exists_for" "Number of future host failures we have managed to find a plan for. Once this reaches zero any future host failures will cause the failure of protected VMs."
			; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool false)) "ha_allow_overcommit" "If set to false then operations which would cause the Pool to become overcommitted will be blocked."
			; field ~in_oss_since:None ~in_product_since:rel_orlando ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "ha_overcommitted" "True if the Pool is considered to be overcommitted i.e. if there exist insufficient physical resources to tolerate the configured number of host failures"
			; field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this pool"
			; field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes"
			; field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "gui_config" "gui-specific configuration for pool"
			; field ~lifecycle:wlb_removed ~qualifier:DynamicRO ~ty:String ~default_value:(Some (VString "")) "wlb_url" "Url for the configured workload balancing host"
			; field ~lifecycle:wlb_removed ~qualifier:DynamicRO ~ty:String ~default_value:(Some (VString "")) "wlb_username" "Username for accessing the workload balancing host"
			; field ~lifecycle:wlb_removed ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _secret) "wlb_password" "Password for accessing the workload balancing host"
			; field ~lifecycle:wlb_removed ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool false)) "wlb_enabled" "true if workload balancing is enabled on the pool, false otherwise"
			; field ~lifecycle:wlb_removed ~qualifier:RW ~ty:Bool ~default_value:(Some (VBool false)) "wlb_verify_cert" "true if communication with the WLB server should enforce SSL certificate verification."
			; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:Bool ~default_value:(Some (VBool false)) "redo_log_enabled" "true a redo-log is to be used other than when HA is enabled, false otherwise"
			; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:(Ref _vdi) ~default_value:(Some (VRef (Ref.string_of Ref.null))) "redo_log_vdi" "indicates the VDI to use for the redo-log other than when HA is enabled"
			; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:String ~default_value:(Some (VString "")) "vswitch_controller" "address of the vswitch controller"
			; field ~in_oss_since:None ~in_product_since:rel_midnight_ride ~qualifier:DynamicRO ~ty:(Map(String, String)) ~default_value:(Some (VMap [])) "restrictions" "Pool-wide restrictions currently in effect"
			; field ~in_oss_since:None ~in_product_since:rel_boston ~qualifier:DynamicRO ~ty:(Set (Ref _vdi)) "metadata_VDIs" "The set of currently known metadata VDIs for this pool"
			]
		()

(** Auth class *)
let auth_get_subject_identifier = call ~flags:[`Session]
  ~name:"get_subject_identifier"
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~params:[
    (*Ref _auth, "auth", "???";*)
    String, "subject_name", "The human-readable subject_name, such as a username or a groupname" ; 
    ]
  ~result:(String, "the subject_identifier obtained from the external directory service")
  ~doc:"This call queries the external directory service to obtain the subject_identifier as a string from the human-readable subject_name"
  ~allowed_roles:_R_READ_ONLY
  ()

let auth_get_subject_information_from_identifier = call ~flags:[`Session]
  ~name:"get_subject_information_from_identifier"
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~params:[
    String, "subject_identifier", "A string containing the subject_identifier, unique in the external directory service"
    ]
  ~result:(Map(String,String), "key-value pairs containing at least a key called subject_name")
  ~doc:"This call queries the external directory service to obtain the user information (e.g. username, organization etc) from the specified subject_identifier"
  ~allowed_roles:_R_READ_ONLY
  ()

let auth_get_group_membership = call ~flags:[`Session]
  ~name:"get_group_membership"
  ~in_oss_since:None
  ~in_product_since:rel_george
  ~params:[
    String, "subject_identifier", "A string containing the subject_identifier, unique in the external directory service"
    ]
  ~result:(Set(String), "set of subject_identifiers that provides the group membership of subject_identifier passed as argument, it contains, recursively, all groups a subject_identifier is member of.")
  ~doc:"This calls queries the external directory service to obtain the transitively-closed set of groups that the the subject_identifier is member of."
  ~allowed_roles:_R_READ_ONLY
  ()

let auth =
  create_obj ~in_db:false ~in_product_since:rel_george ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_auth ~descr:"Management of remote authentication services"
    ~gen_events:false
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_READ_ONLY
    ~messages: [auth_get_subject_identifier;
      auth_get_subject_information_from_identifier;
      auth_get_group_membership;]
    ~contents:[]
    ()

(** Subject class *)
let subject_add_to_roles = call ~flags:[`Session]
  ~name:"add_to_roles"
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~params:[
    Ref _subject, "self", "The subject who we want to add the role to";
    Ref _role, "role", "The unique role reference" ; 
    ]
  ~doc:"This call adds a new role to a subject"
  ~allowed_roles:_R_POOL_ADMIN
  ()
let subject_remove_from_roles = call ~flags:[`Session]
  ~name:"remove_from_roles"
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~params:[
    Ref _subject, "self", "The subject from whom we want to remove the role";
    Ref _role, "role", "The unique role reference in the subject's roles field" ; 
    ]
  ~doc:"This call removes a role from a subject"
  ~allowed_roles:_R_POOL_ADMIN
  ()
let subject_get_permissions_name_label = call ~flags:[`Session]
  ~name:"get_permissions_name_label"
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~params:[
    Ref _subject, "self", "The subject whose permissions will be retrieved";
    ]
  ~result:(Set(String), "a list of permission names")
  ~doc:"This call returns a list of permission names given a subject"
  ~allowed_roles:_R_READ_ONLY
  ()
(* a subject is a user/group that can log in xapi *)
let subject =
  create_obj ~in_db:true ~in_product_since:rel_george ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_subject ~descr:"A user or group that can log in xapi"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~messages: [
      subject_add_to_roles;
      subject_remove_from_roles;
      subject_get_permissions_name_label;
      ]
    ~contents:[uid ~in_oss_since:None _subject;
      field ~in_product_since:rel_george ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String "subject_identifier" "the subject identifier, unique in the external directory service";
      field ~in_product_since:rel_george ~default_value:(Some (VMap [])) ~qualifier:StaticRO ~ty:(Map(String, String)) "other_config" "additional configuration";
      (* DynamicRO fields do not show up in the constructor, as it should be because a subject must be created without receiving any roles as a parameter *)
      field ~in_product_since:rel_midnight_ride ~default_value:(Some (VSet [ 
        (VRef ("OpaqueRef:"^Constants.rbac_pool_admin_uuid))])) (* pool-admin, according to rbac_static.ml, used during upgrade from pre-rbac xapis *)
        ~ignore_foreign_key:true ~qualifier:DynamicRO ~ty:(Set((Ref _role))) "roles" "the roles associated with this subject";
      ]
    ()

(** Role class *)
let role_get_permissions = call ~flags:[`Session]
  ~name:"get_permissions"
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~params:[
    Ref _role, "self", "a reference to a role";
    ]
  ~result:(Set(Ref _role), "a list of permissions")
  ~doc:"This call returns a list of permissions given a role"
  ~allowed_roles:_R_READ_ONLY
  ()
let role_get_permissions_name_label = call ~flags:[`Session]
  ~name:"get_permissions_name_label"
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~params:[
    Ref _role, "self", "a reference to a role";
    ]
  ~result:(Set(String), "a list of permission names")
  ~doc:"This call returns a list of permission names given a role"
  ~allowed_roles:_R_READ_ONLY
  ()
let role_get_by_permission = call ~flags:[`Session]
  ~name:"get_by_permission"
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~params:[
    Ref _role, "permission", "a reference to a permission" ;
    ]
  ~result:(Set(Ref _role), "a list of references to roles")
  ~doc:"This call returns a list of roles given a permission"
  ~allowed_roles:_R_READ_ONLY
  ()
let role_get_by_permission_name_label = call ~flags:[`Session]
  ~name:"get_by_permission_name_label"
  ~in_oss_since:None
  ~in_product_since:rel_midnight_ride
  ~params:[
    String, "label", "The short friendly name of the role" ;
    ]
  ~result:(Set(Ref _role), "a list of references to roles")
  ~doc:"This call returns a list of roles given a permission name"
  ~allowed_roles:_R_READ_ONLY
  ()

(* A role defines a set of API call privileges associated with a subject *)
(* A role is synonymous to permission or privilege *)
(* A role is a recursive definition: it is either a basic role or it points to a set of roles *)
(* - full/complete role: is the one meant to be used by the end-user, a root in the tree of roles *)
(* - basic role: is the 1x1 mapping to each XAPI/HTTP call being protected, a leaf in the tree of roles *)
(* - intermediate role: an intermediate node in the recursive tree of roles, usually not meant to the end-user *)
let role =
  create_obj ~in_db:true ~in_product_since:rel_midnight_ride ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_role ~descr:"A set of permissions associated with a subject"
    ~gen_events:true
    ~force_custom_actions:(Some(StaticRO)) (* force custom actions for getters *)
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_ADMIN
    ~messages: [
      role_get_permissions;
      role_get_permissions_name_label;
      role_get_by_permission;
      role_get_by_permission_name_label;
      ]
    ~contents: [uid ~in_oss_since:None _role;
      namespace ~name:"name" ~contents:(
        [
          field ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String "label" "a short user-friendly name for the role";
          field ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~qualifier:StaticRO ~ty:String "description" "what this role is for";
        ]) ();
      field ~in_product_since:rel_midnight_ride ~default_value:(Some (VSet [])) ~ignore_foreign_key:true ~qualifier:StaticRO ~ty:(Set(Ref _role)) "subroles" "a list of pointers to other roles or permissions";
      (*RBAC2: field ~in_product_since:rel_midnight_ride ~default_value:(Some (VBool false)) ~qualifier:StaticRO ~ty:Bool "is_complete" "if this is a complete role, meant to be used by the end-user";*)
      ]
    ()

(** A virtual disk interface *)
let vtpm =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vtpm ~descr:"A virtual TPM device"
      ~gen_events:false
      ~doccomments:[] 
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[]
      ~contents:
      [ uid _vtpm;
	field ~qualifier:StaticRO ~ty:(Ref _vm) "VM" "the virtual machine"; 
	field ~qualifier:StaticRO ~ty:(Ref _vm) "backend" "the domain where the backend is located" ]
	()

(** Console protocols *)
let console_protocol = Enum("console_protocol", [
			      "vt100", "VT100 terminal";
			      "rfb", "Remote FrameBuffer protocol (as used in VNC)";
			      "rdp", "Remote Desktop Protocol"
			    ])

(** A virtual console device *)
let console = 
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_console ~descr:"A console"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[]  ~contents:
      [ uid _console;
	field ~qualifier:DynamicRO ~ty:console_protocol "protocol" "the protocol used by this console";
	field ~qualifier:DynamicRO ~ty:String "location" "URI for the console service";
	field ~qualifier:DynamicRO ~ty:(Ref _vm) "VM" "VM to which this console is attached";
	field  ~ty:(Map(String, String)) "other_config" "additional configuration";
	field ~in_oss_since:None ~internal_only:true ~ty:Int "port" "port in dom0 on which the console server is listening";
      ]
	()

(* PV domain booting *)
let pv =
  [
    field "bootloader" "name of or path to bootloader";
    field "kernel" "path to the kernel";
    field "ramdisk" "path to the initrd";
    field "args" "kernel command-line arguments";
    field "bootloader_args" "miscellaneous arguments for the bootloader";
    field ~in_oss_since:None "legacy_args" "to make Zurich guests boot";
  ]

(** HVM domain booting *)
let hvm =
  [
    field "boot_policy" "HVM boot policy";
    field ~ty:(Map(String, String)) "boot_params" "HVM boot params";
    field ~writer_roles:_R_VM_POWER_ADMIN ~in_oss_since:None ~ty:Float ~in_product_since:rel_miami ~qualifier:StaticRO "shadow_multiplier" "multiplier applied to the amount of shadow that will be made available to the guest" ~default_value:(Some (VFloat 1.))
  ]

(** Action to take on guest reboot/power off/sleep etc *)
(*
let power_behaviour =
  Enum ("power_behaviour", [ "destroy", "destroy the VM state"; 
			     "restart", "automatically restart the VM"; 
			     "preserve", "leave VM running"; 
			     "rename_restart", "leave VM running and restart a new one" ])
*)
let on_crash_behaviour = 
  Enum ("on_crash_behaviour", [ "destroy", "destroy the VM state";
				"coredump_and_destroy", "record a coredump and then destroy the VM state";
				"restart", "restart the VM";
				"coredump_and_restart", "record a coredump and then restart the VM";
				"preserve", "leave the crashed VM paused";
				"rename_restart", "rename the crashed VM and start a new copy" ])

let on_normal_exit_behaviour = 
  Enum ("on_normal_exit", [ "destroy", "destroy the VM state";
			    "restart", "restart the VM" ])


(** Virtual CPUs *)
let vcpus =
  [
    field ~ty:(Map(String, String)) "params" "configuration parameters for the selected VCPU policy";
    field ~qualifier:StaticRO ~ty:Int "max" "Max number of VCPUs";
    field ~qualifier:StaticRO ~ty:Int "at_startup" "Boot number of VCPUs";
  ]

(** Default actions *)
let actions =
  let crash = field ~effect:true ~ty:on_crash_behaviour in
  let normal = field ~effect:true ~ty:on_normal_exit_behaviour in
  [
    normal "after_shutdown" "action to take after the guest has shutdown itself";
    normal "after_reboot" "action to take after the guest has rebooted itself";
    crash "after_crash" "action to take if the guest crashes";
  ]

let vm_power_state =
  Enum ("vm_power_state", [ "Halted", "VM is offline and not using any resources";
			    "Paused", "All resources have been allocated but the VM itself is paused and its vCPUs are not running";
			    "Running", "Running";
				"Suspended", "VM state has been saved to disk and it is nolonger running. Note that disks remain in-use while the VM is suspended."])

let vm_operations =
  Enum ("vm_operations",
	List.map operation_enum
	  [ vm_snapshot; vm_clone; vm_copy; vm_create_template; vm_revert; vm_checkpoint; vm_snapshot_with_quiesce;
		vm_provision; vm_start; vm_start_on; vm_pause; vm_unpause; vm_cleanShutdown;
	    vm_cleanReboot; vm_hardShutdown; vm_stateReset; vm_hardReboot;
	    vm_suspend; csvm; vm_resume; vm_resume_on;
	    vm_pool_migrate;
        vm_migrate_send;
	    vm_get_boot_record; vm_send_sysrq; vm_send_trigger;
		vm_query_services;vm_shutdown;
	  ]
	@ [ "changing_memory_live", "Changing the memory settings";
	    "awaiting_memory_live", "Waiting for the memory settings to change";
	    "changing_dynamic_range", "Changing the memory dynamic range";
	    "changing_static_range", "Changing the memory static range";
	    "changing_memory_limits", "Changing the memory limits";
	    "changing_shadow_memory", "Changing the shadow memory for a halted VM.";
	    "changing_shadow_memory_live", "Changing the shadow memory for a running VM.";
	    "changing_VCPUs", "Changing VCPU settings for a halted VM.";
	    "changing_VCPUs_live", "Changing VCPU settings for a running VM.";
	    "assert_operation_valid", "";
	    "data_source_op", "Add, remove, query or list data sources";
	    "update_allowed_operations", "";
	    "make_into_template", "Turning this VM into a template";
	    "import", "importing a VM from a network stream";
	    "export", "exporting a VM to a network stream";
	    "metadata_export", "exporting VM metadata to a network stream";
	    "reverting", "Reverting the VM to a previous snapshotted state";
	    "destroy", "refers to the act of uninstalling the VM"; ]
       )

(** VM (or 'guest') configuration: *)
let vm =
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vm ~descr:"A virtual machine (or 'guest')."
      ~gen_events:true
      ~doccomments:[ "destroy", "Destroy the specified VM.  The VM is completely removed from the system.  This function can only be called when the VM is in the Halted State." ]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[ vm_snapshot; vm_snapshot_with_quiesce; vm_clone; vm_copy; vm_revert; vm_checkpoint;
		vm_provision; vm_start; vm_start_on; vm_pause; vm_unpause; vm_cleanShutdown;vm_shutdown;
		vm_cleanReboot; vm_hardShutdown; vm_stateReset; vm_hardReboot; vm_suspend; csvm; vm_resume;
		vm_hardReboot_internal;
		vm_resume_on;
		vm_pool_migrate; vm_pool_migrate_complete;
		set_vcpus_number_live;
		vm_add_to_VCPUs_params_live;
		vm_set_ha_restart_priority;  (* updates the allowed-operations of the VM *)
		vm_set_ha_always_run;        (* updates the allowed-operations of the VM *)
		vm_compute_memory_overhead;
		vm_set_memory_dynamic_max;
		vm_set_memory_dynamic_min;
		vm_set_memory_dynamic_range;
		vm_set_memory_static_max;
		vm_set_memory_static_min;
		vm_set_memory_static_range;
		vm_set_memory_limits;
		vm_set_memory_target_live;
		vm_wait_memory_target_live;
		vm_get_cooperative;
		vm_set_HVM_shadow_multiplier;
		vm_set_shadow_multiplier_live;
		vm_set_VCPUs_max;
		vm_set_VCPUs_at_startup;
		vm_send_sysrq; vm_send_trigger;
		vm_maximise_memory;
		vm_migrate_send;
		vm_assert_can_migrate;
		vm_get_boot_record;
		vm_get_data_sources; vm_record_data_source; vm_query_data_source; vm_forget_data_source_archives;
		assert_operation_valid vm_operations _vm _self;
		update_allowed_operations vm_operations _vm _self;
		vm_get_allowed_VBD_devices;
		vm_get_allowed_VIF_devices;
		vm_get_possible_hosts;
		vm_assert_can_boot_here;
		vm_atomic_set_resident_on;
		vm_create_new_blob;
		vm_s3_suspend;
		vm_s3_resume;
		vm_assert_agile;
		vm_update_snapshot_metadata;
		vm_retrieve_wlb_recommendations;
		vm_copy_bios_strings;
    vm_set_protection_policy;
		vm_set_start_delay;
		vm_set_shutdown_delay;
		vm_set_order;
		vm_set_suspend_VDI;
		vm_assert_can_be_recovered;
		vm_get_SRs_required_for_recovery;
		vm_recover;
		vm_import_convert;
		vm_set_appliance;
		vm_query_services;
		]
      ~contents:
      ([ uid _vm;
      ] @ (allowed_and_current_operations vm_operations) @ [
	field ~writer_roles:_R_VM_OP ~qualifier:DynamicRO ~ty:vm_power_state "power_state" "Current power state of the machine";
	namespace ~name:"name" ~contents:(names oss_since_303 RW) ();

	field ~ty:Int "user_version" "a user version number for this machine";
	field ~effect:true ~ty:Bool "is_a_template" "true if this is a template. Template VMs can never be started, they are used only for cloning other VMs";
	field ~qualifier:DynamicRO ~ty:(Ref _vdi) "suspend_VDI" "The VDI that a suspend image is stored on. (Only has meaning if VM is currently suspended)";

	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~ty:(Ref _host) "resident_on" "the host the VM is currently resident on";
	field ~writer_roles:_R_VM_POWER_ADMIN ~in_oss_since:None ~internal_only:true ~qualifier:DynamicRO ~ty:(Ref _host) "scheduled_to_be_resident_on" "the host on which the VM is due to be started/resumed/migrated. This acts as a memory reservation indicator";
	field ~writer_roles:_R_VM_POWER_ADMIN ~in_oss_since:None ~ty:(Ref _host) "affinity" "a host which the VM has some affinity for (or NULL). This is used as a hint to the start call when it decides where to run the VM. Implementations are free to ignore this field.";

	namespace ~name:"memory" ~contents:guest_memory ();
	namespace ~name:"VCPUs" ~contents:vcpus ();
	namespace ~name:"actions" ~contents:actions ();
	
	field ~writer_roles:_R_POOL_ADMIN ~qualifier:DynamicRO ~ty:(Set (Ref _console)) "consoles" "virtual console devices";
	field ~qualifier:DynamicRO ~ty:(Set (Ref _vif)) "VIFs" "virtual network interfaces";
	field ~qualifier:DynamicRO ~ty:(Set (Ref _vbd)) "VBDs" "virtual block devices";
	field ~writer_roles:_R_POOL_ADMIN ~qualifier:DynamicRO ~ty:(Set (Ref _crashdump)) "crash_dumps" "crash dumps associated with this VM";
	field ~qualifier:DynamicRO ~ty:(Set (Ref _vtpm)) "VTPMs" "virtual TPMs";
	
	namespace ~name:"PV" ~contents:pv ();
	namespace ~name:"HVM" ~contents:hvm ();
	field ~ty:(Map(String, String)) "platform" "platform-specific configuration";

	field ~lifecycle:[
		Published, rel_rio, "PCI bus path for pass-through devices";
		Deprecated, rel_boston, "Field was never used"]
		"PCI_bus" "PCI bus path for pass-through devices";
	field  ~ty:(Map(String, String)) "other_config" "additional configuration" ~map_keys_roles:["pci", _R_POOL_ADMIN; ("folder",(_R_VM_OP));("XenCenter.CustomFields.*",(_R_VM_OP))];
	field ~qualifier:DynamicRO ~ty:Int "domid" "domain ID (if available, -1 otherwise)";
	field ~qualifier:DynamicRO ~in_oss_since:None ~ty:String "domarch" "Domain architecture (if available, null string otherwise)";
	field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map(String, String)) "last_boot_CPU_flags" "describes the CPU flags on which the VM was last booted";
	field ~qualifier:DynamicRO ~ty:Bool "is_control_domain" "true if this is a control domain (domain 0 or a driver domain)";
	field ~qualifier:DynamicRO ~ty:(Ref _vm_metrics) "metrics" "metrics associated with this VM";
	field ~qualifier:DynamicRO ~ty:(Ref _vm_guest_metrics) "guest_metrics" "metrics associated with the running guest";
	(* This was an internal field in Rio, Miami beta1, Miami beta2 but is now exposed so that
	   it will be included automatically in Miami GA exports and can be restored, important if
	   the VM is in a suspended state *)
	field ~in_oss_since:None ~internal_only:false ~in_product_since:rel_miami ~qualifier:DynamicRO ~ty:String "last_booted_record" "marshalled value containing VM record at time of last boot, updated dynamically to reflect the runtime state of the domain" ~default_value:(Some (VString ""));
	field ~in_oss_since:None ~ty:String "recommendations" "An XML specification of recommended values and ranges for properties of this VM";
	field ~effect:true ~in_oss_since:None ~ty:(Map(String, String)) ~in_product_since:rel_miami ~qualifier:RW "xenstore_data" "data to be inserted into the xenstore tree (/local/domain/<domid>/vm-data) after the VM is created." ~default_value:(Some (VMap []));
	field ~writer_roles:_R_POOL_OP ~in_oss_since:None ~ty:Bool ~in_product_since:rel_orlando ~internal_deprecated_since:rel_boston ~qualifier:StaticRO "ha_always_run" "if true then the system will attempt to keep the VM running as much as possible." ~default_value:(Some (VBool false));
	field ~writer_roles:_R_POOL_OP ~in_oss_since:None ~ty:String ~in_product_since:rel_orlando ~qualifier:StaticRO "ha_restart_priority" "has possible values: \"best-effort\" meaning \"try to restart this VM if possible but don't consider the Pool to be overcommitted if this is not possible\"; \"restart\" meaning \"this VM should be restarted\"; \"\" meaning \"do not try to restart this VM\"" ~default_value:(Some (VString ""));
	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VBool false))          ~ty:Bool            "is_a_snapshot" "true if this is a snapshot. Snapshotted VMs can never be started, they are used only for cloning other VMs";
	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VRef ""))              ~ty:(Ref _vm)       "snapshot_of" "Ref pointing to the VM this snapshot is of.";
	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando                                              ~ty:(Set (Ref _vm)) "snapshots" "List pointing to all the VM snapshots.";
	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VDateTime Date.never)) ~ty:DateTime        "snapshot_time" "Date/time when this snapshot was created.";
	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VString ""))           ~ty:String          "transportable_snapshot_id" "Transportable ID of the snapshot VM";
	field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~ty:(Map(String, Ref _blob)) ~default_value:(Some (VMap [])) "blobs" "Binary blobs associated with this VM";
	field ~writer_roles:_R_VM_OP ~in_product_since:rel_orlando ~default_value:(Some (VSet [])) ~ty:(Set String) "tags" "user-specified tags for categorization purposes";
	field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~qualifier:RW ~ty:(Map(vm_operations, String)) "blocked_operations" "List of operations which have been explicitly blocked and an error code";
	
	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap []))    ~ty:(Map (String, String)) "snapshot_info"     "Human-readable information concerning this snapshot";
	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VString "")) ~ty:String                 "snapshot_metadata" "Encoded information about the VM's metadata this is a snapshot of";

	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VRef "")) ~ty:(Ref _vm)       "parent"       "Ref pointing to the parent of this VM";
	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride                                 ~ty:(Set (Ref _vm)) "children"     "List pointing to all the children of this VM";

	field ~qualifier:DynamicRO ~in_product_since:rel_midnight_ride ~default_value:(Some (VMap [])) ~ty:(Map (String,String)) "bios_strings" "BIOS strings";
	field ~writer_roles:_R_VM_POWER_ADMIN ~qualifier:StaticRO ~lifecycle:[Published, rel_cowley, ""; Deprecated, rel_clearwater, "The VMPR feature was removed"] ~default_value:(Some (VRef (Ref.string_of Ref.null))) ~ty:(Ref _vmpp) "protection_policy" "Ref pointing to a protection policy for this VM";
	field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~lifecycle:[Published, rel_cowley, ""; Deprecated, rel_clearwater, "The VMPR feature was removed"] ~default_value:(Some (VBool false)) ~ty:Bool "is_snapshot_from_vmpp" "true if this snapshot was created by the protection policy";
	field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~ty:(Ref _vm_appliance) ~default_value:(Some (VRef (Ref.string_of Ref.null))) "appliance" "the appliance to which this VM belongs";
	field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~in_product_since:rel_boston ~default_value:(Some (VInt 0L)) ~ty:Int "start_delay" "The delay to wait before proceeding to the next order in the startup sequence (seconds)";
	field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~in_product_since:rel_boston ~default_value:(Some (VInt 0L)) ~ty:Int "shutdown_delay" "The delay to wait before proceeding to the next order in the shutdown sequence (seconds)";
	field ~writer_roles:_R_POOL_OP ~qualifier:StaticRO ~in_product_since:rel_boston ~default_value:(Some (VInt 0L)) ~ty:Int "order" "The point in the startup or shutdown sequence at which this VM will be started";
	field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _vgpu)) "VGPUs" "Virtual GPUs";
	field ~qualifier:DynamicRO ~lifecycle:[Published, rel_boston, ""] ~ty:(Set (Ref _pci)) "attached_PCIs" "Currently passed-through PCI devices";
	field ~writer_roles:_R_VM_ADMIN ~qualifier:RW ~in_product_since:rel_boston ~default_value:(Some (VRef (Ref.string_of Ref.null))) ~ty:(Ref _sr) "suspend_SR" "The SR on which a suspend image is stored";
	field ~qualifier:StaticRO ~in_product_since:rel_boston ~default_value:(Some (VInt 0L)) ~ty:Int "version" "The number of times this VM has been recovered";
	field ~qualifier:StaticRO ~in_product_since:rel_clearwater ~default_value:(Some (VString "0:0")) ~ty:(String) "generation_id" "Generation ID of the VM";
      ])
	()

let vm_memory_metrics = 
  [
    field ~qualifier:DynamicRO ~ty:Int "actual" "Guest's actual memory (bytes)" ~persist:false
  ]

let vm_vcpu_metrics =
  [
    field ~qualifier:DynamicRO ~ty:Int "number" "Current number of VCPUs" ~persist:true;
    field ~qualifier:DynamicRO ~ty:(Map (Int, Float)) "utilisation" "Utilisation for all of guest's current VCPUs" ~persist:false;
    field ~qualifier:DynamicRO ~ty:(Map (Int, Int)) "CPU" "VCPU to PCPU map" ~persist:false;
    field ~qualifier:DynamicRO ~ty:(Map (String, String)) "params" "The live equivalent to VM.VCPUs_params" ~persist:false;
    field ~qualifier:DynamicRO ~ty:(Map (Int, Set String)) "flags" "CPU flags (blocked,online,running)" ~persist:false;
  ]

let vm_metrics = 
    create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_vm_metrics ~descr:"The metrics associated with a VM"
      ~gen_events:true
      ~doccomments:[]
      ~messages_default_allowed_roles:_R_VM_ADMIN
      ~messages:[] ~contents:
      [ uid _vm_metrics;
	namespace ~name:"memory" ~contents:vm_memory_metrics ();
	namespace ~name:"VCPUs" ~contents:vm_vcpu_metrics ();
	field ~qualifier:DynamicRO ~ty:(Set (String)) "state" "The state of the guest, eg blocked, dying etc" ~persist:false;
	field ~qualifier:DynamicRO ~ty:DateTime "start_time" "Time at which this VM was last booted";
	field ~in_oss_since:None ~qualifier:DynamicRO ~ty:DateTime "install_time" "Time at which the VM was installed";
	field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated" ~persist:false;
	field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration" ~persist:false;
      ]
	()

(* Some of this stuff needs to persist (like PV drivers vsns etc.) so we know about what's likely to be in the VM even when it's off.
   Other things don't need to persist, so we specify these on a per-field basis *)
let vm_guest_metrics =
  create_obj ~in_db:true ~in_product_since:rel_rio ~in_oss_since:oss_since_303 ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_vm_guest_metrics ~descr:"The metrics reported by the guest (as opposed to inferred from outside)"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_VM_ADMIN
    ~messages:[] ~contents:
    [ uid _vm_guest_metrics;
      field ~qualifier:DynamicRO ~ty:(Map(String, String)) "os_version" "version of the OS";
      field ~qualifier:DynamicRO ~ty:(Map(String, String)) "PV_drivers_version" 
	"version of the PV drivers";
      field ~qualifier:DynamicRO ~ty:Bool ~in_oss_since:None "PV_drivers_up_to_date"
	"true if the PV drivers appear to be up to date";

      field ~qualifier:DynamicRO ~ty:(Map(String, String)) "memory" "free/used/total memory";
      field ~qualifier:DynamicRO ~ty:(Map(String, String)) "disks" "disk configuration/free space";
      field ~qualifier:DynamicRO ~ty:(Map(String, String)) "networks" "network configuration";
      field ~qualifier:DynamicRO ~ty:(Map(String, String)) "other" "anything else";
      field ~qualifier:DynamicRO ~ty:DateTime "last_updated" "Time at which this information was last updated";
      field ~in_product_since:rel_orlando ~default_value:(Some (VMap [])) ~ty:(Map(String, String)) "other_config" "additional configuration";
      field ~qualifier:DynamicRO ~in_product_since:rel_orlando ~default_value:(Some (VBool false)) ~ty:Bool "live" "True if the guest is sending heartbeat messages via the guest agent";
    ]
    ()

(* VM protection policy *)
let vmpr_removed = [
  Published, rel_cowley, "";
  Removed, rel_clearwater, "The VMPR feature was removed";
]
let vmpp_protect_now = call ~flags:[`Session]
  ~name:"protect_now"
  ~lifecycle:vmpr_removed
  ~params:[Ref _vmpp, "vmpp", "The protection policy to execute";]
  ~doc:"This call executes the protection policy immediately"
  ~allowed_roles:_R_POOL_OP
  ~result:(String, "An XMLRPC result")
  ()
let vmpp_archive_now = call ~flags:[`Session]
  ~name:"archive_now"
  ~lifecycle:vmpr_removed
  ~params:[Ref _vm, "snapshot", "The snapshot to archive";]
  ~doc:"This call archives the snapshot provided as a parameter"
  ~allowed_roles:_R_VM_POWER_ADMIN
  ~result:(String, "An XMLRPC result")
  ()
let vmpp_create_alert = call ~flags:[`Session]
  ~name:"create_alert"
  ~lifecycle:vmpr_removed
  ~params:[Ref _vmpp, "vmpp", "The protection policy where the alert should be created";
     String, "name", "The name of the message";
	   Int, "priority", "The priority of the message";
	   String, "body", "The body of the email message";
     String, "data", "The data in xml";
  ]
  ~doc:"This call creates an alert for some protection policy"
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ~hide_from_docs:true
  ()
let vmpp_get_alerts = call ~flags:[`Session]
  ~name:"get_alerts"
  ~lifecycle:vmpr_removed
  ~params:[Ref _vmpp, "vmpp", "The protection policy";
    Int, "hours_from_now", "how many hours in the past the oldest record to fetch is";
  ]
  ~doc:"This call fetches a history of alerts for a given protection policy"
  ~allowed_roles:_R_POOL_OP
  ~result:(Set (String), "A list of alerts encoded in xml")
  ()
let vmpp_backup_type = Enum ("vmpp_backup_type",
  [
    "snapshot", "The backup is a snapshot";
    "checkpoint", "The backup is a checkpoint";
  ])
let vmpp_backup_frequency = Enum ("vmpp_backup_frequency",
  [
    "hourly", "Hourly backups";
    "daily", "Daily backups";
    "weekly", "Weekly backups";
  ])
let vmpp_archive_frequency = Enum ("vmpp_archive_frequency",
  [
    "never", "Never archive";
    "always_after_backup", "Archive after backup";
    "daily", "Daily archives";
    "weekly", "Weekly backups";
  ])
let vmpp_archive_target_type = Enum ("vmpp_archive_target_type",
  [
    "none", "No target config";
    "cifs", "CIFS target config";
    "nfs", "NFS target config";
  ])
let vmpp_schedule_min = "min"
let vmpp_schedule_hour = "hour"
let vmpp_schedule_days = "days"
let vmpp_archive_target_config_location = "location"
let vmpp_archive_target_config_username = "username"
let vmpp_archive_target_config_password = "password"
let vmpp_set_backup_retention_value = call ~flags:[`Session]
  ~name:"set_backup_retention_value"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    Int, "value", "the value to set"
  ]
  ()
let vmpp_set_is_backup_running = call ~flags:[`Session]
  ~name:"set_is_backup_running"
  ~lifecycle:vmpr_removed
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    Bool, "value", "true to mark this protection policy's backup is running"
  ]
  ~doc:"Set the value of the is_backup_running field"
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ~hide_from_docs:true
  ()
let vmpp_set_is_archive_running = call ~flags:[`Session]
  ~name:"set_is_archive_running"
  ~lifecycle:vmpr_removed
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    Bool, "value", "true to mark this protection policy's archive is running"
  ]
  ~doc:"Set the value of the is_archive_running field"
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ~hide_from_docs:true
  ()
let vmpp_set_is_alarm_enabled = call ~flags:[`Session]
  ~name:"set_is_alarm_enabled"
  ~lifecycle:vmpr_removed
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    Bool, "value", "true if alarm is enabled for this policy"
  ]
  ~doc:"Set the value of the is_alarm_enabled field"
  ~allowed_roles:_R_POOL_OP
  ()
let vmpp_set_archive_frequency = call ~flags:[`Session]
  ~name:"set_archive_frequency"
  ~lifecycle:vmpr_removed
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    vmpp_archive_frequency, "value", "the archive frequency"
  ]
  ~doc:"Set the value of the archive_frequency field"
  ~allowed_roles:_R_POOL_OP
  ()
let vmpp_set_archive_target_type = call ~flags:[`Session]
  ~name:"set_archive_target_type"
  ~lifecycle:vmpr_removed
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    vmpp_archive_target_type, "value", "the archive target config type"
  ]
  ~doc:"Set the value of the archive_target_config_type field"
  ~allowed_roles:_R_POOL_OP
  ()
let vmpp_set_backup_frequency = call ~flags:[`Session]
  ~name:"set_backup_frequency"
  ~lifecycle:vmpr_removed
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    vmpp_backup_frequency, "value", "the backup frequency"
  ]
  ~doc:"Set the value of the backup_frequency field"
  ~allowed_roles:_R_POOL_OP
  ()
let vmpp_set_backup_schedule = call ~flags:[`Session]
  ~name:"set_backup_schedule"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    Map(String,String), "value", "the value to set"
  ]
  ()
let vmpp_set_archive_target_config = call ~flags:[`Session]
  ~name:"set_archive_target_config"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    Map(String,String), "value", "the value to set"
  ]
  ()
let vmpp_set_archive_schedule = call ~flags:[`Session]
  ~name:"set_archive_schedule"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    Map(String,String), "value", "the value to set"
  ]
  ()
let vmpp_set_alarm_config = call ~flags:[`Session]
  ~name:"set_alarm_config"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    Map(String,String), "value", "the value to set"
  ]
  ()
let vmpp_set_backup_last_run_time = call ~flags:[`Session]
  ~name:"set_backup_last_run_time"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    DateTime, "value", "the value to set"
  ]
  ()
let vmpp_set_archive_last_run_time = call ~flags:[`Session]
  ~name:"set_archive_last_run_time"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_LOCAL_ROOT_ONLY
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    DateTime, "value", "the value to set"
  ]
  ()
let vmpp_add_to_backup_schedule = call ~flags:[`Session]
  ~name:"add_to_backup_schedule"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    String, "key", "the key to add";
    String, "value", "the value to add";
  ]
  ()
let vmpp_add_to_archive_target_config = call ~flags:[`Session]
  ~name:"add_to_archive_target_config"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    String, "key", "the key to add";
    String, "value", "the value to add";
  ]
  ()
let vmpp_add_to_archive_schedule = call ~flags:[`Session]
  ~name:"add_to_archive_schedule"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    String, "key", "the key to add";
    String, "value", "the value to add";
  ]
  ()
let vmpp_add_to_alarm_config = call ~flags:[`Session]
  ~name:"add_to_alarm_config"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    String, "key", "the key to add";
    String, "value", "the value to add";
  ]
  ()
let vmpp_remove_from_backup_schedule = call ~flags:[`Session]
  ~name:"remove_from_backup_schedule"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    String, "key", "the key to remove";
  ]
  ()
let vmpp_remove_from_archive_target_config = call ~flags:[`Session]
  ~name:"remove_from_archive_target_config"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    String, "key", "the key to remove";
  ]
  ()
let vmpp_remove_from_archive_schedule = call ~flags:[`Session]
  ~name:"remove_from_archive_schedule"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    String, "key", "the key to remove";
  ]
  ()
let vmpp_remove_from_alarm_config = call ~flags:[`Session]
  ~name:"remove_from_alarm_config"
  ~lifecycle:vmpr_removed
  ~allowed_roles:_R_POOL_OP
  ~params:[
    Ref _vmpp, "self", "The protection policy";
    String, "key", "the key to remove";
  ]
  ()
let vmpp =
  create_obj ~in_db:true ~in_oss_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vmpp ~descr:"VM Protection Policy"
    ~gen_events:true
    ~lifecycle:vmpr_removed
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[
      vmpp_protect_now;
      vmpp_archive_now;
      vmpp_create_alert;
      vmpp_get_alerts;
      vmpp_set_backup_retention_value;
      vmpp_set_is_backup_running;
      vmpp_set_is_archive_running;
      vmpp_set_backup_frequency;
      vmpp_set_backup_schedule;
      vmpp_set_archive_frequency;
      vmpp_set_archive_schedule;
      vmpp_set_archive_target_type;
      vmpp_set_archive_target_config;
      vmpp_set_is_alarm_enabled;
      vmpp_set_alarm_config;
      vmpp_add_to_backup_schedule;
      vmpp_add_to_archive_target_config;
      vmpp_add_to_archive_schedule;
      vmpp_add_to_alarm_config;
      vmpp_remove_from_backup_schedule;
      vmpp_remove_from_archive_target_config;
      vmpp_remove_from_archive_schedule;
      vmpp_remove_from_alarm_config;
      vmpp_set_backup_last_run_time;
      vmpp_set_archive_last_run_time;
    ]
    ~contents:[
      uid ~lifecycle:vmpr_removed _vmpp;
      namespace ~name:"name" ~contents:(names None RW) ();
      field ~lifecycle:vmpr_removed ~qualifier:RW ~ty:Bool "is_policy_enabled" "enable or disable this policy" ~default_value:(Some (VBool true));
      field ~lifecycle:vmpr_removed ~qualifier:RW ~ty:vmpp_backup_type "backup_type" "type of the backup sub-policy" ~default_value:(Some (VEnum "snapshot"));
      field ~lifecycle:vmpr_removed ~qualifier:StaticRO ~ty:Int "backup_retention_value" "maximum number of backups that should be stored at any time" ~default_value:(Some (VInt 7L));
      field ~lifecycle:vmpr_removed ~qualifier:StaticRO ~ty:vmpp_backup_frequency "backup_frequency" "frequency of the backup schedule" ~default_value:(Some (VEnum "daily"));
      field ~lifecycle:vmpr_removed ~qualifier:StaticRO ~ty:(Map (String,String)) "backup_schedule" "schedule of the backup containing 'hour', 'min', 'days'. Date/time-related information is in XenServer Local Timezone" ~default_value:(Some (VMap []));
      field ~lifecycle:vmpr_removed ~qualifier:DynamicRO ~ty:Bool "is_backup_running" "true if this protection policy's backup is running";
      field ~lifecycle:vmpr_removed ~qualifier:DynamicRO ~ty:DateTime "backup_last_run_time" "time of the last backup" ~default_value:(Some(VDateTime(Date.of_float 0.)));
      field ~lifecycle:vmpr_removed ~qualifier:StaticRO ~ty:vmpp_archive_target_type "archive_target_type" "type of the archive target config" ~default_value:(Some (VEnum "none"));
      field ~lifecycle:vmpr_removed ~qualifier:StaticRO ~ty:(Map (String,String)) "archive_target_config" "configuration for the archive, including its 'location', 'username', 'password'" ~default_value:(Some (VMap []));
      field ~lifecycle:vmpr_removed ~qualifier:StaticRO ~ty:vmpp_archive_frequency "archive_frequency" "frequency of the archive schedule" ~default_value:(Some (VEnum "never"));
      field ~lifecycle:vmpr_removed ~qualifier:StaticRO ~ty:(Map (String,String)) "archive_schedule" "schedule of the archive containing 'hour', 'min', 'days'. Date/time-related information is in XenServer Local Timezone" ~default_value:(Some (VMap []));
      field ~lifecycle:vmpr_removed ~qualifier:DynamicRO ~ty:Bool "is_archive_running" "true if this protection policy's archive is running";
      field ~lifecycle:vmpr_removed ~qualifier:DynamicRO ~ty:DateTime "archive_last_run_time" "time of the last archive" ~default_value:(Some(VDateTime(Date.of_float 0.)));
      field ~lifecycle:vmpr_removed ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) "VMs" "all VMs attached to this protection policy";
      field ~lifecycle:vmpr_removed ~qualifier:StaticRO ~ty:Bool "is_alarm_enabled" "true if alarm is enabled for this policy" ~default_value:(Some (VBool false));
      field ~lifecycle:vmpr_removed ~qualifier:StaticRO ~ty:(Map (String,String)) "alarm_config" "configuration for the alarm" ~default_value:(Some (VMap []));
      field ~lifecycle:vmpr_removed ~qualifier:DynamicRO ~ty:(Set (String)) "recent_alerts" "recent alerts" ~default_value:(Some (VSet []));
    ]
    ()

(* VM appliance *)
let vm_appliance_operations = Enum ("vm_appliance_operation",
	[
		"start", "Start";
		"clean_shutdown", "Clean shutdown";
		"hard_shutdown", "Hard shutdown";
		"shutdown", "Shutdown";
	])


let vm_appliance =
	let vm_appliance_start = call
		~name:"start"
		~in_product_since:rel_boston
		~params:[
			Ref _vm_appliance, "self", "The VM appliance";
			Bool, "paused", "Instantiate all VMs belonging to this appliance in paused state if set to true."
		]
		~errs:[Api_errors.operation_partially_failed]
		~doc:"Start all VMs in the appliance"
		~allowed_roles:_R_POOL_OP
		() in
	let vm_appliance_clean_shutdown = call
		~name:"clean_shutdown"
		~in_product_since:rel_boston
		~params:[Ref _vm_appliance, "self", "The VM appliance"]
		~errs:[Api_errors.operation_partially_failed]
		~doc:"Perform a clean shutdown of all the VMs in the appliance"
		~allowed_roles:_R_POOL_OP
		() in
	let vm_appliance_hard_shutdown = call
		~name:"hard_shutdown"
		~in_product_since:rel_boston
		~params:[Ref _vm_appliance, "self", "The VM appliance"]
		~errs:[Api_errors.operation_partially_failed]
		~doc:"Perform a hard shutdown of all the VMs in the appliance"
		~allowed_roles:_R_POOL_OP
		() in
	let vm_appliance_shutdown = call
		~name:"shutdown"
		~in_product_since:rel_boston
		~params:[Ref _vm_appliance, "self", "The VM appliance"]
		~errs:[Api_errors.operation_partially_failed]
		~doc:"For each VM in the appliance, try to shut it down cleanly. If this fails, perform a hard shutdown of the VM."
		~allowed_roles:_R_POOL_OP
		() in
	let vm_appliance_assert_can_be_recovered = call
		~name:"assert_can_be_recovered"
		~in_product_since:rel_boston
		~params:[Ref _vm_appliance, "self", "The VM appliance to recover";
			Ref _session, "session_to", "The session to which the VM appliance is to be recovered."]
		~errs:[Api_errors.vm_requires_sr]
		~doc:"Assert whether all SRs required to recover this VM appliance are available."
		~allowed_roles:_R_READ_ONLY
		() in
	let vm_appliance_get_SRs_required_for_recovery = call
		~name:"get_SRs_required_for_recovery"
		~in_product_since:rel_clearwater
		~params:[Ref _vm_appliance , "self" , "The VM appliance for which the required list of SRs has to be recovered.";
			Ref _session , "session_to", "The session to which the list of SRs have to be recovered ."]
		~result:(Set(Ref _sr), "refs for SRs required to recover the VM")
		~errs:[]
		~doc:"Get the list of SRs required by the VM appliance to recover."
		~allowed_roles:_R_READ_ONLY
		() in
	let vm_appliance_recover = call
		~name:"recover"
		~in_product_since:rel_boston
		~params:[Ref _vm_appliance, "self", "The VM appliance to recover";
			Ref _session, "session_to", "The session to which the VM appliance is to be recovered.";
			Bool, "force", "Whether the VMs should replace newer versions of themselves."]
		~errs:[Api_errors.vm_requires_sr]
		~doc:"Recover the VM appliance"
		~allowed_roles:_R_READ_ONLY
		() in
	create_obj ~in_db:true ~in_product_since:rel_boston ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_vm_appliance ~descr:"VM appliance"
		~gen_events:true
		~doccomments:[]
		~messages_default_allowed_roles:_R_POOL_OP
		~messages:[
			vm_appliance_start;
			vm_appliance_clean_shutdown;
			vm_appliance_hard_shutdown;
			vm_appliance_shutdown;
			vm_appliance_assert_can_be_recovered;
			vm_appliance_get_SRs_required_for_recovery;
			vm_appliance_recover;
		]
		~contents:([
			uid _vm_appliance;
			namespace ~name:"name" ~contents:(names None RW) ();
			] @ (allowed_and_current_operations vm_appliance_operations) @ [
			field ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) "VMs" "all VMs in this appliance";
		])
		()

(* DR_task *)
let dr_task =
	let create = call
		~name:"create"
		~in_product_since:rel_boston
		~params:[
			String, "type", "The SR driver type of the SRs to introduce";
			Map(String, String), "device_config", "The device configuration of the SRs to introduce";
			Set(String), "whitelist", "The devices to use for disaster recovery"
		]
		~result:(Ref _dr_task, "The reference to the created task")
		~doc:"Create a disaster recovery task which will query the supplied list of devices"
		~allowed_roles:_R_POOL_OP
		() in
	let destroy = call
		~name:"destroy"
		~in_product_since:rel_boston
		~params:[
			Ref _dr_task, "self", "The disaster recovery task to destroy"
		]
		~doc:"Destroy the disaster recovery task, detaching and forgetting any SRs introduced which are no longer required"
		~allowed_roles:_R_POOL_OP
		() in
	create_obj
		~in_db:true
		~in_product_since:rel_boston
		~in_oss_since:None
		~internal_deprecated_since:None
		~persist:PersistEverything
		~gen_constructor_destructor:false
		~name:_dr_task
		~descr:"DR task"
		~gen_events:true
		~doccomments:[]
		~messages_default_allowed_roles:_R_POOL_OP
		~messages:[create; destroy]
		~contents:[
			uid _dr_task;
			field ~qualifier:DynamicRO ~ty:(Set (Ref _sr)) "introduced_SRs" "All SRs introduced by this appliance";
		]
		()


(** events handling: *)

let event_operation = Enum ("event_operation",
			    [ "add", "An object has been created";
			      "del", "An object has been deleted";
                              "mod", "An object has been modified"])
let event =
  let register = call
    ~name:"register" 
    ~in_product_since:rel_rio
    ~params:[Set String, "classes", "register for events for the indicated classes"]
    ~doc:"Registers this session with the event system.  Specifying * as the desired class will register for all classes."
    ~allowed_roles:_R_ALL
    () in
  let unregister = call
    ~name:"unregister"
    ~in_product_since:rel_rio
    ~params:[Set String, "classes", "remove this session's registration for the indicated classes"]
    ~doc:"Unregisters this session with the event system"
    ~allowed_roles:_R_ALL
    () in
  let next = call
    ~name:"next" ~params:[]
    ~in_product_since:rel_rio
    ~doc:"Blocking call which returns a (possibly empty) batch of events"
    ~custom_marshaller:true
    ~flags:[`Session]
    ~result:(Set (Record _event), "the batch of events")
    ~errs:[Api_errors.session_not_registered;Api_errors.events_lost]
    ~allowed_roles:_R_ALL
      () in
  let from = call
    ~name:"from" 
	~params:[Set String, "classes", "register for events for the indicated classes";
			 String, "token", "A token representing the point from which to generate database events. The empty string represents the beginning.";
			 Float, "timeout", "Return after this many seconds if no events match";
	]
    ~in_product_since:rel_boston
    ~doc:"Blocking call which returns a (possibly empty) batch of events"
    ~custom_marshaller:true
    ~flags:[`Session]
    ~result:(Set (Record _event), "the batch of events")
    ~errs:[Api_errors.session_not_registered;Api_errors.events_lost]
    ~allowed_roles:_R_ALL
      () in
  let get_current_id = call
    ~name:"get_current_id" ~params:[]
    ~in_product_since:rel_rio
    ~doc:"Return the ID of the next event to be generated by the system"
    ~flags:[`Session]
    ~result:(Int, "the event ID")
    ~allowed_roles:_R_ALL
    () in
  let inject = call
	  ~name:"inject" ~params:[
		  String, "class", "class of the object";
		  String, "ref", "A reference to the object that will be changed.";
	  ]
	  ~in_product_since:rel_tampa
	  ~doc:"Injects an artificial event on the given object and return the corresponding ID"
	  ~flags:[`Session]
	  ~result:(String, "the event ID")
	  ~allowed_roles:_R_ALL
	  () in
  (* !!! This should call create_obj ~in_db:true like everything else... !!! *)
  {
    obj_lifecycle=[];
    name = _event;
    gen_events = false;
    description = "Asynchronous event registration and handling";
    gen_constructor_destructor = false;
    doccomments = [];
    messages = [ register; unregister; next; from; get_current_id; inject ];
    obj_release = {internal=get_product_releases rel_rio; opensource=get_oss_releases (Some "3.0.3"); internal_deprecated_since=None};
    contents = [
      field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:Int "id" "An ID, monotonically increasing, and local to the current session";
      field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:DateTime "timestamp" "The time at which the event occurred";
      field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:String "class" "The name of the class of the object that changed";
      field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:event_operation "operation" "The operation that was performed";
      field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:String "ref" "A reference to the object that changed";
      field ~reader_roles:_R_ALL ~qualifier:StaticRO ~ty:String "obj_uuid" "The uuid of the object that changed";
    ];
    persist = PersistNothing;
    in_database=false;
    force_custom_actions=None;
    obj_allowed_roles=_R_POOL_ADMIN;
    obj_implicit_msg_allowed_roles=_R_ALL;
  }

(** Blobs - binary blobs of data *)

let blob = 
  let create = call
    ~name:"create"
    ~in_product_since:rel_orlando
	~versioned_params:
	[{param_type=String; param_name="mime_type"; param_doc="The mime-type of the blob. Defaults to 'application/octet-stream' if the empty string is supplied"; param_release=orlando_release; param_default=None};
	{param_type=Bool; param_name="public"; param_doc="True if the blob should be publicly available"; param_release=tampa_release; param_default=Some (VBool false)}]
    ~doc:"Create a placeholder for a binary blob"
    ~flags:[`Session]
    ~result:(Ref _blob, "The reference to the created blob")
    ~allowed_roles:_R_POOL_OP
    () in
  let destroy = call
    ~name:"destroy"
    ~in_product_since:rel_orlando
    ~params:[Ref _blob, "self", "The reference of the blob to destroy"]
    ~flags:[`Session]
    ~allowed_roles:_R_POOL_OP
    () in
  create_obj ~in_db:true ~in_product_since:rel_orlando ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:false ~name:_blob ~descr:"A placeholder for a binary blob"
    ~gen_events:true
    ~doccomments:[]
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[create;destroy] ~contents:
    [ uid _blob;
      namespace ~name:"name" ~contents:(names oss_since_303 RW) ();
      field ~qualifier:DynamicRO ~ty:Int "size" "Size of the binary data, in bytes";
	  field ~writer_roles:_R_POOL_OP ~qualifier:RW ~in_product_since:rel_tampa ~default_value:(Some (VBool false)) ~ty:Bool "public" "True if the blob is publicly accessible";
      field ~qualifier:StaticRO ~ty:DateTime "last_updated" "Time at which the data in the blob was last updated";
      field ~qualifier:StaticRO ~ty:String "mime_type" "The mime type associated with this object. Defaults to 'application/octet-stream' if the empty string is supplied"]
    ()

let message =
  let cls =
    Enum ("cls", [ "VM", "VM";
                   "Host", "Host";
		   "SR", "SR";
		   "Pool","Pool";
       "VMPP","VMPP";
    ])
  in
  let create = call
    ~name:"create"
    ~in_product_since:rel_orlando
    ~params:[String, "name", "The name of the message";
	     Int, "priority", "The priority of the message";
	     cls, "cls", "The class of object this message is associated with";
	     String, "obj_uuid", "The uuid of the object this message is associated with";
	     String, "body", "The body of the message"]
    ~flags:[`Session]
    ~result:(Ref _message, "The reference of the created message")
    ~allowed_roles:_R_POOL_OP
    ()
  in
  let destroy = call
    ~name:"destroy"
    ~in_product_since:rel_orlando
    ~params:[Ref _message, "self", "The reference of the message to destroy"]
    ~flags:[`Session]
    ~allowed_roles:_R_POOL_OP
    ()
  in
  let get_all = call 
    ~name:"get_all"
    ~in_product_since:rel_orlando
    ~params:[]
    ~flags:[`Session]
    ~result:(Set(Ref _message), "The references to the messages")
    ~allowed_roles:_R_READ_ONLY
    ()
  in
  let get = call
    ~name:"get"
    ~in_product_since:rel_orlando
    ~params:[cls, "cls", "The class of object";
	     String, "obj_uuid", "The uuid of the object";
	     DateTime, "since", "The cutoff time"]
    ~flags:[`Session]
    ~result:(Map(Ref _message, Record _message), "The relevant messages")
    ~allowed_roles:_R_READ_ONLY
    ()
  in
  let get_since = call
    ~name:"get_since"
    ~in_product_since:rel_orlando
    ~params:[DateTime, "since", "The cutoff time"]
    ~flags:[`Session]
    ~result:(Map(Ref _message, Record _message), "The relevant messages")
    ~allowed_roles:_R_READ_ONLY
    ()
  in
  let get_by_uuid = call
    ~name:"get_by_uuid"
    ~in_product_since:rel_orlando
    ~params:[String, "uuid", "The uuid of the message"]
    ~flags:[`Session]
    ~result:(Ref _message, "The message reference")
    ~allowed_roles:_R_READ_ONLY
    ()
  in
  let get_record = call
    ~name:"get_record"
    ~in_product_since:rel_orlando
    ~params:[Ref _message, "self", "The reference to the message"]
    ~flags:[`Session]
    ~result:(Record _message, "The message record")
    ~allowed_roles:_R_READ_ONLY
    ()
  in
  let get_all_records = call 
    ~name:"get_all_records"
    ~in_product_since:rel_orlando
    ~params:[]
    ~flags:[`Session]
    ~result:(Map(Ref _message, Record _message), "The messages")
    ~allowed_roles:_R_READ_ONLY
    ()
  in
  let get_all_records_where = call 
    ~name:"get_all_records_where"
    ~in_product_since:rel_orlando
    ~params:[String, "expr", "The expression to match (not currently used)"]
    ~flags:[`Session]
    ~result:(Map(Ref _message, Record _message), "The messages")
    ~allowed_roles:_R_READ_ONLY
    ()
  in
  create_obj ~in_db:false ~in_product_since:rel_orlando ~in_oss_since:None ~persist:PersistNothing ~gen_constructor_destructor:false ~name:_message ~descr:"An message for the attention of the administrator" ~gen_events:true
    ~doccomments:[] ~internal_deprecated_since:None
    ~messages_default_allowed_roles:_R_POOL_OP
    ~messages:[create;destroy;get;get_all; get_since; get_record; get_by_uuid; get_all_records; get_all_records_where] ~contents:
    [ uid _message;
      field ~qualifier:DynamicRO ~ty:String "name" "The name of the message";
      field ~qualifier:DynamicRO ~ty:Int "priority" "The message priority, 0 being low priority";
      field ~qualifier:DynamicRO ~ty:cls "cls" "The class of the object this message is associated with";
      field ~qualifier:DynamicRO ~ty:String "obj_uuid" "The uuid of the object this message is associated with";
      field ~qualifier:DynamicRO ~ty:DateTime "timestamp" "The time at which the message was created";
      field ~qualifier:DynamicRO ~ty:String "body" "The body of the message"; ]
    ()
    
let secret =
	let introduce = call
		~name:"introduce"
		~in_product_since:rel_midnight_ride
		~versioned_params:[
			{param_type=String; param_name="uuid"; param_doc=""; param_release=midnight_ride_release; param_default=None};
			{param_type=String; param_name="value"; param_doc=""; param_release=midnight_ride_release; param_default=None};
			{param_type=(Map (String, String)); param_name="other_config"; param_doc=""; param_release=boston_release; param_default=Some (VMap [])}
		]
		~flags:[`Session]
		~result:(Ref _secret, "")
		~secret:true
		~hide_from_docs:true
		~allowed_roles:_R_POOL_OP
		()
	in
	create_obj
		~descr:"A secret"
		~doccomments:[]
		~gen_constructor_destructor:true
		~gen_events:false
		~in_db:true
		~in_oss_since:None
		~in_product_since:rel_midnight_ride
		~internal_deprecated_since:None
		~messages:[introduce]
		~messages_default_allowed_roles:_R_POOL_OP
		~implicit_messages_allowed_roles:_R_POOL_OP
		~name:_secret
		~persist:PersistEverything
		~contents:
			[ uid ~reader_roles:_R_POOL_OP _secret
			; field ~reader_roles:_R_POOL_OP ~qualifier:RW ~ty:String "value" "the secret"
                        ; field ~qualifier:RW ~ty:(Map (String,String)) "other_config" "other_config" ~default_value:(Some (VMap []));
			]
		()

(*

let alert =
  create_obj ~in_product_since:rel_miami ~in_oss_since:None ~internal_deprecated_since:None ~persist:PersistEverything ~gen_constructor_destructor:true ~name:_alert ~descr:"Notification information"
    ~gen_events:true
    ~doccomments:[]
    ~messages: []
    ~contents:
    [
     uid ~in_oss_since:None _alert;
     field ~in_oss_since:None ~qualifier:StaticRO ~ty:String "message" "description of the alert";
     field ~in_oss_since:None ~qualifier:StaticRO ~ty:(Map (String, String)) ~default_value:(Some (VMap [])) "params" "parameters of the alert";
     field ~in_oss_since:None ~qualifier:StaticRO ~ty:alert_level "level" "level of importance (info/warning/error/critical)";
     field ~in_oss_since:None ~qualifier:DynamicRO ~ty:Bool "system" "system task";
     field ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Ref _task) "task" "task related to this alert (null reference if there's no task associated)";
    ]
    ()
*)

(** PCI devices *)

let pci =
	create_obj
		~name:_pci
		~descr:"A PCI device"
		~doccomments:[]
		~gen_constructor_destructor:false
		~gen_events:true
		~in_db:true
		~lifecycle:[Published, rel_boston, ""]
		~messages:[]
		~messages_default_allowed_roles:_R_POOL_OP
		~persist:PersistEverything
		~in_oss_since:None
		~contents:[
			uid _pci ~lifecycle:[Published, rel_boston, ""];
			field ~qualifier:StaticRO ~ty:String ~lifecycle:[] "class_id" "PCI class ID" ~default_value:(Some (VString "")) ~internal_only:true;
			field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_boston, ""] "class_name" "PCI class name" ~default_value:(Some (VString ""));
			field ~qualifier:StaticRO ~ty:String ~lifecycle:[] "vendor_id" "Vendor ID" ~default_value:(Some (VString "")) ~internal_only:true;
			field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_boston, ""] "vendor_name" "Vendor name" ~default_value:(Some (VString ""));
			field ~qualifier:StaticRO ~ty:String ~lifecycle:[] "device_id" "Device ID" ~default_value:(Some (VString "")) ~internal_only:true;
			field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_boston, ""] "device_name" "Device name" ~default_value:(Some (VString ""));
			field ~qualifier:StaticRO ~ty:(Ref _host) ~lifecycle:[Published, rel_boston, ""] "host" "Physical machine that owns the PCI device" ~default_value:(Some (VRef (Ref.string_of Ref.null)));
			field ~qualifier:StaticRO ~ty:String ~lifecycle:[Published, rel_boston, ""] "pci_id" "PCI ID of the physical device" ~default_value:(Some (VString ""));
			field ~qualifier:DynamicRO ~ty:Int ~lifecycle:[] ~default_value:(Some (VInt 1L)) "functions" "Number of physical + virtual PCI functions" ~internal_only:true;
			field ~qualifier:DynamicRO ~ty:(Set (Ref _vm)) ~lifecycle:[] "attached_VMs"
				"VMs that currently have a function of this PCI device passed-through to them" ~internal_only:true;
			field ~qualifier:DynamicRO ~ty:(Set (Ref _pci)) ~lifecycle:[Published, rel_boston, ""] "dependencies" "List of dependent PCI devices" ~ignore_foreign_key:true;
			field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle:[Published, rel_boston, ""] "other_config" "Additional configuration" ~default_value:(Some (VMap []));
			]
		()

(** Physical GPUs (pGPU) *)

let pgpu =
	create_obj
		~name:_pgpu
		~descr:"A physical GPU (pGPU)"
		~doccomments:[]
		~gen_constructor_destructor:false
		~gen_events:true
		~in_db:true
		~lifecycle:[Published, rel_boston, ""]
		~messages:[]
		~messages_default_allowed_roles:_R_POOL_OP
		~persist:PersistEverything
		~in_oss_since:None
		~contents:[
			uid _pgpu ~lifecycle:[Published, rel_boston, ""];
			field ~qualifier:StaticRO ~ty:(Ref _pci) ~lifecycle:[Published, rel_boston, ""] "PCI" "Link to underlying PCI device" ~default_value:(Some (VRef (Ref.string_of Ref.null)));
			field ~qualifier:StaticRO ~ty:(Ref _gpu_group) ~lifecycle:[Published, rel_boston, ""] "GPU_group" "GPU group the pGPU is contained in" ~default_value:(Some (VRef (Ref.string_of Ref.null)));
			field ~qualifier:DynamicRO ~ty:(Ref _host) ~lifecycle:[Published, rel_boston, ""] "host" "Host that own the GPU" ~default_value:(Some (VRef (Ref.string_of Ref.null)));
			field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle:[Published, rel_boston, ""] "other_config" "Additional configuration" ~default_value:(Some (VMap []));
			]
		()

(** Groups of GPUs *)

let gpu_group =
	let create = call
		~name:"create"
		~lifecycle:[Published, rel_boston, ""]
		~versioned_params:[
			{param_type=(String); param_name="name_label"; param_doc=""; param_release=boston_release; param_default=Some (VString "")};
			{param_type=(String); param_name="name_description"; param_doc=""; param_release=boston_release; param_default=Some (VString "")};
			{param_type=(Map (String, String)); param_name="other_config"; param_doc=""; param_release=boston_release; param_default=Some (VMap [])}
		]
		~result:(Ref _gpu_group, "")
		~allowed_roles:_R_POOL_OP
		~hide_from_docs:true
		()
	in
	let destroy = call
		~name:"destroy"
		~lifecycle:[Published, rel_boston, ""]
		~params:[
			Ref _gpu_group, "self", "The vGPU to destroy"
		]
		~allowed_roles:_R_POOL_OP
		~hide_from_docs:true
		()
	in
	create_obj
		~name:_gpu_group
		~descr:"A group of compatible GPUs across the resource pool"
		~doccomments:[]
		~gen_constructor_destructor:false
		~gen_events:true
		~in_db:true
		~lifecycle:[Published, rel_boston, ""]
		~messages:[create; destroy]
		~messages_default_allowed_roles:_R_POOL_OP
		~persist:PersistEverything
		~in_oss_since:None
		~contents:[
			uid _gpu_group ~lifecycle:[Published, rel_boston, ""];
			namespace ~name:"name" ~contents:(names None RW ~lifecycle:[Published, rel_boston, ""]) ();
			field ~qualifier:DynamicRO ~ty:(Set (Ref _pgpu)) ~lifecycle:[Published, rel_boston, ""] "PGPUs" "List of pGPUs in the group";
			field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu)) ~lifecycle:[Published, rel_boston, ""] "VGPUs" "List of vGPUs using the group";
			field ~qualifier:DynamicRO ~ty:(Set String) ~lifecycle:[Published, rel_boston, ""] "GPU_types" "List of GPU types (vendor+device ID) that can be in this group" ~default_value:(Some (VSet []));
			field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle:[Published, rel_boston, ""] "other_config" "Additional configuration" ~default_value:(Some (VMap []));
			]
		()

(** Virtual GPUs (vGPU) *)

let vgpu =
	let create = call
		~name:"create"
		~lifecycle:[Published, rel_boston, ""]
		~versioned_params:[
			{param_type=(Ref _vm); param_name="VM"; param_doc=""; param_release=boston_release; param_default=None};
			{param_type=(Ref _gpu_group); param_name="GPU_group"; param_doc=""; param_release=boston_release; param_default=None};
			{param_type=String; param_name="device"; param_doc=""; param_release=boston_release; param_default=Some (VString "0")};
			{param_type=(Map (String, String)); param_name="other_config"; param_doc=""; param_release=boston_release; param_default=Some (VMap [])}
		]
		~result:(Ref _vgpu, "reference to the newly created object")
		~allowed_roles:_R_POOL_OP
		()
	in
	let destroy = call
		~name:"destroy"
		~lifecycle:[Published, rel_boston, ""]
		~params:[
			Ref _vgpu, "self", "The vGPU to destroy"
		]
		~allowed_roles:_R_POOL_OP
		()
	in
	create_obj
		~name:_vgpu
		~descr:"A virtual GPU (vGPU)"
		~doccomments:[]
		~gen_constructor_destructor:false
		~gen_events:true
		~in_db:true
		~lifecycle:[Published, rel_boston, ""]
		~messages:[create; destroy]
		~messages_default_allowed_roles:_R_POOL_OP
		~persist:PersistEverything
		~in_oss_since:None
		~contents:[
			uid _vgpu ~lifecycle:[Published, rel_boston, ""];
			field ~qualifier:DynamicRO ~ty:(Ref _vm) ~lifecycle:[Published, rel_boston, ""] "VM" "VM that owns the vGPU";
			field ~qualifier:DynamicRO ~ty:(Ref _gpu_group) ~lifecycle:[Published, rel_boston, ""] "GPU_group" "GPU group used by the vGPU";
			field ~qualifier:DynamicRO ~ty:String ~lifecycle:[Published, rel_boston, ""] ~default_value:(Some (VString "0")) "device" "Order in which the devices are plugged into the VM";
			field ~qualifier:DynamicRO ~ty:Bool ~lifecycle:[Published, rel_boston, ""] ~default_value:(Some (VBool false)) "currently_attached" "Reflects whether the virtual device is currently connected to a physical device";
			field ~qualifier:RW ~ty:(Map (String,String)) ~lifecycle:[Published, rel_boston, ""] "other_config" "Additional configuration" ~default_value:(Some (VMap []));
			]
		()

(******************************************************************************************)

(** All the objects in the system in order they will appear in documentation: *)
let all_system =
	[
		session;
		auth;
		subject;
		(role:Datamodel_types.obj);
		task;
		event;
		(* alert; *)

		pool;
		pool_patch;

		vm;
		vm_metrics;
		vm_guest_metrics;
		vmpp;
		vm_appliance;
		dr_task;
		host;
		host_crashdump;
		host_patch;
		host_metrics;
		hostcpu;
		(* network_manager; *)
		network;
		vif;
		vif_metrics;
		pif;
		pif_metrics;
		bond;
		vlan;
		storage_plugin;
		storage_repository;
		vdi;
		vbd;
		vbd_metrics;
		pbd;
		crashdump;
		(* misc *)
		vtpm;
		console;
		(* filesystem; *)
		user; 
		data_source;
		blob;
		message;
		secret;
		tunnel;
		pci;
		pgpu;
		gpu_group;
		vgpu;
	]

(** These are the pairs of (object, field) which are bound together in the database schema *)
(* If the relation is one-to-many, the "many" nodes (one edge each) must come before the "one" node (many edges) *)
let all_relations =
  [
    (* snapshots *)
    (_vm, "snapshot_of"), (_vm, "snapshots");
    (_vdi, "snapshot_of"), (_vdi, "snapshots");
    (_vm, "parent"), (_vm, "children");

    (* subtasks hierarchy *)
    (_task, "subtask_of"), (_task, "subtasks");
    (_task, "session"), (_session, "tasks");
    
    (_pif, "bond_slave_of"), (_bond, "slaves");
    (_bond, "master"), (_pif, "bond_master_of");
    (_vlan, "tagged_PIF"), (_pif, "VLAN_slave_of");
    (_tunnel, "access_PIF"), (_pif, "tunnel_access_PIF_of");
    (_tunnel, "transport_PIF"), (_pif, "tunnel_transport_PIF_of");

    (_pbd, "host"), (_host, "PBDs");
    (_pbd, "SR"), (_sr, "PBDs");

    (_vbd, "VDI"), (_vdi, "VBDs"); 
    (_crashdump, "VDI"), (_vdi, "crash_dumps");
(*  (_vdi, "parent"), (_vdi, "children"); *)

    (_vbd, "VM"), (_vm, "VBDs");
    (_crashdump, "VM"), (_vm, "crash_dumps");

    (* VM <-> VIF <-> network *)
    (_vif, "VM"), (_vm, "VIFs");
    (_vif, "network"), (_network, "VIFs");

    (* host <-> PIF <-> network *)
    (_pif, "host"), (_host, "PIFs");
    (_pif, "network"), (_network, "PIFs");

    (_vdi, "SR"), (_sr, "VDIs");

(*  (_alert, "task"), (_task, "alerts"); *)

    (_vtpm, "VM"), (_vm, "VTPMs");
    (_console, "VM"), (_vm, "consoles");

    (_vm, "resident_on"), (_host, "resident_VMs");
    (_hostcpu, "host"), (_host, "host_CPUs");

    (_host_crashdump, "host"), (_host, "crashdumps");
    (_host_patch, "host"), (_host, "patches");
    (_host_patch, "pool_patch"), (_pool_patch, "host_patches");

    (_subject, "roles"), (_subject, "roles");
    (*(_subject, "roles"), (_role, "subjects");*)
    (_role, "subroles"), (_role, "subroles");

    (_vm, "protection_policy"), (_vmpp, "VMs");
    (_vm, "appliance"), (_vm_appliance, "VMs");

    (_pgpu, "GPU_group"), (_gpu_group, "PGPUs");
    (_vgpu, "GPU_group"), (_gpu_group, "VGPUs");
    (_vgpu, "VM"), (_vm, "VGPUs");
    (_pci, "host"), (_host, "PCIs");
    (_pgpu, "host"), (_host, "PGPUs");
    (_pci, "attached_VMs"), (_vm, "attached_PCIs");

    (_vdi, "metadata_of_pool"), (_pool, "metadata_VDIs");
    (_sr, "introduced_by"), (_dr_task, "introduced_SRs");
  ]

(** the full api specified here *)
let all_api = Dm_api.make (all_system, all_relations)

(** These are the "emergency" calls that can be performed when a host is in "emergency mode" *)
let emergency_calls =
  [ (pool,pool_slave_reset_master);
    (pool,pool_transition_to_master); 
    (pool,pool_ping_slave);
    (session,slave_local_login);
    (session,slave_local_login_with_password);
    (session,local_logout);
    (host,host_propose_new_master);
    (host,host_commit_new_master);
    (host,host_abort_new_master);
    (host,host_local_assert_healthy);
    (host,host_signal_networking_change);
    (host,host_local_management_reconfigure);
    (host,host_ha_xapi_healthcheck);
    (host,host_emergency_ha_disable);
    (host,host_management_disable);
    (host,host_get_system_status_capabilities);
    (host,host_is_in_emergency_mode);
    (host,host_shutdown_agent);
  ]

(** Whitelist of calls that will not get forwarded from the slave to master via the unix domain socket *)
let whitelist = [ (session,session_login); 
		  (session,slave_login); 
		] @ emergency_calls

(* perform consistency checks on api at initialisation time *)
let _ = Dm_api.check all_api (List.map (fun (obj,msg) -> obj.name, msg.msg_name) emergency_calls)

(** List of classes to skip generating async handlers for *)
let no_async_messages_for = [ _session; _event; (* _alert; *) _task; _data_source; _blob ]

(** List of classes to generate 'get_all' messages for. Currently we don't
 ** allow a user to enumerate all the VBDs or VDIs directly: that must be
 ** through a VM or SR. *)
(* Note on the above: it looks like we _do_ have {VBD,VDI}.get_all! *)
let expose_get_all_messages_for = [
	_task;
	(* _alert; *)
	_host;
	_host_metrics;
	_hostcpu;
	_sr;
	_vm;
	_vm_metrics;
	_vm_guest_metrics;
	_network;
	_vif;
	_vif_metrics;
	_pif;
	_pif_metrics;
	_pbd;
	_vdi;
	_vbd;
	_vbd_metrics;
	_console;
	_crashdump;
	_host_crashdump;
	_host_patch;
	_pool;
	_sm;
	_pool_patch;
	_bond;
	_vlan;
	_blob;
	_subject;
	_role;
	_secret;
	_tunnel;
	_vmpp;
	_vm_appliance;
	_pci;
	_pgpu;
	_gpu_group;
	_vgpu;
	_dr_task;
]

let no_task_id_for = [ _task; (* _alert; *) _event ]

let current_operations_for = [ _vm; (* _vdi; _host; _sr *) ]

(*** HTTP actions ***)

type action_arg =   (* I'm not using Datamodel_types here because we need varargs *)
   String_query_arg of string |
   Int64_query_arg of string |
   Bool_query_arg of string |
   Varargs_query_arg

type http_meth = Get | Put | Post | Connect | Options
let rbac_http_permission_prefix = "http/"

(* Each action has:
   (unique public name, (HTTP method, URI, whether to expose in SDK, [args to expose in SDK], [allowed_roles], [(sub-action,allowed_roles)]))
*)
let http_actions = [
  ("get_services", (Get, Constants.services_uri, true, [], _R_READ_ONLY, []));
  ("post_services", (Post, Constants.services_uri, false, [], _R_POOL_ADMIN, []));
  ("put_services", (Put, Constants.services_uri, false, [], _R_POOL_ADMIN, []));
  ("post_remote_db_access", (Post, Constants.remote_db_access_uri, false, [], _R_POOL_ADMIN, []));
  ("post_remote_db_access_v2", (Post, Constants.remote_db_access_uri_v2, false, [], _R_POOL_ADMIN, []));
  ("connect_migrate", (Connect, Constants.migrate_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("get_services_xenops", (Get, Constants.xenops_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("post_services_xenops", (Post, Constants.xenops_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("put_services_xenops", (Put, Constants.xenops_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("get_services_sm", (Get, Constants.sm_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("post_services_sm", (Post, Constants.sm_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("put_services_sm", (Put, Constants.sm_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("put_import", (Put, Constants.import_uri, true,
		  [Bool_query_arg "restore"; Bool_query_arg "force"; String_query_arg "sr_id"], _R_VM_ADMIN, []));
  ("put_import_metadata", (Put, Constants.import_metadata_uri, true,
			   [Bool_query_arg "restore"; Bool_query_arg "force"], _R_VM_ADMIN, []));
  ("put_import_raw_vdi", (Put, Constants.import_raw_vdi_uri, true, [String_query_arg "vdi"], _R_VM_ADMIN, []));
  ("get_export", (Get, Constants.export_uri, true, [String_query_arg "uuid"], _R_VM_ADMIN, []));
  ("get_export_metadata", (Get, Constants.export_metadata_uri, true, [String_query_arg "uuid"], _R_VM_ADMIN, []));
  ("get_export_raw_vdi", (Get, Constants.export_raw_vdi_uri, true, [String_query_arg "vdi"], _R_VM_ADMIN, []));
  ("connect_console", (Connect, Constants.console_uri, false, [], _R_VM_OP, 
    [("host_console", _R_POOL_ADMIN)])); (* only _R_POOL_ADMIN can access the host/Dom0 console *)
  ("connect_console_ws", (Get, Constants.console_uri, false, [], _R_VM_OP, 
    [("host_console_ws", _R_POOL_ADMIN)])); (* only _R_POOL_ADMIN can access the host/Dom0 console *)
  ("get_root", (Get, "/", false, [], _R_READ_ONLY, []));
  ("post_cli", (Post, Constants.cli_uri, false, [], _R_READ_ONLY, []));
  ("get_host_backup", (Get, Constants.host_backup_uri, true, [], _R_POOL_ADMIN, []));
  ("put_host_restore", (Put, Constants.host_restore_uri, true, [], _R_POOL_ADMIN, []));
  ("get_host_logs_download", (Get, Constants.host_logs_download_uri, true, [], _R_POOL_OP, []));
  ("put_pool_patch_upload", (Put, Constants.pool_patch_upload_uri, true, [], _R_POOL_OP, []));
  ("get_pool_patch_download", (Get, Constants.pool_patch_download_uri, true, [String_query_arg "uuid"], _R_POOL_OP, []));
  ("put_oem_patch_stream", (Put, Constants.oem_patch_stream_uri, true, [], _R_POOL_OP, []));
  ("get_vncsnapshot", (Get, Constants.vncsnapshot_uri, true, [String_query_arg "uuid"], _R_VM_OP, 
    [("host_console", _R_POOL_ADMIN)])); (* only _R_POOL_ADMIN can snapshot host/Dom0 console *)
  ("get_pool_xml_db_sync", (Get, Constants.pool_xml_db_sync, true, [], _R_POOL_ADMIN, []));
  ("put_pool_xml_db_sync", (Put, Constants.pool_xml_db_sync, false, [], _R_POOL_ADMIN, []));
  ("get_config_sync", (Get, Constants.config_sync_uri, false, [], _R_POOL_ADMIN, []));
  ("get_vm_connect", (Get, Constants.vm_connect_uri, false, [], _R_POOL_ADMIN, []));
  ("put_vm_connect", (Put, Constants.vm_connect_uri, false, [], _R_POOL_ADMIN, []));
  ("get_system_status", (Get, Constants.system_status_uri, true,
			 [String_query_arg "entries"; String_query_arg "output"], _R_POOL_OP, []));
	(Constants.get_vm_rrd, (Get, Constants.get_vm_rrd_uri, true, [String_query_arg "uuid"], _R_READ_ONLY, []));
	(Constants.get_host_rrd, (Get, Constants.get_host_rrd_uri, true, [Bool_query_arg "json"], _R_READ_ONLY, []));
	(Constants.get_rrd_updates, (Get, Constants.get_rrd_updates_uri, true,
		[Int64_query_arg "start"; String_query_arg "cf"; Int64_query_arg "interval";
		Bool_query_arg "host"; String_query_arg "uuid"; Bool_query_arg "json"], _R_READ_ONLY, []));
  (Constants.put_rrd, (Put, Constants.put_rrd_uri, false, [], _R_POOL_ADMIN, []));
  ("get_blob", (Get, Constants.blob_uri, false, [], _R_READ_ONLY, []));
  ("put_blob", (Put, Constants.blob_uri, true, [String_query_arg "ref"], _R_VM_POWER_ADMIN, []));
  ("get_message_rss_feed", (Get, Constants.message_rss_feed, false, [], _R_POOL_ADMIN, []));  (* not enabled in xapi *)
  ("put_messages", (Put, Constants.message_put_uri, false, [], _R_VM_POWER_ADMIN, []));
  ("connect_remotecmd", (Connect, Constants.remotecmd_uri, false, [], _R_POOL_ADMIN, []));
  ("post_remote_stats", (Post, Constants.remote_stats_uri, false, [], _R_POOL_ADMIN, []));  (* deprecated *)
  ("get_wlb_report", (Get, Constants.wlb_report_uri, true, [String_query_arg "report"; Varargs_query_arg], _R_READ_ONLY, [])); (* deprecated since Clearwater *)
  ("get_wlb_diagnostics", (Get, Constants.wlb_diagnostics_uri, true, [], _R_READ_ONLY, [])); (* deprecated since Clearwater *)
  ("get_audit_log", (Get, Constants.audit_log_uri, true, [], _R_READ_ONLY, []));

  (* XMLRPC callback *)
  ("post_root", (Post, "/", false, [], _R_READ_ONLY, []));
  (* JSON callback *)
  ("post_json", (Post, Constants.json_uri, false, [], _R_READ_ONLY, []));
  ("post_root_options", (Options, "/", false, [], _R_READ_ONLY, []));
  ("post_json_options", (Options, Constants.json_uri, false, [], _R_READ_ONLY, []));
  ("post_jsonrpc", (Post, Constants.jsonrpc_uri, false, [], _R_READ_ONLY, []));
  ("post_jsonrpc_options", (Options, Constants.jsonrpc_uri, false, [], _R_READ_ONLY, []));
]

(* these public http actions will NOT be checked by RBAC *)
(* they are meant to be used in exceptional cases where RBAC is already *)
(* checked inside them, such as in the XMLRPC (API) calls *)
let public_http_actions_with_no_rbac_check =
	[
		"post_root"; (* XMLRPC (API) calls -> checks RBAC internally *)
		"post_cli";  (* CLI commands -> calls XMLRPC *)
		"post_json"; (* JSON -> calls XMLRPC *)
		"get_root";  (* Make sure that downloads, personal web pages etc do not go through RBAC asking for a password or session_id *)
		             (* also, without this line, quicktest_http.ml fails on non_resource_cmd and bad_resource_cmd with a 401 instead of 404 *)
		"get_blob"; (* Public blobs don't need authentication *)
		"post_root_options"; (* Preflight-requests are not RBAC checked *)
		"post_json_options"; 
		"post_jsonrpc";
		"post_jsonrpc_options";
	]

(* permissions not associated with any object message or field *)
let extra_permissions = [
	(extra_permission_task_destroy_any, _R_POOL_OP); (* only POOL_OP can destroy any tasks *)
]
