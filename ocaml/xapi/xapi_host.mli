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
(** Module that defines API functions for Host objects
 * @group XenAPI functions
*)

(** {2 (Fill in Title!)} *)

val set_emergency_mode_error : string -> string list -> unit
(** When starting xapi we begin in 'emergency mode' and hope to transition out of it by contacting
    the master etc. As we make progress we set the 'emergency_mode_error' which is the error returned
    by the CLI, indicating stuff like: failure to get a management IP address, the master doesn't
    recognise us etc. *)

val local_assert_healthy : __context:'a -> unit

val set_power_on_mode :
  __context:Context.t ->
  self:[ `host ] Ref.t -> power_on_mode: string -> power_on_config:(string * string) list -> unit

val bugreport_upload :
  __context:'a ->
  host:'b -> url:string -> options:(string * string) list -> unit

val signal_networking_change : __context:Context.t -> unit
val signal_cdrom_event : __context:Context.t -> string -> unit
val notify : __context:Context.t -> ty:string -> params:string -> unit

(** {2 (Fill in title!)} *)

val assert_can_evacuate : __context:Context.t -> host:API.ref_host -> unit
val get_vms_which_prevent_evacuation :
  __context:Context.t -> self:API.ref_host -> (API.ref_VM * string list) list
val evacuate : __context:Context.t -> host:API.ref_host -> unit
val retrieve_wlb_evacuate_recommendations :
  __context:Context.t -> self:API.ref_host -> (API.ref_VM * string list) list

(** {2 (Fill in title!)} *)

val restart_agent : __context:'a -> host:'b -> unit
val shutdown_agent : __context:'a -> unit
val disable : __context:Context.t -> host:[ `host ] Ref.t -> unit
val enable : __context:Context.t -> host:[ `host ] Ref.t -> unit
val shutdown : __context:Context.t -> host:[ `host ] Ref.t -> unit
val reboot : __context:Context.t -> host:[ `host ] Ref.t -> unit
val power_on : __context:Context.t -> host:[ `host ] Ref.t -> unit
val dmesg : __context:Context.t -> host:'b -> string
val dmesg_clear : __context:'a -> host:'b -> 'c
val get_log : __context:'a -> host:'b -> 'c
val send_debug_keys : __context:Context.t -> host:'b -> keys:string -> unit
val list_methods : __context:'a -> 'b
val is_slave : __context:'a -> host:'b -> bool

(** Contact the host and return whether it is a slave or not.
    If the host is dead then one of the xmlrpcclient exceptions will be thrown *)
val ask_host_if_it_is_a_slave :
  __context:Context.t -> host:API.ref_host -> bool

(** Returns true if a host is alive, false otherwise. Note that if a host has already been marked
    as dead by the GC thread then this is treated as definitive. Otherwise attempt to contact the host
    to make sure. *)
val is_host_alive : __context:Context.t -> host:API.ref_host -> bool

val create :
  __context:Context.t ->
  uuid:string ->
  name_label:string ->
  name_description:string ->
  hostname:string ->
  address:string ->
  external_auth_type:string ->
  external_auth_service_name:string ->
  external_auth_configuration:(string * string) list ->
  license_params:(string * string) list ->
  edition:string ->
  license_server:(string * string) list ->
  local_cache_sr:[ `SR ] Ref.t ->
  chipset_info:(string * string) list ->
  ssl_legacy:bool ->
  [ `host ] Ref.t
val destroy : __context:Context.t -> self:API.ref_host -> unit
val declare_dead : __context:Context.t -> host:API.ref_host -> unit
val ha_disable_failover_decisions : __context:'a -> host:'b -> unit
val ha_disarm_fencing : __context:'a -> host:'b -> unit
val ha_stop_daemon : __context:'a -> host:'b -> unit
val ha_release_resources : __context:Context.t -> host:'a -> unit
val ha_wait_for_shutdown_via_statefile : __context:'a -> host:'b -> unit
val ha_xapi_healthcheck : __context:'a -> bool
val preconfigure_ha :
  __context:Context.t ->
  host:[ `host ] Ref.t ->
  statefiles:API.ref_VDI list ->
  metadata_vdi:[ `VDI ] Ref.t -> generation:string -> unit
val ha_join_liveset : __context:'a -> host:'b Ref.t -> unit
val propose_new_master : __context:'a -> address:string -> manual:'b -> unit
val commit_new_master : __context:Context.t -> address:string -> unit
val abort_new_master : __context:'a -> address:string -> unit
val update_master : __context:'a -> host:'b -> master_address:'c -> 'd
val emergency_ha_disable : __context:'a -> soft:bool -> unit
val request_backup :
  __context:Context.t -> host:API.ref_host -> generation:int64 -> force:bool -> unit
val request_config_file_sync : __context:'a -> host:'b -> hash:string -> unit
val syslog_reconfigure : __context:Context.t -> host:'a -> unit

(** {2 Management Interface} *)

val get_management_interface : __context:Context.t -> host:API.ref_host -> API.ref_PIF
val change_management_interface : __context:Context.t -> string -> [ `IPv4 | `IPv6 ] -> unit
val local_management_reconfigure :
  __context:Context.t -> interface:string -> unit
val management_reconfigure :
  __context:Context.t -> pif:[ `PIF ] Ref.t -> unit
val management_disable : __context:Context.t -> unit

(** {2 (Fill in title!)} *)

val get_system_status_capabilities :
  __context:Context.t -> host:API.ref_host -> string
val get_diagnostic_timing_stats :
  __context:Context.t -> host:'b -> (string * string) list
val set_hostname_live :
  __context:Context.t -> host:[ `host ] Ref.t -> hostname:string -> unit
val is_in_emergency_mode : __context:'a -> bool

val set_stunnel_legacy :
  __context:Context.t -> bool -> unit
val set_ssl_legacy :
  __context:Context.t -> self:[ `host ] API.Ref.t -> value:bool -> unit
val compute_free_memory :
  __context:Context.t -> host:[ `host ] Ref.t -> int64
val compute_memory_overhead :
  __context:Context.t -> host:API.ref_host -> int64
val get_data_sources : __context:'a -> host:'b -> API.data_source_t list
val record_data_source :
  __context:'a -> host:'b -> data_source:string -> unit
val query_data_source :
  __context:'a -> host:'b -> data_source:string -> float
val forget_data_source_archives :
  __context:'a -> host:'b -> data_source:string -> unit
val tickle_heartbeat :
  __context:Context.t ->
  host:API.ref_host -> stuff:(string * string) list -> 'a list
val create_new_blob :
  __context:Context.t ->
  host:[ `host ] Ref.t -> name:string -> mime_type:string -> public:bool -> [ `blob ] Ref.t
val serialize_host_enable_disable_extauth : Mutex.t
val extauth_hook_script_name : string
val call_extauth_plugin_nomutex :
  __context:Context.t ->
  host:[ `host ] Ref.t -> fn:string -> args:(string * string) list -> string
val call_extauth_plugin :
  __context:Context.t ->
  host:[ `host ] Ref.t -> fn:string -> args:(string * string) list -> string
val call_plugin :
  __context:Context.t ->
  host:[ `host ] Ref.t ->
  plugin:string -> fn:string -> args:(string * string) list -> string
val call_extension :
  __context:Context.t ->
  host:[ `host ] Ref.t -> call:string -> Rpc.t
val has_extension :
  __context:Context.t ->
  host:[ `host ] Ref.t -> name:string -> bool
val sync_data : __context:Context.t -> host:API.ref_host -> unit
val backup_rrds : __context:Context.t -> host:'b -> delay:float -> unit
val get_servertime : __context:'a -> host:'b -> Stdext.Date.iso8601
val get_server_localtime : __context:'a -> host:'b -> Stdext.Date.iso8601
val enable_binary_storage :
  __context:Context.t -> host:[ `host ] Ref.t -> unit
val disable_binary_storage :
  __context:Context.t -> host:[ `host ] Ref.t -> unit
val get_uncooperative_resident_VMs : __context:Context.t -> self:[`host] Ref.t -> API.ref_VM_set
val get_uncooperative_domains : __context:Context.t -> self:[`host] Ref.t -> string list
val certificate_install :
  __context:'a -> host:'b -> name:string -> cert:string -> unit
val certificate_uninstall : __context:'a -> host:'b -> name:string -> unit
val certificate_list : __context:'a -> host:'b -> string list
val crl_install :
  __context:'a -> host:'b -> name:string -> crl:string -> unit
val crl_uninstall : __context:'a -> host:'b -> name:string -> unit
val crl_list : __context:'a -> host:'b -> string list
val certificate_sync : __context:'a -> host:'b -> unit
val get_server_certificate : __context:'a -> host:'b -> string
val detect_nonhomogeneous_external_auth_in_host :
  __context:Context.t -> host:API.ref_host -> unit
val enable_external_auth :
  __context:Context.t ->
  host:API.ref_host ->
  config:(string * string) list ->
  service_name:string -> auth_type:string -> unit
val disable_external_auth_common :
  ?during_pool_eject:bool ->
  __context:Context.t ->
  host:API.ref_host -> config:(string * string) list -> unit
val disable_external_auth :
  __context:Context.t ->
  host:API.ref_host -> config:(string * string) list -> unit


(** {2 Static VDIs} *)

(** Make the given VDIs static on the host, such that they will automatically be attached
 * when xapi is restarted on this host. Supply a [reason] string for each VDI to be
 * included. *)
val attach_static_vdis :
  __context:Context.t ->
  host:API.ref_host -> vdi_reason_map:([ `VDI ] Ref.t * string) list -> unit

(** Remove the given VDIs from the list of static VDIs on the host. *)
val detach_static_vdis :
  __context:Context.t -> host:API.ref_host -> vdis:API.ref_VDI list -> unit


(** {2 Local Database} *)

(** Set a key in the Local DB of the host. *)
val set_localdb_key : __context:Context.t -> host:API.ref_host -> key:string -> value:string -> unit


(** {2 Secrets} *)

val update_pool_secret :
  __context:'a -> host:'b -> pool_secret:string -> unit


(** {2 Supplemental Packs} *)

(** Refresh the list of Supplemental Packs in the host.software_version field. *)
val refresh_pack_info : __context:Context.t -> host:API.ref_host -> unit


(** {2 Licensing} *)

(** Called by post-floodgate slaves to update the database AND recompute the pool_sku on the master *)
val set_license_params :
  __context:Context.t ->
  self:[ `host ] Ref.t -> value:(string * string) list -> unit

val copy_license_to_db :
  __context:Context.t ->
  host:[ `host ] Ref.t ->
  features:Features.feature list -> additional:(string * string) list -> unit

val license_add : __context:Context.t -> host:API.ref_host -> contents:string -> unit

val license_remove : __context:Context.t -> host:API.ref_host -> unit

(** Attempt to activate the given edition.
 *  In needed, the function automatically checks v6 licenses in and out
 *  from the license server (via the v6 daemon). If the requested edition is not
 *  available, the call will fail with an exception, leaving the edition as it is.
 *  Also call this function to change to a different license server, after the
 *  connection details in host.license_server have been amended. *)
val apply_edition : __context:Context.t -> host:API.ref_host -> edition:string -> force:bool -> unit
val apply_edition_internal : __context:Context.t -> host:API.ref_host ->
  edition:string -> additional:(string * string) list -> unit

(** {2 CPU Feature Masking} *)

(** Control the local caching behaviour of the host *)
val enable_local_storage_caching : __context:Context.t -> host:API.ref_host -> sr:API.ref_SR -> unit
val disable_local_storage_caching : __context:Context.t -> host:API.ref_host -> unit

(** Purge all network-related metadata associated with the given host. *)
val reset_networking : __context:Context.t -> host:API.ref_host -> unit

(** Query diagnostics from the SM layer *)
val get_sm_diagnostics : __context:Context.t -> host:API.ref_host -> string

(** Query diagnostics about running threads *)
val get_thread_diagnostics : __context:Context.t -> host:API.ref_host -> string

(** Attempt to cleanup and destroy an SM datapath *)
val sm_dp_destroy : __context:Context.t -> host:API.ref_host -> dp:string -> allow_leak:bool -> unit

(** Synchronise slave VLANs with master *)
val sync_vlans : __context:Context.t -> host:API.ref_host -> unit

(** Synchronise slave tunnels with master *)
val sync_tunnels : __context:Context.t -> host:API.ref_host -> unit

(** Synchronise slave network sriovs with master *)
val sync_network_sriovs : __context:Context.t -> host:API.ref_host -> unit

(** Synchronise PIF.currently_attached fields on given host.
 *  The parameter [bridges] contains a list of bridge names reflecting all bridges that are up. *)
val sync_pif_currently_attached : __context:Context.t -> host:API.ref_host -> bridges:string list -> unit

val migrate_receive : __context:Context.t -> host:API.ref_host -> network:API.ref_network -> options:API.string_to_string_map -> API.string_to_string_map

val enable_display : __context:Context.t -> host:API.ref_host -> API.host_display

val disable_display : __context:Context.t -> host:API.ref_host -> API.host_display

val sync_display : __context:Context.t -> host:API.ref_host -> unit

val apply_guest_agent_config : __context:Context.t -> host:API.ref_host -> unit

(* See Xapi_pgpu.mxgpu_vf_setup *)
val mxgpu_vf_setup : __context:Context.t -> host:API.ref_host -> unit

val allocate_resources_for_vm : __context:Context.t -> self:API.ref_host -> vm:API.ref_VM -> live:bool -> unit
