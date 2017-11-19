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
(** Module that defines API functions for VM objects
 * @group XenAPI functions
*)

(** {2 (Fill in Title!)} *)

exception InvalidOperation of string
val assert_operation_valid :
  __context:Context.t -> self:[ `VM ] Ref.t -> op:API.vm_operations -> unit
val update_allowed_operations :
  __context:Context.t -> self:[ `VM ] Ref.t -> unit
val assert_can_boot_here :
  __context:Context.t -> self:[ `VM ] Ref.t -> host:API.ref_host -> unit
val retrieve_wlb_recommendations :
  __context:Context.t ->
  vm:[ `VM ] Ref.t -> (API.ref_host * string list) list
val assert_agile : __context:Context.t -> self:[ `VM ] Ref.t -> unit
val immediate_complete : __context:Context.t -> unit
val set_actions_after_crash :
  __context:Context.t ->
  self:[ `VM ] Ref.t ->
  value:[< `coredump_and_destroy
        | `coredump_and_restart
        | `destroy
        | `preserve
        | `rename_restart
        | `restart ] ->
  unit
val set_is_a_template :
  __context:Context.t -> self:[ `VM ] Ref.t -> value:bool -> unit
val set_is_default_template :
  __context:Context.t -> vm:[ `VM ] Ref.t -> value:bool -> unit
val validate_restart_priority : string -> unit
val set_ha_always_run :
  __context:Context.t -> self:API.ref_VM -> value:bool -> unit
val set_ha_restart_priority :
  __context:Context.t -> self:API.ref_VM -> value:string -> unit
val compute_memory_overhead :
  __context:Context.t -> vm:[ `VM ] Ref.t -> int64
val set_memory_static_range :
  __context:Context.t ->
  self:[ `VM ] Ref.t -> min:Int64.t -> max:Int64.t -> unit
val set_memory_dynamic_min : __context:'a -> self:'b -> value:'c -> 'd
val set_memory_dynamic_max : __context:'a -> self:'b -> value:'c -> 'd
val set_memory_static_min : __context:'a -> self:'b -> value:'c -> 'd
val set_memory_static_max : __context:'a -> self:'b -> value:'c -> 'd
val set_memory_limits :
  __context:Context.t ->
  self:[ `VM ] Ref.t ->
  static_min:Int64.t ->
  static_max:Int64.t -> dynamic_min:Int64.t -> dynamic_max:Int64.t -> unit
val set_memory : __context:Context.t -> self:[ `VM ] Ref.t -> value:int64 -> unit
val assert_not_ha_protected : __context:Context.t -> vm:[ `VM ] Ref.t -> unit
val pause : __context:Context.t -> vm:API.ref_VM -> unit
val unpause : __context:Context.t -> vm:API.ref_VM -> unit
val set_xenstore_data : __context:Context.t -> self:API.ref_VM -> value:(string * string) list -> unit
val start :
  __context:Context.t ->
  vm:API.ref_VM -> start_paused:bool -> force:bool -> unit
val assert_host_is_localhost : __context:Context.t -> host:API.ref_host -> unit
val start_on :
  __context:Context.t ->
  vm:API.ref_VM -> host:API.ref_host -> start_paused:bool -> force:bool -> unit
val hard_reboot : __context:Context.t -> vm:API.ref_VM -> unit
val hard_shutdown : __context:Context.t -> vm:API.ref_VM -> unit
val clean_reboot : __context:Context.t -> vm:API.ref_VM -> unit
val clean_shutdown : __context:Context.t -> vm:API.ref_VM -> unit
val shutdown : __context:Context.t -> vm:API.ref_VM -> unit
val hard_reboot_internal : __context:Context.t -> vm:API.ref_VM -> unit
val power_state_reset : __context:Context.t -> vm:API.ref_VM -> unit
val suspend : __context:Context.t -> vm:API.ref_VM -> unit
val resume :
  __context:Context.t ->
  vm:API.ref_VM -> start_paused:bool -> force:bool -> unit
val resume_on :
  __context:Context.t ->
  vm:API.ref_VM -> host:API.ref_host -> start_paused:bool -> force:bool -> unit
val create :
  __context:Context.t ->
  name_label:string ->
  name_description:string ->
  user_version:int64 ->
  is_a_template:bool ->
  affinity:[ `host ] Ref.t ->
  memory_target:int64 ->
  memory_static_max:int64 ->
  memory_dynamic_max:int64 ->
  memory_dynamic_min:int64 ->
  memory_static_min:int64 ->
  vCPUs_params:(string * string) list ->
  vCPUs_max:int64 ->
  vCPUs_at_startup:int64 ->
  actions_after_shutdown:[< `destroy | `restart ] ->
  actions_after_reboot:[< `destroy | `restart ] ->
  actions_after_crash:[< `coredump_and_destroy
                      | `coredump_and_restart
                      | `destroy
                      | `preserve
                      | `rename_restart
                      | `restart ] ->
  pV_bootloader:string ->
  pV_kernel:string ->
  pV_ramdisk:string ->
  pV_args:string ->
  pV_bootloader_args:string ->
  pV_legacy_args:string ->
  hVM_boot_policy:string ->
  hVM_boot_params:(string * string) list ->
  hVM_shadow_multiplier:float ->
  platform:(string * string) list ->
  pCI_bus:string ->
  other_config:(string * string) list ->
  recommendations:string ->
  xenstore_data:(string * string) list ->
  ha_always_run:bool ->
  ha_restart_priority:string ->
  tags:string list -> blocked_operations:'a ->
  protection_policy:[ `VMPP ] Ref.t ->
  is_snapshot_from_vmpp:bool ->
  snapshot_schedule:[ `VMSS ] Ref.t ->
  is_vmss_snapshot:bool ->
  appliance:API.ref_VM_appliance ->
  start_delay:int64 ->
  shutdown_delay:int64 ->
  order:int64 ->
  suspend_SR:[ `SR ] Ref.t ->
  version:int64 ->
  generation_id:string ->
  hardware_platform_version:int64 ->
  has_vendor_device:bool ->
  reference_label:string ->
  domain_type:API.domain_type
  -> API.ref_VM
val destroy : __context:Context.t -> self:[ `VM ] Ref.t -> unit
val clone :
  __context:Context.t -> vm:API.ref_VM -> new_name:string -> [ `VM ] Ref.t
val snapshot :
  __context:Context.t -> vm:API.ref_VM -> new_name:string -> [ `VM ] Ref.t
val snapshot_with_quiesce :
  __context:Context.t -> vm:[ `VM ] Ref.t -> new_name:string -> [ `VM ] Ref.t
val revert : __context:Context.t -> snapshot:[ `VM ] Ref.t -> unit
val checkpoint :
  __context:Context.t -> vm:API.ref_VM -> new_name:string -> [ `VM ] Ref.t
val copy :
  __context:Context.t ->
  vm:API.ref_VM -> new_name:string -> sr:API.ref_SR -> [ `VM ] Ref.t
val provision : __context:Context.t -> vm:API.ref_VM -> unit
val set_VCPUs_max :
  __context:Context.t -> self:[ `VM ] Ref.t -> value:int64 -> unit
val set_VCPUs_at_startup :
  __context:Context.t -> self:[ `VM ] Ref.t -> value:int64 -> unit
val set_VCPUs_number_live :
  __context:Context.t -> self:API.ref_VM -> nvcpu:int64 -> unit
val add_to_VCPUs_params_live :
  __context:'a -> self:API.ref_VM -> key:'b -> value:'c -> 'd
val set_memory_dynamic_range :
  __context:Context.t ->
  self:API.ref_VM -> min:Int64.t -> max:Int64.t -> unit
val set_memory_target_live :
  __context:'a -> self:API.ref_VM -> target:'b -> unit
val wait_memory_target_live : __context:Context.t -> self:API.ref_VM -> unit
val get_cooperative : __context:Context.t -> self:[ `VM ] Ref.t -> bool
val set_HVM_shadow_multiplier :
  __context:Context.t -> self:[ `VM ] Ref.t -> value:float -> unit
val set_shadow_multiplier_live :
  __context:Context.t -> self:API.ref_VM -> multiplier:float -> unit
val send_sysrq : __context:'a -> vm:API.ref_VM -> key:'b -> 'c
val send_trigger : __context:'a -> vm:API.ref_VM -> trigger:'b -> 'c
val get_boot_record : __context:Context.t -> self:API.ref_VM -> API.vM_t
val get_data_sources :
  __context:Context.t -> self:[ `VM ] Ref.t -> API.data_source_t list
val record_data_source :
  __context:Context.t -> self:[ `VM ] Ref.t -> data_source:string -> unit
val query_data_source :
  __context:Context.t -> self:[ `VM ] Ref.t -> data_source:string -> float
val forget_data_source_archives :
  __context:Context.t -> self:[ `VM ] Ref.t -> data_source:string -> unit
val get_possible_hosts :
  __context:Context.t -> vm:API.ref_VM -> API.ref_host list
val get_allowed_VBD_devices :
  __context:Context.t -> vm:[ `VM ] Ref.t -> string list
val get_allowed_VIF_devices :
  __context:Context.t -> vm:[ `VM ] Ref.t -> string list
val csvm : __context:Context.t -> vm:API.ref_VM -> [ `VM ] Ref.t
val maximise_memory :
  __context:Context.t ->
  self:[ `VM ] Ref.t -> total:int64 -> approximate:bool -> int64
val atomic_set_resident_on : __context:'a -> vm:'b -> host:'c -> 'd
val update_snapshot_metadata :
  __context:'a -> vm:'b -> snapshot_of:'c -> snapshot_time:'d -> 'e
val create_new_blob :
  __context:Context.t ->
  vm:[ `VM ] Ref.t -> name:string -> mime_type:string -> public:bool -> [ `blob ] Ref.t

(** {2 Experimental support for S3 suspend/ resume} *)

val s3_suspend : __context:Context.t -> vm:API.ref_VM -> unit
val s3_resume : __context:Context.t -> vm:API.ref_VM -> unit

(** {2 BIOS strings} *)
val set_bios_strings :
  __context:Context.t -> self:[ `VM ] Ref.t -> value:(string * string) list -> unit
val copy_bios_strings :
  __context:Context.t -> vm:[ `VM ] Ref.t -> host:[ `host ] Ref.t -> unit
(** Copy the BIOS strings from a host to the VM, unless the VM's BIOS strings
 *  had already been set. *)

val set_protection_policy : __context:Context.t -> self:API.ref_VM -> value:API.ref_VMPP -> unit
val set_snapshot_schedule : __context:Context.t -> self:API.ref_VM -> value:API.ref_VMSS -> unit

val set_start_delay : __context:Context.t -> self:API.ref_VM -> value:int64 -> unit
val set_shutdown_delay : __context:Context.t -> self:API.ref_VM -> value:int64 -> unit
val set_order : __context:Context.t -> self:API.ref_VM -> value:int64 -> unit

val assert_can_be_recovered : __context:Context.t -> self:API.ref_VM -> session_to:API.ref_session -> unit
val get_SRs_required_for_recovery : __context:Context.t -> self:API.ref_VM -> session_to:API.ref_session ->API.ref_SR list
val recover : __context:Context.t -> self:API.ref_VM ->
  session_to:API.ref_session -> force:bool -> unit
val set_suspend_VDI : __context:Context.t -> self:API.ref_VM ->
  value:API.ref_VDI -> unit
val set_appliance : __context:Context.t -> self:API.ref_VM -> value:API.ref_VM_appliance -> unit
val import_convert : __context:Context.t -> _type:string -> username:string -> password:string ->
  sr:API.ref_SR -> remote_config:(string * string) list -> unit

(** [query_services __context self] returns a Map of service type -> name label provided
    	by the specific VM. *)
val query_services : __context:Context.t -> self:API.ref_VM -> (string * string) list

val request_rdp_on : __context:Context.t -> vm:API.ref_VM -> unit
val request_rdp_off: __context:Context.t -> vm:API.ref_VM -> unit

val call_plugin : __context:Context.t -> vm:API.ref_VM -> plugin:string -> fn:string -> args:(string * string) list -> string

val set_has_vendor_device : __context:Context.t -> self:API.ref_VM -> value:bool -> unit
val assert_can_set_has_vendor_device : __context:Context.t -> self:API.ref_VM -> value:bool -> unit

val import : __context:Context.t -> url:string -> sr:API.ref_SR -> full_restore:bool -> force:bool -> API.ref_VM list
