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
module D = Debug.Debugger(struct let name="xapi" end)
open D

let vmpr_plugin = "vmpr"

(*
    val protect_now : __context:Context.t -> self:ref_VMPP -> unit
    val archive_now : __context:Context.t -> self:ref_VM -> unit
    val test_archive_settings :
      __context:Context.t -> settings:API.string_to_string_map -> unit
    val create :
      __context:Context.t ->
      name_label:string ->
      name_description:string ->
      is_policy_enabled:bool ->
      backup_frequency:API.vmpp_backup_frequency ->
      backup_retention_value:int64 ->
      backup_schedule:API.string_to_string_map ->
      backup_last_run_time:API.datetime ->
      archive_target_config_type:API.vmpp_archive_target_config_type ->
      archive_target_config:API.string_to_string_map ->
      archive_frequency:API.vmpp_archive_frequency ->
      archive_schedule:API.string_to_string_map ->
      archive_last_run_time:API.datetime ->
      is_alarm_enabled:bool ->
      alarm_config:API.string_to_string_map -> API.ref_VMPP
    val destroy : __context:Context.t -> self:API.ref_VMPP -> unit
*)

let create ~__context ~name_label ~name_description ~is_policy_enabled
  ~backup_type ~backup_retention_value ~backup_frequency ~backup_schedule ~backup_last_run_time
  ~archive_target_type ~archive_target_config ~archive_frequency ~archive_schedule  ~archive_last_run_time
  ~is_alarm_enabled ~alarm_config
: API.ref_VMPP =
  let ref=Ref.make() in
  let uuid=Uuid.to_string (Uuid.make_uuid()) in
  Db.VMPP.create ~__context ~ref ~uuid
    ~name_label ~name_description ~is_policy_enabled
    ~backup_type ~backup_retention_value
    ~backup_frequency ~backup_schedule ~backup_last_run_time
    ~is_backup_running:false ~is_archive_running:false
    ~archive_target_config ~archive_target_type
    ~archive_frequency ~archive_schedule ~archive_last_run_time
    ~is_alarm_enabled ~alarm_config ~recent_alerts:[];
  ref

let destroy ~__context ~self = 
  Db.VMPP.destroy ~__context ~self

let protect_now ~__context ~vmpp = 
  let vmpp_uuid = Db.VMPP.get_uuid ~__context ~self:vmpp in
  let args = [ "vmpp_uuid", vmpp_uuid ] in
  Xapi_plugins.call_plugin
    (Context.get_session_id __context)
    vmpr_plugin
    "protect_now"
    args

let archive_now ~__context ~snapshot = ""
(*
  let archive_target_config 
  Xapi_plugins.call_plugin
    (Context.get_session_id __context)
    vmpr_plugin
    "mount_archive_target"
    args

  Xapi_plugins.call_plugin
    (Context.get_session_id __context)
    vmpr_plugin
    "unmount_archive_target"
    args
*)

let set_is_backup_running ~__context ~self ~value =
  Db.VMPP.set_is_backup_running ~__context ~self ~value
let set_is_archive_running ~__context ~self ~value =
  Db.VMPP.set_is_archive_running ~__context ~self ~value


