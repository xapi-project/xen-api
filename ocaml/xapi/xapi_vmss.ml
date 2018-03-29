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
module D = Debug.Make(struct let name="xapi" end)
open D

open Map_check

let vmss_plugin = "vmss"
let vmss_username = "__dom0__vmss"
let vmss_snapshot_other_config_show_in_xencenter = "ShowInXenCenter"
let vmss_snapshot_other_config_applies_to = "applies_to"

let assert_licensed ~__context =
	Pool_features.assert_enabled ~__context ~f:Features.VMSS

(* Create VM snapshots just after creating a VMSS *)
let snapshot_now ~__context ~vmss =
  assert_licensed ~__context;
  let vmss_uuid = Db.VMSS.get_uuid ~__context ~self:vmss in
  let args = [ "vmss_uuid", vmss_uuid ] in
  Xapi_plugins.call_plugin
    (Context.get_session_id __context)
    vmss_plugin
    "snapshot_now"
    args

(* mini datamodel for type and key value restrictions in the vmss schedule map fields *)
let schedule_days_enum = ["Monday";"Tuesday";"Wednesday";"Thursday";"Friday";"Saturday";"Sunday"]
let schedule_frequency_hourly = "hourly"
let schedule_frequency_daily = "daily"
let schedule_frequency_weekly = "weekly"
let schedule_min_enum = ["0";"15";"30";"45"]
let schedule_field = "schedule"
let schedule_min_default = List.hd schedule_min_enum
let schedule_hour_default = "0"
let schedule_days_default = List.hd schedule_days_enum

let schedule_frequency_hourly_keys = schedule_field,[schedule_frequency_hourly,[Datamodel.VMSS.schedule_min, ((Enum schedule_min_enum), schedule_min_default)]]
let schedule_frequency_daily_keys = schedule_field,[schedule_frequency_daily,[Datamodel.VMSS.schedule_hour, ((IntRange(0,23)), schedule_hour_default);Datamodel.VMSS.schedule_min, ((Enum schedule_min_enum), schedule_min_default)]]
let schedule_frequency_weekly_keys = schedule_field,[schedule_frequency_weekly,[Datamodel.VMSS.schedule_hour, ((IntRange(0,23)), schedule_hour_default);Datamodel.VMSS.schedule_min, ((Enum schedule_min_enum), schedule_min_default);Datamodel.VMSS.schedule_days, ((EnumSet schedule_days_enum), schedule_days_default)]]

(* look-up structures, contain allowed map keys in a specific map type *)
let schedule_keys = schedule_field, (List.map
  (function (f,[k]) -> k
    | _ -> assert false
  )
  [schedule_frequency_hourly_keys;schedule_frequency_daily_keys;schedule_frequency_weekly_keys])

(* look-up structures, contain allowed map keys in all map types *)
let schedule_all_keys = schedule_field,["",(List.fold_left (fun acc (sf,ks)->acc@ks) [] (let (f,kss)=schedule_keys in kss))]

(* assert VMSS frequency *)
let assert_set_frequency ~frequency ~schedule=
  let ty = Record_util.vmss_frequency_to_string frequency in
  Map_check.assert_all_keys ~ty ~ks:schedule_keys ~value:schedule ~db:schedule

(* assert VMSS retained_snapshots count *)
let assert_retained_snapshots ~retained_snapshots =
  let value = retained_snapshots in
  (if (value < 1L) || (value > 10L)
  then
    err "retained_snapshots" "" (Printf.sprintf "%Li" value)
  )

let set_frequency ~__context ~self ~value =
  assert_licensed ~__context;
  let schedule = Db.VMSS.get_schedule ~__context ~self in
  let new_schedule = assert_set_frequency ~frequency:value ~schedule in
  Db.VMSS.set_frequency ~__context ~self ~value;
  (* update dependent maps *)
  Db.VMSS.set_schedule ~__context ~self ~value:new_schedule

let set_schedule ~__context ~self ~value =
  assert_licensed ~__context;
  let value = Map_check.assert_keys ~ty:"" ~ks:schedule_all_keys ~value ~db:(Db.VMSS.get_schedule ~__context ~self) in
  Db.VMSS.set_schedule ~__context ~self ~value

let set_type ~__context ~self ~value =
  assert_licensed ~__context;
  (* For VMSS snapshot_type=snapshot_with_quiesce, Check VMs supports the snapshot_with_quiesce *)
  if value = `snapshot_with_quiesce then begin
    Pool_features.assert_enabled ~__context ~f:Features.VSS;
    Db.VMSS.get_VMs ~__context ~self
    |> List.iter (fun vm ->
       Xapi_vm_helpers.assert_vm_supports_quiesce_snapshot ~__context ~self:vm
    )
  end;
  Db.VMSS.set_type ~__context ~self ~value

(* Workaround for `param-set` calling `remove_from_schedule` first then `add_to_schedule`
 * In case `value` supplied is invalid for `add_to_schedule` it must not remove the key
 * We need the cache the original value before removing the key
 * *)
let schedule_backup = ref []

let remove_from_schedule ~__context ~self ~key =
  assert_licensed ~__context;
  schedule_backup := Db.VMSS.get_schedule ~__context ~self;
  Db.VMSS.remove_from_schedule ~__context ~self ~key

let add_to_schedule ~__context ~self ~key ~value =
  assert_licensed ~__context;
  try
    let value = List.assoc key (Map_check.assert_keys ~ty:"" ~ks:schedule_all_keys ~value:[(key,value)] ~db:(Db.VMSS.get_schedule ~__context ~self)) in
    Db.VMSS.add_to_schedule ~__context ~self ~key ~value
  with e ->
    Db.VMSS.add_to_schedule ~__context ~self ~key ~value:(List.assoc key !schedule_backup);
    raise e

let set_last_run_time ~__context ~self ~value =
  assert_licensed ~__context;
  Db.VMSS.set_last_run_time ~__context ~self ~value

let set_retained_snapshots ~__context ~self ~value =
  assert_licensed ~__context;
  assert_retained_snapshots ~retained_snapshots:value;
  Db.VMSS.set_retained_snapshots ~__context ~self ~value

(* VMSS constructors/destructors *)

let create ~__context ~name_label ~name_description ~enabled
  ~_type ~retained_snapshots ~frequency ~schedule
  : API.ref_VMSS =

  assert_licensed ~__context;
  if _type = `snapshot_with_quiesce then
    Pool_features.assert_enabled ~__context ~f:Features.VSS;
  (* assert all provided field values, key names and key values are valid *)
  let (_: (string*string) list) = Map_check.assert_keys ~ty:(Record_util.vmss_frequency_to_string frequency) ~ks:schedule_keys ~value:schedule ~db:[] in

  (* assert inter-field constraints and fix values if possible *)
  let schedule = assert_set_frequency ~frequency ~schedule in

  (* assert retained_snapshots *)
  assert_retained_snapshots ~retained_snapshots;
  let ref=Ref.make() in
  let uuid=Uuid.to_string (Uuid.make_uuid()) in
  Db.VMSS.create ~__context ~ref ~uuid
    ~name_label ~name_description ~enabled ~_type
    ~retained_snapshots ~frequency ~schedule ~last_run_time:(Stdext.Date.of_float 0.);
  ref

let destroy_all_messages ~__context ~self =
  let uuid = Db.VMSS.get_uuid ~__context ~self in
  Xapi_message.get_all_records ~__context
    |> List.filter (fun (_, record) -> record.API.message_obj_uuid = uuid)
    |> List.iter (fun (ref, _) -> Xapi_message.destroy ~__context ~self:ref)

let destroy ~__context ~self =
  assert_licensed ~__context;
  let vms = Db.VMSS.get_VMs ~__context ~self in
  if List.length vms > 0
  then ( (* we can't delete a VMSS that contains VMs *)
    raise (Api_errors.Server_error (Api_errors.vmss_has_vm,[]))
  )
  else (
    destroy_all_messages ~__context ~self;
    Db.VMSS.destroy ~__context ~self
  )

(* Verify if snapshot is happening due to a VM Schedule Snapshot *)
let is_vmss_snapshot ~__context =
  try
    (let session = Xapi_session.get_top ~__context ~self:(Context.get_session_id __context) in
    let uname = Db.Session.get_auth_user_name ~__context ~self:session in
    let is_lsu = Db.Session.get_is_local_superuser ~__context ~self:session in
    is_lsu && (uname = vmss_username)
    )
  with e ->
    debug "Error obtaining is_vmss_snapshot: %s" (Printexc.to_string e);
    false

let show_task_in_xencenter ~__context ~vm =
  if is_vmss_snapshot ~__context then
    (
      let task = Context.get_task_id __context in
      try
        debug "show_in_xencenter: task=%s" (Ref.string_of task);
        (* this key is used to make sure the snapshotting task *)
	(* is seen from all xencenter clients *)
        Db.Task.add_to_other_config ~__context ~self:task
          ~key:vmss_snapshot_other_config_show_in_xencenter
          ~value:"";
        Db.Task.add_to_other_config ~__context ~self:task
          ~key:vmss_snapshot_other_config_applies_to
          ~value:(Ref.string_of vm)
      with e->
        debug "Error adding other_config:show_in_xencenter to task %s: %s"
        (Ref.string_of task) (Printexc.to_string e)
    )

