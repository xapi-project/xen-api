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

module T = Test_common
module X = Xapi_db_upgrade

let upgrade_vm_memory_for_dmc () =
  let __context = T.make_test_database () in

  let self = List.hd (Db.VM.get_all ~__context) in

  (* Set control domain's dynamic_min <> dynamic_max <> target *)
  Db.VM.set_memory_dynamic_min ~__context ~self ~value:1L;
  Db.VM.set_memory_target ~__context ~self ~value:2L;
  Db.VM.set_memory_dynamic_max ~__context ~self ~value:3L;
  (* Apply the upgrade rule *)
  X.upgrade_vm_memory_for_dmc.fn ~__context;
  let r = Db.VM.get_record ~__context ~self in
  Alcotest.(check int64)
    "upgrade_vm_memory_for_dmc: control domain memory_dynamic_min <> memory_target"
    r.API.vM_memory_dynamic_min
    r.API.vM_memory_target;
  Alcotest.(check int64)
    "upgrade_vm_memory_for_dmc: control domain memory_dynamic_max <> memory_target"
    r.API.vM_memory_dynamic_max
    r.API.vM_memory_target;

  (* Make this a non-control domain and change all memory fields *)
  Db.VM.set_is_control_domain ~__context ~self ~value:false;
  Db.VM.set_memory_static_min ~__context ~self ~value:5L;
  Db.VM.set_memory_dynamic_min ~__context ~self ~value:1L;
  Db.VM.set_memory_target ~__context ~self ~value:2L;
  Db.VM.set_memory_dynamic_max ~__context ~self ~value:3L;
  Db.VM.set_memory_static_max ~__context ~self ~value:4L;
  (* Apply the upgrade rule *)
  X.upgrade_vm_memory_for_dmc.fn ~__context;

  let r = Db.VM.get_record ~__context ~self in
  Alcotest.(check int64)
    "upgrade_vm_memory_for_dmc: memory_dynamic_max <> memory_static_max"
    r.API.vM_memory_dynamic_max
    r.API.vM_memory_static_max;

  Alcotest.(check int64)
    "upgrade_vm_memory_for_dmc: memory_target <> memory_static_max"
    r.API.vM_memory_target
    r.API.vM_memory_static_max;

  Alcotest.(check int64)
    "upgrade_vm_memory_for_dmc: memory_dynamic_min <> memory_static_max"
    r.API.vM_memory_dynamic_min
    r.API.vM_memory_static_max;

  Alcotest.(check bool)
    "upgrade_vm_memory_for_dmc: memory_static_min > memory_static_max"
    true
    (r.API.vM_memory_static_min <= r.API.vM_memory_static_max)

let upgrade_bios () =

  let check inventory bios_strings =
    Xapi_stdext_unix.Unixext.mkdir_safe "/var/tmp" 0o755;
    Xapi_stdext_unix.Unixext.write_string_to_file "/var/tmp/.previousInventory" inventory;
    let __context = T.make_test_database () in
    X.upgrade_bios_strings.fn ~__context;
    let _, vm_r = List.hd (Db.VM.get_all_records ~__context) in
    Alcotest.(check (list (pair string string)))
      "bios strings upgrade"
      vm_r.API.vM_bios_strings
      bios_strings
  in
  check "OEM_MANUFACTURER=Dell" Xapi_globs.old_dell_bios_strings;
  check "OEM_MANUFACTURER=HP" Xapi_globs.old_hp_bios_strings;
  check "" Xapi_globs.generic_bios_strings;
  Xapi_stdext_unix.Unixext.unlink_safe "/var/tmp/.previousInventory"

let update_snapshots () =
  let __context = T.make_test_database () in
  let a = T.make_vm ~__context ~name_label:"a" () in
  let a_snap = T.make_vm ~__context ~name_label:"a snap" () in
  Db.VM.set_snapshot_of ~__context ~self:a_snap ~value:a;
  Db.VM.set_snapshot_time ~__context ~self:a_snap ~value:(Stdext.Date.of_float 1.);

  let b = T.make_vm ~__context ~name_label:"b" () in
  let b_snap = T.make_vm ~__context ~name_label:"b snap" () in
  Db.VM.set_snapshot_of ~__context ~self:b_snap ~value:b;
  Db.VM.set_snapshot_time ~__context ~self:b_snap ~value:(Stdext.Date.of_float 1.);

  let b_snap2 = T.make_vm ~__context ~name_label:"b snap2" () in
  Db.VM.set_snapshot_of ~__context ~self:b_snap2 ~value:b;
  Db.VM.set_snapshot_time ~__context ~self:b_snap2 ~value:(Stdext.Date.of_float 2.);

  X.update_snapshots.fn ~__context;

  let check_vm = Alcotest.check Alcotest_comparators.(ref ()) in
  (* a.parent = a_snap *)
  check_vm
    "a.parent <> a_snap"
    a_snap
    (Db.VM.get_parent ~__context ~self:a);

  (* b.parent = b_snap2 *)
  check_vm
    "b.parent <> b_snap2"
    b_snap2
    (Db.VM.get_parent ~__context ~self:b);

  (* b_snap2.parent = b_snap *)
  check_vm
    "b_snap2.parent <> b_snap"
    b_snap
    (Db.VM.get_parent ~__context ~self:b_snap2)

let remove_restricted_pbd_keys () =
  let restricted_keys = ["SRmaster"] in
  let other_keys = ["foo"; "bar"] in (* to check we don't remove too much *)
  let device_config = List.map (fun k -> (k, "some_value")) (restricted_keys @ other_keys) in
  let __context = T.make_test_database () in
  let pbd = T.make_pbd ~__context ~device_config () in

  X.remove_restricted_pbd_keys.fn ~__context;

  let device_config' = Db.PBD.get_device_config ~__context ~self:pbd in

  List.iter (fun k ->
      Alcotest.(check bool)
        (Printf.sprintf "Restricted key, %s, not removed from PBD.device_config" k)
        false
        (List.mem_assoc k device_config')
    ) restricted_keys;

  List.iter (fun k ->
      Alcotest.(check bool)
        (Printf.sprintf "Non-restricted key, %s, removed from PBD.device_config" k)
        true
        (List.mem_assoc k device_config')
    ) other_keys

let test =
  [ "upgrade_vm_memory_for_dmc", `Quick, upgrade_vm_memory_for_dmc
  ; "upgrade_bios", `Quick, upgrade_bios
  ; "update_snapshots", `Quick, update_snapshots
  ; "remove_restricted_pbd_keys", `Quick, remove_restricted_pbd_keys
  ]
