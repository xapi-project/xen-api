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
module Date = Xapi_stdext_date.Date

let upgrade_bios () =
  let tmp_filename =
    Filename.(concat (get_temp_dir_name ()) "previousInventory")
  in
  let check inventory bios_strings =
    Xapi_stdext_unix.Unixext.write_string_to_file tmp_filename inventory ;
    let __context = T.make_test_database () in
    X.upgrade_bios_strings.fn ~__context ;
    let _, vm_r = List.hd (Db.VM.get_all_records ~__context) in
    Alcotest.(check (list (pair string string)))
      "bios strings upgrade" vm_r.API.vM_bios_strings bios_strings
  in
  check "OEM_MANUFACTURER=Dell" Constants.old_dell_bios_strings ;
  check "OEM_MANUFACTURER=HP" Constants.old_hp_bios_strings ;
  check "" Constants.generic_bios_strings ;
  Xapi_stdext_unix.Unixext.unlink_safe tmp_filename

let update_snapshots () =
  let __context = T.make_test_database () in
  let a = T.make_vm ~__context ~name_label:"a" () in
  let a_snap = T.make_vm ~__context ~name_label:"a snap" () in
  Db.VM.set_snapshot_of ~__context ~self:a_snap ~value:a ;
  Db.VM.set_snapshot_time ~__context ~self:a_snap ~value:(Date.of_unix_time 1.) ;
  let b = T.make_vm ~__context ~name_label:"b" () in
  let b_snap = T.make_vm ~__context ~name_label:"b snap" () in
  Db.VM.set_snapshot_of ~__context ~self:b_snap ~value:b ;
  Db.VM.set_snapshot_time ~__context ~self:b_snap ~value:(Date.of_unix_time 1.) ;
  let b_snap2 = T.make_vm ~__context ~name_label:"b snap2" () in
  Db.VM.set_snapshot_of ~__context ~self:b_snap2 ~value:b ;
  Db.VM.set_snapshot_time ~__context ~self:b_snap2 ~value:(Date.of_unix_time 2.) ;
  X.update_snapshots.fn ~__context ;
  let check_vm = Alcotest.check Alcotest_comparators.(ref ()) in
  (* a.parent = a_snap *)
  check_vm "a.parent <> a_snap" a_snap (Db.VM.get_parent ~__context ~self:a) ;
  (* b.parent = b_snap2 *)
  check_vm "b.parent <> b_snap2" b_snap2 (Db.VM.get_parent ~__context ~self:b) ;
  (* b_snap2.parent = b_snap *)
  check_vm "b_snap2.parent <> b_snap" b_snap
    (Db.VM.get_parent ~__context ~self:b_snap2)

let remove_restricted_pbd_keys () =
  let restricted_keys = ["SRmaster"] in
  let other_keys = ["foo"; "bar"] in
  (* to check we don't remove too much *)
  let device_config =
    List.map (fun k -> (k, "some_value")) (restricted_keys @ other_keys)
  in
  let __context = T.make_test_database () in
  let pbd = T.make_pbd ~__context ~device_config () in
  X.remove_restricted_pbd_keys.fn ~__context ;
  let device_config' = Db.PBD.get_device_config ~__context ~self:pbd in
  List.iter
    (fun k ->
      Alcotest.(check bool)
        (Printf.sprintf "Restricted key, %s, not removed from PBD.device_config"
           k
        )
        false
        (List.mem_assoc k device_config')
    )
    restricted_keys ;
  List.iter
    (fun k ->
      Alcotest.(check bool)
        (Printf.sprintf "Non-restricted key, %s, removed from PBD.device_config"
           k
        )
        true
        (List.mem_assoc k device_config')
    )
    other_keys

let test =
  [
    ("upgrade_bios", `Quick, upgrade_bios)
  ; ("update_snapshots", `Quick, update_snapshots)
  ; ("remove_restricted_pbd_keys", `Quick, remove_restricted_pbd_keys)
  ]
