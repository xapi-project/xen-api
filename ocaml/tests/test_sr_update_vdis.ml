(*
 * Copyright (C) Citrix Systems Inc.
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

let create_base_environment () =
  let __context = T.make_test_database () in
  let sr = T.make_sr ~__context () in
  __context, sr

let assert_snapshot_of_is_not_null ~__context ~vdi_snapshot =
  let vdi = Db.VDI.get_snapshot_of ~__context ~self:vdi_snapshot in
  Alcotest.(check bool)
    "VDI snapshot's `snapshot_of` reference has become null"
    true (vdi <> Ref.null)

let default_vdi_info = Storage_interface.default_vdi_info

(* CA-254515 *)
(* Tests that a single VDI snapshot from the SR is properly updating the
   existing snapshot in the database; the test should pass if the `snapshot_of`
   reference remains the same in the VDI snapshot's database record, whereas
   before the fix it would become null. *)
let test_update_existing_snapshot () =
  let __context, sr = create_base_environment () in

  (* create VDI and snapshot *)
  let vdi_uuid = T.make_uuid () in
  let vdi = T.make_vdi ~__context ~uuid:vdi_uuid ~location:vdi_uuid ~sR:sr () in
  let vdi_snapshot_uuid = T.make_uuid () in
  let vdi_snapshot = T.make_vdi ~__context ~uuid:vdi_snapshot_uuid ~sR:sr
      ~location:vdi_snapshot_uuid ~snapshot_of:vdi ~is_a_snapshot:true () in

  (* create mock snapshot record which we would get from an SR scan *)
  let vdi_snapshot_sr_record = Storage_interface.({ default_vdi_info with
                                                    vdi = Storage_interface.Vdi.of_string vdi_snapshot_uuid;
                                                    uuid = Some vdi_snapshot_uuid;
                                                    is_a_snapshot = true;
                                                    snapshot_of = Storage_interface.Vdi.of_string vdi_uuid;
                                                  }) in

  (* attempt to reproduce the issue by updating the snapshot *)
  let vdi_snapshot_record = Db.VDI.get_record ~__context ~self:vdi_snapshot in
  Xapi_sr.update_vdis ~__context ~sr
    [(vdi_snapshot,vdi_snapshot_record)]
    [vdi_snapshot_sr_record];
  assert_snapshot_of_is_not_null ~__context ~vdi_snapshot

(* CA-254515 *)
(* A VDI and its snapshot could exist on an SR but not in the database. If
   calling `Xapi_sr.update_vdis` with these VDIs only existing in the SR map
   parameter, then the function will try to create them; the order in which
   they are created is undefined so, before the fix, we may attempt to create
   the VDI snapshot before we create the original/snapshoted VDI... if this
   happens then the snapshot's `snapshot_of` field will become null because
   it will fail to find the original VDI in the database because it has not
   been created yet. The fix ensures that all newly-created VDIs are also
   updated again after they have all been created, avoiding such an issue. *)
let test_update_new_vdi_and_snapshot () =
  let __context, sr = create_base_environment () in

  (* make sure the snapshot UUID is less-than the VDI UUID, because
     `Xapi_sr.update_vdis` creates new VDIs in alphabetical order of the
     VDIs' UUIDs, and we want the snapshot to be created before the
     snapshoted VDI in order to reproduce this edge case *)
  let vdi_uuid = "87654321-7357-7357-7357-735773577357" in
  let vdi_snapshot_uuid = "12345678-7357-7357-7357-735773577357" in

  (* create mock VDI/snapshot records which we would get from an SR scan *)
  let vdi_sr_record = Storage_interface.({ default_vdi_info with
                                           vdi = Storage_interface.Vdi.of_string vdi_uuid;
                                           uuid = Some vdi_uuid;
                                         }) in
  let vdi_snapshot_sr_record = Storage_interface.({ default_vdi_info with
                                                    vdi = Storage_interface.Vdi.of_string vdi_snapshot_uuid;
                                                    uuid = Some vdi_snapshot_uuid;
                                                    snapshot_of = Storage_interface.Vdi.of_string vdi_uuid;
                                                    is_a_snapshot = true;
                                                  }) in

  (* attempt to reproduce the issue by creating the snapshot before the VDI *)
  Xapi_sr.update_vdis ~__context ~sr [] [vdi_sr_record; vdi_snapshot_sr_record];
  let vdi_snapshot = Db.VDI.get_by_uuid ~__context ~uuid:vdi_snapshot_uuid in
  assert_snapshot_of_is_not_null ~__context ~vdi_snapshot

(* CA-274152 *)
(* Tests that the sharable field of the VDIs is properly set to reflect the
   values returned by the storage layer. This is particularly important in case
   of SMAPIv3 plugins that are not backed by xapi's database, in that case xapi
   really needs to check the vdi_info records returned from the storage layer
   and update the database if necessary, to keep the VDIs' fields in sync. *)
let test_sharable_field_updated_for_existing_vdi () =
  let __context, sr = create_base_environment () in
  let vdi_uuid = T.make_uuid () in

  (* In Xapi's database, the sharable field is incorrect: it is false *)
  let vdi = T.make_vdi ~__context ~sR:sr ~uuid:vdi_uuid ~location:vdi_uuid ~sharable:false () in

  (* SR.scan returned the correct vdi_info with the up-to-date sharable field *)
  let vdi_sr_record = Storage_interface.({ default_vdi_info with
                                           vdi = Storage_interface.Vdi.of_string vdi_uuid;
                                           uuid = Some vdi_uuid;
                                           sharable = true;
                                         }) in

  (* When we call this function from our SR.scan XenAPI call for example, it should
     update the VDI's sharable field to the correct value returned by the
     storage layer. *)
  let vdi_record = Db.VDI.get_record ~__context ~self:vdi in
  Xapi_sr.update_vdis ~__context ~sr [(vdi, vdi_record)] [vdi_sr_record];
  Alcotest.(check bool)
    "test_sharable_field_updated_for_existing_vdi"
    true (Db.VDI.get_sharable ~__context ~self:vdi)

let test_sharable_field_correct_for_new_vdi () =
  let __context, sr = create_base_environment () in
  let vdi_uuid = T.make_uuid () in

  (* We do not have this VDI in xapi's database. SR.scan returned it with the
     correct vdi_info containing the up-to-date sharable field. *)
  let vdi_sr_record = Storage_interface.({ default_vdi_info with
                                           vdi = Storage_interface.Vdi.of_string vdi_uuid;
                                           uuid = Some vdi_uuid;
                                           sharable = true;
                                         }) in

  (* When we call this function from our SR.scan XenAPI call for example, it should
     add the VDI to xapi's database with the correct sharable field returned
     by the storage layer. *)
  Xapi_sr.update_vdis ~__context ~sr [] [vdi_sr_record];
  let vdi = Db.VDI.get_by_uuid ~__context ~uuid:vdi_uuid in
  Alcotest.(check bool)
    "test_sharable_field_correct_for_new_vdi"
    true (Db.VDI.get_sharable ~__context ~self:vdi)

let test =
  [ "test_update_existing_snapshot", `Quick, test_update_existing_snapshot
  ; "test_update_new_vdi_and_snapshot", `Quick, test_update_new_vdi_and_snapshot
  ; "test_sharable_field_updated_for_existing_vdi", `Quick, test_sharable_field_updated_for_existing_vdi
  ; "test_sharable_field_correct_for_new_vdi", `Quick, test_sharable_field_correct_for_new_vdi
  ]
