(*
 * Copyright (C) 2017 Citrix Systems Inc.
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

(** For function application without extra indentation and brackets *)
let ($) a b = a b

module QC = Quicktest_common

let rpc = QC.rpc
module VDI = Client.Client.VDI

module Testable = struct
  let vdi_type =
    let fmt = Fmt.of_to_string (fun t -> API.rpc_of_vdi_type t |> Rpc.to_string) in
    Alcotest.testable fmt (=)
end

let get_cbt_status ~session_id ~vDI = VDI.get_cbt_enabled ~session_id ~rpc:!rpc ~self:vDI

let assert_cbt_status boolean ~session_id  ~vDI ~msg =
  let cbt_status = (get_cbt_status ~session_id ~vDI) in
  Alcotest.(check bool) msg boolean cbt_status

(* ------------------ *
   Test declarations
 * ------------------ *)

(* Note that tests including expensive VDI operations (snapshot, clone, copy etc)
 * output debug info at most steps to justify waiting time to user
 * with the exception of VDI.update, which is called after every VDI operation *)

(* Test enable/disable_cbt, data_destroy, and snapshot update the necessary fields *)
let vdi_data_destroy_test session_id sr_info =
  Storage_test.VDI.with_any session_id sr_info (fun vDI ->
      print_endline "Enabling CBT on original VDI";
      VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
      assert_cbt_status true ~session_id ~vDI
        ~msg:"VDI.enable_cbt failed";
      Storage_test.VDI.test_update session_id vDI;

      print_endline "Snapshotting original VDI with CBT enabled";
      let snapshot = VDI.snapshot ~session_id ~rpc:!rpc ~vdi:vDI ~driver_params:[] in
      Storage_test.VDI.with_destroyed session_id snapshot $ fun () ->
      assert_cbt_status true ~session_id  ~vDI:snapshot
        ~msg:"VDI.snapshot failed, cbt_enabled field didn't carry over";
      List.iter (Storage_test.VDI.test_update session_id) [snapshot ; vDI];

      print_endline "Disabling CBT on original VDI";
      VDI.disable_cbt ~session_id ~rpc:!rpc ~self:vDI;
      assert_cbt_status false ~session_id  ~vDI
        ~msg:"VDI.disable_cbt failed";
      Storage_test.VDI.test_update session_id  vDI;

      print_endline "Snapshotting original VDI with CBT disabled";
      let snapshot_no_cbt = VDI.snapshot ~session_id ~rpc:!rpc ~vdi:vDI ~driver_params:[] in
      Storage_test.VDI.with_destroyed session_id snapshot_no_cbt $ fun () ->
      assert_cbt_status false ~session_id  ~vDI:snapshot_no_cbt
        ~msg:"VDI.snapshot failed, cbt_enabled field didn't carry over";
      List.iter (Storage_test.VDI.test_update session_id) [snapshot_no_cbt ; vDI];

      print_endline "Destroying snapshot VDI data";
      VDI.data_destroy ~session_id ~rpc:!rpc ~self:snapshot;
      Alcotest.check Testable.vdi_type "VDI.data_destroy failed to update VDI.type"
        `cbt_metadata
        (VDI.get_type ~session_id ~rpc:!rpc ~self:snapshot);
      assert_cbt_status true ~session_id  ~vDI:snapshot
        ~msg:"VDI snapshot cbt_enabled field erroneously set to false";
      (*  test_vdi_update ~session_id  snapshot;
          temporarily comment this out as it is blocked on CA-273981
          VDI.update doesn't currently work on cbt-metadata VDIs *)

      let content_id_str = "/No content: this is a cbt_metadata VDI/" in
      Alcotest.(check string)
        (Printf.sprintf "VDI.data_destroy failed to update VDI.content_id to \"%s\"" content_id_str)
        (VDI.get_other_config ~session_id ~rpc:!rpc ~self:snapshot |> List.assoc "content_id")
        content_id_str
    )

(* Check VDI.{copy, clone} all properly update cbt_enabled
 * Debug output included as VDI operations are expensive and take longer than other calls *)
let vdi_clone_copy_test session_id sr_info =
  Storage_test.VDI.with_any session_id sr_info (fun vDI ->
      print_endline "Enabling CBT on original VDI";
      VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
      assert_cbt_status true  ~session_id ~vDI
        ~msg:"VDI.enable_cbt failed";

      print_endline "Cloning VDI";
      let vdi_clone = VDI.clone ~session_id ~rpc:!rpc ~vdi:vDI ~driver_params:[] in
      Storage_test.VDI.with_destroyed session_id vdi_clone $ fun () ->
      (* Test VDI.copy for copying from existing to fresh VDI in same SR *)
      print_endline "Copying VDI into a freshly created VDI in same SR";
      let vdi_copy_fresh = VDI.copy ~session_id ~rpc:!rpc ~vdi:vDI ~base_vdi:(Ref.null) ~into_vdi:(Ref.null) ~sr:sr_info.Storage_test.sr in
      Storage_test.VDI.with_destroyed session_id vdi_copy_fresh $ fun () ->

      (* Test VDI.copy for backing up differences between freshly copied VDI and original *)
      print_endline "Copying differences between original VDI and fresh copy to a new VDI";
      Storage_test.VDI.with_any session_id sr_info
        (fun into_vdi ->
           VDI.copy ~session_id ~rpc:!rpc ~vdi:vDI ~base_vdi:vdi_copy_fresh ~into_vdi ~sr:(Ref.null) |> ignore;

           (* Test cbt_enabled field of the original VDI and new copies *)
           [ true , vDI , "VDI.copy erroneously reset the original VDI's cbt_enabled to false"
           ; false , into_vdi , "VDI.copy failed to initialise cbt_enabled to false"
           ; false , vdi_copy_fresh , "VDI.copy erroneously reset the copied VDI's cbt_enabled field to true"
           ; false , vdi_clone , "VDI.clone failed to set cbt_enabled to false";
           ] |> List.iter
             (fun (boolean, vDI, msg) ->
                assert_cbt_status boolean  ~session_id ~vDI ~msg;
                Storage_test.VDI.test_update session_id  vDI)
        )
    )

(* ---------------- *
    Test execution
 * ---------------- *)

(** Each test specifies the set of SR capabilities it requires *)
let tests session_id =
  let module F = Storage_test.Sr_filter in
  [ "vdi_data_destroy_test", `Slow, vdi_data_destroy_test, F.allowed_operations [ `vdi_enable_cbt ; `vdi_disable_cbt ; `vdi_data_destroy ; `vdi_snapshot ]
  ; "vdi_clone_copy_test", `Slow, vdi_clone_copy_test, F.allowed_operations [ `vdi_create; `vdi_destroy; `vdi_enable_cbt; `vdi_clone ]
  ]
  |> Storage_test.get_test_cases session_id
