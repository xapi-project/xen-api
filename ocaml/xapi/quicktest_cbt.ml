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

open Client (* import module *)
open Quicktest_common
exception Test_failed of string
open Client (* import namespace, don't have to type Client.VDI.function each time *)

(* Helper for test failure due to unexpected error *)
let report_failure error test =
  failed test (Printf.sprintf "%s failed: %s" test.name
                 (ExnHelper.string_of_exn error))

(* Define exception so that if test fails, exception is passed to try-with statement and fails there
 * so that the test only fails once and doesn't erroneously assume the test never started *)
let test_assert ~test op ~msg =
  if not op then raise (Test_failed msg)

let test_compare ~test left_op right_op ~msg =
  let op = (left_op = right_op) in test_assert ~test op ~msg

let get_cbt_status ~session_id ~vDI = VDI.get_cbt_enabled ~session_id ~rpc:!rpc ~self:vDI

let name_description = "VDI for CBT quicktest"
let make_vdi_from ~session_id ~sR = (* SR has VDI.create as allowed *)
  VDI.create
    ~sR
    ~session_id
    ~rpc:!rpc
    ~name_label:"qt-cbt"
    ~name_description
    ~_type:`user
    ~sharable:false
    ~read_only:false
    ~virtual_size:(4L ** mib)
    ~xenstore_data:[]
    ~other_config:[]
    ~tags:[]
    ~sm_config:[]


(**** Test declarations ****)

(* Test enable/disable CBT, test cbt_enabled:false for new VDI *)
let enable_disable_cbt_test ~session_id ~vDI =
  let enable_cbt_test = make_test "Testing VDI.enable/disable_CBT" 4 in
  try
    start enable_cbt_test;
    test_assert ~test:enable_cbt_test
      (not (get_cbt_status ~session_id ~vDI))
      ~msg:"VDI.cbt_enabled field should be set to false for new VDIs";
    VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
    test_assert ~test:enable_cbt_test
      (get_cbt_status ~session_id ~vDI)
      ~msg:"VDI.enable_cbt failed";
    VDI.disable_cbt ~session_id ~rpc:!rpc ~self:vDI;
    test_assert ~test:enable_cbt_test
      (not (get_cbt_status ~session_id ~vDI)) (* disable_cbt fails *)
      ~msg:"VDI.disable_CBT failed";
    success enable_cbt_test
  with
  | Test_failed msg -> failed enable_cbt_test msg
  | e -> report_failure e enable_cbt_test

(* Test data_destroy and snapshot update the necessary fields *)
let vdi_data_destroy_test ~session_id ~vDI =
  let data_destroy_test = make_test "Testing VDI.data_destroy" 4 in
  try
    start data_destroy_test;
    debug data_destroy_test "Enabling CBT on original VDI";
    VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
    test_assert ~test:data_destroy_test
      (get_cbt_status ~session_id ~vDI)
      ~msg:"VDI.enable_cbt failed";

    debug data_destroy_test "Snapshotting original VDI.";
    let newvdi = VDI.snapshot ~session_id ~rpc:!rpc ~vdi:vDI ~driver_params:[] in
    test_assert ~test:data_destroy_test
      (get_cbt_status ~session_id ~vDI:newvdi)
      ~msg:"VDI.snapshot failed, cbt_enabled field didn't carry over";

    debug data_destroy_test "Destroying snapshot VDI data";
    VDI.data_destroy ~session_id ~rpc:!rpc ~self:newvdi;
    test_compare ~test:data_destroy_test
      (VDI.get_type ~session_id ~rpc:!rpc ~self:newvdi)
      `cbt_metadata
      ~msg:"VDI.data_destroy failed to update VDI.type";

    let content_id_str = "/No content: this is a cbt_metadata VDI/" in
    test_compare ~test:data_destroy_test
      (VDI.get_other_config ~session_id ~rpc:!rpc ~self:newvdi |> List.assoc "content_id")
      content_id_str
      ~msg:"VDI.data_destroy failed to update VDI.content_id";

    success data_destroy_test
  with
  | Test_failed msg -> failed data_destroy_test msg
  | e -> report_failure e data_destroy_test

(* Check VDI.(copy, clone, and snapshot) all properly update cbt_enabled
 * Debug output included as VDI operations are expensive and take longer than other calls *)
let vdi_clone_copy_snapshot_test ~session_id ~sR ~vDI =
  let vdi_duplicate_test = make_test "Testing VDI.{clone,copy,snapshot}" 4 in
  try
    start vdi_duplicate_test;
    debug vdi_duplicate_test "Enabling CBT on original VDI";
    VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
    test_assert ~test:vdi_duplicate_test
      (get_cbt_status ~session_id ~vDI)
      ~msg:"VDI.enable_cbt failed";

    debug vdi_duplicate_test "Cloning VDI";
    let vdi_clone = VDI.clone ~session_id ~rpc:!rpc ~vdi:vDI ~driver_params:[] in
    test_assert ~test:vdi_duplicate_test
      (not (get_cbt_status ~session_id ~vDI:vdi_clone))
      ~msg:"VDI.clone failed to set cbt_enabled to false";

    debug vdi_duplicate_test "Snapshotting VDI";
    let vdi_snapshot = VDI.snapshot ~session_id ~rpc:!rpc ~vdi:vDI ~driver_params:[] in
    test_assert ~test:vdi_duplicate_test
      (get_cbt_status ~session_id ~vDI:vdi_snapshot)
      ~msg:"VDI.snapshot failed to update cbt_enabled";

    (* Test VDI.copy for copying from existing to fresh VDI in same SR *)
    debug vdi_duplicate_test "Copying VDI into a freshly created VDI in same SR";
    let vdi_copy_fresh = VDI.copy ~session_id ~rpc:!rpc ~vdi:vDI ~base_vdi:(Ref.null) ~into_vdi:(Ref.null) ~sr:sR in
    test_assert ~test:vdi_duplicate_test
      (not (get_cbt_status ~session_id ~vDI:vdi_copy_fresh))
      ~msg:"VDI.copy failed to initialise cbt_enabled to false";

    (* Test VDI.copy for copying from one existing to another *)
    debug vdi_duplicate_test "Copying VDI into existing VDI";
    let into_vdi = make_vdi_from ~session_id ~sR in
    let vdi_copy_existing = VDI.copy ~session_id ~rpc:!rpc ~vdi:vDI ~base_vdi:(Ref.null) ~into_vdi ~sr:(Ref.null) in
    test_assert ~test:vdi_duplicate_test
      (not (get_cbt_status ~session_id ~vDI:vdi_copy_existing))
      ~msg:"VDI.copy failed to initialise cbt_enabled to false";
    (* Test VDI copied into *)
    test_assert ~test:vdi_duplicate_test
      (not (get_cbt_status ~session_id ~vDI:into_vdi))
      ~msg:"VDI.copy failed to initialise cbt_enabled to false";

    success vdi_duplicate_test
  with
  | Test_failed msg -> failed vdi_duplicate_test msg
  | e -> report_failure e vdi_duplicate_test


(* Overall test executes individual unit tests *)
let test ~session_id =
  let cbt_test = make_test "Testing changed block tracking" 2 in
  try
    start cbt_test;

    (* For each test, check the given sR is capable of the associated operations
     * If not, skip that test, otherwise run it *)
    let run_test_suite ~session_id ~sR ~vDI =
      let sr_ops = (SR.get_allowed_operations ~session_id ~rpc:!rpc ~self:sR) in
      [ (fun () -> enable_disable_cbt_test ~session_id ~vDI) ,
        [ `vdi_enable_cbt ; `vdi_disable_cbt ]
      ; (fun () -> vdi_data_destroy_test ~session_id ~vDI) ,
        [ `vdi_enable_cbt ; `vdi_data_destroy ; `vdi_snapshot ]
      ; (fun () -> vdi_clone_copy_snapshot_test ~session_id ~sR ~vDI),
        [ `vdi_enable_cbt ; `vdi_create ; `vdi_clone ; `vdi_snapshot ]
      ]
      |> List.iter
        (fun (test,list_vdi_ops) ->
           if List.for_all (fun vdi_op -> List.mem vdi_op sr_ops) list_vdi_ops
           then test ()
           else debug cbt_test "SR lacks capabilities for this test, skipping"
        ) in

    (* Try running test suite, definitively destroy all VDIs created, regardless of success or errors *)
    let handle_storage_objects ~session_id ~sR ~vDI =
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () -> run_test_suite ~session_id ~sR ~vDI) (* try running test suite *)
        (fun () ->                                      (* no matter what, destroy all VDIs created during test *)
           (VDI.get_all ~session_id ~rpc:!rpc)
           |> List.filter
             (fun vdi -> (VDI.get_name_label ~session_id ~rpc:!rpc ~self:vdi = "qt-cbt")
                         && (VDI.get_name_description ~session_id ~rpc:!rpc ~self:vdi = name_description)
             )
           |> List.iter (fun vdi -> VDI.destroy ~session_id ~rpc:!rpc ~self:vdi)
        ) in

    (* Obtain list of CBT-capable SRs able to create VDIs (i.e. not iso), and run them all through test suite *)
    (SR.get_all ~session_id ~rpc:!rpc)
    |> List.filter
      (fun sR -> (List.mem `vdi_create (SR.get_allowed_operations ~session_id ~rpc:!rpc ~self:sR))
                 && (SR.get_type ~session_id ~rpc:!rpc ~self:sR <> "iso")
      )
    |> List.iter
      (fun sR ->
         debug cbt_test (Printf.sprintf "Testing SR: \"%s\"" (SR.get_name_label ~session_id ~rpc:!rpc ~self:sR));
         let vDI = make_vdi_from ~session_id ~sR in
         handle_storage_objects ~session_id ~sR ~vDI
      );

    (* Overall test will fail if VDI.destroy messes up, or any other exception is thrown *)
    debug cbt_test "Finished testing changed block tracking";
    success cbt_test
  with
  | e -> report_failure e cbt_test
