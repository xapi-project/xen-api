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
open Client (* import namespace for convenience, Client.VDI.func -> VDI.func *)
open Quicktest_common

(* Throw this exception within assertion helpers when assertion fails, instead of
 * actually failing the test within a helper called from the main test body *)
exception Test_failed of string

(* Helper for test failure due to unexpected error
 * Can fail test here as this is called outside test body *)
let report_failure error test =
  failed test (Printf.sprintf "%s failed: %s" test.name (ExnHelper.string_of_exn error))

let get_cbt_status ~session_id ~vDI = VDI.get_cbt_enabled ~session_id ~rpc:!rpc ~self:vDI

let test_assert ~test op ~msg =
  if not op then raise (Test_failed msg)

let test_compare ~test left_op right_op ~msg =
  let op = (left_op = right_op) in test_assert ~test op ~msg

let assert_cbt_status boolean ~session_id ~test ~vDI ~msg =
  let cbt_status = (get_cbt_status ~session_id ~vDI) in
  test_compare ~test cbt_status boolean ~msg

(* This naming is used to identify VDIs to destroy later on *)
let name_label = "qt-cbt"
let name_description = "VDI for CBT quicktest"
(* This is only called on VDI-create capable SRs *)
let make_vdi_from ~session_id ~sR =
  VDI.create
    ~sR
    ~session_id
    ~rpc:!rpc
    ~name_label
    ~name_description
    ~_type:`user
    ~sharable:false
    ~read_only:false
    ~virtual_size:(4L ** mib)
    ~xenstore_data:[]
    ~other_config:[]
    ~tags:[]
    ~sm_config:[]


(* ******************
 * Test declarations
 * ******************)

(* Note that tests including expensive VDI operations (snapshot, clone, copy etc)
 * output debug info at most steps to justify waiting time to user *)

(* Test enable/disable_cbt, data_destroy, and snapshot update the necessary fields *)
let vdi_data_destroy_test ~session_id ~vDI =
  let test = make_test "Testing VDI.{enable/disable_cbt, data_destroy, snapshot}" 4 in
  try
    start test;
    let debug_test = debug test in
    debug_test "Enabling CBT on original VDI";
    VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
    assert_cbt_status true ~session_id ~test ~vDI
      ~msg:"VDI.enable_cbt failed";

    debug_test "Snapshotting original VDI with CBT enabled";
    let snapshot = VDI.snapshot ~session_id ~rpc:!rpc ~vdi:vDI ~driver_params:[] in
    assert_cbt_status true ~session_id ~test ~vDI:snapshot
      ~msg:"VDI.snapshot failed, cbt_enabled field didn't carry over";

    debug_test "Disabling CBT on original VDI";
    VDI.disable_cbt ~session_id ~rpc:!rpc ~self:vDI;
    assert_cbt_status false ~session_id ~test ~vDI
      ~msg:"VDI.disable_cbt failed";

    debug_test "Snapshotting original VDI with CBT disabled";
    let snapshot_no_cbt = VDI.snapshot ~session_id ~rpc:!rpc ~vdi:vDI ~driver_params:[] in
    assert_cbt_status false ~session_id ~test ~vDI:snapshot_no_cbt
      ~msg:"VDI.snapshot failed, cbt_enabled field didn't carry over";

    debug_test "Destroying snapshot VDI data";
    VDI.data_destroy ~session_id ~rpc:!rpc ~self:snapshot;
    test_compare ~test
      (VDI.get_type ~session_id ~rpc:!rpc ~self:snapshot)
      `cbt_metadata
      ~msg:"VDI.data_destroy failed to update VDI.type";
    assert_cbt_status true ~session_id ~test ~vDI:snapshot
      ~msg:"VDI snapshot cbt_enabled field erroneously set to false";

    let content_id_str = "/No content: this is a cbt_metadata VDI/" in
    test_compare ~test
      (VDI.get_other_config ~session_id ~rpc:!rpc ~self:snapshot |> List.assoc "content_id")
      content_id_str
      ~msg:(Printf.sprintf "VDI.data_destroy failed to update VDI.content_id to \"%s\"" content_id_str);

    success test
  with
  | Test_failed msg -> failed test msg
  | e -> report_failure e test

(* Check VDI.(copy, clone, and snapshot) all properly update cbt_enabled
 * Debug output included as VDI operations are expensive and take longer than other calls *)
let vdi_clone_copy_test ~session_id ~sR ~vDI =
  let test = make_test "Testing VDI.clone and VDI.copy" 4 in
  try
    start test;
    debug test "Enabling CBT on original VDI";
    VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
    assert_cbt_status true ~test ~session_id ~vDI
      ~msg:"VDI.enable_cbt failed";

    debug test "Cloning VDI";
    let vdi_clone = VDI.clone ~session_id ~rpc:!rpc ~vdi:vDI ~driver_params:[] in
    assert_cbt_status false ~test ~session_id ~vDI:vdi_clone
      ~msg:"VDI.clone failed to set cbt_enabled to false";

    (* Test VDI.copy for copying from existing to fresh VDI in same SR *)
    debug test "Copying VDI into a freshly created VDI in same SR";
    let vdi_copy_fresh = VDI.copy ~session_id ~rpc:!rpc ~vdi:vDI ~base_vdi:(Ref.null) ~into_vdi:(Ref.null) ~sr:sR in

    (* Test VDI.copy for backing up differences between freshly copied VDI and original *)
    debug test "Copying differences between original VDI and fresh copy to a new VDI";
    let into_vdi = make_vdi_from ~session_id ~sR in
    ignore (VDI.copy ~session_id ~rpc:!rpc ~vdi:vDI ~base_vdi:vdi_copy_fresh ~into_vdi ~sr:(Ref.null));

    (* Test cbt_enabled field of the original VDI and new copies *)
    [ true , vDI , "VDI.copy erroneously reset the original VDI's cbt_enabled to false"
    ; false , into_vdi , "VDI.copy failed to initialise cbt_enabled to false"
    ; false , vdi_copy_fresh , "VDI.copy erroneously reset the copied VDI's cbt_enabled field to true"
    ] |> List.iter
      ( fun (boolean, vDI, msg) -> assert_cbt_status boolean ~test ~session_id ~vDI ~msg);

    success test
  with
  | Test_failed msg -> failed test msg
  | e -> report_failure e test

(* ****************
 *  Test execution
 * ****************)

(* Overall test executes individual unit tests *)
let test ~session_id =
  let cbt_test = make_test "Testing changed block tracking\n" 2 in
  try
    start cbt_test;

    (* For each test, check the given sR is capable of the associated operations
     * Then create a VDI that will be destroyed at the end of test suite *)
    let run_test_suite ~session_id ~sR =
      let sr_ops = (SR.get_allowed_operations ~session_id ~rpc:!rpc ~self:sR) in
      [
        (fun ~vDI -> vdi_data_destroy_test ~session_id ~vDI) ,
        [ `vdi_enable_cbt ; `vdi_disable_cbt ; `vdi_data_destroy ; `vdi_snapshot ]
      ; (fun ~vDI -> vdi_clone_copy_test ~session_id ~sR ~vDI),
        [ `vdi_enable_cbt ; `vdi_create ; `vdi_clone ; `vdi_snapshot ]
      ]
      |> List.iter
        (fun (test , list_vdi_ops) ->
           if List.for_all (fun vdi_op -> List.mem vdi_op sr_ops) list_vdi_ops
           then begin
             debug cbt_test "Creating VDI. . .";
             let vDI = make_vdi_from ~sR ~session_id in
             test ~vDI end
           else debug cbt_test "SR lacks capabilities for this test, skipping"
        ) in

    (* Try running test suite, clean up newly-created VDIs regardless of exceptions thrown in test suite *)
    let handle_storage_objects ~session_id ~sR =
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () -> run_test_suite ~session_id ~sR) (* try running test suite *)
        (fun () ->                                 (* destroy all new VDIs no matter what *)
           begin
             debug cbt_test "Destroying VDIs created in test. . .";
             (VDI.get_all ~session_id ~rpc:!rpc)
             |> List.filter
               (fun vdi -> (VDI.get_name_label ~session_id ~rpc:!rpc ~self:vdi = name_label)
                           && (VDI.get_name_description ~session_id ~rpc:!rpc ~self:vdi = name_description)
               )
             |> List.iter (fun vdi -> VDI.destroy ~session_id ~rpc:!rpc ~self:vdi);
             debug cbt_test "Successfully destroyed all VDIs created for CBT test\n"
         end
        ) in

    (* Obtain list of SRs capable of creating VDIs, and run them all through test suite *)
    (SR.get_all ~session_id ~rpc:!rpc)
    |> List.filter
      (fun sR -> (List.mem `vdi_create (SR.get_allowed_operations ~session_id ~rpc:!rpc ~self:sR))
                 && (SR.get_type ~session_id ~rpc:!rpc ~self:sR <> "iso")
      )
    |> List.iter
      (fun sR ->
         debug cbt_test (Printf.sprintf "Testing SR: \"%s\"\n" (SR.get_name_label ~session_id ~rpc:!rpc ~self:sR));
         handle_storage_objects ~session_id ~sR
      );

    (* Overall test will fail if VDI.destroy messes up, or any other exception is thrown *)
    debug cbt_test "Finished testing changed block tracking";
    success cbt_test
  with
  | e -> report_failure e cbt_test
