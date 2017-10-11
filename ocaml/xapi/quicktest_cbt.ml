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

open Client
open Quicktest_common
exception Test_failed of string

let start session_id =
  let open Client in

  (* Helper for test failure due to unexpected error *)
  let report_failure error test =
    failed test (Printf.sprintf "%s failed: %s" test.name
                   (ExnHelper.string_of_exn error)) in

  (* Define exception so that if test fails, exception is passed to try-with statement and fails there
   * so that the test only fails once and doesn't erroneously assume the test never started *)
  let test_assert ~test op ~msg =
    if not op then raise (Test_failed msg) in

  let name_description = "VDI for CBT quicktest" in
  let make_vdi_from sR = (* SR has VDI.create as allowed *)
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
  in

  (* overall test, runs smaller unit tests *)
  let cbt_test = make_test "Testing changed block tracking" 2 in
  try
    start cbt_test;
    let get_cbt_status vDI = VDI.get_cbt_enabled ~session_id ~rpc:!rpc ~self:vDI in

    (* Test enable/disable CBT, test cbt_enabled:false for new VDI *)
    let enable_disable_cbt_test ~vDI =
      let enable_cbt_test = make_test "Testing VDI.enable/disable_CBT" 4 in
      try
        start enable_cbt_test;
        test_assert ~test:enable_cbt_test
          (not (VDI.get_cbt_enabled ~session_id ~rpc:!rpc ~self:vDI))
          ~msg:"VDI.cbt_enabled field should be set to false for new VDIs";
        VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
        test_assert ~test:enable_cbt_test
          (get_cbt_status vDI)
          ~msg:"VDI.enable_cbt failed";
        VDI.disable_cbt ~session_id ~rpc:!rpc ~self:vDI;
        test_assert ~test:enable_cbt_test
          (not (get_cbt_status vDI)) (* disable_cbt fails *)
          ~msg:"VDI.disable_CBT failed";
        success enable_cbt_test
      with
      | Test_failed msg -> failed enable_cbt_test msg
      | e -> report_failure e enable_cbt_test in

    (* For each test, check the given sR is capable of the associated operations
     * If not, skip that test, otherwise run it *)
    let run_test_suite ~sR ~vDI =
      let sr_ops = (SR.get_allowed_operations ~session_id ~rpc:!rpc ~self:sR) in
      [ (fun () -> enable_disable_cbt_test ~vDI) ,
        [ `vdi_enable_cbt ; `vdi_disable_cbt ]
      ]
      |> List.iter
        (fun (test,list_vdi_ops) ->
           if List.for_all (fun vdi_op -> List.mem vdi_op sr_ops) list_vdi_ops
           then test ()
           else debug cbt_test "SR lacks capabilities for this test, skipping"
        ) in

    (* Try running test suite, definitively destroy all VDIs created, regardless of success or errors *)
    let handle_storage_objects ~sR ~vDI =
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () -> run_test_suite ~sR ~vDI)
        (fun () ->
           (VDI.get_all ~session_id ~rpc:!rpc)
           |> List.filter
             (fun vdi -> (VDI.get_name_label ~session_id ~rpc:!rpc ~self:vdi = "qt-cbt")
                         && (VDI.get_name_description ~session_id ~rpc:!rpc ~self:vdi = name_description)
             )
           |> List.iter (fun vdi -> VDI.destroy ~session_id ~rpc:!rpc ~self:vdi)
        ) in

    (* Obtain list of SRs capable of creating VDIs, and run them all through test suite *)
    (SR.get_all ~session_id ~rpc:!rpc)
    |> List.filter
      (fun sR -> (List.mem `vdi_create (SR.get_allowed_operations ~session_id ~rpc:!rpc ~self:sR))
                 && (SR.get_type ~session_id ~rpc:!rpc ~self:sR <> "iso")
      )
    |> List.iter
      (fun sR ->
         debug cbt_test (Printf.sprintf "Testing SR: \"%s\"" (SR.get_name_label ~session_id ~rpc:!rpc ~self:sR));
         let vDI = make_vdi_from sR in
         handle_storage_objects ~sR ~vDI
      );

    (* Overall test will fail if VDI.destroy messes up, or any other exception is thrown *)
    debug cbt_test "Finished testing changed block tracking";
    success cbt_test
  with
  | e -> report_failure e cbt_test
