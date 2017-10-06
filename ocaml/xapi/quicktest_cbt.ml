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
let start session_id =
  let open Client in

  (* Helper for test failure due to unexpected error *)
  let report_failure error test_name test =
    failed test (Printf.sprintf "%s failed: %s" test_name
                   (ExnHelper.string_of_exn error)) in


  let test_assert ~test op ~msg =
    try assert op with assert_failure -> failed test msg in

  (* Overall test suite runs smaller unit tests *)
  let cbt_test = make_test "Testing changed block tracking" 2 in
  try
    start cbt_test;

    (* generate list of SRs attached to a host *)
    let list_of_attached_srs =
      let is_attached sr =
        let pbds = SR.get_PBDs ~session_id ~rpc:!rpc ~self:sr in
        List.fold_left (||) false
          (List.map (fun pbd ->
               PBD.get_currently_attached ~session_id ~rpc:!rpc ~self:pbd
             ) pbds) in
      List.filter is_attached (SR.get_all ~session_id ~rpc:!rpc) in

    (* find lvm SR (that passes data_destroy) to attach new VDI *)
    let sR = List.filter
        (fun sr ->
           ((SR.get_type ~session_id ~rpc:!rpc ~self:sr) = "lvm")
           && (not (SR.get_is_tools_sr ~session_id ~rpc:!rpc ~self:sr))
        ) list_of_attached_srs |> List.hd in

    let vDI = VDI.create ~session_id ~rpc:!rpc ~name_label:"qt-cbt"
        ~name_description:"VDI for CBT quicktest" ~_type:`user ~sR
        ~sharable:false ~other_config:[] ~read_only:false ~sm_config:[]
        ~virtual_size:(2L ** mib) ~xenstore_data:[] ~tags:[] in

    (* Test enable/disable CBT, test cbt_enabled:false for new VDI *)
    let enable_disable_cbt_test vDI =
      let enable_cbt_test = make_test "Testing VDI.enable/disable_CBT" 4 in
      let get_cbt_status vDI = VDI.get_cbt_enabled ~session_id ~rpc:!rpc ~self:vDI in
      let test = enable_cbt_test in
      try
        start enable_cbt_test;
        test_assert ~test
          (not (VDI.get_cbt_enabled ~session_id ~rpc:!rpc ~self:vDI))
          ~msg:"VDI.cbt_enabled field should be set to false for new VDIs, but wasn't ";
        VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
        test_assert ~test
          (get_cbt_status vDI)
          ~msg:"VDI.enable_cbt failed";
        VDI.disable_cbt ~session_id ~rpc:!rpc ~self:vDI;
        test_assert ~test
          (not (get_cbt_status vDI)) (* disable_cbt fails *)
          ~msg:"VDI.disable_CBT failed";
        success enable_cbt_test;
      with e -> report_failure e "enable/disable CBT" enable_cbt_test in

    (* Call unit tests *)
    let run_tests ~vDI =
      List.iter (fun test -> test vDI)
        [ enable_disable_cbt_test
        ]; in

    (* Finally, destroy VDI regardless of how test suite progresses *)
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () -> run_tests ~vDI)
      (fun () -> VDI.destroy ~session_id ~rpc:!rpc ~self:vDI);

    (* Overall test will fail if VDI.destroy messes up, or any other exception is thrown *)
    success cbt_test
  with
  | (Failure hd) -> failed cbt_test "Could not find lvm SR, cannot create VDI"
  | e -> report_failure e "CBT" cbt_test
