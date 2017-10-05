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

  let test_compare ~test l_op r_op ~msg =
    let op = (l_op=r_op) in
    test_assert ~test ~op ~msg in


  (* Overall test suite runs smaller unit tests *)
  let cbt_test = make_test "Testing changed block tracking" 2 in
  try
    start cbt_test;
    (* Create new VDI, attach to lvm SR *)
    let sR = List.filter
        (fun sr -> (SR.get_type ~session_id ~rpc:!rpc ~self:sr) = "lvm")
        (SR.get_all ~session_id ~rpc:!rpc)
             |> List.hd in
    let vDI = VDI.create ~session_id ~rpc:!rpc ~name_label:"qt-cbt"
        ~name_description:"VDI for CBT quicktest" ~_type:`user ~sR
        ~sharable:false ~other_config:[] ~read_only:false ~sm_config:[]
        ~virtual_size:(2L ** mib) ~xenstore_data:[] ~tags:[] in

    (* Test enable/disable CBT, test cbt_enabled:false for new VDI *)
    let enable_disable_cbt_test () =
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
          (get_cbt_status vDI) (* disable_cbt fails *)
          ~msg:"VDI.disable_CBT failed";
        success enable_cbt_test;
      with e -> report_failure e "enable/disable CBT" enable_cbt_test in

    (* Call unit tests *)
    enable_disable_cbt_test ();

    (* Finally, destroy VDI *)
    VDI.destroy ~session_id ~rpc:!rpc ~self:vDI;
    success cbt_test
  with
  | (Failure hd) -> failed cbt_test "Could not find lvm SR, cannot create VDI"
  | e -> report_failure e "CBT" cbt_test
