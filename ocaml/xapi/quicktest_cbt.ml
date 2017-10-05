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

  (* helper for test failure due to unexpected error *)
  let report_failure error test_name test =
    failed test (Printf.sprintf "%s failed: %s" test_name  (ExnHelper.string_of_exn error)) in

  (* overall test suite runs smaller unit tests *)
  let cbt_test = make_test "Testing changed block tracking" 2 in
  try
    start cbt_test;
    (* find an lvm SR to attach new VDI to *)
    let sR = List.filter
        (fun sr -> (SR.get_type ~session_id ~rpc:!rpc ~self:sr) = "lvm")
        (SR.get_all ~session_id ~rpc:!rpc)
             |> List.hd in
    let vDI = VDI.create ~session_id ~rpc:!rpc ~name_label:"qt-cbt"
        ~name_description:"VDI for CBT quicktest" ~_type:`user ~sR
        ~sharable:false ~other_config:[] ~read_only:false ~sm_config:[]
        ~virtual_size:(4L ** mib) ~xenstore_data:[] ~tags:[] in

    (* test enable/disable CBT, test cbt_enabled:false for new VDI *)
    let enable_disable_cbt_test () =
      let enable_cbt_test = make_test "Testing VDI.enable/disable_CBT" 4 in
      let get_cbt_status vDI = VDI.get_cbt_enabled ~session_id ~rpc:!rpc ~self:vDI in
      try
        start enable_cbt_test;
        assert (not (VDI.get_cbt_enabled ~session_id ~rpc:!rpc ~self:vDI));
        VDI.enable_cbt ~session_id ~rpc:!rpc ~self:vDI;
        if get_cbt_status vDI
        then begin (* enable_cbt works *)
          VDI.disable_cbt ~session_id ~rpc:!rpc ~self:vDI;
          if get_cbt_status vDI (* disable_cbt fails *)
          then failed enable_cbt_test "VDI.disable_CBT failed"
          else success enable_cbt_test end
        else failed enable_cbt_test "VDI.enable_CBT failed"
      with e -> report_failure e "enable/disable CBT" enable_cbt_test in

    (* call unit tests *)
    enable_disable_cbt_test ();
    VDI.destroy ~session_id ~rpc:!rpc ~self:vDI;
    success cbt_test
  with
  | (Failure hd) -> failed cbt_test "could not find lvm SR, cannot create VDI"
  | e -> failed cbt_test (ExnHelper.string_of_exn e)
