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

let create_base_environment () =
  let __context = Test_common.make_test_database () in
  let pusb = Test_common. make_sr ~__context () in
  __context, pusb

let start_thread ~__context info =
  let usbs = Xapi_pusb_helpers.get_usbs info in
  let f () = Xapi_pusb.scan_start ~__context usbs in
  Xapi_pusb.start_thread f

let test_scan_with_usb_add_and_remove () =
  let __context = Test_common.make_test_database () in
  let test_pusb = "[{
                    \"product-desc\": \"\",
                    \"product-id\": \"5591\",
                    \"description\": \"SanDisk Corp._4C530001131223117342\",
                    \"vendor-desc\": \"SanDisk Corp.\",
                    \"version\": \"3.00\",
                    \"vendor-id\": \"0781\",
                    \"path\": \"2-2\",
                    \"serial\": \"4C530001131223117342\"
                }]"
  in
  (* add usb*)
  start_thread ~__context test_pusb;
  let host = Helpers.get_localhost ~__context in
  Xapi_pusb.scan ~__context ~host;
  Thread.delay 1.0;
  (* delete PUSB from DB*)
  List.iter (fun (self, _) ->
                  let usb_group = Db.PUSB.get_USB_group ~__context ~self in
                  Db.PUSB.destroy ~__context ~self;
                  Db.USB_group.destroy ~__context ~self:usb_group) (Db.PUSB.get_all_records ~__context);

  Xapi_pusb.scan ~__context ~host;
  Thread.delay 1.0;
  Alcotest.(check int)
    "test_scan_with_usb_add_and_remove called assertion for number of PUSB records"
    1 (List.length (Db.PUSB.get_all_records ~__context))

let test =
  [	"test_scan_with_usb_add_and_remove", `Quick, test_scan_with_usb_add_and_remove
  ]
