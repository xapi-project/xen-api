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

open Test_common

let test_pool_update_destroy () =
  let __context = make_test_database () in
  let self = make_pool_update ~__context () in
  Xapi_pool_update.destroy ~__context ~self;
  Alcotest.(check bool)
    "test_pool_update_destroy: pool update ref should be invalid"
    false (Db.is_valid_ref __context self)

let test_pool_update_refcount () =
  let assert_equal = Alcotest.(check int) "assertion called by test_pool_update_refcount" in
  let __context = Mock.make_context_with_new_db "Mock context" in
  let uuid = Helpers.get_localhost_uuid () in
  let vdi = make_vdi ~__context ~virtual_size:4096L () in
  Xapi_pool_update.with_inc_refcount ~__context ~uuid ~vdi (fun ~__context ~uuid ~vdi -> ());
  Xapi_pool_update.with_inc_refcount ~__context ~uuid ~vdi (fun ~__context ~uuid ~vdi -> assert_equal 0 1);
  Xapi_pool_update.with_dec_refcount ~__context ~uuid ~vdi (fun ~__context ~uuid ~vdi -> assert_equal 0 1);
  Xapi_pool_update.with_dec_refcount ~__context ~uuid ~vdi (fun ~__context ~uuid ~vdi -> ())

let test_assert_space_available () =
  let free_bytes = 1_000_000L in
  Alcotest.check_raises
    "test_assert_space_available should raise out_of_space"
    Api_errors.(Server_error (out_of_space, [!Xapi_globs.host_update_dir]))
    (fun () -> Xapi_pool_update.assert_space_available ~get_free_bytes:(fun _ -> free_bytes) "./" (Int64.div free_bytes 2L))

let test_download_restriction () =
  Xapi_globs.host_update_dir := ".";
  let assert_no_dots s =
    Alcotest.(check bool)
      "test_download_restriction: path must not contain ."
      true
      (String.index_opt s '.' = None)
  in
  let test path =
    path
    |> Filename.concat Constants.get_pool_update_download_uri
    |> Xapi_pool_update.path_from_uri
    |> assert_no_dots
  in
  List.iter test ["myfile"; ".."; "%2e%2e"]

let test =
  [ "test_pool_update_destroy", `Quick, test_pool_update_destroy
  ; "test_pool_update_refcount", `Quick, test_pool_update_refcount
  ; "test_assert_space_available", `Quick, test_assert_space_available
  ; "test_download_restriction", `Quick, test_download_restriction
  ]
