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

open OUnit
open Test_common
open Stdext.Unixext

let test_pool_update_destroy () =
  let __context = make_test_database () in
  let self = make_pool_update ~__context () in
  Xapi_pool_update.destroy ~__context ~self;
  assert_equal (Db.is_valid_ref __context self) false

let test_pool_update_refcount () =
  let __context = Mock.make_context_with_new_db "Mock context" in
  let uuid = Helpers.get_localhost_uuid () in
  let vdi = make_vdi ~__context ~virtual_size:4096L () in
  Xapi_pool_update.with_inc_refcount ~__context ~uuid ~vdi (fun ~__context ~uuid ~vdi -> ());
  Xapi_pool_update.with_inc_refcount ~__context ~uuid ~vdi (fun ~__context ~uuid ~vdi -> assert_equal 0 1);
  Xapi_pool_update.with_dec_refcount ~__context ~uuid ~vdi (fun ~__context ~uuid ~vdi -> assert_equal 0 1);
  Xapi_pool_update.with_dec_refcount ~__context ~uuid ~vdi (fun ~__context ~uuid ~vdi -> ())

let test_assert_space_available () =
  let stat = statvfs "./" in
  let free_bytes = Int64.mul stat.f_frsize stat.f_bfree in
  Xapi_pool_update.assert_space_available "./" (Int64.div free_bytes 3L);
  assert_raises_api_error Api_errors.out_of_space
    (fun () -> Xapi_pool_update.assert_space_available "./" (Int64.div free_bytes 2L))

let test_download_restriction () =
  Xapi_globs.host_update_dir := ".";
  let assert_no_dots s =
    assert_raises Not_found (fun () -> String.index s '.')
  in
  let test path =
    path
    |> Filename.concat Constants.get_pool_update_download_uri
    |> Xapi_pool_update.path_from_uri
    |> assert_no_dots
  in
  List.iter test ["myfile"; ".."; "%2e%2e"]

let test =
  "test_pool_update" >:::
  [
    "test_pool_update_destroy" >:: test_pool_update_destroy;
    "test_pool_update_refcount" >:: test_pool_update_refcount;
    "test_assert_space_available" >:: test_assert_space_available;
    "test_download_restriction" >:: test_download_restriction;
  ]
