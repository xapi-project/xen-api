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
  let vdi = make_vdi ~__context ~virtual_size:4096L () in
  Xapi_pool_update.with_inc_refcount ~__context ~uuid:"a" ~vdi (fun ~__context ~uuid ~vdi -> ());
  Xapi_pool_update.with_inc_refcount ~__context ~uuid:"a" ~vdi (fun ~__context ~uuid ~vdi -> assert_equal 0 1);
  Xapi_pool_update.with_dec_refcount ~__context ~uuid:"a" ~vdi (fun ~__context ~uuid ~vdi -> assert_equal 0 1);
  Xapi_pool_update.with_dec_refcount ~__context ~uuid:"a" ~vdi (fun ~__context ~uuid ~vdi -> ())

let test_assert_space_available () =
  let stat = statvfs "./" in
  let free_bytes = Int64.mul stat.f_frsize stat.f_bfree in
  Xapi_pool_update.assert_space_available "./" (Int64.div free_bytes 3L);
  assert_raises_api_error Api_errors.out_of_space
    (fun () -> Xapi_pool_update.assert_space_available "./" (Int64.div free_bytes 2L))

let test_download_restriction () =
  Xapi_globs.host_update_dir := ".";
  let assert_no_dots (_, s) =
    assert_raises Not_found (fun () -> String.index s '.')
  in
  let test path =
    path
    |> Filename.concat Constants.get_pool_update_download_uri
    |> Xapi_pool_update.path_and_host_from_uri
    |> assert_no_dots
  in
  List.iter test ["/myfile"; "/.."; "/%2e%2e"]

let test_parse_update_info () =
  let open Xapi_pool_update in
  let no_key_xml =
    Xml.parse_string
    {|
      <update enforce-homogeneity="false" installation-size="0" name-label="XS71ECU2001" uuid="7ec1a8c1-4e7a-46f4-a935-02ae4c2003dd" version="1.0">
        <name-description>Public Availability: fixes to Dom0 kernel</name-description>
        <rolled-up-by name-label="XS71ECU2012" uuid="85655e3a-fbc8-44b6-b233-15823632add6"/>
      </update>
    |}
  in
  let update_info = parse_update_info no_key_xml in
  assert_equal update_info.key "";

  let key_xml =
    Xml.parse_string
    {|
      <update enforce-homogeneity="false" key="SOME_KEY" installation-size="0" name-label="XS71ECU2001" uuid="7ec1a8c1-4e7a-46f4-a935-02ae4c2003dd" version="1.0">
        <name-description>Public Availability: fixes to Dom0 kernel</name-description>
        <rolled-up-by name-label="XS71ECU2012" uuid="85655e3a-fbc8-44b6-b233-15823632add6"/>
      </update>
    |}
  in
  let update_info = parse_update_info key_xml in
  assert_equal update_info.key "SOME_KEY"

let test =
  "test_pool_update" >:::
  [
    "test_pool_update_destroy" >:: test_pool_update_destroy;
    "test_pool_update_refcount" >:: test_pool_update_refcount;
    "test_assert_space_available" >:: test_assert_space_available;
    "test_download_restriction" >:: test_download_restriction;
    "test_parse_update_info" >:: test_parse_update_info;
  ]
