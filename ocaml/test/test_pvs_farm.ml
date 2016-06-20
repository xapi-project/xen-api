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

let test_introduce () =
  let __context = make_test_database () in
  let name = "my_pvs_farm" in
  let pvs_farm = Xapi_pvs_farm.introduce ~__context ~name in
  assert_equal name (Db.PVS_farm.get_name ~__context ~self:pvs_farm);
  assert_equal [] (Db.PVS_farm.get_cache_storage ~__context ~self:pvs_farm)

let test =
  "test_pvs_farm" >:::
  [
    "test_introduce" >:: test_introduce;
  ]
