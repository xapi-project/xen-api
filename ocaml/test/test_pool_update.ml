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

let test_pool_update_destroy () =
  let __context = make_test_database () in
  let self = make_pool_update ~__context () in
  Xapi_pool_update.destroy ~__context ~self;
  assert_equal (Db.is_valid_ref __context self) false

let test =
  "test_pool_update" >:::
  [
    "test_pool_update_destroy" >:: test_pool_update_destroy;
  ]
