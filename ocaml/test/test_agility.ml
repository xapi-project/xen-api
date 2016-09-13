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

let test_vm_agility_with_vgpu () =
  let __context = make_test_database () in
  let vm = make_vm ~__context () in
  (* VM has no VIFs, VBDs or VGPUs, so should be agile. *)
  Agility.vm_assert_agile ~__context ~self:vm;
  (* Create a VGPU - VM should no longer be agile. *)
  let (_: API.ref_VGPU) = make_vgpu ~__context ~vM:vm () in
  assert_raises_api_error
    ~args:[Ref.string_of vm]
    Api_errors.vm_has_vgpu
    (fun () -> Agility.vm_assert_agile ~__context ~self:vm)

let test =
  "test_agility" >:::
  [
    "test_vm_agility_with_vgpu" >:: test_vm_agility_with_vgpu;
  ]
