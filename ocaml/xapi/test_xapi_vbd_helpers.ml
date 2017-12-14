(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

(* Helpers for testing Xapi_vbd_helpers.valid_operations *)

let setup_test ~__context ~vdi_fun =
  let _sm_ref = make_sm ~__context () in
  let sr_ref = make_sr ~__context () in
  let (_: API.ref_PBD) = make_pbd ~__context ~sR:sr_ref () in
  let vm_ref = make_vm ~__context () in
  let vdi_ref = vdi_fun sr_ref in
  let vbd_ref = make_vbd ~vDI:vdi_ref ~vM:vm_ref ~__context () in
  let vbd_record = Db.VBD.get_record_internal ~__context ~self:vbd_ref in
  vbd_ref, vbd_record

let my_cmp a b = match a,b with
  | Some(code1, _), Some(code2, _) -> code1 = code2
  | None, None -> true
  | _	-> false

let string_of_api_exn_opt = function
  | None -> "None"
  | Some (code, args) ->
    Printf.sprintf "Some (%s, [%s])" code (String.concat "; " args)

let run_assert_equal_with_vdi ~__context ?(cmp = my_cmp) ?(expensive_sharing_checks=true) ~vdi_fun op exc =
  let vbd_ref, vbd_record = setup_test ~__context ~vdi_fun in
  assert_equal
    ~cmp
    ~printer:string_of_api_exn_opt
    exc (Hashtbl.find (Xapi_vbd_helpers.valid_operations ~__context ~expensive_sharing_checks vbd_record vbd_ref) op)

(* These are to test Xapi_vbd_helpers.valid_operations against CA-253933 *)
let test_ca253933_invalid_operations () =
  let __context = Mock.make_context_with_new_db "Mock context" in
  (* When attach a VDI which is under operation which cannot perform live, should raise other_operation_in_progress *)
  let invalid_operations =  [`resize ; `destroy ; `forget ; `update ; `force_unlock ; `generate_config; `snapshot; `resize_online; `blocked; `clone] in
  let operation_is_invalid op =
    run_assert_equal_with_vdi ~__context
      ~vdi_fun:(fun sr_ref ->
          make_vdi ~sR:sr_ref ~__context ~managed:true ~current_operations:[("x", op)] ())
      `attach (Some(Api_errors.other_operation_in_progress, []))
  in List.iter operation_is_invalid invalid_operations

let test_ca253933_valid_operations () =
  let __context = Mock.make_context_with_new_db "Mock context" in
  (* Should pass *)
  let valid_operations = [`mirror; `copy] in
  let operation_is_valid op =
    run_assert_equal_with_vdi ~__context
      ~vdi_fun:(fun sr_ref ->
          make_vdi ~sR:sr_ref ~__context ~managed:true ~current_operations:[("x", op)] ())
      `attach None
  in List.iter operation_is_valid valid_operations

let test =
  "test_xapi_vbd_helpers" >:::
  [
    "test_ca253933_invalid_operations" >:: test_ca253933_invalid_operations;
    "test_ca253933_valid_operations" >:: test_ca253933_valid_operations;
  ]

