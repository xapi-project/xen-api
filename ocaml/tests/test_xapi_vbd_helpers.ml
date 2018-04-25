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

module T = Test_common

(* Helpers for testing Xapi_vbd_helpers.valid_operations *)

let setup_test ~__context ~vdi_fun =
  let _sm_ref = T.make_sm ~__context () in
  let sr_ref = T.make_sr ~__context () in
  let (_: API.ref_PBD) = T.make_pbd ~__context ~sR:sr_ref () in
  let vm_ref = T.make_vm ~__context () in
  let vdi_ref = vdi_fun sr_ref in
  let vbd_ref = T.make_vbd ~vDI:vdi_ref ~vM:vm_ref ~__context () in
  let vbd_record = Db.VBD.get_record_internal ~__context ~self:vbd_ref in
  vbd_ref, vbd_record

let run_assert_equal_with_vdi ~__context msg ?(expensive_sharing_checks=true) ~vdi_fun op expected_error_if_any =
  let vbd_ref, vbd_record = setup_test ~__context ~vdi_fun in
  let get_error_code_of op =
    let valid_ops = (Xapi_vbd_helpers.valid_operations ~__context ~expensive_sharing_checks vbd_record vbd_ref) in
    match Hashtbl.find valid_ops op with
    | Some (code, _ ) -> Some code
    | None -> None
  in
  Alcotest.(check (option string))
    msg
    expected_error_if_any
    (get_error_code_of op)

(* These are to test Xapi_vbd_helpers.valid_operations against CA-253933 *)
let test_ca253933_invalid_operations () =
  let __context = Mock.make_context_with_new_db "Mock context" in
  (* When attaching a VDI which is currently under operation that cannot perform live, should raise other_operation_in_progress *)
  let invalid_operations =  [`resize ; `destroy ; `forget ; `update ; `force_unlock ; `generate_config; `snapshot; `resize_online; `blocked; `clone] in
  let operation_is_invalid op =
    run_assert_equal_with_vdi ~__context
      "test_ca253933_invalid_operations"
      ~vdi_fun:(fun sr_ref -> T.make_vdi ~sR:sr_ref ~__context ~managed:true ~current_operations:[("x", op)] ())
      `attach
      (Some Api_errors.other_operation_in_progress)
  in List.iter operation_is_invalid invalid_operations

let test_ca253933_valid_operations () =
  let __context = Mock.make_context_with_new_db "Mock context" in
  (* Should pass *)
  let valid_operations = [`mirror; `copy] in
  let operation_is_valid op =
    run_assert_equal_with_vdi ~__context
      "test_ca253933_valid_operations"
      ~vdi_fun:(fun sr_ref -> T.make_vdi ~sR:sr_ref ~__context ~managed:true ~current_operations:[("x", op)] ())
      `attach
      None
  in List.iter operation_is_valid valid_operations

let test =
  [ "test_ca253933_invalid_operations" , `Quick, test_ca253933_invalid_operations
  ; "test_ca253933_valid_operations" , `Quick, test_ca253933_valid_operations
  ]

