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

open Test_common

(* Helpers for testing Xapi_sr_operations.assert_operation_valid *)

let setup_test ~__context ?sm_fun ?vdi_fun () =
  let run f x = match f with Some f -> ignore(f x) | None -> () in
  let _sm_ref = make_sm ~__context () in
  let sr_ref = make_sr ~__context () in
  let (_: API.ref_PBD) = make_pbd ~__context ~sR:sr_ref () in
  run sm_fun _sm_ref;
  let sr_record = Db.SR.get_record ~__context ~self:sr_ref in
  sr_ref, sr_record

let check_same_error_code =
  let open Alcotest in
  let open Alcotest_comparators in
  check (option error_code) "Same error code"

let check_operation_error f exn =
  let exn' = try ignore (f ()); None with Api_errors.Server_error(c,p) -> Some (c,p) in
  check_same_error_code exn exn' 

(** The set of allowed operations must be restricted during rolling pool
    upgrade to the enums known by older releases. *)
let test_operations_restricted_during_rpu =
  let test_check_operation_error () =
    let __context = Mock.make_context_with_new_db "Mock context" in
    let master = Test_common.make_host __context () in
    let pool = Test_common.make_pool ~__context ~master () in
    let sr_ref, _ = setup_test ~__context () in

    (* Artificially put xapi into RPU mode - see Helpers.rolling_upgrade_in_progress *)
    Db.Pool.add_to_other_config
      ~__context
      ~self:pool
      ~key:Xapi_globs.rolling_upgrade_in_progress
      ~value:"x";

    (* Check that a recent allowed_operation won't appear in allowed_operations
     * during RPU mode, and that the error is correct *)
    check_operation_error
      (fun () ->
        Xapi_sr_operations.assert_operation_valid
          ~__context ~self:sr_ref ~op:`vdi_enable_cbt) 
      (Some(Api_errors.not_supported_during_upgrade, []));

    (* Take us out of RPU mode*)
    Db.Pool.remove_from_other_config
      ~__context
      ~self:pool
      ~key:Xapi_globs.rolling_upgrade_in_progress;

    (* Check that the operation does appear when we're not in RPU mode *)
    check_operation_error
      (fun () ->
         Xapi_sr_operations.assert_operation_valid
           ~__context ~self:sr_ref ~op:`vdi_enable_cbt)
      None
  in

  [ "test_check_operation_error", `Quick, test_check_operation_error
  ]

let test =
  test_operations_restricted_during_rpu
