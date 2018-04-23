(*
 * Copyright (C) 2013 Citrix Systems Inc.
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
module VGPU_T = Test_vgpu_common

(*--- Helper functions ---*)

let on_host_with_k2 (f : Context.t -> API.ref_PGPU -> 'a) =
  let __context = T.make_test_database () in
  let p = VGPU_T.(make_pgpu ~__context default_k2) in
  f __context p

let assert_raises_api_error msg expected_error f =
  match (f ()) with
  | exception Api_errors.(Server_error (actual_error,params)) ->
    Alcotest.(check string)
      msg
      expected_error
      actual_error
  | _ -> Alcotest.fail msg

(*--- Xapi_pgpu.assert_can_run_VGPU tests ---*)

let test_can_run_VGPU_succeeds_empty_pgpu () =
  on_host_with_k2 (fun __context p ->
      let vgpu = VGPU_T.(make_vgpu ~__context k260q) in
      Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu)

let test_can_run_VGPU_succeeds_enabled_types () =
  on_host_with_k2 (fun __context p ->
      let vgpus = VGPU_T.(List.map (make_vgpu ~__context) [k200; k240q; k260q]) in
      ignore (List.map (fun vgpu ->
          Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu)
          vgpus))

let test_can_run_VGPU_succeeds_same_type () =
  on_host_with_k2 (fun __context p ->
      let (_:API.ref_VGPU) = VGPU_T.(make_vgpu ~__context ~resident_on:p k260q) in
      let vgpu = VGPU_T.(make_vgpu ~__context k260q) in
      Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu)

let test_can_run_VGPU_fails_unsupported_types () =
  on_host_with_k2 (fun __context p ->
      let vgpus = VGPU_T.(List.map (make_vgpu ~__context) [k100; k140q]) in
      ignore (List.map (fun vgpu ->
          assert_raises_api_error
            "test_can_run_VGPU_fails_unsupported_types"
            Api_errors.vgpu_type_not_supported
            (fun () -> Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu))
          vgpus))

let test_can_run_VGPU_fails_disabled_type () =
  on_host_with_k2 (fun __context p ->
      let vgpu = VGPU_T.(make_vgpu ~__context k200) in
      let vgpu_type = Db.VGPU.get_type ~__context ~self:vgpu in
      Db.PGPU.remove_enabled_VGPU_types ~__context ~self:p ~value:vgpu_type;
      assert_raises_api_error
        "test_can_run_VGPU_fails_disabled_type"
        Api_errors.vgpu_type_not_enabled
        (fun () -> Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu))

let test_can_run_VGPU_fails_different_type () =
  on_host_with_k2 (fun __context p ->
      let (_:API.ref_VGPU) = VGPU_T.(make_vgpu ~__context ~resident_on:p k260q) in
      let vgpu = VGPU_T.(make_vgpu ~__context k240q) in
      assert_raises_api_error
        "test_can_run_VGPU_fails_different_type"
        Api_errors.vgpu_type_not_compatible_with_running_type
        (fun () -> Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu))

let test_can_run_VGPU_fails_no_capacity () =
  on_host_with_k2 (fun __context p ->
      (* Fill up the pGPU with 2 x K260Q *)
      let (_:API.ref_VGPU list) =
        VGPU_T.(List.map (make_vgpu ~__context ~resident_on:p) [k260q; k260q]) in
      (* Should fail to put another one on *)
      let vgpu = VGPU_T.(make_vgpu ~__context k260q) in
      assert_raises_api_error
        "test_can_run_VGPU_fails_no_capacity"
        Api_errors.pgpu_insufficient_capacity_for_vgpu
        (fun () -> Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu))

(*--- Xapi_pgpu.get_remaining_capacity tests ---*)

let check_capacity_is ~__context expected_capacity pgpu vgpu_type =
  let vgpu_type = Xapi_vgpu_type.find_or_create ~__context vgpu_type in
  Alcotest.(check int64)
    "check_capacity_is"
    expected_capacity
    (Xapi_pgpu.get_remaining_capacity ~__context ~self:pgpu ~vgpu_type)

let expected_capacities = VGPU_T.([(k200, 8L); (k240q, 4L); (k260q, 2L)])

let test_remaining_capacity_unsupported_types () =
  on_host_with_k2 (fun __context p ->
      ignore (List.map (check_capacity_is ~__context 0L p) VGPU_T.([k100; k140q])))

let test_remaining_capacity_supported_types () =
  on_host_with_k2 (fun __context p ->
      ignore (List.map
                (fun (v, c) -> check_capacity_is ~__context c p v) expected_capacities))

let test_remaining_capacity_decreases () =
  on_host_with_k2 (fun __context _ ->
      let rec check_remaining_capacity_and_fill p c vgpu_type =
        check_capacity_is ~__context c p vgpu_type;
        if c > 0L then begin
          ignore VGPU_T.(make_vgpu ~__context ~resident_on:p vgpu_type);
          check_remaining_capacity_and_fill p (Int64.sub c 1L) vgpu_type
        end
      in
      ignore (List.map
                (fun (vgpu_type, capacity) ->
                   let p = VGPU_T.(make_pgpu ~__context default_k2) in
                   check_remaining_capacity_and_fill p capacity vgpu_type)
                expected_capacities))

(*--- Xapi_pgpu.set_GPU_group ---*)

let test_set_GPU_group_succeeds_empty_pgpu () =
  on_host_with_k2 (fun __context p ->
      let group_ref = T.make_gpu_group ~__context () in
      Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group_ref)

let test_set_GPU_group_succeeds_orphan_vgpu () =
  (* This is OK since vGPUs can be created on empty GPU groups *)
  on_host_with_k2 (fun __context p ->
      let group, group' =
        (T.make_gpu_group ~__context (), T.make_gpu_group ~__context ())
      in
      Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group;
      let (_: API.ref_VGPU) =
        T.make_vgpu ~__context ~gPU_group:group ()
      in
      Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group')

let test_set_GPU_group_fails_resident_vgpu () =
  on_host_with_k2 (fun __context p ->
      let group, group' =
        (T.make_gpu_group ~__context (), T.make_gpu_group ~__context ())
      in
      Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group;
      ignore VGPU_T.(make_vgpu ~__context ~resident_on:p k200);
      assert_raises_api_error
        "test_set_GPU_group_fails_resident_vgpu"
        Api_errors.pgpu_in_use_by_vm
        (fun () -> Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group'))

let test_can_run_vgpu =
  [ "test_can_run_VGPU_succeeds_empty_pgpu", `Quick, test_can_run_VGPU_succeeds_empty_pgpu
  ; "test_can_run_VGPU_succeeds_enabled_types", `Quick, test_can_run_VGPU_succeeds_enabled_types
  ; "test_can_run_VGPU_succeeds_same_type", `Quick, test_can_run_VGPU_succeeds_same_type
  ; "test_can_run_VGPU_fails_unsupported_types", `Quick, test_can_run_VGPU_fails_unsupported_types
  ; "test_can_run_VGPU_fails_disabled_type", `Quick, test_can_run_VGPU_fails_disabled_type
  ; "test_can_run_VGPU_fails_different_type", `Quick, test_can_run_VGPU_fails_different_type
  ; "test_can_run_VGPU_fails_no_capacity", `Quick, test_can_run_VGPU_fails_no_capacity
  ]

let test_remaining_capacity =
  [ "test_remaining_capacity_unsupported_types", `Quick, test_remaining_capacity_unsupported_types
  ; "test_remaining_capacity_supported_types", `Quick, test_remaining_capacity_supported_types
  ; "test_remaining_capacity_decreases", `Quick, test_remaining_capacity_decreases
  ]

let test_set_GPU_group =
  [ "test_set_GPU_group_succeeds_empty_pgpu", `Quick, test_set_GPU_group_succeeds_empty_pgpu
  ; "test_set_GPU_group_succeeds_orphan_vgpu", `Quick, test_set_GPU_group_succeeds_orphan_vgpu
  ; "test_set_GPU_group_fails_resident_vgpu", `Quick, test_set_GPU_group_fails_resident_vgpu
  ]

let test =
  (  test_can_run_vgpu
     @ test_remaining_capacity
     @ test_set_GPU_group
  )
