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
  match f () with
  | exception Api_errors.(Server_error (actual_error, _)) ->
      Alcotest.(check string) msg expected_error actual_error
  | _ ->
      Alcotest.fail msg

(*--- Xapi_pgpu.assert_can_run_VGPU tests ---*)

let test_can_run_VGPU_succeeds_empty_pgpu () =
  on_host_with_k2 (fun __context p ->
      let vgpu = VGPU_T.(make_vgpu ~__context k260q) in
      Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu
  )

let test_can_run_VGPU_succeeds_enabled_types () =
  on_host_with_k2 (fun __context p ->
      let vgpus =
        VGPU_T.(List.map (make_vgpu ~__context) [k200; k240q; k260q])
      in
      ignore
        (List.map
           (fun vgpu -> Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu)
           vgpus
        )
  )

let test_can_run_VGPU_succeeds_same_type () =
  on_host_with_k2 (fun __context p ->
      let (_ : API.ref_VGPU) =
        VGPU_T.(make_vgpu ~__context ~resident_on:p k260q)
      in
      let vgpu = VGPU_T.(make_vgpu ~__context k260q) in
      Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu
  )

let test_can_run_VGPU_fails_unsupported_types () =
  on_host_with_k2 (fun __context p ->
      let vgpus = VGPU_T.(List.map (make_vgpu ~__context) [k100; k140q]) in
      ignore
        (List.map
           (fun vgpu ->
             assert_raises_api_error "test_can_run_VGPU_fails_unsupported_types"
               Api_errors.vgpu_type_not_supported (fun () ->
                 Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu
             )
           )
           vgpus
        )
  )

let test_can_run_VGPU_fails_disabled_type () =
  on_host_with_k2 (fun __context p ->
      let vgpu = VGPU_T.(make_vgpu ~__context k200) in
      let vgpu_type = Db.VGPU.get_type ~__context ~self:vgpu in
      Db.PGPU.remove_enabled_VGPU_types ~__context ~self:p ~value:vgpu_type ;
      assert_raises_api_error "test_can_run_VGPU_fails_disabled_type"
        Api_errors.vgpu_type_not_enabled (fun () ->
          Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu
      )
  )

let test_can_run_VGPU_fails_different_type () =
  on_host_with_k2 (fun __context p ->
      let (_ : API.ref_VGPU) =
        VGPU_T.(make_vgpu ~__context ~resident_on:p k260q)
      in
      let vgpu = VGPU_T.(make_vgpu ~__context k240q) in
      assert_raises_api_error "test_can_run_VGPU_fails_different_type"
        Api_errors.vgpu_type_not_compatible_with_running_type (fun () ->
          Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu
      )
  )

let test_can_run_VGPU_fails_no_capacity () =
  on_host_with_k2 (fun __context p ->
      (* Fill up the pGPU with 2 x K260Q *)
      let (_ : API.ref_VGPU list) =
        VGPU_T.(List.map (make_vgpu ~__context ~resident_on:p) [k260q; k260q])
      in
      (* Should fail to put another one on *)
      let vgpu = VGPU_T.(make_vgpu ~__context k260q) in
      assert_raises_api_error "test_can_run_VGPU_fails_no_capacity"
        Api_errors.pgpu_insufficient_capacity_for_vgpu (fun () ->
          Xapi_pgpu.assert_can_run_VGPU ~__context ~self:p ~vgpu
      )
  )

(*--- Xapi_pgpu.get_remaining_capacity tests ---*)

let check_capacity_is ~__context expected_capacity pgpu vgpu_type =
  let vgpu_type = Xapi_vgpu_type.find_or_create ~__context vgpu_type in
  Alcotest.(check int64)
    "check_capacity_is" expected_capacity
    (Xapi_pgpu.get_remaining_capacity ~__context ~self:pgpu ~vgpu_type)

let expected_capacities = VGPU_T.[(k200, 8L); (k240q, 4L); (k260q, 2L)]

let test_remaining_capacity_unsupported_types () =
  on_host_with_k2 (fun __context p ->
      ignore (List.map (check_capacity_is ~__context 0L p) VGPU_T.[k100; k140q])
  )

let test_remaining_capacity_supported_types () =
  on_host_with_k2 (fun __context p ->
      ignore
        (List.map
           (fun (v, c) -> check_capacity_is ~__context c p v)
           expected_capacities
        )
  )

let test_remaining_capacity_decreases () =
  on_host_with_k2 (fun __context _ ->
      let rec check_remaining_capacity_and_fill p c vgpu_type =
        check_capacity_is ~__context c p vgpu_type ;
        if c > 0L then (
          ignore VGPU_T.(make_vgpu ~__context ~resident_on:p vgpu_type) ;
          check_remaining_capacity_and_fill p (Int64.sub c 1L) vgpu_type
        )
      in
      ignore
        (List.map
           (fun (vgpu_type, capacity) ->
             let p = VGPU_T.(make_pgpu ~__context default_k2) in
             check_remaining_capacity_and_fill p capacity vgpu_type
           )
           expected_capacities
        )
  )

(*--- Xapi_pgpu.set_GPU_group ---*)

let test_set_GPU_group_succeeds_empty_pgpu () =
  on_host_with_k2 (fun __context p ->
      let group_ref = T.make_gpu_group ~__context () in
      Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group_ref
  )

let test_set_GPU_group_succeeds_orphan_vgpu () =
  (* This is OK since vGPUs can be created on empty GPU groups *)
  on_host_with_k2 (fun __context p ->
      let group, group' =
        (T.make_gpu_group ~__context (), T.make_gpu_group ~__context ())
      in
      Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group ;
      let (_ : API.ref_VGPU) = T.make_vgpu ~__context ~gPU_group:group () in
      Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group'
  )

let test_set_GPU_group_fails_resident_vgpu () =
  on_host_with_k2 (fun __context p ->
      let group, group' =
        (T.make_gpu_group ~__context (), T.make_gpu_group ~__context ())
      in
      Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group ;
      ignore VGPU_T.(make_vgpu ~__context ~resident_on:p k200) ;
      assert_raises_api_error "test_set_GPU_group_fails_resident_vgpu"
        Api_errors.pgpu_in_use_by_vm (fun () ->
          Xapi_pgpu.set_GPU_group ~__context ~self:p ~value:group'
      )
  )

let test_can_run_vgpu =
  [
    ( "test_can_run_VGPU_succeeds_empty_pgpu"
    , `Quick
    , test_can_run_VGPU_succeeds_empty_pgpu
    )
  ; ( "test_can_run_VGPU_succeeds_enabled_types"
    , `Quick
    , test_can_run_VGPU_succeeds_enabled_types
    )
  ; ( "test_can_run_VGPU_succeeds_same_type"
    , `Quick
    , test_can_run_VGPU_succeeds_same_type
    )
  ; ( "test_can_run_VGPU_fails_unsupported_types"
    , `Quick
    , test_can_run_VGPU_fails_unsupported_types
    )
  ; ( "test_can_run_VGPU_fails_disabled_type"
    , `Quick
    , test_can_run_VGPU_fails_disabled_type
    )
  ; ( "test_can_run_VGPU_fails_different_type"
    , `Quick
    , test_can_run_VGPU_fails_different_type
    )
  ; ( "test_can_run_VGPU_fails_no_capacity"
    , `Quick
    , test_can_run_VGPU_fails_no_capacity
    )
  ]

let test_remaining_capacity =
  [
    ( "test_remaining_capacity_unsupported_types"
    , `Quick
    , test_remaining_capacity_unsupported_types
    )
  ; ( "test_remaining_capacity_supported_types"
    , `Quick
    , test_remaining_capacity_supported_types
    )
  ; ( "test_remaining_capacity_decreases"
    , `Quick
    , test_remaining_capacity_decreases
    )
  ]

let test_set_GPU_group =
  [
    ( "test_set_GPU_group_succeeds_empty_pgpu"
    , `Quick
    , test_set_GPU_group_succeeds_empty_pgpu
    )
  ; ( "test_set_GPU_group_succeeds_orphan_vgpu"
    , `Quick
    , test_set_GPU_group_succeeds_orphan_vgpu
    )
  ; ( "test_set_GPU_group_fails_resident_vgpu"
    , `Quick
    , test_set_GPU_group_fails_resident_vgpu
    )
  ]

(*--- Xapi_pgpu.update_partition_mode tests ---*)

(* Each row: the value before, the axes read, the value written *)
let test_partition_mode =
  let open Gpu.Partition_mode in
  List.map
    (fun (name, before, axes, expected) ->
      ( name
      , `Quick
      , fun () ->
          let __context = T.make_test_database () in
          let self = T.make_pgpu ~__context ~partition_mode:before () in
          Xapi_pgpu.update_partition_mode ~__context ~self axes ;
          Alcotest.(check string)
            name
            (API.partition_mode_to_string expected)
            (API.partition_mode_to_string
               (Db.PGPU.get_partition_mode ~__context ~self)
            )
      )
    )
    [
      ("no axes: not_supported", `unknown, [], `not_supported)
    ; ("never divided, unread: unknown", `not_supported, [None], `unknown)
    ; ("one axis, undivided: disabled", `unknown, [Some Disabled], `disabled)
    ; ( "one of two axes partitioned: enabled"
      , `disabled
      , [Some Enabled; Some Disabled]
      , `enabled
      )
    ; ( "divided, one of two axes unread: possibly_enabled"
      , `enabled
      , [Some Enabled; None]
      , `possibly_enabled
      )
    ; ( "one axis, pending: enable_on_reboot"
      , `possibly_enabled
      , [Some Enable_on_reboot]
      , `enable_on_reboot
      )
    ; ( "pending division, unread: possibly_enabled"
      , `enable_on_reboot
      , [None]
      , `possibly_enabled
      )
    ]

(*--- Xapi_pgpu.sync_held_back and sync_gpus ---*)

(* The coordinator (this host) and a second host, both at one version; returns
   both *)
let pool_of_two ~gpu_partitioning =
  let __context = T.make_test_database () in
  let master = Helpers.get_localhost ~__context in
  let other = T.make_host ~__context () in
  List.iter
    (fun host ->
      T.set_host_version ~__context ~host ~key:Xapi_globs._platform_version
        "1.0.0" ;
      T.set_host_version ~__context ~host ~key:Xapi_globs._xapi_version "1.0"
    )
    [master; other] ;
  Db.Pool.set_gpu_partitioning ~__context
    ~self:(Helpers.get_pool ~__context)
    ~value:gpu_partitioning ;
  (__context, master, other)

let ahead ~__context host =
  T.set_host_version ~__context ~host ~key:Xapi_globs._xapi_version "99.0"

let held_back name expected ~__context host =
  Alcotest.(check bool) name expected (Xapi_pgpu.sync_held_back ~__context ~host)

let test_held_back_disabled_behind_host_syncs () =
  let __context, master, other = pool_of_two ~gpu_partitioning:`disabled in
  ahead ~__context other ;
  held_back "behind, setting disabled" false ~__context master

let test_held_back_newest_host_syncs () =
  let __context, _, other = pool_of_two ~gpu_partitioning:`enabled in
  ahead ~__context other ;
  held_back "newest host" false ~__context other

let test_held_back_behind_host () =
  let __context, master, other = pool_of_two ~gpu_partitioning:`enabled in
  ahead ~__context other ;
  held_back "behind host" true ~__context master

(* default_policy means disabled in this version *)
let test_held_back_default_policy_behind_host_syncs () =
  let __context, master, other =
    pool_of_two ~gpu_partitioning:`default_policy
  in
  ahead ~__context other ;
  held_back "behind, setting default_policy" false ~__context master

let test_held_back_same_versions () =
  let __context, master, other = pool_of_two ~gpu_partitioning:`enabled in
  held_back "coordinator" false ~__context master ;
  held_back "other host" false ~__context other

(* Edits the pool's row in the database, as a coordinator of another version
   holds it *)
let edit_pool_row ~__context edit =
  let open Xapi_database.Db_cache_types in
  let pool = Ref.string_of (Helpers.get_pool ~__context) in
  let edit_pool = Table.update 0L pool Row.empty edit in
  Xapi_database.Db_ref.update_database
    (Context.database_of __context)
    (Database.update (TableSet.update 0L "pool" Table.empty edit_pool))

(* A coordinator of a version without the setting: it is read as not
   enabled *)
let test_held_back_no_field () =
  let __context, master, other = pool_of_two ~gpu_partitioning:`enabled in
  ahead ~__context other ;
  edit_pool_row ~__context
    (Xapi_database.Db_cache_types.Row.remove 0L "gpu_partitioning") ;
  held_back "coordinator without the setting" false ~__context master

(* A value of a later version: it is read as not enabled *)
let test_held_back_unknown_value () =
  let __context, master, other = pool_of_two ~gpu_partitioning:`enabled in
  ahead ~__context other ;
  edit_pool_row ~__context
    (Xapi_database.Db_cache_types.Row.add 0L "gpu_partitioning"
       (Schema.Value.String "a_later_value")
    ) ;
  held_back "a value this version does not know" false ~__context master

let local_pgpus ~__context =
  Db.Host.get_PGPUs ~__context ~self:(Helpers.get_localhost ~__context)
  |> List.map Ref.string_of
  |> List.sort String.compare

let sync_result = function `Synced -> "synced" | `Held_back -> "held back"

(* Held back, the sync does not run: it would remove the GPU records that the
   test's machine does not have *)
let test_sync_gpus_behind_host_held_back () =
  let __context, master, other = pool_of_two ~gpu_partitioning:`enabled in
  ahead ~__context other ;
  let pCI = T.make_pci ~__context ~host:master ~vendor_id:"10de" () in
  let (_ : API.ref_PGPU) = T.make_pgpu ~__context ~pCI ~host:master () in
  let before = local_pgpus ~__context in
  let result = Xapi_pgpu.sync_gpus ~__context () in
  Alcotest.(check (pair string (list string)))
    "held back, rows unchanged" ("held back", before)
    (sync_result result, local_pgpus ~__context)

(* The sync's mutex is released after a held-back sync *)
let test_sync_gpus_held_back_twice () =
  let __context, _, other = pool_of_two ~gpu_partitioning:`enabled in
  ahead ~__context other ;
  let first = Xapi_pgpu.sync_gpus ~__context () in
  let second = Xapi_pgpu.sync_gpus ~__context () in
  Alcotest.(check (pair string string))
    "both held back" ("held back", "held back")
    (sync_result first, sync_result second)

(* Not held back, the sync runs. With no GPU rows it reads nothing of the
   test's machine *)
let test_sync_gpus_one_version_synced () =
  let __context, _, _ = pool_of_two ~gpu_partitioning:`enabled in
  Context.set_test_rpc __context (Mock_rpc.rpc __context) ;
  Xapi_pgpu.sync_gpus ~__context ()
  |> sync_result
  |> Alcotest.(check string) "synced" "synced"

(* A held-back sync keeps the row of a GPU that has left the PCI scan, whose
   PCI record the start has removed *)
let test_has_nvidia_gpu_pci_gone () =
  let __context = T.make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let pCI = T.make_pci ~__context ~host ~vendor_id:"10de" () in
  let (_ : API.ref_PGPU) = T.make_pgpu ~__context ~pCI ~host () in
  Db.PCI.destroy ~__context ~self:pCI ;
  Xapi_pgpu.has_nvidia_gpu ~__context ~host
  |> Alcotest.(check bool) "not counted" false

let test_sync_held_back =
  [
    ( "held back: setting disabled, a behind host syncs as before"
    , `Quick
    , test_held_back_disabled_behind_host_syncs
    )
  ; ( "held back: setting enabled, the newest host syncs"
    , `Quick
    , test_held_back_newest_host_syncs
    )
  ; ( "held back: setting enabled, a behind host is held back"
    , `Quick
    , test_held_back_behind_host
    )
  ; ( "held back: setting default_policy, a behind host syncs as before"
    , `Quick
    , test_held_back_default_policy_behind_host_syncs
    )
  ; ( "held back: setting enabled, one version, no host is held back"
    , `Quick
    , test_held_back_same_versions
    )
  ; ( "held back: the coordinator's version has no setting, a behind host syncs"
    , `Quick
    , test_held_back_no_field
    )
  ; ( "held back: a stored value this version does not know, a behind host syncs"
    , `Quick
    , test_held_back_unknown_value
    )
  ; ( "sync_gpus: a behind host is held back and its rows are unchanged"
    , `Quick
    , test_sync_gpus_behind_host_held_back
    )
  ; ( "sync_gpus: a second sync after a held-back one is not blocked"
    , `Quick
    , test_sync_gpus_held_back_twice
    )
  ; ( "sync_gpus: hosts at one version, the sync runs"
    , `Quick
    , test_sync_gpus_one_version_synced
    )
  ; ( "has_nvidia_gpu: a GPU whose PCI record is gone is not counted"
    , `Quick
    , test_has_nvidia_gpu_pci_gone
    )
  ]

let test =
  test_can_run_vgpu
  @ test_remaining_capacity
  @ test_set_GPU_group
  @ test_partition_mode
  @ test_sync_held_back
