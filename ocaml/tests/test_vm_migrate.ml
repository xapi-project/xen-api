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

let mac1 = "00:00:00:00:00:01"
let mac2 = "00:00:00:00:00:02"

let check_network_map =
  Alcotest.(check (slist
                    (pair
                      (Alcotest_comparators.ref ())
                      (Alcotest_comparators.ref ()))
                    compare))

let test_infer_vif_map_empty () =
  let __context = Test_common.make_test_database () in
  check_network_map "Asserted by test_infer_vif_map_empty"
    []
    (Xapi_vm_migrate.infer_vif_map ~__context [] [])

let test_infer_vif_map () =
  let __context = Test_common.make_test_database () in
  let vm_vif1 = Test_common.make_vif ~__context ~mAC:mac1 () in
  let vm_vif2 = Test_common.make_vif ~__context ~mAC:mac2 () in
  let snap_vif1 = Test_common.make_vif ~__context ~mAC:mac1 () in
  let snap_vif2 = Test_common.make_vif ~__context ~mAC:mac2 () in
  (* In reality this network won't be in the local database, but for our
     	 * purposes it is a meaningless UUID so it's OK for it to be in the local
     	 * database. *)
  let network1 = Test_common.make_network ~__context () in
  check_network_map
    "test_infer_vif_map: check that a map with a single VIF -> network pair is unchanged"
    (Xapi_vm_migrate.infer_vif_map ~__context [vm_vif1] [vm_vif1, network1])
    [vm_vif1, network1];
  Alcotest.check_raises
    "test_infer_vif_map: check that a missing VIF is caught"
    Api_errors.(Server_error (vif_not_in_map, [Ref.string_of vm_vif2]))
    (fun () -> ignore
       (Xapi_vm_migrate.infer_vif_map ~__context
         [vm_vif1; vm_vif2]
         [vm_vif1, network1]));
  let inferred_map =
    Xapi_vm_migrate.infer_vif_map ~__context
      [vm_vif1; snap_vif1]
      [vm_vif1, network1]
  in
  Alcotest.(check (Alcotest_comparators.ref ()))
    "test_infer_vif_map: check that a snapshot VIF is mapped implicitly"
    (List.assoc snap_vif1 inferred_map) network1;
  Alcotest.check_raises
    "Check that an orphaned, unmapped snapshot VIF is caught."
    Api_errors.(Server_error (vif_not_in_map, [Ref.string_of snap_vif2]))
    (fun () -> ignore
       (Xapi_vm_migrate.infer_vif_map ~__context
         [vm_vif1; snap_vif1; snap_vif2]
         [vm_vif1, network1]))

let test =
  [ "test_infer_vif_map_empty", `Quick, test_infer_vif_map_empty
  ; "test_infer_vif_map", `Quick, test_infer_vif_map
  ]
