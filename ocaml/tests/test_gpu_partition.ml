(*
 * Copyright (C) Cloud Software Group
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

(* The GPU_partition class is declared but nothing populates it yet. What can
   be checked without a driver is the shape of the declaration: that the three
   set-valued ends of its relations are maintained by the database from the
   reference on the other end, so that no code ever writes them. *)

let refs () = Alcotest.list (Alcotest_comparators.ref ())

let test_partitions_of_pgpu () =
  let __context = Test_common.make_test_database () in
  let pGPU = Test_common.make_pgpu ~__context () in
  let partition = Test_common.make_gpu_partition ~__context ~pGPU () in
  Alcotest.check (refs ()) "PGPU.partitions follows GPU_partition.PGPU"
    [partition]
    (Db.PGPU.get_partitions ~__context ~self:pGPU) ;
  Db.GPU_partition.set_PGPU ~__context ~self:partition ~value:Ref.null ;
  Alcotest.check (refs ())
    "PGPU.partitions empties when the reference is cleared" []
    (Db.PGPU.get_partitions ~__context ~self:pGPU)

let test_resident_vgpus () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let vgpu = Test_common.make_vgpu ~__context () in
  Alcotest.check (refs ()) "a fresh partition has no resident VGPUs" []
    (Db.GPU_partition.get_resident_VGPUs ~__context ~self:partition) ;
  Db.VGPU.set_resident_on_partition ~__context ~self:vgpu ~value:partition ;
  Alcotest.check (refs ())
    "GPU_partition.resident_VGPUs follows VGPU.resident_on_partition" [vgpu]
    (Db.GPU_partition.get_resident_VGPUs ~__context ~self:partition)

let test_scheduled_vgpus () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let vgpu = Test_common.make_vgpu ~__context () in
  Db.VGPU.set_scheduled_to_be_resident_on_partition ~__context ~self:vgpu
    ~value:partition ;
  Alcotest.check (refs ())
    "GPU_partition.scheduled_VGPUs follows \
     VGPU.scheduled_to_be_resident_on_partition"
    [vgpu]
    (Db.GPU_partition.get_scheduled_VGPUs ~__context ~self:partition) ;
  Alcotest.check (refs ()) "a reservation is not also an occupancy" []
    (Db.GPU_partition.get_resident_VGPUs ~__context ~self:partition)

let test =
  [
    ("test_partitions_of_pgpu", `Quick, test_partitions_of_pgpu)
  ; ("test_resident_vgpus", `Quick, test_resident_vgpus)
  ; ("test_scheduled_vgpus", `Quick, test_scheduled_vgpus)
  ]
