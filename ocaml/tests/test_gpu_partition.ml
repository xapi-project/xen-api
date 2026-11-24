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
   reference on the other end, so that no code ever writes them; and what a
   VGPU's reference to a partition means once the partition, its card or its
   host has changed under it. *)

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

(* -- resolve_ref: what a partition reference means now -------------------- *)

module P = Xapi_gpu_partition
module Lifecycle = Gpu.Gpu_partition_lifecycle

let resolution () =
  Alcotest.testable (Fmt.of_to_string P.string_of_resolution) ( = )

let both = [P.Resident; P.Scheduled]

(* A host with a card divided into one partition, the card at layout
   generation [generation]. *)
type card = {
    host: API.ref_host
  ; pgpu: API.ref_PGPU
  ; partition: API.ref_GPU_partition
}

let make_card ~__context ?(host = Test_common.make_host ~__context ())
    ?(generation = 3L) () =
  let pgpu =
    Test_common.make_pgpu ~__context ~host
      ~partition_layout_generation:generation ()
  in
  let partition = Test_common.make_gpu_partition ~__context ~pGPU:pgpu () in
  {host; pgpu; partition}

(* A VGPU bound to [card]'s partition through [binding] alone, at the card's
   current generation. *)
let make_bound ~__context ?(generation = 3L) card binding =
  let partition = card.partition in
  match binding with
  | P.Resident ->
      Test_common.make_vgpu ~__context ~resident_on:card.pgpu
        ~resident_on_partition:partition ~partition_layout_generation:generation
        ()
  | P.Scheduled ->
      Test_common.make_vgpu ~__context ~scheduled_to_be_resident_on:card.pgpu
        ~scheduled_to_be_resident_on_partition:partition
        ~partition_layout_generation:generation ()

let check_resolves ~__context msg expected ~self binding ~on_host =
  Alcotest.check (resolution ())
    (Printf.sprintf "%s (%s)" msg (P.string_of_binding binding))
    expected
    (P.resolve_ref ~__context ~self binding ~on_host)

let test_live () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context () in
  List.iter
    (fun binding ->
      let self = make_bound ~__context card binding in
      check_resolves ~__context
        "exists, on this host, same generation: the reference holds"
        (P.Live card.partition) ~self binding ~on_host:card.host
    )
    both

let test_unbound () =
  let __context = Test_common.make_test_database () in
  let host = Test_common.make_host ~__context () in
  let self = Test_common.make_vgpu ~__context () in
  List.iter
    (fun binding ->
      check_resolves ~__context "a null reference is no binding, not Destroyed"
        P.Unbound ~self binding ~on_host:host
    )
    both

(* The premise of the whole guard: destroying a partition does not clear the
   references to it. If the first check fails, the database has started
   clearing them and the premise has moved, not the code. *)
let test_destroyed_partition () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context () in
  let resident = make_bound ~__context card P.Resident in
  let scheduled = make_bound ~__context card P.Scheduled in
  Db.GPU_partition.destroy ~__context ~self:card.partition ;
  Alcotest.check (refs ())
    "the resident reference is still non-null after its partition is destroyed"
    [card.partition]
    [Db.VGPU.get_resident_on_partition ~__context ~self:resident] ;
  Alcotest.check (refs ())
    "the scheduled reference is still non-null after its partition is destroyed"
    [card.partition]
    [
      Db.VGPU.get_scheduled_to_be_resident_on_partition ~__context
        ~self:scheduled
    ] ;
  check_resolves ~__context "a reference to a destroyed partition" P.Destroyed
    ~self:resident P.Resident ~on_host:card.host ;
  check_resolves ~__context "a reference to a destroyed partition" P.Destroyed
    ~self:scheduled P.Scheduled ~on_host:card.host

let test_destroyed_card () =
  let __context = Test_common.make_test_database () in
  List.iter
    (fun binding ->
      let card = make_card ~__context () in
      let self = make_bound ~__context card binding in
      Db.PGPU.destroy ~__context ~self:card.pgpu ;
      check_resolves ~__context "a partition whose card is gone is gone with it"
        P.Destroyed ~self binding ~on_host:card.host
    )
    both

(* The case that looks right: the partition is a valid row, just not on the
   host the VM is on. *)
let test_wrong_host () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context () in
  let elsewhere = Test_common.make_host ~__context () in
  List.iter
    (fun binding ->
      let self = make_bound ~__context card binding in
      Alcotest.(check bool)
        "the partition is a valid row" true
        (Db.is_valid_ref __context card.partition) ;
      check_resolves ~__context "a valid partition on another host" P.Wrong_host
        ~self binding ~on_host:elsewhere
    )
    both

let test_stale_generation () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context ~generation:3L () in
  List.iter
    (fun binding ->
      let self = make_bound ~__context ~generation:3L card binding in
      Db.PGPU.set_partition_layout_generation ~__context ~self:card.pgpu
        ~value:4L ;
      check_resolves ~__context "the card was re-divided after binding"
        P.Stale_generation ~self binding ~on_host:card.host ;
      Db.PGPU.set_partition_layout_generation ~__context ~self:card.pgpu
        ~value:3L
    )
    both

(* -- clearing destroyed references -------------------------------------- *)

let test_clear_is_scheduled () =
  Alcotest.(check bool)
    "the garbage collector clears destroyed references" true
    (List.mem_assoc "VGPU partition refs" Db_gc_util.gc_subtask_list)

let test_clear_keeps_the_vgpu () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context () in
  let vm = Test_common.make_vm ~__context () in
  let self =
    Test_common.make_vgpu ~__context ~vM:vm ~resident_on:card.pgpu
      ~resident_on_partition:card.partition
      ~scheduled_to_be_resident_on_partition:card.partition
      ~partition_layout_generation:3L ()
  in
  Db.GPU_partition.destroy ~__context ~self:card.partition ;
  Db_gc_util.gc_VGPU_partition_refs ~__context ;
  Alcotest.(check bool)
    "the VGPU still exists" true
    (Db.is_valid_ref __context self) ;
  Alcotest.check (refs ()) "the VGPU still belongs to its VM" [vm]
    [Db.VGPU.get_VM ~__context ~self] ;
  Alcotest.check (refs ()) "the VM still has the VGPU" [self]
    (Db.VM.get_VGPUs ~__context ~self:vm) ;
  List.iter
    (fun binding ->
      check_resolves ~__context "a cleared reference is null" P.Unbound ~self
        binding ~on_host:card.host
    )
    both

let test_clear_leaves_the_rest_alone () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context () in
  let elsewhere = Test_common.make_host ~__context () in
  let live = make_bound ~__context card P.Resident in
  let stale = make_bound ~__context ~generation:2L card P.Resident in
  let unbound = Test_common.make_vgpu ~__context () in
  Db_gc_util.gc_VGPU_partition_refs ~__context ;
  check_resolves ~__context "a live reference is kept" (P.Live card.partition)
    ~self:live P.Resident ~on_host:card.host ;
  check_resolves ~__context
    "a reference on another host survives, so resume can say why" P.Wrong_host
    ~self:live P.Resident ~on_host:elsewhere ;
  check_resolves ~__context "a stale reference survives, so resume can say why"
    P.Stale_generation ~self:stale P.Resident ~on_host:card.host ;
  check_resolves ~__context "an unbound VGPU stays unbound" P.Unbound
    ~self:unbound P.Resident ~on_host:card.host ;
  Alcotest.(check int64)
    "clearing does not touch the recorded generation" 2L
    (Db.VGPU.get_partition_layout_generation ~__context ~self:stale)

(* One re-division strands every reference on the card, so clearing runs
   over many at once. It must clear exactly the destroyed ones and lose no
   VGPU. *)
let test_clear_at_fleet_scale () =
  let __context = Test_common.make_test_database () in
  let hosts = List.init 4 (fun _ -> Test_common.make_host ~__context ()) in
  let cards =
    List.concat_map
      (fun host ->
        List.init 4 (fun _ -> Test_common.make_pgpu ~__context ~host ())
      )
      hosts
  in
  let partitions =
    List.concat_map
      (fun pgpu ->
        List.init 7 (fun _ ->
            (pgpu, Test_common.make_gpu_partition ~__context ~pGPU:pgpu ())
        )
      )
      cards
    |> Array.of_list
  in
  let n = 1000 in
  let vgpus =
    List.init n (fun i ->
        let pgpu, partition = partitions.(i mod Array.length partitions) in
        let host = Db.PGPU.get_host ~__context ~self:pgpu in
        let self =
          if i mod 3 = 0 then
            Test_common.make_vgpu ~__context ~scheduled_to_be_resident_on:pgpu
              ~scheduled_to_be_resident_on_partition:partition ()
          else
            Test_common.make_vgpu ~__context ~resident_on:pgpu
              ~resident_on_partition:partition ()
        in
        ( self
        , host
        , partition
        , if i mod 3 = 0 then
            P.Scheduled
          else
            P.Resident
        )
    )
  in
  (* Re-divide every card on the first two hosts, and lose one card of the
     third outright. *)
  let doomed_hosts = [List.nth hosts 0; List.nth hosts 1] in
  let lost_card = List.nth cards 8 in
  let doomed =
    Array.to_list partitions
    |> List.filter (fun (pgpu, _) ->
        List.mem (Db.PGPU.get_host ~__context ~self:pgpu) doomed_hosts
    )
    |> List.map snd
  in
  List.iter (fun self -> Db.GPU_partition.destroy ~__context ~self) doomed ;
  Db.PGPU.destroy ~__context ~self:lost_card ;
  let lost =
    Array.to_list partitions
    |> List.filter (fun (pgpu, _) -> pgpu = lost_card)
    |> List.map snd
  in
  let destroyed partition =
    List.mem partition doomed || List.mem partition lost
  in
  let expected_cleared =
    List.length (List.filter (fun (_, _, p, _) -> destroyed p) vgpus)
  in
  Alcotest.(check bool)
    "the scenario strands some references and keeps others" true
    (expected_cleared > 0 && expected_cleared < n) ;
  Db_gc_util.gc_VGPU_partition_refs ~__context ;
  Alcotest.(check int)
    "no VGPU is destroyed" n
    (List.length (Db.VGPU.get_all ~__context)) ;
  let cleared, kept =
    List.fold_left
      (fun (cleared, kept) (self, host, partition, binding) ->
        match P.resolve_ref ~__context ~self binding ~on_host:host with
        | P.Unbound when destroyed partition ->
            (cleared + 1, kept)
        | P.Live p when p = partition && not (destroyed partition) ->
            (cleared, kept + 1)
        | other ->
            Alcotest.failf "VGPU %s: %s after clearing" (Ref.string_of self)
              (P.string_of_resolution other)
      )
      (0, 0) vgpus
  in
  Alcotest.(check int)
    "every destroyed reference is cleared" expected_cleared cleared ;
  Alcotest.(check int)
    "every live reference is kept" (n - expected_cleared) kept

(* -- what the lifecycle writes ------------------------------------------ *)

let pick partition ~__context:_ ~self:_ = Some partition

let test_reserve_refuses_another_card () =
  let __context = Test_common.make_test_database () in
  let reserved = make_card ~__context () in
  let other = make_card ~__context ~host:reserved.host () in
  let self =
    Test_common.make_vgpu ~__context ~scheduled_to_be_resident_on:reserved.pgpu
      ~partition_layout_generation:7L ()
  in
  P.apply ~__context ~self ~choose:(pick other.partition) Lifecycle.Reserve ;
  check_resolves ~__context
    "a partition on another card than the reservation is not written" P.Unbound
    ~self P.Scheduled ~on_host:reserved.host ;
  Alcotest.(check int64)
    "a refused pick records no generation" 7L
    (Db.VGPU.get_partition_layout_generation ~__context ~self) ;
  (* positive control: the same transition, on the right card *)
  P.apply ~__context ~self ~choose:(pick reserved.partition) Lifecycle.Reserve ;
  check_resolves ~__context "a partition on the reserved card is written"
    (P.Live reserved.partition) ~self P.Scheduled ~on_host:reserved.host

let test_reserve_refuses_destroyed () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context () in
  let self =
    Test_common.make_vgpu ~__context ~scheduled_to_be_resident_on:card.pgpu ()
  in
  Db.GPU_partition.destroy ~__context ~self:card.partition ;
  P.apply ~__context ~self ~choose:(pick card.partition) Lifecycle.Reserve ;
  check_resolves ~__context "a destroyed partition is not written" P.Unbound
    ~self P.Scheduled ~on_host:card.host

let test_reserve_records_generation () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context ~generation:5L () in
  let self =
    Test_common.make_vgpu ~__context ~scheduled_to_be_resident_on:card.pgpu ()
  in
  P.apply ~__context ~self ~choose:(pick card.partition) Lifecycle.Reserve ;
  Alcotest.(check int64)
    "binding records the card's generation" 5L
    (Db.VGPU.get_partition_layout_generation ~__context ~self) ;
  check_resolves ~__context "a fresh reservation is live"
    (P.Live card.partition) ~self P.Scheduled ~on_host:card.host

(* A reservation made stale before it is confirmed must stay stale: Confirm
   carries the binding over, it does not make a new one. *)
let test_confirm_keeps_generation () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context ~generation:5L () in
  let self =
    Test_common.make_vgpu ~__context ~scheduled_to_be_resident_on:card.pgpu ()
  in
  P.apply ~__context ~self ~choose:(pick card.partition) Lifecycle.Reserve ;
  Db.PGPU.set_partition_layout_generation ~__context ~self:card.pgpu ~value:6L ;
  Db.VGPU.set_resident_on ~__context ~self ~value:card.pgpu ;
  Db.VGPU.set_scheduled_to_be_resident_on ~__context ~self ~value:Ref.null ;
  P.apply ~__context ~self ~choose:(pick card.partition) Lifecycle.Confirm ;
  Alcotest.(check int64)
    "Confirm keeps the generation recorded at Reserve" 5L
    (Db.VGPU.get_partition_layout_generation ~__context ~self) ;
  check_resolves ~__context "the promoted reservation is still stale"
    P.Stale_generation ~self P.Resident ~on_host:card.host

let test_confirm_of_a_new_partition_records_generation () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context ~generation:9L () in
  let self = Test_common.make_vgpu ~__context ~resident_on:card.pgpu () in
  P.apply ~__context ~self ~choose:(pick card.partition) Lifecycle.Confirm ;
  check_resolves ~__context
    "confirming a partition that was not reserved is a new binding"
    (P.Live card.partition) ~self P.Resident ~on_host:card.host

(* -- occupancy ---------------------------------------------------------- *)

let test_occupants () =
  let __context = Test_common.make_test_database () in
  let card = make_card ~__context ~generation:3L () in
  let running = make_bound ~__context card P.Resident in
  let reserved = make_bound ~__context card P.Scheduled in
  let _stale = make_bound ~__context ~generation:2L card P.Resident in
  let both_refs =
    Test_common.make_vgpu ~__context ~resident_on_partition:card.partition
      ~scheduled_to_be_resident_on_partition:card.partition
      ~partition_layout_generation:3L ()
  in
  let sorted l = List.sort compare l in
  Alcotest.check (refs ())
    "live bindings occupy the partition, once each; a stale one does not"
    (sorted [running; reserved; both_refs])
    (sorted (P.occupants ~__context ~self:card.partition)) ;
  Db.GPU_partition.destroy ~__context ~self:card.partition ;
  Alcotest.check (refs ()) "a destroyed partition has no occupants" []
    (P.occupants ~__context ~self:card.partition)

let test =
  [
    ("test_partitions_of_pgpu", `Quick, test_partitions_of_pgpu)
  ; ("test_resident_vgpus", `Quick, test_resident_vgpus)
  ; ("test_scheduled_vgpus", `Quick, test_scheduled_vgpus)
  ; ("resolve_ref: live", `Quick, test_live)
  ; ("resolve_ref: unbound", `Quick, test_unbound)
  ; ("resolve_ref: destroyed partition", `Quick, test_destroyed_partition)
  ; ("resolve_ref: destroyed card", `Quick, test_destroyed_card)
  ; ("resolve_ref: wrong host", `Quick, test_wrong_host)
  ; ("resolve_ref: stale generation", `Quick, test_stale_generation)
  ; ("clear_destroyed_refs: run by the GC", `Quick, test_clear_is_scheduled)
  ; ( "clear_destroyed_refs: clears the reference, keeps the VGPU"
    , `Quick
    , test_clear_keeps_the_vgpu
    )
  ; ( "clear_destroyed_refs: leaves the rest alone"
    , `Quick
    , test_clear_leaves_the_rest_alone
    )
  ; ("clear_destroyed_refs: fleet scale", `Quick, test_clear_at_fleet_scale)
  ; ( "Reserve: refuses a partition on another card"
    , `Quick
    , test_reserve_refuses_another_card
    )
  ; ( "Reserve: refuses a destroyed partition"
    , `Quick
    , test_reserve_refuses_destroyed
    )
  ; ("Reserve: records the generation", `Quick, test_reserve_records_generation)
  ; ("Confirm: keeps the generation", `Quick, test_confirm_keeps_generation)
  ; ( "Confirm: a new partition records the generation"
    , `Quick
    , test_confirm_of_a_new_partition_records_generation
    )
  ; ("occupants", `Quick, test_occupants)
  ]
