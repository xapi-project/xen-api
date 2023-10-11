(*
  Copyright (C) Cloud Software Group
 
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation; version 2.1 only. with the special
  exception on linking described in file LICENSE.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.
*)

open Bechamel

external bench_fixed_work : int -> int = "caml_bench_fixed_work"

(* Performs a fixed amount of work, see also the comment in the C stub.

   We could calibrate how long this takes on a given system and adjust loop count accordingly, but that means
    that when rerunning the executable we'll get slightly different numbers (e.g. due to unpredictability of time slices inside a VM, etc.)
   Use a fixed number here, the actual value doesn't matter much as long as the time is reasonably short (e.g. on the order of 10ms, comparable to PAM) and deterministic.
   This will change with different compilers, or when run on different CPU architectures, but for a given binary and system it should perform a fixed amount of work.

   The goal is to use this fixed amount of work to evaluate how well OCaml (4.x) is able to dispatch parallel work to C functions, and
   how much intereference there is from the master lock.
*)
let parallel_c_work () =
  let (_ : int) = Sys.opaque_identity @@ bench_fixed_work @@ 10_000_000 in
  ()

let args = [1; 2; 4; 8; 16]

open Ezbechamel_concurrent

let () =
  Ezbechamel_alcotest_notty.run
    [
      Test.make ~name:"overhead" (Staged.stage ignore)
    ; Test.make_grouped ~name:"fixedwork"
        [
          Test.make ~name:"fixedwork" (Staged.stage parallel_c_work)
        ; test_concurrently ~allocate:ignore ~free:ignore
            ~name:"concurrent fixedwork"
            Staged.(stage parallel_c_work)
        ]
    ; Test.make_indexed ~name:"Thread create/join" ~args (fun n ->
          Staged.stage @@ fun () ->
          let threads = Array.init n @@ Thread.create ignore in
          Array.iter Thread.join threads
      )
    ; (let open Pingpong in
      Test.make_grouped ~name:"pingpong"
        [
          test_pingpong ~args (module TestEvent)
        ; test_pingpong ~args (module TestBinSem)
          (* they are implemented using Mutex/Condition internally so we don't need a separate test for that *)
        ; test_pingpong ~args (module TestCountSem)
          (* they are implemented using Mutex/Condition internally so we don't need a separate test for that *)
        ]
      )
    ; (let open Barriers in
      Test.make_grouped ~name:"barrier"
        [
          test_barrier (module BarrierCond)
        ; test_barrier (module BarrierPreloaded)
        ; test_barrier (module BarrierBinary)
        ; test_barrier (module BarrierCounting)
        ; test_barrier (module BarrierBinaryArray)
        ; test_concurrently ~allocate:ignore ~free:ignore
            ~name:"concurrent workers"
            Staged.(stage ignore)
        ; test_barrier (module BarrierYield)
        ]
      )
    ]
