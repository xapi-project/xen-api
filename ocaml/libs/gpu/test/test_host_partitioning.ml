(*
 * Copyright (C) 2026 Cloud Software Group
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

(* Each row: the checks of the desired state, most specific first; a check
   after a failed one must not run. *)
let gpu_partitioning_desired (name, partitionable_gpu, pool_enabled, expected) =
  let test () =
    Alcotest.(check (result unit string))
      name expected
      Gpu.Host_partitioning.(
        desired ~partitionable_gpu ~pool_enabled |> Result.map_error token
      )
  in
  (name, `Quick, test)

let not_run () = Alcotest.fail "a check ran after an earlier one failed"

let tests =
  List.map gpu_partitioning_desired
    [
      ( "desired: no partitionable GPU"
      , Fun.const false
      , not_run
      , Error "no_partitionable_gpu"
      )
    ; ( "desired: a partitionable GPU, the pool switch off"
      , Fun.const true
      , Fun.const false
      , Error "pool_disabled"
      )
    ; ( "desired: a partitionable GPU, the pool switch on"
      , Fun.const true
      , Fun.const true
      , Ok ()
      )
    ]

let () = Alcotest.run "host_partitioning" [("desired", tests)]
