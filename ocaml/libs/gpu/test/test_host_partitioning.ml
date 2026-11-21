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
let gpu_partitioning_desired
    (name, partitionable_gpu, forced, pool_enabled, expected) =
  let test () =
    Alcotest.(check (result unit string))
      name expected
      Gpu.Host_partitioning.(
        desired ~partitionable_gpu ~forced ~pool_enabled
        |> Result.map_error token
      )
  in
  (name, `Quick, test)

(* Each row: a read of the force-off file, then the setting's value in the
   file and whether the read was reported as unexpected. *)
let forced_of_file (name, read, expected, reported) =
  let test () =
    let unexpected = ref false in
    let forced =
      Gpu.Host_partitioning.forced_of_file
        ~unexpected:(fun _ -> unexpected := true)
        read
    in
    Alcotest.(check (pair string bool))
      name (expected, reported)
      (Gpu.Host_partitioning.forced_token forced, !unexpected)
  in
  (name, `Quick, test)

let not_run () = Alcotest.fail "a check ran after an earlier one failed"

let tests =
  List.map gpu_partitioning_desired
    [
      ( "desired: no partitionable GPU"
      , Fun.const false
      , not_run
      , not_run
      , Error "no_partitionable_gpu"
      )
    ; ( "desired: a partitionable GPU, forced off on the host"
      , Fun.const true
      , Fun.const Gpu.Host_partitioning.Forced
      , not_run
      , Error "forced_off"
      )
    ; ( "desired: a partitionable GPU, the pool switch off"
      , Fun.const true
      , Fun.const Gpu.Host_partitioning.Not_forced
      , Fun.const false
      , Error "pool_disabled"
      )
    ; ( "desired: a partitionable GPU, the pool switch on"
      , Fun.const true
      , Fun.const Gpu.Host_partitioning.Not_forced
      , Fun.const true
      , Ok ()
      )
    ]

let file_tests =
  List.map forced_of_file
    [
      ("force-off file: absent, not forced", Ok None, "not_forced", false)
    ; ( "force-off file: the not-forced value"
      , Ok (Some "not_forced")
      , "not_forced"
      , false
      )
    ; ( "force-off file: the forced-off value"
      , Ok (Some "forced_off")
      , "forced_off"
      , false
      )
    ; ( "force-off file: another value, forced off and reported"
      , Ok (Some "no")
      , "forced_off"
      , true
      )
    ; ( "force-off file: empty, forced off and reported"
      , Ok (Some "")
      , "forced_off"
      , true
      )
    ; ( "force-off file: the not-forced value and a newline, forced off and \
         reported"
      , Ok (Some "not_forced\n")
      , "forced_off"
      , true
      )
    ; ( "force-off file: unreadable, forced off and reported"
      , Error "not a regular file"
      , "forced_off"
      , true
      )
    ]

let () =
  Alcotest.run "host_partitioning"
    [("desired", tests); ("forced_of_file", file_tests)]
