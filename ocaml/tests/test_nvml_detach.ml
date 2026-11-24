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

(* The ordering and failure paths of Nvml_detach.Make.with_detached, against
   a fake gpumon that records every call. *)

exception Gpumon_down

exception F_failed

(* What the fake gpumon does on each call. [attached] is its NVML state;
   [restart] is what sriov-manage does to gpumon: it comes back attached. *)
module Fake = struct
  let calls : string list ref = ref []

  let attached = ref true

  let answers = ref true

  let detach_fails = ref false

  let detach_fails_after = ref false

  let attach_fails = ref false

  let reset () =
    calls := [] ;
    attached := true ;
    answers := true ;
    detach_fails := false ;
    detach_fails_after := false ;
    attach_fails := false

  let record s = calls := s :: !calls

  let is_attached () =
    record "is_attached" ;
    if not !answers then raise Gpumon_down ;
    !attached

  let detach () =
    record "detach" ;
    if !detach_fails then raise Gpumon_down ;
    attached := false ;
    if !detach_fails_after then raise Gpumon_down

  let attach () =
    record "attach" ;
    if !attach_fails then raise Gpumon_down ;
    attached := true

  let restart () =
    record "restart" ;
    attached := true
end

module Detach = Nvml_detach.Make (Fake)

let run f = Detach.with_detached f

let f_ok () = Fake.record "f" ; 42

let check_calls expected =
  Alcotest.(check (list string))
    "calls, in order" expected (List.rev !Fake.calls)

let check_attached expected =
  Alcotest.(check bool) "NVML attached afterwards" expected !Fake.attached

(* The plain case: detach, f, ask again, attach; f's result comes back. *)
let test_detach_f_attach () =
  Fake.reset () ;
  Alcotest.(check int) "result" 42 (run f_ok) ;
  check_calls ["is_attached"; "detach"; "f"; "is_attached"; "attach"] ;
  check_attached true

(* f sees NVML detached while it runs. *)
let test_detached_during_f () =
  Fake.reset () ;
  let during = run (fun () -> !Fake.attached) in
  Alcotest.(check bool) "attached during f" false during

(* Already detached (by someone else): leave it alone. *)
let test_already_detached () =
  Fake.reset () ;
  Fake.attached := false ;
  Alcotest.(check int) "result" 42 (run f_ok) ;
  check_calls ["is_attached"; "f"] ;
  check_attached false

(* f raises: NVML is attached again and f's exception reaches the caller. *)
let test_f_raises () =
  Fake.reset () ;
  Alcotest.check_raises "f's exception" F_failed (fun () ->
      run (fun () -> Fake.record "f" ; raise F_failed)
  ) ;
  check_calls ["is_attached"; "detach"; "f"; "is_attached"; "attach"] ;
  check_attached true

(* gpumon restarted while f ran (sriov-manage): it is attached already, so
   it is not attached a second time. *)
let test_restarted_during_f () =
  Fake.reset () ;
  let f () = Fake.record "f" ; Fake.restart () ; 42 in
  Alcotest.(check int) "result" 42 (run f) ;
  check_calls ["is_attached"; "detach"; "f"; "restart"; "is_attached"] ;
  check_attached true

(* gpumon is down when we start: nothing holds the card, so f just runs. *)
let test_down_before () =
  Fake.reset () ;
  Fake.answers := false ;
  Alcotest.(check int) "result" 42 (run f_ok) ;
  check_calls ["is_attached"; "f"]

(* gpumon stops answering while f runs (stopped, mid-restart): no attach is
   attempted, and f's result comes back. *)
let test_down_after () =
  Fake.reset () ;
  let f () =
    Fake.record "f" ;
    Fake.answers := false ;
    42
  in
  Alcotest.(check int) "result" 42 (run f) ;
  check_calls ["is_attached"; "detach"; "f"; "is_attached"]

(* The attach fails: f's result is not masked. *)
let test_attach_fails () =
  Fake.reset () ;
  Fake.attach_fails := true ;
  Alcotest.(check int) "result" 42 (run f_ok) ;
  check_calls ["is_attached"; "detach"; "f"; "is_attached"; "attach"] ;
  check_attached false

(* f raises and the attach fails: f's exception, not Fun.Finally_raised. *)
let test_f_raises_attach_fails () =
  Fake.reset () ;
  Fake.attach_fails := true ;
  Alcotest.check_raises "f's exception" F_failed (fun () ->
      run (fun () -> raise F_failed)
  ) ;
  check_calls ["is_attached"; "detach"; "is_attached"; "attach"] ;
  check_attached false

(* The detach fails: a warning, f runs anyway, and NVML (still attached) is
   not attached a second time. *)
let test_detach_fails () =
  Fake.reset () ;
  Fake.detach_fails := true ;
  Alcotest.(check int) "result" 42 (run f_ok) ;
  check_calls ["is_attached"; "detach"; "f"; "is_attached"] ;
  check_attached true

(* The detach raises after it has detached: f runs, and NVML is attached
   again afterwards. *)
let test_detach_fails_after_detaching () =
  Fake.reset () ;
  Fake.detach_fails_after := true ;
  Alcotest.(check int) "result" 42 (run f_ok) ;
  check_calls ["is_attached"; "detach"; "f"; "is_attached"; "attach"] ;
  check_attached true

(* Two calls in a row each detach and attach once. *)
let test_twice () =
  Fake.reset () ;
  ignore (run f_ok) ;
  ignore (run f_ok) ;
  let count s = List.length (List.filter (( = ) s) !Fake.calls) in
  Alcotest.(check int) "detaches" 2 (count "detach") ;
  Alcotest.(check int) "attaches" 2 (count "attach") ;
  check_attached true

(* Which path a start takes: the detach only for a VM without pass-through,
   and only with the switch on. *)
let test_how () =
  let check name expected ~detach_switch ~pass_through =
    Alcotest.(check bool)
      name true
      (Nvml_detach.how ~detach_switch ~pass_through = expected)
  in
  check "switch off, vGPU" Stop_gpumon ~detach_switch:false ~pass_through:false ;
  check "switch off, pass-through" Stop_gpumon ~detach_switch:false
    ~pass_through:true ;
  check "switch on, vGPU" Detach_nvml ~detach_switch:true ~pass_through:false ;
  check "switch on, pass-through" Stop_gpumon ~detach_switch:true
    ~pass_through:true

(* A VM counts as pass-through if any of its VGPUs needs a whole GPU: an
   SR-IOV vGPU (a virtual function) does not. *)
let test_has_pass_through () =
  let __context = Test_common.make_test_database () in
  let pt =
    Test_common.make_vgpu_type ~__context ~implementation:`passthrough ()
  in
  let nv = Test_common.make_vgpu_type ~__context ~implementation:`nvidia () in
  let sriov =
    Test_common.make_vgpu_type ~__context ~implementation:`nvidia_sriov ()
  in
  let vgpu _type = Test_common.make_vgpu ~__context ~_type () in
  let check name expected vgpus =
    Alcotest.(check bool)
      name expected
      (Xapi_gpumon.has_pass_through ~__context ~vgpus)
  in
  check "no VGPU" false [] ;
  check "one vGPU" false [vgpu nv] ;
  check "one SR-IOV vGPU" false [vgpu sriov] ;
  check "one pass-through" true [vgpu pt] ;
  check "vGPU and pass-through" true [vgpu nv; vgpu pt]

let test =
  [
    ("detach, f, attach", `Quick, test_detach_f_attach)
  ; ("detached while f runs", `Quick, test_detached_during_f)
  ; ("already detached", `Quick, test_already_detached)
  ; ("f raises", `Quick, test_f_raises)
  ; ("gpumon restarted during f", `Quick, test_restarted_during_f)
  ; ("gpumon down before f", `Quick, test_down_before)
  ; ("gpumon down after f", `Quick, test_down_after)
  ; ("attach fails", `Quick, test_attach_fails)
  ; ("f raises and attach fails", `Quick, test_f_raises_attach_fails)
  ; ("detach fails", `Quick, test_detach_fails)
  ; ("detach fails after detaching", `Quick, test_detach_fails_after_detaching)
  ; ("two calls", `Quick, test_twice)
  ; ("which path a start takes", `Quick, test_how)
  ; ("pass-through VMs", `Quick, test_has_pass_through)
  ]

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Nvml_detach suite" [("Test_nvml_detach", test)]
