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

open Alcotest.V1

let in_thread f =
  let res = ref (Error (`Msg "Thread not finished")) in
  let t = Thread.create (fun () -> res := Rresult.R.trap_exn f ()) () in
  Thread.join t ;
  match !res with
  | Ok r ->
      r
  | Error (`Msg m) ->
      failwith m
  | Error (`Exn_trap (e, bt)) ->
      Printexc.raise_with_backtrace e bt

let check_asserts ~pos f arg =
  try
    let () = f arg in
    fail ~pos "Use after free not detected"
  with Assert_failure _ -> (* Expected result *)
                           ()

let with_pam f =
  let h = Pam.authorize_start () in
  Fun.protect ~finally:(fun () -> Pam.authorize_stop h) (fun () -> f h)

let pam_start_stop () =
  let h = Pam.authorize_start () in
  Pam.authorize_stop h

let pam_start_stop_stop () =
  let h = Pam.authorize_start () in
  Pam.authorize_stop h ;
  check_asserts ~pos:__POS__ Pam.authorize_stop h

let pam_start_stop_run () =
  let h = Pam.authorize_start () in
  Pam.authorize_stop h ;
  check_asserts ~pos:__POS__ (fun () -> Pam.authorize_run h "" "") ()

let pam_startN_stopN () =
  (* start and stop in opposite order: start1, start2, ... stop1, stop2...
     instead of start1, start2, stop2, stop1
  *)
  let handles = Array.init 100 (fun _ -> Pam.authorize_start ()) in
  Array.iter Pam.authorize_stop handles

let pam_thread_owner1 () =
  with_pam @@ fun h ->
  in_thread @@ fun () ->
  check_asserts ~pos:__POS__ (fun () -> Pam.authorize_run h "" "") ()

let pam_thread_owner2 () =
  with_pam @@ fun h ->
  in_thread @@ fun () -> check_asserts ~pos:__POS__ Pam.authorize_stop h

let pam_badauth () =
  with_pam @@ fun h ->
  try
    let () = Pam.authorize_run h "" "" in
    fail ~pos:__POS__ "Expected PAM to fail on empty user/password"
  with Failure _ -> (* Expected failure, OK *)
                    ()

let tests =
  [
    ( "PAM"
    , [
        ("start/stop", `Quick, pam_start_stop)
      ; ("start/stop/stop", `Quick, pam_start_stop_stop)
      ; ("start/stop/run", `Quick, pam_start_stop_run)
      ; ("startN/stopN", `Quick, pam_startN_stopN)
      ; ("thread ownership (run)", `Quick, pam_thread_owner1)
      ; ("thread ownership (stop)", `Quick, pam_thread_owner2)
      ; ("badauth", `Slow, pam_badauth)
      ]
    )
  ]
