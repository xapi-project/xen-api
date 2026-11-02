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

(* Unprivileged tests; what the types guarantee is not tested *)
open Xapi_stdext_unix
open Lock_rw
module L = Flock

let once =
  {stop= (fun () -> false); wait= (fun ~retry:_ ~blockers:_ -> Error `Busy)}

let base = List.find Sys.file_exists ["/dev/shm"; Filename.get_temp_dir_name ()]

(* two runs at once never take each other's lock files for theirs *)
let magic = Printf.sprintf "test-%d" (Unix.getpid ())

let world () =
  let dir =
    Filename.concat base
      (Printf.sprintf "flock-%d-%d" (Unix.getpid ()) (Random.bits ()))
  in
  Unix.mkdir dir 0o700 ;
  at_exit (fun () -> Unixext.rm_rec dir) ;
  let dir = Unix.realpath dir ^ "/lock" in
  (dir ^ "/k.lock", L.create ~boot_id:(fun () -> "boot") ~magic dir)

let plain path =
  Unix.mkdir (Filename.dirname path) 0o700 ;
  Unixext.write_string_to_file ~perms:0o600 path ""

let with_held h f = Fun.protect ~finally:(fun () -> L.release h) (fun () -> f h)

let holding lock f =
  Result.map (fun h -> with_held h f) (L.acquire lock "k" Exclusive once)

let take lock = holding lock (fun h -> List.length (L.fds_for_child h))

let state lock = holding lock L.state

let check = Alcotest.(check bool)

let unsafe_objects_refused () =
  let refused_with make =
    let path, lock = world () in
    check "absent" true (take lock = Error `Absent) ;
    plain (path ^ ".target") ;
    make path ;
    check "refused" true
      (match take lock with Error (`Failed _) -> true | _ -> false)
  in
  refused_with (fun p -> Unix.symlink (p ^ ".target") p) ;
  refused_with (fun p -> Unixext.write_string_to_file ~perms:0o622 p "") ;
  refused_with (fun p -> Unix.mkfifo p 0o600) ;
  refused_with (fun p -> Unix.chmod (Filename.dirname p) 0o755)

let child_keeps_lock () =
  let path, lock = world () in
  plain path ;
  check "not attested" true (state lock = Ok Unattested) ;
  let h = Result.get_ok (L.acquire lock "k" Exclusive once) in
  let child =
    with_held h @@ fun h ->
    List.iter Unix.clear_close_on_exec (L.fds_for_child h) ;
    Unix.create_process "sleep" [|"sleep"; "60"|] Unix.stdin Unix.stdout
      Unix.stderr
  in
  check "held by the child" true (take lock = Error `Busy) ;
  Unix.kill child Sys.sigkill ;
  ignore (Unix.waitpid [] child : int * Unix.process_status) ;
  check "free when it ends" true (take lock = Ok 1)

let shared_holds () =
  let path, lock = world () in
  plain path ;
  let shared () = L.acquire lock "k" Shared once in
  let first = Result.get_ok (shared ()) in
  check "shared twice" true (Result.map L.release (shared ()) = Ok ()) ;
  check "not exclusive" true (take lock = Error `Busy) ;
  L.release first

let () =
  Alcotest.run "flock"
    [
      ( "flock"
      , List.map
          (fun (name, f) -> Alcotest.test_case name `Quick f)
          [
            ("unsafe objects are refused", unsafe_objects_refused)
          ; ("a child keeps the lock", child_keeps_lock)
          ; ("shared holds coexist, not with an exclusive", shared_holds)
          ]
      )
    ]
