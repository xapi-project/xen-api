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

(* The census counts a holder whose descriptors it cannot read (the product runs
   as root), so tests use a file system where other users hold no flock *)
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

(* what the creator does: a new or the current lock object, attested *)
let attest lock =
  ignore (L.prepare lock : (unit, string) result) ;
  let attest h = L.attest_if_alone lock h once in
  match L.fresh lock "k" with
  | Ok h ->
      with_held h attest
  | Error _ ->
      Result.(join (map_error (fun _ -> "refused") (holding lock attest)))

let check = Alcotest.(check bool)

(* holds the lock file as another holder would: flock(2) locks on two open
   files conflict also within one process *)
let hold path =
  let fd = Unix.openfile path [O_RDONLY; O_CLOEXEC] 0 in
  assert (Unixext.flock_try fd ~exclusive:true) ;
  fd

let inode path = (Unix.stat path).st_ino

let mark path = Unixext.string_of_file path

let has affix path = Astring.String.is_infix ~affix (mark path)

(* rewrites the lock file in place: its inode stays *)
let rewrite path text =
  Out_channel.with_open_gen [Open_wronly; Open_trunc] 0 path (fun oc ->
      output_string oc text
  )

let holds lock =
  Result.map (List.map (fun h -> (h.pid, h.hold))) (L.holders lock "k" once)

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
  refused_with (fun p -> Unix.chmod (Filename.dirname p) 0o755) ;
  let path, lock = world () in
  Unixext.write_string_to_file ~perms:0o600 (Filename.dirname path) "" ;
  check "no census of a non-directory" true (Result.is_error (holds lock))

let copy_adopted_in_place () =
  let path, lock = world () in
  check "created" true (attest lock = Ok ()) ;
  check "attested" true (state lock = Ok Attested) ;
  Unixext.write_string_to_file ~perms:0o600 path (mark path) ;
  check "a copy" true (state lock = Ok Unattested) ;
  check "adopted" true (attest lock = Ok ()) ;
  check "marked anew" true (has (Printf.sprintf "ino=%d " (inode path)) path)

let other_boot () =
  let path, lock = world () in
  check "created" true (attest lock = Ok ()) ;
  let ino = inode path in
  let dir = Filename.dirname path in
  let later = L.create ~boot_id:(fun () -> "later") ~magic dir in
  check "not attested" true (state later = Ok Unattested) ;
  check "attested" true (attest later = Ok () && inode path = ino) ;
  check "boot" true (has "boot=later" path)

let renamed_holder_counted () =
  let path, lock = world () in
  check "created" true (attest lock = Ok ()) ;
  let old = hold path in
  Unix.rename path (path ^ ".old") ;
  check "renamed lock held" true (Result.is_error (attest lock)) ;
  let comm = String.trim (Unixext.string_of_file "/proc/self/comm") in
  let named h = h.pid = Unix.getpid () && h.command = comm in
  check "named" true
    (Result.map (List.exists named) (L.holders lock "k" once) = Ok true) ;
  Unix.close old ;
  check "attested" true (attest lock = Ok ())

let creator_counted () =
  let _, lock = world () in
  check "prepared" true (L.prepare lock = Ok ()) ;
  let created = Result.get_ok (L.fresh lock "k") in
  check "held by the creator" true (take lock = Error `Busy) ;
  check "the census names the creator" true
    (holds lock = Ok [(Unix.getpid (), Write)]) ;
  let copies = List.map Unix.dup (L.fds_for_child created) in
  check "once per process" true (Result.map List.length (holds lock) = Ok 1) ;
  let other = Result.get_ok (L.fresh lock "x") in
  check "not for another key" true (Result.map List.length (holds lock) = Ok 1) ;
  L.release other ;
  List.iter Unix.close copies ;
  L.release created

let shared_mark_ignored () =
  let path, lock = world () in
  check "created" true (attest lock = Ok ()) ;
  let forged = Filename.(concat (dirname (dirname path)) "forged") in
  Unixext.write_string_to_file ~perms:0o620 forged (mark path) ;
  let fd = hold forged in
  check "not counted" true (attest lock = Ok ()) ;
  Unix.close fd

let links_and_versions () =
  let path, lock = world () in
  check "created" true (attest lock = Ok ()) ;
  Unix.link path (path ^ ".second") ;
  check "two links" true (state lock = Ok Unattested) ;
  Unix.unlink (path ^ ".second") ;
  rewrite path (String.trim (mark path) ^ " id=k\n") ;
  check "a name twice" true (state lock = Ok Unattested) ;
  let other = magic ^ " v=2.0 id=k\n" in
  rewrite path other ;
  check "other version" true (Result.is_error (attest lock)) ;
  check "unchanged" true (mark path = other)

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
  check "the census names the child" true (holds lock = Ok [(child, Write)]) ;
  Unix.kill child Sys.sigkill ;
  ignore (Unix.waitpid [] child : int * Unix.process_status) ;
  check "free when it ends" true (take lock = Ok 1)

let child_outlives_taker () =
  let path, lock = world () in
  check "created" true (attest lock = Ok ()) ;
  let r, w = Unix.pipe () in
  let gone, ends = Unix.pipe () in
  ( match Unix.fork () with
  | 0 ->
      ignore (L.acquire lock "k" Exclusive once : (_, acquire_error) result) ;
      if Unix.fork () = 0 then (
        Unix.close ends ;
        ignore (Unix.write_substring w "x" 0 1 : int) ;
        ignore (Unix.read gone (Bytes.create 1) 0 1 : int)
      ) ;
      Unix._exit 0
  | taker ->
      Unix.close w ;
      ignore (Unix.read r (Bytes.create 1) 0 1 : int) ;
      ignore (Unix.waitpid [] taker : int * Unix.process_status)
  ) ;
  Unix.unlink path ;
  check "older lock held by the child" true (Result.is_error (attest lock)) ;
  Unix.close ends

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
          ; ("a copy is not attested; adopted in place", copy_adopted_in_place)
          ; ("a mark of another boot: marked anew in place", other_boot)
          ; ("a renamed holder is counted", renamed_holder_counted)
          ; ("the creator is counted", creator_counted)
          ; ("a mark others may write counts for no key", shared_mark_ignored)
          ; ("links and versions", links_and_versions)
          ; ("a child keeps the lock", child_keeps_lock)
          ; ("a child that outlives its taker keeps it", child_outlives_taker)
          ; ("shared holds coexist, not with an exclusive", shared_holds)
          ]
      )
    ]
