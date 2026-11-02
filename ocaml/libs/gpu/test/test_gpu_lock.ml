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
module Unixext = Xapi_stdext_unix.Unixext
module L = Gpu_lock

let ms n = Mtime.Span.(n * ms)

let card = Xcp_pci.address_of_string "0000:3b:00.0"

(* The census counts a holder whose descriptors it cannot read (the product runs
   as root), so tests use a file system where other users hold no flock; two
   runs at once on one host may refuse each other, as the magic is fixed *)
let base = List.find Sys.file_exists ["/dev/shm"; Filename.get_temp_dir_name ()]

let world ?(engaged = true) () =
  let dir =
    Filename.concat base
      (Printf.sprintf "gpu-lock-%d-%d" (Unix.getpid ()) (Random.bits ()))
  in
  Unix.mkdir dir 0o700 ;
  at_exit (fun () -> Unixext.rm_rec dir) ;
  let dir = Unix.realpath dir ^ "/lock" in
  (dir ^ "/host.lock", L.create ~dir ~engaged:(fun () -> engaged) ())

let run ?(timeout = ms 300) lock =
  let count ~keep_fds ~bounds:_ = List.length keep_fds in
  try Ok (L.with_lock lock (Vf_enable card) ~timeout count)
  with L.Refused r -> Error r

let bounds_seen lock =
  let hangs = Option.iter (fun _ -> raise Forkhelpers.Subprocess_timeout) in
  let body ~keep_fds ~bounds =
    let report =
      try
        L.sriov_manage bounds ~enable:false card hangs ;
        "ended"
      with e -> Printexc.to_string e
    in
    (List.length keep_fds, L.timeout bounds (ms 1), report)
  in
  L.with_lock lock (Unbind card) ~timeout:(ms 300) body

let check = Alcotest.(check bool)

let refused ~busy msg result =
  check msg true (Result.map_error L.transient result = Error busy)

let says affix = function
  | Ok _ ->
      false
  | Error r ->
      Astring.String.is_infix ~affix (Printexc.to_string (L.Refused r))

(* holds the lock file as another holder would: flock(2) locks on two open
   files conflict also within one process *)
let hold path =
  let fd = Unix.openfile path [O_RDONLY; O_CLOEXEC] 0 in
  assert (Unixext.flock_try fd ~exclusive:true) ;
  fd

let inode path = (Unix.stat path).st_ino

let not_engaged () =
  let path, lock = world ~engaged:false () in
  check "ran unlocked, unbounded" true (bounds_seen lock = (0, None, "ended")) ;
  check "nothing made" false (Sys.file_exists (Filename.dirname path))

let bounds_where_engaged () =
  let _, lock = world () in
  let killed =
    "sriov-manage -d 0000:3b:00.0 did not end within 4min and was killed: the \
     card may be left without its driver or with its driver_override set, and \
     the GPU monitoring services stopped"
  in
  check "bounded" true (bounds_seen lock = (1, Some (ms 1), killed))

let busy_then_free () =
  let path, lock = world () in
  Unix.mkdir (Filename.dirname path) 0o700 ;
  Unixext.write_string_to_file ~perms:0o600 path "" ;
  check "ran" true (run lock = Ok 1) ;
  check "marked" true
    (Astring.String.is_prefix ~affix:"xapi-gpu-lock v=1.0 id=host "
       (Unixext.string_of_file path)
    ) ;
  let other = hold path in
  refused ~busy:true "busy" (run lock) ;
  Unix.close other ;
  check "free, one descriptor" true (run lock = Ok 1)

let no_mark_while_old_held () =
  let path, lock = world () in
  check "ran" true (run lock = Ok 1) ;
  let old = hold path in
  Unix.unlink path ;
  let refusal = run lock in
  let unattested = inode path in
  Unix.close old ;
  refused ~busy:false "older lock held" refusal ;
  check "says why" true (says "1 hold(s) on an older lock file" refusal) ;
  check "repaired" true (run lock = Ok 1) ;
  check "adopted in place" true (inode path = unattested)

let refusal_counts_holders () =
  let path, lock = world () in
  check "ran" true (run lock = Ok 1) ;
  let other = hold path in
  let refusal = run ~timeout:(ms 0) lock in
  Unix.close other ;
  check "counted" true (says "; holders: 1 " refusal)

let () =
  Alcotest.run "gpu_lock"
    [
      ( "gpu_lock"
      , List.map
          (fun (name, f) -> Alcotest.test_case name `Quick f)
          [
            ("not engaged: no lock, no directory, no bounds", not_engaged)
          ; ("engaged: bounded work", bounds_where_engaged)
          ; ("busy until the wait ends, then free", busy_then_free)
          ; ("older lock held: refused, then adopted", no_mark_while_old_held)
          ; ("a refusal counts holders, names none", refusal_counts_holders)
          ]
      )
    ]
