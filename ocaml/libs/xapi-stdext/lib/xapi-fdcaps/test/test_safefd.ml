(*
 * Copyright (C) 2023 Cloud Software Group
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

open Xapi_fdcaps
open Safefd

let make_safefd () =
  let rd, wr = Unix.pipe ~cloexec:true () in
  (of_file_descr rd, of_file_descr wr)

let test_safefd_regular () =
  let rd, wr = Unix.pipe ~cloexec:true () in
  let rd, wr = (of_file_descr rd, of_file_descr wr) in
  let (_ : Unix.LargeFile.stats) = with_fd_exn rd Unix.LargeFile.fstat
  and (_ : Unix.LargeFile.stats) = with_fd_exn wr Unix.LargeFile.fstat in
  close_exn rd ; close_exn wr

let test_safefd_double_close () =
  let rd, wr = make_safefd () in
  close_exn rd ;
  close_exn wr ;
  let exn = Unix.(Unix_error (EBADF, "close_exn", "")) in
  Alcotest.check_raises "double close" exn (fun () -> close_exn wr)

let test_safefd_idempotent_close () =
  let rd, wr = make_safefd () in
  close_exn rd ;
  idempotent_close_exn wr ;
  idempotent_close_exn wr ;
  idempotent_close_exn wr ;
  idempotent_close_exn wr

let test_safefd_unix_close () =
  let rd, wr = make_safefd () in
  close_exn rd ;
  let exn = Unix.(Unix_error (EBADF, "fstat", "")) in
  Alcotest.check_raises "Unix.close detected" exn (fun () ->
      with_fd_exn wr Unix.close
  )

let remove_unix_error_arg f =
  try f ()
  with Unix.Unix_error (code, fn, _) ->
    (* remove arg, so we can match with [Alcotest.check_raises] *)
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (Unix.Unix_error (code, fn, "")) bt

let with_fd_exn f arg = remove_unix_error_arg (fun () -> with_fd_exn f arg)

let close_reuse fd =
  Unix.close fd ;
  (* open and leak fd, this should reuse the FD number of [fd], but we should be able to detect via stat *)
  let _, _ = Unix.pipe () in
  ()

let test_safefd_unix_close_reuse () =
  let rd, wr = make_safefd () in
  close_exn rd ;
  let exn = Unix.(Unix_error (EBADF, "with_fd_exn", "")) in
  Alcotest.check_raises "Unix.close detected" exn (fun () ->
      with_fd_exn wr close_reuse
  )

let leak () =
  let rd, wr = make_safefd () in
  close_exn rd ;
  (* leak wr *)
  unsafe_to_file_descr_exn wr

let test_safefd_finalised () =
  let _leaked_fd : Unix.file_descr = leak () in
  Gc.full_major () ;
  Alcotest.(
    check' int ~msg:"leak detected" ~expected:2 ~actual:(Safefd.leaked ())
  )

let test_pp_and_dump () =
  let a, b = make_safefd () in
  Format.printf "a: %a@,b: %a@." Safefd.pp a Safefd.pp b ;
  Format.printf "a: %a@,b: %a@." Safefd.dump a Safefd.dump b

let test_nop () =
  let ebadf = Unix.(Unix_error (EBADF, "close_exn", "")) in
  Alcotest.check_raises "nop close raises" ebadf (fun () -> close_exn nop)

let test_unsafe_closed () =
  let ebadf = Unix.(Unix_error (EBADF, "unsafe_to_file_descr_exn", "")) in
  dump Fmt.stdout nop ;
  Alcotest.check_raises "unsafe raises" ebadf (fun () ->
      let (_ : Unix.file_descr) = unsafe_to_file_descr_exn nop in
      ()
  )

let tests_safefd =
  Alcotest.
    [
      test_case "nop" `Quick test_nop
    ; test_case "regular ops" `Quick test_safefd_regular
    ; test_case "double close detected" `Quick test_safefd_double_close
    ; test_case "idempotent close" `Quick test_safefd_idempotent_close
    ; test_case "Unix.close detected" `Quick test_safefd_unix_close
    ; test_case "Unix.close detected after reuse" `Quick
        test_safefd_unix_close_reuse
    ; test_case "FD leak detected" `Quick test_safefd_finalised
    ; test_case "test pp and dump" `Quick test_pp_and_dump
    ; test_case "unsafe of closed fd" `Quick test_unsafe_closed
    ]

let () =
  setup () ;
  Sys.enable_runtime_warnings true ;
  Alcotest.run ~show_errors:true "xapi_fdcaps" [("safefd", tests_safefd)]
