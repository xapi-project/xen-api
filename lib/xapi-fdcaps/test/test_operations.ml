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
open Properties
open Operations
open Syntax

let b = Bytes.make 256 'x'

let read_fd fd =
  let (_ : int) = read fd b 0 (Bytes.length b) in
  ()

let check_unsafe_raises ?(exn = Unix.EBADF) name t op =
  (* if we bypass the type safety then we should get an error at runtime,
     but only when the capability is 'no', not when it is 'removed'
  *)
  let fd = For_test.unsafe_fd_exn t in
  let msg = Printf.sprintf "%s when <%s: no; ..>" name name in
  let exn = Unix.Unix_error (exn, name, "") in
  Alcotest.check_raises msg exn @@ fun () -> op fd

let error_read_fd (t : ([< wronly], _) make) =
  let@ fd = check_unsafe_raises "read" t in
  let (_ : int) = Unix.read fd b 0 (Bytes.length b) in
  ()

let str = "test"

let write_fd fd =
  let (_ : int) = single_write_substring fd str 0 (String.length str) in
  ()

let error_write_fd (t : ([< rdonly], _) make) =
  let@ fd = check_unsafe_raises "single_write" t in
  let (_ : int) = Unix.single_write_substring fd str 0 (String.length str) in
  ()

let test_ro fd = read_fd fd ; error_write_fd fd

let test_wo fd = write_fd fd ; error_read_fd fd

let test_lseek t =
  let actual = lseek t 0L Unix.SEEK_SET in
  Alcotest.(check' int64) ~msg:"starting position" ~expected:0L ~actual ;
  let expected = 17L in
  let actual = lseek t expected Unix.SEEK_SET in
  Alcotest.(check' int64) ~msg:"jump1 position" ~expected ~actual ;
  let actual = lseek t 3L Unix.SEEK_CUR in
  Alcotest.(check' int64) ~msg:"jump2 position" ~expected:20L ~actual

let error_lseek (t : (_, [< espipe]) make) =
  let@ fd = check_unsafe_raises ~exn:Unix.ESPIPE "lseek" t in
  let (_ : int) = Unix.lseek fd 0 Unix.SEEK_CUR in
  ()

let test_ftruncate t =
  let expected = 4000L in
  ftruncate t expected ;
  let actual = lseek t 0L Unix.SEEK_END in
  Alcotest.(check' int64) ~msg:"size after ftruncate" ~expected ~actual

type not_truncate = [blk | chr | dir | lnk | fifo | sock]

let error_ftruncate (t : (_, [< not_truncate]) make) =
  let@ fd = check_unsafe_raises ~exn:Unix.EINVAL "ftruncate" t in
  Unix.LargeFile.ftruncate fd 4000L

type not_sock = [reg | blk | chr | dir | lnk | fifo]

let error_shutdown (t : (_, [< not_sock]) make) =
  let@ fd = check_unsafe_raises ~exn:Unix.ENOTSOCK "shutdown" t in
  Unix.shutdown fd Unix.SHUTDOWN_RECEIVE

let test_fd2 make ops =
  ops
  |> List.map @@ fun (name, op1, op2) ->
     let test () =
       let@ fd1, fd2 = with_fd2 @@ make () in
       pp Fmt.stdout fd1 ;
       dump Fmt.stdout fd1 ;
       (* the 2 operations may depend on each-other, e.g. write and read on a pipe, so must be part of same testcase *)
       set_nonblock fd1 ;
       set_nonblock fd2 ;
       op2 fd2 ;
       op1 fd1 ;
       clear_nonblock fd1 ;
       clear_nonblock fd2
     in
     Alcotest.(test_case name `Quick) test

let test_fd with_make ops =
  ops
  |> List.map @@ fun (name, op) ->
     let test () =
       let@ fd = with_make () in
       op fd
     in
     Alcotest.(test_case name `Quick) test

let test_pipe =
  test_fd2 pipe
    [
      ("wo,ro", test_ro, test_wo)
    ; ("error_lseek", error_lseek, error_lseek)
    ; ("error_ftruncate", error_ftruncate, error_ftruncate)
    ; ("error_shutdown", error_shutdown, error_shutdown)
    ]

let test_sock =
  let make () = socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  test_fd2 make
    [
      ("read,write", read_fd, write_fd)
    ; ("error_lseek", error_lseek, error_lseek)
    ; ("error_ftruncate", error_ftruncate, error_ftruncate)
    ]

let with_fd fd f = pp Fmt.stdout fd ; dump Fmt.stdout fd ; with_fd fd f

let with_tempfile () f =
  let@ name, fd = with_tempfile () in
  Fmt.pf Fmt.stdout "%s: %a@." name pp fd ;
  f (name, fd)

let test_single make f () =
  let@ t = with_fd @@ make () in
  error_shutdown t ; f t

let test_safe_close () =
  let@ t = with_fd @@ dev_null_in () in
  close t ; close t

let test_regular =
  let with_make () f =
    let@ _name, out = with_tempfile () in
    f out
  in
  test_fd with_make
    [
      ("wo", test_wo)
    ; ("lseek", test_lseek)
    ; ("ftruncate", test_ftruncate)
    ; ("error_shutdown", error_shutdown)
    ]

let test_sock_shutdown_r () =
  let@ fd1, fd2 = with_fd2 @@ socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  shutdown_recv fd1 ;
  let exn = Unix.Unix_error (Unix.EPIPE, "single_write", "") in
  let@ () = Alcotest.check_raises "write after shutdown of other end" exn in
  write_fd fd2

let test_sock_shutdown_w () =
  let@ _fd1, fd2 = with_fd2 @@ socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  write_fd fd2 ;
  shutdown_send fd2 ;
  let exn = Unix.Unix_error (Unix.EPIPE, "single_write", "") in
  let@ () = Alcotest.check_raises "write after shutdown" exn in
  write_fd fd2

let test_sock_shutdown_all () =
  let@ fd1, fd2 = with_fd2 @@ socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  write_fd fd2 ;
  shutdown_all fd2 ;
  let exn = Unix.Unix_error (Unix.EPIPE, "single_write", "") in
  let () =
    let@ () = Alcotest.check_raises "write after shutdown" exn in
    write_fd fd2
  in
  let@ () = Alcotest.check_raises "write after shutdown" exn in
  write_fd fd1

let test_block sector_size =
  let with_make () f =
    let@ name, fd = with_tempfile () in
    ftruncate fd 8192L ;
    let run () =
      try
        let@ _blkname, fd = with_temp_blk ~sector_size name in
        f fd
      with Failure _ ->
        let bt = Printexc.get_raw_backtrace () in
        Printexc.raise_with_backtrace (Failure "with_temp_blk") bt
    in
    if Unix.geteuid () = 0 then
      run ()
    else
      Alcotest.check_raises "non-root fails to create blockdevice"
        (Failure "with_temp_blk") run
  in
  test_fd with_make
    [("read", read_fd); ("write", write_fd); ("lseek", test_lseek)]

let test_block_nest =
  let with_make () f =
    if Unix.geteuid () <> 0 then
      Alcotest.skip () ;
    let@ name, fd = with_tempfile () in
    ftruncate fd 8192L ;
    let@ blkname, _fd = with_temp_blk ~sector_size:4096 name in
    let@ _blkname, fd = with_temp_blk ~sector_size:512 blkname in
    f fd
  in
  test_fd with_make
    [("read", read_fd); ("write", write_fd); ("lseek", test_lseek)]

let test_creat () =
  let name = Filename.temp_file __MODULE__ (Unix.getpid () |> string_of_int) in
  Unix.unlink name ;
  let@ fd1 = with_fd @@ creat name [] 0o600 in
  pp Fmt.stdout fd1 ;
  read_fd fd1 ;
  write_fd fd1 ;
  let@ fd2 = with_fd @@ openfile_rw `reg name [] in
  pp Fmt.stdout fd2 ; read_fd fd2 ; write_fd fd2

let tests =
  Alcotest.
    [
      test_case "/dev/null in" `Quick @@ test_single dev_null_in test_ro
    ; test_case "/dev/null out" `Quick @@ test_single dev_null_out test_wo
    ; test_case "/dev/zero" `Quick @@ test_single dev_zero test_ro
    ; test_case "safe close" `Quick test_safe_close
    ; test_case "socket shutdown read" `Quick test_sock_shutdown_r
    ; test_case "socket shutdown write" `Quick test_sock_shutdown_w
    ; test_case "socket shutdown both" `Quick test_sock_shutdown_all
    ; test_case "create" `Quick test_creat
    ]

(* this must be the last test *)
let test_no_leaks () =
  Gc.full_major () ;
  Alcotest.(check' int)
    ~msg:"Check for no FD leaks" ~expected:0 ~actual:(Safefd.leaked ())

let () =
  setup () ;
  Sys.enable_runtime_warnings true ;
  Alcotest.run ~show_errors:true "xapi_fdcaps"
    [
      ("pipe", test_pipe)
    ; ("socket", test_sock)
    ; ("regular", test_regular)
    ; ("block 512", test_block 512)
    ; ("block 4k", test_block 4096)
    ; ("block 512 on 4k", test_block_nest)
    ; ("xapi_fdcaps", tests)
    ; ("no fd leaks", [Alcotest.test_case "no leaks" `Quick test_no_leaks])
    ]
