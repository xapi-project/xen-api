(* Copyright (C) Cloud Software Group Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module Sys = Private.Lib.Sys
module Signal = Private.Lib.Signal
module Process = Private.Lib.Process

let ( let* ) = Lwt.bind

let test_content_rountrip =
  let test () =
    let contents = "yes" in
    let path = Filename.temp_file "" "" in
    let* () = Sys.save ~contents path in
    let* result = Sys.read_file_contents path in
    Alcotest.(check string) "Write and read roundtrip" contents result ;
    Lwt.return ()
  in
  ("Write and read file", `Quick, test)

let test_readdir =
  let test () =
    let path = Filename.temp_file "" "" in
    let filename = Filename.basename path in
    let tmpdir = Filename.dirname path in
    let* dir_contents = Sys.readdir tmpdir in
    let file_present = List.exists (String.equal filename) dir_contents in
    Alcotest.(check bool) "Temp file detected" true file_present ;
    Lwt.return ()
  in
  ("Read directory", `Quick, test)

let test_assert_is_exec =
  let test name path is_expected =
    let* result = Sys.assert_is_executable path in
    Alcotest.(check bool) name true (is_expected result) ;
    Lwt.return ()
  in
  let test () =
    let path = "/missing/path" in
    let is_expected = function
      | Error (`missing p) ->
          Alcotest.(check string) "Missing paths match" path p ;
          true
      | _ ->
          false
    in
    let* () = test "File is missing" path is_expected in

    let path = Filename.temp_file "" "" in
    let is_expected = function
      | Error (`not_executable (p, _)) ->
          Alcotest.(check string) "Non-exec paths match" path p ;
          true
      | _ ->
          false
    in
    let* () = test "File is not executable" path is_expected in

    let* () = Lwt_unix.chmod path 0o700 in
    let is_expected = function Ok () -> true | _ -> false in
    let* () = test "File is now executable" path is_expected in

    Lwt.return ()
  in
  ("Executable file detection", `Quick, test)

let test_sys =
  ("Sys", [test_content_rountrip; test_readdir; test_assert_is_exec])

let exit_or_signal_pp ppf es =
  match es with
  | Process.Output.Signal s ->
      Fmt.pf ppf "Signal %s" (Signal.to_string s)
  | Process.Output.Exit_non_zero int ->
      Fmt.pf ppf "Exit %i" int

let output_pp =
  let module O = Process.Output in
  let module Dump = Fmt.Dump in
  Dump.record
    [
      Dump.field "exit_status"
        (fun t -> t.O.exit_status)
        (Dump.result ~ok:Fmt.(any "()") ~error:exit_or_signal_pp)
    ; Dump.field "stdout" (fun t -> t.O.stdout) Dump.string
    ; Dump.field "stderr" (fun t -> t.O.stderr) Dump.string
    ]

let output_c = Alcotest.testable output_pp Stdlib.( = )

let test_run_status =
  let module P = Process in
  let test () =
    let* output = P.run ~prog:"true" ~args:[] ~input:"" ~env:[] in
    let expected =
      P.Output.{exit_status= Ok (); pid= output.pid; stdout= ""; stderr= ""}
    in
    Alcotest.(check output_c) "Exit status is correct" expected output ;

    let* output = P.run ~prog:"false" ~args:[] ~input:"" ~env:[] in
    let expected =
      P.Output.
        {
          exit_status= Error (Exit_non_zero 1)
        ; pid= output.pid
        ; stdout= ""
        ; stderr= ""
        }
    in
    Alcotest.(check output_c) "Exit status is correct" expected output ;

    Lwt.return ()
  in
  ("Run's exit status", `Quick, test)

let test_run_output =
  let module P = Process in
  let test () =
    let content = "@@@@@@" in
    let* output = P.run ~prog:"cat" ~args:["-"] ~input:content ~env:[] in
    let expected =
      P.Output.
        {exit_status= Ok (); pid= output.pid; stdout= content; stderr= ""}
    in
    Alcotest.(check output_c) "Stdout is correct" expected output ;

    let* output = P.run ~prog:"cat" ~args:[content] ~input:content ~env:[] in
    let stderr =
      Printf.sprintf "cat: %s: No such file or directory\n" content
    in
    let expected =
      P.Output.
        {
          exit_status= Error (Exit_non_zero 1)
        ; pid= output.pid
        ; stdout= ""
        ; stderr
        }
    in
    Alcotest.(check output_c) "Stderr is correct" expected output ;
    Lwt.return ()
  in
  ("Run output collection", `Quick, test)

let test_proc = ("Process", [test_run_status; test_run_output])

let tests = [test_sys; test_proc]

let () = Lwt_main.run @@ Alcotest_lwt.run "xapi-storage-script lib" tests
