(*
 * Copyright (c) Cloud Software Group, Inc.
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

open Stunnel_log_scanner

(** Path to test data directory - relative to where dune runs the test *)
let data_dir = "data"

(** Helper to build path to test log file *)
let log_path filename = Filename.concat data_dir filename

(** Collect logged lines for verification *)
let make_logger () =
  let lines = ref [] in
  let log line = lines := line :: !lines in
  (log, fun () -> List.rev !lines)

(** Calculate byte position after a line containing the target substring *)
let calculate_position_of_line filename target_substring =
  let acc_bytes = ref 0 in
  Xapi_stdext_unix.Unixext.readfile_line
    (fun line ->
      let line_bytes = String.length line + 1 in
      (* +1 for newline *)
      acc_bytes := !acc_bytes + line_bytes ;
      if Astring.String.is_infix ~affix:target_substring line then
        raise Xapi_stdext_unix.Unixext.Break
    )
    filename ;
  !acc_bytes

(** Test successful connection log *)
let test_successful_connection () =
  let logfile = log_path "successful_connection.log" in
  let logger, _get_lines = make_logger () in

  match check_stunnel_logfile_from_position logger logfile 0 with
  | End pos ->
      let size = Unix.(stat logfile).st_size in
      Alcotest.(check int) "Should reach end of file" size pos
  | ScanFound _ ->
      Alcotest.fail "Should not get ScanFound in successful log"
  | ScanError (e, _) ->
      Alcotest.fail
        ("Should not error on successful connection log: "
        ^ Stunnel_error.to_string e
        )

let test_certificate_verify logfile expected_substring () =
  let logfile = log_path logfile in
  let logger, _get_lines = make_logger () in
  let expected_pos =
    calculate_position_of_line logfile "certificate verify failed"
  in

  match check_stunnel_logfile_from_position logger logfile 0 with
  | ScanError (Stunnel_error.Certificate_verify msg, pos) ->
      Alcotest.(check int) "Position should match expected" expected_pos pos ;
      Alcotest.(check bool)
        "Error message should contain expected substring" true
        (Astring.String.is_infix ~affix:expected_substring msg)
  | _ ->
      Alcotest.fail "Should detect certificate verification failure"

let test_stunnel_error logfile expected_substring () =
  let logfile = log_path logfile in
  let logger, _get_lines = make_logger () in
  let expected_pos = calculate_position_of_line logfile expected_substring in

  match check_stunnel_logfile_from_position logger logfile 0 with
  | ScanError (Stunnel_error.Stunnel msg, pos) ->
      Alcotest.(check int) "Position should match expected" expected_pos pos ;
      Alcotest.(check bool)
        "Error message should contain expected substring" true
        (Astring.String.is_infix ~affix:expected_substring msg)
  | _ ->
      Alcotest.fail "Should detect stunnel error"

let test_check_stunnel_log_until () =
  let logfile = log_path "successful_connection.log" in
  let check_line line =
    if Astring.String.is_infix ~affix:"Configuration successful" line then
      LineFound
    else
      Continue
  in
  match check_stunnel_log_until_found_or_error logfile check_line 0.1 2 0 with
  | ScanFound pos ->
      Alcotest.(check bool) "Should return positive pos" true (pos > 0) ;
      (* Verify we can read the next line from the returned position *)
      let fd = Unix.openfile logfile [Unix.O_RDONLY] 0 in
      let finally = Xapi_stdext_pervasives.Pervasiveext.finally in
      finally
        (fun () ->
          let _ = Unix.lseek fd pos Unix.SEEK_SET in
          let ic = Unix.in_channel_of_descr fd in
          let line = input_line ic in
          (* The next line after "Configuration successful" should be about service accepting connection *)
          Alcotest.(check bool)
            "Next line should be about service accepted" true
            (Astring.String.is_infix ~affix:"Service [client-proxy] accepted"
               line
            )
        )
        (fun () -> Unix.close fd)
  | ScanError (e, _pos) ->
      Alcotest.fail ("Should find target line: " ^ Stunnel_error.to_string e)
  | End _ ->
      Alcotest.fail "Should not reach end without finding target line"

let tests =
  [
    ( "test_stunnel_log_scanner"
    , [
        Alcotest.test_case "successful_connection" `Quick
          test_successful_connection
      ; Alcotest.test_case "certificate_self_signed" `Quick
          (test_certificate_verify "certificate_self_signed.log"
             "self-signed certificate"
          )
      ; Alcotest.test_case "certificate_expired" `Quick
          (test_certificate_verify "certificate_expired.log"
             "certificate has expired"
          )
      ; Alcotest.test_case "subject_checks_failed" `Quick
          (test_certificate_verify "subject_checks_failed.log"
             "Subject checks failed"
          )
      ; Alcotest.test_case "connection_refused" `Quick
          (test_stunnel_error "connection_refused.log" "Connection refused")
      ; Alcotest.test_case "no_host_resolved" `Quick
          (test_stunnel_error "no_host_resolved.log" "No host resolved")
      ; Alcotest.test_case "configuration_failed" `Quick
          (test_stunnel_error "configuration_failed.log" "Configuration failed")
      ; Alcotest.test_case "check_stunnel_log_until" `Quick
          test_check_stunnel_log_until
      ]
    )
  ]

let () = Alcotest.run "StunnelLogScanner" tests
