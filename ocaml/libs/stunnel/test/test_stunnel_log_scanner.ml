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
  let log line =
    Printf.printf "%s\n" line ;
    lines := line :: !lines
  in
  (log, fun () -> List.rev !lines)

let calculate_next_line logfile substring =
  let lines = Xapi_stdext_unix.Unixext.read_lines ~path:logfile in
  match
    Xapi_stdext_std.Listext.List.find_index
      (fun line -> Astring.String.is_infix ~affix:substring line)
      lines
  with
  | Some index ->
      List.nth_opt lines (index + 1)
  | None ->
      None

(** Read the next line from an input channel *)
let read_next_line ic = try Some (input_line ic) with End_of_file -> None

(** Test successful connection log *)
let test_successful_connection () =
  let logfile = log_path "successful_connection.log" in
  let logger, _get_lines = make_logger () in
  Xapi_stdext_unix.Unixext.with_input_channel logfile @@ fun ic ->
  match check_stunnel_logfile ~ic logger with
  | Ok () ->
      Alcotest.(check bool)
        "Should reach the end of the log" true
        (read_next_line ic = None)
  | Error e ->
      Alcotest.fail
        ("Should not error on successful connection log: "
        ^ Stunnel_error.to_string e
        )

let test_certificate_verify logfile expected_substring () =
  let logfile = log_path logfile in
  let logger, _get_lines = make_logger () in
  Xapi_stdext_unix.Unixext.with_input_channel logfile @@ fun ic ->
  match check_stunnel_logfile ~ic logger with
  | Error (Stunnel_error.Certificate_verify msg) ->
      Alcotest.(check bool)
        "Error message should contain expected substring" true
        (Astring.String.is_infix ~affix:expected_substring msg) ;
      let next_line = calculate_next_line logfile "certificate verify failed" in
      let next_line_str = Option.value next_line ~default:"<no next line>" in
      Alcotest.(check bool)
        ("next line should be matched: " ^ next_line_str)
        true
        (read_next_line ic = next_line)
  | Ok () ->
      Alcotest.fail "Should detect certificate verification failure"
  | Error e ->
      Alcotest.fail
        ("Wrong error type: "
        ^ Stunnel_error.to_string e
        ^ ", expected certificate verify error"
        )

let test_stunnel_error logfile expected_substring () =
  let logfile = log_path logfile in
  let logger, _get_lines = make_logger () in
  Xapi_stdext_unix.Unixext.with_input_channel logfile @@ fun ic ->
  match check_stunnel_logfile ~ic logger with
  | Error (Stunnel_error.Stunnel msg) ->
      Alcotest.(check string)
        "Error message should be expected substring" expected_substring msg ;
      let next_line = calculate_next_line logfile expected_substring in
      let next_line_str = Option.value next_line ~default:"<no next line>" in
      Alcotest.(check bool)
        ("next line should be matched: " ^ next_line_str)
        true
        (read_next_line ic = next_line)
  | Ok () ->
      Alcotest.fail "Should detect stunnel error"
  | Error e ->
      Alcotest.fail
        ("Wrong error type: "
        ^ Stunnel_error.to_string e
        ^ ", expected stunnel error"
        )

let test_wait_for_configuration_success () =
  let logfile = log_path "successful_connection.log" in
  Xapi_stdext_unix.Unixext.with_input_channel logfile @@ fun ic ->
  match wait_for_configuration_success ~ic with
  | Ok ind ->
      Alcotest.(check string)
        "Indicator should be expected substring" "Configuration successful" ind
  | Error e ->
      Alcotest.fail
        ("Should detect configuration success, but got error: "
        ^ Stunnel_error.to_string e
        )

let test_wait_for_configuration_fail () =
  let logfile = log_path "configuration_failed.log" in
  Xapi_stdext_unix.Unixext.with_input_channel logfile @@ fun ic ->
  match wait_for_configuration_success ~ic with
  | Ok _ ->
      Alcotest.fail "Should detect configuration failure"
  | Error (Stunnel_error.Stunnel msg) ->
      Alcotest.(check string)
        "Should detect configuration failure" "Configuration failed" msg
  | Error e ->
      Alcotest.fail
        ("Wrong error type: "
        ^ Stunnel_error.to_string e
        ^ ", expected stunnel error for configuration failure"
        )

let test_wait_timeout_for_configuration_success () =
  let logfile = log_path "empty.log" in
  Xapi_stdext_unix.Unixext.with_input_channel logfile @@ fun ic ->
  match
    check_stunnel_log_until_found_or_error ~ic
      ~line_checker:configuration_success_checker 1.0 3
  with
  | Ok _ ->
      Alcotest.fail "Should detect timeout waiting for configuration success"
  | Error (Stunnel_error.Stunnel msg) ->
      Printf.printf "Received error message: %s\n" msg ;
      Alcotest.(check string)
        "Should detect timeout" "Timed out waiting for stunnel condition" msg
  | Error e ->
      Alcotest.fail
        ("Wrong error type: "
        ^ Stunnel_error.to_string e
        ^ ", expected stunnel error for timeout"
        )

let file_append filename contents =
  let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 filename in
  output_string oc contents ; close_out oc

let with_created_logfile logfile f =
  Xapi_stdext_unix.Unixext.touch_file logfile ;
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f logfile)
    (fun () -> Xapi_stdext_unix.Unixext.unlink_safe logfile)

let test_writing_log () =
  (* Create a test log file *)
  let logfile = log_path "test.log" in
  let logger, get_lines = make_logger () in
  let config_part =
    {|2026.01.09 05:45:23 LOG5[ui]: stunnel 5.60 on x86_64-koji-linux-gnu platform
2026.01.09 05:45:23 LOG5[ui]: Compiled/running with OpenSSL 3.0.9 30 May 2023
2026.01.09 05:45:23 LOG5[ui]: Threading:PTHREAD Sockets:POLL,IPv6 TLS:ENGINE,OCSP,SNI Auth:LIBWRAP
2026.01.09 05:45:23 LOG5[ui]: Reading configuration from descriptor 8
2026.01.09 05:45:23 LOG5[ui]: UTF-8 byte order mark not detected
2026.01.09 05:45:23 LOG5[ui]: FIPS mode disabled
2026.01.09 05:45:23 LOG5[ui]: Configuration successful
|}
  in
  let connection_part =
    {|2026.01.09 05:45:24 LOG5[0]: Service [client-proxy] accepted connection from unnamed socket
2026.01.09 05:45:24 LOG5[0]: s_connect: connecting 192.168.1.100:443
2026.01.09 05:45:24 LOG3[0]: s_connect: connect 192.168.1.100:443: Connection refused
2026.01.09 05:45:24 LOG5[0]: Connection reset: 0 byte(s) sent to TLS, 0 byte(s) sent to socket
|}
  in
  with_created_logfile logfile @@ fun logfile ->
  file_append logfile config_part ;
  Xapi_stdext_unix.Unixext.with_input_channel logfile @@ fun ic ->
  ( match wait_for_configuration_success ~ic with
  | Ok ind ->
      Alcotest.(check string)
        "Indicator should be expected substring" "Configuration successful" ind
  | Error e ->
      Alcotest.fail
        ("Should detect configuration success, but got error: "
        ^ Stunnel_error.to_string e
        )
  ) ;
  file_append logfile connection_part ;
  let next_line = calculate_next_line logfile "Configuration successful" in
  let next_line_str = Option.value next_line ~default:"<no next line>" in
  (* test stunnel error *)
  match check_stunnel_logfile ~ic logger with
  | Error (Stunnel_error.Stunnel msg) ->
      Alcotest.(check string)
        "Error message should be expected substring" "Connection refused" msg ;
      let first_line =
        match get_lines () with
        | first :: _ ->
            first
        | [] ->
            "<no logged lines>"
      in
      Alcotest.(check string)
        ("first logged line should be matched: " ^ first_line)
        next_line_str first_line
  | Ok () ->
      let str = String.concat "\n" (get_lines ()) in
      Alcotest.fail ("Should detect stunnel error, but got: " ^ str)
  | Error e ->
      Alcotest.fail
        ("Wrong error type: "
        ^ Stunnel_error.to_string e
        ^ ", expected stunnel error"
        )

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
      ; Alcotest.test_case "wait_for_configuration_success" `Quick
          test_wait_for_configuration_success
      ; Alcotest.test_case "wait_for_configuration_fail" `Quick
          test_wait_for_configuration_fail
      ; Alcotest.test_case "test_wait_timeout_for_configuration_success" `Quick
          test_wait_timeout_for_configuration_success
      ; Alcotest.test_case "writing_log" `Quick test_writing_log
      ]
    )
  ]

let () = Alcotest.run "StunnelLogScanner" tests
