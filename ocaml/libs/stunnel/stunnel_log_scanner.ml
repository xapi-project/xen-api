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

(** Stunnel log file scanning and analysis *)

(** Monadic bind for log_line_status composition *)
let ( >>= ) check1 check2 line =
  match check1 line with None -> check2 line | r -> r

let find ~indicators ~box line =
  List.find_map
    (fun ind ->
      if Astring.String.is_infix ~affix:ind line then
        Some (box ind)
      else
        None
    )
    indicators

let stunnel_error_indicators =
  [
    "Configuration failed"
  ; "Connection refused"
  ; "No host resolved"
  ; "No route to host"
  ; "Invalid argument"
  ; "Address already in use"
  ]

let stunnel_error_checker =
  find ~indicators:stunnel_error_indicators ~box:(fun ind ->
      Error (Stunnel_error.Stunnel ind)
  )

let certificate_verify_error_indicators = ["certificate verify failed"]

let make_check_verify_error () =
  let cert_errors = ref [] in
  fun line ->
    (* Extract and accumulate CERT errors from the line
     * examples:
     * CERT: Pre-verification error: certificate has expired
     * CERT: Subject checks failed *)
    Astring.String.cut ~rev:true ~sep:"CERT: " line
    |> Option.iter (fun (_, cert_error) ->
        cert_errors := cert_error :: !cert_errors
    ) ;
    (* When verified with a mismatched certificate, one line of log from stunnel
     * would look like:
       SSL_connect: ssl/statem/statem_clnt.c:1889: error:0A000086:SSL routines::certificate verify failed
     * in this case, Stunnel_verify_error can be raised with detailed error as
     * reason if it can find in the log *)
    find ~indicators:certificate_verify_error_indicators
      ~box:(fun _ ->
        Error (Stunnel_error.Certificate_verify (String.concat ";" !cert_errors))
      )
      line

let configuration_success_indicators = ["Configuration successful"]

let configuration_success_checker =
  find ~indicators:configuration_success_indicators ~box:(fun ind -> Ok ind)

let scan ~ic ~line_checker =
  let rec loop () =
    match input_line ic with
    | exception End_of_file ->
        None
    | line -> (
      match line_checker line with None -> loop () | r -> r
    )
  in
  loop ()

let check_stunnel_logfile ~ic logger =
  let verify_error_checker = make_check_verify_error () in
  let line_checker line =
    logger line ;
    (verify_error_checker >>= stunnel_error_checker) line
  in
  match scan ~ic ~line_checker with Some (Error e) -> Error e | _ -> Ok ()

let check_stunnel_log_until_found_or_error ~ic ~line_checker interval
    max_retries =
  let rec check cnt =
    match scan ~ic ~line_checker with
    | None when cnt < max_retries ->
        Thread.delay interval ;
        check (cnt + 1)
    | None ->
        Error (Stunnel_error.Stunnel "Timed out waiting for stunnel condition")
    | Some (Error e) ->
        Error e
    | Some (Ok ind) ->
        Ok ind
  in
  check 1

let wait_for_configuration_success ~ic =
  let line_checker = configuration_success_checker >>= stunnel_error_checker in
  check_stunnel_log_until_found_or_error ~ic ~line_checker 1.0 3
