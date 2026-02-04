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

type log_line_status = Continue | LineFound | LineError of Stunnel_error.t

type log_scan_result =
  | End of int
  | ScanError of Stunnel_error.t * int
  | ScanFound of int

(** Monadic bind for log_line_status composition *)
let ( >>= ) check1 check2 line =
  match check1 line with Continue -> check2 line | r -> r

let if_contain affix line matcher =
  if Astring.String.is_infix ~affix line then
    matcher
  else
    Continue

(** Early return unless get line status Continue *)
let try_iter f lst =
  let rec aux = function
    | [] ->
        Continue
    | x :: xs -> (
      match f x with Continue -> aux xs | r -> r
    )
  in
  aux lst

let check_stunnel_error line =
  [
    "Configuration failed"
  ; "Connection refused"
  ; "No host resolved"
  ; "No route to host"
  ; "Invalid argument"
  ; "Address already in use"
  ]
  |> try_iter (fun s -> if_contain s line (LineError (Stunnel_error.Stunnel s)))

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
     * reason if it can found in the log *)
    if Astring.String.is_infix ~affix:"certificate verify failed" line then
      let err_msg = String.concat "; " !cert_errors in
      LineError (Stunnel_error.Certificate_verify err_msg)
    else
      Continue

let check_configuration_success line =
  if Astring.String.is_infix ~affix:"Configuration successful" line then
    LineFound
  else
    Continue

let check_connection_established line =
  if Astring.String.is_infix ~affix:"Certificate accepted" line then
    LineFound
  else
    Continue

(** Stream through lines from a specific position, applying function to each.
    Returns new_position. Stops early on Error. *)
let stream_from_position (filepath : string) (start_pos : int)
    (f : string -> log_line_status) : log_scan_result =
  try
    let fd = Unix.openfile filepath [Unix.O_RDONLY] 0 in
    let finally = Xapi_stdext_pervasives.Pervasiveext.finally in
    finally
      (fun () ->
        let _ = Unix.lseek fd start_pos Unix.SEEK_SET in
        let ic = Unix.in_channel_of_descr fd in
        let rec loop () =
          match input_line ic with
          | exception End_of_file ->
              let end_pos = pos_in ic in
              End end_pos
          | line -> (
            match f line with
            | Continue ->
                loop ()
            | LineError e ->
                let pos = pos_in ic in
                ScanError (e, pos)
            | LineFound ->
                let pos = pos_in ic in
                ScanFound pos
          )
        in
        loop ()
      )
      (fun () -> Unix.close fd)
  with
  | Unix.Unix_error (err, fn, arg) ->
      ScanError
        ( Stunnel_error.Stunnel
            (Printf.sprintf "%s: %s(%s)" (Unix.error_message err) fn arg)
        , start_pos
        )
  | e ->
      ScanError (Stunnel_error.Stunnel (Printexc.to_string e), start_pos)

let check_stunnel_logfile_from_position logger logfile start_pos =
  let check_verify_error = make_check_verify_error () in
  let check_line line =
    logger line ;
    (check_verify_error >>= check_stunnel_error) line
  in
  stream_from_position logfile start_pos check_line

let check_stunnel_log_until_found_or_error logfile check_line interval
    max_retries start_pos =
  let rec check ~max_retries cnt start_pos =
    match stream_from_position logfile start_pos check_line with
    | End new_pos when cnt <= max_retries ->
        Thread.delay interval ;
        check ~max_retries (cnt + 1) new_pos
    | End pos ->
        ScanError
          (Stunnel_error.Stunnel "Timed out waiting for stunnel condition", pos)
    | r ->
        r
  in
  check ~max_retries 0 start_pos
