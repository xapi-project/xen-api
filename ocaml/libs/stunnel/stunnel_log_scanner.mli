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

val stunnel_error_indicators : string list
(** List of known stunnel error indicator strings *)

val stunnel_error_checker : string -> (string, Stunnel_error.t) result option
(** Check if a log line contains common stunnel error patterns *)

val certificate_verify_error_indicators : string list
(** List of certificate verification error indicator strings *)

val make_check_verify_error :
  unit -> string -> (string, Stunnel_error.t) result option
(** Create a stateful checker for certificate verification errors.
    The returned function accumulates CERT error messages from log lines
    and detects certificate verification failures.
    @return A stateful checker function that maintains its own error accumulation *)

val configuration_success_indicators : string list
(** List of configuration success indicator strings *)

val configuration_success_checker :
  string -> (string, Stunnel_error.t) result option
(** Check if a log line indicates successful configuration *)

val scan :
     ic:in_channel
  -> line_checker:(string -> (string, Stunnel_error.t) result option)
  -> (string, Stunnel_error.t) result option
(** Scan through an input channel line by line, applying the line checker.
    Returns the first non-None result from the checker, or None if EOF is reached. *)

val check_stunnel_logfile :
  ic:in_channel -> (string -> unit) -> (unit, Stunnel_error.t) result
(** Check stunnel log file with custom line checker and logger.
    This logs lines and accumulates CERT errors.
    @param ic Input channel to read from
    @param logger Function to log each line (e.g., for debugging)
    @return Ok () if no errors found, Error with Stunnel_error.t if error detected *)

val check_stunnel_log_until_found_or_error :
     ic:in_channel
  -> line_checker:(string -> (string, Stunnel_error.t) result option)
  -> float
  -> int
  -> (string, Stunnel_error.t) result
(** Poll a stunnel log file until a condition is met or timeout.
    @param ic Input channel to read from
    @param line_checker Function to check each line
    @param interval Delay between polling attempts (seconds)
    @param max_retries Maximum number of retry attempts
    @return Ok with signature string if target found, Error if error detected or timeout *)

val wait_for_configuration_success :
  ic:in_channel -> (string, Stunnel_error.t) result
(** Wait for configuration success message in stunnel log.
    Polls the log file until "Configuration successful" is found or timeout.
    @param ic Input channel to read from
    @return Ok with success message if found, Error if timeout or error detected *)
