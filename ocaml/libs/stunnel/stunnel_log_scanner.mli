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

(** Status after checking a single log line *)
type log_line_status =
  | Continue  (** Keep scanning, nothing interesting found *)
  | LineFound  (** Target line found, stop scanning *)
  | LineError of Stunnel_error.t  (** Error detected in this line *)

(** Result of scanning through a log file *)
type log_scan_result =
  | End of int  (** Reached end of file at this position *)
  | ScanError of Stunnel_error.t * int  (** Error found at this position *)
  | ScanFound of int  (** Target found at this position *)

val ( >>= ) :
     (string -> log_line_status)
  -> (string -> log_line_status)
  -> string
  -> log_line_status
(** Monadic bind for composing log line checks.
    Usage: [check1 >>= check2] tries check1 first, only runs check2 if Continue *)

val check_stunnel_error : string -> log_line_status
(** Check if a log line contains common stunnel error patterns *)

val make_check_verify_error : unit -> string -> log_line_status
(** Create a stateful checker for certificate verification errors.
    The returned function accumulates CERT error messages from log lines
    and detects certificate verification failures.
    @return A stateful checker function that maintains its own error accumulation *)

val check_configuration_success : string -> log_line_status

val check_connection_established : string -> log_line_status

val stream_from_position :
  string -> int -> (string -> log_line_status) -> log_scan_result
(** Stream through lines from a file position, applying checker to each line.
    @param filepath Path to the log file
    @param start_pos Starting position in bytes (0-indexed)
    @param f Function to check each line
    @return Result indicating end position, found position, or error *)

val check_stunnel_logfile_from_position :
  (string -> unit) -> string -> int -> log_scan_result
(** Check stunnel log file from a position with custom line checker.
    This also logs lines and accumulates CERT errors.
    @param logger Function to log each line (e.g., for debugging)
    @param logfile Path to the stunnel log file
    @param start_pos Starting position in bytes
    @return Scan result *)

val check_stunnel_log_until_found_or_error :
     string
  -> (string -> log_line_status)
  -> float
  -> int
  -> int
  -> log_scan_result
(** Poll a stunnel log file until a condition is met or timeout.
    @param logfile Path to the log file
    @param check_line Function to check each line
    @param interval Delay between polling attempts (seconds)
    @param max_retries Maximum number of retry attempts
    @param start_pos Starting position in bytes
    @return ScanFound with position if target found, ScanError if error detected,
            or ScanError with timeout message if max retries exceeded *)
