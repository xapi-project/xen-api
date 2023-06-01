(*
 * Copyright (C) 2019 Citrix Systems Inc.
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
(** systemd service status *)
type status = {
    result: string
  ; exec_main_pid: int
  ; exec_main_status: int
  ; active_state: string
}

(** key -> value map *)
type 'a string_map = 'a Map.Make(String).t

val start_transient :
     ?env:string string_map
  -> ?properties:(string * string list) list
  -> service:string
  -> string
  -> string list
  -> unit
(** [start_transient ?env ?properties ~service cmd args] generates and starts a transient systemd
 * [service] that will execute [cmd args].
 * stdout/stderr from the service is redirected to syslog with [service] as syslog key.
 * Additional [properties] can be specified that are written into the systemd unit file's [Service]
 * section.
 * By default the service is not auto-restarted when it fails, and there is a 10s timeout between
 * SIGTERM and SIGKILL on stop.
 * On failure it raises [Spawn_internal_error(stderr, stdout, Unix.process_status)] *)

val is_active : service:string -> bool
(** [is_active ~service] checks whether the [service] is still running *)

val show : service:string -> status
(** [shows ~service] retrieves the exitcodes and PIDs of the specified [service] *)

val stop : service:string -> status
(** [stop ~service] stops the specified systemd unit (usually a transient service created above).
  * On failure of the command it raises [Spawn_internal_error(stderr, stdout, Unix.process_status)].
  * Returns the service's status, and unloads the unit.
  * *)

val exists : service:string -> bool
(** [exists ~service] checks whether [service] still exists in systemd.
 * Note: stopped transient services get cleaned up and this will return false *)
