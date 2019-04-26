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
type status = {
  result: string;
  exec_main_pid: int;
  exec_main_status: int;
  active_state: string;
}

(** [start_transient ?env ?properties ~service cmd args] generates and starts a transient systemd
 * [service] that will execute [cmd args].
 * stdout/stderr from the service is redirected to syslog with [service] as syslog key.
 * Additional [properties] can be specified that are written into the systemd unit file's [Service]
 * section.
 * By default the service is not auto-restarted when it fails, and there is a 10s timeout between
 * SIGTERM and SIGKILL on stop.
 * On failure it raises [Spawn_internal_error(stderr, stdout, Unix.process_status)] *)
val start_transient : ?env:string array -> ?properties:(string*string) list -> service:string ->
  string -> string list -> unit

(** [is_active ~service] checks whether the [service] is still running *)
val is_active: service:string -> bool

(** [shows ~service] retrieves the exitcodes and PIDs of the specified [service] *)
val show: service:string -> status

(** [stop ~service] stops the specified systemd unit (usually a transient service created above).
  * On failure of the command it raises [Spawn_internal_error(stderr, stdout, Unix.process_status)].
  * Returns the service's status, and unloads the unit.
  * *)
val stop: service:string -> status

(** [exists ~service] checks whether [service] still exists in systemd.
 * Note: stopped transient services get cleaned up and this will return false *)
val exists: service:string -> bool
