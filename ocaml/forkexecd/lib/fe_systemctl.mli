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

val set_properties :
     ?env:string string_map
  -> ?properties:(string * string list) list
  -> service:string
  -> unit
  -> unit
(** [set_properties ?env ?properties ~service ()] sets properties on systemd
      [service].

  @param env Environment variables to set

  @param properties systemd service properties to override.
    [Some value] overrides the property.
    [None] removes any override for the property, setting back to default

  @param service the systemd service to change. Changes take effect on next
  restart.

*)

val start_templated : template:string -> instance:string -> unit
(** [start_templated ~template ~instance] starts [template@instance.service]
  instance of the [template@.service] systemd template unit.
  The service is expected to exist already.

  @param template the [template@.service] name
  @param instance parameter for template, available as %i and %I for substitution
*)

val start_transient :
     ?env:string string_map
  -> ?properties:(string * string list) list
  -> service:string
  -> string
  -> string list
  -> unit
(** [start_transient ?env ?properties ~service cmd args] generates and starts a transient systemd
  [service] that will execute [cmd args].
  stdout/stderr from the service is redirected to syslog with [service] as syslog key.
  Additional [properties] can be specified that are written into the systemd unit file's [Service]
  section.
  By default the service is not auto-restarted when it fails, and there is a 10s timeout between
  SIGTERM and SIGKILL on stop.
  On failure it raises [Spawn_internal_error(stderr, stdout, Unix.process_status)] *)

val is_active : service:string -> bool
(** [is_active ~service] checks whether the [service] is still running *)

val show : service:string -> status
(** [shows ~service] retrieves the exitcodes and PIDs of the specified [service] *)

val stop : service:string -> status
(** [stop ~service] stops the specified systemd unit (usually a transient service created above).
  On failure of the command it raises [Spawn_internal_error(stderr, stdout, Unix.process_status)].
  Returns the service's status, and unloads/deletes the unit if it was a transient service.
  (the deletion is a no-op for regular services since they won't be found in
  the transient path)
 *)

val exists : service:string -> bool
(** [exists ~service] checks whether [service] still exists in systemd.
  Note: stopped transient services get cleaned up and this will return false *)

(**/**)

val set_test : unit -> unit
