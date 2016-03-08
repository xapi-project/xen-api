(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

type level = Emerg | Alert | Crit | Err | Warning | Notice | Info | Debug
type facility =
    Auth
  | Authpriv
  | Cron
  | Daemon
  | Ftp
  | Kern
  | Local0
  | Local1
  | Local2
  | Local3
  | Local4
  | Local5
  | Local6
  | Local7
  | Lpr
  | Mail
  | News
  | Syslog
  | User
  | Uucp

external log : facility -> level -> string -> unit = "stub_syslog"
external close : unit -> unit = "stub_closelog"

val facility_of_string : string -> facility
(** [facility_of_string facility] Return the Syslog facility corresponding to [facility].
    Raises [Unknown_facility facility] if facility is unrecognized. *)

val level_of_string : string -> level
(** [level_of_string level] Return the Syslog level corresponding to [level].
    Raises [Unknown_level level] if level is unrecognized. *)

val string_of_level : level -> string
(** [string_of_level level] Return the string corresponding to the Syslog level [level] *)

val is_masked : threshold:level -> level -> bool
(** [is_masked ~threshold level] Return true if [level] is below [threshold] and should therefore
    not be logged. *)
