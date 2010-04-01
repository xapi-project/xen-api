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
(** Helper module to let the licensing daemon send alerts to xapi *)

(** Send a XenAPI message *)
val send_alert : string -> string -> API.ref_message

val send_v6_grace_license : unit -> unit

val send_v6_upgrade_grace_license : unit -> unit

val send_v6_rejected : unit -> unit

val send_v6_comm_error : unit -> unit
