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

val add_dhcp_ntp_servers : unit -> unit

val remove_dhcp_ntp_servers : unit -> unit

val set_servers_in_conf : string list -> unit

val clear_servers_in_conf : unit -> unit

val restart_ntp_service : unit -> unit

val enable_ntp_service : unit -> unit

val disable_ntp_service : unit -> unit

val is_ntp_service_active : unit -> bool

val get_servers_from_conf : unit -> string list

val is_ntp_dhcp_enabled : unit -> bool
