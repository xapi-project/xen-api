(*
 * Copyright (C) Cloud Software Group
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

val delay : Scheduler.PipeDelay.t

exception Uninitialised

val is_slave : (unit -> bool) ref

val get_master_address : (unit -> string) ref

val master_rpc_path : string ref

exception Cannot_connect_to_master

val force_connection_reset : unit -> unit

val start_master_connection_watchdog : unit -> unit

exception Goto_handler

val on_database_connection_established : (unit -> unit) ref

val open_secure_connection : unit -> unit

val connection_timeout : float ref

val restart_on_connection_timeout : bool ref

exception Content_length_required

val execute_remote_fn : string -> Db_interface.response
