(*
 * Copyright (C) Citrix Systems Inc.
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

val db_FLUSH_TIMER : float

val master_database : Db_cache_types.Database.t ref

val make : unit -> Db_ref.t

val blow_away_non_persistent_fields :
	Schema.t -> Db_cache_types.Database.t -> Db_cache_types.Database.t

val register_session_with_database : API.ref_session -> Db_ref.t -> unit

val unregister_session : API.ref_session -> unit

val is_session_registered : API.ref_session -> bool

val get_registered_database : API.ref_session -> Db_ref.t option
