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

(** A setter for the master database. Only used by the unit testing code. *)
val __test_set_master_database : Db_cache_types.Database.t -> unit

val make : unit -> Db_ref.t

val blow_away_non_persistent_fields :
	Schema.t -> Db_cache_types.Database.t -> Db_cache_types.Database.t

val create_registered_session :
	(unit -> API.ref_session) -> Db_ref.t -> API.ref_session

val unregister_session : API.ref_session -> unit

val is_session_registered : API.ref_session -> bool

val get_registered_database : API.ref_session -> Db_ref.t option
