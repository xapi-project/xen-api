(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

module Database = struct

	let _schema = Datamodel_schema.of_datamodel ()

	let make () =
		(* generic_database_upgrade will create and populate with
		   default values all the tables which don't exist. *)
		let db = Db_upgrade.generic_database_upgrade (Db_cache_types.Database.make (Datamodel_schema.of_datamodel ())) in
		Db_ref.in_memory (ref (ref db))

end (* Database *)

module Context : (module type of Context with type t = Context.t) = struct
	include Context
end (* Context *)
