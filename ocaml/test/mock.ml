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
	let _db_ref = ref (ref (Db_cache_types.Database.make _schema))

	let make () =
		(* generic_database_upgrade will create and populate with
		   default values all the tables which don't exist. *)
		!_db_ref := Db_upgrade.generic_database_upgrade !(!_db_ref) ;
		Db_ref.in_memory _db_ref

end (* Database *)

module Context : (module type of Context with type t = Context.t) = struct
	include Context
end (* Context *)
