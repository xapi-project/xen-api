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

  let _schema = Datamodel_schema.of_datamodel () ;;

  let conn = [ Parse_db_conf.make "./xapi-db.xml" ] ;;

  let flush ?(conn=conn) __context =
    Db_cache_impl.sync conn (Db_ref.get_database (Context.database_of __context))
  ;;

  let make_global ~conn ~reuse () =
    Db_backend.__test_set_master_database
      (Db_cache_types.Database.make Schema.empty);
    let db = Db_backend.make () in
    Db_cache_impl.make
      db (if reuse then conn else []) (Datamodel_schema.of_datamodel ());
    Db_cache_impl.sync conn (Db_ref.get_database db);
    Db_ref.update_database db (Db_cache_types.Database.register_callback "events" Eventgen.database_callback);
    db
  ;;

end (* Database *)

module Context : (module type of Context with type t = Context.t) = struct
  include Context
end (* Context *)

let make_context_with_new_db ?(conn=Database.conn) ?(reuse=false) task_name =
  Database.make_global ~conn ~reuse () |> ignore;
  Context.make task_name
