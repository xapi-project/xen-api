module D=Debug.Make(struct let name="xapi_slave_db" end)
open D

open Stdext
open Threadext

let slave_db_mutex = Mutex.create ()
let slave_db = ref (Db_ref.get_database (Db_backend.make ()))

let clear_db () =
  let schema = Datamodel_schema.of_datamodel () in
  slave_db := Db_cache_types.Database.set_generation 0L !slave_db;
  slave_db := (Db_cache_types.Database.make schema)

let update_context_db (__context:Context.t) =
  if Pool_role.is_slave () then
    Context.update_database __context (Db_ref.in_memory (ref slave_db))
  else
    __context

let call_with_updated_context (__context:Context.t) ?(session_id=None) f =
  let __context = update_context_db __context in
  match session_id with
  | None -> f ~__context;
  | Some id -> f ~__context:(Context.update_session_id (Some id) __context);

