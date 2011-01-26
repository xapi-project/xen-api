(** Constructs a database RPC client speaking protocol v2 *)
module Make : functor (RPC : Db_interface.RPC) -> Db_interface.DB_ACCESS
