type row 
type table 
type cache 

type where_record = {
  table : string;
  return : string;
  where_field : string;
  where_value : string;
}
type structured_op_t = AddSet | RemoveSet | AddMap | RemoveMap
type db_dump_manifest = {
  installation_uuid : string;
  control_domain_uuid : string;
  pool_conf : string;
  pool_token : string;
  schema_major_vsn : int;
  schema_minor_vsn : int;
  product_version : string;
  product_brand : string;
  build_number : string;
  xapi_major_vsn : int;
  xapi_minor_vsn : int;
  generation_count : Int64.t;
}
val gen_manifest : Int64.t -> db_dump_manifest

val lookup_field_in_row : row -> string -> string
val lookup_table_in_cache : cache -> string -> table
val lookup_row_in_table : table -> string -> string -> row
val iter_over_rows : (string -> row -> unit) -> table -> unit
val iter_over_tables : (string -> table -> unit) -> cache -> unit
val iter_over_fields : (string -> string -> unit) -> row -> unit
val set_field_in_row : row -> string -> string -> unit
val set_row_in_table : table -> string -> row -> unit
val set_table_in_cache : cache -> string -> table -> unit
val create_empty_row : unit -> row
val create_empty_table : unit -> table
val create_empty_cache : unit -> cache
val fold_over_fields : (string -> string -> 'a -> 'a) -> row -> 'a -> 'a
val fold_over_rows : (string -> row -> 'a -> 'a) -> table -> 'a -> 'a
val fold_over_tables : (string -> table -> 'a -> 'a) -> cache -> 'a -> 'a

val get_rowlist : table -> row list
val get_reflist : table -> string list
val get_column : cache -> string -> string -> string list
val find_row : cache -> string -> string -> row
val remove_row_from_table : table -> string -> unit
val snapshot : cache -> cache

(* This only exists because we want to artificially create events where
   relationships exist between database tables. If the relationships were
   stored explicitly in the rows, this wouldn't be necessary *)
val bump_event_number_in_row : row -> unit

(* These are for the event mechanism *)
exception Too_many_deletion_events
val fold_over_recent_rows : (int64 -> int64 -> int64 -> string -> 'a -> 'a) -> table -> int64 -> 'a -> 'a
val get_current_event_number : unit -> int64
