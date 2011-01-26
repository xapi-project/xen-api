module Row :
  sig
    type t
    val add : string -> string -> t -> t
	val add_defaults : Schema.Table.t -> t -> t
    val empty : t
    val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a
    val find : string -> t -> string
    val iter : (string -> string -> unit) -> t -> unit
    val remove : string -> t -> t
    val update : string -> string -> (string -> string) -> t -> t
  end

module Table :
  sig
    type t
    val add : string -> Row.t -> t -> t
    val empty : t
    val fold : (string -> Row.t -> 'a -> 'a) -> t -> 'a -> 'a
	val find_exn : string -> string -> t -> Row.t
    val find : string -> t -> Row.t
    val iter : (string -> Row.t -> unit) -> t -> unit
    val remove : string -> t -> t
    val update : string -> Row.t -> (Row.t -> Row.t) -> t -> t

	val rows : t -> Row.t list
  end

module TableSet :
  sig
    type t
    val add : string -> Table.t -> t -> t
    val empty : t
    val fold : (string -> Table.t -> 'a -> 'a) -> t -> 'a -> 'a
    val find : string -> t -> Table.t
    val iter : (string -> Table.t -> unit) -> t -> unit
    val remove : string -> t -> t
    val update : string -> Table.t -> (Table.t -> Table.t) -> t -> t
  end

module Manifest :
  sig
    type t
    val empty : t
    val make : int -> int -> Generation.t -> t
    val generation : t -> Generation.t
    val update_generation : (Generation.t -> Generation.t) -> t -> t
    val next : t -> t
	val schema : t -> int * int
	val update_schema : ((int * int) option -> (int * int) option) -> t -> t
  end

(** The core database updates (PreDelete is more of an 'event') *)
type update = 
	| WriteField of string (* tblname *) * string (* objref *) * string (* fldname *) * string  (* oldval *) * string (* newval *)
	| PreDelete of string (* tblname *) * string (* objref *)
	| Delete of string (* tblname *) * string (* objref *) * (string * string) list (* values *)
	| Create of string (* tblname *) * string (* objref *) * (string * string) list (* values *)

module Database :
  sig
    type t
    val update_manifest : (Manifest.t -> Manifest.t) -> t -> t
    val update_tableset : (TableSet.t -> TableSet.t) -> t -> t
    val manifest : t -> Manifest.t
	val tableset : t -> TableSet.t
	val schema : t -> Schema.t
    val increment : t -> t
    val update : (TableSet.t -> TableSet.t) -> t -> t
    val set_generation : Generation.t -> t -> t
    val make : Schema.t -> t

	val table_of_ref : string -> t -> string
	val lookup_key : string -> t -> (string * string) option
	val reindex : t -> t

	val register_callback : string -> (update -> t -> unit) -> t -> t
	val unregister_callback : string -> t -> t
	val notify : update -> t -> unit
  end


exception Duplicate
val add_to_set : string -> string -> string
val remove_from_set : string -> string -> string
val add_to_map : string -> string -> string -> string
val remove_from_map : string -> string -> string

val set_table : string -> Table.t -> Database.t -> Database.t
val set_row_in_table : string -> string -> Row.t -> Database.t -> Database.t
val set_field_in_row :
  string -> string -> string -> string -> Database.t -> Database.t
val remove_row_from_table : string -> string -> Database.t -> Database.t

type where_record = {
	table: string;       (** table from which ... *)
	return: string;      (** we'd like to return this field... *)
	where_field: string; (** where this other field... *)
	where_value: string; (** contains this value *)
}
val where_record_of_rpc: Rpc.t -> where_record
val rpc_of_where_record: where_record -> Rpc.t

type structured_op_t = 
	| AddSet
	| RemoveSet
	| AddMap
	| RemoveMap
val structured_op_t_of_rpc: Rpc.t -> structured_op_t
val rpc_of_structured_op_t: structured_op_t -> Rpc.t

