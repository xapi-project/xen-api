module Row :
  sig
    type t
    val add : int64 -> string -> string -> t -> t
	val add_defaults : int64 -> Schema.Table.t -> t -> t
    val empty : t
    val fold : (string -> int64 -> int64 -> string -> 'a -> 'a) -> t -> 'a -> 'a
    val find : string -> t -> string
    val iter : (string -> string -> unit) -> t -> unit
    val remove : string -> t -> t
    val update : int64 -> string -> string -> (string -> string) -> t -> t
	val fold_over_recent : int64 -> (int64 -> int64 -> int64 -> string -> string -> 'b -> 'b) -> t -> 'b -> 'b
  end

module Table :
  sig
    type t
    val add : int64 -> string -> Row.t -> t -> t
    val empty : t
    val fold : (string -> int64 -> int64 -> Row.t -> 'a -> 'a) -> t -> 'a -> 'a
	val find_exn : string -> string -> t -> Row.t
    val find : string -> t -> Row.t
	val mem : string -> t -> bool
    val iter : (string -> Row.t -> unit) -> t -> unit
    val remove : int64 -> string -> t -> t
    val update : int64 -> string -> Row.t -> (Row.t -> Row.t) -> t -> t
    val fold_over_recent : int64 -> (int64 -> int64 -> int64 -> string -> 'b -> 'b) -> (unit -> unit) -> t -> 'b -> 'b
	val rows : t -> Row.t list
  end

module TableSet :
  sig
    type t
    val add : int64 -> string -> Table.t -> t -> t
    val empty : t
    val fold : (string -> int64 -> int64 -> Table.t -> 'a -> 'a) -> t -> 'a -> 'a
    val find : string -> t -> Table.t
    val iter : (string -> Table.t -> unit) -> t -> unit
    val remove : string -> t -> t
    val update : int64 -> string -> Table.t -> (Table.t -> Table.t) -> t -> t
    val fold_over_recent : int64 -> (int64 -> int64 -> int64 -> string -> Table.t -> 'b -> 'b) -> t -> 'b -> 'b
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

(** The core database updates (RefreshRow and PreDelete is more of an 'event') *)
type update = 
	| RefreshRow of string (* tblname *) * string (* objref *)
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

val set_field : string -> string -> string -> string -> Database.t -> Database.t
val get_field : string -> string -> string -> Database.t -> string
val remove_row : string -> string -> Database.t -> Database.t
val add_row : string -> string -> Row.t -> Database.t -> Database.t

val update_generation : string -> string -> Database.t -> Database.t

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

