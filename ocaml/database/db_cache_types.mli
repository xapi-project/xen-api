(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
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

module Time : sig
  type t = Generation.t
  (** A monotonically increasing counter associated with this database *)
end

module Stat : sig
  type t = {
    created: Time.t;  (** Time this value was created *)
    modified: Time.t; (** Time this value was last modified *)
    deleted: Time.t;  (** Time this value was deleted (or 0L meaning it is still alive) *)
  }
  (** Metadata associated with a database value *)
end

module type MAP = sig
  type t
  (** A map from string to some value *)

  type value
  (** The type of the values in the map *)

  val empty : t
  (** The empty map *)

  val add: Time.t -> string -> value -> t -> t
  (** [add now key value map] returns a new map with [key] associated with [value],
      with creation time [now] *)

  val remove : Time.t -> string -> t -> t
  (** [remove now key t] removes the binding of [key] from [t]. *)

  val fold : (string -> Stat.t -> value -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold f t initial] folds [f key stats value acc] over the items in [t] *)

  val fold_over_recent : Time.t -> (string -> Stat.t -> value -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold_over_recent since f t initial] folds [f key stats value acc] over all the
      items with a modified time larger than [since] *)

  val find : string -> t -> value
  (** [find key t] returns the value associated with [key] in [t] or raises
      [DBCache_NotFound] *)

  val mem : string -> t -> bool
  (** [mem key t] returns true if [value] is associated with [key] in [t] or false
      otherwise *)

  val iter : (string -> value -> unit) -> t -> unit
  (** [iter f t] applies [f key value] to each binding in [t] *)

  val update : Time.t -> string -> value -> (value -> value) -> t -> t
  (** [update now key default f t] returns a new map which is the same as [t] except:
      if there is a value associated with [key] it is replaced with [f key]
      or if there is no value associated with [key] then [default] is associated with [key].
      This function touches the modification time of [key] *unless* [f key] is physically
      equal with the current value: in this case the modification time isn't bumped as
      an optimisation.
  *)

  val touch : Time.t -> string -> value -> t -> t
  (** [touch now key default t] returns a new map which is the same as [t] except:
      if there is a value associated with [t] then its modification time is set to [now];
      if there is no value associated with [t] then one is created with value [default].
      On exit there will be a binding of [key] whose modification time is [now] *)
end

module Row : sig
  include MAP
    with type value = Schema.Value.t

  val add_defaults: Time.t -> Schema.Table.t -> t -> t
  (** [add_defaults now schema t]: returns a row which is [t] extended to contain
      all the columns specified in the schema, with default values set if not already
      in [t]. If the schema is missing a default value then raises [DBCache_NotFound]:
      this would happen if a client failed to provide a necessary field. *)

end

module Table : sig
  include MAP
    with type value = Row.t

  val fold_over_deleted : Time.t -> (string -> Stat.t -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold_over_deleted now f t initial] folds [f key stat acc] over the keys
      which have been recently deleted. Note this is not guaranteed to remember
      all events, so the list may be short. *)
end

module TableSet : MAP with type value = Table.t

module Manifest :
sig
  type t
  val empty : t
  val make : int -> int -> Generation.t -> t
  val generation : t -> Generation.t
  val touch : (Generation.t -> Generation.t) -> t -> t
  val next : t -> t
  val schema : t -> int * int
  val update_schema : ((int * int) option -> (int * int) option) -> t -> t
end

(** The core database updates (RefreshRow and PreDelete is more of an 'event') *)
type update =
  | RefreshRow of string (* tblname *) * string (* objref *)
  | WriteField of string (* tblname *) * string (* objref *) * string (* fldname *) * Schema.Value.t (* newval *)
  | PreDelete of string (* tblname *) * string (* objref *)
  | Delete of string (* tblname *) * string (* objref *) * (string * Schema.Value.t) list (* values *)
  | Create of string (* tblname *) * string (* objref *) * (string * Schema.Value.t) list (* values *)

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
val add_to_set : string -> Schema.Value.t -> Schema.Value.t
val remove_from_set : string -> Schema.Value.t -> Schema.Value.t
val add_to_map : idempotent:bool -> string -> string -> Schema.Value.t -> Schema.Value.t
val remove_from_map : string -> Schema.Value.t -> Schema.Value.t

val set_field : string -> string -> string -> Schema.Value.t -> Database.t -> Database.t
val get_field : string -> string -> string -> Database.t -> Schema.Value.t
val remove_row : string -> string -> Database.t -> Database.t
val add_row : string -> string -> Row.t -> Database.t -> Database.t
val touch : string -> string -> Database.t -> Database.t

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
  | AddMapLegacy
val structured_op_t_of_rpc: Rpc.t -> structured_op_t
val rpc_of_structured_op_t: structured_op_t -> Rpc.t
