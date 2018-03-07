(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

type response = String of string

(** A generic RPC interface *)
module type RPC = sig

  (** [initialise ()] should be called before [rpc] *)
  val initialise : unit -> unit

  (** [rpc request] transmits [request] and receives a response *)
  val rpc : string -> response
end

(** dictionary of regular fields x dictionary of associated set_ref values *)
type db_record = (string * string) list * (string * (string list)) list

(** The client interface to the database *)
module type DB_ACCESS = sig
  (** [initialise ()] must be called before any other function in this
      		interface *)
  val initialise : unit -> unit

  (** [get_table_from_ref ref] returns [Some tbl] if [ref] is a
      		valid reference; None otherwise *)
  val get_table_from_ref : Db_ref.t -> string -> string option

  (** [is_valid_ref ref] returns true if [ref] is valid; false otherwise *)
  val is_valid_ref : Db_ref.t -> string -> bool

  (** [read_refs tbl] returns a list of all references in table [tbl] *)
  val read_refs : Db_ref.t -> string -> string list

  (** [find_refs_with_filter tbl expr] returns a list of all references
      		to rows which match [expr] *)
  val find_refs_with_filter :
    Db_ref.t -> string -> Db_filter_types.expr -> string list

  (** [read_field_where {tbl,return,where_field,where_value}] returns a
      		list of the [return] fields in table [tbl] where the [where_field]
      		equals [where_value] *)
  val read_field_where : Db_ref.t -> Db_cache_types.where_record -> string list

  (** [db_get_by_uuid tbl uuid] returns the single object reference
      		associated with [uuid] *)
  val db_get_by_uuid : Db_ref.t -> string -> string -> string

  (** [db_get_by_name_label tbl label] returns the list of object references
      		associated with [label] *)
  val db_get_by_name_label : Db_ref.t -> string -> string -> string list

  (** [create_row tbl kvpairs ref] create a new row in [tbl] with
      		key [ref] and contents [kvpairs] *)
  val create_row :
    Db_ref.t -> string -> (string * string) list -> string -> unit

  (** [delete_row context tbl ref] deletes row [ref] from table [tbl] *)
  val delete_row : Db_ref.t -> string -> string -> unit

  (** [write_field context tbl ref fld val] changes field [fld] to [val] in
      		row [ref] in table [tbl] *)
  val write_field : Db_ref.t -> string -> string -> string -> string -> unit

  (** [read_field context tbl ref fld] returns the value of field [fld]
      		in row [ref] in table [tbl] *)
  val read_field : Db_ref.t -> string -> string -> string -> string

  (** [read_record tbl ref] returns
      		[ (field, value) ] * [ (set_ref fieldname * [ ref ]) ] *)
  val read_record : Db_ref.t -> string -> string -> db_record

  (** [read_records_where tbl expr] returns a list of the values returned
      		by read_record that match the expression *)
  val read_records_where : Db_ref.t -> string -> Db_filter_types.expr ->
    (string * db_record) list

  (** [process_structured_field context kv tbl fld ref op] modifies the
      		value of field [fld] in row [ref] in table [tbl] according to [op]
      		which may be one of AddSet RemoveSet AddMap RemoveMap with
      		arguments [kv] *)
  val process_structured_field :
    Db_ref.t -> string * string ->
    string -> string -> string -> Db_cache_types.structured_op_t -> unit
end


