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

type response = string

(** A generic RPC interface *)
module type RPC = sig
  val initialise : unit -> unit
  (** [initialise ()] should be called before [rpc] *)

  val rpc : string -> response
  (** [rpc request] transmits [request] and receives a response *)
end

type table = string

type field_name = string

type field = string

type db_ref = string

type uuid = string

type regular_fields = (field_name * field) list

type associated_fields = (field_name * db_ref list) list

(** dictionary of regular fields x dictionary of associated set_ref values *)
type db_record = regular_fields * associated_fields

(** The client interface to the database *)
module type DB_ACCESS = sig
  val initialise : unit -> unit
  (** [initialise ()] must be called before any other function in this
      		interface *)

  val get_table_from_ref : Db_ref.t -> db_ref -> table option
  (** [get_table_from_ref ref tbl] returns [Some tbl] if [ref] is a
      		valid reference; None otherwise *)

  val is_valid_ref : Db_ref.t -> db_ref -> bool
  (** [is_valid_ref ref] returns true if [ref] is valid; false otherwise *)

  val read_refs : Db_ref.t -> table -> db_ref list
  (** [read_refs tbl] returns a list of all references in table [tbl] *)

  val find_refs_with_filter :
    Db_ref.t -> table -> Db_filter_types.expr -> db_ref list
  (** [find_refs_with_filter tbl expr] returns a list of all references
      		to rows which match [expr] *)

  val read_field_where : Db_ref.t -> Db_cache_types.where_record -> field list
  (** [read_field_where {tbl,return,where_field,where_value}] returns a
      		list of the [return] fields in table [tbl] where the [where_field]
      		equals [where_value] *)

  val db_get_by_uuid : Db_ref.t -> table -> uuid -> db_ref
  (** [db_get_by_uuid tbl uuid] returns the single object reference
      		associated with [uuid] *)

  val db_get_by_name_label : Db_ref.t -> table -> string -> db_ref list
  (** [db_get_by_name_label tbl label] returns the list of object references
      		associated with [label] *)

  val create_row : Db_ref.t -> table -> regular_fields -> db_ref -> unit
  (** [create_row tbl kvpairs ref] create a new row in [tbl] with
      		key [ref] and contents [kvpairs] *)

  val delete_row : Db_ref.t -> db_ref -> table -> unit
  (** [delete_row context tbl ref] deletes row [ref] from table [tbl] *)

  val write_field : Db_ref.t -> table -> db_ref -> field_name -> field -> unit
  (** [write_field context tbl ref fld val] changes field [fld] to [val] in
      		row [ref] in table [tbl] *)

  val read_field : Db_ref.t -> table -> db_ref -> field_name -> field
  (** [read_field context tbl ref fld] returns the value of field [fld]
      		in row [ref] in table [tbl] *)

  val read_record : Db_ref.t -> table -> db_ref -> db_record
  (** [read_record tbl ref] returns
      		[ (field, value) ] * [ (set_ref fieldname * [ ref ]) ] *)

  val read_records_where :
    Db_ref.t -> table -> Db_filter_types.expr -> (db_ref * db_record) list
  (** [read_records_where tbl expr] returns a list of the values returned
      		by read_record that match the expression *)

  val process_structured_field :
       Db_ref.t
    -> field_name * field
    -> table
    -> field_name
    -> db_ref
    -> Db_cache_types.structured_op_t
    -> unit
  (** [process_structured_field context kv tbl fld ref op] modifies the
      		value of field [fld] in row [ref] in table [tbl] according to [op]
      		which may be one of AddSet RemoveSet AddMap RemoveMap with
      		arguments [kv] *)
end
