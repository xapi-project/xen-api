(*
 * Copyright (C) 2010 Citrix Systems Inc.
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

(** Marshall/unmarshall functions and types for db remote access protocol v2 *)

module Request = struct

  (** All possible request messages *)
  type t =
    | Get_table_from_ref of string
    | Is_valid_ref of string
    | Read_refs of string
    | Find_refs_with_filter of string * Db_filter_types.expr
    | Read_field_where of Db_cache_types.where_record
    | Db_get_by_uuid of string * string
    | Db_get_by_name_label of string * string
    | Create_row of string * (string * string) list * string
    | Delete_row of string * string
    | Write_field of string * string * string * string
    | Read_field of string * string * string
    | Read_record of string * string
    | Read_records_where of string * Db_filter_types.expr
    | Process_structured_field of (string * string) * string * string * string * Db_cache_types.structured_op_t
  [@@deriving rpc]

  (* Make sure the slave only ever uses the idempotent version *)
  let rpc_of_t t =
    let t' =
      match t with
      | Process_structured_field (a,b,c,d,Db_cache_types.AddMapLegacy) ->
        Process_structured_field (a,b,c,d,Db_cache_types.AddMap)
      | x -> x
    in
    rpc_of_t t'
end

module Response = struct

  (** All possible response messages *)
  type t =
    | Get_table_from_ref of string option
    | Is_valid_ref of bool
    | Read_refs of string list
    | Find_refs_with_filter of string list
    | Read_field_where of string list
    | Db_get_by_uuid of string
    | Db_get_by_name_label of string list
    | Create_row of unit
    | Delete_row of unit
    | Write_field of unit
    | Read_field of string
    | Read_record of (string * string) list * (string * string list) list
    | Read_records_where of (string * ((string * string) list * (string * string list) list )) list
    | Process_structured_field of unit

    | Dbcache_notfound of string * string * string
    | Duplicate_key_of of string * string * string * string
    | Uniqueness_constraint_violation of string * string * string
    | Read_missing_uuid of string * string * string
    | Too_many_values of string * string * string
  [@@deriving rpc]
end
