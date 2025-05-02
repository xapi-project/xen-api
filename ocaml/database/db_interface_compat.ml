(*
 * Copyright (C) Cloud Software Group
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

open Db_interface

module OfCached (DB : DB_ACCESS2) : DB_ACCESS = struct
  include DB include DB.Compat
end

module OfCompat (DB : DB_ACCESS) : DB_ACCESS2 = struct
  module Compat = DB
  include Compat

  type field_in = Schema.Value.t

  type field_out = Schema.maybe_cached_value

  let field_of_compat = Schema.CachedValue.of_string

  let compat_of_field = Schema.Value.marshal

  let regular_field_of_compat (k, v) = (k, field_of_compat v)

  let regular_fields_of_compat l = List.map regular_field_of_compat l

  let compat_of_regular_field (k, v) = (k, compat_of_field v)

  let compat_of_regular_fields l = List.map compat_of_regular_field l

  let db_record_of_compat (regular, assoc) =
    (regular_fields_of_compat regular, assoc)

  let db_record_entry_of_compat (ref, record) = (ref, db_record_of_compat record)

  let read_field_where t where =
    read_field_where t where |> List.map field_of_compat

  let create_row t tbl fields ref =
    create_row t tbl (compat_of_regular_fields fields) ref

  let write_field t tbl ref fld field =
    write_field t tbl ref fld (compat_of_field field)

  let read_field t tbl fld ref = read_field t tbl fld ref |> field_of_compat

  let read_record t tbl ref = read_record t tbl ref |> db_record_of_compat

  let read_records_where t tbl expr =
    read_records_where t tbl expr |> List.map db_record_entry_of_compat
end
