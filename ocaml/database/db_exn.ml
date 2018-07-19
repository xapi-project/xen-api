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

exception Duplicate_key of (*class*) string * (*field*) string * (*uuid*) string * (*key*) string
exception DBCache_NotFound of string*string*string
exception Uniqueness_constraint_violation of string*string*string

exception Read_missing_uuid of (*class*) string * (*ref*) string * (*uuid*) string
exception Too_many_values of   (*class*) string * (*ref*) string * (*uuid*) string

exception Remote_db_server_returned_unknown_exception
exception Remote_db_server_returned_bad_message

exception Empty_key_in_map
exception Invalid_value (* Non utf8 *)
