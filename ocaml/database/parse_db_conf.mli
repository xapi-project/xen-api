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

type db_connection_mode = Write_limit | No_limit

type db_connection = {
    path: string
  ; mode: db_connection_mode
  ; compress: bool
  ; write_limit_period: int
  ; write_limit_write_cycles: int
  ; is_on_remote_storage: bool
  ; other_parameters: (string * string) list
  ; mutable last_generation_count: Generation.t
}

val dummy_conf : db_connection

val make : string -> db_connection

val generation_filename : db_connection -> string

val generation_read : db_connection -> Generation.t

val write_db_conf : db_connection list -> unit

exception Cannot_parse_database_config_file

exception Cannot_have_multiple_dbs_in_sr

val parse_db_conf : string -> db_connection list

val get_db_conf : string -> db_connection list
