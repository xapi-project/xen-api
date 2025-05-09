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

exception DB_remote_marshall_error

val marshall_4strings : string * string * string * string -> XMLRPC.xmlrpc

val unmarshall_4strings : XMLRPC.xmlrpc -> string * string * string * string

val marshall_3strings : string * string * string -> XMLRPC.xmlrpc

val unmarshall_3strings : XMLRPC.xmlrpc -> string * string * string

val marshall_get_table_from_ref_args : string -> XMLRPC.xmlrpc

val unmarshall_get_table_from_ref_args : XMLRPC.xmlrpc -> string

val marshall_get_table_from_ref_response : string option -> XMLRPC.xmlrpc

val unmarshall_get_table_from_ref_response : XMLRPC.xmlrpc -> string option

val marshall_is_valid_ref_args : string -> XMLRPC.xmlrpc

val unmarshall_is_valid_ref_args : XMLRPC.xmlrpc -> string

val marshall_is_valid_ref_response : bool -> XMLRPC.xmlrpc

val unmarshall_is_valid_ref_response : XMLRPC.xmlrpc -> bool

val marshall_read_refs_args : string -> XMLRPC.xmlrpc

val unmarshall_read_refs_args : XMLRPC.xmlrpc -> string

val marshall_read_refs_response : string list -> XMLRPC.xmlrpc

val unmarshall_read_refs_response : XMLRPC.xmlrpc -> string list

val marshall_read_field_where_args :
  Db_cache_types.where_record -> XMLRPC.xmlrpc

val unmarshall_read_field_where_args :
  XMLRPC.xmlrpc -> Db_cache_types.where_record

val marshall_read_field_where_response : string list -> XMLRPC.xmlrpc

val unmarshall_read_field_where_response : XMLRPC.xmlrpc -> string list

val marshall_db_get_by_uuid_args : string * string -> XMLRPC.xmlrpc

val unmarshall_db_get_by_uuid_args : XMLRPC.xmlrpc -> string * string

val marshall_db_get_by_uuid_response : string -> XMLRPC.xmlrpc

val marshall_db_get_by_uuid_opt_response : string option -> XMLRPC.xmlrpc

val unmarshall_db_get_by_uuid_response : XMLRPC.xmlrpc -> string

val unmarshall_db_get_by_uuid_opt_response : XMLRPC.xmlrpc -> string option

val marshall_db_get_by_name_label_args : string * string -> XMLRPC.xmlrpc

val unmarshall_db_get_by_name_label_args : XMLRPC.xmlrpc -> string * string

val marshall_db_get_by_name_label_response : string list -> XMLRPC.xmlrpc

val unmarshall_db_get_by_name_label_response : XMLRPC.xmlrpc -> string list

val marshall_create_row_args :
  string * (string * string) list * string -> XMLRPC.xmlrpc

val unmarshall_create_row_args :
  XMLRPC.xmlrpc -> string * (string * string) list * string

val marshall_create_row_response : unit -> XMLRPC.xmlrpc

val unmarshall_create_row_response : XMLRPC.xmlrpc -> unit

val marshall_delete_row_args : string * string -> XMLRPC.xmlrpc

val unmarshall_delete_row_args : XMLRPC.xmlrpc -> string * string

val marshall_delete_row_response : unit -> XMLRPC.xmlrpc

val unmarshall_delete_row_response : XMLRPC.xmlrpc -> unit

val marshall_write_field_args :
  string * string * string * string -> XMLRPC.xmlrpc

val unmarshall_write_field_args :
  XMLRPC.xmlrpc -> string * string * string * string

val marshall_write_field_response : unit -> XMLRPC.xmlrpc

val unmarshall_write_field_response : XMLRPC.xmlrpc -> unit

val marshall_read_field_args : string * string * string -> XMLRPC.xmlrpc

val unmarshall_read_field_args : XMLRPC.xmlrpc -> string * string * string

val marshall_read_field_response : string -> XMLRPC.xmlrpc

val unmarshall_read_field_response : XMLRPC.xmlrpc -> string

val marshall_find_refs_with_filter_args :
  string * Db_filter_types.expr -> XMLRPC.xmlrpc

val unmarshall_find_refs_with_filter_args :
  XMLRPC.xmlrpc -> string * Db_filter_types.expr

val marshall_find_refs_with_filter_response : string list -> XMLRPC.xmlrpc

val unmarshall_find_refs_with_filter_response : XMLRPC.xmlrpc -> string list

val marshall_process_structured_field_args :
     (string * string)
     * string
     * string
     * string
     * Db_cache_types.structured_op_t
  -> XMLRPC.xmlrpc

val unmarshall_process_structured_field_args :
     XMLRPC.xmlrpc
  -> (string * string)
     * string
     * string
     * string
     * Db_cache_types.structured_op_t

val marshall_process_structured_field_response : unit -> XMLRPC.xmlrpc

val unmarshall_process_structured_field_response : XMLRPC.xmlrpc -> unit

val marshall_read_record_args : string * string -> XMLRPC.xmlrpc

val unmarshall_read_record_args : XMLRPC.xmlrpc -> string * string

val marshall_read_record_response :
  (string * string) list * (string * string list) list -> XMLRPC.xmlrpc

val unmarshall_read_record_response :
  XMLRPC.xmlrpc -> (string * string) list * (string * string list) list

val marshall_read_records_where_args :
  string * Db_filter_types.expr -> XMLRPC.xmlrpc

val unmarshall_read_records_where_args :
  XMLRPC.xmlrpc -> string * Db_filter_types.expr

val marshall_read_records_where_response :
     (string * ((string * string) list * (string * string list) list)) list
  -> XMLRPC.xmlrpc

val unmarshall_read_records_where_response :
     XMLRPC.xmlrpc
  -> (string * ((string * string) list * (string * string list) list)) list

val marshall_stringstringlist : (string * string) list -> Xml.xml

val unmarshall_stringstringlist : Xml.xml -> (string * string) list

val marshall_structured_op : Db_cache_types.structured_op_t -> Xml.xml

val unmarshall_structured_op : Xml.xml -> Db_cache_types.structured_op_t
