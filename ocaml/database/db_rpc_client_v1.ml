(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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

open Db_rpc_common_v1
open Db_exn

module Make = functor(RPC: Db_interface.RPC) -> struct
  exception Remote_db_server_returned_unknown_exception

  (* Process an exception returned from server, throwing local exception *)
  let process_exception_xml xml =
    match XMLRPC.From.array (fun x->x) xml with
      [exn_name_xml; exn_params_xml] ->
      let exn_name = XMLRPC.From.string exn_name_xml in
      begin
        match exn_name with
        | "dbcache_notfound" ->
          let (x,y,z) = unmarshall_3strings exn_params_xml in
          raise (DBCache_NotFound (x,y,z))
        | "duplicate_key_of" ->
          let (w,x,y,z) = unmarshall_4strings exn_params_xml in
          raise (Duplicate_key (w,x,y,z))
        | "uniqueness_constraint_violation" ->
          let (x,y,z) = unmarshall_3strings exn_params_xml in
          raise (Uniqueness_constraint_violation (x,y,z))
        | "read_missing_uuid" ->
          let (x,y,z) = unmarshall_3strings exn_params_xml in
          raise (Read_missing_uuid (x,y,z))
        | "too_many_values" ->
          let (x,y,z) = unmarshall_3strings exn_params_xml in
          raise (Too_many_values (x,y,z))
        | _ -> raise DB_remote_marshall_error
      end
    | _ -> raise Remote_db_server_returned_unknown_exception


  exception Remote_db_server_returned_bad_message
  let do_remote_call marshall_args unmarshall_resp fn_name args =
    let xml = marshall_args args in
    let xml = XMLRPC.To.array [XMLRPC.To.string fn_name; XMLRPC.To.string "" (* unused *); xml] in
    let resp = 
      let Db_interface.String res = RPC.rpc (Xml.to_string xml)
      in Xml.parse_string res
    in
    match XMLRPC.From.array (fun x->x) resp with
      [status_xml; resp_xml] ->
      let status = XMLRPC.From.string status_xml in
      if status="success" then unmarshall_resp resp_xml
      else process_exception_xml resp_xml
    | _ -> raise Remote_db_server_returned_bad_message

  let get_table_from_ref _ x =
    do_remote_call
      marshall_get_table_from_ref_args
      unmarshall_get_table_from_ref_response
      "get_table_from_ref"
      x

  let is_valid_ref _ x =
    do_remote_call
      marshall_is_valid_ref_args
      unmarshall_is_valid_ref_response
      "is_valid_ref"
      x

  let read_refs _ x =
    do_remote_call
      marshall_read_refs_args
      unmarshall_read_refs_response
      "read_refs"
      x

  let read_field_where _ x =
    do_remote_call
      marshall_read_field_where_args
      unmarshall_read_field_where_response
      "read_field_where"
      x


  let db_get_by_uuid _ t u =
    do_remote_call
      marshall_db_get_by_uuid_args
      unmarshall_db_get_by_uuid_response
      "db_get_by_uuid"
      (t,u)

  let db_get_by_name_label _ t l =
    do_remote_call
      marshall_db_get_by_name_label_args
      unmarshall_db_get_by_name_label_response
      "db_get_by_name_label"
      (t,l)

  let create_row _ x y z =
    do_remote_call
      marshall_create_row_args
      unmarshall_create_row_response
      "create_row"
      (x,y,z)

  let delete_row _ x y =
    do_remote_call
      marshall_delete_row_args
      unmarshall_delete_row_response
      "delete_row"
      (x,y)

  let write_field _ a b c d =
    do_remote_call
      marshall_write_field_args
      unmarshall_write_field_response
      "write_field"
      (a,b,c,d)

  let read_field _ x y z =
    do_remote_call
      marshall_read_field_args
      unmarshall_read_field_response
      "read_field"
      (x,y,z)

  let find_refs_with_filter _ s e =
    do_remote_call
      marshall_find_refs_with_filter_args
      unmarshall_find_refs_with_filter_response
      "find_refs_with_filter"
      (s,e)

  let read_record _ x y =
    do_remote_call
      marshall_read_record_args
      unmarshall_read_record_response
      "read_record"
      (x,y)

  let read_records_where _ x e =
    do_remote_call
      marshall_read_records_where_args
      unmarshall_read_records_where_response
      "read_records_where"
      (x,e)

  let process_structured_field _ a b c d e =
    do_remote_call
      marshall_process_structured_field_args
      unmarshall_process_structured_field_response
      "process_structured_field"
      (a,b,c,d,e)

  let initialise = RPC.initialise

end
