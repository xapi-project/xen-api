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

(** server-side for remote database access protocol v2 *)

open Db_rpc_common_v2
open Db_exn

(** Convert a marshalled Request Rpc.t into a marshalled Response Rpc.t *)
let process_rpc (req: Rpc.t) =
  let module DB = (Db_cache_impl : Db_interface.DB_ACCESS) in
  let t = Db_backend.make () in
  Response.rpc_of_t
    (try
       match Request.t_of_rpc req with
       | Request.Get_table_from_ref x ->
         Response.Get_table_from_ref (DB.get_table_from_ref t x)
       | Request.Is_valid_ref x ->
         Response.Is_valid_ref (DB.is_valid_ref t x)
       | Request.Read_refs x ->
         Response.Read_refs (DB.read_refs t x)
       | Request.Find_refs_with_filter (x, e) ->
         Response.Find_refs_with_filter (DB.find_refs_with_filter t x e)
       | Request.Read_field_where w ->
         Response.Read_field_where (DB.read_field_where t w)
       | Request.Db_get_by_uuid (a, b) ->
         Response.Db_get_by_uuid (DB.db_get_by_uuid t a b)
       | Request.Db_get_by_name_label (a, b) ->
         Response.Db_get_by_name_label (DB.db_get_by_name_label t a b)
       | Request.Create_row (a, b, c) ->
         Response.Create_row (DB.create_row t a b c)
       | Request.Delete_row (a, b) ->
         Response.Delete_row (DB.delete_row t a b)
       | Request.Write_field (a, b, c, d) ->
         Response.Write_field (DB.write_field t a b c d)
       | Request.Read_field (a, b, c) ->
         Response.Read_field (DB.read_field t a b c)
       | Request.Read_record (a, b) ->
         let a', b' = DB.read_record t a b in
         Response.Read_record (a', b')
       | Request.Read_records_where (a, b) ->
         Response.Read_records_where (DB.read_records_where t a b)
       | Request.Process_structured_field (a, b, c, d, e) ->
         Response.Process_structured_field (DB.process_structured_field t a b c d e)
     with
     | DBCache_NotFound (x,y,z) ->
       Response.Dbcache_notfound (x, y, z)
     | Duplicate_key (w,x,y,z) ->
       Response.Duplicate_key_of (w, x, y, z)
     | Uniqueness_constraint_violation (x,y,z) ->
       Response.Uniqueness_constraint_violation (x, y, z)
     | Read_missing_uuid (x,y,z) ->
       Response.Read_missing_uuid (x, y, z)
     | Too_many_values (x,y,z) ->
       Response.Too_many_values (x, y, z)

    )

let handler req bio _ =
  let fd = Buf_io.fd_of bio in (* fd only used for writing *)
  let body = Http_svr.read_body ~limit:Db_globs.http_limit_max_rpc_size req bio in
  let request_rpc = Jsonrpc.of_string body in
  let reply_rpc = process_rpc request_rpc in
  (* XXX: need to cope with > 16MiB responses *)
  let response = Jsonrpc.to_string reply_rpc in
  Http_svr.response_str req fd response
