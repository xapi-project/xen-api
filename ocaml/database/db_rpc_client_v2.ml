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

(** client-side for remote database access protocol v2 *)

open Db_rpc_common_v2
open Db_exn

module Make = functor(RPC: Db_interface.RPC) -> struct
  let initialise = RPC.initialise
  let rpc x =
    match RPC.rpc (Jsonrpc.to_string x) with
    | Db_interface.String s -> Jsonrpc.of_string s
    | Db_interface.Bigbuf b -> raise (Failure "Response too large - cannot convert bigbuffer to json!")

  let process (x: Request.t) =
    let y : Response.t = Response.t_of_rpc (rpc (Request.rpc_of_t x)) in
    match y with
    | Response.Dbcache_notfound (x, y, z) ->
      raise (DBCache_NotFound (x,y,z))
    | Response.Duplicate_key_of (w, x, y, z) ->
      raise (Duplicate_key (w,x,y,z))
    | Response.Uniqueness_constraint_violation (x, y, z) ->
      raise (Uniqueness_constraint_violation (x,y,z))
    | Response.Read_missing_uuid (x, y, z) ->
      raise (Read_missing_uuid (x,y,z))
    | Response.Too_many_values (x, y, z) ->
      raise (Too_many_values (x,y,z))
    | y -> y

  let get_table_from_ref _ x =
    match process (Request.Get_table_from_ref x) with
    | Response.Get_table_from_ref y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let is_valid_ref _ x =
    match process (Request.Is_valid_ref x) with
    | Response.Is_valid_ref y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let read_refs _ x =
    match process (Request.Read_refs x) with
    | Response.Read_refs y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let read_field_where _ x =
    match process (Request.Read_field_where x) with
    | Response.Read_field_where y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let db_get_by_uuid _ t u =
    match process (Request.Db_get_by_uuid (t, u)) with
    | Response.Db_get_by_uuid y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let db_get_by_name_label _ t l =
    match process (Request.Db_get_by_name_label (t, l)) with
    | Response.Db_get_by_name_label y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let create_row _ x y z =
    match process (Request.Create_row (x, y, z)) with
    | Response.Create_row y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let delete_row _ x y =
    match process (Request.Delete_row (x, y)) with
    | Response.Delete_row y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let write_field _ a b c d =
    match process (Request.Write_field (a, b, c, d)) with
    | Response.Write_field y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let read_field _ x y z =
    match process (Request.Read_field (x, y, z)) with
    | Response.Read_field y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let find_refs_with_filter _ s e =
    match process (Request.Find_refs_with_filter (s, e)) with
    | Response.Find_refs_with_filter y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let read_record _ x y =
    match process (Request.Read_record (x, y)) with
    | Response.Read_record (x, y) -> x, y
    | _ -> raise Remote_db_server_returned_bad_message

  let read_records_where _ x e =
    match process (Request.Read_records_where (x, e)) with
    | Response.Read_records_where y -> y
    | _ -> raise Remote_db_server_returned_bad_message

  let process_structured_field _ a b c d e =
    match process (Request.Process_structured_field(a, b, c, d, e)) with
    | Response.Process_structured_field y -> y
    | _ -> raise Remote_db_server_returned_bad_message
end
