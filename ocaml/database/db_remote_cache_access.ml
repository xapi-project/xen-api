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

module DBCacheRemoteListener =
struct
  open Db_remote_marshall
  open Db_action_helper
  open Db_cache
  open Db_exn

  exception DBCacheListenerInvalidMessageReceived
  exception DBCacheListenerUnknownMessageName of string

  module D = Debug.Debugger(struct let name = "db_server" end)
  open D

  let ctr_mutex = Mutex.create()
  let calls_processed = ref 0
  let total_recv_len = ref 0
  let total_transmit_len = ref 0
 
  (* Performance counters for debugging *)
  let update_lengths msg resp =
      Mutex.lock ctr_mutex;
      total_transmit_len := (!total_transmit_len) + (String.length (Xml.to_string_fmt resp));
      total_recv_len := (!total_recv_len) + (String.length (Xml.to_string_fmt msg));
      Mutex.unlock ctr_mutex

  let success xml =
    let resp = 
      XMLRPC.To.array
	[XMLRPC.To.string "success";
	 xml] in
      (* update_lengths xml resp; *)
      (* let s = Xml.to_string_fmt resp  in *)
	(* debug "Resp [Len = %d]: %s" (String.length s) s; *)
	debug "Call succeeded";
	resp

  let failure exn_name xml =
    let resp =
      XMLRPC.To.array
	[XMLRPC.To.string "failure";
	 XMLRPC.To.array
	   [XMLRPC.To.string exn_name;
	    xml]] in
      (* update_lengths xml resp; *)
      debug "Call failed";
      resp

  (** Unmarshals the request, calls the DBCache function and marshals the result.
      Note that, although the messages still contain the pool_secret for historical reasons,
      access has already been applied by the RBAC code in Xapi_http.add_handler. *)
  let process_xmlrpc xml =
    Mutex.lock ctr_mutex;
    calls_processed := !calls_processed + 1;
    Mutex.unlock ctr_mutex;
    let fn_name, args =
      match (XMLRPC.From.array (fun x->x) xml) with
	  [fn_name; _; args] ->
	    XMLRPC.From.string fn_name, args
	| _ -> raise DBCacheListenerInvalidMessageReceived in
      try
	debug "Received [total=%d rx=%d tx=%d] %s" !calls_processed !total_recv_len !total_transmit_len fn_name;
	match fn_name with
	    "get_table_from_ref" ->
	      let s = unmarshall_get_table_from_ref_args args in
		success (marshall_get_table_from_ref_response (DBCache.get_table_from_ref s))
	  | "is_valid_ref" ->
	      let s = unmarshall_is_valid_ref_args args in
		success (marshall_is_valid_ref_response (DBCache.is_valid_ref s))
	  | "read_refs" ->
	      let s = unmarshall_read_refs_args args in
		success (marshall_read_refs_response (DBCache.read_refs s))
	  | "read_field_where" ->
	      let w = unmarshall_read_field_where_args args in
		success (marshall_read_field_where_response (DBCache.read_field_where w))
	  | "read_set_ref" ->
	      let w = unmarshall_read_set_ref_args args in
		success (marshall_read_set_ref_response (DBCache.read_field_where w))
	  | "create_row" ->
	      let (s1,ssl,s2) = unmarshall_create_row_args args in
		success (marshall_create_row_response (DBCache.create_row Context.initial s1 ssl s2))
	  | "delete_row" ->
	      let (s1,s2) = unmarshall_delete_row_args args in
		success (marshall_delete_row_response (DBCache.delete_row Context.initial s1 s2))
	  | "write_field" ->
	      let (s1,s2,s3,s4) = unmarshall_write_field_args args in
		success (marshall_write_field_response (DBCache.write_field Context.initial s1 s2 s3 s4))
	  | "read_field" ->
	      let (s1,s2,s3) = unmarshall_read_field_args args in
		success (marshall_read_field_response (DBCache.read_field Context.initial s1 s2 s3))
	  | "find_refs_with_filter" ->
	      let (s,e) = unmarshall_find_refs_with_filter_args args in
		success (marshall_find_refs_with_filter_response (DBCache.find_refs_with_filter s e))
	  | "process_structured_field" ->
	      let (ss,s1,s2,s3,op) = unmarshall_process_structured_field_args args in
		success (marshall_process_structured_field_response (DBCache.process_structured_field Context.initial ss s1 s2 s3 op))
	  | "read_record" ->
	      let (s1,s2) = unmarshall_read_record_args args in
		success (marshall_read_record_response (DBCache.read_record s1 s2))
	  | "read_records_where" ->
	      let (s,e) = unmarshall_read_records_where_args args in
		success (marshall_read_records_where_response (DBCache.read_records_where s e))
	  | "db_get_by_uuid" ->
	      let (s,e) = unmarshall_db_get_by_uuid_args args in
		success (marshall_db_get_by_uuid_response (DBCache.db_get_by_uuid s e))
	  | "db_get_by_name_label" ->
	      let (s,e) = unmarshall_db_get_by_name_label_args args in
		success (marshall_db_get_by_name_label_response (DBCache.db_get_by_name_label s e))
	  | _ -> raise (DBCacheListenerUnknownMessageName fn_name)
      with
	  Duplicate_key (c,f,u,k) ->
	    failure "duplicate_key_of" (marshall_4strings (c,f,u,k))
	| DBCache_NotFound (s1,s2,s3) ->
	    failure "dbcache_notfound" (marshall_3strings (s1,s2,s3))
	| Uniqueness_constraint_violation (s1,s2,s3) ->
	    failure "uniqueness_constraint_violation" (marshall_3strings (s1,s2,s3))
	| Read_missing_uuid (s1,s2,s3) ->
	    failure "read_missing_uuid" (marshall_3strings (s1,s2,s3))
	| Too_many_values (s1,s2,s3) ->
	    failure "too_many_values" (marshall_3strings (s1,s2,s3))
	| e -> raise e
end
