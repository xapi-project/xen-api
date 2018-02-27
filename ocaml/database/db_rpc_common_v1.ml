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

(** Marshall/unmarshall functions for relevant types to XMLRPC *)

open Db_cache_types

exception DB_remote_marshall_error

let marshall_2strings (x,y) =
  XMLRPC.To.array [XMLRPC.To.string x; XMLRPC.To.string y]
let unmarshall_2strings xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [x1;x2] ->
    (XMLRPC.From.string x1,
     XMLRPC.From.string x2)
  | _ -> raise DB_remote_marshall_error

let marshall_4strings (x,y,w,z) =
  XMLRPC.To.array [XMLRPC.To.string x; XMLRPC.To.string y; XMLRPC.To.string w; XMLRPC.To.string z]
let unmarshall_4strings xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [x1;x2;x3;x4] ->
    (XMLRPC.From.string x1,
     XMLRPC.From.string x2,
     XMLRPC.From.string x3,
     XMLRPC.From.string x4)
  | _ -> raise DB_remote_marshall_error

let marshall_3strings (x,y,w) =
  XMLRPC.To.array [XMLRPC.To.string x; XMLRPC.To.string y; XMLRPC.To.string w]
let unmarshall_3strings xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [x1;x2;x3] ->
    (XMLRPC.From.string x1,
     XMLRPC.From.string x2,
     XMLRPC.From.string x3)
  | _ -> raise DB_remote_marshall_error

let marshall_stringlist sl =
  XMLRPC.To.array (List.map XMLRPC.To.string sl)
let unmarshall_stringlist xml =
  List.map XMLRPC.From.string (XMLRPC.From.array (fun x->x) xml)

let marshall_stringstringlist ssl =
  XMLRPC.To.array (List.map marshall_2strings ssl)
let unmarshall_stringstringlist xml =
  List.map unmarshall_2strings (XMLRPC.From.array (fun x->x) xml)

let marshall_stringopt x =
  match x with
    None -> XMLRPC.To.array []
  | (Some x) -> XMLRPC.To.array [XMLRPC.To.string x]
let unmarshall_stringopt xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [] -> None
  | [xml] -> Some (XMLRPC.From.string xml)
  | _ -> raise DB_remote_marshall_error

let marshall_expr x =
  Db_filter.xml_of_expr x
let unmarshall_expr xml =
  Db_filter.expr_of_xml xml

let marshall_structured_op x =
  let str =
    match x with
      AddSet -> "addset"
    | RemoveSet -> "removeset"
    | AddMap -> "addmap"
    | RemoveMap -> "removemap"
    | AddMapLegacy -> "addmap" (* Nb, we always use 'non-legacy' mode for remote access *)
  in
  XMLRPC.To.string str
let unmarshall_structured_op xml =
  match (XMLRPC.From.string xml) with
    "addset" -> AddSet
  | "removeset" -> RemoveSet
  | "addmap" -> AddMap
  | "removemap" -> RemoveMap
  | _ -> raise DB_remote_marshall_error

let marshall_where_rec r =
  XMLRPC.To.array [XMLRPC.To.string r.table;
                   XMLRPC.To.string r.return;
                   XMLRPC.To.string r.where_field;
                   XMLRPC.To.string r.where_value]
let unmarshall_where_rec xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [t;r;wf;wv] ->
    {table=XMLRPC.From.string t; return=XMLRPC.From.string r;
     where_field=XMLRPC.From.string wf; where_value=XMLRPC.From.string wv}
  | _ -> raise DB_remote_marshall_error

let marshall_unit () =
  XMLRPC.To.string ""
let unmarshall_unit xml =
  match XMLRPC.From.string xml with
    "" -> ()
  | _ -> raise DB_remote_marshall_error

(* get_table_from_ref *)
let marshall_get_table_from_ref_args s = XMLRPC.To.string s
let unmarshall_get_table_from_ref_args xml = XMLRPC.From.string xml
let marshall_get_table_from_ref_response so = marshall_stringopt so
let unmarshall_get_table_from_ref_response so = unmarshall_stringopt so

(* is_valid_ref *)
let marshall_is_valid_ref_args s = XMLRPC.To.string s
let unmarshall_is_valid_ref_args xml = XMLRPC.From.string xml
let marshall_is_valid_ref_response b = XMLRPC.To.boolean b
let unmarshall_is_valid_ref_response xml = XMLRPC.From.boolean xml

(* read_refs *)
let marshall_read_refs_args s = XMLRPC.To.string s
let unmarshall_read_refs_args s = XMLRPC.From.string s
let marshall_read_refs_response sl = marshall_stringlist sl
let unmarshall_read_refs_response xml = unmarshall_stringlist xml

(* read_field_where *)
let marshall_read_field_where_args w = marshall_where_rec w
let unmarshall_read_field_where_args xml = unmarshall_where_rec xml
let marshall_read_field_where_response sl =
  marshall_stringlist sl
let unmarshall_read_field_where_response xml =
  unmarshall_stringlist xml

(* db_get_by_uuid *)
let marshall_db_get_by_uuid_args (s1,s2) =
  marshall_2strings (s1,s2)
let unmarshall_db_get_by_uuid_args xml =
  unmarshall_2strings xml
let marshall_db_get_by_uuid_response s =
  XMLRPC.To.string s
let unmarshall_db_get_by_uuid_response xml =
  XMLRPC.From.string xml

(* db_get_by_name_label *)
let marshall_db_get_by_name_label_args (s1,s2) =
  marshall_2strings (s1,s2)
let unmarshall_db_get_by_name_label_args xml =
  unmarshall_2strings xml
let marshall_db_get_by_name_label_response sl =
  marshall_stringlist sl
let unmarshall_db_get_by_name_label_response xml =
  unmarshall_stringlist xml

(* create_row *)
let marshall_create_row_args (s1,ssl,s2) =
  XMLRPC.To.array
    [XMLRPC.To.string s1;
     XMLRPC.To.array (List.map marshall_2strings ssl);
     XMLRPC.To.string s2]
let unmarshall_create_row_args xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [s1_xml; ssl_xml; s2_xml] ->
    (XMLRPC.From.string s1_xml,
     List.map unmarshall_2strings (XMLRPC.From.array (fun x->x) ssl_xml),
     XMLRPC.From.string s2_xml)
  | _ -> raise DB_remote_marshall_error
let marshall_create_row_response () =
  marshall_unit ()
let unmarshall_create_row_response xml =
  unmarshall_unit xml

(* delete_row *)
let marshall_delete_row_args (s1,s2) =
  XMLRPC.To.array
    [XMLRPC.To.string s1;
     XMLRPC.To.string s2]
let unmarshall_delete_row_args xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [s1_xml; s2_xml] ->
    (XMLRPC.From.string s1_xml, XMLRPC.From.string s2_xml)
  | _ -> raise DB_remote_marshall_error
let marshall_delete_row_response () =
  marshall_unit ()
let unmarshall_delete_row_response xml =
  unmarshall_unit xml

(* write_field *)
let marshall_write_field_args (s1,s2,s3,s4) =
  XMLRPC.To.array
    (List.map XMLRPC.To.string [s1;s2;s3;s4])
let unmarshall_write_field_args xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [s1x;s2x;s3x;s4x] ->
    (XMLRPC.From.string s1x, XMLRPC.From.string s2x,
     XMLRPC.From.string s3x, XMLRPC.From.string s4x)
  | _ -> raise DB_remote_marshall_error
let marshall_write_field_response () =
  marshall_unit ()
let unmarshall_write_field_response xml =
  unmarshall_unit xml

(* read_field *)
let marshall_read_field_args (s1,s2,s3) =
  XMLRPC.To.array
    (List.map XMLRPC.To.string [s1;s2;s3])
let unmarshall_read_field_args xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [s1x;s2x;s3x] ->
    (XMLRPC.From.string s1x, XMLRPC.From.string s2x,
     XMLRPC.From.string s3x)
  | _ -> raise DB_remote_marshall_error
let marshall_read_field_response s =
  XMLRPC.To.string s
let unmarshall_read_field_response xml =
  XMLRPC.From.string xml

(* find_refs_with_filter *)
let marshall_find_refs_with_filter_args (s,e) =
  XMLRPC.To.array
    [XMLRPC.To.string s; marshall_expr e]
let unmarshall_find_refs_with_filter_args xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [s;e] -> (XMLRPC.From.string s, unmarshall_expr e)
  | _ -> raise DB_remote_marshall_error
let marshall_find_refs_with_filter_response sl =
  marshall_stringlist sl
let unmarshall_find_refs_with_filter_response xml =
  unmarshall_stringlist xml

(* process_structured_field *)
let marshall_process_structured_field_args (ss,s1,s2,s3,op) =
  XMLRPC.To.array
    [marshall_2strings ss;
     XMLRPC.To.string s1;
     XMLRPC.To.string s2;
     XMLRPC.To.string s3;
     marshall_structured_op op]
let unmarshall_process_structured_field_args xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [ss_xml;s1_xml;s2_xml;s3_xml;op_xml] ->
    (unmarshall_2strings ss_xml,
     XMLRPC.From.string s1_xml,
     XMLRPC.From.string s2_xml,
     XMLRPC.From.string s3_xml,
     unmarshall_structured_op op_xml)
  | _ -> raise DB_remote_marshall_error
let marshall_process_structured_field_response () =
  marshall_unit ()
let unmarshall_process_structured_field_response xml =
  unmarshall_unit xml

(* read_record *)
let marshall_read_record_args = marshall_2strings
let unmarshall_read_record_args = unmarshall_2strings
let marshall_read_record_response (ssl, ssll) =
  XMLRPC.To.array
    [XMLRPC.To.array (List.map marshall_2strings ssl);
     XMLRPC.To.array
       (List.map
          (fun (s,sl) ->
             XMLRPC.To.array [XMLRPC.To.string s;
                              XMLRPC.To.array (List.map XMLRPC.To.string sl)]) ssll)]
let unmarshall_read_record_response xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [ssl_xml; ssll_xml] ->
    (List.map unmarshall_2strings (XMLRPC.From.array (fun x->x) ssl_xml),
     List.map
       (fun xml ->
          match XMLRPC.From.array (fun x->x) xml with
            [s_xml; sl_xml] -> (XMLRPC.From.string s_xml, unmarshall_stringlist sl_xml)
          | _ -> raise DB_remote_marshall_error)
       (XMLRPC.From.array (fun x->x) ssll_xml))
  | _ -> raise DB_remote_marshall_error

(* read_records_where *)
let marshall_read_records_where_args (s,e) =
  XMLRPC.To.array
    [XMLRPC.To.string s; marshall_expr e]
let unmarshall_read_records_where_args xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    [s_xml; expr_xml] ->
    (XMLRPC.From.string s_xml,
     unmarshall_expr expr_xml)
  | _ -> raise DB_remote_marshall_error

let marshall_read_records_where_response refs_and_recs_list =
  XMLRPC.To.array
    (List.map
       (fun (ref,record)->
          XMLRPC.To.array
            [XMLRPC.To.string ref;
             marshall_read_record_response record]) refs_and_recs_list)
let unmarshall_read_records_where_response xml =
  match (XMLRPC.From.array (fun x->x) xml) with
    xml_refs_and_recs_list ->
    List.map
      (fun xml_ref_and_rec ->
         match (XMLRPC.From.array (fun x->x) xml_ref_and_rec) with
           [ref_xml; rec_xml] -> (XMLRPC.From.string ref_xml, unmarshall_read_record_response rec_xml)
         | _ -> raise DB_remote_marshall_error)
      xml_refs_and_recs_list
