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
open Db_exn

type row = (string, string) Hashtbl.t 
type table = (string, row) Hashtbl.t
type cache = (string, table) Hashtbl.t

type where_record = {table:string; return:string; where_field:string; where_value:string}
type structured_op_t = AddSet | RemoveSet | AddMap | RemoveMap

let string_of_structured_op op = match op with
  | AddSet -> "add_set"
  | RemoveSet -> "remove_set"
  | AddMap -> "add_map"
  | RemoveMap -> "remove_map"

type db_dump_manifest =
    {
      pool_token : string;
      schema_major_vsn : int;
      schema_minor_vsn : int;
      product_version : string;
      product_brand : string;
      build_number : string;
      xapi_major_vsn : int;
      xapi_minor_vsn : int;
      generation_count : Generation.t
    }

let gen_manifest gen_count =
  {
    pool_token = Unixext.string_of_file Xapi_globs.pool_secret_path;
    schema_major_vsn = Datamodel.schema_major_vsn;
    schema_minor_vsn = Datamodel.schema_minor_vsn;
    product_version = Version.product_version;
    product_brand = Version.product_brand;
    build_number = Version.build_number;
    xapi_major_vsn = Xapi_globs.version_major;
    xapi_minor_vsn = Xapi_globs.version_minor;
    generation_count = gen_count
  }

(* Our versions of hashtbl.find *)
let lookup_field_in_row row fld =
  try
    Hashtbl.find row fld
  with Not_found -> raise (DBCache_NotFound ("missing field",fld,""))
    
let lookup_table_in_cache cache tbl =
  try
    Hashtbl.find cache tbl
  with Not_found -> raise (DBCache_NotFound ("missing table",tbl,""))

let lookup_row_in_table tbl tblname objref =
  try
    Hashtbl.find tbl objref
  with Not_found ->  raise (DBCache_NotFound ("missing row",tblname,objref))

let iter_over_rows func table =
  Hashtbl.iter func table

let iter_over_tables func cache =
  Hashtbl.iter func cache

let iter_over_fields func row =
  Hashtbl.iter func row
  
let set_field_in_row row fldname newval =
  Hashtbl.replace row fldname newval

let set_row_in_table table objref newval =
  Hashtbl.replace table objref newval

let set_table_in_cache cache tblname newtbl =
  Hashtbl.replace cache tblname newtbl

let create_empty_row () = Hashtbl.create 20

let create_empty_table () = Hashtbl.create 20

let create_empty_cache () = Hashtbl.create 20

let fold_over_fields func row acc = Hashtbl.fold func row acc

let fold_over_rows func table acc = Hashtbl.fold func table acc

let fold_over_tables func cache acc = Hashtbl.fold func cache acc

let remove_row_from_table tbl objref = Hashtbl.remove tbl objref

let get_rowlist tbl =
  fold_over_rows (fun k d env -> d::env) tbl []

let get_reflist tbl =
  fold_over_rows (fun k d env -> k::env) tbl []

(* Find row with specified reference in specified table *)
let find_row cache (tblname:string) (objref:string) : row =
  let tbl = lookup_table_in_cache cache tblname in
  lookup_row_in_table tbl tblname objref

(* Read column, fname, from database rows: *)
let get_column cache tblname fname =
  let rec f rows col_so_far =
    match rows with
      [] -> col_so_far
    | (r::rs) ->
	let value = try Some (lookup_field_in_row r fname) with _ -> None in
	match value with
	  None -> f rs col_so_far
	| (Some u) -> f rs (u::col_so_far) in
  f (get_rowlist (lookup_table_in_cache cache tblname)) []

(** Return a snapshot of the database cache suitable for slow marshalling across the network *)
let snapshot cache : cache = 
  Db_lock.with_lock 
    (fun () ->
       let row table rf vals = 
	 let newrow = create_empty_row () in
	 iter_over_fields (set_field_in_row newrow) vals;
	 set_row_in_table table rf newrow in
       
       let table cache name tbl = 
	 let newtable = create_empty_table () in
	 iter_over_rows (row newtable) tbl;
	 set_table_in_cache cache name newtable in
       
       let newcache = create_empty_cache () in  
       iter_over_tables (table newcache) cache;
       newcache)
