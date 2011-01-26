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
type cache = {
	cache: (string, table) Hashtbl.t;
	schema: (int * int) option ref;
	generation: Generation.t ref;
}

type where_record = {table:string; return:string; where_field:string; where_value:string} with rpc
type structured_op_t = AddSet | RemoveSet | AddMap | RemoveMap with rpc

let string_of_structured_op op = match op with
  | AddSet -> "add_set"
  | RemoveSet -> "remove_set"
  | AddMap -> "add_map"
  | RemoveMap -> "remove_map"

type db_dump_manifest =
    {
      schema_major_vsn : int;
      schema_minor_vsn : int;
      generation_count : Generation.t
    }

let make_manifest schema_major_vsn schema_minor_vsn gen_count =
  {
    schema_major_vsn = schema_major_vsn;
    schema_minor_vsn = schema_minor_vsn;
    generation_count = gen_count
  }

let schema_of_cache cache = match !(cache.schema) with
| None -> (0, 0)
| Some (major, minor) -> major, minor

let manifest_of_cache cache = 
	let major, minor = schema_of_cache cache in
	make_manifest major minor !(cache.generation)

let set_schema_vsn cache (major, minor) = cache.schema := Some (major, minor)

let increment cache = cache.generation := Int64.add !(cache.generation) 1L

let generation_of_cache cache = !(cache.generation)

let set_generation cache generation = cache.generation := generation

(* Our versions of hashtbl.find *)
let lookup_field_in_row row fld =
  try
    Hashtbl.find row fld
  with Not_found -> raise (DBCache_NotFound ("missing field",fld,""))
    
let lookup_table_in_cache cache tbl =
  try
    Hashtbl.find cache.cache tbl
  with Not_found -> raise (DBCache_NotFound ("missing table",tbl,""))

let lookup_row_in_table tbl tblname objref =
  try
    Hashtbl.find tbl objref
  with Not_found ->  raise (DBCache_NotFound ("missing row",tblname,objref))

let iter_over_rows func table =
  Hashtbl.iter func table

let iter_over_tables func cache =
  Hashtbl.iter func cache.cache

let iter_over_fields func row =
  Hashtbl.iter func row
  
let set_field_in_row row fldname newval =
  Hashtbl.replace row fldname newval

let set_row_in_table table objref newval =
  Hashtbl.replace table objref newval

let set_table_in_cache cache tblname newtbl =
  Hashtbl.replace cache.cache tblname newtbl

let create_empty_row () = Hashtbl.create 20

let create_empty_table () = Hashtbl.create 20

let create_empty_cache () = { cache = Hashtbl.create 20; schema = ref None; generation = ref Generation.null_generation }

let fold_over_fields func row acc = Hashtbl.fold func row acc

let fold_over_rows func table acc = Hashtbl.fold func table acc

let fold_over_tables func cache acc = Hashtbl.fold func cache.cache acc

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

	   set_generation newcache (generation_of_cache cache);
       newcache)
