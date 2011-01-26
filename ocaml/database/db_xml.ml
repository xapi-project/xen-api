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
open Db_cache_types
open Pervasiveext
module R = Debug.Debugger(struct let name = "redo_log" end)

(** Functions to marshall/unmarshall the database as XML *)

exception Unmarshall_error of string

let name x = ("", x) (* no namespace *)
let make_tag n attrs : Xmlm.tag = (name n), List.map (fun (k, v) -> name k, v) attrs

let _schema_major_vsn = "schema_major_vsn"
let _schema_minor_vsn = "schema_minor_vsn"
let _product_brand = "product_brand"
let _build_number = "build_number"
let _xapi_major_vsn = "xapi_major_vsn"
let _xapi_minor_vsn = "xapi_minor_vsn"
let _generation_count = "generation_count"

module To = struct

  (* Write out a key/value pair *)
  let pair (output: Xmlm.output) (key: string) (v: string) = 
    Xmlm.output output (`El_start (make_tag "pair" [ "key", key; "value", v ]));
    Xmlm.output output `El_end 
  (* Write out a string *)
  let string (output: Xmlm.output) (key: string) (x: string) = pair output key x
  (* Write out an int *)
  let int (output: Xmlm.output) (key: string) (x: int) = pair output key (string_of_int x)
  (* Write out an int64 *)
  let int64 (output: Xmlm.output) (key: string) (x: Int64.t) = pair output key (Int64.to_string x)

  (* Marshal a whole database table to an Xmlm output abstraction *)
  let table (output: Xmlm.output) name (tbl: table) = 
    let record rf (row: row) = 
      let (tag: Xmlm.tag) = make_tag "row" (("ref", rf) :: (fold_over_fields (fun k v acc -> (k, Xml_spaces.protect v) :: acc) row [])) in
      Xmlm.output output (`El_start tag);
      Xmlm.output output `El_end in
    let tag = make_tag "table" [ "name", name ] in
    Xmlm.output output (`El_start tag);
    (* we write a table entry whether or not the table persists, because populate happens to assume
       that all tables will be present. However, if the table is marked as "don't persist" then we
       don't write any row entries: *)
    if Db_backend.this_table_persists name then
      iter_over_rows record tbl;
    Xmlm.output output `El_end
	
  (* Write out a manifest *)
  let manifest (output: Xmlm.output) (manifest: db_dump_manifest) : unit = 
    Xmlm.output output (`El_start (make_tag "manifest" []));
    int    output _schema_major_vsn manifest.schema_major_vsn;
    int    output _schema_minor_vsn manifest.schema_minor_vsn;
    string output _product_brand manifest.product_brand;
    string output _build_number manifest.build_number;
    int    output _xapi_major_vsn manifest.xapi_major_vsn;
    int    output _xapi_minor_vsn manifest.xapi_minor_vsn;
    int64  output _generation_count manifest.generation_count;
    Xmlm.output output `El_end

  (* Write out a full database cache dump *)
  let cache (output: Xmlm.output) (m, cache) : unit =
    Xmlm.output output (`Dtd None);	
    Xmlm.output output (`El_start (make_tag "database" []));
    manifest output m;
    iter_over_tables (table output) cache;
    Xmlm.output output `El_end

  let fd (fd: Unix.file_descr) (m, c) : unit = 
    let oc = Unix.out_channel_of_descr fd in
    cache (Xmlm.make_output (`Channel oc)) (m, c);
    flush oc

  let file (filename: string) (m, c) : unit = 
    let fdescr = Unix.openfile filename [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
    finally
      (fun () -> fd fdescr (m, c))
      (fun () -> Unix.close fdescr)
end

module From = struct

  let cache (input: Xmlm.input) =
    let tags = Stack.create () in
    let maybe_return f accu =
      if Xmlm.eoi input then begin
        if Stack.is_empty tags then
          accu
        else
          raise (Unmarshall_error "Unexpected end of file")
      end else
        f accu in
    let rec f ((cache, table, manifest) as acc) = match Xmlm.input input with
    (* On reading a start tag... *)
	| `El_start (tag: Xmlm.tag) ->
      Stack.push tag tags;
      begin match tag with
      | (_, ("database" | "manifest")), _ -> f acc
      | (_, "table"), [ (_, "name"), _ ] ->
	  f (cache, create_empty_table (), manifest)
      | (_, "row"), ((_, "ref"), rf) :: rest ->
	  let row = create_empty_row () in
	  List.iter (fun (("", k), v) -> set_field_in_row row k (Xml_spaces.unprotect v)) rest;
	  set_row_in_table table rf row;
	  f acc
      | (_, "pair"), [ (_, "key"), k; (_, "value"), v ] ->
	  f (cache, table, (k, v) :: manifest)
      | (_, name), _ -> raise (Unmarshall_error (Printf.sprintf "Unexpected tag: %s" name))
      end
    (* On reading an end tag... *)
    | `El_end ->
      let tag = Stack.pop tags in
      begin match tag with
      | (_, ("database" | "manifest" | "row" | "pair")), _ -> maybe_return f acc
      | (_, "table"), [ (_, "name"), name ] ->
	  set_table_in_cache cache name table;
	  maybe_return f acc
      | (_, name), _ -> raise (Unmarshall_error (Printf.sprintf "Unexpected tag: %s" name))
      end
    | _ -> f acc
    in
    let (cache, _, manifest) = f (create_empty_cache (), create_empty_table (), []) in
    (* Manifest is actually a record *)
    let manifest = { 
      schema_major_vsn = int_of_string (List.assoc _schema_major_vsn manifest);
      schema_minor_vsn = int_of_string (List.assoc _schema_minor_vsn manifest);
      product_brand = List.assoc _product_brand manifest;
      build_number = List.assoc _build_number manifest;
      xapi_major_vsn = int_of_string (List.assoc _xapi_major_vsn manifest);
      xapi_minor_vsn = int_of_string (List.assoc _xapi_minor_vsn manifest);
      generation_count = Int64.of_string (List.assoc _generation_count manifest)
    } in
    manifest, cache

  let file xml_filename =
    let input = open_in xml_filename in
    finally 
      (fun () -> cache (Xmlm.make_input (`Channel input))) 
      (fun () -> close_in input) 

  let channel inchan =
    cache (Xmlm.make_input (`Channel inchan))

end

