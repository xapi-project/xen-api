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
open Db_cache_types
open Pervasiveext
module R = Debug.Make(struct let name = "redo_log" end)

(** Functions to marshall/unmarshall the database as XML *)

exception Unmarshall_error of string

let persist_generation_counts = true

let name x = ("", x) (* no namespace *)
let make_tag n attrs : Xmlm.tag = (name n), List.map (fun (k, v) -> name k, v) attrs

let _schema_major_vsn = "schema_major_vsn"
let _schema_minor_vsn = "schema_minor_vsn"
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
  let table schema (output: Xmlm.output) name (tbl: Table.t) = 
    let record rf ctime mtime (row: Row.t) _ = 
		let preamble = 
			if persist_generation_counts 
			then [("__mtime",Int64.to_string mtime); ("__ctime",Int64.to_string ctime); ("ref",rf)] 
			else [("ref",rf)] 
		in
	  let (tag: Xmlm.tag) = make_tag "row" (List.rev (Row.fold (fun k _ _ v acc -> (k, Xml_spaces.protect v) :: acc) row preamble)) in
      Xmlm.output output (`El_start tag);
      Xmlm.output output `El_end in
    let tag = make_tag "table" [ "name", name ] in
    Xmlm.output output (`El_start tag);
    (* we write a table entry whether or not the table persists, because populate happens to assume
       that all tables will be present. However, if the table is marked as "don't persist" then we
       don't write any row entries: *)
	if Schema.is_table_persistent schema name 
	then Table.fold record tbl ();
    Xmlm.output output `El_end
	
  (* Write out a manifest *)
  let manifest (output: Xmlm.output) (manifest: Manifest.t) : unit = 
      Xmlm.output output (`El_start (make_tag "manifest" []));
	  let major, minor = Manifest.schema manifest in
      int    output _schema_major_vsn major;
      int    output _schema_minor_vsn minor;
      int64  output _generation_count (Manifest.generation manifest);
      Xmlm.output output `El_end

  (* Write out a full database *)
  let database (output: Xmlm.output) db : unit =
    Xmlm.output output (`Dtd None);	
    Xmlm.output output (`El_start (make_tag "database" []));
    manifest output (Database.manifest db);
    TableSet.iter (table (Database.schema db) output) (Database.tableset db);
    Xmlm.output output `El_end

  let fd (fd: Unix.file_descr) db : unit = 
    let oc = Unix.out_channel_of_descr fd in
    database (Xmlm.make_output (`Channel oc)) db;
    flush oc

  let file (filename: string) db : unit = 
    let fdescr = Unix.openfile filename [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
    finally
      (fun () -> fd fdescr db)
      (fun () -> Unix.close fdescr)
end

module From = struct

	let database schema (input: Xmlm.input) =
		let tags = Stack.create () in
		let maybe_return f accu =
			if Xmlm.eoi input then begin
				if Stack.is_empty tags then
					accu
				else
					raise (Unmarshall_error "Unexpected end of file")
			end else
				f accu in
		let rec f ((tableset, table, manifest) as acc) = match Xmlm.input input with
				(* On reading a start tag... *)
			| `El_start (tag: Xmlm.tag) ->
				Stack.push tag tags;
				begin match tag with
					| (_, ("database" | "manifest")), _ -> f acc
					| (_, "table"), [ (_, "name"), _ ] ->
						f (tableset, Table.empty, manifest)
					| (_, "row"), ((_, "ref"), rf) :: rest ->
						(* Remove any other duplicate "ref"s which might have sneaked in there *)
						let rest = List.filter (fun ((_,k), _) -> k <> "ref") rest in
						let (ctime_l,rest) = List.partition (fun ((_, k), _) -> k="__ctime") rest in
						let (mtime_l,rest) = List.partition (fun ((_, k), _) -> k="__mtime") rest in
						let ctime = match ctime_l with | [(_,ctime_s)] -> Int64.of_string ctime_s | _ -> 0L in
						let mtime = match mtime_l with | [(_,mtime_s)] -> Int64.of_string mtime_s | _ -> 0L in
						let row = List.fold_left (fun row ((_, k), v) -> 
							Row.update mtime k "" (fun _ -> (Xml_spaces.unprotect v)) (Row.add ctime k (Xml_spaces.unprotect v) row)
						) Row.empty rest in
						f (tableset, (Table.update mtime rf Row.empty (fun _ -> row) (Table.add ctime rf row table)), manifest)
					| (_, "pair"), [ (_, "key"), k; (_, "value"), v ] ->
						f (tableset, table, (k, v) :: manifest)
					| (_, name), _ -> 
						raise (Unmarshall_error (Printf.sprintf "Unexpected tag: %s" name))
				end
					(* On reading an end tag... *)
			| `El_end ->
				let tag = Stack.pop tags in
				begin match tag with
					| (_, ("database" | "manifest" | "row" | "pair")), _ -> maybe_return f acc
					| (_, "table"), [ (_, "name"), name ] ->
						maybe_return f (TableSet.add 0L name table tableset, Table.empty, manifest)
					| (_, name), _ -> 
						raise (Unmarshall_error (Printf.sprintf "Unexpected tag: %s" name))
				end
			| _ -> f acc
		in
		let (ts, _, manifest) = f (TableSet.empty, Table.empty, []) in
		let g = Int64.of_string (List.assoc _generation_count manifest) in
		let major_vsn = int_of_string (List.assoc _schema_major_vsn manifest) in
		let minor_vsn = int_of_string (List.assoc _schema_minor_vsn manifest) in
		let manifest = Manifest.make major_vsn minor_vsn g in
		((Database.update_manifest (fun _ -> manifest)) 
		++ (Database.update_tableset (fun _ -> ts)))
			(Database.make schema)


  let file schema xml_filename =
      let input = open_in xml_filename in
      finally 
		  (fun () -> database schema (Xmlm.make_input (`Channel input))) 
		  (fun () -> close_in input) 

  let channel schema inchan =
      database schema (Xmlm.make_input (`Channel inchan))

end

