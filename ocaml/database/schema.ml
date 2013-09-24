(*
 * Copyright (C) 2010-2011 Citrix Systems Inc.
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

module Column = struct
	type t = {
		name: string;
		persistent: bool;         (** see is_field_persistent *)
		empty: string;            (** fresh value used when loading non-persistent fields *)
		default: string option;   (** if column is missing, this is default value is used *)

		issetref: bool;           (** only so we can special case set refs in the interface *)
	}
end

module Table = struct
	type t = {
		name: string;
		columns: Column.t list;
		persistent: bool;
	}
	let find name t = List.find (fun col -> col.Column.name = name) t.columns
end

type relationship = 
	| OneToMany of string * string * string * string

module Database = struct
	type t = {
		tables: Table.t list;
	}
	let find name t = List.find (fun tbl -> tbl.Table.name = name) t.tables
end

module StringMap = Map.Make(struct
	type t = string
	let compare = Pervasives.compare
end)

type t = {
	major_vsn: int;
	minor_vsn: int;
	database: Database.t;
	(** indexed by table name, a list of (this field, foreign table, foreign field) *)
	one_to_many: ((string * string * string) list) StringMap.t;
	many_to_many: ((string * string * string) list) StringMap.t;
}

let database x = x.database

let table tblname x = 
	try
		Database.find tblname (database x)
	with Not_found as e ->
		Printf.printf "Failed to find table: %s\n%!" tblname;
		raise e

let empty = {
	major_vsn = 0;
	minor_vsn = 0;
	database = { Database.tables = [] };
	one_to_many = StringMap.empty;
	many_to_many = StringMap.empty;
}

let is_table_persistent schema tblname = 
	(table tblname schema).Table.persistent

let is_field_persistent schema tblname fldname = 
	let tbl = table tblname schema in
	let col = Table.find fldname tbl in
	tbl.Table.persistent && col.Column.persistent

let table_names schema = 
	List.map (fun t -> t.Table.name) (database schema).Database.tables

module D=Debug.Make(struct let name="xapi" end)
open D
let one_to_many tblname schema = 
	(* If there is no entry in the map it means that the table has no one-to-many relationships *)
	try
		StringMap.find tblname schema.one_to_many
	with Not_found -> []

let many_to_many tblname schema = 
	(* If there is no entry in the map it means that the table has no many-to-many relationships *)
	try
		StringMap.find tblname schema.many_to_many
	with Not_found -> []

