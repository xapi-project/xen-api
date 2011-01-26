
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
}

let is_table_persistent schema tblname = 
	(table tblname schema).Table.persistent

let is_field_persistent schema tblname fldname = 
	let tbl = table tblname schema in
	let col = Table.find fldname tbl in
	tbl.Table.persistent && col.Column.persistent

let table_names schema = 
	List.map (fun t -> t.Table.name) (database schema).Database.tables

module D=Debug.Debugger(struct let name="xapi" end)
open D
let one_to_many tblname schema = 
	(* If there is no entry in the map it means that the table has no one-to-many relationships *)
	try
		StringMap.find tblname schema.one_to_many
	with Not_found -> []

(* This code could live higher up the stack *)
let of_datamodel () = 
	let rec flatten_fields fs acc =
		match fs with
				[] -> acc
			| (Datamodel_types.Field f)::fs -> flatten_fields fs (f::acc)
			| (Datamodel_types.Namespace (_,internal_fs))::fs -> flatten_fields fs (flatten_fields internal_fs acc) in
	let column f = {
		Column.name = Escaping.escape_id f.Datamodel_types.full_name;
		persistent = f.Datamodel_types.field_persist;
		empty = Datamodel_values.gen_empty_db_val f.Datamodel_types.ty;
		(* NB Set(Ref _) fields aren't allowed to have a default value specified so we hardcode one here *)
		default = begin match f.Datamodel_types.ty with
			| Datamodel_types.Set (Datamodel_types.Ref _) -> Some (SExpr.string_of (SExpr.Node []))
			| _ -> Opt.map Datamodel_values.to_db_string f.Datamodel_types.default_value
		end ;
		issetref = begin match f.Datamodel_types.ty with
			| Datamodel_types.Set (Datamodel_types.Ref _) -> true
			| _ -> false
		end ;
	} in

	(* We store the reference in two places for no good reason still: *)
	let _ref = {
		Column.name = Db_names.ref;
		persistent = true;
		empty = "";
		default = None;
		issetref = false;
	} in

	let table obj = {
		Table.name = Escaping.escape_obj obj.Datamodel_types.name;
		columns = _ref :: (List.map column (flatten_fields obj.Datamodel_types.contents []));
		persistent = obj.Datamodel_types.persist = Datamodel_types.PersistEverything;
	} in
	let one_to_many t ((one_tbl, one_fld), (many_tbl, many_fld)) =
		let key = (one_fld, many_tbl, many_fld) in
		let l = if StringMap.mem one_tbl t then StringMap.find one_tbl t else [] in
		StringMap.add one_tbl ((one_fld, many_tbl, many_fld) :: l) t in

	let database api = {
		Database.tables = List.map table (Dm_api.objects_of_api api)
	} in
	{ 
		major_vsn = Datamodel.schema_major_vsn;
		minor_vsn = Datamodel.schema_minor_vsn;
		database = database Datamodel.all_api;
		one_to_many = List.fold_left one_to_many StringMap.empty (Dm_api.relations_of_api Datamodel.all_api);
	}
