open Db_exn

(** Database tables, columns and rows are all indexed by string, each
	using a specialised StringMap *)
module StringMap = struct
	include Map.Make(struct
		type t = string
		let compare = Pervasives.compare
	end)
	let update key default f t = 
		let v = try find key t with Not_found -> default in
		add key (f v) t
end		

module type VAL = sig
	type v
end
(** A specialised StringMap whose range type is V.v *)
module Map2 = functor(V: VAL) -> struct
	type t = V.v StringMap.t
	let empty = StringMap.empty
	let fold = StringMap.fold
	let add = StringMap.add
	let find = StringMap.find
	let mem = StringMap.mem
	let iter = StringMap.iter
	let remove = StringMap.remove
	let update = StringMap.update
end

module StringStringMap = Map2(struct type v = string end)

module type ROW = sig
	type t
	val add: string -> string -> t -> t
	val add_defaults: Schema.Table.t -> t -> t
    val empty : t
    val fold : (string -> string -> 'b -> 'b) -> t -> 'b -> 'b
    val find : string -> t -> string
	val mem : string -> t -> bool
    val iter : (string -> string -> unit) -> t -> unit
    val remove : string -> t -> t
	val update : string -> string -> (string -> string) -> t -> t
end

module Row : ROW = struct
	include StringStringMap
	let find key t = 
		try find key t
		with Not_found -> raise (DBCache_NotFound ("missing field", key, ""))
	let add_defaults (schema: Schema.Table.t) t = 
		List.fold_left (fun t c -> 
			if not(mem c.Schema.Column.name t)
			then match c.Schema.Column.default with
				| Some default -> add c.Schema.Column.name default t
				| None -> raise (DBCache_NotFound ("missing field", c.Schema.Column.name, ""))
			else t) t schema.Schema.Table.columns
end

module StringRowMap = Map2(struct type v = Row.t end)

module type TABLE = sig
	type t
	val add: string -> Row.t -> t -> t
    val empty : t
    val fold : (string -> Row.t -> 'b -> 'b) -> t -> 'b -> 'b
	val find_exn : string -> string -> t -> Row.t
    val find : string -> t -> Row.t
	val mem : string -> t -> bool
    val iter : (string -> Row.t -> unit) -> t -> unit
    val remove : string -> t -> t
	val update : string -> Row.t -> (Row.t -> Row.t) -> t -> t
		
	val rows : t -> Row.t list
end

module Table : TABLE = struct
	include StringRowMap
	let find_exn tbl key t = 
		try find key t
		with Not_found -> raise (DBCache_NotFound ("missing row", tbl, key))
	let rows t = 
		fold (fun _ r rs -> r :: rs) t []
end

module StringTableMap = Map2(struct type v = Table.t end)

module type TABLESET = sig
	type t
	val add: string -> Table.t -> t -> t
    val empty : t
    val fold : (string -> Table.t -> 'b -> 'b) -> t -> 'b -> 'b
    val find : string -> t -> Table.t
	val mem : string -> t -> bool
    val iter : (string -> Table.t -> unit) -> t -> unit
    val remove : string -> t -> t
	val update : string -> Table.t -> (Table.t -> Table.t) -> t -> t
end

module TableSet : TABLESET = struct
	include StringTableMap
	let find key t = 
		try find key t
		with Not_found -> raise (DBCache_NotFound ("missing table", key, ""))
end

type common_key = 
	| Ref of string
	| Uuid of string
let string_of_common_key = function
	| Ref x -> x
	| Uuid x -> x

module KeyMap = struct 
	include Map.Make(struct
		type t = common_key
		let compare = Pervasives.compare
	end)
	let add_unique tblname fldname k v t = 
		if mem k t 
		then raise (Uniqueness_constraint_violation ( tblname, fldname, string_of_common_key k ))
		else add k v t
end


module Manifest = struct
	type t = {
		schema : (int * int) option;
		generation_count : Generation.t
	}

	let empty = { 
		schema = None; generation_count = Generation.null_generation 
	}

	let make schema_major_vsn schema_minor_vsn gen_count = {
		schema = Some (schema_major_vsn, schema_minor_vsn);
		generation_count = gen_count
	}

	let generation x = x.generation_count

	let update_generation f x = { 
		x with generation_count = f x.generation_count 
	}

	let next = update_generation (Int64.add 1L)

	let schema x = match x.schema with
		| None -> 0, 0
		| Some (x, y) -> x, y

	let update_schema f x = {
		x with schema = f x.schema
	}
end

(** The core database updates (PreDelete is more of an 'event') *)
type update = 
	| WriteField of string (* tblname *) * string (* objref *) * string (* fldname *) * string (* oldval *) * string (* newval *)
	| PreDelete of string (* tblname *) * string (* objref *)
	| Delete of string (* tblname *) * string (* objref *) * (string * string) list (* values *)
	| Create of string (* tblname *) * string (* objref *) * (string * string) list (* values *)

module Database = struct
	type t = {
		tables:    TableSet.t;
		manifest : Manifest.t;
		schema:    Schema.t;
		keymap:    (string * string) KeyMap.t;
		callbacks: (string * (update -> t -> unit)) list
	}
	let update_manifest f x = 
		{ x with manifest = f x.manifest }

	let manifest x = x.manifest

	let increment = update_manifest Manifest.next

	let tableset x = x.tables

	let schema x = x.schema 

	let update f x = 
		{ x with tables = f x.tables }

	let set_generation g = 
		update_manifest (Manifest.update_generation (fun _ -> g))

	let update_tableset f x = 
		{ x with tables = f x.tables }

	let update_keymap f x = 
		{ x with keymap = f x.keymap }

	let register_callback name f x = 
		{ x with callbacks = (name, f) :: x.callbacks }

	let unregister_callback name x = 
		{ x with callbacks = List.filter (fun (x, _) -> x <> name) x.callbacks }

	let notify e db = 
		List.iter (fun (name, f) ->
			try
				f e db
			with e ->
				Printf.printf "Caught %s from database callback '%s'\n%!" (Printexc.to_string e) name;
				()
		) db.callbacks

	let reindex x = 
		(* Recompute the keymap *)
		let keymap = 
			TableSet.fold
				(fun tblname tbl acc ->
					Table.fold
						(fun rf row acc -> 
							let acc = KeyMap.add_unique tblname Db_names.ref (Ref rf) (tblname, rf) acc in
							if Row.mem Db_names.uuid row
							then KeyMap.add_unique tblname Db_names.uuid (Uuid (Row.find Db_names.uuid row)) (tblname, rf) acc
							else acc
						) 
						tbl acc)
				x.tables KeyMap.empty in
		(* For each of the one-to-many relationships, recompute the many end *)
		let tables = 
			Schema.StringMap.fold
				(fun one_tblname rels tables ->
					List.fold_left (fun tables (one_fldname, many_tblname, many_fldname) ->
						(* VBD.VM : Ref(VM) -> VM.VBDs : Set(Ref(VBD)) *)
						let one_tbl = TableSet.find one_tblname tables in
						let many_tbl = TableSet.find many_tblname tables in
						(* Initialise all VM.VBDs = [] (otherwise VMs with no 
						   VBDs may be missing a VBDs field altogether on
						   upgrade) *)
						let many_tbl' = Table.fold
							(fun vm row acc ->
								let row' = Row.add many_fldname (SExpr.string_of (SExpr.Node [])) row in
								Table.add vm row' acc)
							many_tbl Table.empty in

						(* Build up a table of VM -> VBDs *)

						let vm_to_vbds = Table.fold
							(fun vbd row acc ->
								let vm = Row.find one_fldname row in
								let existing = if Schema.StringMap.mem vm acc then Schema.StringMap.find vm acc else [] in
								Schema.StringMap.add vm (vbd :: existing) acc)
							one_tbl Schema.StringMap.empty in
						let many_tbl'' = Schema.StringMap.fold
							(fun vm vbds acc ->
								if not(Table.mem vm acc)
								then acc
								else
									let row = Table.find vm acc in
									let vbds' = SExpr.string_of (SExpr.Node (List.map (fun x -> SExpr.String x) vbds)) in
									let row' = Row.add many_fldname vbds' row in
									Table.add vm row' acc)
							vm_to_vbds many_tbl' in
						TableSet.add many_tblname many_tbl'' tables)
						tables rels)
				x.schema.Schema.one_to_many
				x.tables in
		
		{ x with keymap = keymap; tables = tables }


	let table_of_ref rf db = fst (KeyMap.find (Ref rf) db.keymap)
	let lookup_key key db = 
		if KeyMap.mem (Ref key) db.keymap
		then Some (KeyMap.find (Ref key) db.keymap)
		else
			if KeyMap.mem (Uuid key) db.keymap
			then Some (KeyMap.find (Uuid key) db.keymap)
			else None

	let make schema = {
		tables    = TableSet.empty;
		manifest  = Manifest.empty;
		schema    = schema;
		keymap    = KeyMap.empty;
		callbacks = [];
	}
end

(* Helper functions to deal with Sets and Maps *)
let add_to_set key t = 
	let existing = Db_action_helper.parse_sexpr t in
	let processed = Db_action_helper.add_key_to_set key existing in
	SExpr.string_of (SExpr.Node processed)

let remove_from_set key t = 
	let existing = Db_action_helper.parse_sexpr t in
	let processed = List.filter (function SExpr.String x -> x <> key | _ -> true) existing in
	SExpr.string_of (SExpr.Node processed)

exception Duplicate
let add_to_map key value t = 
	let existing = Db_action_helper.parse_sexpr t in
	let kv = SExpr.Node [ SExpr.String key; SExpr.String value ] in
	let duplicate = List.fold_left (||) false 
			(List.map (function SExpr.Node (SExpr.String k :: _) when k = key -> true
				| _ -> false) existing) in
	if duplicate then raise Duplicate;
	let processed = kv::existing in
	SExpr.string_of (SExpr.Node processed)

let remove_from_map key t = 
	let existing = Db_action_helper.parse_sexpr t in
	let processed = List.filter (function SExpr.Node [ SExpr.String x; _ ] -> x <> key
		| _ -> true) existing in
	SExpr.string_of (SExpr.Node processed)


let (++) f g x = f (g x)
let id x = x

let set_table tblname newval =
	(Database.update ++ (TableSet.update tblname Table.empty)) 
		(fun _ -> newval)

let get_field tblname objref fldname db =
	Row.find fldname (Table.find objref (TableSet.find tblname (Database.tableset db)))
let set_field tblname objref fldname newval = 
	((Database.update 
	++ (TableSet.update tblname Table.empty) 
	++ (Table.update objref Row.empty)
	++ (Row.update fldname ""))
		(fun _ -> newval))

let update_one_to_many tblname objref f db = 
	List.fold_left (fun db (one_fld, many_tbl, many_fld) ->
		(* the value one_fld_val is the Ref _ *)
		let one_fld_val = get_field tblname objref one_fld db in
		let valid = try ignore(Database.table_of_ref one_fld_val db); true with _ -> false in
		if valid
		then set_field many_tbl one_fld_val many_fld (f objref (get_field many_tbl one_fld_val many_fld db)) db
		else db
	) db (Schema.one_to_many tblname (Database.schema db))

let set_row_in_table tblname objref newval =
	id
		(* For any field in a one-to-many, add objref to the foreign Set(Ref_) fields *)
		(* NB this requires the new row to exist already *)
	++ (update_one_to_many tblname objref add_to_set)
	++ ((Database.update ++ (TableSet.update tblname Table.empty) ++ (Table.update objref Row.empty))
		(fun _ -> newval))

	++ (Database.update_keymap (KeyMap.add_unique tblname Db_names.ref (Ref objref) (tblname, objref)))
	++ (Database.update_keymap (fun m -> 
		if Row.mem Db_names.uuid newval
		then KeyMap.add_unique tblname Db_names.uuid (Uuid (Row.find Db_names.uuid newval)) (tblname, objref) m
		else m))



let remove_row tblname objref uuid = 
	id
	++ ((Database.update ++ (TableSet.update tblname Table.empty))
		(Table.remove objref))
		(* For any field in a one-to-many, remove objref from the foreign Set(Ref_) fields *)
		(* NB this requires the original row to still exist *)
	++ (update_one_to_many tblname objref remove_from_set)

	++ (Database.update_keymap (KeyMap.remove (Ref objref)))
	++ (Database.update_keymap (fun m ->
		match uuid with
			| Some u -> KeyMap.remove (Uuid u) m
			| None -> m))
		
let set_field_in_row tblname objref fldname newval db =
	if fldname = Db_names.ref
	then failwith (Printf.sprintf "Cannot safely update field: %s" fldname);

	let oldrow = Table.find objref (TableSet.find tblname (Database.tableset db)) in
	let newrow = Row.add fldname newval oldrow in
	let olduuid = try Some(Row.find Db_names.uuid oldrow) with _ -> None in

	((set_row_in_table tblname objref newrow)
	++ (remove_row tblname objref olduuid)) db

let remove_row_from_table tblname objref db =
	let uuid = 
		try 
			Some (Row.find Db_names.uuid (Table.find objref (TableSet.find tblname (Database.tableset db))))
		with _ -> None in
	remove_row tblname objref uuid db


type where_record = {
	table: string;       (** table from which ... *)
	return: string;      (** we'd like to return this field... *)
	where_field: string; (** where this other field... *)
	where_value: string; (** contains this value *)
} with rpc

type structured_op_t = 
	| AddSet
	| RemoveSet
	| AddMap
	| RemoveMap
with rpc

