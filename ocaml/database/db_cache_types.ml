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

(** A specialised StringMap whose range type is V.v, and which keeps a record of when records are created/updated *)
module Map2 = functor(V: VAL) -> struct
	type x = {
		created : int64;
		updated : int64;
		v : V.v }
	type map_t = x StringMap.t
	let empty = StringMap.empty
	let fold f = StringMap.fold (fun key x -> f key x.created x.updated x.v)
	let add generation key value = StringMap.add key {created=generation; updated=generation; v=value}
	let find key map = (StringMap.find key map).v 
	let mem = StringMap.mem
	let iter f = StringMap.iter (fun key x -> f key x.v)
	let remove = StringMap.remove
	let update_generation generation key default f row =
		StringMap.update key {created=generation; updated=generation; v=default} (fun x -> {x with updated=generation; v=f x.v}) row
	let update generation key default f row = 
		let updatefn () = StringMap.update key {created=generation; updated=generation; v=default} (fun x -> {x with updated=generation; v=f x.v}) row in
		if mem key row 
		then
			let old = find key row in
			let newv = f old in
			if newv == old 
			then row 
			else updatefn ()				
		else
			updatefn ()
	let fold_over_recent since f = StringMap.fold (fun x y z -> if y.updated > since then f y.created y.updated 0L x y.v z else z)
end

module StringStringMap = Map2(struct type v = string end)

module type ROW = sig
	type t
	val add: int64 -> string -> string -> t -> t
	val add_defaults: int64 -> Schema.Table.t -> t -> t
	val empty : t
	val fold : (string -> int64 -> int64 -> string -> 'b -> 'b) -> t -> 'b -> 'b
	val find : string -> t -> string
	val mem : string -> t -> bool
	val iter : (string -> string -> unit) -> t -> unit
	val remove : string -> t -> t
	val update : int64 -> string -> string -> (string -> string) -> t -> t
	val fold_over_recent : int64 -> (int64 -> int64 -> int64 -> string -> string -> 'b -> 'b) -> t -> 'b -> 'b
end

module Row : ROW = struct
	include StringStringMap
	type t=map_t
	let find key t =
		try find key t
		with Not_found -> raise (DBCache_NotFound ("missing field", key, ""))
	let add_defaults g (schema: Schema.Table.t) t =
		List.fold_left (fun t c ->
			if not(mem c.Schema.Column.name t)
			then match c.Schema.Column.default with
				| Some default -> add g c.Schema.Column.name default t
				| None -> raise (DBCache_NotFound ("missing field", c.Schema.Column.name, ""))
			else t) t schema.Schema.Table.columns
end

module StringRowMap = Map2(struct type v = Row.t end)

module type TABLE = sig
	type t
	val add: int64 -> string -> Row.t -> t -> t
	val empty : t
	val fold : (string -> int64 -> int64 -> Row.t -> 'b -> 'b) -> t -> 'b -> 'b
	val find_exn : string -> string -> t -> Row.t
	val find : string -> t -> Row.t
	val mem : string -> t -> bool
	val iter : (string -> Row.t -> unit) -> t -> unit
	val remove : int64 -> string -> t -> t
	val update_generation : int64 -> string -> Row.t -> (Row.t -> Row.t) -> t -> t
	val update : int64 -> string -> Row.t -> (Row.t -> Row.t) -> t -> t
	val fold_over_recent : int64 -> (int64 -> int64 -> int64 -> string -> Row.t option -> 'b -> 'b) -> (unit -> unit) -> t -> 'b -> 'b
	val rows : t -> Row.t list
end

module Table : TABLE = struct
	type t = { rows : StringRowMap.map_t;
			   deleted_len : int;
			   deleted : (int64 * int64 * string) list }
	let add g key value t = {t with rows=StringRowMap.add g key value t.rows}
	let empty = {rows=StringRowMap.empty; deleted_len = 1; deleted=[(0L,0L,"")] }
	let fold f t acc = StringRowMap.fold f t.rows acc
	let find_exn tbl key t =
		try StringRowMap.find key t.rows
		with Not_found -> raise (DBCache_NotFound ("missing row", tbl, key))
	let find key t = StringRowMap.find key t.rows
	let mem key t = StringRowMap.mem key t.rows
	let iter f t = StringRowMap.iter f t.rows
	let remove g key t =
		let upper_length_deleted_queue = 512 in
		let lower_length_deleted_queue = 256 in
		let created = (StringMap.find key t.rows).StringRowMap.created in
		let new_element = (created,g,key) in
		let new_len,new_deleted =
			if t.deleted_len + 1 < upper_length_deleted_queue
			then t.deleted_len + 1, (new_element::t.deleted)
			else lower_length_deleted_queue + 1, (new_element::(Listext.List.take lower_length_deleted_queue t.deleted))
		in
		{rows = StringRowMap.remove key t.rows;
		 deleted_len = new_len;
		 deleted = new_deleted}
	let update_generation g key default f t = {t with rows = StringRowMap.update_generation g key default f t.rows }
	let update g key default f t = {t with rows = StringRowMap.update g key default f t.rows}
	let fold_over_recent since f errf t acc =
		let acc = StringRowMap.fold_over_recent since (fun c u d x y z -> f c u d x (Some y) z) t.rows acc in
		let rec fold_over_deleted deleted acc =
			match deleted with
				| (created,destroyed,r)::xs ->
					let new_acc =
						if (destroyed > since) && (created <= since)
						then (f created 0L destroyed r None acc)
						else acc
					in
					if destroyed <= since then new_acc else fold_over_deleted xs new_acc
				| [] ->
					errf ();
					acc
		in fold_over_deleted t.deleted acc
	let rows t =
		fold (fun _ _ _ r rs -> r :: rs) t []
end

module StringTableMap = Map2(struct type v = Table.t end)

module type TABLESET = sig
	type t
	val add: int64 -> string -> Table.t -> t -> t
	val empty : t
	val fold : (string -> int64 -> int64 -> Table.t -> 'b -> 'b) -> t -> 'b -> 'b
	val find : string -> t -> Table.t
	val mem : string -> t -> bool
	val iter : (string -> Table.t -> unit) -> t -> unit
	val remove : string -> t -> t
	val update : int64 -> string -> Table.t -> (Table.t -> Table.t) -> t -> t
	val fold_over_recent : int64 -> (int64 -> int64 -> int64 -> string -> Table.t -> 'b -> 'b) -> t -> 'b -> 'b
end

module TableSet : TABLESET = struct
	include StringTableMap
	type t=map_t
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

(** The core database updates (RefreshRow and PreDelete is more of an 'event') *)
type update =
	| RefreshRow of string (* tblname *) * string (* objref *)
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
		let g = x.manifest.Manifest.generation_count in
		(* Recompute the keymap *)
		let keymap =
			TableSet.fold
				(fun tblname _ _ tbl acc ->
					Table.fold
						(fun rf _ _ row acc ->
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
							(fun vm _ _ row acc ->
								let row' = Row.add g many_fldname (SExpr.string_of (SExpr.Node [])) row in
								Table.add g vm row' acc)
							many_tbl Table.empty in

						(* Build up a table of VM -> VBDs *)

						let vm_to_vbds = Table.fold
							(fun vbd _ _ row acc ->
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
									let row' = Row.add g many_fldname vbds' row in
									Table.add g vm row' acc)
							vm_to_vbds many_tbl' in
						TableSet.add g many_tblname many_tbl'' tables)
						tables rels)
				x.schema.Schema.one_to_many
				x.tables in

		{ x with keymap = keymap; tables = tables }


	let table_of_ref rf db = fst (KeyMap.find (Ref rf) db.keymap)
	let lookup_key key db =
		if KeyMap.mem (Ref key) db.keymap
		then Some (KeyMap.find (Ref key) db.keymap)
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

let set_of_string t =
	List.map
		(function SExpr.String x -> x
			| x -> failwith (Printf.sprintf "Unexpected sexpr: %s" t))
		(Db_action_helper.parse_sexpr t)
let string_of_set t = SExpr.string_of (SExpr.Node (List.map (fun x -> SExpr.String x) t))

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

let is_valid tblname objref db =
	Table.mem objref (TableSet.find tblname (Database.tableset db))



let get_field tblname objref fldname db =
	Row.find fldname (Table.find_exn tblname objref (TableSet.find tblname (Database.tableset db)))

let unsafe_set_field g tblname objref fldname newval =
	(Database.update 
	++ (TableSet.update g tblname Table.empty)
	++ (Table.update g objref Row.empty)
	++ (Row.update g fldname ""))
		(fun _ -> newval) 

let update_one_to_many g tblname objref f db =
	if not (is_valid tblname objref db) then db else
	List.fold_left (fun db (one_fld, many_tbl, many_fld) ->
		(* the value one_fld_val is the Ref _ *)
		let one_fld_val = get_field tblname objref one_fld db in
		let valid = try ignore(Database.table_of_ref one_fld_val db); true with _ -> false in
		if valid
		then unsafe_set_field g many_tbl one_fld_val many_fld (f objref (get_field many_tbl one_fld_val many_fld db)) db
		else db
	) db (Schema.one_to_many tblname (Database.schema db))

let update_many_to_many g tblname objref f db =
	if not (is_valid tblname objref db) then db else
	List.fold_left (fun db (this_fld, other_tbl, other_fld) ->
		let this_fld_val = get_field tblname objref this_fld db in
		let this_fld_refs = set_of_string this_fld_val in
		(* for each of this_fld_refs, apply f *)
		List.fold_left (fun db other_ref ->
			let valid = try ignore(Database.table_of_ref other_ref db); true with _ -> false in
			if valid
			then
				let other_field = get_field other_tbl other_ref other_fld db in
				unsafe_set_field g other_tbl other_ref other_fld (f objref other_field) db
			else db)
			db this_fld_refs
	) db (Schema.many_to_many tblname (Database.schema db))

let set_field tblname objref fldname newval db =
	if fldname = Db_names.ref
	then failwith (Printf.sprintf "Cannot safely update field: %s" fldname);
	let need_other_table_update =
		let schema = Database.schema db in
		match (Schema.one_to_many tblname schema, Schema.many_to_many tblname schema) with
			| [],[] -> 
				false
			| o2m,m2m ->
				List.exists (fun (fld,tbl,otherfld) -> fld = fldname) o2m
				|| List.exists (fun (fld,tbl,otherfld) -> fld = fldname) m2m
	in

	if need_other_table_update then begin
		let g = Manifest.generation (Database.manifest db) in
		(Database.increment 
		 ++ (update_one_to_many g tblname objref add_to_set)
		 ++ (update_many_to_many g tblname objref add_to_set)
		 ++ ((Database.update 
			  ++ (TableSet.update g tblname Table.empty)
			  ++ (Table.update g objref Row.empty)
			  ++ (Row.update g fldname "")) (fun _ -> newval))
		 ++ (update_one_to_many g tblname objref remove_from_set)
		 ++ (update_many_to_many g tblname objref remove_from_set)) db
	end else begin
		let g = Manifest.generation (Database.manifest db) in
		(Database.increment 
		 ++ ((Database.update 
			 ++ (TableSet.update g tblname Table.empty)
			 ++ (Table.update g objref Row.empty)
			 ++ (Row.update g fldname ""))
				(fun _ -> newval))) db
	end

let update_generation tblname objref db =
	let g = Manifest.generation (Database.manifest db) in
	(* We update the generation twice so that we can return the lower count
	   for the "event.inject" API to guarantee that the token from a later
	   event.from will always compare as strictly greater. See the definition
	   of the event token datatype. *)
	(Database.increment ++ Database.increment
		++ ((Database.update
			++ (TableSet.update g tblname Table.empty)
			++ (Table.update_generation g objref Row.empty)) id
		)) db

let add_row tblname objref newval db =
	let g = db.Database.manifest.Manifest.generation_count in
	(Database.increment
		(* Update foreign Set(Ref _) fields *)
		(* NB this requires the new row to exist already *)
	 ++ (update_one_to_many g tblname objref add_to_set)
	 ++ (update_many_to_many g tblname objref add_to_set)
	 ++ ((Database.update ++ (TableSet.update g tblname Table.empty) ++ (Table.update g objref newval))
			 (fun _ -> newval))
	 ++ (Database.update_keymap (KeyMap.add_unique tblname Db_names.ref (Ref objref) (tblname, objref)))
	 ++ (Database.update_keymap (fun m ->
									 if Row.mem Db_names.uuid newval
									 then KeyMap.add_unique tblname Db_names.uuid (Uuid (Row.find Db_names.uuid newval)) (tblname, objref) m
									 else m))) db

let remove_row tblname objref db =
	let uuid =
		try
			Some (Row.find Db_names.uuid (Table.find objref (TableSet.find tblname (Database.tableset db))))
		with _ -> None in
	let g = db.Database.manifest.Manifest.generation_count in
	(Database.increment
	 ++ ((Database.update ++ (TableSet.update g tblname Table.empty))
			 (Table.remove g objref))
	 (* Update foreign (Set(Ref _)) fields *)
	 (* NB this requires the original row to still exist *)
	 ++ (update_one_to_many g tblname objref remove_from_set)
	 ++ (update_many_to_many g tblname objref remove_from_set)
	 ++ (Database.update_keymap (KeyMap.remove (Ref objref)))
	 ++ (Database.update_keymap (fun m ->
									 match uuid with
										 | Some u -> KeyMap.remove (Uuid u) m
										 | None -> m))) db


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

let __callback : ((?snapshot: XMLRPC.xmlrpc -> ?row:Row.t -> string -> string -> string -> unit) option ref) = ref None
let events_register f = __callback := Some f
let events_unregister () = __callback := None
    
let events_notify ?(snapshot) ?(row) ty op ref =
  match !__callback with
    | None -> ()
    | Some f -> f ?snapshot ?row ty op ref
