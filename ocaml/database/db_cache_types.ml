(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
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

module Time = struct
  type t = Generation.t
end

module Stat = struct
  type t = {
    created: Time.t;
    modified: Time.t;
    deleted: Time.t;
  }
  let make x = { created = x; modified = x; deleted = 0L }
end

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
  type t
end

module type MAP = sig
  type t
  type value
  val empty : t
  val add: Time.t -> string -> value -> t -> t
  val remove : Time.t -> string -> t -> t
  val fold : (string -> Stat.t -> value -> 'b -> 'b) -> t -> 'b -> 'b
  val fold_over_recent : Time.t -> (string -> Stat.t -> value -> 'b -> 'b) -> t -> 'b -> 'b
  val find : string -> t -> value
  val mem : string -> t -> bool
  val iter : (string -> value -> unit) -> t -> unit
  val update : int64 -> string -> value -> (value -> value) -> t -> t
  val touch : int64 -> string -> value -> t -> t
end

(** A specialised StringMap whose range type is V.t, and which keeps a record of when records are created/updated *)
module Make = functor(V: VAL) -> struct
  type x = {
    stat: Stat.t;
    v : V.t
  }
  type map_t = x StringMap.t
  let empty = StringMap.empty
  let fold f = StringMap.fold (fun key x -> f key x.stat x.v)
  let add generation key v =
    let stat = Stat.make generation in
    StringMap.add key { stat; v }
  let find key map = (StringMap.find key map).v
  let mem = StringMap.mem
  let iter f = StringMap.iter (fun key x -> f key x.v)
  let remove _ = StringMap.remove
  let touch generation key default row =
    let default = { stat = Stat.make generation; v = default } in
    StringMap.update key default (fun x -> { x with stat = { x.stat with Stat.modified=generation } }) row
  let update generation key default f row =
    let default = { stat = Stat.make generation; v = default } in
    let updatefn () = StringMap.update key default (fun x -> { stat = { x.stat with Stat.modified=generation }; v=f x.v}) row in
    if mem key row
    then
      let old = find key row in
      let newv = f old in
      if newv == old
      then row
      else updatefn ()
    else
      updatefn ()
  let fold_over_recent since f t initial = StringMap.fold (fun x y z -> if y.stat.Stat.modified > since then f x y.stat y.v z else z) t initial
end

module Row = struct
  include Make(Schema.Value)

  type t=map_t
  type value = Schema.Value.t
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

module Table = struct
  module StringRowMap = Make(Row)

  type t = { rows : StringRowMap.map_t;
             deleted_len : int;
             deleted : (Time.t * Time.t * string) list }
  type value = Row.t
  let add g key value t = {t with rows=StringRowMap.add g key value t.rows}
  let empty = {rows=StringRowMap.empty; deleted_len = 1; deleted=[(0L,0L,"")] }
  let fold f t acc = StringRowMap.fold f t.rows acc
  let find key t = StringRowMap.find key t.rows
  let mem key t = StringRowMap.mem key t.rows
  let iter f t = StringRowMap.iter f t.rows
  let remove g key t =
    let upper_length_deleted_queue = 512 in
    let lower_length_deleted_queue = 256 in
    let created = (StringMap.find key t.rows).StringRowMap.stat.Stat.created in
    let new_element = (created,g,key) in
    let new_len,new_deleted =
      if t.deleted_len + 1 < upper_length_deleted_queue
      then t.deleted_len + 1, (new_element::t.deleted)
      else lower_length_deleted_queue + 1, (new_element::(Xapi_stdext_std.Listext.List.take lower_length_deleted_queue t.deleted))
    in
    {rows = StringRowMap.remove g key t.rows;
     deleted_len = new_len;
     deleted = new_deleted}
  let touch g key default t = {t with rows = StringRowMap.touch g key default t.rows }
  let update g key default f t = {t with rows = StringRowMap.update g key default f t.rows}
  let fold_over_recent since f t acc = StringRowMap.fold_over_recent since f t.rows acc

  let fold_over_deleted since f t acc =
    let rec loop xs acc = match xs with
      | (created,deleted,r)::xs ->
        let new_acc =
          if (deleted > since) && (created <= since)
          then (f r { Stat.created; modified = deleted; deleted } acc)
          else acc
        in
        if deleted <= since then new_acc else loop xs new_acc
      | [] ->
        acc in
    loop t.deleted acc
end

module TableSet = struct
  include Make(Table)

  type t=map_t
  type value = Table.t
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

  let touch f x = {
    x with generation_count = f x.generation_count
  }

  let next = touch (Int64.add 1L)

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
  | WriteField of string (* tblname *) * string (* objref *) * string (* fldname *) * Schema.Value.t (* newval *)
  | PreDelete of string (* tblname *) * string (* objref *)
  | Delete of string (* tblname *) * string (* objref *) * (string * Schema.Value.t) list (* values *)
  | Create of string (* tblname *) * string (* objref *) * (string * Schema.Value.t) list (* values *)

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
    update_manifest (Manifest.touch (fun _ -> g))

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
        (fun tblname _ tbl acc ->
           Table.fold
             (fun rf _ row acc ->
                let acc = KeyMap.add_unique tblname Db_names.ref (Ref rf) (tblname, rf) acc in
                if Row.mem Db_names.uuid row
                then KeyMap.add_unique tblname Db_names.uuid (Uuid (Schema.Value.Unsafe_cast.string (Row.find Db_names.uuid row))) (tblname, rf) acc
                else acc
             )
             tbl acc)
        x.tables KeyMap.empty in
    (* For each of the one-to-many relationships, recompute the many end *)
    let tables =
      Schema.ForeignMap.fold
        (fun one_tblname rels tables ->
           List.fold_left (fun tables (one_fldname, many_tblname, many_fldname) ->
               (* VBD.VM : Ref(VM) -> VM.VBDs : Set(Ref(VBD)) *)
               let one_tbl = TableSet.find one_tblname tables in
               let many_tbl = TableSet.find many_tblname tables in
               (* Initialise all VM.VBDs = [] (otherwise VMs with no
                  						   VBDs may be missing a VBDs field altogether on
                  						   upgrade) *)
               let many_tbl' = Table.fold
                   (fun vm _ row acc ->
                      let row' = Row.add g many_fldname (Schema.Value.Set []) row in
                      Table.add g vm row' acc)
                   many_tbl Table.empty in

               (* Build up a table of VM -> VBDs *)

               let vm_to_vbds = Table.fold
                   (fun vbd _ row acc ->
                      let vm = Schema.Value.Unsafe_cast.string (Row.find one_fldname row) in
                      let existing = if Schema.ForeignMap.mem vm acc then Schema.ForeignMap.find vm acc else [] in
                      Schema.ForeignMap.add vm (vbd :: existing) acc)
                   one_tbl Schema.ForeignMap.empty in
               let many_tbl'' = Schema.ForeignMap.fold
                   (fun vm vbds acc ->
                      if not(Table.mem vm acc)
                      then acc
                      else
                        let row = Table.find vm acc in
                        let row' = Row.add g many_fldname (Schema.Value.Set vbds) row in
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
  let t = Schema.Value.Unsafe_cast.set t in
  Schema.Value.Set (if List.mem key t then t else key :: t)

let remove_from_set key t =
  let t = Schema.Value.Unsafe_cast.set t in
  Schema.Value.Set (List.filter (fun x -> x <> key) t)

exception Duplicate
let add_to_map ~idempotent key value t =
  if String.length key = 0 then raise Db_exn.Empty_key_in_map;
  let t = Schema.Value.Unsafe_cast.pairs t in
  if List.mem_assoc key t && (not idempotent || List.assoc key t <> value) then raise Duplicate;
  Schema.Value.Pairs ((key, value) :: List.filter (fun (k, _) -> k <> key) t)

let remove_from_map key t =
  let t = Schema.Value.Unsafe_cast.pairs t in
  Schema.Value.Pairs (List.filter (fun (k, _) -> k <> key) t)


let id x = x

let is_valid tblname objref db =
  Table.mem objref (TableSet.find tblname (Database.tableset db))


let get_field tblname objref fldname db =
  try
    Row.find fldname (Table.find objref (TableSet.find tblname (Database.tableset db)))
  with Not_found ->
    raise (DBCache_NotFound ("missing row", tblname, objref))

let unsafe_set_field g tblname objref fldname newval =
  (fun _ -> newval)
  |> Row.update g fldname (Schema.Value.String "")
  |> Table.update g objref Row.empty
  |> TableSet.update g tblname Table.empty
  |> Database.update

let update_one_to_many g tblname objref f db =
  if not (is_valid tblname objref db) then db else
    List.fold_left (fun db (one_fld, many_tbl, many_fld) ->
        (* the value one_fld_val is the Ref _ *)
        let one_fld_val = Schema.Value.Unsafe_cast.string (get_field tblname objref one_fld db) in
        let valid = try ignore(Database.table_of_ref one_fld_val db); true with _ -> false in
        if valid
        then unsafe_set_field g many_tbl one_fld_val many_fld (f objref (get_field many_tbl one_fld_val many_fld db)) db
        else db
      ) db (Schema.one_to_many tblname (Database.schema db))

let update_many_to_many g tblname objref f db =
  if not (is_valid tblname objref db) then db else
    List.fold_left (fun db (this_fld, other_tbl, other_fld) ->
        let this_fld_refs = Schema.Value.Unsafe_cast.set (get_field tblname objref this_fld db) in
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
    db
    |> update_many_to_many g tblname objref remove_from_set
    |> update_one_to_many g tblname objref remove_from_set
    |> Database.update (
      (fun _ -> newval)
      |> Row.update g fldname (Schema.Value.String "")
      |> Table.update g objref Row.empty
      |> TableSet.update g tblname Table.empty
    )
    |> update_many_to_many g tblname objref add_to_set
    |> update_one_to_many g tblname objref add_to_set
    |> Database.increment
  end else begin
    let g = Manifest.generation (Database.manifest db) in
    db
    |> (
      (fun _ -> newval)
      |> Row.update g fldname (Schema.Value.String "")
      |> Table.update g objref Row.empty
      |> TableSet.update g tblname Table.empty
      |> Database.update
    )
    |> Database.increment
  end

let touch tblname objref db =
  let g = Manifest.generation (Database.manifest db) in
  (* We update the generation twice so that we can return the lower count
     for the "event.inject" API to guarantee that the token from a later
     event.from will always compare as strictly greater. See the definition
     of the event token datatype. *)
  db
  |> (
    Row.empty
    |> Table.touch g objref
    |> TableSet.update g tblname Table.empty
    |> Database.update
  )
  |> Database.increment
  |> Database.increment

let add_row tblname objref newval db =
  let g = db.Database.manifest.Manifest.generation_count in
  db
  |> Database.update_keymap (fun m ->
      if Row.mem Db_names.uuid newval
      then KeyMap.add_unique tblname Db_names.uuid (Uuid (Schema.Value.Unsafe_cast.string (Row.find Db_names.uuid newval))) (tblname, objref) m
      else m)
  |> Database.update_keymap (KeyMap.add_unique tblname Db_names.ref (Ref objref) (tblname, objref))
  |> (
    (fun _ -> newval)
    |> Table.update g objref newval
    |> TableSet.update g tblname Table.empty
    |> Database.update
  )
  |> update_many_to_many g tblname objref add_to_set
  (* Update foreign Set(Ref _) fields *)
  (* NB this requires the new row to exist already *)
  |> update_one_to_many g tblname objref add_to_set
  |> Database.increment

let remove_row tblname objref db =
  let uuid =
    try
      Some (Schema.Value.Unsafe_cast.string (Row.find Db_names.uuid (Table.find objref (TableSet.find tblname (Database.tableset db)))))
    with _ -> None in
  let g = db.Database.manifest.Manifest.generation_count in
  db
  |> Database.update_keymap (fun m ->
  match uuid with
  | Some u -> KeyMap.remove (Uuid u) m
  | None -> m)
  |> Database.update_keymap (KeyMap.remove (Ref objref))
  |> update_many_to_many g tblname objref remove_from_set
  (* Update foreign (Set(Ref _)) fields *)
   (* NB this requires the original row to still exist *)
  |> update_one_to_many g tblname objref remove_from_set
  |> (
    Table.remove g objref
    |> TableSet.update g tblname Table.empty
    |> Database.update
  )
  |> Database.increment

type where_record = {
  table: string;       (** table from which ... *)
  return: string;      (** we'd like to return this field... *)
  where_field: string; (** where this other field... *)
  where_value: string; (** contains this value *)
} [@@deriving rpc]

type structured_op_t =
  | AddSet
  | RemoveSet
  | AddMap
  | RemoveMap
  | AddMapLegacy
[@@deriving rpc]
