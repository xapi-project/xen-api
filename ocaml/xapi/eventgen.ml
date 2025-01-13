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
open Debug.Make (struct let name = "sql" end)

type get_record = unit -> Rpc.t

let get_record_table :
    (string, __context:Context.t -> self:string -> get_record) Hashtbl.t =
  Hashtbl.create 64

let set_get_record = Hashtbl.replace get_record_table

let find_get_record obj_name ~__context ~self () : Rpc.t option =
  Option.map
    (fun f -> f ~__context ~self ())
    (Hashtbl.find_opt get_record_table obj_name)

(* Bidirectional lookup for relations encoded in all_relations. *)
let lookup_object_relation =
  (* Precompute the symmetric closure of all_relations and store it as
     a hash table. *)
  let symmetric_table =
    let table = Hashtbl.create 128 in
    let relate = Hashtbl.replace table in
    let api = Datamodel.all_api in
    let close (p, p') =
      (* R U= { (p, p'), (p', p) } where p, p' are of the form
         (object, field) *)
      relate p p' ; relate p' p
    in
    Dm_api.relations_of_api api |> List.iter close ;
    table
  in
  Hashtbl.find_opt symmetric_table

(* If a record is modified, events must be emitted for related objects' records.
   We collect a list of related objects by querying the (Ref _)-typed
   fields of the input object against the relations encoded by the datamodel.

   The result of this function is a list of pairs [(object, field);, ...].
   Note that the field component refers to a field in the input object,
   not the related object. *)
let compute_object_references_to_follow (obj_name : string) =
  let module DT = Datamodel_types in
  let api = Datamodel.all_api in
  let obj = Dm_api.get_obj_by_name api ~objname:obj_name in
  (* Find an object related to the input field using the datamodel. *)
  let find_related_object = function
    | DT.{field_name; ty= Ref _; _} ->
        let this_end = (obj_name, field_name) in
        lookup_object_relation this_end
        |> Option.map (fun (other_object, _) -> (other_object, field_name))
    | _ ->
        None
  in
  let fields = Datamodel_utils.fields_of_obj obj in
  List.filter_map find_related_object fields

(* For each object, precompute a list of related objects [(object,
   field); ...] and store it in a hash table.

   If looking up an entry for some object, "foo", yields a list
   containing ("bar", "baz"), then it must be the case that "foo"'s
   field "baz" (which is necessarily of type Ref _) is related to some
   field within "bar". In which case, the database will have already
   updated the related field(s) and we must emit events for those fields. *)
let obj_references_table : (string, (string * string) list) Hashtbl.t =
  let table = Hashtbl.create 64 in
  let populate_follows (obj : Datamodel_types.obj) =
    let follows = compute_object_references_to_follow obj.name in
    Hashtbl.replace table obj.name follows
  in
  Dm_api.objects_of_api Datamodel.all_api |> List.iter populate_follows ;
  table

let follow_references (obj_name : string) =
  Hashtbl.find obj_references_table obj_name

(* Compute a modify event's snapshot by attemping to invoke its
   getter. If the record cannot be found, the event is dropped (as the
   reference is probably dangling). *)
let snapshots_of_other_tbl_refs other_tbl_refs =
  let try_get_records (table, field, getter) =
    try Some (table, field, getter ())
    with _ ->
      (* Probably means the reference was dangling *)
      warn "%s: skipping event for dangling reference %s: %s" __FUNCTION__ table
        field ;
      None
  in
  List.filter_map try_get_records other_tbl_refs

open Xapi_database.Db_cache_types
open Xapi_database.Db_action_helper

let is_valid_ref db = function
  | Schema.Value.String r -> (
    try
      ignore (Database.table_of_ref r db) ;
      true
    with _ -> false
  )
  | _ ->
      false

type event_kind = Modify | Delete | Add

let strings_of_event_kind = function
  | Modify ->
      ("mod", "MOD")
  | Delete ->
      ("del", "DEL")
  | Add ->
      ("add", "ADD")

let emit_events ~kind events =
  let kind, upper = strings_of_event_kind kind in
  let emit = function
    | tbl, ref, None ->
        error "%s: Failed to generate %s event on %s %s" __FUNCTION__ upper tbl
          ref
    | tbl, ref, Some snapshot ->
        events_notify ~snapshot tbl kind ref
  in
  List.iter emit events

let database_callback_inner event db ~__context =
  let other_tbl_refs tblname = follow_references tblname in
  let other_tbl_refs_for_this_field tblname fldname =
    List.filter (fun (_, fld) -> fld = fldname) (other_tbl_refs tblname)
  in
  let compute_other_table_events table kvs =
    (* Given a table and a deleted/new row's key-values, compute event
       snapshots for all objects potentially referenced by values in the
       row. *)
    let get_potential_event (other_tbl, field) =
      (* If a deleted/new field could refer to a row within
         other_tbl, collect a potential event for it. *)
      let field_value = List.assoc field kvs in
      if is_valid_ref db field_value then
        let self = Schema.Value.Unsafe_cast.string field_value in
        let getter = find_get_record other_tbl ~__context ~self in
        Some (other_tbl, self, getter)
      else
        None
    in
    follow_references table
    |> List.filter_map get_potential_event
    |> snapshots_of_other_tbl_refs
  in
  match event with
  | RefreshRow (tblname, objref) ->
      (* To refresh a row, emit a modify event with the current record's
         snapshot. *)
      let getter = find_get_record tblname ~__context ~self:objref in
      let snapshot = getter () in
      emit_events ~kind:Modify [(tblname, objref, snapshot)]
  | WriteField (tblname, objref, fldname, oldval, newval) ->
      (* When a field is written, both the new and old values of the
         field may be references to other objects, which have already
         been rewritten by the database layer. To follow up, we must
         emit events for the previously referenced object, the current
         row, and the newly referenced object.*)
      let other_tbl_refs = other_tbl_refs_for_this_field tblname fldname in
      (* Compute list of potential events. Some snapshots may fail to
         be reified because the reference is no longer valid (i.e. it's
         dangling). *)
      let get_other_ref_events maybe_ref =
        if is_valid_ref db maybe_ref then
          let self = Schema.Value.Unsafe_cast.string maybe_ref in
          let go (other_tbl, _) =
            let get_record = find_get_record other_tbl ~__context ~self in
            (other_tbl, self, get_record)
          in
          List.map go other_tbl_refs |> snapshots_of_other_tbl_refs
        else
          []
      in
      (* Compute modify events for the old and new field values, if
         either value appears to be a reference. *)
      let events_old_val = get_other_ref_events oldval in
      let events_new_val = get_other_ref_events newval in
      (* Emit modify events for records referenced by the old row's value. *)
      emit_events ~kind:Modify events_old_val ;
      (* Emit a modify event for the current row. *)
      let getter = find_get_record tblname ~__context ~self:objref in
      let snapshot = getter () in
      emit_events ~kind:Modify [(tblname, objref, snapshot)] ;
      (* Emit modify events for records referenced by the new row's value. *)
      emit_events ~kind:Modify events_new_val
  | PreDelete (tblname, objref) ->
      (* Emit a deletion event for the deleted row. *)
      let getter = find_get_record tblname ~__context ~self:objref in
      let snapshot = getter () in
      emit_events ~kind:Delete [(tblname, objref, snapshot)]
  | Delete (tblname, _objref, kvs) ->
      (* Deleting a row requires similar modify events as overwriting a
         field's value does. If any of the deleted cells may be a
         reference, we must emit modify events for each of the related
         objects. *)
      compute_other_table_events tblname kvs |> emit_events ~kind:Modify
  | Create (tblname, new_objref, kvs) ->
      (* Emit an add event for the new object. *)
      let getter = find_get_record tblname ~__context ~self:new_objref in
      let snapshot = getter () in
      emit_events ~kind:Add [(tblname, new_objref, snapshot)] ;
      (* Emit modification events for any newly-referenced objects. *)
      compute_other_table_events tblname kvs |> emit_events ~kind:Modify

let database_callback event db =
  let __context = Context.make __MODULE__ in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> database_callback_inner event db ~__context)
    (fun () -> Context.complete_tracing __context)
