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
module D = Debug.Make(struct let name = "sql" end)
open D

type getrecord = unit -> Rpc.t

let get_record_table : (string, __context:Context.t -> self:string -> getrecord ) Hashtbl.t = Hashtbl.create 20

let find_get_record x ~__context ~self () : Rpc.t option =
  if Hashtbl.mem get_record_table x
  then Some (Hashtbl.find get_record_table x ~__context ~self ())
  else None

(* If a record is created or destroyed, then
   for any (Ref _) field which is one end of a relationship, need to send
   modified events for all those other objects. *)
(* we build a hashtable of these references and then look them up by object on each db write: *)
let compute_object_references_to_follow (obj_name:string) =
  let api = Datamodel.all_api in
  let objs = Dm_api.objects_of_api api in
  let obj = List.find (fun obj -> obj.Datamodel_types.name = obj_name) objs in
  let relations = Dm_api.relations_of_api api in
  let symmetric = List.concat (List.map (fun (a, b) -> [ a, b; b, a ]) relations) in
  let set = Stdext.Listext.List.setify symmetric in
  List.concat
    (List.map
       (function { Datamodel_types.ty = Datamodel_types.Ref x;
                   Datamodel_types.field_name = field_name } ->
         let this_end = obj.Datamodel_types.name, field_name in
         if List.mem_assoc this_end set
         then begin
           let other_end = List.assoc this_end set in
           let other_obj = fst other_end in
           [ other_obj, field_name ]
         end else []
               | _ -> []) (Datamodel_utils.fields_of_obj obj))
let obj_references_table : (string, (string*string) list) Hashtbl.t = Hashtbl.create 30

(* populate obj references table *)
let _ =
  List.iter
    (fun obj ->
       let obj_name = obj.Datamodel_types.name in
       Hashtbl.replace obj_references_table obj_name (compute_object_references_to_follow obj_name))
    (Dm_api.objects_of_api Datamodel.all_api)

let follow_references (obj_name:string) = Hashtbl.find obj_references_table obj_name

(** Compute a set of modify events but skip any for objects which were missing
    (must have been dangling references) *)
let events_of_other_tbl_refs other_tbl_refs =
  List.concat
    (List.map (fun (tbl, fld, x) ->
         try [ tbl, fld, x () ]
         with _ ->
           (* Probably means the reference was dangling *)
           warn "skipping event for dangling reference %s: %s" tbl fld;
           []) other_tbl_refs)

open Db_cache_types
open Db_action_helper

let database_callback event db =
  let context = Context.make "eventgen" in


  match event with
  | RefreshRow (tblname, objref) ->
    (* Generate event *)
    let snapshot = find_get_record tblname ~__context:context ~self:objref in
    let record = snapshot() in
    begin match record with
      | None ->
        error "Failed to send MOD event for %s %s" tblname objref;
        Printf.printf "Failed to send MOD event for %s %s\n%!" tblname objref;
      | Some record ->
        events_notify ~snapshot:record tblname "mod" objref;
    end
  | WriteField (tblname, objref, fldname, newval) ->
    (* Generate event *)
    let snapshot = find_get_record tblname ~__context:context ~self:objref in
    let record = snapshot() in
    begin match record with
      | None ->
        error "Failed to send MOD event for %s %s" tblname objref;
        Printf.printf "Failed to send MOD event for %s %s\n%!" tblname objref;
      | Some record ->
        events_notify ~snapshot:record tblname "mod" objref;
    end;
  | PreDelete(tblname, objref) ->
    begin match find_get_record tblname ~__context:context ~self:objref () with
      | None ->
        error "Failed to generate DEL event for %s %s" tblname objref;
        (*				Printf.printf "Failed to generate DEL event for %s %s\n%!" tblname objref; *)
      | Some snapshot ->
        events_notify ~snapshot tblname "del" objref
    end
  | Delete(tblname, objref, kv) ->
    let other_tbl_refs = follow_references tblname in
    let other_tbl_refs =
      List.fold_left (fun accu (remote_tbl,fld) ->
          let fld_value = List.assoc fld kv in
          let fld_value = Schema.Value.Unsafe_cast.string fld_value in
            (remote_tbl, fld_value, find_get_record remote_tbl ~__context:context ~self:fld_value) :: accu)
        [] other_tbl_refs in
    let other_tbl_ref_events = events_of_other_tbl_refs other_tbl_refs in
    List.iter (function
        | tbl, ref, None ->
          error "Failed to generate MOD event on %s %s" tbl ref;
          (*					Printf.printf "Failed to generate MOD event on %s %s\n%!" tbl ref; *)
        | tbl, ref, Some s ->
          events_notify ~snapshot:s tbl "mod" ref
      ) other_tbl_ref_events

  | Create (tblname, new_objref, kv) ->
    let snapshot = find_get_record tblname ~__context:context ~self:new_objref in
    let other_tbl_refs = follow_references tblname in
    let other_tbl_refs =
      List.fold_left (fun accu (tbl,fld) ->
          let fld_value = List.assoc fld kv in
            let fld_value = Schema.Value.Unsafe_cast.string fld_value in
            (tbl, fld_value, find_get_record tbl ~__context:context ~self:fld_value) :: accu)
        [] other_tbl_refs in
    let other_tbl_events = events_of_other_tbl_refs other_tbl_refs in
    begin match snapshot() with
      | None ->
        error "Failed to generate ADD event for %s %s" tblname new_objref;
        (*              Printf.printf "Failed to generate ADD event for %s %s\n%!" tblname new_objref; *)
      | Some snapshot ->
        events_notify ~snapshot tblname "add" new_objref;
    end;
    List.iter (function
        | tbl, ref, None ->
          error "Failed to generate MOD event for %s %s" tbl ref;
          (*                Printf.printf "Failed to generate MOD event for %s %s\n%!" tbl ref;*)
        | tbl, ref, Some s ->
          events_notify ~snapshot:s tbl "mod" ref
      ) other_tbl_events

