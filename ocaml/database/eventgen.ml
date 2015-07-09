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
  let set = Listext.List.setify symmetric in
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

open Db_cache_types
open Db_action_helper

let database_callback event db =
        (* Take a database snapshot. We don't need to worry about other
           threads racing with us and deleting data we need. *)
        let database = Db_ref.in_memory (ref (ref db)) in
        let __context = Context.make ~database "eventgen" in

	let other_tbl_refs tblname = follow_references tblname in
	let other_tbl_refs_for_this_field tblname fldname =
                other_tbl_refs tblname
            |>  List.filter (fun (_, fld) -> fld=fldname)
            |>  List.map fst in

	let is_valid_ref = function
                | Schema.Value.String r ->
                        begin
	        	        try
	        	        	ignore(Database.table_of_ref r db);
	        	        	true
	        	        with _ -> false
                        end
                | _ -> false in

	match event with
		| RefreshRow (tblname, objref) ->
			(* Generate event *)
			let snapshot_fn = find_get_record tblname ~__context ~self:objref in
			events_notify ~snapshot_fn tblname "mod" objref;
		| WriteField (tblname, objref, fldname, oldval, newval) ->
			let events_old_val = 
				if is_valid_ref oldval then 
                                        let oldval = Schema.Value.Unsafe_cast.string oldval in
					List.map (fun tbl ->
                                                let snapshot_fn = find_get_record tbl ~__context ~self:oldval in
                                                tbl, oldval, snapshot_fn)
                                                (other_tbl_refs_for_this_field tblname fldname) 
				else [] in
			let events_new_val =
				if is_valid_ref newval then
                                        let newval = Schema.Value.Unsafe_cast.string newval in
					List.map (fun tbl ->
                                                let snapshot_fn = find_get_record tbl ~__context ~self:newval in
                                                tbl, newval, snapshot_fn)
                                                (other_tbl_refs_for_this_field tblname fldname)
				else [] 
			in
			List.iter (fun (tbl, ref, snapshot_fn) ->
				events_notify ~snapshot_fn tbl "mod" ref
			) events_old_val;
			let snapshot_fn = find_get_record tblname ~__context ~self:objref in
			events_notify ~snapshot_fn tblname "mod" objref;
			List.iter (fun (tbl, ref, snapshot_fn) ->
				events_notify ~snapshot_fn tbl "mod" ref
			) events_new_val;
		| PreDelete(tblname, objref) ->
                        let snapshot_fn = find_get_record tblname ~__context ~self:objref in
			events_notify ~snapshot_fn tblname "del" objref
		| Delete(tblname, objref, kv) ->
			let other_tbl_refs = follow_references tblname in
			let other_tbl_refs =
				List.fold_left (fun accu (remote_tbl,fld) ->
					let fld_value = List.assoc fld kv in
					if is_valid_ref fld_value then begin
                                                let fld_value = Schema.Value.Unsafe_cast.string fld_value in
                                                let snapshot_fn = find_get_record remote_tbl ~__context ~self:fld_value in
					        (remote_tbl, fld_value, snapshot_fn) :: accu 
                                        end else accu) 
					[] other_tbl_refs in
			List.iter (fun (tbl, ref, snapshot_fn) ->
				events_notify ~snapshot_fn tbl "mod" ref
			) other_tbl_refs

		| Create (tblname, new_objref, kv) ->
			let other_tbl_refs = follow_references tblname in
			let other_tbl_refs =
				List.fold_left (fun accu (tbl,fld) ->
					let fld_value = List.assoc fld kv in
                                        if is_valid_ref fld_value then begin
                                                let fld_value = Schema.Value.Unsafe_cast.string fld_value in
                                                let snapshot_fn = find_get_record tbl ~__context ~self:fld_value in
                                                (tbl, fld_value, snapshot_fn) :: accu
                                        end else accu) 
					[] other_tbl_refs in
			let snapshot_fn = find_get_record tblname ~__context ~self:new_objref in
			events_notify ~snapshot_fn tblname "add" new_objref;
			List.iter (fun (tbl, ref, snapshot_fn) ->
				events_notify ~snapshot_fn tbl "mod" ref
			) other_tbl_refs
