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
module D = Debug.Debugger(struct let name = "sql" end)
open D

type getrecord = unit -> XMLRPC.xmlrpc 
let get_record_table : (string, __context:Context.t -> self:string -> getrecord ) Hashtbl.t = Hashtbl.create 20
let find_get_record x = try Hashtbl.find get_record_table x with Not_found as e -> debug "Failed to find get_record function for class: %s" x; raise e
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
		     Datamodel_types.field_name = field_name } as f -> 
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
