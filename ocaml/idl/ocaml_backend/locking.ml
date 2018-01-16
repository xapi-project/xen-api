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

(* ---------------------------------------------------------------------------------------------
   These functions are called by used to statically determine which locks we should acquire when
    autogenerating server functor.
   --------------------------------------------------------------------------------------------- *)

module DT = Datamodel_types
module DM = Datamodel
module DU = Datamodel_utils
module OU = Ocaml_utils
module Client = Gen_client
open DT

(** For a particular operation, decide which locks are required *)
let of_message (obj: obj) (x: message) =
  let this_class = obj.DT.name in
  let self = [ this_class, Client._self ] in
  let session = [ DM._session, Client._session_id ] in

  (* Take all the arguments of type Ref x, select those which are part of
     a relationship (associated with a foreign Set(Ref _) and compute the
     list of foreign objects to lock *)
  let from_ref_relationships (x: obj) =
    let fields = DU.fields_of_obj x in
    let consider = function
      | { DT.ty = DT.Ref obj; field_name = field_name } as arg ->
        let this_end = this_class, field_name in
        begin
          try
            let class', name' = DU.Relations.other_end_of DM.all_api this_end in
            if obj <> class' then failwith "relationship lookup failed";
            let fld' = Dm_api.get_field_by_name DM.all_api ~objname:class' ~fieldname:name' in
            if fld'.DT.field_has_effect
            then [ class', OU.ocaml_of_record_field arg.full_name ]
            else []
          with Failure(_) -> []
        end
      | _ -> [] in
    List.concat (List.map consider fields) in

  let session = if x.msg_session && this_class = DM._session then session else [] in

  let all = match x.msg_tag with
    | FromField(_, { DT.field_has_effect = false }) ->
      (* for simple cases, the database handles these. Recall that for
         	 Set(Ref x) types, the magic is in the constructor of the other
         	 end of the relation *)
      []
    | FromField(_, { DT.field_has_effect = true }) ->
      self
    | FromObject(Make) ->
      (* Lock this instance and, for any Ref x argument where it corresponds to the
         	 'N' side of a 1-N relationship and where the '1' side field has
         	 field_has_effect = true.
         Example: creating a VBD with a VM ref *)
      from_ref_relationships obj (* NB self doesn't exist yet *)
    | FromObject(Delete) ->
      (* Lock this instance and, for any Ref x field which is in a relationship,
         	 lock those other objects if they have field_has_effect = true
         	 Example: deleting a VBD
         	 (Note: deleting a VM containing a Set(Ref VBD) won't cause a side-effect
          on the VBD because we currently ban fields of type Ref _ having
         	  field_has_effect = true) *)
      (* XXX: need to fetch IDs from the database *)
      self
(*
      self @ (from_ref_relationships obj)
*)
    | FromObject(_) ->
      (* Database handles everything else *)
      []
    | Custom ->
      (* Lock the session and the "self" parameter (NB not every message has a self, eg login) *)
      let self_lock = try [ this_class, DU.find_self_parameter x ] with _ -> [] in
      session @ self_lock
  in Xapi_stdext_std.Listext.List.setify all
