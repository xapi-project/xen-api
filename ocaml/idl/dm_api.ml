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
open Datamodel_types

(** The api is made up of objects (which contain fields and additional RPCs), and
    relationships, which specify which fields are bound together -- i.e. refer to the
    same underlying data *)
type api = (obj list) * (relation list)

(** Since api is an abstract type we need to provide functions for splitting one apart *)
let objects_of_api (objs, _) = objs
let relations_of_api (_, rels) = rels

let print_api_stats (system, relations) =
  Printf.printf "%d objects and %d relations\n"
    (List.length system) (List.length relations);
  Printf.printf "objects = [ %s ]\n"
    (String.concat "; " (List.map (fun x -> x.name) system))

let get_obj_by_name (system, relations) ~objname:name =
  match List.filter (fun obj -> obj.name = name) system with
  | [ obj ] -> obj
  | _::_ ->
    failwith (Printf.sprintf "Multiple instances of name [%s] found in system" name)
  | [] -> failwith (Printf.sprintf "Object with name [%s] not found in system" name)

let obj_exists api name =
  try
    let (_: obj) = get_obj_by_name api ~objname:name in
    true
  with e -> false

(** Retrieves the field of an obj given its name *)
let get_field_by_name api ~objname:objname ~fieldname:name =
  let obj = get_obj_by_name api ~objname in
  let rec contents = function
    | Field field :: rest when field.field_name = name -> Some field
    | Namespace(_, sub) :: rest ->
      let result = contents sub in
      if result = None then contents rest else result
    | _ :: rest -> contents rest
    | [] -> None in
  match (contents obj.contents) with
  | Some x -> x
  | _ ->
    failwith (Printf.sprintf "field not found (field %s in object %s)" name obj.name)

let field_exists api ~objname ~fieldname =
  try
    let (_: field) = get_field_by_name api ~objname ~fieldname in
    true
  with e -> false

(** Takes a predicate and a list of objects, returning the objects with all the fields
    removed for which the field applied to the predicate returned false.
    Empty objects and namespaces are removed.
    Do not use this function directly; use filter_api instead
*)
let filter_field (pred: field -> bool) (system: obj list) =
  (* NB using lists rather than options - maybe change later? *)
  let concat_map f xs = List.concat (List.map f xs) in
  let rec content = function
    | Field field as x -> if pred field then [ x ] else [ ]
    | Namespace(name, contents) -> [ Namespace(name, concat_map content contents) ] in
  (* remove empty /leaf/ namespaces *)
  let rec remove_leaf = function
    | Field _ as x -> [ x ]
    | Namespace(_, []) -> [ ] (* no children so removed *)
    | Namespace(name, contents) -> [ Namespace(name, concat_map remove_leaf contents) ] in
  let rec fixpoint f x = let result = f x in if result = x then x else fixpoint f result in
  let obj x = { x with contents =
                         let contents = concat_map content x.contents in
                         fixpoint (concat_map remove_leaf) contents } in
  List.map obj system

(** Takes a predicate and a list of objects, returning the objects with only the messages
    for which (predicate message) returned true. *)
let filter_messages (pred: message -> bool) (system: obj list) =
  let obj x = { x with messages = List.filter pred x.messages } in
  List.map obj system

(** Transforms all the fields in an API *)
let map_field (f: field -> field) (system: obj list) =
  let rec content = function
    | Field x -> Field (f x)
    | Namespace(name, contents) -> Namespace(name, List.map content contents) in
  List.map (fun x -> { x with contents = List.map content x.contents }) system

(** Removes all those relations which refer to non-existent objects or fields *)
let filter_relations ((system,relations) as api)=
  List.filter (function ((a_obj, a_name), (b_obj, b_name)) ->
      (obj_exists api a_obj) &&
      (obj_exists api b_obj) &&
      (field_exists api ~objname:a_obj ~fieldname:a_name) &&
      (field_exists api ~objname:b_obj ~fieldname:b_name)) relations

let rebuild system relations =
  (* remove all relations which refer to non-existent objects or fields *)
  let relations = filter_relations (system, relations) in
  let api = system, relations in
  api

let filter (obj: obj -> bool) (field: field -> bool) (message: message -> bool)
    ((system, relations) : api) : api =
  let system = List.filter obj system in
  let system = filter_field field system in
  let system = filter_messages message system in
  (* remove all objects with no fields *)
  let system = List.filter (fun obj -> obj.messages <> [] || obj.contents <> []) system in
  rebuild system relations

let map (field: field -> field) (message: message -> message)
    ((system, relations) : api) : api =
  let system = map_field field system in
  let system = List.map (fun obj -> { obj with messages = List.map message obj.messages }) system in
  rebuild system relations



(*
let map_api_fields (f: field -> field) ((system, relations) : api) : api =
  (map_field f system, relations)
*)
let make api = (api:api)

let check api emergency_calls =
  let truefn _ = true in
  let api' = filter truefn truefn truefn api in
  if api <> api'
  then begin
    print_endline "original:";
    print_api_stats api;
    print_endline "filtered:";
    print_api_stats api';
    failwith "filter_api seems to be broken"
  end;
  let system,relations = api' in
  (* Sanity check 1: all the objects in the relations should exist in the system *)
  List.iter (fun ((a_obj, _), (b_obj, _)) ->
      ignore (get_obj_by_name api ~objname:a_obj);
      ignore (get_obj_by_name api ~objname:b_obj)) relations;

  (* Sanity check 2: all fields mentioned in the relations should exist *)
  List.iter (fun ((a_obj, a_name), (b_obj, b_name)) ->
      ignore (get_field_by_name api ~objname:a_obj ~fieldname:a_name);
      ignore (get_field_by_name api ~objname:b_obj ~fieldname:b_name) ) relations;

  (* Sanity check 3: no side-effects for Ref fields *)

  let (_: obj list) = map_field (function { ty = Ref _; field_has_effect = true } ->
      failwith "Can't have a Ref field with a side-effect: it makes the destructors too complicated"
                                        | x -> x) system in
  (* Sanity check: all Set(Ref _) fields should be one of:
     	 1. one-to-many: the many end should be DynamicRO
     	 2. many-to-many: the many end should be DynamicRO or RW
     	 3. something else with field_ignore_foreign_key
  *)
  let rec flatten_fields fs acc =
    match fs with
      [] -> acc
    | (Field f)::fs -> flatten_fields fs (f::acc)
    | (Namespace (_,internal_fs))::fs -> flatten_fields fs (flatten_fields internal_fs acc) in
  let _ =
    let field objname = function
        { ty = Set(Ref y); qualifier = q; field_ignore_foreign_key = false } as x ->
        let relations = relations @ (List.map (fun (x, y) -> y, x) relations) in
        if not(List.mem_assoc (objname, x.field_name) relations)
        then failwith (Printf.sprintf "Set(Ref _) field is not in relations table: %s.%s" objname x.field_name);
        let other_obj, other_fld = List.assoc (objname, x.field_name) relations in
        let other_f = get_field_by_name api ~objname:other_obj ~fieldname:other_fld in
        begin match other_f.ty with
          | Set(Ref _) ->
            if q <> DynamicRO && q <> RW
            then failwith (Printf.sprintf "many-to-many Set(Ref _) is not RW or DynamicRO: %s.%s" objname x.field_name);
            if not x.field_persist
            then failwith (Printf.sprintf "many-to-many Set(Ref _) is not persistent: %s.%s" objname x.field_name);
            if not other_f.field_persist
            then failwith (Printf.sprintf "many-to-many Set(Ref _) is not persistent: %s.%s" other_obj other_fld);
          | Ref _ ->
            if q <> DynamicRO
            then failwith (Printf.sprintf "many-to-many Set(Ref _) is not DynamicRO: %s.%s" objname x.field_name)
          | ty ->
            failwith (Printf.sprintf "field in relationship has bad type (Ref or Set(Ref) only): %s.%s" other_obj other_fld)
        end
      | _ -> () in
    let obj o = List.iter (field o.name) (flatten_fields o.contents []) in
    List.iter obj (objects_of_api api) in


  (* Sanity check 4: all fields not in rel_rio and not dynamic_RO must have default values *)
  let (_: obj list) = map_field (function { qualifier=q; release={internal=ir}; default_value=None } as x ->
      if not (List.mem rel_rio ir) && not (q=DynamicRO) then
        failwith (Printf.sprintf "Field %s not in release Rio, is not DynamicRO and does not have default value specified" (String.concat "/" x.full_name))
      else x
                                        | x -> x) system in

  (* Sanity check 5: no (Set Ref _) fields can have default values *)
  let (_: obj list) = map_field (function { qualifier=q; release={internal=ir}; default_value=Some _; ty=ty; field_ignore_foreign_key=false } as x ->
      begin
        match ty with
          (Set (Ref _)) ->
          failwith (Printf.sprintf "Field %s is a (Set (Ref _)) and has a default value specified. Please remove default value." (String.concat "/" x.full_name))
        | _ -> x
      end
                                        | x -> x) system in

  (* Sanity check 6: all values specfieid in IDL must be of the right type *)
  let (_: obj list) = map_field (function { default_value=Some v; ty=ty } as x ->
      if not (type_checks v ty) then
        failwith (Printf.sprintf "Field %s has default value with wrong type." (String.concat "/" x.full_name));
      x
                                        | x -> x) system in

  (* Sanity check 7: message parameters must be in increasing order of in_product_since *)
  let are_in_vsn_order ps =
    let rec getlast l = (* TODO: move to standard library *)
      match l with [x] -> x | _::xs -> getlast xs | [] -> raise (Invalid_argument "getlast") in
    let release_lt x y = release_leq x y && x<>y in
    let in_since releases = (* been in since the lowest of releases *)
      let rec find_smallest sofar l =
        match l with
          [] -> sofar
        | "closed"::xs -> find_smallest sofar xs (* closed is not a real release, so skip it *)
        | x::xs -> if release_lt x sofar then find_smallest x xs else find_smallest sofar xs in
      find_smallest (getlast release_order |> code_name_of_release) releases in
    let rec check_vsns max_release_sofar ps =
      match ps with
        [] -> true
      | (p::rest) ->
        let param_in_product_since = in_since p.param_release.internal in
        if release_lt param_in_product_since max_release_sofar then false
        else check_vsns param_in_product_since (* <-- new max *) rest in
    check_vsns rel_rio ps
  in
  let _ = List.iter
      (fun obj ->
         List.iter
           (fun msg ->
              if msg.msg_tag=Custom &&
                 not (are_in_vsn_order msg.msg_params) then failwith (Printf.sprintf "Msg %s.%s does not have parameters in version order"
                                                                        obj.name msg.msg_name)
           )
           obj.messages
      ) system in

  (* Sanity check 8: any "emergency calls" must not support async mode of operation -- if they do then our
     server dispatch logic will try and create a task for them; since this requires database access they
     will just block indefinitely if the server is in emergency mode. *)
  let _ =
    List.iter
      (fun obj ->
         List.iter
           (fun msg ->
              if msg.msg_async && (List.mem (obj.name,msg.msg_name) emergency_calls) then
                failwith (Printf.sprintf "Msg %s.%s is marked as supports async and also appears in emergency_call list. These are mutually exclusive choices." obj.name msg.msg_name)
           ) obj.messages
      ) system in
  ()
