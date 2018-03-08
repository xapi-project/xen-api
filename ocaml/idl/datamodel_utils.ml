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
open Datamodel
open Datamodel_common
open Datamodel_types
open Dm_api
open Printf
open Xapi_stdext_std

(** Utility functions relating to the types in the datamodel *)
module Types = struct
  (** Print a representation of the datamodel type suitable for debugging and error messages *)
  let rec to_string ty = rpc_of_ty ty |> Rpc.to_string

  let rec fold_left f accu ty =
    let accu = f accu ty in
    match ty with
    | Set ty -> fold_left f accu ty
    | Map(key, value) -> fold_left f (fold_left f accu key) value
    | _ -> accu

  (** Extract a list of all the types used in an object: *)
  let rec of_content = function
    | Field f -> [ f.ty ]
    | Namespace(_, fields) -> List.concat (List.map of_content fields)

  (** Decompose a recursive type into a list of component types
      (eg a Set(String) -> String :: Set(String) ) *)
  let rec decompose = function
    | Set x as y -> y :: decompose x
    | Map (a, b) as y -> y :: decompose a @ decompose b
    | x -> [ x ]

  (** All types in a list of objects (automatically decomposes) *)
  let of_objects system =
    let fields = List.concat (List.map (fun x -> x.contents) system) in
    let field_types = List.concat (List.map of_content fields) in

    let messages = List.concat (List.map (fun x -> x.messages) system) in
    let return_types =
      let aux accu msg = match msg.msg_result with
        | None -> accu
        | Some(ty, _) -> ty :: accu in
      List.fold_left aux [] messages in
    let param_types =
      List.map (fun p -> p.param_type)
        (List.concat (List.map (fun x -> x.msg_params) messages)) in
    let selves = List.map (fun obj -> Ref(obj.name)) system in
    let set_self = List.map (fun t -> Set(t)) selves in

    let all = Listext.List.setify (selves @ set_self @ field_types @ return_types @ param_types) in
    Listext.List.setify (List.concat (List.map decompose all))

end

(** Functions for processing relationships from the model *)
module Relations = struct

  (** Compute how many times a type <a> appears in <inb>: `None `One or `Many:
      NB Sets and Maps are `Many, not single instances.
      This is only used for computing the class relationships diagram. *)
  let rec of_types a inb = match inb with
    | Set x -> if of_types a x = `None then `None else `Many
    | Map(x, y) -> if of_types a x = `None && of_types a y = `None
      then `None else `Many
    | x -> if a = x then `One else `None

  let classify api ((a, a_field_name), (b, b_field_name)) =
    let a_field = get_field_by_name api ~objname:a ~fieldname:a_field_name
    and b_field = get_field_by_name api ~objname:b ~fieldname:b_field_name in
    of_types (Ref b) a_field.ty, of_types (Ref a) b_field.ty

  let string_of_classification = function
    | `One, `One -> "one-to-one"
    | `One, `Many -> "one-to-many"
    | `Many, `One -> "many-to-one"
    | `Many, `Many -> "many-to-many"
    | _, _ -> "unknown type"

  let other_end_of api ((a, b) as one_end) =
    let rels = relations_of_api api in
    match (List.concat (List.map (function
        | (x, other_end) when x = one_end -> [ other_end ]
        | (other_end, x) when x = one_end -> [ other_end ]
        | _ -> []) rels)) with
    | [ other_end ] -> other_end
    | [] -> failwith (Printf.sprintf "Couldn't find other end of relation (%s,%s)" a b)
    | _ -> failwith ("Found multiple other ends of relation?!")

  let is_in_relation api x =
    let rels = relations_of_api api in
    List.mem_assoc x rels || (List.mem_assoc x (List.map (fun (k, v) -> v, k) rels))

end

(** Compute a flat list of fields from a datamodel object *)
let fields_of_obj (x: obj) : field list =
  let rec of_contents = function
    | Namespace(_, xs) -> List.concat (List.map of_contents xs)
    | Field x -> [ x ] in
  List.concat (List.map of_contents x.contents)

(* True if an object has a label (and therefore should have a get_by_name_label message *)
let obj_has_get_by_name_label x =
  let all_fields = fields_of_obj x in
  List.filter (fun fld -> fld.full_name = [ "name"; "label" ]) all_fields <> []

(* True if an object has tags (and therefore should have a get_tags message *)
let obj_has_get_tags x =
  let all_fields = fields_of_obj x in
  List.filter (fun fld -> fld.full_name = [ "tags" ]) all_fields <> []

(** XXX: unfortunately we don't mark which parameters of a message refer to the self;
    return the first parameter of the correct type *)
let find_self_parameter (msg: message) =
  match List.filter (fun p -> p.param_type = Ref msg.msg_obj_name) msg.msg_params with
  | {param_name=x} :: _ -> x
  | _ -> failwith (Printf.sprintf "Failed to determine self parameter for message %s" msg.msg_name)

let plural name =
  if Xstringext.String.endswith "metrics" name then
    name ^ " instances"
  else
    name ^ "s"

let default_doccomments =
  ["create",
   (fun x ->
      sprintf "Create a new %s instance, and return its handle." x.name);
   "destroy",
   (fun x ->
      sprintf "Destroy the specified %s instance." x.name);
   "get_by_uuid",
   (fun x ->
      sprintf "Get a reference to the %s instance with the specified UUID."
        x.name);
   "get_by_name_label",
   (fun x ->
      sprintf "Get all the %s instances with the given label." x.name);
   "get_record",
   (fun x ->
      sprintf "Get a record containing the current state of the given %s." x.name);
   "get_record_internal",
   (fun x ->
      sprintf "returns a record containing the state of an instance of class %s"
        x.name);
   "get_allowed_messages",
   (fun _ ->
      sprintf "dynamically compute the set of message names which it is legal to send to the object at this point");
   "get_all",
   (fun x ->
      sprintf "Return a list of all the %s known to the system."
        (plural x.name));
   "get_all_records",
   (fun x ->
      sprintf "Return a map of %s references to %s records for all %s known to the system."
        x.name x.name (plural x.name));
   "copy",
   (fun x ->
      sprintf "returns a reference to an object which is a shallow-copy of the original. NB all Set(Ref _) fields will be empty in the duplicate.");
  ]


let doccomment (x : obj) (meth : string) : string =
  if List.mem_assoc meth x.doccomments then
    List.assoc meth x.doccomments
  else
    (List.assoc meth default_doccomments) x

let get_lifecycle (x : obj) (meth : string) : lifecycle_transition list =
  if List.mem_assoc meth x.msg_lifecycles then
    List.assoc meth x.msg_lifecycles
  else
    x.obj_lifecycle

(**
 * The C bindings set this to get the self variable named after the class,
 * as opposed to being called "self".  It also means that constructor argument
 * records are called "record" as oppsed to "args".
*)
let named_self = ref false

let self_of_obj x =
  let self_name = if !named_self then x.name else _self in
  {param_type=Ref x.name; param_name=self_name; param_doc="reference to the object";
   param_release=x.obj_release; param_default=None}

(** Compute the list of messages corresponding to a single field *)
let new_messages_of_field x order fld =
  let self = self_of_obj x in
  let prefix prefix = prefix ^ (String.concat "_" fld.full_name) in
  let common = { msg_name = ""; msg_params = []; msg_result = None;
                 msg_errors = [];
                 msg_doc = "no documentation available";
                 msg_async = false;
                 msg_session = true;
                 msg_secret = false;
                 msg_release = fld.release;
                 msg_lifecycle = fld.lifecycle;
                 msg_has_effect = fld.field_has_effect;
                 msg_force_custom = x.force_custom_actions;
                 msg_no_current_operations = false;
                 msg_tag = Custom;
                 msg_obj_name = x.name;
                 msg_custom_marshaller = false;
                 msg_hide_from_docs = false;
                 msg_pool_internal = false;
                 msg_db_only = fld.internal_only;
                 msg_allowed_roles = None;
                 msg_map_keys_roles = [];
                 msg_doc_tags = [];
                 msg_forward_to = None
               } in
  let getter = { common with
                 msg_name = prefix "get_";
                 msg_params = [ self ];
                 msg_result = Some (fld.ty, "value of the field");
                 msg_errors = [];
                 msg_doc = (Printf.sprintf
                              "Get the %s field of the given %s."
                              (String.concat "/" fld.full_name) x.name);
                 msg_allowed_roles = fld.field_getter_roles;
                 msg_tag = FromField(Getter, fld) } in
  let setter = { common with
                 msg_name = prefix "set_";
                 msg_params = [ self;
                                {param_type=fld.ty; param_name=(if !named_self then fld.field_name else "value"); param_doc="New value to set";
                                 param_release=fld.release; param_default=None}
                              ];
                 msg_result = None;
                 msg_errors = [];
                 msg_doc = (Printf.sprintf
                              "Set the %s field of the given %s."
                              (String.concat "/" fld.full_name) x.name);
                 msg_allowed_roles = fld.field_setter_roles;
                 msg_tag = FromField(Setter, fld) } in
  (* Set(Ref _) fields in a many-to-many generate symmetrical add_to, remove_from etc *)
  let is_many_to_many =
    let api = Datamodel.all_api in
    let this = x.name, fld.field_name in
    Relations.is_in_relation api this &&
    (Relations.classify api (this,(Relations.other_end_of api this)) = (`Many, `Many)) in

  match (fld.ty, fld.field_ignore_foreign_key, is_many_to_many) with
  | Set(Ref _), false, false -> if order = 0 then [getter] else []
  | Set(t), _, _ ->
    if order = 0 then [getter] else [
      setter; (* only makes sense to the database *)
      { common with
        msg_name = prefix "add_";
        msg_params = [ self;
                       {param_type=t; param_name="value"; param_doc="New value to add"; param_release=fld.release; param_default=None} ];
        msg_result = None;
        msg_doc = (sprintf
                     "Add the given value to the %s field of the given %s.  If the value is already in that Set, then do nothing."
                     (String.concat "/" fld.full_name) x.name);
        msg_allowed_roles = fld.field_setter_roles;
        msg_tag = FromField(Add, fld) };
      { common with
        msg_name = prefix "remove_";
        msg_params = [ self;
                       {param_type=t; param_name="value"; param_doc="Value to remove"; param_release=fld.release; param_default=None} ];
        msg_result = None;
        msg_doc = (sprintf
                     "Remove the given value from the %s field of the given %s.  If the value is not in that Set, then do nothing."
                     (String.concat "/" fld.full_name) x.name);
        msg_allowed_roles = fld.field_setter_roles;
        msg_tag = FromField(Remove, fld) };
    ]
  | Map(k, v), _, _ ->
    if order = 0 then [getter] else [
      setter; (* only makes sense to the database *)
      { common with
        msg_name = prefix "add_to_";
        msg_params = [ self;
                       {param_type=k; param_name="key"; param_doc="Key to add"; param_release=fld.release; param_default=None};
                       {param_type=v; param_name="value"; param_doc="Value to add"; param_release=fld.release; param_default=None}];
        msg_result = None;
        msg_doc = (sprintf
                     "Add the given key-value pair to the %s field of the given %s."
                     (String.concat "/" fld.full_name) x.name);
        msg_allowed_roles = fld.field_setter_roles;
        msg_map_keys_roles = List.map (fun (k,(w))->(k,w)) fld.field_map_keys_roles;
        msg_tag = FromField(Add, fld) };
      { common with
        msg_name = prefix "remove_from_";
        msg_params = [ self;
                       {param_type=k; param_name="key"; param_doc="Key to remove"; param_release=fld.release; param_default=None} ];
        msg_result = None;
        msg_doc = (sprintf
                     "Remove the given key and its corresponding value from the %s field of the given %s.  If the key is not in that Map, then do nothing."
                     (String.concat "/" fld.full_name) x.name);
        msg_allowed_roles = fld.field_setter_roles;
        msg_map_keys_roles = List.map (fun (k,(w))->(k,w)) fld.field_map_keys_roles;
        msg_tag = FromField(Remove, fld) };
    ]
  | t, _, _ -> [
      if order = 0 then getter else setter
    ]

let all_new_messages_of_field obj fld =
  new_messages_of_field obj 0 fld @ (new_messages_of_field obj 1 fld)

(** Compute a list of all messages associated with an object including the
    implicit ones.
    NB this list requires filtering before being used for (eg) a client *)
let messages_of_obj (x: obj) document_order : message list =
  let all_fields = fields_of_obj x in
  let self = self_of_obj x in
  (* Generate appropriate get/set/add/remove messages for each field.
     Note we don't consider whether anything is RO/RW etc - this list
     needs to be filtered before getting to the client *)

  (* Dummy message *)
  let common = { msg_secret=false; msg_name=""; msg_params=[]; msg_result=None; msg_errors = []; msg_doc="";
                 msg_async=false; msg_custom_marshaller = false; msg_db_only = false;
                 msg_no_current_operations = false;
                 msg_hide_from_docs = false; msg_pool_internal = false;
                 msg_session=false; msg_release=x.obj_release; msg_lifecycle=x.obj_lifecycle; msg_has_effect=false; msg_tag=Custom;
                 msg_force_custom = x.force_custom_actions;
                 msg_allowed_roles = None;
                 msg_map_keys_roles = [];
                 msg_obj_name=x.name;
                 msg_doc_tags = [];
                 msg_forward_to = None;
               } in
  (* Constructor *)
  let ctor = let name = "create" in { common with
                                      msg_name = name;
                                      msg_params = [ {param_type=Record x.name;
                                                      param_name=(if !named_self then "record" else "args");
                                                      param_doc="All constructor arguments";
                                                      param_release=x.obj_release; param_default = None
                                                     }];
                                      msg_result = Some (Ref x.name, "reference to the newly created object");
                                      msg_doc = doccomment x name;
                                      msg_lifecycle = get_lifecycle x name;
                                      msg_async = true;
                                      msg_session = true;
                                      msg_has_effect = true;
                                      msg_allowed_roles = x.obj_allowed_roles;
                                      msg_tag = FromObject Make } in
  (* Destructor *)
  let dtor = let name = "destroy" in { common with
                                       msg_name = name;
                                       msg_params = [ self ];
                                       msg_result = None;
                                       msg_doc = doccomment x name;
                                       msg_lifecycle = get_lifecycle x name;
                                       msg_async = true;
                                       msg_session = true;
                                       msg_has_effect = true;
                                       msg_allowed_roles = x.obj_allowed_roles;
                                       msg_tag = FromObject Delete } in
  (* Get by UUID *)
  let uuid = let name = "get_by_uuid" in { common with
                                           msg_name = name;
                                           msg_params = [ {param_type=String; param_name="uuid"; param_doc="UUID of object to return"; param_release=x.obj_release; param_default = None} ];
                                           msg_result = Some (Ref x.name, "reference to the object");
                                           msg_doc = doccomment x name;
                                           msg_lifecycle = get_lifecycle x name;
                                           msg_async = false;
                                           msg_session = true;
                                           msg_has_effect = false;
                                           msg_allowed_roles = x.obj_implicit_msg_allowed_roles;
                                           msg_tag = FromObject GetByUuid } in
  (* Get by label *)
  let get_by_name_label = let name = "get_by_name_label" in { common with
                                                              msg_name = name;
                                                              msg_params = [ {param_type=String; param_name="label"; param_doc="label of object to return"; param_release=x.obj_release; param_default = None} ];
                                                              msg_result = Some (Set(Ref x.name), "references to objects with matching names");
                                                              msg_doc = doccomment x name;
                                                              msg_lifecycle = get_lifecycle x name;
                                                              msg_async = false;
                                                              msg_session = true;
                                                              msg_has_effect = false;
                                                              msg_allowed_roles = x.obj_implicit_msg_allowed_roles;
                                                              msg_tag = FromObject GetByLabel } in
  (* Get Record *)
  let get_record = let name = "get_record" in { common with
                                                msg_name = name;
                                                msg_params = [ self ];
                                                msg_result = Some (Record x.name, "all fields from the object");
                                                msg_doc = doccomment x name;
                                                msg_lifecycle = get_lifecycle x name;
                                                msg_async = false;
                                                msg_session = true;
                                                msg_has_effect = false;
                                                msg_allowed_roles = x.obj_implicit_msg_allowed_roles;
                                                msg_tag = FromObject GetRecord } in

  (* Get Record (private db version) *)
  let get_record_internal = let name = "get_record_internal" in { common with
                                                                  msg_name = name;
                                                                  msg_params = [ self ];
                                                                  msg_result = Some (Record x.name, "all fields from the object, including implementation-only ones");
                                                                  msg_doc = doccomment x name;
                                                                  msg_lifecycle = get_lifecycle x name;
                                                                  msg_async = false;
                                                                  msg_session = true;
                                                                  msg_db_only = true;
                                                                  msg_release = {opensource=[]; internal=[]; internal_deprecated_since=None}; (* internal messages not in an any API releases... *)
                                                                  msg_has_effect = false;
                                                                  msg_tag = FromObject (Private GetDBRecord);
                                                                  msg_hide_from_docs = true;
                                                                } in

  (* Internal database-only get_all function *)
  let get_all = let name = "get_all" in { common with
                                          msg_name = name;
                                          msg_params = [];
                                          msg_result = Some(Set(Ref x.name), "references to all objects");
                                          msg_doc = doccomment x name;
                                          msg_lifecycle = get_lifecycle x name;
                                          msg_async = false;
                                          msg_session = true; (* but irrelevant because currently not exposed *)
                                          msg_release = {opensource=[]; internal=[]; internal_deprecated_since=None};
                                          msg_db_only = true;
                                          msg_has_effect = false;
                                          msg_tag = FromObject (Private GetDBAll);
                                          msg_hide_from_docs = true } in

  (* Optional public version *)
  let get_all_public = { get_all with msg_release = x.obj_release; msg_tag = FromObject GetAll; msg_hide_from_docs = false; msg_db_only = false;
                                      msg_allowed_roles = x.obj_implicit_msg_allowed_roles;
                       } in

  (* And the 'get_all_records_where' semi-public function *)
  let get_all_records_where = { get_all_public with
                                msg_name = "get_all_records_where";
                                msg_tag = FromObject GetAllRecordsWhere;
                                msg_params = [ {param_type=String; param_name="expr"; param_doc="expression representing records to fetch";
                                                param_release=x.obj_release; param_default = None}
                                             ];
                                msg_result = Some(Map(Ref x.name, Record x.name), "records of all matching objects");
                                msg_release = {opensource=[]; internal=x.obj_release.internal; internal_deprecated_since=None};
                                msg_allowed_roles = x.obj_implicit_msg_allowed_roles;
                                msg_hide_from_docs = true;
                              } in

  (* And the 'get_all_records' public function *)
  let get_all_records = let name = "get_all_records" in { get_all_public with
                                                          msg_name = name;
                                                          msg_tag = FromObject GetAllRecords;
                                                          msg_params = [ ];
                                                          msg_result = Some(Map(Ref x.name, Record x.name), "records of all objects");
                                                          msg_release = {opensource=[]; internal=x.obj_release.internal; internal_deprecated_since=None};
                                                          msg_allowed_roles = x.obj_implicit_msg_allowed_roles;
                                                          msg_doc = doccomment x name;
                                                          msg_lifecycle = get_lifecycle x name
                                                        } in

  let name_label = if obj_has_get_by_name_label x then [ get_by_name_label ] else [ ] in
  let get_all_public = if List.mem x.name expose_get_all_messages_for then [ get_all_public; get_all_records_where; get_all_records ] else [] in
  (* Always generate the ctor and dtor here, filter in 'on_client_side' below *)
  let constructor_destructor = [ctor; dtor ] in

  (* Fill in the obj_name field on all messages *)
  let messages = List.map (fun y -> { y with msg_obj_name = x.name }) x.messages in

  if not x.in_database then
    messages (* @ [ get_all; get_record; get_record_internal ]*)
  else if document_order then
    messages @
    get_all_public @
    [ get_all ] @
    List.concat (List.map (all_new_messages_of_field x) all_fields) @
    constructor_destructor @
    [ uuid; get_record ] @
    name_label @
    [ get_record_internal ]
  else
    [ get_record; get_record_internal; get_all; uuid] @
    constructor_destructor @
    name_label @
    List.concat (List.map (new_messages_of_field x 0) all_fields) @
    List.concat (List.map (new_messages_of_field x 1) all_fields) @
    messages @
    get_all_public

let add_implicit_messages ?(document_order = false) (api: api) =
  let objs = objects_of_api api
  and rels = relations_of_api api in
  let objs = List.map (fun obj ->
      (* list of all messages, existing plus implicit *)
      let messages = messages_of_obj obj document_order in
      let obj' = { obj with messages = messages } in
      obj') objs in
  Dm_api.make (objs, rels)

(* Message filter which selects only those message visible to the client *)
let on_client_side (x: message) : bool = match x with
  (* Anything that's msg_db_only is not on client-side *)
  | { msg_db_only = true } -> false

  (* Client cannot modify (set/add/remove) a non-RW field *)
  | { msg_tag = FromField((Setter|Add|Remove), { qualifier = RW }) } -> true
  | { msg_tag = FromField((Setter|Add|Remove), _) } -> false
  (* If an object is tagged with custom ctor/dtor, omit the default one *)
  | { msg_tag = FromObject(Make|Delete) } ->
    let obj = Dm_api.get_obj_by_name Datamodel.all_api ~objname:x.msg_obj_name in
    obj.gen_constructor_destructor
  | { msg_obj_name = "event" } ->
    x.msg_name <> "get_record"
  | _ -> true

let wire_name_common sep ~sync (obj: obj) (msg: message) =
  let sync_name = obj.name ^ sep ^ msg.msg_name in
  if sync
  then sync_name
  else "Async" ^ sep ^ sync_name

let wire_name = wire_name_common "."
let alternative_wire_name = wire_name_common "_"

let wire_name_of_field (fld: field) = String.concat "_" fld.full_name

let string_of_doc_tag = function
  | VM_lifecycle -> "vm-lifecycle"
  | Snapshots -> "snapshots"
  | Networking -> "networking"
  | Memory -> "memory"
  | Windows -> "windows-vm"

let string_of_lifecycle_transition = function
  | Prototyped -> "prototyped"
  | Published -> "published"
  | Extended -> "extended"
  | Changed -> "changed"
  | Deprecated -> "deprecated"
  | Removed -> "removed"

(* Check whether the last transition in an API message's lifecycle is Removed.
 * This allows us to remove API calls and re-add them, and fully list the
 * corresponding lifecycle changes. *)
let rec has_been_removed = function
  | [] -> false
  | (Removed, _, _) :: [] -> true
  | _ :: other_transitions -> has_been_removed other_transitions

