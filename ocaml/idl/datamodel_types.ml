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

(** Data Model and Message Specification for Xen Management Tools *)

(*

  IMPORTANT NOTICE: This file is a critical dependency to the whole
  tree.

  ***********************************************************************
  PLEASE DO NOT MODIFY WITHOUT CONSULTING OTHER OCAML PROGRAMMERS
  FIRST
  ***********************************************************************

*)

module Date = struct
	include Date
	let iso8601_of_rpc rpc = Date.of_string (Rpc.string_of_rpc rpc)
	let rpc_of_iso8601 date = Rpc.rpc_of_string (Date.to_string date)
end

(* useful constants for product vsn tracking *)
let oss_since_303 = Some "3.0.3"
let rel_george = "george"
let rel_orlando = "orlando"
let rel_orlando_update_1 = "orlando-update-1"
let rel_symc = "symc"
let rel_miami = "miami"
let rel_rio = "rio"
let rel_midnight_ride = "midnight-ride"
let rel_cowley = "cowley"
let rel_boston = "boston"
let rel_tampa = "tampa"
let rel_clearwater = "clearwater"
let rel_vgpu_tech_preview = "vgpu-tech-preview"
let rel_vgpu_productisation = "vgpu-productisation"
let rel_clearwater_felton = "clearwater-felton"
let rel_clearwater_whetstone = "clearwater-whetstone"
let rel_creedence = "creedence"
let rel_cream = "cream"
let rel_dundee = "dundee"

let release_order =
	[ rel_rio
	; rel_miami
	; rel_symc
	; rel_orlando
	; rel_orlando_update_1
	; rel_george
	; rel_midnight_ride
	; rel_cowley
	; rel_boston
	; rel_tampa
	; rel_clearwater
	; rel_vgpu_tech_preview
	; rel_vgpu_productisation
	; rel_clearwater_felton
	; rel_clearwater_whetstone
	; rel_creedence
	; rel_cream
	; rel_dundee
	]

exception Unknown_release of string
(* ordering function on releases *)
let release_leq x y =
  let rec posn_in_list i x l =
    match l with
      [] -> raise (Unknown_release x)
    | r::rs -> if r=x then i else posn_in_list (i+1) x rs in
  (posn_in_list 0 x release_order) <= (posn_in_list 0 y release_order)

(** Types of object fields. Accessor functions are generated for each field automatically according to its type and qualifiers. *)
type ty =
    | String | Int | Float | Bool | DateTime 
    | Enum of string * (string * string) list
    | Set of ty
    | Map of ty * ty
    | Ref of string
    | Record of string
    with rpc

type api_value =
    VString of string
  | VInt of int64
  | VFloat of float
  | VBool of bool
  | VDateTime of Date.iso8601
  | VEnum of string
  | VMap of (api_value*api_value) list
  | VSet of api_value list
  | VRef of string
	with rpc
	
(** Each database field has a qualifier associated with it: *)
type qualifier =
	| RW        (** Read-write database field whose initial value is specified at the
	              * time of object construction. *)
	| StaticRO  (** Read-only database field whose final value is specified at the time
	              * of object construction. *)
	| DynamicRO (** Read-only database field whose value is computed dynamically and
	              * not specified at the time of object construction. *)
	with rpc
	
(** Release keeps track of which versions of opensource/internal products fields and messages are included in *)
type release = {
  opensource: string list;
  internal: string list;
  internal_deprecated_since: string option; (* first release we said it was deprecated *)
} with rpc

type lifecycle_change =
	| Prototyped
	| Published
	| Extended
	| Changed
	| Deprecated
	| Removed

and lifecycle_transition = lifecycle_change * string * string
with rpc

(** Messages are tagged with one of these indicating whether the message was
    specified explicitly in the datamodel, or is one of the automatically
    generated ones. If automatically generated, the tag tells you where it came
    from: this is needed for the server implementation. *)
type tag = 
    | FromField of field_op * field
    | FromObject of obj_op
    | Custom

and field_op = Getter | Setter | Add | Remove

and private_op = GetDBRecord | GetDBAll | Copy

and obj_op = Make | Delete | GetByUuid | GetByLabel | GetRecord | GetAll | GetAllRecordsWhere | GetAllRecords
    | Private of private_op

and param = {param_type:ty; param_name:string; param_doc:string; param_release: release; param_default: api_value option}

and doc_tag = VM_lifecycle | Snapshots | Networking | Memory | Windows

(** Types of RPC messages; in addition to those generated for object fields *)
and message = { 
    msg_name: string;
    msg_params: param list;
    msg_result: (ty * string) option;
    msg_errors: error list;
    msg_doc: string;
    msg_async: bool;
    msg_session: bool;
    msg_secret: bool; (* don't put stuff in logs *)
    msg_pool_internal: bool; (* only allow on "pool-login" sessions *)
    msg_db_only: bool; (* this is a db_* only message; not exposed through api *)
    msg_release: release;
    msg_lifecycle: lifecycle_transition list;
    msg_has_effect: bool; (* if true it appears in the custom operations *)
    msg_force_custom: qualifier option; (* unlike msg_has_effect, msg_force_custom=Some(RO|RW) always forces msg into custom operations, see gen_empty_custom.ml *)
    msg_no_current_operations: bool; (* if true it doesnt appear in the current operations *)
    msg_tag: tag;
    msg_obj_name: string;
    msg_custom_marshaller: bool;
    msg_hide_from_docs: bool; (* don't list the function in the documentation and do not include it in the SDK *)
    msg_allowed_roles: string list option;
    msg_map_keys_roles: (string * (string list option)) list;
    msg_doc_tags: doc_tag list;
} 

and field = {
    release: release;
    lifecycle: lifecycle_transition list;
    field_persist: bool;
    default_value: api_value option;
    internal_only: bool;
    qualifier: qualifier;
    field_name: string;
    full_name: string list;
    ty: ty;
    field_description: string;
    field_has_effect: bool;
    field_ignore_foreign_key: bool;
    field_setter_roles: string list option;
    field_getter_roles: string list option;
    field_map_keys_roles: (string * (string list option)) list;
    field_doc_tags: doc_tag list;
} 

and error = { 
    err_name: string;
    err_params: string list;
    err_doc: string;
}

and mess = {
    mess_name: string;
    mess_doc: string;
} with rpc

(** Getters and Setters will be generated for each field, depending on the qualifier. 
    Namespaces allow fields to be grouped together (and this can get reflected in the XML
    document structure)
*)
type content =
    | Field of field                     (** An individual field *)
    | Namespace of string * content list (** A nice namespace for a group of fields *)
	with rpc
	
(* Note: there used be more than 2 persist_options -- that's why it isn't a bool.
   I figured even though there's only 2 now I may as well leave it as an enumeration type.. *)

type persist_option = PersistNothing | PersistEverything with rpc
(* PersistEverything - all creates/writes persisted;
   PersistNothing - no creates/writes to this table persisted *)

(** An object (or entity) is represented by one of these: *)
type obj = {
	name : string;
	description : string;
	obj_lifecycle: lifecycle_transition list;
	contents : content list;
	messages : message list;
	doccomments : (string * string) list;
	gen_constructor_destructor: bool;
	force_custom_actions: qualifier option; (* None,Some(RW),Some(StaticRO) *)
	obj_allowed_roles: string list option; (* for construct, destruct and explicit obj msgs*)
	obj_implicit_msg_allowed_roles: string list option; (* for all other implicit obj msgs*)
	gen_events: bool;
	persist: persist_option;
	obj_release: release;
	in_database: bool; (* If the object is in the database *)
	obj_doc_tags: doc_tag list;
} with rpc

(* val rpc_of_obj : obj -> Rpc.t *)
(* let s = Jsonrpc.to_string (rpc_of_obj o) *)

(** A relation binds two fields together *)
type relation = (string * string) * (string * string) 

(* Check if a value is of a given type *)
let rec type_checks v t =
  let all_true l = List.fold_left (fun env v->env && v) true l in
  match v, t with
    VString _, String -> true
  | VInt _, Int -> true
  | VFloat _, Float -> true
  | VBool _, Bool -> true
  | VDateTime _, DateTime -> true
  | VEnum ev, Enum (_,enum_spec) ->
      let enum_possibles = List.map fst enum_spec in
      List.mem ev enum_possibles
  | VMap vvl, Map (t1,t2) ->
      all_true (List.map (fun (k,v)->type_checks k t1 && type_checks v t2) vvl)
  | VSet vl, Set t ->
      all_true (List.map (fun v->type_checks v t) vl)
  | VRef r, Ref _ -> true
  | _, _ -> false

module TypeToXML = struct

  let string x = Xml.Element(x, [], [])
  let box tag vs = Xml.Element(tag, [], vs)

  let rec marshal_ = function
    | String         -> string "string"
    | Int            -> string "int"
    | Float          -> string "float"
    | Bool           -> string "bool"
    | DateTime       -> string "datetime"
    | Enum (name, _) -> box "enum" [ string name ]
    | Ref x          -> box "ref" [ string x ]
    | Set ty         -> box "set" [ marshal_ ty ]
    | Map (a, b)     -> box "map" [ marshal_ a; marshal_ b ]
    | Record x       -> box "record" [ string x ]

  let marshal = function
    | None   -> string "none"
    | Some x -> box "some" [ marshal_ x ]

  let rec unmarshal_ = function
    | Xml.Element("string", [], [])                    -> String
    | Xml.Element("int", [], [])                       -> Int
    | Xml.Element("float", [], [])                     -> Float
    | Xml.Element("datetime", [], [])                  -> DateTime
    | Xml.Element("enum", [], [Xml.Element(name, [], [])]) -> Enum(name, [])
    | Xml.Element("ref", [],  [Xml.Element(name, [], [])]) -> Ref name
    | Xml.Element("set", [], [t])                      -> Set(unmarshal_ t)
    | Xml.Element("map", [], [a;b])                    -> Map(unmarshal_ a, unmarshal_ b)
    | _ -> failwith "Type unmarshal error"

  let unmarshal = function
    | Xml.Element("none", [], [])  -> None
    | Xml.Element("some", [], [x]) -> Some (unmarshal_ x)
    | _ -> failwith "Type unmarshal error"

end
