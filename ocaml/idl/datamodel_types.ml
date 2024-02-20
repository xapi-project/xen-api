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
  open Xapi_stdext_date
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

let rel_indigo = "indigo"

let rel_dundee = "dundee"

let rel_ely = "ely"

let rel_falcon = "falcon"

let rel_inverness = "inverness"

let rel_jura = "jura"

let rel_kolkata = "kolkata"

let rel_lima = "lima"

let rel_naples = "naples"

let rel_oslo = "oslo"

let rel_quebec = "quebec"

let rel_stockholm = "stockholm"

let rel_stockholm_psr = "stockholm_psr"

let rel_nile_preview = "nile-preview"

let rel_nile = "nile"

type api_release = {
    code_name: string option
  ; version_major: int
  ; version_minor: int
  ; branding: string
  ; release_date: string option
}

(* When you add a new release, use the version number of the latest release, "Unreleased"
   for the branding, and Some "" for the release date, until the actual values are finalised. *)

let release_order_full =
  [
    {
      code_name= Some rel_rio
    ; version_major= 1
    ; version_minor= 1
    ; branding= "XenServer 4.0"
    ; release_date= Some "August 2007"
    }
  ; {
      code_name= Some rel_miami
    ; version_major= 1
    ; version_minor= 2
    ; branding= "XenServer 4.1"
    ; release_date= Some "March 2008"
    }
  ; {
      code_name= Some rel_symc
    ; version_major= 1
    ; version_minor= 2
    ; branding= "XenServer 4.1.1"
    ; release_date= Some "" (* unknown date *)
    }
  ; {
      code_name= Some rel_orlando
    ; version_major= 1
    ; version_minor= 3
    ; branding= "XenServer 5.0"
    ; release_date= Some "September 2008"
    }
  ; {
      code_name= Some rel_orlando_update_1
    ; version_major= 1
    ; version_minor= 3
    ; branding= "XenServer 5.0 Update 1"
    ; release_date= Some "" (* unknown date *)
    }
  ; {
      code_name= None
    ; version_major= 1
    ; version_minor= 4
    ; branding= "Unreleased"
    ; release_date= None
    }
  ; {
      code_name= None
    ; version_major= 1
    ; version_minor= 5
    ; branding= "XenServer 5.0 update 3"
    ; release_date= Some "September 2008"
    }
  ; {
      code_name= Some rel_george
    ; version_major= 1
    ; version_minor= 6
    ; branding= "XenServer 5.5"
    ; release_date= Some "June 2009"
    }
  ; {
      code_name= Some rel_midnight_ride
    ; version_major= 1
    ; version_minor= 7
    ; branding= "XenServer 5.6"
    ; release_date= Some "May 2010"
    }
  ; {
      code_name= Some rel_cowley
    ; version_major= 1
    ; version_minor= 8
    ; branding= "XenServer 5.6 FP1"
    ; release_date= Some "December 2010"
    }
  ; {
      code_name= Some rel_boston
    ; version_major= 1
    ; version_minor= 9
    ; branding= "XenServer 6.0"
    ; release_date= Some "September 2011"
    }
  ; {
      code_name= Some rel_tampa
    ; version_major= 1
    ; version_minor= 10
    ; branding= "XenServer 6.1"
    ; release_date= Some "September 2012"
    }
  ; {
      code_name= Some rel_clearwater
    ; version_major= 2
    ; version_minor= 0
    ; branding= "XenServer 6.2"
    ; release_date= Some "June 2013"
    }
  ; {
      code_name= Some rel_vgpu_tech_preview
    ; version_major= 2
    ; version_minor= 0
    ; branding= "XenServer 6.2 SP1 Tech-Preview"
    ; release_date= Some "" (* unknown date *)
    }
  ; {
      code_name= Some rel_vgpu_productisation
    ; version_major= 2
    ; version_minor= 1
    ; branding= "XenServer 6.2 SP1"
    ; release_date= Some "December 2013"
    }
  ; {
      code_name= Some rel_clearwater_felton
    ; version_major= 2
    ; version_minor= 2
    ; branding= "XenServer 6.2 SP1 Hotfix 4"
    ; release_date= Some "April 2014"
    }
  ; {
      code_name= Some rel_clearwater_whetstone
    ; version_major= 2
    ; version_minor= 2
    ; branding= "XenServer 6.2 SP1 Hotfix 11"
    ; release_date= Some "October 2014"
    }
  ; {
      code_name= Some rel_creedence
    ; version_major= 2
    ; version_minor= 3
    ; branding= "XenServer 6.5"
    ; release_date= Some "January 2015"
    }
  ; {
      code_name= Some rel_cream
    ; version_major= 2
    ; version_minor= 4
    ; branding= "XenServer 6.5 SP1"
    ; release_date= Some "May 2015"
    }
  ; {
      code_name= Some rel_indigo
    ; version_major= 2
    ; version_minor= 4
    ; branding= "XenServer 6.5 SP1 Hotfix 31"
    ; release_date= Some "December 2015"
    }
  ; {
      code_name= Some rel_dundee
    ; version_major= 2
    ; version_minor= 5
    ; branding= "XenServer 7.0"
    ; release_date= Some "May 2016"
    }
  ; {
      code_name= Some rel_ely
    ; version_major= 2
    ; version_minor= 6
    ; branding= "XenServer 7.1"
    ; release_date= Some "February 2017"
    }
  ; {
      code_name= Some rel_falcon
    ; version_major= 2
    ; version_minor= 7
    ; branding= "XenServer 7.2"
    ; release_date= Some "May 2017"
    }
  ; {
      code_name= Some rel_inverness
    ; version_major= 2
    ; version_minor= 8
    ; branding= "XenServer 7.3"
    ; release_date= Some "December 2017"
    }
  ; {
      code_name= Some rel_jura
    ; version_major= 2
    ; version_minor= 9
    ; branding= "XenServer 7.4"
    ; release_date= Some "February 2018"
    }
  ; {
      code_name= Some rel_kolkata
    ; version_major= 2
    ; version_minor= 10
    ; branding= "XenServer 7.5"
    ; release_date= Some "May 2018"
    }
  ; {
      code_name= Some rel_lima
    ; version_major= 2
    ; version_minor= 11
    ; branding= "XenServer 7.6"
    ; release_date= Some "September 2018"
    }
  ; {
      code_name= Some rel_naples
    ; version_major= 2
    ; version_minor= 12
    ; branding= "Citrix Hypervisor 8.0"
    ; release_date= Some "March 2019"
    }
  ; {
      code_name= Some rel_oslo
    ; version_major= 2
    ; version_minor= 13
    ; branding= "Unreleased"
    ; release_date= None
    }
  ; {
      code_name= Some rel_quebec
    ; version_major= 2
    ; version_minor= 14
    ; branding= "Citrix Hypervisor 8.1"
    ; release_date= Some "December 2019"
    }
  ; {
      code_name= Some rel_stockholm
    ; version_major= 2
    ; version_minor= 15
    ; branding= "Citrix Hypervisor 8.2"
    ; release_date= Some "June 2020"
    }
  ; {
      code_name= Some rel_stockholm_psr
    ; version_major= 2
    ; version_minor= 15
    ; branding= "Citrix Hypervisor 8.2 Hotfix 2"
    ; release_date= Some "November 2020"
    }
  ; {
      code_name= Some rel_nile_preview
    ; version_major= 2
    ; version_minor= 20
    ; branding= "XenServer 8 Preview"
    ; release_date= Some "August 2023"
    }
  ; {
      code_name= Some rel_nile
    ; version_major= 2
    ; version_minor= 21
    ; branding= "XenServer 8"
    ; release_date= None
    }
  ]
(* When you add a new release, use the version number of the latest release, "Unreleased"
   for the branding, and Some "" for the release date, until the actual values are finalised. *)

let release_order =
  List.filter (fun x -> x.code_name <> None) release_order_full

exception Unknown_release of string

exception UnspecifiedRelease

let code_name_of_release x =
  match x.code_name with Some r -> r | None -> raise UnspecifiedRelease

let version_leq x y =
  let to_list v = v |> String.split_on_char '.' |> List.map int_of_string in
  to_list x <= to_list y

(* ordering function on releases *)
let release_leq x y =
  let rec posn_in_list i x l =
    match l with
    | [] ->
        None
    | r :: _ when code_name_of_release r = x ->
        Some i
    | _ :: rs ->
        posn_in_list (i + 1) x rs
  in
  match (posn_in_list 0 x release_order, posn_in_list 0 y release_order) with
  | Some a, Some b ->
      (* x and y are both named releases *)
      a <= b
  | Some _, None ->
      (* x is a named release, y is numbered and thus newer *)
      true
  | None, Some _ ->
      (* x is a number release, y is named and thus older *)
      false
  | None, None ->
      (* x and y are both numbered releases *)
      version_leq x y

(** Types of object fields. Accessor functions are generated for each field automatically according to its type and qualifiers. *)
type ty =
  | SecretString
  | String
  | Int
  | Float
  | Bool
  | DateTime
  | Enum of string * (string * string) list
  | Set of ty
  | Map of ty * ty
  | Ref of string
  | Record of string
  | Option of ty
[@@deriving rpc]

[@@@ocaml.warning "-33"]

type api_value =
  | VString of string
  | VInt of int64
  | VFloat of float
  | VBool of bool
  | VDateTime of Date.iso8601
  | VEnum of string
  | VMap of (api_value * api_value) list
  | VSet of api_value list
  | VRef of string
  | VCustom of string * api_value
[@@deriving rpc]

(* For convenience, we use the same value here as is defined in the Ref module in
 * xapi-types. It's not terribly important, since all refs should be validated before
 * use anyway, but it's nice to be consistent *)
let null_ref = "OpaqueRef:NULL"

(** Each database field has a qualifier associated with it:
  * "Static" means the initial value is specified as a parameter in the object constructor.
  * "Dynamic" means the opposite: its initial value is a predefined default.
  * "RW" means the value can be updated by XenAPI clients via the autogenerated API.
  * "RO" means the opposite, but the value can be updated by direct database calls. *)
type qualifier =
  | RW  (** Implicitly static: set in constructor and updatable through API *)
  | StaticRO
      (** Specified in constructor; no autogenerated setter in XenAPI. *)
  | DynamicRO
      (** Initial value is a default; no autogenerated setter in XenAPI. *)
[@@deriving rpc]

(** Release keeps track of which versions of opensource/internal products fields and messages are included in *)
type release = {
    opensource: string list
  ; internal: string list
  ; internal_deprecated_since: string option
        (* first release we said it was deprecated *)
}
[@@deriving rpc]

module Lifecycle = struct
  type state =
    | Unreleased_s
    | Prototyped_s
    | Published_s
    | Deprecated_s
    | Removed_s
  [@@deriving rpc]

  type change =
    | Prototyped
    | Published
    | Extended
    | Changed
    | Deprecated
    | Removed
  [@@deriving rpc]

  type transition = change * string * string [@@deriving rpc]

  type t = {state: state; transitions: transition list} [@@deriving rpc]

  exception Invalid of string

  type ('state, 'change) automaton = {
      initial: 'state
    ; next: 'change -> 'state -> 'state
  }

  let raise_invalid msg = raise (Invalid msg)

  let string_of_state s = Rpc.string_of_rpc (rpc_of_state s)

  let string_of_change c = Rpc.string_of_rpc (rpc_of_change c)

  let raise_invalid_next ~from ~into =
    raise_invalid
      (Printf.sprintf "Invalid transition %s from %s" (string_of_change into)
         (string_of_state from)
      )

  (* lifecycle automaton, rules:
     - Unreleased is the initial state
     - Extended and Changed do not change the state
     - Objects cannot be Changed nor Extended before being prototyped or after
       being deprecated
     - The rest of the changes are not idempotent, they cannot be applied twice
       in a row
     - Objects can only be removed when are prototyped or deprecated
  *)
  let automaton =
    {
      initial= Unreleased_s
    ; next=
        (function
        | (Prototyped as into), _, _ -> (
            function
            | Unreleased_s ->
                Prototyped_s
            | from ->
                raise_invalid_next ~from ~into
          )
        | (Published as into), _, _ -> (
            function
            | Unreleased_s | Prototyped_s ->
                Published_s
            | from ->
                raise_invalid_next ~from ~into
          )
        | (Extended as into), _, _ -> (
            function
            | Prototyped_s ->
                Prototyped_s
            | Published_s ->
                Published_s
            | from ->
                raise_invalid_next ~from ~into
          )
        | (Changed as into), _, _ -> (
            function
            | Prototyped_s ->
                Prototyped_s
            | Published_s ->
                Published_s
            | from ->
                raise_invalid_next ~from ~into
          )
        | (Deprecated as into), _, _ -> (
            function
            | Published_s ->
                Deprecated_s
            | from ->
                raise_invalid_next ~from ~into
          )
        | (Removed as into), _, _ -> (
            function
            | Prototyped_s | Deprecated_s ->
                Removed_s
            | from ->
                raise_invalid_next ~from ~into
          )
        )
    }

  let from : transition list -> t = function
    | [] ->
        {state= Unreleased_s; transitions= []}
    | transitions ->
        let rec loop from = function
          | [] ->
              {state= from; transitions}
          | into :: rest ->
              loop (automaton.next into from) rest
        in
        loop automaton.initial transitions
end

(** Messages are tagged with one of these indicating whether the message was
    specified explicitly in the datamodel, or is one of the automatically
    generated ones. If automatically generated, the tag tells you where it came
    from: this is needed for the server implementation. *)
type tag = FromField of field_op * field | FromObject of obj_op | Custom

and field_op = Getter | Setter | Add | Remove

and private_op = GetDBRecord | GetDBAll | Copy

and obj_op =
  | Make
  | Delete
  | GetByUuid
  | GetByLabel
  | GetRecord
  | GetAll
  | GetAllRecordsWhere
  | GetAllRecords
  | Private of private_op

and param = {
    param_type: ty
  ; param_name: string
  ; param_doc: string
  ; param_release: release
  ; param_default: api_value option
}

and doc_tag = VM_lifecycle | Snapshots | Networking | Memory | Windows

and forward = Extension of string | HostExtension of string

(** Types of RPC messages; in addition to those generated for object fields *)
and message = {
    msg_name: string
  ; msg_params: param list
  ; msg_result: (ty * string) option
  ; msg_errors: error list
  ; msg_doc: string
  ; msg_async: bool
  ; msg_session: bool
  ; msg_secret: bool
  ; (* don't put stuff in logs *)
    msg_pool_internal: bool
  ; (* only allow on "pool-login" sessions *)
    msg_db_only: bool
  ; (* this is a db_* only message; not exposed through api *)
    msg_release: release
  ; msg_lifecycle: Lifecycle.t
  ; msg_has_effect: bool
  ; (* if true it appears in the custom operations *)
    msg_force_custom: qualifier option
  ; (* unlike msg_has_effect, msg_force_custom=Some(RO|RW) always forces msg into custom operations, see gen_empty_custom.ml *)
    msg_no_current_operations: bool
  ; (* if true it doesnt appear in the current operations *)
    msg_tag: tag
  ; msg_obj_name: string
  ; msg_custom_marshaller: bool
  ; msg_hide_from_docs: bool
  ; (* don't list the function in the documentation and do not include it in the SDK *)
    msg_allowed_roles: string list option
  ; msg_map_keys_roles: (string * string list option) list
  ; msg_doc_tags: doc_tag list
  ; msg_forward_to: forward option (* proxy the RPC elsewhere *)
}

and field = {
    release: release
  ; lifecycle: Lifecycle.t
  ; field_persist: bool
  ; default_value: api_value option
  ; internal_only: bool
  ; qualifier: qualifier
  ; field_name: string
  ; full_name: string list
  ; ty: ty
  ; field_description: string
  ; field_has_effect: bool
  ; field_ignore_foreign_key: bool
  ; field_setter_roles: string list option
  ; field_getter_roles: string list option
  ; field_map_keys_roles: (string * string list option) list
  ; field_doc_tags: doc_tag list
}

and error = {err_name: string; err_params: string list; err_doc: string}

and mess = {mess_name: string; mess_doc: string} [@@deriving rpc]

let default_message =
  {
    msg_name= ""
  ; msg_params= []
  ; msg_result= None
  ; msg_errors= []
  ; msg_doc= "This message has no documentation."
  ; msg_async= true
  ; msg_session= true
  ; msg_secret= false
  ; msg_pool_internal= true
  ; msg_db_only= false
  ; msg_release=
      {
        internal= ["Never released"]
      ; opensource= []
      ; internal_deprecated_since= None
      }
  ; msg_lifecycle= Lifecycle.from []
  ; msg_has_effect= true
  ; msg_force_custom= None
  ; msg_no_current_operations= false
  ; msg_tag= Custom
  ; msg_obj_name= ""
  ; msg_custom_marshaller= false
  ; msg_hide_from_docs= true
  ; msg_allowed_roles= None
  ; msg_map_keys_roles= []
  ; msg_doc_tags= []
  ; msg_forward_to= None
  }

(** Getters and Setters will be generated for each field, depending on the qualifier.
    Namespaces allow fields to be grouped together (and this can get reflected in the XML
    document structure)
*)
type content =
  | Field of field  (** An individual field *)
  | Namespace of string * content list
      (** A nice namespace for a group of fields *)
[@@deriving rpc]

(* Note: there used be more than 2 persist_options -- that's why it isn't a bool.
   I figured even though there's only 2 now I may as well leave it as an enumeration type.. *)

type persist_option = PersistNothing | PersistEverything [@@deriving rpc]
(* PersistEverything - all creates/writes persisted;
   PersistNothing - no creates/writes to this table persisted *)

type db_logging = Log_destroy [@@deriving rpc]

(** An object (or entity) is represented by one of these: *)
type obj = {
    name: string
  ; description: string
  ; obj_lifecycle: Lifecycle.t
  ; contents: content list
  ; messages: message list
  ; doccomments: (string * string) list
  ; gen_constructor_destructor: bool
  ; force_custom_actions: qualifier option
  ; (* None,Some(RW),Some(StaticRO) *)
    obj_allowed_roles: string list option
  ; (* for construct, destruct and explicit obj msgs*)
    obj_implicit_msg_allowed_roles: string list option
  ; (* for all other implicit obj msgs*)
    gen_events: bool
  ; persist: persist_option
  ; obj_release: release
  ; in_database: bool
  ; (* If the object is in the database *)
    obj_doc_tags: doc_tag list
  ; db_logging: db_logging option
}
[@@deriving rpc]

(* val rpc_of_obj : obj -> Rpc.t *)
(* let s = Jsonrpc.to_string (rpc_of_obj o) *)

(** A relation binds two fields together *)
type relation = (string * string) * (string * string)

(* Check if a value is of a given type *)
let rec type_checks v t =
  let all_true l = List.fold_left (fun env v -> env && v) true l in
  match (v, t) with
  | VString _, String ->
      true
  | VInt _, Int ->
      true
  | VFloat _, Float ->
      true
  | VBool _, Bool ->
      true
  | VDateTime _, DateTime ->
      true
  | VEnum ev, Enum (_, enum_spec) ->
      let enum_possibles = List.map fst enum_spec in
      List.mem ev enum_possibles
  | VMap vvl, Map (t1, t2) ->
      all_true
        (List.map (fun (k, v) -> type_checks k t1 && type_checks v t2) vvl)
  | VSet vl, Set t ->
      all_true (List.map (fun v -> type_checks v t) vl)
  | VRef _, Ref _ ->
      true
  | VCustom _, _ ->
      true (* Type checks defered to phase-2 compile time *)
  | _, _ ->
      false
