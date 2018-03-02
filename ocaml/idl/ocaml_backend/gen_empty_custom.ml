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
(** Generate default implementation of the Custom actions signature *)

(* open Api_lowlevel *)
module OU = Ocaml_utils
module DT = Datamodel_types
module DU = Datamodel_utils
module DM = Datamodel
module O = Ocaml_syntax
module Client = Gen_client
open DT

let debug_module_name   = "DebugVersion"
let release_module_name = "ReleaseVersion"
let signature_name = "CUSTOM_ACTIONS"
let forwarding_signature_name = "CUSTOM_FORWARDING"

let _task_id = "task_id"

(* Notes:
   1. Only those members derived from fields and messages from the datamodel
      which are marked as requiring side-effects should be present
   2. get_field returns unit (values always come from the database)
*)


let operation_requires_side_effect ({ msg_tag = tag } as msg) =
  (match msg.DT.msg_force_custom (* this flag always forces msg into custom_actions.ml *)
   with None -> false | Some (mode) ->
     if mode=RW then true (*RW=force both setters and getters into custom_actions *)
     else (*{Static/Dynamic}RO=force only getters into custom_actions *)
       (match msg with
        | { msg_tag = FromField((Setter|Add|Remove), _) } -> false
        | { msg_tag = FromObject(Make|Delete) } -> false
        | _ -> true)
  )
  ||
  match tag with
  | FromField(Setter, fld) -> fld.DT.field_has_effect
  | FromObject(GetRecord | GetByUuid | GetByLabel | GetAll | GetAllRecordsWhere | GetAllRecords) -> false
  | FromObject(_) -> true
  | Custom -> msg.DT.msg_has_effect && msg.DT.msg_forward_to = None
  | _ -> false

let make_custom_api api =
  Dm_api.filter (fun _ -> true) (fun _ -> true)
    (fun msg -> (operation_requires_side_effect msg) && (Client.objfilter msg api)) api

let gen_debug_module name_override result_type_override body_override api : O.Module.t =
  let api = make_custom_api api in

  let operation (obj: obj) (x: message) =
    let args = Client.args_of_message obj x in
    (* filter out the session_id *)
    let args = List.filter (function O.Named("session_id", _) -> false | _ -> true) args in

    let result_type =
      match result_type_override with
        None ->
        begin
          match x.msg_custom_marshaller, x.msg_result with
          | true, _ -> "Rpc.t"
          | _, Some (ty, _) -> OU.alias_of_ty ty
          | _, None -> "unit"
        end
      | Some t -> t in

    let body =
      match body_override with
        None -> [ "raise (Not_implemented \""^x.msg_name^"\")" ]
      | Some b -> b in

    O.Let.make
      ~name:x.msg_name
      ~params:(Gen_common.context_arg :: args)
      ~ty:result_type
      ~body:body
      ~doc:""
      () in

  let obj (obj: obj) =
    let messages = List.filter (fun x -> not (DU.has_been_removed x.DT.msg_lifecycle)) obj.messages in
    let fields = List.map (fun x -> O.Module.Let (operation obj x)) messages in
    O.Module.make
      ~name:(OU.ocaml_of_obj_name obj.DT.name)
      ~elements:fields ()
  in

  O.Module.make
    ~name:(match name_override with None->debug_module_name | Some n->n)
    ~preamble: [ "exception Not_implemented of string" ]
    ~elements:(List.map (fun x -> O.Module.Module (obj x)) (Dm_api.objects_of_api api)) ()

let gen_signature signature_name result_type_override api : O.Signature.t =
  (* debug version has full signature *)
  let x = O.Signature.of_module (gen_debug_module None result_type_override None api) in
  { x with O.Signature.name = signature_name }


(** 'release' version has the same structures but none of the methods; so
    this will cause the compile of the server to fail unless it has provided
    an implementation of everything. *)
let gen_release_module api : O.Module.t =
  let obj (obj: obj) = O.Module.make
      ~name:(OU.ocaml_of_obj_name obj.DT.name) ~elements:[] () in
  O.Module.make
    ~name:release_module_name
    ~elements:(List.map (fun x -> O.Module.Module (obj x)) (Dm_api.objects_of_api api)) ()
