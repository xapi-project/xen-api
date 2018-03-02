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
module O = Ocaml_syntax
module DT = Datamodel_types
module DU = Datamodel_utils
module OU = Ocaml_utils
open DT
open Printf

let module_name = "ClientF"
let async_module_name = "Async"
let signature_name = "API"

(** If true then all create arguments are packaged in an RPC <struct>
    otherwise they are unrolled into a <param> list *)
let use_structure_in_ctor = true

(** Commonly-used ocaml function arguments *)
let _session_id = "session_id"
let _self = "self"
let _value = "value"
let _key = "key"
let _rpc = O.Named("rpc", "(Rpc.call -> Rpc.response)")
let custom name ty = O.Named(name, OU.alias_of_ty ty)
let session           = custom _session_id (DT.Ref Datamodel_common._session)
let self (x: DT.obj)  = custom _self (DT.Ref x.DT.name)
let value (ty: DT.ty) = custom _value ty
let key (ty: DT.ty)   = custom _key ty

let of_param p  = custom (OU.ocaml_of_record_field [p.param_name]) p.param_type
let param_of_field fld = custom (OU.ocaml_of_record_field fld.full_name) fld.ty

(** True if a message has an asynchronous counterpart *)
let has_async = function
  | { msg_tag = FromField(_) } -> false
  | { msg_tag = Custom; msg_async = async } -> async
  | { msg_tag = FromObject(Make | Delete) } -> true
  | { msg_tag = FromObject(_) } -> false

(* true if msg is constructor or desctructor and the msg's object specifies not to make constructor/destructor *)
let objfilter msg api =
  let obj_name = msg.DT.msg_obj_name in
  if obj_name="" then failwith (Printf.sprintf "message %s has no obj_name" msg.DT.msg_name)
  else
    let obj = Dm_api.get_obj_by_name api obj_name in
    let obj_gen_con_and_des = obj.DT.gen_constructor_destructor in
    let msg_is_con_or_des =
      (msg.DT.msg_tag = DT.FromObject (DT.Make)) ||
      (msg.DT.msg_tag = DT.FromObject (DT.Delete)) in
    not msg_is_con_or_des || obj_gen_con_and_des

let client_api ~sync api =
  let filter f = Dm_api.filter (fun _ -> true) (fun _ -> true) f in
  let api = filter (fun msg-> (DU.on_client_side msg) && (objfilter msg api)) api in
  if sync then api else filter has_async api

(* Client constructor takes all object fields which are StaticRO or RW *)
let ctor_fields (obj: obj) =
  List.filter (function { DT.qualifier = (DT.StaticRO | DT.RW) } -> true | _ -> false)
    (DU.fields_of_obj obj)

(* Compute a message parameter list from a message suitable for the client (only!) *)
let args_of_message ?(expand_record=true) (obj: obj) ( { msg_tag = tag } as msg) =
  let arg_of_param = function
    | {param_type=Record x; param_name=name; param_doc=doc} ->
      begin match tag with
        | FromObject(Make) ->
          if x <> obj.DT.name then failwith "args_of_message";
          if expand_record
          then List.map param_of_field (ctor_fields obj)
          else [ custom _value (Record x) ]
        | _ -> failwith "arg_of_param: encountered a Record in an unexpected place"
      end
    | p -> [ of_param p ] in
  let session = if msg.msg_session then [ session ] else [ ] in
  List.concat (session :: (List.map arg_of_param msg.msg_params))

let gen_module api : O.Module.t =
  (* Generate any additional helper functions for an operation here *)
  let helper_record_constructor ~sync (obj: obj) (x: message) =
    if x.msg_tag <> FromObject(Make) then []
    else [
      let fields = ctor_fields obj in
      let binding x =
        let arg = OU.ocaml_of_record_field x.DT.full_name in
        let fld = OU.ocaml_of_record_field (obj.DT.name :: x.DT.full_name) in
        sprintf "~%s:%s.%s" arg _value fld in
      let all = List.map binding fields in
      let all = if x.msg_session then "~session_id"::all else all in
      O.Let.make
        ~name:(x.msg_name ^ "_from_record")
        ~params:(_rpc :: (args_of_message ~expand_record:false obj x))
        ~ty:(if sync then (match x.msg_result with Some (x,_) ->
            OU.alias_of_ty x | _ -> "unit")
           else OU.alias_of_ty (DT.Ref Datamodel_common._task))
        ~body:(x.msg_name :: "~rpc" :: all) ()
    ] in

  (* Convert an operation into a Let-binding *)
  let operation ~sync (obj: obj) (x: message) =
    let args = args_of_message obj x in

    let to_rpc (arg: O.param) =
      let binding = O.string_of_param arg in
      let converter = O.type_of_param arg in
      Printf.sprintf "let %s = rpc_of_%s %s in" binding converter binding in

    (* Constructors use a <struct> on the wire *)
    let is_ctor = x.msg_tag = FromObject(Make) && use_structure_in_ctor in
    let ctor_record =
      let fields = ctor_fields obj in
      let of_field f = Printf.sprintf "\"%s\", %s"
          (DU.wire_name_of_field f)
          (O.string_of_param (param_of_field f)) in
      "let args = Dict [ " ^ (String.concat "; " (List.map of_field fields)) ^ "] in" in
    let rpc_args =
      if is_ctor
      then [ O.string_of_param session; "args" ]
      else List.map O.string_of_param args in

    let task = DT.Ref Datamodel_common._task in

    let from_xmlrpc t = match x.msg_custom_marshaller, t, sync with
      | true, _, true             -> "" (* already in RPC form *)
      | true, _, false            -> failwith "No implementation for custom_marshaller && async"
      | false, Some (ty,_), true  -> Printf.sprintf "%s_of_rpc " (OU.alias_of_ty ty)
      | false, _,           false -> Printf.sprintf "%s_of_rpc " (OU.alias_of_ty task)
      | false, None,        true  -> "ignore" in

    let wire_name = DU.wire_name ~sync obj x in

    let return_type =
      if x.msg_custom_marshaller
      then "Rpc.t"
      else begin
        if sync then (match x.msg_result with Some (x,_) ->
            OU.alias_of_ty x | _ -> "unit")
        else OU.alias_of_ty task
      end in

    O.Let.make
      ~name:x.msg_name
      (* Plus ~rpc:(xml -> xml) function (alternative to using functor) *)
      ~params:(_rpc :: args)
      ~ty:return_type
      ~body:(List.map to_rpc args @ [
          if is_ctor then ctor_record else "";
          Printf.sprintf "rpc_wrapper rpc \"%s\" [ %s ] >>= fun x -> return (%s x)"
            wire_name
            (String.concat "; " rpc_args)
            (from_xmlrpc x.msg_result)
        ]) () in

  (* Convert an object into a Module *)
  let obj ~sync (obj: obj) =
    let fields_of = List.map (fun x -> O.Module.Let x) in
    let operations = List.map (fun x -> operation ~sync obj x) obj.messages in
    let helpers = List.concat (List.map (fun x -> helper_record_constructor ~sync obj x) obj.messages) in
    let fields = fields_of (operations @ helpers) in
(*
    let fields = List.map (fun x -> O.Module.Let (operation ~sync obj x)) obj.messages in
*)
    O.Module.make
      ~name:(OU.ocaml_of_obj_name obj.DT.name)
      ~elements:fields ()
  in
  let preamble = [
    "let (>>=) = X.bind";
    "let return = X.return";
    "let rpc_wrapper rpc name args = ";
    "  rpc (Rpc.call name args) >>= fun response -> ";
    "  if response.Rpc.success then";
    "    return response.Rpc.contents";
    "  else match response.Rpc.contents with";
    "    | Rpc.Enum [ Rpc.String \"Fault\"; Rpc.String code ] -> failwith (\"INTERNAL ERROR: \"^code)";
    "    | Rpc.Enum ((Rpc.String code) :: args) -> return (server_failure code (List.map Rpc.string_of_rpc args))";
    "    | rpc -> failwith (\"Client.rpc: \" ^ Rpc.to_string rpc)";
  ]
  in
  let async =
    (* Small subset of the API is async *)
    let api = client_api ~sync:false api in
    let async_objs = Dm_api.objects_of_api api in

    O.Module.make
      ~name:async_module_name
      ~elements:(List.map (fun x -> O.Module.Module (obj ~sync:false x)) async_objs) () in

  let api = client_api ~sync:true api in
  let all_objs = Dm_api.objects_of_api api in
  (* Generate the main client functor *)
  O.Module.make
    ~name:module_name
    ~preamble:preamble
    ~args:["X : IO"]
    ~elements:(O.Module.Module async ::
               List.map (fun x -> O.Module.Module (obj ~sync:true x)) all_objs) ()

let gen_signature api : O.Signature.t =
  (* Ensure the 'API' signature (the client's PoV matches the client implementation) *)
  let x = O.Signature.of_module (gen_module api) in
  { x with O.Signature.name = signature_name }

