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
 open Printf
 
 module DT = Datamodel_types
 module DU = Datamodel_utils
 module OU = Ocaml_utils
 
 module O = Ocaml_syntax
 
 let oc = ref stdout
 let print s = output_string !oc (s^"\n")
 let between = Xapi_stdext_std.Listext.List.between

let overrides = [
  "vm_operations_to_string_map",(
    "let rpc_of_vm_operations_to_string_map x = Rpc.Dict (List.map (fun (x,y) -> (match rpc_of_vm_operations x with Rpc.String x -> x | _ -> failwith \"Marshalling error\"), Rpc.String y) x)\n" ^
    "let vm_operations_to_string_map_of_rpc x = match x with Rpc.Dict l -> List.map (function (x,y) -> vm_operations_of_rpc (Rpc.String x), string_of_rpc y) l | _ -> failwith \"Unmarshalling error\"\n");
  "bond_mode",(
    "let rpc_of_bond_mode x = match x with `balanceslb -> Rpc.String \"balance-slb\" | `activebackup -> Rpc.String \"active-backup\" | `lacp -> Rpc.String \"lacp\"\n"^
    "let bond_mode_of_rpc x = match x with Rpc.String \"balance-slb\" -> `balanceslb | Rpc.String \"active-backup\" -> `activebackup | Rpc.String \"lacp\" -> `lacp | _ -> failwith \"Unmarshalling error in bond-mode\"\n");
  "int64_to_float_map",(
    "let rpc_of_int64_to_float_map x = Rpc.Dict (List.map (fun (x,y) -> Int64.to_string x, Rpc.Float y) x)\n" ^
    "let int64_to_float_map_of_rpc x = match x with Rpc.Dict x -> List.map (fun (x,y) -> Int64.of_string x, float_of_rpc y) x | _ -> failwith \"Unmarshalling error\"");
  "int64_to_int64_map",(
    "let rpc_of_int64_to_int64_map x = Rpc.Dict (List.map (fun (x,y) -> Int64.to_string x, Rpc.Int y) x)\n" ^
    "let int64_to_int64_map_of_rpc x = match x with Rpc.Dict x -> List.map (fun (x,y) -> Int64.of_string x, int64_of_rpc y) x | _ -> failwith \"Unmarshalling error\"");
  "int64_to_string_set_map",(
    "let rpc_of_int64_to_string_set_map x = Rpc.Dict (List.map (fun (x,y) -> Int64.to_string x, rpc_of_string_set y) x)\n" ^
    "let int64_to_string_set_map_of_rpc x = match x with Rpc.Dict x -> List.map (fun (x,y) -> Int64.of_string x, string_set_of_rpc y) x | _ -> failwith \"Unmarshalling error\"");
  "event_operation",(
    "let rpc_of_event_operation x = match x with | `add -> Rpc.String \"add\" | `del -> Rpc.String \"del\" | `_mod -> Rpc.String \"mod\"\n"^
    "let event_operation_of_rpc x = match x with | Rpc.String \"add\" -> `add | Rpc.String \"del\" -> `del | Rpc.String \"mod\" -> `_mod | _ -> failwith \"Unmarshalling error\"");

]


(** Generate a single type declaration for simple types (eg not containing references to record objects) *)
let gen_non_record_type highapi tys =
  let rec aux accu = function
    | []                           -> accu
    | DT.String               :: t
    | DT.Int                  :: t
    | DT.Float                :: t
    | DT.Bool                 :: t
    | DT.Record _             :: t
    | DT.Map (_, DT.Record _) :: t
    | DT.Set (DT.Record _)    :: t -> aux accu t
    | DT.Set (DT.Enum (n,_) as e) as ty :: t ->
      aux (sprintf "type %s = %s list [@@deriving rpc]" (OU.alias_of_ty ty) (OU.alias_of_ty e) :: accu) t
    | ty                      :: t ->
      let alias = OU.alias_of_ty ty in
      if List.mem_assoc alias overrides
      then aux ((sprintf "type %s = %s\n%s\n" alias (OU.ocaml_of_ty ty) (List.assoc alias overrides))::accu) t
      else aux (sprintf "type %s = %s [@@deriving rpc]" (OU.alias_of_ty ty) (OU.ocaml_of_ty ty) :: accu) t in
  aux [] tys

(** Generate a list of modules for each record kind *)
let gen_record_type ~with_module highapi tys =
  let rec aux accu = function
    | []                    -> accu
    | DT.Record record :: t ->

      let obj_name = OU.ocaml_of_record_name record in
      let all_fields = DU.fields_of_obj (Dm_api.get_obj_by_name highapi ~objname:record) in
      let field fld = OU.ocaml_of_record_field (obj_name :: fld.DT.full_name) in
      let map_fields fn = String.concat "; " (List.map (fun field -> fn field) all_fields) in
      let regular_def fld = sprintf "%s : %s" (field fld) (OU.alias_of_ty fld.DT.ty) in

      let make_of_field fld =
        sprintf "\"%s\",rpc_of_%s x.%s" (String.concat "_" fld.DT.full_name)
          (OU.alias_of_ty fld.DT.ty) (OU.ocaml_of_record_field (obj_name :: fld.DT.full_name))
      in

      let make_to_field fld =
        sprintf "%s = %s_of_rpc (List.assoc \"%s\" x)" (field fld) (OU.alias_of_ty fld.DT.ty)
          (String.concat "_" fld.DT.full_name)
      in

      let type_t = sprintf "type %s_t = { %s }" obj_name (map_fields regular_def) in
      let others = if not with_module then
          []
        else [
          sprintf "let rpc_of_%s_t x = Rpc.Dict [ %s ]" obj_name (map_fields make_of_field);
          sprintf "let %s_t_of_rpc x = on_dict (fun x -> { %s }) x" obj_name (map_fields make_to_field);
          sprintf "type ref_%s_to_%s_t_map = (ref_%s * %s_t) list [@@deriving rpc]" record obj_name record obj_name;
          sprintf "type %s_t_set = %s_t list [@@deriving rpc]" obj_name obj_name;
          ""
        ] in
      aux (type_t :: others @ accu) t
    | _                :: t -> aux accu t in
  aux [] tys

let gen_client highapi =
  List.iter (List.iter print)
    (between [""] [
        [
          "open API";
          "open Rpc";
          "module type RPC = sig val rpc: Rpc.t -> Rpc.t end";
          "module type IO = sig type 'a t val bind : 'a t -> ('a -> 'b t) -> 'b t val return : 'a -> 'a t end";
          "";
          "let server_failure code args = raise (Api_errors.Server_error (code, args))";
        ];
        O.Module.strings_of (Gen_client.gen_module highapi);
        [ "module Id = struct type 'a t = 'a let bind x f = f x let return x = x end";
          "module Client = ClientF(Id)" ]
      ])

let add_set_enums types =
  List.concat (
    List.map (fun ty ->
        match ty with
        | DT.Enum _ ->
          if List.exists (fun ty2 -> ty2 = DT.Set ty) types then [ty] else [DT.Set ty; ty]
        | _ -> [ty]) types)

let gen_client_types highapi =
  let all_types = DU.Types.of_objects (Dm_api.objects_of_api highapi) in
  let all_types = add_set_enums all_types in
  List.iter (List.iter print)
    (between [""] [
        [
          "type failure = (string list) [@@deriving rpc]";
          "let response_of_failure code params =";
          "  Rpc.failure (rpc_of_failure (code::params))";
          "let response_of_fault code =";
          "  Rpc.failure (rpc_of_failure ([\"Fault\"; code]))";
        ]; [
          "include Rpc";
          "type string_list = string list [@@deriving rpc]";
        ]; [
          "module Ref = struct";
          "  include Ref";
          "  let rpc_of_t (_:'a -> Rpc.t) (x: 'a Ref.t) = rpc_of_string (Ref.string_of x)";
          "  let t_of_rpc (_:Rpc.t -> 'a) x : 'a t = of_string (string_of_rpc x);";
          "end";
        ]; [
          "module Date = struct";
          "  open Xapi_stdext_date";
          "  include Date";
          "  let rpc_of_iso8601 x = DateTime (Date.to_string x)";
          "  let iso8601_of_rpc = function String x | DateTime x -> Date.of_string x | _ -> failwith \"Date.iso8601_of_rpc\"";
          "end";
        ]; [
          "let on_dict f = function | Rpc.Dict x -> f x | _ -> failwith \"Expected Dictionary\""
        ];
        gen_non_record_type highapi all_types;
        gen_record_type ~with_module:true highapi all_types;
        O.Signature.strings_of (Gen_client.gen_signature highapi);
        [ "module Legacy = struct";
          "open XMLRPC";
          "module D=Debug.Make(struct let name=\"legacy_marshallers\" end)";
          "open D" ];
        GenOCaml.gen_of_xmlrpc highapi all_types;
        GenOCaml.gen_to_xmlrpc highapi all_types;
        ["end"];
      ])

let gen_server highapi =
  List.iter (List.iter print)
    (between [""] [
        [ "open API"; "open Server_helpers" ];
        O.Module.strings_of (Gen_server.gen_module highapi);
      ])

let gen_custom_actions highapi =
  List.iter (List.iter print)
    (between [""] [
        [ "open API" ];
        O.Signature.strings_of (Gen_empty_custom.gen_signature Gen_empty_custom.signature_name None highapi);
        O.Module.strings_of (Gen_empty_custom.gen_release_module highapi);
      ])

open Gen_db_actions

let gen_db_actions highapi =
  let all_types = DU.Types.of_objects (Dm_api.objects_of_api highapi) in
  let only_records = List.filter (function DT.Record _ -> true | _ -> false) all_types in

  List.iter (List.iter print)
    (between [""]
       [
         [ "open API" ];

         (* These records have the hidden fields inside *)
         gen_record_type ~with_module:false highapi only_records;

         (* NB record types are ignored by dm_to_string and string_to_dm *)
         O.Module.strings_of (dm_to_string all_types);
         O.Module.strings_of (string_to_dm all_types);
         O.Module.strings_of (db_action highapi); ]
     @ (List.map O.Module.strings_of (Gen_db_check.all highapi)) @ [

     ]
    )

let gen_rbac highapi =
  print (Gen_rbac.gen_permissions_of_static_roles highapi)
