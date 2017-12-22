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

let print s = output_string stdout (s^"\n")

let rec gen_test_type highapi ty =
  let rec aux = function
    | DT.String -> "\"teststring\""
    | DT.Int -> "123456789123456789L"
    | DT.Float -> "0.123456789"
    | DT.Bool -> "true"
    | DT.DateTime -> "(Date.of_string \"20120101T00:00:00Z\")"
    | DT.Enum (_,(x,_)::_) -> Printf.sprintf "(%s)" (OU.constructor_of x)
    | DT.Set (DT.Enum (x,y)) ->
      Printf.sprintf "[ %s ]"
        (String.concat ";"
           (List.map (fun (x,y) -> OU.constructor_of x) y))
    | DT.Set x -> Printf.sprintf "[ %s ]" (aux x)
    | DT.Map (x,y) -> Printf.sprintf "[ (%s,%s) ]" (aux x) (aux y)
    | DT.Ref x -> Printf.sprintf "(Ref.of_string \"OpaqueRef:foo\")"
    | DT.Record x -> gen_record_type highapi x
    | _ -> failwith "Invalid type"
  in
  aux ty

(** Generate a list of modules for each record kind *)
and gen_record_type highapi record =
  let obj_name = OU.ocaml_of_record_name record in
  let all_fields = DU.fields_of_obj (Dm_api.get_obj_by_name highapi ~objname:record) in
  let field fld = OU.ocaml_of_record_field (obj_name :: fld.DT.full_name) in
  let map_fields fn = String.concat "; " (List.map (fun field -> fn field) all_fields) in
  let regular_def fld = sprintf "%s=%s" (field fld) (gen_test_type highapi fld.DT.ty) in
  sprintf "{ %s }" (map_fields regular_def)


let gen_test highapi =
  let open Xapi_stdext_std.Listext in
  let all_types = DU.Types.of_objects (Dm_api.objects_of_api highapi) in
  let all_types = Gen_api.add_set_enums all_types in
  ignore(all_types);
  List.iter (List.iter print)
    (List.between [""] [
        ["open API"];
        ["let _ ="];
        List.concat (List.map (fun ty ->
            [
              sprintf "let oc = open_out \"rpc-light_%s.xml\" in" (OU.alias_of_ty ty);
              sprintf "let x = %s in" (gen_test_type highapi ty);
              sprintf "Printf.fprintf oc \"%%s\" (Xmlrpc.to_string (API.rpc_of_%s x));" (OU.alias_of_ty ty);
              "close_out oc;";
              sprintf "let oc = open_out \"xml-light2_%s.xml\" in" (OU.alias_of_ty ty);
              sprintf "Printf.fprintf oc \"%%s\" (Xml.to_string (API.Legacy.To.%s x));" (OU.alias_of_ty ty);
              "close_out oc;";
              (*					sprintf "let s = Xml.to_string (API.Legacy.To.%s x) in" (OU.alias_of_ty ty);*)
              (*					sprintf "let y =" *)
            ]
          ) all_types)
      ])

