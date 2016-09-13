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

(** Generate OCaml code to perform a consistency check across the database *)

module O = Ocaml_syntax
module OU = Ocaml_utils
module Client = Gen_client
module DT = Datamodel_types
module DU = Datamodel_utils
module DM = Datamodel
open DT

(* Names of the modules we're going to use to perform the db operations *)
let _db_action = Gen_db_actions._db_action

(** True if a field is actually in this table, false if stored elsewhere
    (ie Set(Ref _) are stored in foreign tables *)
let field_in_this_table = Gen_db_actions.field_in_this_table

(*  Escaping.escape_id f.full_name *)

let _db_exists = "Db_exists"

let self obj = O.Named(Client._self, OU.alias_of_ty (Ref obj.name))

let record_exists api : O.Module.t =
  let ref_exists (obj: obj) =
    let body =
      if obj.DT.in_database then begin
        Printf.sprintf
          "try ignore(%s.%s.get_record_internal ~%s ~%s); true with _ -> false"
          Gen_db_actions._db_action
          (OU.ocaml_of_obj_name obj.name)
          Gen_common.context
          Client._self
      end else begin
        "false"
      end
    in
    O.Let.make
      ~name: ("_" ^ (OU.ocaml_of_obj_name obj.name))
      ~params: [ Gen_common.context_arg; self obj ]
      ~ty: "bool"
      ~body: [ body ] () in
  O.Module.make
    ~name:_db_exists
    ~preamble:[ ]
    ~elements:(List.map (fun x -> O.Module.Let (ref_exists x)) (Dm_api.objects_of_api api)) ()


let _db_check = "Db_check"

let db_check api : O.Module.t =

  let check_refs (obj: obj) =
    (* List all the fields of the object which are references AND stored in
       this table *)
    let fields = List.filter field_in_this_table (DU.fields_of_obj obj) in
    let fields = List.filter (function { DT.ty = Ref _ } -> true | _ -> false ) fields in
    let getrecord =
      Printf.sprintf "let _r = %s.%s.get_record_internal ~%s ~%s in"
        Gen_db_actions._db_action
        (OU.ocaml_of_obj_name obj.name)
        Gen_common.context
        Client._self in
    let check = function
      | { ty = Ref x; full_name = full_name } ->
        Printf.sprintf "(fun () -> %s._%s ~%s ~%s:_r.%s)"
          _db_exists
          (OU.ocaml_of_obj_name x)
          Gen_common.context
          Client._self
          (OU.ocaml_of_record_field (obj.DT.name :: full_name))
      | _ -> assert false
    in
    let wrapper f =
      Printf.sprintf "(runcheck \"%s\" %s \"%s\" %s)"
        obj.name Client._self (String.concat "/" f.full_name) (check f) in
    let body = if fields = [] then ["true"] else [getrecord; String.concat " &&\n     " (List.map wrapper fields)] in

    O.Let.make
      ~name: ("_" ^ (OU.ocaml_of_obj_name obj.name))
      ~params: [ Gen_common.context_arg; self obj ]
      ~ty: "'a"
      ~body () in

  let all_records (obj: obj) =
    let obj_name = OU.ocaml_of_obj_name obj.name in
    let fold =
      if obj.DT.in_database then
        Printf.sprintf
          "List.fold_left (&&) true (List.map (fun self -> _%s ~%s ~self) (%s.%s.get_all ~%s))"
          obj_name Gen_common.context _db_action obj_name Gen_common.context
      else
        "true"
    in
    O.Let.make
      ~name: ("all_" ^ obj_name)
      ~params: [ Gen_common.context_arg  ]
      ~ty: "bool"
      ~body: [ fold ] () in

  let all (objs: obj list) =
    let one obj =
      let obj_name = OU.ocaml_of_obj_name obj.name in
      Printf.sprintf "(all_%s ~%s)" obj_name Gen_common.context in
    O.Module.Let (O.Let.make
                    ~name:"all"
                    ~params: [ Gen_common.context_arg ]
                    ~ty: "bool"
                    ~body: [ String.concat "&&\n" (List.map one objs) ] ()) in

  let objects = Dm_api.objects_of_api api in
  let lets_of f = List.map (fun x -> O.Module.Let (f x)) objects in

  O.Module.make
    ~name:_db_check
    ~preamble:[ "let runcheck cls row col fn = ";
                "  Printf.printf \"Checking %s %s.%s: \" (Ref.string_of row) cls col;";
                "  if fn ()";
                "  then (print_endline \"OK\"; true)";
                "  else (print_endline \"FAILED\"; false)" ]
    ~elements:( lets_of check_refs @ (lets_of all_records) @ [ all objects ] )
    ()

let all api = [ record_exists api; db_check api ]
