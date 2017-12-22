(*
 * Copyright (C) 2010-2011 Citrix Systems Inc.
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

open Schema

(* This code could live higher up the stack *)
let of_datamodel () =
  let rec flatten_fields fs acc =
    match fs with
      [] -> acc
    | (Datamodel_types.Field f)::fs -> flatten_fields fs (f::acc)
    | (Datamodel_types.Namespace (_,internal_fs))::fs -> flatten_fields fs (flatten_fields internal_fs acc) in
  let column obj f =
    let issetref = match f.Datamodel_types.ty with
      | Datamodel_types.Set (Datamodel_types.Ref _) -> true
      | _ -> false in
    let is_many_to_many f =
      let api = Datamodel.all_api in
      let this = obj.Datamodel_types.name, f.Datamodel_types.field_name in
      Datamodel_utils.Relations.is_in_relation api this &&
      (Datamodel_utils.Relations.classify api (this,(Datamodel_utils.Relations.other_end_of api this)) = (`Many, `Many)) in
    let ty = match f.Datamodel_types.ty with
      | Datamodel_types.Set _ -> Type.Set
      | Datamodel_types.Map(_,_) -> Type.Pairs
      | _ -> Type.String in
    {
      Column.name = Escaping.escape_id f.Datamodel_types.full_name;
      (* NB we always regenerate one-to-many Set(Ref _) fields *)
      persistent = f.Datamodel_types.field_persist && (is_many_to_many f || not issetref || f.Datamodel_types.field_ignore_foreign_key);
      empty = Datamodel_values.gen_empty_db_val f.Datamodel_types.ty;
      (* NB Set(Ref _) fields aren't allowed to have a default value specified so we hardcode one here *)
      default =
        if issetref
        then Some (Value.Set [])
        else Xapi_stdext_monadic.Opt.map Datamodel_values.to_db f.Datamodel_types.default_value ;
      ty = ty;
      issetref = issetref;
    } in

  (* We store the reference in two places for no good reason still: *)
  let _ref = {
    Column.name = Db_names.ref;
    persistent = true;
    empty = Value.String "";
    default = None;
    ty = Type.String;
    issetref = false;
  } in

  let table obj = {
    Table.name = Escaping.escape_obj obj.Datamodel_types.name;
    columns = _ref :: (List.map (column obj) (flatten_fields obj.Datamodel_types.contents []));
    persistent = obj.Datamodel_types.persist = Datamodel_types.PersistEverything;
  } in
  let is_one_to_many x =
    match Datamodel_utils.Relations.classify Datamodel.all_api x with
    | `One, `Many | `Many, `One -> true
    | _ -> false in
  let is_many_to_many x =
    match Datamodel_utils.Relations.classify Datamodel.all_api x with
    | `Many, `Many -> true
    | _ -> false in
  let add_relation p t (((one_tbl, one_fld), (many_tbl, many_fld)) as r) =
    let l = if ForeignMap.mem one_tbl t then ForeignMap.find one_tbl t else [] in
    if p r
    then ForeignMap.add one_tbl ((one_fld, many_tbl, many_fld) :: l) t
    else t in

  let database api = {
    Database.tables = List.map table (Dm_api.objects_of_api api)
  } in
  {
    major_vsn = Datamodel.schema_major_vsn;
    minor_vsn = Datamodel.schema_minor_vsn;
    database = database Datamodel.all_api;
    one_to_many = List.fold_left (add_relation is_one_to_many) ForeignMap.empty (Dm_api.relations_of_api Datamodel.all_api);
    many_to_many = List.fold_left (add_relation is_many_to_many) ForeignMap.empty (Dm_api.relations_of_api Datamodel.all_api);
  }

(* For now this is a convenience debugging function. Eventually we should
   separate the datamodel from the database and load the schema from disk. *)
let write_schema_to_file filename =
  let t = of_datamodel () in
  let sexp = Schema.sexp_of_t t in
  let oc = open_out filename in
  let txt = Sexplib.Sexp.to_string_hum sexp in
  output_string oc txt;
  close_out oc
