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

module DT = Datamodel_types

type field_op = Get | Set | Add | Remove
type obj_op = Make | Delete | GetAll

type operation =
  | Field of field_op * DT.obj * DT.field
  | Object of obj_op * DT.obj
  | Msg of DT.obj * DT.message

let obj_of_operation = function
  | Field(_, x, _) -> x
  | Object(_, x) -> x
  | Msg (x, _) -> x

(** Computes the RPC wire name of an operation *)
let wire_name_of_operation ~sync operation =
  (if sync
   then ""
   else "Async.") ^
  String.capitalize_ascii ((obj_of_operation operation).DT.name) ^ "." ^
  (match operation with
   | Field(op, obj, fld) ->
     (match op with
      | Get -> "get_" | Set -> "set_"
      | Add -> "add_" | Remove -> "remove_") ^
     (String.concat "__" fld.DT.full_name)
   | Object(Make, obj) -> "make"
   | Object(Delete, obj) -> "delete"
   | Object(GetAll, _) -> failwith "GetAll not implemented yet"
   | Msg(obj, msg) -> "do_" ^ msg.DT.msg_name)

(** A flat list of all the possible operations concerning an object.
    Ideally filter the datamodel on release (opensource, closed) first
    and then filter this according to the needs of the specific backend *)
let operations_of_obj (x: DT.obj) : operation list =
  let rec of_contents = function
    | DT.Namespace(_, xs) -> List.concat (List.map of_contents xs)
    | DT.Field y -> List.map (fun tag -> Field(tag, x, y))
                      [ Get; Set; Add; Remove ] in
  let fields = List.concat (List.map of_contents x.DT.contents) in
  let objects = List.map (fun tag -> Object(tag, x))
      [ Make; Delete; GetAll ] in
  let msg = List.map (fun msg -> Msg(x, msg)) x.DT.messages in
  objects @ fields @ msg

(** The whole API is an association list of objects and their operations *)
type t = (DT.obj * (operation list)) list

let filter (operation: operation -> bool) (api: t) =
  List.map (fun (obj, ops) -> obj, List.filter operation ops) api

let operations_which_make_sense = function
  (* cannot atomically set all values in a set or a map *)
  | Field(Set, _, ({ DT.ty = DT.Set _ } | { DT.ty = DT.Map(_,_)})) -> false
  (* Set(Ref _) values are stored as foreign keys in other tables *)
  | Field((Add | Remove), _, { DT.ty = DT.Set (DT.Ref _) }) -> false
  (* Add/Remove from 'normal' sets and maps is fine *)
  | Field((Add | Remove), _, ({ DT.ty = DT.Set _ }|{ DT.ty = DT.Map(_,_) }) ) -> true
  (* Add/Remove from anything else is bad *)
  | Field((Add | Remove), _, _) -> false

  | _ -> true


let of_api (api: Dm_api.api) : t =
  let objects = Dm_api.objects_of_api api in
  let api = List.map (fun obj -> obj, operations_of_obj obj) objects in
  filter operations_which_make_sense api

