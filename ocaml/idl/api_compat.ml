(*
 * Copyright (c) 2026 Vates.
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

open Datamodel_types

let rec string_of_ty = function
  | SecretString | String ->
      "string"
  | Int ->
      "int"
  | Float ->
      "float"
  | Bool ->
      "bool"
  | DateTime ->
      "datetime"
  | Enum (name, _) ->
      Printf.sprintf "enum %s" name
  | Set ty ->
      Printf.sprintf "%s set" (string_of_ty ty)
  | Map (k, v) ->
      Printf.sprintf "(%s -> %s) map" (string_of_ty k) (string_of_ty v)
  | Ref obj ->
      Printf.sprintf "%s ref" obj
  | Record name ->
      Printf.sprintf "%s record" name
  | Option ty ->
      Printf.sprintf "%s option" (string_of_ty ty)

let string_of_param p =
  let default =
    match p.param_default with
    | None ->
        ""
    | Some v ->
        " = " ^ Jsonrpc.to_string (Datamodel_values.to_rpc v)
  in
  let ty = string_of_ty p.param_type in
  Printf.sprintf "%s: %s%s" p.param_name ty default

let string_of_result msg =
  match msg.msg_result with None -> "void" | Some (ty, _) -> string_of_ty ty

let session_param =
  {
    param_type= Ref Datamodel_common._session
  ; param_name= "session_id"
  ; param_doc= ""
  ; param_release= Datamodel_common.rio_release
  ; param_default= None
  }

let params_of msg =
  if msg.msg_session then
    session_param :: msg.msg_params
  else
    msg.msg_params

let messages =
  let objects = Dm_api.objects_of_api Datamodel.all_api in
  let go obj =
    let go' msg =
      match msg.msg_tag with
      | _ when msg.msg_db_only ->
          None
      | FromObject Make ->
          None
      | _ ->
          Some (Printf.sprintf "%s.%s" obj.name msg.msg_name, msg)
    in
    List.filter_map go' obj.messages
  in
  List.concat_map go objects

let is_released msg =
  match msg.msg_lifecycle.Lifecycle.state with
  | Published_s | Deprecated_s ->
      true
  | Unreleased_s | Prototyped_s | Removed_s ->
      false

(* Encode a message as a JSON array, ["name", "result", "param : ty [=
   default]"...]. *)
let line_of (name, msg) =
  let result = string_of_result msg in
  let params = List.map string_of_param (params_of msg) in
  Rpc.Enum (List.map (fun s -> Rpc.String s) (name :: result :: params))
  |> Jsonrpc.to_string

(* Emit all the messages as line-delimited JSON arrays. *)
let print () =
  messages
  |> List.filter_map (fun ((_, msg) as e) ->
      if is_released msg then
        Some (line_of e)
      else
        None
  )
  |> List.sort String.compare
  |> List.iter print_endline

let () = match Sys.argv with [|_; "-print"|] -> print () | _ -> exit 2
