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
  let es = List.concat_map go objects in
  let tbl = Hashtbl.create 1024 in
  List.iter (fun (n, m) -> Hashtbl.replace tbl n m) es ;
  tbl

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
  |> Hashtbl.to_seq
  |> Seq.filter_map (fun ((_, msg) as e) ->
      if is_released msg then
        Some (line_of e)
      else
        None
  )
  |> List.of_seq
  |> List.sort String.compare
  |> List.iter print_endline

(* Parse each line, where each line is of the format produced by
   line_of. *)
let read_messages file =
  let param line = function
    | Rpc.String p ->
        p
    | _ ->
        failwith ("Bad parameter in line: " ^ line)
  in
  In_channel.with_open_text file In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (( <> ) "")
  |> List.map (fun line ->
      match Jsonrpc.of_string line with
      | Rpc.(Enum (String name :: String result :: params)) ->
          let params = List.map (param line) params in
          (name, result, params)
      | _ ->
          failwith ("Unable to parse line: " ^ line)
  )

(* Compare the provided message against the datamodel's version. *)
let check_message (name, result, params) =
  match Hashtbl.find_opt messages name with
  | None ->
      [
        "has been removed from the datamodel: mark it as removed in its \
         lifecycle instead"
      ]
  | Some msg when msg.msg_lifecycle.Lifecycle.state = Removed_s ->
      []
  | Some msg ->
      (* Ensure the result type is unchanged. *)
      let result_problem =
        let r = string_of_result msg in
        if r <> result then
          [Printf.sprintf "returns %s instead of %s" r result]
        else
          []
      in
      (* Recursively compare the parameter listings. *)
      let rec compare_params i prev curr =
        match (prev, curr) with
        | [], added ->
            (* If we've extended a message's parameters, all the new
               parameters must be optional (have default values). *)
            added
            |> List.filter_map (fun p ->
                if p.param_default = None then
                  Some
                    ("new parameter has no default value: " ^ string_of_param p)
                else
                  None
            )
        | r :: _, [] ->
            [Printf.sprintf "has lost parameter %d (%s)" (i + 1) r]
        | r :: rs, c :: cs ->
            (* Pairwise comparison of parameters: parameters occurring
             at the same indices must have retained their name, type,
             and whether they have a default value (and, if they do,
             that the default value is unchanged). *)
            let c = string_of_param c in
            if r = c then
              compare_params (i + 1) rs cs
            else
              [
                Printf.sprintf "parameter %d is now %s instead of %s" (i + 1) c r
              ]
      in
      result_problem @ compare_params 0 params (params_of msg)

let error_message =
  {|
Clients pass arguments by position, these changes to the datamodel
have caused breaking changes to the API.
|}

let check_file file =
  let check ((name, _, _) as msg) =
    List.map (fun p -> (name, p)) (check_message msg)
  in
  let all = List.concat_map check (read_messages file) in
  match all with
  | [] ->
      ()
  | _ ->
      List.iter (fun (name, p) -> Printf.eprintf "%s: %s\n" name p) all ;
      Printf.eprintf "%s" error_message ;
      exit 1

let () =
  match Sys.argv with
  | [|_; "-print"|] ->
      print ()
  | [|_; "-check"; file|] ->
      check_file file
  | _ ->
      exit 2
