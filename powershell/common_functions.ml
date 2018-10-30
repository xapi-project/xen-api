(*
 * Copyright (c) Citrix Systems, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1) Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2) Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


open Printf
open Datamodel
open Datamodel_types

module DU = Datamodel_utils

let rec pascal_case_ s =
  let ss =
    Astring.String.cuts ~sep:"_" ~empty:true s |>
    List.map String.capitalize_ascii
  in
  match ss with
  | [] -> ""
  | h::tl ->
    let h' = if String.length h > 1 then
        let sndchar = String.sub h 1 1 in
        if sndchar = String.uppercase_ascii sndchar then h
        else String.capitalize_ascii h
      else String.uncapitalize_ascii h
    in
    h' ^ (String.concat "" tl)

and pascal_case s =
  let str = pascal_case_ s in
  if(String.length str > 3
     && ((String.lowercase_ascii (String.sub str 0 3)) = "set"
         || (String.lowercase_ascii (String.sub str 0 3)) = "get"))
  then String.sub str 3 ((String.length str) - 3)
  else str

and lower_and_underscore_first s =
  sprintf "_%s%s"
    (String.uncapitalize_ascii (String.sub s 0 1))
    (String.sub s 1 ((String.length s) - 1))

and ocaml_class_to_csharp_property classname =
  if (classname = "host") then "XenHost"
  else (exposed_class_name (pascal_case classname))

and ocaml_class_to_csharp_class classname =
  exposed_class_name (pascal_case classname)

and ocaml_class_to_csharp_local_var classname =
  if classname = "event" then "evt"
  else String.lowercase_ascii (exposed_class_name classname)

and ocaml_field_to_csharp_local_var field =
  String.lowercase_ascii (full_name field)

and ocaml_field_to_csharp_property field =
  ocaml_class_to_csharp_property (full_name field)

and exposed_class_name classname =
  match String.lowercase_ascii(classname) with
  | "vm"  -> "VM"
  | "vdi" -> "VDI"
  | "vbd" -> "VBD"
  | "pbd" -> "PBD"
  | "sr"  -> "SR"
  | "vif" -> "VIF"
  | "pif" -> "PIF"
  | "url" -> "Url_"
  |  _     -> String.capitalize_ascii classname

and qualified_class_name classname =
  "XenAPI."^(exposed_class_name classname)

and type_default ty =
  match ty with
  | Int         -> ""
  | String      -> ""
  | Float       -> ""
  | Bool        -> ""
  | Enum _      -> ""
  | Record _    -> ""
  | Ref _       -> ""
  | Map(_, _)   -> " = new Hashtable()"
  | Set(String) -> " = new string[0]"
  | _           -> sprintf " = new %s()"(exposed_type ty)

and escaped = function
  | "params" -> "paramz"
  | s -> s

and full_name field =
  escaped (String.concat "_" field.full_name)

and exposed_type_opt = function
  | Some (typ, _) -> exposed_type typ
  | None -> "void"

and exposed_type = function
  | String                  -> "string"
  | Int                     -> "long"
  | Float                   -> "double"
  | Bool                    -> "bool"
  | DateTime                -> "DateTime"
  | Ref name                -> sprintf "XenRef<%s>" (qualified_class_name name)
  | Set(Ref name)           -> sprintf "List<XenRef<%s>>" (qualified_class_name name)
  | Set(Enum(name, _))      -> sprintf "List<%s>" name
  | Set(Int)                -> "long[]"
  | Set(String)             -> "string[]"
  | Enum(name, _)           -> name
  | Map(u, v)               -> sprintf "Dictionary<%s, %s>" (exposed_type u)
                                 (exposed_type v)
  | Record name             -> qualified_class_name name
  | Set(Record name)        -> sprintf "List<%s>" (qualified_class_name name)
  | _                       -> assert false

and obj_internal_type = function
  | Ref x         -> sprintf "XenRef<%s>" (qualified_class_name x)
  | Set(Ref x)    -> sprintf "List<XenRef<%s>>" (qualified_class_name x)
  | Map(_, _)     -> "Hashtable"
  | Record x      -> qualified_class_name x
  | Set(Record x) -> sprintf "List<%s>" (qualified_class_name x)
  | x             -> exposed_type x

and is_invoke message =
  message.msg_tag = Custom &&
  not (is_setter message) &&
  not (is_getter message) &&
  not (is_adder message) &&
  not (is_remover message) &&
  not (is_constructor message) &&
  not (is_destructor message)

and is_setter message =
  (String.length message.msg_name >= 3)
  && (String.sub message.msg_name 0 3) = "set"

and is_getter message =
  (String.length message.msg_name >= 3)
  && (String.sub message.msg_name 0 3) = "get"

and is_adder message =
  (String.length message.msg_name >= 3)
  && (String.sub message.msg_name 0 3) = "add"

and is_remover message =
  (String.length message.msg_name >= 6)
  && (String.sub message.msg_name 0 6) = "remove"

and is_constructor message =
  message.msg_tag = (FromObject Make) || message.msg_name = "create"

and is_real_constructor message =
  message.msg_tag = (FromObject Make)

and is_destructor message =
  message.msg_tag = (FromObject Delete) || message.msg_name = "destroy"


(* Some adders/removers are just prefixed by Add or RemoveFrom
   and some are prefixed by AddTo or RemoveFrom *)
and cut_msg_name message_name fn_type =
  let name_len = String.length message_name in
  if (fn_type = "Add") then
    begin
      if (name_len > 5) && (String.sub message_name 0 5) = "AddTo" then
        String.sub message_name 5 (name_len - 5)
      else if (name_len > 3) && (String.sub message_name 0 3) = "Add" then
        String.sub message_name 3 (name_len - 3)
      else
        "" (*Shouldn't happen*)
    end
  else if (fn_type = "Remove") then
    begin
      if (name_len > 10) && (String.sub message_name 0 10) = "RemoveFrom" then
        String.sub message_name 10 (name_len - 10)
      else if (name_len > 6) && (String.sub message_name 0 6) = "Remove" then
        String.sub message_name 6 (name_len - 6)
      else
        message_name (* case of a destructor *)
    end
  else
    message_name

(* True if an object has a uuid (and therefore should have a get_by_uuid message *)
and has_uuid x =
  let all_fields = DU.fields_of_obj x in
  List.filter (fun fld -> fld.full_name = [ "uuid" ]) all_fields <> []

and has_name x =
  DU.obj_has_get_by_name_label x

and get_http_action_verb name meth =
  let parts = Astring.String.cuts ~sep:"_" name in
  if (List.exists (fun x -> x = "import") parts) then "Import"
  else if (List.exists (fun x -> x = "export") parts) then "Export"
  else if (List.exists (fun x -> x = "get") parts) then "Receive"
  else if (List.exists (fun x -> x = "put") parts) then "Send"
  else
    match meth with
    | Get -> "Receive"
    | Put -> "Send"
    | _   -> assert false

and get_common_verb_category verb =
  match verb with
  | "Import"
  | "Export"  -> "VerbsData"
  | "Receive"
  | "Send"    -> "VerbsCommunications"
  | _         -> assert false

and get_http_action_stem name =
  let parts = Astring.String.cuts ~sep:"_" name in
  let filtered = List.filter trim_http_action_stem parts in
  let trimmed = String.concat "_" filtered in
  match trimmed with
  | "" -> pascal_case_ "vm"
  | _  -> pascal_case_ trimmed

and trim_http_action_stem x =
  match x with
  | "get"
  | "put"
  | "import"
  | "export"
  | "download"
  | "upload"   -> false
  | _          -> true
