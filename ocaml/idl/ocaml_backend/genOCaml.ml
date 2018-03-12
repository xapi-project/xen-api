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
(** Convert backend API into OCaml code. *)
open Datamodel_types
open Format
open Ocaml_utils
open Gen_db_actions

let ( @- ) a b = a @ List.map (( ^ ) "  ") b
let ( @-- ) a b = a @ List.map (( ^ ) "    ") b




(** Generate a block with an indented, space-separated middle. *)
let block head middle tail =
  let open Xapi_stdext_std.Listext in
  (head @-
   List.flatten (List.between [""] middle)) @
  tail

let gen_type ty accu = match ty with
  | String | Int | Float | Bool -> accu
  | ty -> ("type "^alias_of_ty ty^" = "^ocaml_of_ty ty) :: accu

(** Generate code to marshal from the given datamodel type to XML-RPC. *)
let ty_to_xmlrpc api ty =
  let indent = String.make 23 ' ' in
  let f = match ty with
    | Bool -> "To.boolean"
    | DateTime -> "To.datetime"
    | Enum(_, cs) ->
      let aux (c, _) = constructor_of c^" -> \""^c^"\"" in
      "    fun v -> To.string(match v with\n  "^indent^
      String.concat ("\n"^indent^"| ") (List.map aux cs)^")"
    | Float -> "To.double"
    | Int -> "fun n -> To.string(Int64.to_string n)"
    | Map(key, value) ->
      let kf = begin match key with
        | Ref x -> "tostring_reference"
        | Enum (name, cs) ->
          let aux (c, _) = Printf.sprintf "%s -> \"%s\"" (constructor_of c) (String.lowercase_ascii c) in
          "   function " ^ (String.concat ("\n" ^ indent ^ "| ") (List.map aux cs))
        | key -> "ToString." ^ (alias_of_ty key)
      end in
      let vf = alias_of_ty value in
      "fun m -> map ("^kf^") ("^vf^") m"
    | Ref _ -> "fun r -> To.string (Ref.string_of r)"
(*
    | Ref "session" -> "fun uuid -> To.string(Uuid.string_of_cookie uuid)"
    | Ref s -> "fun uuid -> To.string(Uuid.string_of_uuid uuid)"
*)
    | Set ty -> "fun s -> set "^alias_of_ty ty^" s"
    | String -> "To.string"
    | Record x ->
      let fields = DU.fields_of_obj (Dm_api.get_obj_by_name api ~objname:x) in
      let kvs = List.map
          (fun fld ->
             alias_of_ty fld.ty ^ " x." ^
             (OU.ocaml_of_record_field (x::fld.full_name)),
             String.concat "_" fld.full_name) fields in
      let kvs = List.map (fun (record, v) -> "\"" ^ v ^ "\", " ^ record) kvs in
      "fun x -> To.structure [ " ^ (String.concat "; " kvs) ^ " ]"
    | Option ty -> "fun s -> To.array (option_to_list "^alias_of_ty ty^" s)"
  in
  ["and "^alias_of_ty ty^" : "^alias_of_ty ty^" -> xml =";
   "  "^f]

(** Generate a module of datamodel type to XML-RPC marshalling functions. *)
let gen_to_xmlrpc api tys = block
    ["module To = struct"]
    ([["open Xml"];

      ["let methodCall = To.methodCall"];
      ["let methodResponse f x = To.methodResponse (f x)"; ];
      ["let tostring_reference = Ref.string_of"];
      ["let set f l =";
       "  To.array (List.map f l)"];
      ["let option_to_list f v =";
       "match v with";
       "| Some v -> [f v]";
       "| None -> []"];
      ["let map fk fv m =";
       "  let elements = List.map (fun (k, v) -> fk k, fv v) m in";
       "  XMLRPC.To.structure elements";
(*
     "  set (fun (k, v) -> XMLRPC.To.structure [\"key\", fk k; \"value\", fv v]) m"
*)
      ];
      ["let structure = To.structure"];
      ["let rec unused' = ()"]] @
     (List.map (ty_to_xmlrpc api) tys))
    ["end"]

(** Generate code to marshal from the given datamodel type to XML-RPC. *)
let ty_of_xmlrpc api ty =
  let alias_of_ty_param t = "("^(alias_of_ty t)^" param)" in
  let wrap var_binding b = "fun " ^ var_binding ^ " -> try ("^b^") with e -> Backtrace.reraise e (Api_errors.Server_error (Api_errors.field_type_error,[param]))" in
  let f = match ty with
    | Bool -> wrap "xml" "From.boolean xml"
    | DateTime -> wrap "xml" "From.datetime xml"
    | Enum(name, cs) ->
      let aux (c, _) = "\""^(String.lowercase_ascii c)^"\" -> "^constructor_of c in
      wrap "xml"
        ("\n    match String.lowercase_ascii (From.string xml) with\n      "^
         String.concat "\n    | " (List.map aux cs)^
         "\n    | _ -> log_backtrace(); raise (RunTimeTypeError(\""^name^"\", xml))")
    | Float -> wrap "xml" "From.double xml"
    | Int -> wrap "xml" "Int64.of_string(From.string xml)"
    | Map(key, value) ->
      let kf = begin match key with
        | Ref x -> "fromstring_reference"
        | Enum (name, cs) ->
          let aux (c, _) = "\""^(String.lowercase_ascii c)^"\" -> "^constructor_of c in
          wrap "txt"
            ("\n    match String.lowercase_ascii txt with\n      "^
             String.concat "\n    | " (List.map aux cs)^
             "\n    | _ -> raise (RunTimeTypeError(\""^name^"\", Xml.parse_string txt))")
        | key -> "FromString." ^ (alias_of_ty key)
      end in
      let vf = alias_of_ty_param value in
      wrap "xml" ("map ("^kf^") ("^vf^") xml")
    | Ref _ -> wrap "xml" "Ref.of_string (From.string xml)"
(*
    | Ref "session" -> "fun uuid -> Uuid.cookie_of_string(From.string uuid)"
    | Ref s -> "fun uuid -> Uuid.uuid_of_string(From.string uuid)"
*)
    | Set ty -> wrap "xml" ("set "^alias_of_ty_param ty^" xml")
    | String -> wrap "xml" "From.string xml"
    | Record x ->
      let fields = DU.fields_of_obj (Dm_api.get_obj_by_name api ~objname:x) in
      let fields =
        List.map (fun fld ->
            (OU.ocaml_of_record_field (x::fld.full_name)) ^ " = " ^
            (alias_of_ty_param fld.ty) ^
            (
              (* generate code to insert default value if none in xml structure *)
              let field_name = String.concat "_" fld.full_name in
              let default_value =
                match fld.DT.ty with
                  DT.Set (DT.Ref _) -> Some (DT.VSet [])
                | _ -> fld.DT.default_value in
              match default_value with
                None -> "(my_assoc \"" ^ field_name ^ "\" all)"
              | Some default ->
                Printf.sprintf "(if (List.mem_assoc \"%s\" all) then (my_assoc \"%s\" all) else %s)"
                  field_name field_name
                  ("Xml.parse_string (\""^(Xml.to_string (Datamodel_values.to_xml default))^"\")")
            ))
          fields in
      let fields = if fields = [] then [ "__unused=()" ] else fields in
      wrap "xml" ("let all = From.structure xml in { " ^
                  (String.concat ";\n " fields) ^ " }")
    | Option ty -> wrap "xml" ("From.array "^alias_of_ty_param ty^" xml |> list_to_option")
  in
  let f = "fun param -> ("^f^")" in
  ["and "^alias_of_ty ty^" : string -> xml -> "^alias_of_ty ty^" =";
   "  "^f]

(** Generate a module of datamodel type to XML-RPC marshalling functions. *)
let gen_of_xmlrpc api tys = block
    ["module From = struct"]
    ([["open Xml"];
      ["exception Dispatcher_FieldNotFound of string"];
      ["let my_assoc fld assoc_list = try List.assoc fld assoc_list with Not_found -> raise (Dispatcher_FieldNotFound fld)"];
      ["let fromstring_reference = Ref.of_string"];
      ["let methodCall = From.methodCall"];
      ["let methodResponse = From.methodResponse"];
      ["let set f (xml: XMLRPC.xmlrpc) =";
       "  From.array f xml"];
      ["let list_to_option = function";
       "| [] -> None";
       "| [x] -> Some x";
       "| _ -> failwith \"0 or 1 list elements expected for option type\""];
      ["let map fk fv (xml: XMLRPC.xmlrpc) =";
       "  List.map (fun (k, v) -> fk k, fv v) (From.structure xml)"
(*
     "  let f m = fk (List.assoc \"key\" m), fv (List.assoc \"value\" m) in";
     "  set (fun b -> f (From.structure b)) xml"
*)
      ];
      ["let structure = From.structure"];
      ["let rec unused' = ()"]] @
     (List.map (ty_of_xmlrpc api) tys))
    ["end"]
