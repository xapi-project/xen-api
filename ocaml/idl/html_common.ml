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
open Pervasiveext

let document_title = ref "Citrix XenServer Management API"

let class_index = "classes.html"
let intro = "index.html"
let tree = "tree.html"
let overlay = "overlay" (* used to incorporate user stuff into the documentation *)

let js_files = [ "jquery-1.1.3.1.pack.js";
		 "jquery.treeview.pack.js" ]

let diagram_basename = ref ""

(* let filename_of_object x = x.Datamodel_types.name ^ ".html" *)
let href x y = Xml.Element("a", [ "href", x ], y)
(* let link_to_object x y = href (filename_of_object x) y *)

let write_html doc filename = 
  ignore(Unix.system (Printf.sprintf "mkdir -p %s" (Filename.dirname filename)));
  let oc = open_out filename in
  let txt = Xml.to_string_fmt doc in
  output_string oc txt;
  close_out oc

let link_to_css css = 
  Xml.Element("link", [ "type", "text/css";
		    "rel", "stylesheet";
		    "href", css;
		    "media", "all" ], [Xml.PCData ""]) 

let javascript uri = 
  Xml.Element("script", [ "src", uri;
		      "type", "text/javascript" ], [Xml.PCData ""])

let li contents = 
  Xml.Element("li", [], contents)

let ul contents = 
  Xml.Element("ul", [], contents)

let flat_list contents = 
  ul (List.map (fun x -> li [ x ]) contents)

let div cls contents = Xml.Element("div", [ "class", cls ], contents)
let span cls contents = Xml.Element("span", [ "class", cls ], contents)

let h1 txt = Xml.Element("h1", [], [ Xml.PCData txt ])

let img src = Xml.Element("img", [ "src", src ], [])

let header title = 
  let logo = img "xensource_toplogo.gif" 
  and text = Xml.Element("p", [], [ Xml.PCData (!document_title ^ ": " ^ title) ]) in
  div "title" [ logo; text ]

let html ?(rel="") ?(hdr=true) ?(css=[]) ?(scripts=[]) title body = 
  let head = 
    let js_files = List.map (fun x -> rel ^ x) js_files in
    let scripts = List.map (fun x -> rel ^ x) scripts in
    Xml.Element("head", [], [
	      Xml.Element("title", [], [ Xml.PCData title ]);
	      Xml.Element("meta", [ "http-equiv", "content-type";
				"content", "application/xhtml+xml; charset=utf-8" ], []);
	      link_to_css (rel ^ "api.css") ]
	      @ (List.map link_to_css css)
	      @ (List.map javascript (js_files @ scripts))) in
  Xml.Element("html", [],
	  [ head;
	    Xml.Element("body", [], if hdr then header title :: body else body) ])

let td ?cls ?colspan x = 
  let cls = match cls with
    | Some c -> [ "class", c ] | None -> [] in
  let colspan = match colspan with
    | None -> [] | Some x -> [ "colspan", x ] in
  Xml.Element("td", cls @ colspan, x)
let tr ?cls x = 
  let cls = match cls with
    | None -> [] | Some c -> [ "class", c ] in
  Xml.Element("tr", cls, x)

let hr = Xml.Element("hr", [], [])
let p ?cls x = Xml.Element("p", (match cls with Some x -> [ "class", x ] | _ -> []), x)

let _API = "API"
let _Classes = "Classes"
let _Enums = "Enums"
let _Implicit = "Implicit"
let _Explicit = "Explicit"
let _Fields = "Fields"

let autogen = "autogen"

open Printf
let filename_of_class ?(ext="html") x = sprintf "%s/%s/%s/index.%s" _API _Classes x.Datamodel_types.name ext
let filename_of_field ?(ext="html") obj f = sprintf "%s/%s/%s/%s/%s.%s" _API _Classes obj.Datamodel_types.name _Fields (String.concat "_" f.Datamodel_types.full_name) ext
let filename_of_enum ?(ext="html") name = sprintf "%s/%s/%s.%s" _API _Enums name ext
let filename_of_implicit_msg ?(ext="html") m = sprintf "%s/%s/%s/%s/%s.%s" _API _Classes m.Datamodel_types.msg_obj_name _Implicit m.Datamodel_types.msg_name ext
let filename_of_custom_msg ?(ext="html") m = sprintf "%s/%s/%s/%s/%s.%s" _API _Classes m.Datamodel_types.msg_obj_name _Explicit m.Datamodel_types.msg_name ext

(* For creating links within the document *)
let link_to_class x = href (filename_of_class x)
let link_to_field ?(prefix="") obj f = href (prefix ^ (filename_of_field obj f))
let link_to_implicit_msg ?(prefix="") x = href (prefix ^ (filename_of_implicit_msg x))
let link_to_custom_msg ?(prefix="") x = href (prefix ^ (filename_of_custom_msg x))
let link_to_enum name = href (filename_of_enum name)

open Datamodel_types

let rec string_of_ty = function
  | String -> "string"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | DateTime -> "datetime"
  | Enum (name, _) -> sprintf "enum %s" name
  | Set t -> sprintf "Set[%s]" (string_of_ty t)
  | Map (a, b) -> sprintf "Map[%s,%s]" (string_of_ty a) (string_of_ty b)
  | Ref x -> x
  | Record x -> sprintf "Record[%s]" x

let rec hyperlink_of_ty = 
  let prim x = [ Xml.PCData x ] in
  let href x = href (sprintf "%s/%s/%s.html" _API _Classes x) [ Xml.PCData x ] in function
    | String -> prim "string" | Int -> prim "int" | Float -> prim "float" 
    | Bool -> prim "bool" | DateTime -> prim "datetime"
    | Enum (name, _) -> [ link_to_enum name [ Xml.PCData name ] ]
    | Set t -> [ Xml.PCData "Set[" ] @ (hyperlink_of_ty t) @ [ Xml.PCData "]" ]
    | Map (a, b) -> [ Xml.PCData "Map[" ] @ (hyperlink_of_ty a) @ [ Xml.PCData "," ] @ (hyperlink_of_ty b) @ [ Xml.PCData "]" ]
    | Ref x -> [ href x ]
    | Record x -> [ Xml.PCData "Record["; href x; Xml.PCData "]" ]

open Datamodel_utils

let expand_params obj m = 
  (* Consider expanding the arguments if this is a constructor *)
  let params =
    if m.msg_tag = FromObject Make
    then 
      let fields = List.filter (fun x -> x.qualifier <> DynamicRO) (fields_of_obj obj) in
      List.map (fun f -> {param_type=f.ty; param_name=(String.concat "_" f.full_name); param_doc=f.field_description; param_release=f.release; param_default=None}) fields 
    else m.msg_params in
  (* Consider adding in a session reference if required *)
  let params = 
    if m.msg_session 
    then {param_type=Ref Datamodel._session; param_name="session_id"; param_doc="valid session reference"; param_release=m.msg_release; param_default=None} :: params
    else params in
  params 

let strings_of_message obj m = 
  let r = default "void" (may (fun (t, _) -> string_of_ty t) m.msg_result) in
  let arg p = string_of_ty p.param_type ^ " " ^ p.param_name in
  let params = expand_params obj m in
  r, m.msg_name, "(" ^ (String.concat ", " (List.map arg params)) ^ ")"

let string_of_message obj m = 
  let return, name, args = strings_of_message obj m in
  sprintf "%s %s%s" return name args

let strings_of_field f = 
  string_of_ty f.ty, String.concat "_" f.full_name
  
let string_of_field f = 
  let ty, name = strings_of_field f in
  ty ^ " " ^ name

(** True if a message came from a field (get/set) *)				 
let derived_from_field f x = match x.msg_tag with
  | FromField(_, f') -> f = f'
  | _ -> false

(** True if a message was 'custom' *)
let custom x = match x.msg_tag with
  | Custom -> true
  | _ -> false

(** True if a message was derived from a class ('implicit') *)
let from_object x = match x.msg_tag with
  | FromObject (Private _) -> false
  | FromObject _ -> true
  | _ -> false
