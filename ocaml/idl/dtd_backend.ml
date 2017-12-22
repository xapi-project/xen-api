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

open Datamodel
open Datamodel_utils
open Datamodel_types
open Dm_api

(** Create an XML DTD *)

let rec split_cols n x =
  if String.length x < n then [ x ]
  else String.sub x 0 n :: (split_cols n (String.sub x n (String.length x - n - 1)))

type attribute = Attribute of string * string list * string option

type dtd_element =
    PCData
  | Element of string * dtd_element list * attribute list


let name_of_dtd_element = function
  | PCData -> "(#PCDATA)"
  | Element(name, _, _)  -> name

let is_element = function
  | Element(_, _, _)  -> true
  | _ -> false


let string_of_attribute = function
    Attribute(n, options, default) ->
    let opt_string =
      if List.length options = 0 then
        "CDATA"
      else
        "(" ^ String.concat " | " options ^ ")"
    in
    let def_string =
      match default with
        Some def ->
        if def = "" then
          "#REQUIRED"
        else
          def
      | None ->
        "#IMPLIED"
    in
    sprintf "%s %s %s" n opt_string def_string


let strings_of_attributes parent atts =
  if List.length atts > 0 then
    let prefix = sprintf "<!ATTLIST %s " parent in
    let body = List.map string_of_attribute atts in

    prefix :: body @ [">"]
  else
    []


let rec strings_of_dtd_element known_els = function
  | PCData -> ["(#PCDATA)"]
  | Element(name, els, attributes)  ->
    if Hashtbl.mem known_els name then
      let el_count = List.length els in
      let att_count = List.length attributes in
      if el_count = 0 && att_count = 0 then
        []
      else
        let prefix = sprintf "<!ELEMENT %s " name in
        let empty = String.make (String.length prefix) ' ' in
        let body =
          (if el_count = 0 then
             "EMPTY"
           else if el_count = 1 then
             name_of_dtd_element (List.hd els)
           else
             "(" ^
             (String.concat ", "
                (List.filter (fun x -> x <> "" && x <> empty)
                   ((name_of_dtd_element (List.hd els)) ::
                    (List.map
                       (fun x -> empty ^ name_of_dtd_element x)
                       (List.tl els))))) ^
             ")") in

        Hashtbl.remove known_els name;
        (sprintf "%s%s>" prefix body) ::
        ((strings_of_attributes name attributes) @
         (List.concat (List.map (strings_of_dtd_element known_els)
                         (List.filter is_element els))))
    else
      []

let element known_els name children atts =
  let existing_children =
    if Hashtbl.mem known_els name then
      match Hashtbl.find known_els name with
        Element (_, c, att) -> (c, att)
      | _ -> assert(false)
    else
      [], [] in
  let open Xapi_stdext_std.Listext in
  let el = Element (name,
                    (List.setify children @ fst existing_children),
                    (List.setify atts @ snd existing_children)) in
  Hashtbl.replace known_els name el;
  el


let add_attribute known_els el_name att_name options default =
  ignore (element known_els el_name []
            [Attribute(att_name, options, default)])

let rec dtd_element_of_contents known_els parent_name accu = function
  | Namespace(name, xs) ->
    element known_els name
      (List.fold_left (dtd_element_of_contents known_els name) [] xs) [] ::
    accu
  | Field{field_name=name; ty=ty} -> match ty with
      Set (Int | Ref _)
    | Int
    | Float
    | DateTime
    | Bool ->
      add_attribute known_els parent_name name [] None;
      accu

    | Set _ ->
      element known_els name [] [] :: accu

    | Ref n ->
      add_attribute known_els parent_name name [] (Some "");
      accu

    | Enum(_, vals) ->
      add_attribute known_els parent_name name
        (List.map fst vals) None;
      accu

    | String ->
      element known_els name [PCData] [] :: accu
    | _ ->
      failwith (sprintf "unimplemented DTD of field %s" name)


let dtd_element_of_obj known_els x =
  element known_els x.name
    (List.fold_left (dtd_element_of_contents known_els x.name) []
       x.contents) []


let of_objs api =
  let xs = objects_of_api api in
  let known_els = Hashtbl.create 10 in
  let elements = List.map (dtd_element_of_obj known_els) xs in

  List.concat (List.map (strings_of_dtd_element known_els) elements)
