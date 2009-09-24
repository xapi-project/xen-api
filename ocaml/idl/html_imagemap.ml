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
(** Generate the clickable imagemap *)

open Html_common

let parse_imagemap map image = 
  let doc = Xml.parse_file map in
  let name = match doc with
    | Xml.Element("map", attr, _) ->
	List.assoc "name" attr 
    | _ -> failwith "Failed to extract name from imagemap" in
  [ Xml.Element("img", [ "src", image; "usemap", "#" ^ name ], []); doc ]

let imagemap api = 
(*  let objects = Dm_api.objects_of_api api in*)
(*   let list = flat_list (List.map (fun x -> link_to_object x [ Xml.PCData x.name ]) objects) in*) 
  let map = parse_imagemap (!diagram_basename ^ ".map") (!diagram_basename ^ ".gif") in
  let title = [ h1 "Class relationship diagram";
		p [ Xml.PCData "Click on a class to view the associated fields and messages" ] ] in
  html ~hdr:false "Class index"  (title @ map)

let go api = 
  write_html (imagemap api) class_index
