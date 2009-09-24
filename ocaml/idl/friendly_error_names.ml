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
open Dm_api
open Datamodel_utils
open Stringext

module DT=Datamodel_types

let escape_rules = ['<', "&lt;";'>', "&gt;"]

let escape s = 
  String.escaped ~rules:escape_rules s

let friendly_names = Hashtbl.create 10

let rec friendly_names_all errors =
  Hashtbl.iter update_entries errors;
  let keys   = Hashtbl.fold get_keys friendly_names [] in
  let keys'  = List.sort String.compare keys in
  let values = List.map get_value keys' in
    List.combine keys' values;

and update_entries _ error = 
  let name = escape error.DT.err_name in
  let desc = escape error.DT.err_doc in
  if not (Hashtbl.mem friendly_names name) then
    Hashtbl.add friendly_names name desc
  else
    ()

and get_keys key _ tail =
  key :: tail

and get_value key = 
  Hashtbl.find friendly_names key

let parse_resx filename =
  let rec get_value_node = function
    | Xml.Element("value", _, [Xml.PCData v]) :: _ -> v
    | _ :: xs -> get_value_node xs
    | [] -> ""
  in

  let update_entry attrs children =
    let key = List.assoc "name" attrs in
    let value = get_value_node children in
      Hashtbl.replace friendly_names key value
  in

  let rec parse_xml = function
    | Xml.Element("root", _, children) ->
	List.iter parse_xml children
    | Xml.Element("data", attrs, children) ->
        update_entry attrs children
    | _ -> ()
  in
    try
      parse_xml (Xml.parse_file filename)
    with
      Xml.Error e as exn ->
        Printf.eprintf "%s\n%!" (Xml.error e);
        raise exn
