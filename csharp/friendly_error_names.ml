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

open Dm_api
open Datamodel_utils
open Datamodel_types

module DT=Datamodel_types

let friendly_names = Hashtbl.create 10

let rec friendly_names_all errors =
  Hashtbl.iter update_entries errors;
  let keys   = Hashtbl.fold get_keys friendly_names [] in
  let keys'  = List.sort String.compare keys in
  let values = List.map get_value keys' in
  List.combine keys' values;

and update_entries _ error =
  let name = error.DT.err_name in
  let desc = error.DT.err_doc in
  if not (Hashtbl.mem friendly_names name) then
    Hashtbl.add friendly_names name desc
  else
    ()

and get_keys key _ tail =
  key :: tail

and get_value key =
  Hashtbl.find friendly_names key

let rec get_node k = function
  | Xml.Element(k', _, [Xml.PCData v]) :: _ when k = k' -> v
  | _ :: xs -> get_node k xs
  | [] -> ""

let parse_sr_xml filename =
  let update_entry children =
    let description = get_node "description" children in
    let value = get_node "value" children in
    Hashtbl.replace friendly_names ("SR_BACKEND_FAILURE_" ^ value) description
  in
  let rec parse_xml = function
    | Xml.Element("SM-errorcodes", _, children) -> List.iter parse_xml children
    | Xml.Element("code", _, children)          -> update_entry children
    | _                                         -> ()
  in
  try
    parse_xml (Xml.parse_file filename)
  with
    Xml.Error e as exn ->
    Printf.eprintf "%s\n%!" (Xml.error e);
    raise exn

let parse_resx filename =
  let update_entry attrs children =
    let key = List.assoc "name" attrs in
    let value = get_node "value" children in
    Hashtbl.replace friendly_names key value
  in
  let rec parse_xml = function
    | Xml.Element("root", _, children)     -> List.iter parse_xml children
    | Xml.Element("data", attrs, children) -> update_entry attrs children
    | _                                    -> ()
  in
  try
    parse_xml (Xml.parse_file filename)
  with
    Xml.Error e as exn ->
    Printf.eprintf "%s\n%!" (Xml.error e);
    raise exn
