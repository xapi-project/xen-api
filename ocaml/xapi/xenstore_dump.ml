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
(** Utility functions to dump and partially restore trees of xenstore data to/from XML
    which can be used to preserve selected parts of xenstore across suspend/resume/migrate.

    Permissions have to be handled separately.
*)

open Stdext.Xstringext
open Xenstore

exception Invalid_path of string
let handle_enoent f x = try f x with Xs_protocol.Enoent _ -> raise (Invalid_path x)

let dump ~xs (path: string) : Xml.xml =
  let rec ls_R prefix path =
    (* Annoyingly xenstore often returns [ "" ] rather than [ ] *)
    let files = List.filter (fun x -> x <> "") (handle_enoent xs.Xs.directory path) in
    let children = List.map (fun x -> ls_R (Filename.concat prefix x) (Filename.concat path x)) files in
    let relative_paths = List.map (Filename.concat prefix) files in
    let absolute_paths = List.map (Filename.concat path) files in
    let kvpairs = List.map
        (fun (relative, absolute) -> relative, handle_enoent xs.Xs.read absolute)
        (List.combine relative_paths absolute_paths) in

    List.concat (kvpairs :: children)
  in
  let all = ls_R "" path in
  let list = List.map (fun (k, v) -> Xml.Element("n", [ "path", k; "value", v ], [])) all in
  Xml.Element("xenstore-dump", [ "version", "0.1" ], list)

let restore ~xs (path: string) (dump: Xml.xml) = match dump with
  | Xml.Element("xenstore-dump", [ "version", _ ], nodes) ->
    let node = function
      | Xml.Element("n", attr, _) ->
        if not(List.mem_assoc "path" attr)
        then failwith "expected path attribute";
        if not(List.mem_assoc "value" attr)
        then failwith "expected value attribute";
        List.assoc "path" attr, List.assoc "value" attr
      | _ -> failwith "expected <n> element" in
    let nodes = List.map node nodes in
    xs.Xs.writev path nodes
  | _ -> failwith "expected <xenstore-dump> element"

