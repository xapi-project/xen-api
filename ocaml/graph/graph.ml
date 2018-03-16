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

(** Attempt to mirror the object graph on the server *)
open Datamodel_types

(** Return all references contained within a getrecord response of
    type cls *)
let refs_of_record cls record =
  let obj = Dm_api.get_obj_by_name Datamodel.all_api ~objname:cls in
  let fields = Datamodel_utils.fields_of_obj obj in
  let rec refs_of ty xml = match ty with
    | Ref _ -> [ XMLRPC.From.string xml ]
    | Set t -> List.concat (XMLRPC.From.array (refs_of t) xml)
    | Map(kt, vt) ->
      let pairs = List.map (fun (k, v) -> k, refs_of vt v) (XMLRPC.From.structure xml) in
      let vs = List.concat (List.map snd pairs) in
      begin match kt with
        | Ref _ -> List.map fst pairs @ vs
        | _ -> vs
      end
    | _ -> [] in
  let pairs = XMLRPC.From.structure record in
  let refs_of_field fld =
    let field_name = String.concat "_" fld.full_name in
    if not(List.mem_assoc field_name pairs)
    then [] (* internal? *)
    else refs_of fld.ty (List.assoc field_name pairs) in
  List.concat (List.map refs_of_field fields)

let name_label_of_record cls record =
  let pairs = XMLRPC.From.structure record in
  if List.mem_assoc "name_label" pairs
  then XMLRPC.From.string (List.assoc "name_label" pairs)
  else "unknown " ^ cls

let all_classes = List.map (fun x -> x.name)
    (Dm_api.objects_of_api Datamodel.all_api)

let do_rpc rpc name args =
  let open XMLRPC in
  match From.methodResponse(rpc(To.methodCall name args)) with
  | Fault _ -> invalid_arg "Fault"
  | Failure(code, strings) -> raise (Api_errors.Server_error(code, strings))
  | Success [] -> invalid_arg "empty result"
  | Success [x] -> x
  | _           -> raise Api_errors.(Server_error(internal_error,
                                                  ["do_rpc: Unexpected response"]))

let get_all rpc session_id cls =
  let name = Printf.sprintf "%s.get_all_records_where" cls in
  let args = [ XMLRPC.To.string (Ref.string_of session_id); XMLRPC.To.string "true" ] in
  XMLRPC.From.structure (do_rpc rpc name args)

type node = { id: string; label: string; cls: string }
type edge = { a: string; b: string }

module NodeSet = Set.Make(
  struct
    type t = node
    let compare a b = compare a.id b.id
  end)
module EdgeSet = Set.Make(
  struct
    type t = edge
    let compare x y = if x.a = y.a then compare x.b y.b else compare x.a y.a
  end)

let node_of_id nodes id =
  let one = NodeSet.filter (fun x -> x.id = id) nodes in
  NodeSet.choose one

let colour_of_cls = function
  | "VM" -> "pink"
  | "network" -> "yellow"
  | "host" -> "green"
  | "SR" -> "lightblue"
  | "VDI" -> "orange"
  | _ -> "white"

let output_dot nodes edges oc =
  let labels = NodeSet.fold (fun x acc -> x :: acc) nodes [] in
  let edges = EdgeSet.fold (fun x acc ->
      try
        ignore(node_of_id nodes x.a);
        ignore(node_of_id nodes x.b);
        x :: acc
      with Not_found -> acc
    ) edges [] in
  let output =
    [ "digraph g{"; ] @
    (List.map (fun x -> Printf.sprintf "node [label=\"%s\" style=filled fillcolor=%s]; \"%s\";" x.label (colour_of_cls x.cls) x.id) labels)
    @
    (List.map (fun x -> Printf.sprintf "\"%s\" -> \"%s\";" x.a x.b) edges)
    @ [
      "}";
    ] in
  List.iter (fun x -> output_string oc x; output_string oc "\n") output

let nodes = ref NodeSet.empty
let edges = ref EdgeSet.empty

open Client
open Printf

let host = ref "127.0.0.1"
let port = ref 80
let username = ref "root"
let password = ref ""
let all = ref false
let singleton = ref false

(* The interface to the ocaml client bindings requires a function which performs the XMLRPC call: *)
let rpc xml =
  let open Xmlrpc_client in
  XML_protocol.rpc ~srcstr:"graph" ~dststr:"xapi" ~transport:(TCP(!host, !port)) ~http:(xmlrpc ~version:"1.0" "/") xml

let newrpc xml =
  let open Xmlrpc_client in
  XMLRPC_protocol.rpc ~srcstr:"graph" ~dststr:"xapi" ~transport:(TCP(!host, !port)) ~http:(xmlrpc ~version:"1.0" "/") xml

let _ =
  let wanted = ref [] in
  Arg.parse [
    "-h", Arg.Set_string host, "hostname to connect to";
    "-p", Arg.Set_int port, "port number to connect to";
    "-u", Arg.Set_string username, "username to connect with";
    "-pw", Arg.Set_string password, "password to connect with";
    "-all", Arg.Set all, "show everything";
    "-singleton", Arg.Set singleton, "show unconnected objects";
  ]
    (fun x -> wanted := x :: !wanted)
    "Display an object graph";

  (* Interesting event stuff starts here: *)
  let session_id = Client.Session.login_with_password ~rpc:newrpc ~uname:!username ~pwd:!password ~version:"1.2" ~originator:"graph" in
  let classes = List.filter (fun x -> List.mem x Datamodel.expose_get_all_messages_for) all_classes in
  List.iter (fun x -> if not(List.mem x classes) then failwith (Printf.sprintf "Class %s not available" x)) !wanted;

  let classes = List.filter (fun x -> x <> "task" && not(Astring.String.is_suffix ~affix:"_metrics" x) && not(Astring.String.is_suffix ~affix:"_cpu" x)) classes in

  let classes = List.filter (fun x -> !all || (List.mem x !wanted)) classes in

  List.iter
    (fun cls ->
       let all = get_all rpc session_id cls in
       List.iter (fun (x, xr) ->
           let node = { id = x; cls = cls; label = name_label_of_record cls xr } in
           nodes := NodeSet.add node !nodes;
           let links = refs_of_record cls xr in
           List.iter (fun y -> edges := EdgeSet.add { a = x; b = y } !edges) links
         ) all) classes;
  (* Filter all singleton nodes *)
  let is_connected edges nodes node =
    let node_exists nodes id = try ignore(node_of_id nodes id); true with _ -> false in
    EdgeSet.fold (fun edge acc ->
        (edge.a = node.id && node_exists nodes edge.b)
        ||
        (edge.b = node.id && node_exists nodes edge.a)
        ||
        acc) edges false in
  let nodes = NodeSet.filter (fun x -> !singleton || is_connected !edges !nodes x) !nodes in
  output_dot nodes !edges stdout
(*

  Client.Event.register ~rpc ~session_id ~classes:["*"];
  while true do
    let events = events_of_xmlrpc (Client.Event.next ~rpc ~session_id) in
    List.iter (fun event -> print_endline (string_of_event event)) events;
    flush stdout
  done
*)
