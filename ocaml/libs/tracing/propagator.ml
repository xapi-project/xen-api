(*
 * Copyright (c) Cloud Software Group, Inc.
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

module type S = sig
  type carrier

  val inject_into : Tracing.TraceContext.t -> carrier -> carrier

  val extract_from : carrier -> Tracing.TraceContext.t
end

let ( let* ) = Option.bind

let ( >> ) f g x = g (f x)

let maybe f = function Some _ as o -> f o | _ -> Fun.id

let[@tail_mod_cons] rec filter_append p xs ys =
  match xs with
  | [] ->
      ys
  | x :: xs when p x ->
      x :: filter_append p xs ys
  | _ :: xs ->
      filter_append p xs ys

module Http = struct
  type carrier = Http.Request.t

  open struct
    let hdr_traceparent = "traceparent"

    let hdr_baggage = "baggage"
  end

  let alloc_assoc k kvs =
    List.filter_map
      (fun (key, value) -> if key = k then Some value else None)
      kvs
    |> function
    | [] ->
        None
    | xs ->
        Some xs

  let parse input =
    let open Astring.String in
    let trim_pair (key, value) = (trim key, trim value) in
    input
    |> cuts ~sep:";"
    |> List.map (cut ~sep:"=" >> Option.map trim_pair)
    |> List.filter_map Fun.id

  let inject_into ctx req =
    let open Tracing in
    let traceparent = (hdr_traceparent, TraceContext.traceparent_of ctx) in
    let baggage =
      let encoded =
        let encode =
          List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
          >> String.concat ";"
        in
        TraceContext.baggage_of ctx |> Option.map encode
      in
      (hdr_baggage, encoded)
    in
    let entries = [traceparent; baggage] in
    let filter_entries entries =
      let tbl = Hashtbl.create 47 in
      let record (k, v) =
        match v with
        | Some v ->
            Hashtbl.replace tbl k () ;
            Some (k, v)
        | _ ->
            None
      in
      let entries = List.filter_map record entries in
      (entries, fst >> Hashtbl.mem tbl)
    in
    let entries, to_replace = filter_entries entries in
    let headers = req.Http.Request.additional_headers in
    let additional_headers =
      filter_append (Fun.negate to_replace) headers entries
    in
    {req with additional_headers}

  let extract_from req =
    let open Tracing in
    let headers = req.Http.Request.additional_headers in
    let traceparent = List.assoc_opt hdr_traceparent headers in
    let baggage =
      let* all = alloc_assoc hdr_baggage headers in
      Some (List.concat_map parse all)
    in
    let open TraceContext in
    empty |> maybe with_traceparent traceparent |> maybe with_baggage baggage
end
