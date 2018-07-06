(*
 * Copyright (C) Citrix Systems Inc.
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

module StringSet = Set.Make(struct type t = string let compare = compare end)

type 'a t = {
  client: 'a;
  tid: int32;                                 (* transaction id in use (0 means no transaction) *)
  mutable accessed_paths: StringSet.t option; (* paths read or written to *)
  mutable watched_paths: StringSet.t;         (* paths being watched *)
}

let make client = {
  client = client;
  tid = 0l;                       (* no transaction *)
  accessed_paths = None;          (* not recording accesses *)
  watched_paths = StringSet.empty (* no paths watched *)
}

let get_tid h = h.tid

let get_client h = h.client

let no_transaction client = make client

let transaction client tid = { (make client) with tid = tid }

let watching client = { (make client) with accessed_paths = Some StringSet.empty }

let accessed_path h path = match h.accessed_paths with
  | None -> h
  | Some ps -> h.accessed_paths <- Some (StringSet.add path ps); h

let get_accessed_paths h = match h.accessed_paths with
  | None -> StringSet.empty
  | Some xs -> xs

let watch h path = h.watched_paths <- StringSet.add path h.watched_paths; h

let unwatch h path = h.watched_paths <- StringSet.remove path h.watched_paths; h

let get_watched_paths h = h.watched_paths


