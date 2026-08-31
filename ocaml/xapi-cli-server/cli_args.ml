(*
 * Copyright (C) 2026 Vates.
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

(* [scope = Some prefix] restricts the visible entries to those whose key is
   [prefix] followed by at least one more character (any separator, matching the
   historical map-parameter parsing); their keys are seen with that prefix and
   its separator removed. [pairs] is shared with the unscoped structure the view
   was taken from. *)
type 'a t = {pairs: (string * 'a) list; scope: string option}

(* The entries visible through [t], with the scope prefix stripped from keys. *)
let visible t =
  match t.scope with
  | None ->
      t.pairs
  | Some prefix ->
      let plen = String.length prefix in
      let len = plen + 1 in
      List.filter_map
        (fun (k, v) ->
          if String.length k > len && String.sub k 0 plen = prefix then
            Some (String.sub k len (String.length k - len), v)
          else
            None
        )
        t.pairs

let from_pairs pairs = {pairs; scope= None}

let to_pairs = visible

let view prefix t = {t with scope= Some prefix}

let reserved = ["server"; "username"; "password"; "port"; "minimal"; "all"]

let is_reserved arg = List.mem arg reserved

let get key t = List.assoc key (visible t)

let get_opt key t = List.assoc_opt key (visible t)

let get_default key t default =
  match List.assoc_opt key (visible t) with None -> default | Some v -> v

let get_all key t =
  List.filter_map
    (fun (k, v) ->
      if k = key then
        Some v
      else
        None
    )
    (visible t)

let exists key t = List.mem_assoc key (visible t)

let keys t = List.map fst (visible t)

(* The mutating helpers below are only ever used on an unscoped structure. *)

let add key value t = {t with pairs= (key, value) :: t.pairs}

let remove key t = {t with pairs= List.remove_assoc key t.pairs}

let filter pred t = {t with pairs= List.filter (fun (k, _) -> pred k) t.pairs}

let filter_out pred t =
  {t with pairs= List.filter (fun (k, _) -> not (pred k)) t.pairs}
