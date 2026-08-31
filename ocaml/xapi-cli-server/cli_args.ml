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

type 'a t = (string * 'a) list

let from_pairs args = args

let to_pairs args = args

let reserved = ["server"; "username"; "password"; "port"; "minimal"; "all"]

let is_reserved arg = List.mem arg reserved

let get = List.assoc

let get_opt = List.assoc_opt

let get_default key args default =
  match List.assoc_opt key args with None -> default | Some v -> v

let get_all key args =
  List.filter_map
    (fun (k, v) ->
      if k = key then
        Some v
      else
        None
    )
    args

let exists = List.mem_assoc

let add key value args = (key, value) :: args

let filter pred = List.filter (fun (k, _) -> pred k)

let filter_out pred = List.filter (fun (k, _) -> not (pred k))

let remove = List.remove_assoc

let keys args = List.map fst args
