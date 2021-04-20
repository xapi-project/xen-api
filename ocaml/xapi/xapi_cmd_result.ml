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
module D = Debug.Make (struct let name = "xapi_cmd_result" end)

open D
module RMap = Map.Make (String)

type t = string RMap.t

let empty = RMap.empty

let add ?(sep = ':') line results =
  match String.index_opt line sep with
  | Some _ ->
      let k, v =
        String.split_on_char sep line |> fun l ->
        (List.nth l 0 |> String.trim, List.nth l 1 |> String.trim)
      in
      RMap.add k v results
  | _ ->
      results

let find key results = RMap.find_opt key results

let of_output_opt ~sep ~key ~lines =
  String.split_on_char '\n' lines
  |> List.fold_left (fun results line -> add ~sep line results) empty
  |> find key
