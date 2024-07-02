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

module Unixext = Xapi_stdext_unix.Unixext
module Config_file = Xcp_service.Config_file

module D = Debug.Make (struct let name = "recommendations" end)

open D
module StringMap = Map.Make (String)

let process_line map data =
  match Config_file.parse_line data with
  | Some (k, v) ->
      debug "Parsing data, key: %s, value: %s" k v ;
      StringMap.add k v map
  | None ->
      map

let parse map filename =
  debug "Parsing recommendations file: %s" filename ;
  Unixext.file_lines_fold process_line map filename

let load ~path =
  (try Sys.readdir path with _ -> [||])
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".conf")
  |> List.stable_sort compare
  |> List.map (Filename.concat path)
  |> List.filter (fun f ->
         match Unix.((stat f).st_kind) with
         | Unix.S_REG ->
             true
         | _ ->
             false
         | exception _ ->
             false
     )
  |> List.fold_left parse StringMap.empty
