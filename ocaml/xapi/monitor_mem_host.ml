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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

module Mtxext = Stdext.Threadext.Mutex
module Lstext = Stdext.Listext.List
module Mcache = Monitor_dbcalls_cache

module D = Debug.Make(struct let name = "monitor_mem_host" end)
open D

let get_changes () =
  let named_dss = List.flatten (List.map (fun filename ->
      try
        let datasources = Monitor_types.datasources_from_filename filename in
        Mcache.log_errors_from filename;

        datasources
        |> Lstext.filter_map (function
          | Rrd.Host, ds when List.mem ds.Ds.ds_name ["memory_total_kib"; "memory_free_kib"]
              -> Some ds
          | _ -> None (* we are only interested in Host memory stats *)
          )
        |> List.map (function ds ->
          let value =
            match ds.Ds.ds_value with
            | Rrd.VT_Int64 v -> Memory.bytes_of_kib v
            | Rrd.VT_Float v -> Memory.bytes_of_kib (Int64.of_float v)
            | Rrd.VT_Unknown -> -1L
          in
          ds.Ds.ds_name, value
          )
      with e ->
        if not (Mcache.is_ignored filename) then begin
          error "Unable to read host memory metrics from %s: %s" filename (Printexc.to_string e);
          Mcache.ignore_errors_from filename
        end;
        []
    ) (Monitor_types.find_rrd_files Xapi_globs.metrics_prefix_mem_host)) in

  let free_bytes = List.assoc_opt "memory_free_kib" named_dss in
  let total_bytes = List.assoc_opt "memory_total_kib" named_dss in

  (* Check if anything has changed since our last reading. *)
  match free_bytes, total_bytes with
   | (Some free, Some total) when
      !Mcache.host_memory_free_cached <> free ||
      !Mcache.host_memory_total_cached <> total ->
     Some (free, total)
   | _ -> None

let set_changes (free_bytes, total_bytes) =
  Mtxext.execute Mcache.host_memory_m (fun _ ->
      Mcache.host_memory_free_cached := free_bytes;
      Mcache.host_memory_total_cached := total_bytes;
    )

let update () =
  Server_helpers.exec_with_new_task "Updating host memory metrics"
    (fun __context ->
    let changes = get_changes () in
    match changes with
    | None -> ()
    | Some (free, total as c) ->
      try
        let host = Helpers.get_localhost ~__context in
        let metrics = Db.Host.get_metrics ~__context ~self:host in
        Db.Host_metrics.set_memory_total ~__context ~self:metrics ~value:total;
        Db.Host_metrics.set_memory_free ~__context ~self:metrics ~value:free;
        set_changes c
      with e ->
        error "Unable to update host memory metrics: %s" (Printexc.to_string e);
    )
