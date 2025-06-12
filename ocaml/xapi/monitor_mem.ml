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

module Mtxext = Xapi_stdext_threads.Threadext.Mutex
module Mcache = Monitor_dbcalls_cache

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let get_datasources rrd_files =
  List.filter_map
    (fun filename ->
      if String.starts_with ~prefix:Xapi_globs.metrics_prefix_mem filename then
        Some (filename, Monitor_types.datasources_from_filename filename)
      else
        None
    )
    rrd_files

module Host = struct
  let get_changes datasources =
    let named_dss =
      List.concat_map
        (fun (filename, datasources) ->
          try
            Mcache.log_errors_from filename ;
            datasources
            |> List.filter_map (function
                 | Rrd.Host, ds
                   when List.mem ds.Ds.ds_name
                          ["memory_total_kib"; "memory_free_kib"] ->
                     Some ds
                 | _ ->
                     None (* we are only interested in Host memory stats *)
                 )
            |> List.map (function ds ->
                   let value =
                     match ds.Ds.ds_value with
                     | Rrd.VT_Int64 v ->
                         Memory.bytes_of_kib v
                     | Rrd.VT_Float v ->
                         Memory.bytes_of_kib (Int64.of_float v)
                     | Rrd.VT_Unknown ->
                         -1L
                   in
                   (ds.Ds.ds_name, value)
                   )
          with e ->
            if not (Mcache.is_ignored filename) then (
              error "Unable to read host memory metrics from %s: %s" filename
                (Printexc.to_string e) ;
              Mcache.ignore_errors_from filename
            ) ;
            []
        )
        datasources
    in
    let free_bytes = List.assoc_opt "memory_free_kib" named_dss in
    let total_bytes = List.assoc_opt "memory_total_kib" named_dss in
    (* Check if anything has changed since our last reading. *)
    match (free_bytes, total_bytes) with
    | Some free, Some total
      when !Mcache.host_memory_free_cached <> free
           || !Mcache.host_memory_total_cached <> total ->
        Some (free, total)
    | _ ->
        None

  let set_changes (free_bytes, total_bytes) =
    Mtxext.execute Mcache.host_memory_m (fun _ ->
        Mcache.host_memory_free_cached := free_bytes ;
        Mcache.host_memory_total_cached := total_bytes
    )

  let update __context datasources =
    match get_changes datasources with
    | None ->
        ()
    | Some ((free, total) as c) -> (
      try
        let host = Helpers.get_localhost ~__context in
        let metrics = Db.Host.get_metrics ~__context ~self:host in
        Db.Host_metrics.set_memory_total ~__context ~self:metrics ~value:total ;
        Db.Host_metrics.set_memory_free ~__context ~self:metrics ~value:free ;
        set_changes c
      with e ->
        error "Unable to update host memory metrics: %s" (Printexc.to_string e)
    )
end

module VMs = struct
  let get_changes datasources =
    List.iter
      (fun (filename, datasources) ->
        try
          Mcache.log_errors_from filename ;
          datasources
          |> List.filter_map (function
               | Rrd.VM vm_uuid, ds when ds.Ds.ds_name = "memory" ->
                   Some (vm_uuid, ds)
               | _ ->
                   None (* we are only interested in VM stats *)
               )
          |> List.iter (function vm_uuid, ds ->
                 let value =
                   match ds.Ds.ds_value with
                   | Rrd.VT_Int64 v ->
                       v
                   | Rrd.VT_Float v ->
                       Int64.of_float v
                   | Rrd.VT_Unknown ->
                       -1L
                 in
                 Hashtbl.add Mcache.vm_memory_tmp vm_uuid value
                 )
        with e ->
          if not (Mcache.is_ignored filename) then (
            error "Unable to read memory usage for VM %s: %s" filename
              (Printexc.to_string e) ;
            Mcache.ignore_errors_from filename
          )
      )
      datasources ;
    (* Check if anything has changed since our last reading. *)
    Mcache.get_updates_map ~before:Mcache.vm_memory_cached
      ~after:Mcache.vm_memory_tmp

  let set_changes ?except () =
    Mtxext.execute Mcache.vm_memory_cached_m (fun _ ->
        Mcache.transfer_map ?except ~source:Mcache.vm_memory_tmp
          ~target:Mcache.vm_memory_cached ()
    )

  let update __context datasources =
    let host = Helpers.get_localhost ~__context in
    let keeps = ref [] in
    List.iter
      (fun (vm_uuid, memory) ->
        try
          let vm = Db.VM.get_by_uuid ~__context ~uuid:vm_uuid in
          let vmm = Db.VM.get_metrics ~__context ~self:vm in
          if Db.VM.get_resident_on ~__context ~self:vm = host then
            Db.VM_metrics.set_memory_actual ~__context ~self:vmm ~value:memory
          else
            Mcache.clear_cache_for_vm ~vm_uuid
        with e ->
          keeps := vm_uuid :: !keeps ;
          error "Unable to update memory usage for VM %s: %s" vm_uuid
            (Printexc.to_string e)
      )
      (get_changes datasources) ;
    set_changes ~except:!keeps ()
end

let update rrd_files =
  let ( let@ ) f x = f x in
  let@ __context =
    Server_helpers.exec_with_new_task "Updating memory metrics"
  in
  let datasources = get_datasources rrd_files in
  if datasources = [] then
    error "%s: no memory datasources found!" __FUNCTION__
  else (
    Host.update __context datasources ;
    VMs.update __context datasources
  )
