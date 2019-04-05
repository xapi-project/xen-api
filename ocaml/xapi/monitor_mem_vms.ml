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

module D = Debug.Make(struct let name = "monitor_mem_vms" end)
open D

let get_changes () =
  List.iter (fun filename ->
      try
        let datasources = Monitor_types.datasources_from_filename filename in
        Mcache.log_errors_from filename;

        datasources
        |> Lstext.filter_map (function
          | Rrd.VM vm_uuid, ds when ds.Ds.ds_name = "memory"
              -> Some (vm_uuid, ds)
          | _ -> None (* we are only interested in VM stats *)
          )
        |> List.iter (function vm_uuid, ds ->
          let value =
            match ds.Ds.ds_value with
            | Rrd.VT_Int64 v -> v
            | Rrd.VT_Float v -> Int64.of_float v
            | Rrd.VT_Unknown -> -1L
          in
          Hashtbl.add Mcache.vm_memory_tmp vm_uuid value
          )
      with e ->
        if not (Mcache.is_ignored filename) then begin
          error "Unable to read memory usage for VM %s: %s" filename (Printexc.to_string e);
          Mcache.ignore_errors_from filename
        end
    ) (Monitor_types.find_rrd_files Xapi_globs.metrics_prefix_mem_vms);

  (* Check if anything has changed since our last reading. *)
  Mcache.get_updates_map ~before:Mcache.vm_memory_cached ~after:Mcache.vm_memory_tmp

let set_changes ?except () =
  Mtxext.execute Mcache.vm_memory_cached_m (fun _ ->
      Mcache.transfer_map ?except ~source:Mcache.vm_memory_tmp ~target:Mcache.vm_memory_cached
    )

let update () =
  Server_helpers.exec_with_new_task "Updating VM memory usage"
    (fun __context ->
      let host = Helpers.get_localhost ~__context in
      let keeps = ref [] in
      List.iter (fun (vm_uuid, memory) ->
          try
            let vm = Db.VM.get_by_uuid ~__context ~uuid:vm_uuid in
            let vmm = Db.VM.get_metrics ~__context ~self:vm in
            if (Db.VM.get_resident_on ~__context ~self:vm = host)
            then Db.VM_metrics.set_memory_actual ~__context ~self:vmm ~value:memory
            else Mcache.clear_cache_for_vm vm_uuid;
          with e ->
            keeps := vm_uuid :: !keeps;
            error "Unable to update memory usage for VM %s: %s" vm_uuid (Printexc.to_string e);
        ) (get_changes ()) ;
      set_changes ~except:!keeps ()
    )
