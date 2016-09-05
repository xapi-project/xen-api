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

open Stdext
open Threadext
open Listext
open Monitor_dbcalls_cache

module D = Debug.Make(struct let name = "monitor_pvs_proxy" end)
open D

let log_error = ref true

let get_changes () =
  try
    let reader = Rrd_reader.FileReader.create
        Xapi_globs.pvs_proxy_metrics_path Rrd_protocol_v2.protocol in
    let payload = reader.Rrd_reader.read_payload () in
    log_error := true;

    List.iter (function
        | Rrd.VM vm_uuid, ds ->
          let value =
            match ds.Ds.ds_value with
            | Rrd.VT_Int64 v -> Int64.to_int v
            | Rrd.VT_Float v -> int_of_float v
            | Rrd.VT_Unknown -> -1
          in
          if ds.Ds.ds_name = Xapi_globs.pvs_proxy_status_ds_name then
            Hashtbl.add pvs_proxy_tmp vm_uuid value
        | _ ->
          () (* we are only interested in VM stats *)
      ) payload.Rrd_protocol.datasources;

    (* Check if anything has changed since our last reading. *)
    Mutex.execute pvs_proxy_cached_m (fun _ ->
        let changes = get_updates_map ~before:pvs_proxy_cached ~after:pvs_proxy_tmp in
        transfer_map ~source:pvs_proxy_tmp ~target:pvs_proxy_cached;
        changes
      )
  with e ->
    if !log_error then begin
      error "Unable to read PVS-proxy status: %s" (Printexc.to_string e);
      log_error := false
    end;
    []

let pvs_proxy_status_of_int = function
  | 0 -> `stopped
  | 1 -> `initialised
  | 2 -> `caching
  | 3 -> `incompatible_write_cache_mode
  | 4 -> `incompatible_protocol_version
  | _ -> failwith "Unknown status"

let update () =
  Server_helpers.exec_with_new_task "Updating PVS-proxy status fields"
    (fun __context ->
       List.iter (fun (vm_uuid, status) ->
           try
             let vm = Db.VM.get_by_uuid ~__context ~uuid:vm_uuid in
             let vifs = Db.VM.get_VIFs ~__context ~self:vm in
             let proxies = List.filter_map (fun vif -> Pvs_proxy_control.find_proxy_for_vif ~__context ~vif) vifs in
             let value = pvs_proxy_status_of_int status in
             List.iter (fun self ->
                 Db.PVS_proxy.set_status ~__context ~self ~value
               ) proxies
           with e ->
             error "Unable to update PVS-proxy status for %s: %s" vm_uuid (Printexc.to_string e);
         ) (get_changes ())
    )
