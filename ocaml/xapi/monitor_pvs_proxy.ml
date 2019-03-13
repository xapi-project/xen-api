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
module Strext = Stdext.Xstringext.String
module Mcache = Monitor_dbcalls_cache

module D = Debug.Make(struct let name = "monitor_pvs_proxy" end)
open D

module StringSet = Set.Make(String)
let dont_log_error = ref StringSet.empty

let find_rrd_files () =
  Sys.readdir Xapi_globs.metrics_root
  |> Array.to_list
  |> List.filter (Strext.startswith Xapi_globs.metrics_prefix_pvs_proxy)

  (* The PVS Proxy status cache [pvs_proxy_cached] contains the status
   * entries from PVS Proxies as reported via RRD. When the status
   * changes, it is updated in the xapi database. However: The xapi
   * databse is only updated for proxies that are currently attached.
   * This can lead to divergence between the cache and the database,
   * leading to error CA-229176. When the PVS Proxy is attached in
   * xapi_xenops.ml, the cache entry for the PVS Proxy is invalidated
   * such that it is picked up again and updated in the xapi database.
   * Inconsistencies are thus limited to the time between when a PVS
   * Proxy starts reporting its status and when it is attached.
   *)

let get_changes () =
  List.iter (fun filename ->
      try
        let path = Filename.concat Xapi_globs.metrics_root filename in
        let reader = Rrd_reader.FileReader.create path Rrd_protocol_v2.protocol in
        let payload = reader.Rrd_reader.read_payload () in
        dont_log_error := StringSet.remove filename !dont_log_error;

        payload.Rrd_protocol.datasources
        |> Lstext.filter_map (function
          | Rrd.VM vm_uuid, ds when ds.Ds.ds_name = "pvscache_status"
              -> Some (vm_uuid, ds)
          | _ -> None (* we are only interested in VM stats *)
          )
        |> List.iter (function vm_uuid, ds ->
          let value =
            match ds.Ds.ds_value with
            | Rrd.VT_Int64 v -> Int64.to_int v
            | Rrd.VT_Float v -> int_of_float v
            | Rrd.VT_Unknown -> -1
          in
          Hashtbl.add Mcache.pvs_proxy_tmp vm_uuid value
          )
      with e ->
        if not (StringSet.mem filename !dont_log_error) then begin
          error "Unable to read PVS-proxy status for %s: %s" filename (Printexc.to_string e);
          dont_log_error := StringSet.add filename !dont_log_error
        end
    ) (find_rrd_files ());

  (* Check if anything has changed since our last reading. *)
  Mcache.get_updates_map ~before:Mcache.pvs_proxy_cached ~after:Mcache.pvs_proxy_tmp

let set_changes ?except () =
  Mtxext.execute Mcache.pvs_proxy_cached_m (fun _ ->
      Mcache.transfer_map ?except ~source:Mcache.pvs_proxy_tmp ~target:Mcache.pvs_proxy_cached
    )

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
      let keeps = ref [] in
      List.iter (fun (vm_uuid, status) ->
          try
            let value = pvs_proxy_status_of_int status in
            let vm = Db.VM.get_by_uuid ~__context ~uuid:vm_uuid in
            Db.VM.get_VIFs ~__context ~self:vm
            |> Lstext.filter_map (fun vif -> Pvs_proxy_control.find_proxy_for_vif ~__context ~vif)
            |> List.filter (fun self -> Db.PVS_proxy.get_currently_attached ~__context ~self)
            |> List.iter (fun self -> Db.PVS_proxy.set_status ~__context ~self ~value)
          with e ->
            keeps := vm_uuid :: !keeps;
            error "Unable to update PVS-proxy status for %s: %s" vm_uuid (Printexc.to_string e);
        ) (get_changes ()) ;
      set_changes ~except:!keeps ()
    )
