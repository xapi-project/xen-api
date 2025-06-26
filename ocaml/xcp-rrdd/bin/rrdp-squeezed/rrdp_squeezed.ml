(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Xapi_stdext_std
open Rrdd_plugin

module Process = Process (struct let name = "xcp-rrdd-squeezed" end)

module Xs = struct
  module Client = Xs_client_unix.Client (Xs_transport_unix_client)
  include Client

  let client = ref None

  (* Initialise the clients on demand - must be done after daemonisation! *)
  let get_client () =
    match !client with
    | Some client ->
        client
    | None ->
        let c = Client.make () in
        client := Some c ;
        c
end

module D = Debug.Make (struct let name = "rrdd-plugins" end)

module XSW = Ez_xenstore_watch.Make (D)
open XSW

let current_dynamic_max_values = ref IntMap.empty

let current_dynamic_min_values = ref IntMap.empty

let current_target_values = ref IntMap.empty

module MemoryActions = struct
  let interesting_paths_for_domain domid _ =
    let keys = ["dynamic-max"; "dynamic-min"; "target"] in
    List.map
      (fun key -> Printf.sprintf "/local/domain/%d/memory/%s" domid key)
      keys

  let watch_token domid =
    Printf.sprintf "xcp-rrdd-plugins/squeezed:domain-%d" domid

  let watch_fired _xc _ path domains _watches =
    D.debug "Watch fired on %s" path ;
    let read_new_value domid current_memory_values =
      let domid = int_of_string domid in
      if not (IntMap.mem domid domains) then
        D.debug "Ignoring watch on shutdown domain %d" domid
      else
        try
          let client = Xs.get_client () in
          let value =
            Xs.immediate client (fun xs -> Xs.read xs path)
            |> Int64.of_string
            |> Int64.mul 1024L
            (* convert from KiB to bytes *)
          in
          current_memory_values := IntMap.add domid value !current_memory_values
        with Xs_protocol.Enoent _ ->
          D.info
            "Couldn't read path %s; forgetting last known value for domain %d"
            path domid ;
          current_memory_values := IntMap.remove domid !current_memory_values
    in
    match List.filter (fun x -> x <> "") (Xstringext.String.split '/' path) with
    | ["local"; "domain"; domid; "memory"; "dynamic-max"] ->
        read_new_value domid current_dynamic_max_values
    | ["local"; "domain"; domid; "memory"; "dynamic-min"] ->
        read_new_value domid current_dynamic_min_values
    | ["local"; "domain"; domid; "memory"; "target"] ->
        read_new_value domid current_target_values
    | _ ->
        D.debug "Ignoring unexpected watch: %s" path

  let unmanaged_domain _ _ = false

  let found_running_domain _ _ = ()

  let domain_appeared _ _ _ = ()

  let domain_disappeared _ _ _ = ()
end

module Watcher = WatchXenstore (MemoryActions)

type values = {
    dynamic_max: int64 option
  ; dynamic_min: int64 option
  ; target: int64 option
}

let get_values (_, uuid, domid) =
  let get_current_value current_values =
    IntMap.find_opt domid !current_values
  in
  ( (uuid, domid)
  , {
      dynamic_max= get_current_value current_dynamic_max_values
    ; dynamic_min= get_current_value current_dynamic_min_values
    ; target= get_current_value current_target_values
    }
  )

let get_domain_stats xc =
  let _, domains, _ = Xenctrl_lib.domain_snapshot xc in
  List.map get_values domains

let generate_host_sources counters =
  let memory_reclaimed, memory_possibly_reclaimed =
    (* Calculate host metrics
       - Host memory reclaimed by squeezed =
       	   sum_across_running_vms(dynamic_max - target)
       - Host memory that could be reclaimed by squeezed =
       		 sum_across_running_vms(target - dynamic_min)
    *)
    let ( let* ) = Option.bind in
    counters
    |> List.fold_left
         (fun (acc1, acc2) (_, {dynamic_max; dynamic_min; target}) ->
           let r =
             let* target in
             let acc1 =
               let* max = dynamic_max in
               Some (Int64.add acc1 (Int64.sub max target))
             in
             let acc2 =
               let* min = dynamic_min in
               Some (Int64.add acc2 (Int64.sub target min))
             in
             Some (acc1, acc2)
           in
           match r with
           | None | Some (None, None) ->
               (acc1, acc2)
           | Some (Some acc1, Some acc2) ->
               (acc1, acc2)
           | Some (Some acc1, None) ->
               (acc1, acc2)
           | Some (None, Some acc2) ->
               (acc1, acc2)
         )
         (Int64.zero, Int64.zero)
  in
  (* Build corresponding Ds.ds values *)
  [
    ( Rrd.Host
    , Ds.ds_make ~name:"memory_reclaimed"
        ~description:"Host memory reclaimed by squeezed"
        ~value:(Rrd.VT_Int64 memory_reclaimed) ~ty:Rrd.Gauge ~default:true
        ~units:"B" ()
    )
  ; ( Rrd.Host
    , Ds.ds_make ~name:"memory_reclaimed_max"
        ~description:"Host memory that could be reclaimed by squeezed"
        ~value:(Rrd.VT_Int64 memory_possibly_reclaimed) ~ty:Rrd.Gauge
        ~default:true ~units:"B" ()
    )
  ]

let generate_sources xc () =
  let counters = get_domain_stats xc in
  generate_host_sources counters

(* This plugin always reports two datasources only, so one page is fine. *)
let host_page_count = 1

let vm_page_count = 0

let shared_page_count = host_page_count + vm_page_count

let () =
  Watcher.create_watcher_thread () ;
  Process.initialise () ;
  Xenctrl.with_intf (fun xc ->
      Process.main_loop ~neg_shift:0.5
        ~target:(Reporter.Local shared_page_count) ~protocol:Rrd_interface.V2
        ~dss_f:(generate_sources xc)
  )
