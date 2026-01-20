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

let current_free_values = ref IntMap.empty

module MemoryActions = struct
  let interesting_paths_for_domain domid _ =
    let keys =
      [
        "memory/dynamic-max"
      ; "memory/dynamic-min"
      ; "memory/target"
      ; "data/meminfo_free"
      ]
    in
    List.map (fun key -> Printf.sprintf "/local/domain/%d/%s" domid key) keys

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
            Xs.immediate client (fun xs -> Xs.read xs path) |> Int64.of_string
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
    | ["local"; "domain"; domid; "data"; "meminfo_free"] ->
        read_new_value domid current_free_values
    | _ ->
        D.debug "Ignoring unexpected watch: %s" path

  let unmanaged_domain _ _ = false

  let found_running_domain _ _ = ()

  let domain_appeared _ _ _ = ()

  let domain_disappeared _ _ _ = ()
end

module Watcher = WatchXenstore (MemoryActions)

(** All these values are reported in KiB *)
type values = {
    dynamic_max: int64 option
  ; dynamic_min: int64 option
  ; target: int64 option
  ; free: int64 option
}

let get_values ((_, _, domid) as dom) =
  let get_current_value current_values =
    IntMap.find_opt domid !current_values
  in
  ( dom
  , {
      dynamic_max= get_current_value current_dynamic_max_values
    ; dynamic_min= get_current_value current_dynamic_min_values
    ; target= get_current_value current_target_values
    ; free= get_current_value current_free_values
    }
  )

let get_domain_stats xc =
  let _, domains, _ = Xenctrl_lib.domain_snapshot xc in
  List.map get_values domains

let bytes_of_kib kib = Int64.mul 1024L kib

let generate_host_sources xc counters =
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
         (fun (acc1, acc2) (_, {dynamic_max; dynamic_min; target; _}) ->
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
  let memory_reclaimed = bytes_of_kib memory_reclaimed in
  let memory_possibly_reclaimed = bytes_of_kib memory_possibly_reclaimed in
  let physinfo = Xenctrl.physinfo xc in
  let total_kib =
    Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.total_pages)
  in
  let free_kib =
    Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.free_pages)
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
  ; ( Rrd.Host
    , Ds.ds_make ~name:"memory_total_kib"
        ~description:"Total amount of memory in the host"
        ~value:(Rrd.VT_Int64 total_kib) ~ty:Rrd.Gauge ~min:0.0 ~default:true
        ~units:"KiB" ()
    )
  ; ( Rrd.Host
    , Ds.ds_make ~name:"memory_free_kib"
        ~description:"Total amount of free memory"
        ~value:(Rrd.VT_Int64 free_kib) ~ty:Rrd.Gauge ~min:0.0 ~default:true
        ~units:"KiB" ()
    )
  ]

let res_error fmt = Printf.ksprintf Result.error fmt

let finally f finally = Fun.protect ~finally f

let scanning path f =
  let io = Scanf.Scanning.open_in path in
  finally (fun () -> f io) (fun () -> Scanf.Scanning.close_in io)

let scan path =
  try
    scanning path @@ fun io ->
    Scanf.bscanf io {|MemTotal: %_d %_s MemFree: %_d %_s MemAvailable: %Ld %s|}
      (fun size kb -> Ok (size, kb)
    )
  with _ -> res_error "failed to scan %s" path

let free_dom0 uuid =
  let result =
    match scan "/proc/meminfo" with
    | Ok (size, "kB") ->
        Ok size
    | Ok (_, unit) ->
        res_error "unexpected unit: %s" unit
    | Error e ->
        Error e
  in
  match result with
  | Ok mem ->
      Some
        ( Rrd.VM uuid
        , Ds.ds_make ~name:"memory_internal_free" ~units:"KiB"
            ~description:"Dom0 current free memory" ~value:(Rrd.VT_Int64 mem)
            ~ty:Rrd.Gauge ~min:0.0 ~default:true ()
        )
  | Error msg ->
      let _ =
        D.error "%s: retrieving  Dom0 free memory failed: %s" __FUNCTION__ msg
      in
      None

let free_other uuid free =
  Some
    ( Rrd.VM uuid
    , Ds.ds_make ~name:"memory_internal_free" ~units:"KiB"
        ~description:"Memory used as reported by the guest agent"
        ~value:(Rrd.VT_Int64 free) ~ty:Rrd.Gauge ~min:0.0 ~default:true ()
    )

let dss_numa_info uuid domid =
  try
    let handle = Xenctrlext.get_handle () in
    let host_nr_nodes = Xenctrlext.get_nr_nodes handle in
    let vm_nodes =
      Xenctrlext.DomainNuma.domain_get_numa_info_node_pages handle domid
    in
    let dss_memory_numa_nodes_of_vm (dss, nr_nodes_used_by_vm)
        (node_id, tot_pages_per_node) =
      (*
        for each numa node used by the host, show the
        corresponding amount of memory used by the vm
      *)
      let is_node_used_by_vm = tot_pages_per_node > 4096L in
      let is_node_used_by_host = node_id < host_nr_nodes in
      if is_node_used_by_host then
        ( ( Rrd.VM uuid
          , Ds.ds_make
              ~name:(Printf.sprintf "memory_numa_node_%d" node_id)
              ~units:"B"
              ~description:
                (Printf.sprintf "Memory from NUMA node %d used by VM" node_id)
              ~value:(Rrd.VT_Int64 (Int64.mul tot_pages_per_node 4096L))
              ~min:0.0 ~ty:Rrd.Gauge ~default:false ()
          )
          :: dss
          (* remember the number of nodes used by vm *)
        , nr_nodes_used_by_vm
          +
          if is_node_used_by_vm then
            1
          else
            0
        )
      else
        (dss, nr_nodes_used_by_vm)
    in
    let dss_numa_nodes_of_vm (dss, nr_nodes_used_by_vm) =
      ( Rrd.VM uuid
      , Ds.ds_make
          ~name:(Printf.sprintf "numa_nodes")
          ~units:"count"
          ~description:(Printf.sprintf "Number of NUMA nodes used by VM")
          ~value:(Rrd.VT_Int64 (Int64.of_int nr_nodes_used_by_vm))
          ~min:0.0 ~ty:Rrd.Gauge ~default:false ()
      )
      :: List.rev dss
    in
    vm_nodes.Xenctrlext.DomainNuma.tot_pages_per_node
    |> Array.mapi (fun i x -> (i, x))
    |> Array.fold_left dss_memory_numa_nodes_of_vm ([], 0)
    |> dss_numa_nodes_of_vm
  with e ->
    D.debug "dss_numa_info: %s" (Printexc.to_string e) ;
    []

let get_list f = Option.to_list (f ())

let generate_vm_sources domains =
  let metrics_of ((dom, uuid, domid), {target; free; _}) =
    let target () =
      Option.map
        (fun target ->
          let target = bytes_of_kib target in
          ( Rrd.VM uuid
          , Ds.ds_make ~name:"memory_target"
              ~description:"Target of VM balloon driver" ~units:"B"
              ~value:(Rrd.VT_Int64 target) ~ty:Rrd.Gauge ~min:0.0 ~default:true
              ()
          )
        )
        target
    in
    let free () =
      if domid = 0 then
        free_dom0 uuid
      else
        Option.bind free (free_other uuid)
    in
    let total () =
      let memory =
        Int64.of_nativeint dom.Xenctrl.total_memory_pages
        |> Xenctrl.pages_to_kib
        |> bytes_of_kib
      in
      Some
        ( Rrd.VM uuid
        , Ds.ds_make ~name:"memory"
            ~description:"Memory currently allocated to VM" ~units:"B"
            ~value:(Rrd.VT_Int64 memory) ~ty:Rrd.Gauge ~min:0.0 ~default:true ()
        )
    in
    let get_list_numa_info = dss_numa_info uuid domid in
    (* CA-34383: Memory updates from paused domains serve no useful purpose.
       During a migrate such updates can also cause undesirable
       discontinuities in the observed value of memory_actual. Hence, we
       ignore changes from paused domains: *)
    if dom.Xenctrl.paused then
      []
    else
      get_list target @ get_list free @ get_list total @ get_list_numa_info
  in

  List.concat_map metrics_of domains

let generate_sources xc () =
  let domain_stats = get_domain_stats xc in
  generate_host_sources xc domain_stats @ generate_vm_sources domain_stats

(** The json-like serialization for 3 dss in dss_mem_vms takes 622 bytes. These
    bytes plus some overhead make 1024 bytes an upper bound. *)

let bytes_per_mem_vm = 1024

let host_page_count = 1

let vm_page_count =
  ((Rrd_interface.max_supported_vms * bytes_per_mem_vm) + 4095) / 4096

let shared_page_count = host_page_count + vm_page_count

let () =
  Watcher.create_watcher_thread () ;
  Process.initialise () ;
  Xenctrl.with_intf (fun xc ->
      Process.main_loop ~neg_shift:0.5
        ~target:(Reporter.Local shared_page_count) ~protocol:Rrd_interface.V2
        ~dss_f:(generate_sources xc)
  )
