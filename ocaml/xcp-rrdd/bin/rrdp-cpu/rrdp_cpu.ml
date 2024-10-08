(*
 * Copyright (C) Cloud Software Group
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

open Rrdd_plugin

module D = Debug.Make (struct let name = "xcp-rrdp-cpu" end)

module Process = Rrdd_plugin.Process (struct let name = "xcp-rrdd-cpu" end)

let xen_flag_complement = Int64.(shift_left 1L 63 |> lognot)

(* This function is used for getting vcpu stats of the VMs present on this host. *)
let dss_vcpus xc doms =
  List.fold_left
    (fun dss (dom, uuid, domid) ->
      let maxcpus = dom.Xenctrl.max_vcpu_id + 1 in
      let rec cpus i dss =
        if i >= maxcpus then
          dss
        else
          let vcpuinfo = Xenctrl.domain_get_vcpuinfo xc domid i in
          (* Workaround for Xen leaking the flag XEN_RUNSTATE_UPDATE; using a
             mask of its complement ~(1 << 63) *)
          let cpu_time =
            Int64.(
              to_float @@ logand vcpuinfo.Xenctrl.cputime xen_flag_complement
            )
          in
          (* Convert from nanoseconds to seconds *)
          let cpu_time = cpu_time /. 1.0e9 in
          let cputime_rrd =
            ( Rrd.VM uuid
            , Ds.ds_make ~name:(Printf.sprintf "cpu%d" i) ~units:"(fraction)"
                ~description:(Printf.sprintf "CPU%d usage" i)
                ~value:(Rrd.VT_Float cpu_time) ~ty:Rrd.Derive ~default:true
                ~min:0.0 ~max:1.0 ()
            )
          in
          cpus (i + 1) (cputime_rrd :: dss)
      in
      (* Runstate info is per-domain rather than per-vcpu *)
      let dss =
        let dom_cpu_time =
          Int64.(to_float @@ logand dom.Xenctrl.cpu_time xen_flag_complement)
        in
        let dom_cpu_time =
          dom_cpu_time /. (1.0e9 *. float_of_int dom.Xenctrl.nr_online_vcpus)
        in
        try
          let ri = Xenctrl.domain_get_runstate_info xc domid in
          ( Rrd.VM uuid
          , Ds.ds_make ~name:"runstate_fullrun" ~units:"(fraction)"
              ~value:(Rrd.VT_Float (Int64.to_float ri.Xenctrl.time0 /. 1.0e9))
              ~description:"Fraction of time that all VCPUs are running"
              ~ty:Rrd.Derive ~default:false ~min:0.0 ()
          )
          :: ( Rrd.VM uuid
             , Ds.ds_make ~name:"runstate_full_contention" ~units:"(fraction)"
                 ~value:(Rrd.VT_Float (Int64.to_float ri.Xenctrl.time1 /. 1.0e9))
                 ~description:
                   "Fraction of time that all VCPUs are runnable (i.e., \
                    waiting for CPU)"
                 ~ty:Rrd.Derive ~default:false ~min:0.0 ()
             )
          :: ( Rrd.VM uuid
             , Ds.ds_make ~name:"runstate_concurrency_hazard"
                 ~units:"(fraction)"
                 ~value:(Rrd.VT_Float (Int64.to_float ri.Xenctrl.time2 /. 1.0e9))
                 ~description:
                   "Fraction of time that some VCPUs are running and some are \
                    runnable"
                 ~ty:Rrd.Derive ~default:false ~min:0.0 ()
             )
          :: ( Rrd.VM uuid
             , Ds.ds_make ~name:"runstate_blocked" ~units:"(fraction)"
                 ~value:(Rrd.VT_Float (Int64.to_float ri.Xenctrl.time3 /. 1.0e9))
                 ~description:
                   "Fraction of time that all VCPUs are blocked or offline"
                 ~ty:Rrd.Derive ~default:false ~min:0.0 ()
             )
          :: ( Rrd.VM uuid
             , Ds.ds_make ~name:"runstate_partial_run" ~units:"(fraction)"
                 ~value:(Rrd.VT_Float (Int64.to_float ri.Xenctrl.time4 /. 1.0e9))
                 ~description:
                   "Fraction of time that some VCPUs are running, and some are \
                    blocked"
                 ~ty:Rrd.Derive ~default:false ~min:0.0 ()
             )
          :: ( Rrd.VM uuid
             , Ds.ds_make ~name:"runstate_partial_contention"
                 ~units:"(fraction)"
                 ~value:(Rrd.VT_Float (Int64.to_float ri.Xenctrl.time5 /. 1.0e9))
                 ~description:
                   "Fraction of time that some VCPUs are runnable and some are \
                    blocked"
                 ~ty:Rrd.Derive ~default:false ~min:0.0 ()
             )
          :: ( Rrd.VM uuid
             , Ds.ds_make
                 ~name:(Printf.sprintf "cpu_usage")
                 ~units:"(fraction)"
                 ~description:(Printf.sprintf "Domain CPU usage")
                 ~value:(Rrd.VT_Float dom_cpu_time) ~ty:Rrd.Derive ~default:true
                 ~min:0.0 ~max:1.0 ()
             )
          :: dss
        with _ -> dss
      in
      try cpus 0 dss with _ -> dss
    )
    [] doms

let physcpus = ref [||]

let dss_pcpus xc =
  let len = Array.length !physcpus in
  let newinfos =
    if len = 0 then (
      let physinfo = Xenctrl.physinfo xc in
      let pcpus = physinfo.Xenctrl.nr_cpus in
      physcpus := if pcpus > 0 then Array.make pcpus 0L else [||] ;
      Xenctrl.pcpu_info xc pcpus
    ) else
      Xenctrl.pcpu_info xc len
  in
  let dss, len_newinfos =
    Array.fold_left
      (fun (acc, i) v ->
        ( ( Rrd.Host
          , Ds.ds_make ~name:(Printf.sprintf "cpu%d" i) ~units:"(fraction)"
              ~description:("Physical cpu usage for cpu " ^ string_of_int i)
              ~value:(Rrd.VT_Float (Int64.to_float v /. 1.0e9))
              ~min:0.0 ~max:1.0 ~ty:Rrd.Derive ~default:true
              ~transform:Rrd.Inverse ()
          )
          :: acc
        , i + 1
        )
      )
      ([], 0) newinfos
  in
  let sum_array = Array.fold_left (fun acc v -> Int64.add acc v) 0L newinfos in
  let avg_array = Int64.to_float sum_array /. float_of_int len_newinfos in
  let avgcpu_ds =
    ( Rrd.Host
    , Ds.ds_make ~name:"cpu_avg" ~units:"(fraction)"
        ~description:"Average physical cpu usage"
        ~value:(Rrd.VT_Float (avg_array /. 1.0e9))
        ~min:0.0 ~max:1.0 ~ty:Rrd.Derive ~default:true ~transform:Rrd.Inverse ()
    )
  in
  avgcpu_ds :: dss

let dss_loadavg () =
  [
    ( Rrd.Host
    , Ds.ds_make ~name:"loadavg" ~units:"(fraction)"
        ~description:"Domain0 loadavg"
        ~value:(Rrd.VT_Float (Rrdd_common.loadavg ()))
        ~ty:Rrd.Gauge ~default:true ()
    )
  ]

let count_power_state_running_domains domains =
  List.fold_left
    (fun count (dom, _, _) ->
      if not dom.Xenctrl.paused then count + 1 else count
    )
    0 domains

let dss_hostload xc domains =
  let physinfo = Xenctrl.physinfo xc in
  let pcpus = physinfo.Xenctrl.nr_cpus in
  let rec sum acc n f =
    match n with n when n >= 0 -> sum (acc + f n) (n - 1) f | _ -> acc
  in
  let load =
    List.fold_left
      (fun acc (dom, _, domid) ->
        sum 0 dom.Xenctrl.max_vcpu_id (fun id ->
            let vcpuinfo = Xenctrl.domain_get_vcpuinfo xc domid id in
            if vcpuinfo.Xenctrl.online && not vcpuinfo.Xenctrl.blocked then
              1
            else
              0
        )
        + acc
      )
      0 domains
  in
  let running_domains = count_power_state_running_domains domains in

  let load_per_cpu = float_of_int load /. float_of_int pcpus in
  [
    ( Rrd.Host
    , Ds.ds_make ~name:"hostload" ~units:"(fraction)"
        ~description:
          ("Host load per physical cpu, where load refers to "
          ^ "the number of vCPU(s) in running or runnable status."
          )
        ~value:(Rrd.VT_Float load_per_cpu) ~min:0.0 ~ty:Rrd.Gauge ~default:true
        ()
    )
  ; ( Rrd.Host
    , Ds.ds_make ~name:"running_vcpus" ~units:"count"
        ~description:"The total number of running vCPUs per host"
        ~value:(Rrd.VT_Int64 (Int64.of_int load))
        ~min:0.0 ~ty:Rrd.Gauge ~default:true ()
    )
  ; ( Rrd.Host
    , Ds.ds_make ~name:"running_domains" ~units:"count"
        ~description:"The total number of running domains per host"
        ~value:(Rrd.VT_Int64 (Int64.of_int running_domains))
        ~min:0.0 ~ty:Rrd.Gauge ~default:true ()
    )
  ]

let generate_cpu_ds_list xc domains () =
  dss_pcpus xc @ dss_vcpus xc domains @ dss_loadavg () @ dss_hostload xc domains

let _ =
  Xenctrl.with_intf (fun xc ->
      let _, domains, _ = Xenctrl_lib.domain_snapshot xc in
      Process.initialise () ;
      (* Share one page per PCPU and dom each *)
      let physinfo = Xenctrl.physinfo xc in
      let shared_page_count = physinfo.Xenctrl.nr_cpus + List.length domains in

      Process.main_loop ~neg_shift:0.5
        ~target:(Reporter.Local shared_page_count) ~protocol:Rrd_interface.V2
        ~dss_f:(generate_cpu_ds_list xc domains)
  )
