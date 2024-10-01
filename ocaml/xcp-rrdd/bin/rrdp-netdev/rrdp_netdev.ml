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

module D = Debug.Make (struct let name = "xcp-rrdp-netdev" end)

module Process = Rrdd_plugin.Process (struct let name = "xcp-rrdd-netdev" end)

let generate_netdev_dss doms () =
  let uuid_of_domid domains domid =
    let _, uuid, _ =
      try List.find (fun (_, _, domid') -> domid = domid') domains
      with Not_found ->
        failwith
          (Printf.sprintf "Failed to find uuid corresponding to domid: %d" domid)
    in
    uuid
  in
  let open Network_stats in
  let stats = Network_stats.read_stats () in
  let dss, sum_rx, sum_tx =
    List.fold_left
      (fun (dss, sum_rx, sum_tx) (dev, stat) ->
        if not Astring.String.(is_prefix ~affix:"vif" dev) then
          let pif_name = "pif_" ^ dev in
          ( ( Rrd.Host
            , Ds.ds_make ~name:(pif_name ^ "_rx")
                ~description:
                  ("Bytes per second received on physical interface " ^ dev)
                ~units:"B/s" ~value:(Rrd.VT_Int64 stat.rx_bytes) ~ty:Rrd.Derive
                ~min:0.0 ~default:true ()
            )
            :: ( Rrd.Host
               , Ds.ds_make ~name:(pif_name ^ "_tx")
                   ~description:
                     ("Bytes per second sent on physical interface " ^ dev)
                   ~units:"B/s" ~value:(Rrd.VT_Int64 stat.tx_bytes)
                   ~ty:Rrd.Derive ~min:0.0 ~default:true ()
               )
            :: ( Rrd.Host
               , Ds.ds_make ~name:(pif_name ^ "_rx_errors")
                   ~description:
                     ("Receive errors per second on physical interface " ^ dev)
                   ~units:"err/s" ~value:(Rrd.VT_Int64 stat.rx_errors)
                   ~ty:Rrd.Derive ~min:0.0 ~default:false ()
               )
            :: ( Rrd.Host
               , Ds.ds_make ~name:(pif_name ^ "_tx_errors")
                   ~description:
                     ("Transmit errors per second on physical interface " ^ dev)
                   ~units:"err/s" ~value:(Rrd.VT_Int64 stat.tx_errors)
                   ~ty:Rrd.Derive ~min:0.0 ~default:false ()
               )
            :: dss
          , Int64.add stat.rx_bytes sum_rx
          , Int64.add stat.tx_bytes sum_tx
          )
        else
          ( ( try
                let d1, d2 =
                  Scanf.sscanf dev "vif%d.%d" (fun d1 d2 -> (d1, d2))
                in
                let vif_name = Printf.sprintf "vif_%d" d2 in
                (* Note: rx and tx are the wrong way round because from dom0 we
                   see the vms backwards *)
                let uuid = uuid_of_domid doms d1 in
                ( Rrd.VM uuid
                , Ds.ds_make ~name:(vif_name ^ "_tx") ~units:"B/s"
                    ~description:
                      ("Bytes per second transmitted on virtual interface \
                        number '"
                      ^ string_of_int d2
                      ^ "'"
                      )
                    ~value:(Rrd.VT_Int64 stat.rx_bytes) ~ty:Rrd.Derive ~min:0.0
                    ~default:true ()
                )
                :: ( Rrd.VM uuid
                   , Ds.ds_make ~name:(vif_name ^ "_rx") ~units:"B/s"
                       ~description:
                         ("Bytes per second received on virtual interface \
                           number '"
                         ^ string_of_int d2
                         ^ "'"
                         )
                       ~value:(Rrd.VT_Int64 stat.tx_bytes) ~ty:Rrd.Derive
                       ~min:0.0 ~default:true ()
                   )
                :: ( Rrd.VM uuid
                   , Ds.ds_make ~name:(vif_name ^ "_rx_errors") ~units:"err/s"
                       ~description:
                         ("Receive errors per second on virtual interface \
                           number '"
                         ^ string_of_int d2
                         ^ "'"
                         )
                       ~value:(Rrd.VT_Int64 stat.tx_errors) ~ty:Rrd.Derive
                       ~min:0.0 ~default:false ()
                   )
                :: ( Rrd.VM uuid
                   , Ds.ds_make ~name:(vif_name ^ "_tx_errors") ~units:"err/s"
                       ~description:
                         ("Transmit errors per second on virtual interface \
                           number '"
                         ^ string_of_int d2
                         ^ "'"
                         )
                       ~value:(Rrd.VT_Int64 stat.rx_errors) ~ty:Rrd.Derive
                       ~min:0.0 ~default:false ()
                   )
                :: dss
              with _ -> dss
            )
          , sum_rx
          , sum_tx
          )
      )
      ([], 0L, 0L) stats
  in
  [
    ( Rrd.Host
    , Ds.ds_make ~name:"pif_aggr_rx"
        ~description:"Bytes per second received on all physical interfaces"
        ~units:"B/s" ~value:(Rrd.VT_Int64 sum_rx) ~ty:Rrd.Derive ~min:0.0
        ~default:true ()
    )
  ; ( Rrd.Host
    , Ds.ds_make ~name:"pif_aggr_tx"
        ~description:"Bytes per second sent on all physical interfaces"
        ~units:"B/s" ~value:(Rrd.VT_Int64 sum_tx) ~ty:Rrd.Derive ~min:0.0
        ~default:true ()
    )
  ]
  @ dss

let _ =
  Xenctrl.with_intf (fun xc ->
      let _, domains, _ = Xenctrl_lib.domain_snapshot xc in
      Process.initialise () ;
      (* Share one page per virtual NIC - documentation specifies max is 512 *)
      let shared_page_count = 512 in
      Process.main_loop ~neg_shift:0.5
        ~target:(Reporter.Local shared_page_count) ~protocol:Rrd_interface.V2
        ~dss_f:(generate_netdev_dss domains)
  )
