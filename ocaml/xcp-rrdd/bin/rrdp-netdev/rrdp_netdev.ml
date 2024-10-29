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
open Ezxenstore_core

module D = Debug.Make (struct let name = "xcp-rrdp-netdev" end)

module Process = Rrdd_plugin.Process (struct let name = "xcp-rrdd-netdev" end)

let fail = Printf.ksprintf failwith

type iface_stats = {
    tx_bytes: int64  (** bytes emitted *)
  ; tx_pkts: int64  (** packets emitted *)
  ; tx_errors: int64  (** error emitted *)
  ; rx_bytes: int64  (** bytes received *)
  ; rx_pkts: int64  (** packets received *)
  ; rx_errors: int64  (** error received *)
}

let default_stats =
  {
    tx_bytes= 0L
  ; tx_pkts= 0L
  ; tx_errors= 0L
  ; rx_bytes= 0L
  ; rx_pkts= 0L
  ; rx_errors= 0L
  }

let monitor_whitelist =
  ref
    [
      "eth"
    ; "vif" (* This includes "tap" owing to the use of standardise_name below *)
    ]

(** Transform names of the form 'tapX.X' to 'vifX.X' so these can be handled
   consistently later *)
let standardise_name name =
  try Scanf.sscanf name "tap%d.%d" @@ Printf.sprintf "vif%d.%d" with _ -> name

let get_link_stats () =
  let open Netlink in
  let s = Socket.alloc () in
  Socket.connect s Socket.NETLINK_ROUTE ;
  let cache = Link.cache_alloc s in
  let links = Link.cache_to_list cache in
  let links =
    let is_whitelisted name =
      List.exists
        (fun s -> Astring.String.is_prefix ~affix:s name)
        !monitor_whitelist
    in
    let is_vlan name =
      Astring.String.is_prefix ~affix:"eth" name && String.contains name '.'
    in
    List.map (fun link -> (standardise_name (Link.get_name link), link)) links
    |> (* Only keep interfaces with prefixes on the whitelist, and exclude VLAN
          devices (ethx.y). *)
    List.filter (fun (name, _) -> is_whitelisted name && not (is_vlan name))
  in
  let devs =
    List.map
      (fun (name, link) ->
        let convert x = Int64.of_int (Unsigned.UInt64.to_int x) in
        let eth_stat =
          {
            rx_bytes= Link.get_stat link Link.RX_BYTES |> convert
          ; rx_pkts= Link.get_stat link Link.RX_PACKETS |> convert
          ; rx_errors= Link.get_stat link Link.RX_ERRORS |> convert
          ; tx_bytes= Link.get_stat link Link.TX_BYTES |> convert
          ; tx_pkts= Link.get_stat link Link.TX_PACKETS |> convert
          ; tx_errors= Link.get_stat link Link.TX_ERRORS |> convert
          }
        in
        (name, eth_stat)
      )
      links
  in
  Cache.free cache ; Socket.close s ; Socket.free s ; devs

let make_bond_info devs (name, interfaces) =
  let devs' = List.filter (fun (name', _) -> List.mem name' interfaces) devs in
  let sum_list f =
    List.fold_left (fun ac (_, stat) -> Int64.add ac (f stat)) 0L devs'
  in
  let eth_stat =
    {
      rx_bytes= sum_list (fun stat -> stat.rx_bytes)
    ; rx_pkts= sum_list (fun stat -> stat.rx_pkts)
    ; rx_errors= sum_list (fun stat -> stat.rx_errors)
    ; tx_bytes= sum_list (fun stat -> stat.tx_bytes)
    ; tx_pkts= sum_list (fun stat -> stat.tx_pkts)
    ; tx_errors= sum_list (fun stat -> stat.tx_errors)
    }
  in
  (name, eth_stat)

let add_bonds bonds devs = List.map (make_bond_info devs) bonds @ devs

let transform_taps devs =
  let newdevnames = Xapi_stdext_std.Listext.List.setify (List.map fst devs) in
  List.map
    (fun name ->
      let devs' = List.filter (fun (n, _) -> n = name) devs in
      let tot =
        List.fold_left
          (fun acc (_, b) ->
            {
              rx_bytes= Int64.add acc.rx_bytes b.rx_bytes
            ; rx_pkts= Int64.add acc.rx_pkts b.rx_pkts
            ; rx_errors= Int64.add acc.rx_errors b.rx_errors
            ; tx_bytes= Int64.add acc.tx_bytes b.tx_bytes
            ; tx_pkts= Int64.add acc.tx_pkts b.tx_pkts
            ; tx_errors= Int64.add acc.tx_errors b.tx_errors
            }
          )
          default_stats devs'
      in
      (name, tot)
    )
    newdevnames

let generate_netdev_dss () =
  let uuid_of_domid domid =
    try
      Xenstore.with_xs (fun xs ->
          let vm = xs.Xenstore.Xs.getdomainpath domid ^ "/vm" in
          let vm_dir = xs.Xenstore.Xs.read vm in
          xs.Xenstore.Xs.read (vm_dir ^ "/uuid")
      )
    with e ->
      fail "Failed to find uuid corresponding to domid: %d (%s)" domid
        (Printexc.to_string e)
  in

  let dbg = "rrdp_netdev" in
  let from_cache = true in
  let bonds : (string * string list) list =
    Network_client.Client.Bridge.get_all_bonds dbg from_cache
  in

  let stats = get_link_stats () |> add_bonds bonds |> transform_taps in
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
                let uuid = uuid_of_domid d1 in
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
  Process.initialise () ;
  (* Share one page per virtual NIC - documentation specifies max is 512 *)
  let shared_page_count = 512 in
  Process.main_loop ~neg_shift:0.5 ~target:(Reporter.Local shared_page_count)
    ~protocol:Rrd_interface.V2 ~dss_f:generate_netdev_dss
