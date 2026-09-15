(*
 * Copyright (c) Cloud Software Group, Inc
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

module Pool = Client.Client.Pool
module Host = Client.Client.Host
module PIF = Client.Client.PIF
module PIF_metrics = Client.Client.PIF_metrics

let string_of_mode = function
  | `inherited ->
      "inherited"
  | `enabled ->
      "enabled"
  | `disabled ->
      "disabled"

let mode = Alcotest.testable (Fmt.of_to_string string_of_mode) ( = )

(* The LLDP status in the metrics is refreshed on networkd's ~30s cadence; wait
   comfortably longer than that before reading it back. *)
let settle_time = 90.

let lldp_state rpc session_id pif =
  let metrics = PIF.get_metrics ~rpc ~session_id ~self:pif in
  PIF_metrics.get_lldp_neighbor ~rpc ~session_id ~self:metrics
  |> List.assoc_opt "state"

let managed_physical_pifs rpc session_id =
  PIF.get_all_records ~rpc ~session_id
  |> List.filter (fun (_, r) ->
      r.API.pIF_managed && r.API.pIF_physical && r.API.pIF_currently_attached
  )

let lldpd_service = "lldpd"

let lldpd_deployed =
  lazy
    ( try
        ignore
          (Forkhelpers.execute_command_get_output "/usr/bin/systemctl"
             ["cat"; lldpd_service]
          ) ;
        true
      with _ -> false
    )

let pick_test_pif rpc session_id =
  managed_physical_pifs rpc session_id
  |> List.filter (fun (_, r) ->
      Host.get_uuid ~rpc ~session_id ~self:r.API.pIF_host = Qt.localhost_uuid
  )
  |> List.map fst
  |> List.find_opt (fun pif -> lldp_state rpc session_id pif <> Some "blocked")

let check_lldp_state rpc session_id pif enabled =
  if Lazy.force lldpd_deployed then (
    Thread.delay settle_time ;
    let expected =
      if enabled then
        "enabled"
      else
        "disabled"
    in
    Alcotest.(check (option string))
      "status follows configuration" (Some expected)
      (lldp_state rpc session_id pif)
  )

let pif_mode_and_status rpc session_id enabled =
  let value =
    if enabled then
      `enabled
    else
      `disabled
  in
  pick_test_pif rpc session_id
  |> Option.iter @@ fun pif ->
     let orig = PIF.get_lldp_mode ~rpc ~session_id ~self:pif in
     Fun.protect
       ~finally:(fun () ->
         PIF.set_lldp_mode ~rpc ~session_id ~self:pif ~value:orig ~force:false
       )
       (fun () ->
         PIF.set_lldp_mode ~rpc ~session_id ~self:pif ~value ~force:true ;
         Alcotest.check mode "lldp_mode recorded in the DB" value
           (PIF.get_lldp_mode ~rpc ~session_id ~self:pif) ;
         check_lldp_state rpc session_id pif enabled
       )

let pif_lldp_enable rpc session_id () = pif_mode_and_status rpc session_id true

let pif_lldp_disable rpc session_id () =
  pif_mode_and_status rpc session_id false

let pool_enable_and_status rpc session_id enabled =
  pick_test_pif rpc session_id
  |> Option.iter @@ fun pif ->
     let pool = Qt.get_pool rpc session_id in
     let orig_pool = Pool.get_lldp_enabled ~rpc ~session_id ~self:pool in
     let orig_mode = PIF.get_lldp_mode ~rpc ~session_id ~self:pif in
     Fun.protect
       ~finally:(fun () ->
         Pool.set_lldp_enabled ~rpc ~session_id ~self:pool ~value:orig_pool
           ~force:false ;
         PIF.set_lldp_mode ~rpc ~session_id ~self:pif ~value:orig_mode
           ~force:false
       )
       (fun () ->
         (* Pin the PIF to inherited so it follows the pool-wide switch. *)
         PIF.set_lldp_mode ~rpc ~session_id ~self:pif ~value:`inherited
           ~force:false ;
         Pool.set_lldp_enabled ~rpc ~session_id ~self:pool ~value:enabled
           ~force:true ;
         Alcotest.(check bool)
           "lldp_enabled recorded in the DB" enabled
           (Pool.get_lldp_enabled ~rpc ~session_id ~self:pool) ;
         check_lldp_state rpc session_id pif enabled
       )

let pool_lldp_enable rpc session_id () =
  pool_enable_and_status rpc session_id true

let pool_lldp_disable rpc session_id () =
  pool_enable_and_status rpc session_id false

let tests () =
  let open Qt_filter in
  [
    [
      ("LLDP pool disable", `Slow, pool_lldp_disable)
    ; ("LLDP pool enable", `Slow, pool_lldp_enable)
    ; ("LLDP PIF disable", `Slow, pif_lldp_disable)
    ; ("LLDP PIF enable", `Slow, pif_lldp_enable)
    ]
    |> conn
  ]
  |> List.concat
