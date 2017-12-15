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

let enabled = ref false

let m = Mutex.create ()
open Xapi_stdext_monadic
open Xapi_stdext_threads.Threadext

module Statefile_latency = struct
  open Rrd.Statefile_latency
  (* There will be more than one statefile at some point *)
  let all = ref []

  let get_one t =
    Ds.ds_make ~name:(Printf.sprintf "statefile/%s/latency" t.id) ~units:"s"
      ~description:"Turn-around time of the latest State-File access from the local host"
      ~value:(Opt.default Rrd.VT_Unknown (Opt.map (fun x -> Rrd.VT_Float x) t.latency))
      ~ty:Rrd.Gauge ~default:false ()

  let get_all () = List.map get_one !all
end

module Heartbeat_latency = struct
  let raw : float option ref = ref None

  let get () =
    Ds.ds_make ~name:"network/latency" ~units:"s"
      ~description:"Interval between the last two heartbeats transmitted from the local host to all Online hosts"
      ~value:(Opt.default Rrd.VT_Unknown (Opt.map (fun x -> Rrd.VT_Float x) !raw))
      ~ty:Rrd.Gauge ~default:false ()
end

module Xapi_latency = struct
  let raw : float option ref = ref None

  let get () =
    Ds.ds_make ~name:"xapi_healthcheck/latency" ~units:"s"
      ~description:"Turn-around time of the latest xapi status monitoring call on the local host"
      ~value:(Opt.default Rrd.VT_Unknown (Opt.map (fun x -> Rrd.VT_Float x) !raw))
      ~ty:Rrd.Gauge ~default:false ()
end

let all () =
  Mutex.execute m (fun _ ->
      if !enabled then (
        let all =
          Statefile_latency.get_all () @
          [Heartbeat_latency.get (); Xapi_latency.get ()]
        in
        List.map (fun x -> Rrd.Host, x) all
      ) else []
    )
