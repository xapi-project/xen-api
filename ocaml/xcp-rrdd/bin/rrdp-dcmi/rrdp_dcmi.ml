(*
 * Copyright (c) 2024 Cloud Software Group, Inc.
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

(** Read power measurements from IPMI DCMI where available.
    There is also IPMI SDR entity 21 that returns the same information (power consumption in watts),
    but isn't always available, and seems to be slower to read, especially when missing.
 *)

open Rrdd_plugin

module Process = Process (struct let name = "xcp-rrdd-dcmi" end)

open Process

let ipmitool_bin = "/usr/bin/ipmitool"

let ipmitool args =
  (* we connect to the local /dev/ipmi0 if available to read measurements from local BMC *)
  ipmitool_bin :: args |> String.concat " "

let discover () =
  Utils.exec_cmd
    (module Process.D)
    ~cmdstring:(ipmitool ["dcmi"; "discover"])
    ~f:(fun line ->
      (* this code runs once on startup, logging all the output here will be useful for debugging *)
      D.debug "DCMI discover: %s" line ;
      if String.trim line = "Power management available" then
        Some ()
      else
        None
    )

let get_dcmi_power_reading () =
  Utils.exec_cmd
    (module Process.D)
    ~cmdstring:(ipmitool ["dcmi"; "power"; "reading"])
    ~f:(fun line ->
      (* example line: '     Instantaneous power reading:                    34 Watts' *)
      try
        Scanf.sscanf line " Instantaneous power reading : %f Watts" Option.some
      with Scanf.Scan_failure _ | End_of_file -> None
    )

let gen_dcmi_power_reading value =
  ( Rrd.Host
  , Ds.ds_make ~name:"DCMI-power-reading"
      ~description:"Host power usage reported by IPMI DCMI"
      ~value:(Rrd.VT_Float value) ~ty:Rrd.Gauge ~default:true ~units:"W"
      ~min:Float.min_float ~max:65534. ()
  )

let generate_dss () =
  match get_dcmi_power_reading () with
  | watts :: _ ->
      [gen_dcmi_power_reading watts]
  | _ ->
      []

let _ =
  initialise () ;
  match discover () with
  | [] ->
      D.warn "IPMI DCMI power readings not available, stopping." ;
      exit 0
  | _ ->
      D.info "IPMI DCMI power reading is available" ;
      main_loop ~neg_shift:0.5 ~target:(Reporter.Local 1)
        ~protocol:Rrd_interface.V2 ~dss_f:generate_dss
