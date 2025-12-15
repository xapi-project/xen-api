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

type discovery_error =
  | Devices_missing
  | Power_management_unavailable
  | Unknown
  | Command of string list * string * int
      (** command, reason, IPMI 2.0 spec error code *)

let discovery_error_to_string = function
  | Devices_missing ->
      "IPMI devices are missing"
  | Power_management_unavailable ->
      "Power management is not available"
  | Command (command, reason, code) ->
      Printf.sprintf "Command %s failed because %s (%x)"
        (String.concat " " command)
        reason code
  | Unknown ->
      "unknown"

let result_of_exec_cmd ~default = function
  | [], [] ->
      default
  | [], err :: _ ->
      Error err
  | oks, _ ->
      Ok oks

let get_dcmi_power_reading () =
  let command = ["power"; "reading"] in
  let read_out_line line =
    (* example line: '     Instantaneous power reading:                    34 Watts' *)
    try Scanf.sscanf line " Instantaneous power reading : %f Watts" Option.some
    with _ -> None
  in
  let read_err_line line =
    (* example line: '    DCMI request failed because: Invalid command (c1)' *)
    try
      Scanf.sscanf line " DCMI request failed because: %S@(%x)"
        (fun reason code -> Some (Command (command, reason, code))
      )
    with _ -> None
  in
  Utils.exec_cmd
    (module Process.D)
    ~cmdstring:(ipmitool ("dcmi" :: command))
    ~read_out_line ~read_err_line
  |> result_of_exec_cmd ~default:(Error Unknown)

let discover () =
  let ( let* ) = Result.bind in
  let read_out_line line =
    (* this code runs once on startup, logging all the output here will be useful for debugging *)
    D.debug "DCMI discover: %s" line ;
    let line = String.trim line in
    if String.equal line "Power management available" then
      Some ()
    else
      None
  in
  let read_err_line line =
    (* this code runs once on startup, logging all the output here will be useful for debugging *)
    D.debug "DCMI discover: %s" line ;
    let line = String.trim line in
    if String.starts_with ~prefix:"Could not open device at" line then
      Some Devices_missing
    else
      None
  in
  let* (_ : unit list) =
    Utils.exec_cmd
      (module Process.D)
      ~cmdstring:(ipmitool ["dcmi"; "discover"])
      ~read_out_line ~read_err_line
    |> result_of_exec_cmd ~default:(Error Power_management_unavailable)
  in
  get_dcmi_power_reading ()

let gen_dcmi_power_reading value =
  ( Rrd.Host
  , Ds.ds_make ~name:"DCMI-power-reading"
      ~description:"Host power usage reported by IPMI DCMI"
      ~value:(Rrd.VT_Float value) ~ty:Rrd.Gauge ~default:true ~units:"W"
      ~min:Float.min_float ~max:65534. ()
  )

let generate_dss () =
  match get_dcmi_power_reading () with
  | Ok (watts :: _) ->
      [gen_dcmi_power_reading watts]
  | _ ->
      []

let _ =
  initialise () ;
  match discover () with
  | Ok _ ->
      D.info "IPMI DCMI power reading is available" ;
      main_loop ~neg_shift:0.5 ~target:(Reporter.Local 1)
        ~protocol:Rrd_interface.V2 ~dss_f:generate_dss
  | Error reason ->
      let reason = discovery_error_to_string reason in
      D.warn "IPMI DCMI power readings not available, stopping. Reason: %s"
        reason
