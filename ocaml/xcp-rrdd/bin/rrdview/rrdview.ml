(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

open Bos_setup

type def = Def of string * Rrd.cf_type

let name ~ds_name ~cf_type =
  cf_type |> Rrd.cf_type_to_string |> String.Ascii.lowercase |>
  Printf.sprintf "%s_%s" ds_name

let def ~data ~ds_name ~cf_type =
  let cfstr = Rrd.cf_type_to_string cf_type in
  let namestr = name ~ds_name ~cf_type in
  Def (ds_name, cf_type), Printf.sprintf "DEF:%s=%s:%s:%s" namestr data ds_name cfstr

let shape kind ~def:(Def (ds_name, cf_type)) ~r ~g ~b =
  let defstr = name ~ds_name ~cf_type in
  Printf.sprintf "%s:%s#%02x%02x%02x:%s(%s)" kind defstr r g b (Rrd.cf_type_to_string cf_type) ds_name

let area = shape "AREA"
let line = shape "LINE"

let rrdtool ~filename ~data title ~ds_name ~first ~last =
  let ds_min, def1 = def ~data ~ds_name ~cf_type:Rrd.CF_Min
  and ds_max, def2 = def ~data ~ds_name ~cf_type:Rrd.CF_Max
  and ds_avg, def3 = def ~data ~ds_name ~cf_type:Rrd.CF_Average
  in
  let graph =
    [ def1
    ; def2
    ; def3
    ; area ~def:ds_min ~r:0 ~g:0xff ~b:0
    ; line ~def:ds_avg ~r:0 ~g:0 ~b:0xff
    ; line ~def:ds_max ~r:0xff ~g:0 ~b:0
    ]
  in
  Cmd.(v "rrdtool" % "graph" % "--imgformat" % "SVG" % filename % "--title" % title
  % "--width" % "1024" % "--height" % "368"
  % "--start" % Int64.to_string first % "--end" % Int64.to_string last
  %% of_list graph)

let process ~data rrd =
  let open Rrd in
  for rra_i = 0 to Array.length rrd.rrd_rras - 1 do
    let rra = rrd.rrd_rras.(rra_i) in
    let timespan =
           Int64.mul
              (Int64.of_int (rra.rra_pdp_cnt * rra.rra_row_cnt))
              rrd.timestep
    in
    let start = rrd.last_updated -. Int64.to_float timespan
    in
    Printf.printf "start=%f\n" start ;
    for j = 0 to Array.length rrd.rrd_dss - 1 do
      let ds_name = rrd.rrd_dss.(j).ds_name in
      Printf.printf "Doing ds: %s\n" ds_name ;
      let filename =
         Printf.sprintf "rrd_data_%s_%s_%Ld.svg" ds_name
             (cf_type_to_string rra.rra_cf)
             (Int64.mul
                (Int64.of_int (rra.rra_pdp_cnt * rra.rra_row_cnt))
                rrd.timestep
             )
      in
      let title = ds_name (* TODO: better name? *) in
      let cmd = rrdtool ~data ~filename title ~ds_name ~first:(Int64.of_float start)
      ~last:(Int64.of_float rrd.last_updated) in
      OS.Cmd.run cmd |>
      Logs.on_error_msg ~use:(fun () -> failwith "rrdtool failed to run")
    done
  done

let finally f ~(always : unit -> unit) =
  match f () with
  | result ->
      always () ; result
  | exception e ->
      always () ; raise e

let with_input_file filename f =
  let ic = open_in filename in
  finally (fun () -> f ic) ~always:(fun () -> close_in ic)

type nicdir = RX | TX
let nicdir_of_string = function
  | "rx" -> RX
  | "tx" -> TX
  | _ -> assert false

let string_of_nicdir = function
  | RX -> "rx"
  | TX -> "tx"

let nicdir = Tyre.(conv nicdir_of_string string_of_nicdir @@ pcre "rx|tx")

type nickind =
  | Data of nicdir
  | Errors of nicdir

let nickind_of_match = function
  | nicdir, None -> Data nicdir
  | nicdir, Some () -> Errors nicdir

let match_of_nickind = function
  | Data nicdir -> nicdir, None
  | Errors nicdir -> nicdir, Some ()

let pp_nicdir = Fmt.(using string_of_nicdir string)

let pp_nickind ppf = function
  | Data nicdir -> Fmt.pf ppf "data %a" pp_nicdir nicdir
  | Errors nicdir -> Fmt.pf ppf "errors %a"  pp_nicdir nicdir

let nicdir_or_errors =
  Tyre.(nicdir <&> opt @@ str "_errors" |> conv nickind_of_match match_of_nickind)

type category =
  | SR of { id: string; dsname: string }
  | CPU of { idx: int; cstate: int option }
  | CPUavg
  | NIC of { name: string; idx:int option; kind: nickind }
  | Tapdisks_lowmem
  | Memory_kib of { dsname: string }
  | Memory_reclaimed_bytes of { dsname: string option }
  | Pool_count of { dsname: string }
  | Xapi_kib of {dsname: string}
  | Openfds of {dsname: string}
  | Loadavg
  | Unknown of string

let make_sr (id, dsname) = SR {id; dsname}
let make_cpu (idx, cstate) = CPU {idx; cstate}
let make_nic ((name, idx), kind) = NIC {name;idx; kind}
let make_memory_kib dsname = Memory_kib {dsname}
let make_memory_bytes dsname = Memory_reclaimed_bytes {dsname}
let make_pool_count dsname = Pool_count {dsname}
let make_xapi_kib dsname = Xapi_kib {dsname}
let make_openfds () = Openfds {dsname = "xapi"}

let pp ppf = function
  (* TODO: load description from datasource list *)
  | SR {id; dsname} -> Fmt.pf ppf "SR (uuid=%s) %s" id dsname
  | CPU {idx; cstate} -> Fmt.pf ppf "CPU %d %a" idx Fmt.(option int) cstate
  | CPUavg -> Fmt.pf ppf "CPUavg"
  | NIC {name;idx; kind} -> Fmt.pf ppf "NIC %s%a %a" name Fmt.(option int) idx pp_nickind kind
  | Tapdisks_lowmem -> Fmt.pf ppf "Tapdisks_lowmem"
  | Memory_kib {dsname} -> Fmt.pf ppf "Memory %s (KiB)" dsname
  | Memory_reclaimed_bytes {dsname} -> Fmt.pf ppf "Memory_reclaimed %a (bytes)" Fmt.(option string) dsname
  | Pool_count {dsname} -> Fmt.pf ppf "pool_count %s" dsname
  | Xapi_kib {dsname} -> Fmt.pf ppf "XAPI memory %s kib" dsname
  | Openfds {dsname} -> Fmt.pf ppf "openfds %s" dsname
  | Loadavg -> Fmt.pf ppf "loadavg"
  | Unknown dsname -> Fmt.pf ppf "Unknown %s" dsname

let groups =
  let open Tyre in
  let uuid8 = pcre "[0-9a-f]{8}" in
  let uuid_rest = pcre "(-[0-9a-f]{4}){3}-[0-9a-f]{12}" in
  let dsname = pcre "[a-zA-Z_]+" in
  [ (dsname <&> char '_' *> uuid8) --> make_sr
  ; (str "sr_" *> uuid8 <* uuid_rest <* char '_' <&> dsname) --> make_sr
  ; (str "cpu" *> int <&> (opt @@ str "-C" *> int)) --> make_cpu
  ; (str "cpu_avg") --> (fun () -> CPUavg)
  ; (pcre "pif_" *> (pcre "bond|eth|aggr") <&> opt int <* char '_' <&> nicdir_or_errors) --> make_nic
  ; (str "Tapdisks_in_low_memory_mode") --> (fun () -> Tapdisks_lowmem)
  ; (str "memory_" *> dsname <* str "_kib") --> make_memory_kib
  ; (str "memory_reclaimed" *> opt dsname) --> make_memory_bytes
  ; (str "pool_" *> dsname  <* str "_count") --> make_pool_count
  ; (str "xapi_" *> dsname <* str "_kib") --> make_xapi_kib
  ; (str "xapi_open_fds") --> make_openfds
  ; (str "loadavg") --> (fun () -> Loadavg)
  ]
  |> route

let pp_error ppf = function
  | `NoMatch (_re, str) -> Fmt.pf ppf "NoMatch '%s'" str
  | `ConverterFailure e -> Fmt.exn ppf e

let group name =
  let use _ = Unknown name in
  (* we have to shorten DS names to fit in RRD's 20 char limit *)
  Tyre.exec groups name
  |> Logs.on_error ~pp:pp_error ~use

let group_graphs rrd =
  let open Rrd in
  let names = ds_names rrd in
  names |> List.iter @@ fun name ->
  Logs.debug (fun m -> m "%s -> %a" name pp (group name))

let _ =
  let filename = Sys.argv.(1) in
  Logs.set_level (Some Logs.Debug);
  (* let data = filename ^ ".rrd" in (* TODO: fixup and rrdtool restore *) *)
  let process_channel ic =
    let input = Xmlm.make_input (`Channel ic) in
    (* TODO: cannot parse xml with whitespace like you'd get out of xmllint -format,
       needs another xmlstrip strip transform *)
    let rrd = Rrd.from_xml input in
    (* process ~data rrd *)
    group_graphs rrd
  in
  with_input_file filename process_channel
