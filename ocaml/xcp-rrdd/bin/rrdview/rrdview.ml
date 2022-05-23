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

let _ =
  let filename = Sys.argv.(1) in
  let data = filename ^ ".rrd" in (* TODO: fixup and rrdtool restore *)
  let process_channel ic =
    let input = Xmlm.make_input (`Channel ic) in
    (* TODO: cannot parse xml with whitespace like you'd get out of xmllint -format,
       needs another xmlstrip strip transform *)
    let rrd = Rrd.from_xml input in
    process ~data rrd
  in
  with_input_file filename process_channel
