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
(*
 * Updates module: this module deals with the updates type, which
 * represents a delta between an RRD's past and current state.
 *)
(**
 * @group Performance Monitoring
*)

open Rrd

type row = {
  time : int64;
  row_data : float array;
}

type t = {
  start_time : int64;
  step       : int;
  end_time   : int64;
  legend     : string array;
  data       : row array;
}

(** Find the RRA with a particular CF that contains a particular start
    time, and also has a minimum pdp_cnt. If it can't find an
    appropriate one, either return the RRA with the correct CF that
    has the most ancient data, or raise No_RRA_Available if there's
    not archive with the correct CF. Assumes the RRAs are stored in
    increasing time-length *)
let find_best_rras rrd pdp_interval cf start =
  let rras = 
    match cf with 
    | Some realcf -> List.filter (fun rra -> rra.rra_cf=realcf) (Array.to_list rrd.rrd_rras) 
    | None -> Array.to_list rrd.rrd_rras in
  (if List.length rras = 0 then raise No_RRA_Available);
  let (last_pdp_time,age) = get_times rrd.last_updated rrd.timestep in
  let contains_time t rra =
    let lasttime = Int64.sub last_pdp_time (Int64.mul rrd.timestep (Int64.of_int (rra.rra_row_cnt * rra.rra_pdp_cnt))) in
    (rra.rra_pdp_cnt >= pdp_interval) && (t > lasttime) 
  in
  try
    let first_ok_rra = List.find (contains_time start) rras in
    let pdp_cnt = first_ok_rra.rra_pdp_cnt in
    let row_cnt = first_ok_rra.rra_row_cnt in
    let ok_rras = List.filter (fun rra -> rra.rra_row_cnt = row_cnt && rra.rra_pdp_cnt=pdp_cnt) rras in
    ok_rras
  with _ -> 
    let rra = List.hd (List.rev rras) in
    let newstarttime = Int64.add 1L (Int64.sub last_pdp_time (Int64.mul rrd.timestep (Int64.of_int (rra.rra_row_cnt * rra.rra_pdp_cnt)))) in
    List.filter (contains_time newstarttime) rras


(** Export data from a bunch of rrds. Specify a prefix per rrd to be
    put onto legend. Note that each rrd *must* have the same timestep
    and have been updated at the same time, and *must* have
    homogeneous rras too. If not, those that dont look like the 1st
    one will be silently dropped. The export format is the rrdtool
    'xport' format. *)
let to_xml output rra_timestep rras first_rra last_cdp_time first_cdp_time start legends =   
  let tag tag next () = 
    Xmlm.output output (`El_start (("",tag),[])); 
    List.iter (fun x -> x ()) next; 
    Xmlm.output output (`El_end) 
  in

  let data dat () = Xmlm.output output (`Data dat) in  

  let rec do_data i accum =
    let time = Int64.sub (last_cdp_time) (Int64.mul (Int64.of_int i) rra_timestep) in
    if (time < start) || (i >= first_rra.rra_row_cnt) then (List.rev accum) else
      let values = 
        List.concat 
          (List.map (fun rra -> 
               List.map (fun ring -> tag "v" [data (Utils.f_to_s (Fring.peek ring i))]) (Array.to_list rra.rra_data)) 
              rras) in
      do_data (i+1) ((tag "row" ((tag "t" [data (Printf.sprintf "%Ld" time)])::values))::accum)
  in

  let rows = do_data 0 [] in
  let mydata = tag "data" rows in

  let meta = tag "meta" [
      tag "start" [data (Printf.sprintf "%Ld" first_cdp_time)];
      tag "step" [data (Printf.sprintf "%Ld" rra_timestep)];
      tag "end" [data (Printf.sprintf "%Ld" last_cdp_time)];
      tag "rows" [data (Printf.sprintf "%d" (List.length rows))];
      tag "columns" [data (Printf.sprintf "%d" (Array.length legends))];
      tag "legend" (List.map (fun x -> tag "entry" [data x]) (Array.to_list legends))] in

  Xmlm.output output (`Dtd None);
  tag "xport" [meta; mydata] ()

let to_json rra_timestep rras first_rra last_cdp_time first_cdp_time start legends = 
  let rec do_data i accum =
    let time = Int64.sub (last_cdp_time) (Int64.mul (Int64.of_int i) rra_timestep) in
    if (time < start) || (i >= first_rra.rra_row_cnt) then (List.rev accum) else
      let values = "[" ^ 
                   (String.concat "," 
                      (List.concat (List.map (fun rra -> 
                           List.map (fun ring -> Utils.f_to_s (Fring.peek ring i)) (Array.to_list rra.rra_data))
                           rras))) ^ "]" in
      do_data (i+1) (("{t:"^(Printf.sprintf "%Ld" time)^",values:"^values^"}")::accum)
  in

  let rows = do_data 0 [] in
  let data = "["^(String.concat "," rows)^"]" in

  "{meta: {start:"^(Printf.sprintf "%Ld" first_cdp_time)^
  ",step:"^(Printf.sprintf "%Ld" rra_timestep)^
  ",end:"^(Printf.sprintf "%Ld" last_cdp_time)^
  ",rows:"^(Printf.sprintf "%d" (List.length rows))^
  ",columns:"^(Printf.sprintf "%d" (Array.length legends))^
  ",legend:["^(String.concat "," (List.map (fun x -> "\"" ^ x ^ "\"") (Array.to_list legends)))^"]},"^
  "data:"^data^"}"

let real_export marshaller prefixandrrds start interval cfopt =
  let first_rrd = snd (List.hd prefixandrrds) in

  let pdp_interval = Int64.to_int (Int64.div interval first_rrd.timestep) in

  (* Sanity - make sure the RRDs are homogeneous *)
  let prefixandrrds = List.filter (fun (prefix,rrd) -> rrd.timestep = first_rrd.timestep) prefixandrrds in

  let rras = 
    (List.map
       (fun (prefix,rrd) ->
          (* Find the rrds that satisfy the requirements *)
          Rrd.find_best_rras rrd pdp_interval cfopt start) prefixandrrds) in

  let legends = Array.concat (List.map2 (fun (prefix,rrd) rras ->
      let ds_legends = Array.map (fun ds -> prefix^ds.ds_name) rrd.rrd_dss in
      let ds_legends_with_cf_prefix = Array.concat
          (List.map (fun rra -> Array.map (fun name -> (cf_type_to_string rra.rra_cf)^":"^name) ds_legends) rras)
      in
      ds_legends_with_cf_prefix) prefixandrrds rras)
  in

  let rras = List.flatten rras in
  let first_rra = List.hd rras in

  let rras = List.filter (fun rra -> rra.rra_pdp_cnt=first_rra.rra_pdp_cnt && rra.rra_row_cnt = first_rra.rra_row_cnt) rras in
  (* The following timestep is that of the archive *)
  let rra_timestep = (Int64.mul first_rrd.timestep (Int64.of_int first_rra.rra_pdp_cnt)) in

  (* Get the last and first times of the CDPs to be returned *)
  let (last_cdp_time,age) = get_times first_rrd.last_updated rra_timestep in
  let (first_cdp_time_minus_one,age) = get_times (Int64.to_float start) rra_timestep in
  let first_cdp_time = Int64.add first_cdp_time_minus_one rra_timestep in

  marshaller rra_timestep rras first_rra last_cdp_time first_cdp_time start legends

let export ?(json=false) prefixandrrds start interval cfopt =
  if json then
    real_export to_json prefixandrrds start interval cfopt
  else
    let buffer = Buffer.create 10 in
    let output = Xmlm.make_output (`Buffer buffer) in
    real_export (to_xml output) prefixandrrds start interval cfopt;
    Buffer.contents buffer



