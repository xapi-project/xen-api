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
  time       : int64;
  row_data   : float array;
}

type t = {
  start_time : int64;
  step       : int64;
  end_time   : int64;
  legend     : string array;
  data       : row array;
}


(** Debugging only *)
let string_of t =
  let leg_string = 
    Printf.sprintf "[%s]" 
      (String.concat ";" 
         (List.map (fun l -> Printf.sprintf "\"%s\"" l) (Array.to_list t.legend)))
  in

  let data_string =
    Printf.sprintf "[|%s|]" 
      (String.concat ";\n" 
         (List.map (fun row -> 
              Printf.sprintf "{time=%Ld; row_data=[|%s|]}" row.time 
                (String.concat "; " 
                   (List.map (fun f -> Printf.sprintf "%0.4f" f) 
                      (Array.to_list row.row_data)))) 
             (Array.to_list t.data))) 
  in

  Printf.sprintf 
    "start_time:\t%Ld\nstep:\t\t%Ld\nend_time:\t%Ld\nlegend:\t\t%s\ndata:\n%s\n"
    t.start_time t.step t.end_time leg_string data_string

(* Helper utility - use create_multi instead *)
let create rra_timestep rras first_rra last_cdp_time first_cdp_time start legends =
  let rec do_data i accum =
    let time = Int64.(sub (last_cdp_time) (mul (of_int i) rra_timestep)) in
    if (time < start) || (i >= first_rra.rra_row_cnt) 
    then (List.rev accum) 
    else
      let extract_row rra = List.map (fun ring -> Fring.peek ring i) (Array.to_list rra.rra_data) in
      let values = List.concat (List.map extract_row rras) in
      do_data (i+1) ({time = time; row_data = Array.of_list values}::accum)
  in

  let data = Array.of_list (do_data 0 []) in

  { start_time = first_cdp_time;
    step       = rra_timestep;
    end_time   = last_cdp_time;
    legend     = legends;
    data       = data;
  }

let xml_of t output =
  let tag tag next () = 
    Xmlm.output output (`El_start (("",tag),[])); 
    List.iter (fun x -> x ()) next; 
    Xmlm.output output (`El_end) 
  in
  let data dat () = Xmlm.output output (`Data dat) in  

  let xml_of_row row =
    let values = List.map (fun v ->
        tag "v" [data (Utils.f_to_s v)]) (Array.to_list row.row_data) in
    tag "row" ((tag "t" [data (Printf.sprintf "%Ld" row.time)])::values)
  in

  let rows = List.map xml_of_row (Array.to_list t.data) in
  let mydata = tag "data" rows in

  let meta = tag "meta" [
      tag "start" [data (Printf.sprintf "%Ld" t.start_time)];
      tag "step" [data (Printf.sprintf "%Ld" t.step)];
      tag "end" [data (Printf.sprintf "%Ld" t.end_time)];
      tag "rows" [data (Printf.sprintf "%d" (List.length rows))];
      tag "columns" [data (Printf.sprintf "%d" (Array.length t.legend))];
      tag "legend" (List.map (fun x -> tag "entry" [data x]) (Array.to_list t.legend))] in

  Xmlm.output output (`Dtd None);
  tag "xport" [meta; mydata] ()


let of_xml input =
  let open Utils.Xmlm_utils in

  let read_row i =
    read_block "row" (fun i ->
        let time = get_el "t" i in
        let values = read_all "v" (get_el "v") i [] in
        { time = Int64.of_string time;
          row_data = Array.of_list (List.map (fun v -> float_of_string v) values);
        }) i
  in

  let read_data i =
    Array.of_list (read_all "row" read_row i [])
  in

  let read_meta i =
    read_block "meta" (fun i ->
        let start_time = get_el "start" i   |> Int64.of_string in
        let step       = get_el "step" i    |> Int64.of_string in
        let end_time   = get_el "end" i     |> Int64.of_string in
        let rows       = get_el "rows" i    |> int_of_string in
        let columns    = get_el "columns" i |> int_of_string in
        let legend     = read_block "legend" 
            (fun i -> read_all "entry" (get_el "entry") i []) i |> Array.of_list in
        let data       = [| |] in
        let meta       = { start_time; step; end_time; legend; data } in 
        (meta, rows, columns)
      ) i
  in

  accept (`Dtd None) input;
  read_block "xport" (fun i ->
      let meta, _, _ = read_meta i in
      let data = read_block "data" read_data i in
      { meta with data = data }
    ) input


let json_of_t t =
  let buffer = Buffer.create 4096 in

  let do_data row =
    Printf.bprintf buffer "{t:%Ld,values:[%s]}" row.time (String.concat "," (List.map Utils.f_to_s (Array.to_list row.row_data)))
  in

  Printf.bprintf buffer "{meta: {start:%Ld,step:%Ld,end:%Ld,rows:%d,columns:%d," t.start_time t.step t.end_time (Array.length t.data) (Array.length t.legend);
  Printf.bprintf buffer "legend:[%s]}," (String.concat "," (List.map (fun x -> "\"" ^ x ^ "\"") (Array.to_list t.legend)));
  Printf.bprintf buffer "data:[";
  for i=0 to Array.length t.data - 2 do
    do_data t.data.(i);
    Printf.bprintf buffer "%s" ",";
  done;
  do_data t.data.(Array.length t.data - 1);
  Printf.bprintf buffer "]}";
  Buffer.contents buffer


(** Export data from a bunch of rrds. Specify a prefix per rrd to be
    put onto legend. Note that each rrd *must* have the same timestep
    and have been updated at the same time, and *must* have
    homogeneous rras too. If not, those that dont look like the 1st
    one will be silently dropped. The export format is the rrdtool
    'xport' format. *)

let create_multi prefixandrrds start interval cfopt =
  let first_rrd = snd (List.hd prefixandrrds) in

  let pdp_interval = Int64.to_int (Int64.div interval first_rrd.timestep) in

  (* Sanity - make sure the RRDs are homogeneous *)
  let prefixandrrds = List.filter (fun (_prefix,rrd) -> rrd.timestep = first_rrd.timestep) prefixandrrds in

  (* Treat -ve start values as relative to the latest update. *)
  let start =
    prefixandrrds
    |> List.map (fun (_, rrd) -> if start < 0L then Int64.(add start (of_float rrd.last_updated)) else start)
    |> List.fold_left min Int64.max_int in

  let rras =
    (List.map
       (fun (_prefix,rrd) ->
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
  let (last_cdp_time,_age) = get_times first_rrd.last_updated rra_timestep in
  let (first_cdp_time_minus_one,_age) = get_times (Int64.to_float start) rra_timestep in
  let first_cdp_time = Int64.add first_cdp_time_minus_one rra_timestep in

  create rra_timestep rras first_rra last_cdp_time first_cdp_time start legends

let export ?(json=false) prefixandrrds start interval cfopt =
  let t = create_multi prefixandrrds start interval cfopt in
  if json then json_of_t t
  else
    let buffer = Buffer.create 10 in
    let output = Xmlm.make_output (`Buffer buffer) in
    xml_of t output;
    Buffer.contents buffer

let of_string s =
  let input = Xmlm.make_input (`String (0,s)) in
  of_xml input
