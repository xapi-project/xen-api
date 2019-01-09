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

let text_export rrd =
  let open Rrd in
  for rra_i=0 to Array.length rrd.rrd_rras - 1 do
    let rra = rrd.rrd_rras.(rra_i) in
    let start = rrd.last_updated -. (Int64.to_float (Int64.mul (Int64.of_int (rra.rra_pdp_cnt * rra.rra_row_cnt)) rrd.timestep)) in
    Printf.printf "start=%f\n" start;
    let rra_timestep = (Int64.mul rrd.timestep (Int64.of_int rra.rra_pdp_cnt)) in

    (* Get the last and first times of the CDPs to be returned *)
    let (last_cdp_time,_age) = get_times rrd.last_updated rra_timestep in

    let time i = Int64.sub (last_cdp_time) (Int64.mul (Int64.of_int i) rra_timestep) in

    for j=0 to Array.length rrd.rrd_dss - 1 do
      Printf.printf "Doing ds: %s\n" rrd.rrd_dss.(j).ds_name;
      let oc = open_out (Printf.sprintf "rrd_data_%s_%s_%Ld.dat" rrd.rrd_dss.(j).ds_name (cf_type_to_string rra.rra_cf) (Int64.mul (Int64.of_int (rra.rra_pdp_cnt* rra.rra_row_cnt)) rrd.timestep)) in
      let rec do_data i accum =
        if (time i < Int64.of_float start) || (i >= rra.rra_row_cnt) then (List.rev accum) else
          do_data (i+1) ((time i, Fring.peek rra.rra_data.(j)  i)::accum)
      in
      let data = do_data 0 [] in
      List.iter (fun (t,d) -> if not (Utils.isnan d) then Printf.fprintf oc "%Ld %f\n" t d) data;
      close_out oc
    done
  done

let with_input_file filename f =
  let ic = open_in filename in
finally (fun () -> f ic) (fun () -> close_in ic)

let _ =
  let dump_channel ic =
    let input = Xmlm.make_input ic in
    let rrd = Rrd.from_xml input in
    text_export rrd
  in
  with_input_file Sys.argv.(1) dump_channel
