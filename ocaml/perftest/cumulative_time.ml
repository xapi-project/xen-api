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

open Perfdebug
open Graphutil

let _ =
  let inputs = ref [] in
  let format = ref `X11 in
  let separate_graphs = ref false in
  let graphic_filename = ref "" in
  Arg.parse [
    "-format", Arg.Symbol ([ "eps"; "gif"; "x11" ],
                           (function
                             | "eps" -> format := `Eps
                             | "gif" -> format := `Gif
                             | "x11" -> format := `X11
                             | _ -> failwith "huh ?")),
    " Set output format (default: X11)";
    "-output", Arg.Set_string graphic_filename,                    " Set default output file (for non-X11 modes)";
    "-separate", Arg.Set separate_graphs,                          " Plot each data series on separate axes";
  ]
    (fun x -> inputs := x :: !inputs)
    "Generate a histogram by convolving sample points with a gaussian.\nusage:";
  if !inputs = [] then failwith "Needs at least one input filename";
  if !format <> `X11 && !graphic_filename = "" then failwith "This format needs an -output";
  let inputs = get_info ~separate:!separate_graphs !inputs in

  let output_files = List.map (fun _ -> Filename.temp_file "cumulative" "dat") inputs in
  let all = List.combine inputs output_files in

  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
       let max_readings = ref 0 in

       List.iter
         (fun ((info,points), output_file) ->
            let (_: string) = get_result info in
            let num_points = List.length points in

            max_readings := max num_points !max_readings;

            let open Xapi_stdext_unix in
            Unixext.with_file output_file [ Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT ] 0o644
              (fun fd ->
                 let points_array = Array.of_list (List.rev points) in
                 let cumulative = ref 0. in
                 for i=0 to num_points-1 do
                   cumulative := points_array.(i) +. !cumulative;
                   Unixext.really_write_string fd (Printf.sprintf "%d %f %f\n" (i+1) !cumulative points_array.(i))
                 done
              )
         ) all;

       (* Plot a line for (a) elapsed time and (b) this particular duration *)
       let ls = List.flatten (List.mapi
                                (fun i ((info,floats), output) ->
                                   let graph_one_label = Printf.sprintf "Cumulative time, SR %d (left axis)" (i+1) in
                                   let graph_two_label = Printf.sprintf "Time per VM, SR %d (right axis)" (i+1) in
                                   [{ Gnuplot.filename = output; title = graph_one_label; graphname = get_result info; field=2; yaxis=1; scale=1./.3600.; style="lines" };
                                    { Gnuplot.filename = output; title = graph_two_label; graphname = get_result info; field=3; yaxis=2; scale=1.; style="lines" }]
                                ) all) in
       List.iter (fun result ->
           let g = { Gnuplot.xlabel = Printf.sprintf "Number of %s" (string_of_result result);
                     ylabel = "Elapsed time (h)";
                     y2label = Some "Duration (s)";
                     lines = List.filter (fun l -> l.Gnuplot.graphname = result) ls;
                     log_x_axis = false;
                     xrange = Some(0., float_of_int !max_readings);
                     normal_probability_y_axis = None;
                   } in
           let output = match !format with
             | `Eps -> Gnuplot.Ps (Printf.sprintf "%s-%s.eps" !graphic_filename  result)
             | `Gif -> Gnuplot.Gif (Printf.sprintf "%s-%s.gif" !graphic_filename result)
             | `X11 -> Gnuplot.X11 in
           ignore (Gnuplot.render g output)
         ) (get_result_types inputs)
    ) (fun () -> List.iter (fun f -> Xapi_stdext_unix.Unixext.unlink_safe f) output_files)

