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
open Statistics
open Graphutil

let _ =
  let sigma = ref 0.1 in
  let inputs = ref [] in
  let format = ref `X11 in
  let graphic_filename = ref "" in
  let integrate = ref false in
  let normal = ref false in
  let log_axis = ref false in
  let min_percentile = ref 1. in
  let max_percentile = ref 95. in
  Arg.parse [
    "-format", Arg.Symbol ([ "eps"; "gif"; "x11" ],
                           (function
                             | "eps" -> format := `Eps
                             | "gif" -> format := `Gif
                             | "x11" -> format := `X11
                             | _ -> failwith "huh ?")),
    " Set output format (default: X11)";
    "-output", Arg.Set_string graphic_filename,                    " Set default output file (for non-X11 modes)";
    "-sigma", Arg.Set_float sigma, Printf.sprintf                  " Set sigma for the gaussian (default %f)" !sigma;
    "-integrate", Arg.Set integrate, Printf.sprintf                " Integrate the probability density function (default: %b)" !integrate;
    "-normal", Arg.Set normal, Printf.sprintf                      " Use a 'normal probability axis' (default: %b)" !normal;
    "-log", Arg.Set log_axis, Printf.sprintf                       " Use a log x axis (default: %b)" !log_axis;
    "-minpercentile", Arg.Set_float min_percentile, Printf.sprintf " Minimum percentile to plot (default: %.2f)" !min_percentile;
    "-maxpercentile", Arg.Set_float max_percentile, Printf.sprintf " Maximum percentile to plot (default: %.2f)" !max_percentile;
  ]
    (fun x -> inputs := x :: !inputs)
    "Generate a histogram by convolving sample points with a gaussian.\nusage:";
  if !inputs = [] then failwith "Needs at least one input filename";
  if !format <> `X11 && !graphic_filename = "" then failwith "This format needs an -output";
  let sigma = !sigma in
  let inputs = get_info !inputs in

  let output_files = List.map (fun _ -> Filename.temp_file "histogram" "dat") inputs in
  let all = List.combine inputs output_files in

  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
       (* Write some summary statistics on stderr *)
       List.iter
         (fun (info, points) ->
            debug ~out:stderr "%s has lognormal mean %f +/- %f" (short_info_to_string info) (LogNormal.mean points) (LogNormal.sigma points);
         ) inputs;

       let min_point = get_min inputs in
       let max_point = get_max inputs in

       (* To make sure that each added gaussian really adds 1 unit of area, we extend the bins
          	  3 sigmas to the left and right *)
       let min_point = List.map (fun (r,n) -> r, n -. 3. *. sigma) min_point
       and max_point = List.map (fun (r,n) -> r, n +. 3. *. sigma) max_point in

       (* Attempt to zoom the graph in on the 10% to 90% region *)
       let xrange_min = ref max_point
       and xrange_max = ref min_point in

       List.iter
         (fun ((info,points), output_file) ->
            let result = get_result info in
            let x = Hist.make (List.assoc result min_point) (List.assoc result max_point) 1000 in

            (* -- Apply the Weierstrass transform -- *)

            (* NB Each call to Hist.convolve (i.e. each VM timing measured) increases the total area under the curve by 1.
               	       By dividing through by 'n' (where 'n' is the total number of VMs i.e. points) we make the total area under
               	       the curve equal 1 so we can consider the result as a probability density function. In particular this means
               	       we can directly compare curves for 10, 100, 1000 measurements without worrying about scale factors and
               	       also trade speed for estimation accuracy. *)
            let num_points = float_of_int (List.length points) in

            List.iter (fun y -> Hist.convolve x (fun z -> (gaussian y sigma z) /. num_points)) points;
            (* Sanity-check: area under histogram should be almost 1.0 *)
            let total_area = Hist.fold x (fun bin_start bin_end height acc -> (bin_end -. bin_start) *. height +. acc) 0. in
            if abs_float (1. -. total_area) > 0.01
            then debug ~out:stderr "WARNING: area under histogram should be 1.0 but is %f" total_area;

            let cumulative = Hist.integrate x in
            let t_10 = Hist.find_x cumulative 0.1 in
            let t_80 = Hist.find_x cumulative 0.8 in
            let t_90 = Hist.find_x cumulative 0.9 in
            let t_95 = Hist.find_x cumulative 0.95 in
            debug ~out:stderr "10th percentile: %f" t_10;
            debug ~out:stderr "80th percentile: %f" t_80;
            debug ~out:stderr "90th percentile: %f" t_90;
            debug ~out:stderr "95th percentile: %f" t_95;
            debug ~out:stderr "Clipping data between %.0f and %.0f percentiles" !min_percentile !max_percentile;
            xrange_min := replace_assoc result (min (List.assoc result !xrange_min) (Hist.find_x cumulative (!min_percentile /. 100.))) !xrange_min;
            xrange_max := replace_assoc result (max (List.assoc result !xrange_max) (Hist.find_x cumulative (!max_percentile /. 100.))) !xrange_max;

            let x = if !integrate then Hist.integrate x else x in
            Xapi_stdext_unix.Unixext.with_file output_file [ Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT ] 0o644 (Hist.to_gnuplot x)
         ) all;

       let ls = List.map (fun ((info,floats), output) -> { Gnuplot.filename = output; title = short_info_to_title info; graphname = get_result info; field = 2; yaxis=1; scale=1.; style="linespoints" }) all in
       let ylabel =
         if !integrate
         then "Cumulative probability"
         else "Estimate of the probability density function" in
       List.iter (fun result ->
           let g = { Gnuplot.xlabel = Printf.sprintf "Time for %s XenAPI calls to complete / seconds" (string_of_result result);
                     ylabel = ylabel;
                     y2label = None;
                     lines = List.filter (fun l -> l.Gnuplot.graphname = result) ls;
                     log_x_axis = !log_axis;
                     xrange = Some(List.assoc result !xrange_min, List.assoc result !xrange_max);
                     normal_probability_y_axis = if !normal then Some(!min_percentile /. 100., !max_percentile /. 100.) else None;
                   } in
           let output = match !format with
             | `Eps -> Gnuplot.Ps (Printf.sprintf "%s-%s.eps" !graphic_filename  result)
             | `Gif -> Gnuplot.Gif (Printf.sprintf "%s-%s.gif" !graphic_filename result)
             | `X11 -> Gnuplot.X11 in
           ignore (Gnuplot.render g output)
         ) (get_result_types inputs)
    ) (fun () -> List.iter Xapi_stdext_unix.Unixext.unlink_safe output_files)

