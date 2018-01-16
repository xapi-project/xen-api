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
(** Module to drive gnuplot *)

open Xapi_stdext_pervasives.Pervasiveext

type line = {
  graphname: string;
  filename: string;
  title: string;
  field: int;
  yaxis: int; (* 1 -> left axis; 2 -> right axis *)
  scale: float; (* multiply the values by this factor *)
  style: string; (* 'linespoints', 'lines', etc *)
}

type t = {
  xlabel: string;
  ylabel: string;
  y2label: string option;
  log_x_axis: bool;
  xrange: (float * float) option;
  normal_probability_y_axis: (float * float) option;
  lines: line list;
}

type output =
  | Ps of string
  | Gif of string
  | X11

let make_normal_probability_tics tics =
  Printf.sprintf "set ytics (%s)"
    (String.concat ", " (List.map (fun tic -> Printf.sprintf "\"%.2f\" invnorm(%f)" tic tic) tics))

let make_log_tics tics =
  Printf.sprintf "set xtics (%s)"
    (String.concat ", " (List.map (fun tic -> Printf.sprintf "\"%.2f\" %f" tic tic) tics))

let invnorm (x: t) (y: string) =
  if x.normal_probability_y_axis = None
  then y
  else Printf.sprintf "invnorm(%s)" y

let render (x: t) output =
  let line (y: line) =
    let field = if x.normal_probability_y_axis = None
      then Printf.sprintf "($%d*%f)" y.field y.scale
      else Printf.sprintf "(invnorm($%d*%f))" y.field y.scale in
    Printf.sprintf "\"%s\" using 1:%s axis x1y%d title \"%s\" with %s" y.filename field y.yaxis y.title y.style in
  let config = [
    Printf.sprintf "set terminal %s"
      (match output with
       | Ps _ -> "postscript eps enhanced color"
       | Gif _ -> "gif"
       | X11 -> "wxt 0");
    Printf.sprintf "set output %s"
      (match output with
       | Ps filename -> "\"" ^ filename ^ "\""
       | Gif filename -> "\"" ^ filename ^ "\""
       | X11 -> ""
      );
    Printf.sprintf "set xlabel \"%s\"" x.xlabel;
    Printf.sprintf "set ylabel \"%s\"" x.ylabel;
  ] @ (match x.y2label with
      | None -> []
      | Some label -> [
          Printf.sprintf "set y2label \"%s\"" label;
          "set ytics nomirror";
          "set y2tics auto";
          "set y2range [0:]";
        ])
    @ (match x.normal_probability_y_axis with
        | Some (min, max) ->
          [ make_normal_probability_tics [ 0.001; 0.01; 0.05; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 0.95; 0.99; 0.999 ];
            Printf.sprintf "set yrange [invnorm(%f):invnorm(%f)]" min max ]
        | None -> [])
    @ (match x.log_x_axis with
        | true ->
          [ "set logscale x";
            "set grid";
            "set xtics (\"1\" 1, \"2\" 2, \"3\" 3, \"4\" 4, \"5\" 5, \"6\" 6, \"7\" 7, \"8\" 8, \"9\" 9, \"10\" 10, \"11\" 11, \"12\" 12, \"13\" 13, \"14\" 14, \"15\" 15, \"20\" 20, \"30\" 30)" ];
        | false ->
          []
      )
    @ [
      (if x.log_x_axis then "set logscale x" else "");
      (match x.xrange with
       | None -> "set xrange [*:*]"
       | Some(min, max) -> Printf.sprintf "set xrange [%f:%f]" min max);
      Printf.sprintf "plot %s" (String.concat ", " (List.map line x.lines))
    ] in

  let f = Filename.temp_file "gnuplot" "gnuplot" in
  let open Xapi_stdext_unix in
  Unixext.write_string_to_file f (String.concat "\n" config);
  finally
    (fun () ->
       Unix.system(Printf.sprintf "gnuplot %s %s" (if output = X11 then "-persist" else "") f);
    ) (fun () -> Unixext.unlink_safe f)
