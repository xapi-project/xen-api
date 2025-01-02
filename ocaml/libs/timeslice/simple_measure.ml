(*
 * Copyright (C) Cloud Software Group
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

(** 95% confidence interval, and median value *)
type t = {low: float; median: float; high: float}

let span_to_s s = Mtime.Span.to_float_ns s *. 1e-9

let ci95 measurements =
  let n = Array.length measurements in
  Array.sort Float.compare measurements ;
  let median = measurements.(n / 2) in
  (* "Performance Evaluation of Computer and Communication Systems", Table A. 1 *)
  let n = float n in
  let d = 0.98 *. sqrt n in
  let lo = (n /. 2.) -. d |> Float.to_int
  and hi = (n /. 2.) +. 1. +. d |> Float.ceil |> Float.to_int in
  {low= measurements.(lo - 1); median; high= measurements.(hi - 1)}

let measure ?(n = 1001) ?(inner = 10) f =
  if n <= 70 then (* some of the formulas below are not valid for smaller [n] *)
    invalid_arg (Printf.sprintf "n must be at least 70: %d" n) ;
  (* warmup *)
  Sys.opaque_identity (f ()) ;

  let measure_inner _ =
    let m = Mtime_clock.counter () in
    for _ = 1 to inner do
      (* opaque_identity prevents the call from being optimized away *)
      Sys.opaque_identity (f ())
    done ;
    let elapsed = Mtime_clock.count m in
    span_to_s elapsed /. float inner
  in
  let measurements = Array.init n measure_inner in
  ci95 measurements

let measure_min ?(n = 1001) f arg =
  (* warmup *)
  Sys.opaque_identity (f arg) ;
  let measure_one _ =
    let m = Mtime_clock.counter () in
    Sys.opaque_identity (f arg) ;
    let elapsed = Mtime_clock.count m in
    span_to_s elapsed
  in
  Seq.ints 0
  |> Seq.take n
  |> Seq.map measure_one
  |> Seq.fold_left Float.min Float.max_float
