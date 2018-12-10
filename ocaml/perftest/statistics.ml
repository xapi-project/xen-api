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
(** Useful stats-related functions for plotting graphs and analysing the results of perftest *)

let pi = atan 1. *. 4.

let gaussian mu sigma x = 1.0 /. (sigma *. sqrt (2.0 *. pi)) *. exp (-.(((x -. mu) ) ** 2.0 ) /. (2.0 *. sigma *. sigma))

module Hist = struct
  type t = {
    bin_start: float array;
    bin_end: float array;
    bin_count: float array; (* height of each bin: multiply by width to get area *)
  }

  (** Initialise a histogram covering values from [min:max] in 'n' uniform steps *)
  let make (min: float) (max: float) (n: int) =
    let range = max -. min in
    { bin_start = Array.init n (fun i -> range /. (float_of_int n) *. (float_of_int i) +. min);
      bin_end = Array.init n (fun i ->  range /. (float_of_int n) *. (float_of_int (i + 1)) +. min);
      bin_count = Array.init n (fun _ -> 0.);
    }

  let integrate (x: t) =
    let n = Array.length x.bin_start in
    let result = make x.bin_start.(0) x.bin_end.(Array.length x.bin_end - 1) n in
    let area = ref 0. in
    for i = 0 to Array.length x.bin_start - 1 do
      assert (x.bin_start.(i) = result.bin_start.(i));
      let width = x.bin_end.(i) -. x.bin_start.(i) in
      area := !area +. x.bin_count.(i) *. width;
      result.bin_count.(i) <- !area;
    done;
    result

  (** Call 'f' with the start, end and height of each bin *)
  let iter (x: t) (f: float -> float -> float -> unit) =
    for i = 0 to Array.length x.bin_start - 1 do
      let width = x.bin_end.(i) -. x.bin_start.(i) in
      f x.bin_start.(i) x.bin_end.(i) (x.bin_count.(i) /. width)
    done

  (** Fold 'f' over the bins calling it with 'bin_start' 'bin_end' 'height' and 'acc' *)
  let fold (x: t) (f: float -> float -> float -> 'a -> 'a) (init: 'a)  =
    let acc = ref init in
    iter x (fun bin_start bin_end height -> acc := f bin_start bin_end height !acc);
    !acc

  (** Write output to a file descriptor in gnuplot format *)
  let to_gnuplot (x: t) (fd: Unix.file_descr) =
    iter x (fun bin_start bin_end height ->
        let center = (bin_start +. bin_end) /. 2.0 in
        let line = Printf.sprintf "%f %f\n" center height |> Bytes.of_string in
        let (_: int) = Unix.write fd line 0 (Bytes.length line) in ()
      )

  exception Stop

  (** Add a sample point *)
  let add (x: t) (y: float) =
    try
      for i = 0 to Array.length x.bin_start - 1 do
        if x.bin_start.(i) <= y && (y <= x.bin_end.(i + 1)) then begin
          x.bin_count.(i) <- x.bin_count.(i) +. 1.0;
          raise Stop
        end
      done
    with Stop -> ()

  (** Evaluate 'f' given the center of each bin and add the result to the bin count *)
  let convolve (x: t) (f: float -> float) =
    for i = 0 to Array.length x.bin_start - 1 do
      let center = (x.bin_start.(i) +. x.bin_end.(i)) /. 2.0 in
      let width = x.bin_end.(i) -. x.bin_start.(i) in
      let result = f center in
      x.bin_count.(i) <- x.bin_count.(i) +. result *. width
    done

  (** Given a monotonically increasing histogram find the 'x' value given a 'y' *)
  let find_x (x: t) (y: float) =
    match fold x (fun bin_start bin_end height acc -> match acc with
        | Some x -> acc (* got it already *)
        | None -> if height > y then Some ((bin_start +. bin_end) /. 2.) (* no interpolation *) else None
      ) None with
    | Some x -> x
    | None -> raise Not_found
end


module Normal = struct
  let mean (points: float list) = List.fold_left (+.) 0. points /. (float_of_int (List.length points))
  let sigma (points: float list) =
    let sum_x = List.fold_left (+.) 0. points
    and sum_xx = List.fold_left (+.) 0. (List.map (fun x -> x *. x) points) in
    let n = float_of_int (List.length points) in
    sqrt (n *. sum_xx -. sum_x *. sum_x) /. n
end

module LogNormal = struct
  let mean (points: float list) =
    let points = List.map log points in
    let normal_sigma = Normal.sigma points in
    let normal_mean = Normal.mean points in
    exp (normal_mean +. normal_sigma *. normal_sigma /. 2.)
  let sigma (points: float list) =
    let points = List.map log points in
    let normal_sigma = Normal.sigma points in
    let normal_mean = Normal.mean points in
    let v = (exp(normal_sigma *. normal_sigma) -. 1.) *. (exp (2. *. normal_mean +. normal_sigma *. normal_sigma)) in
    sqrt v
end
