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
let avg_fct arr =
  let sum = ref 0. in
  Array.iter (fun v -> sum := !sum +. v) arr;
  !sum /. (float_of_int (Array.length arr))

let min_fct arr =
  let min = ref 10000. in
  Array.iter (fun v -> if !min > v then min := v else ()) arr;
  !min

let max_fct arr =
  let max = ref 0. in
  Array.iter (fun v -> if !max < v then max := v else ()) arr;
  !max

let variance_fct arr =
  let n = ref 0 and mean = ref 0. and s = ref 0. in
  Array.iter (fun x ->
      n := !n + 1;
      let delta = x -. !mean in
      mean := !mean +. delta /. (float_of_int !n);
      s := !s +. delta *. (x -. !mean)) arr;
  !s /. (float_of_int (!n - 1))

let output chan name (avg, min, max, var, o, all) =
  let s = Printf.sprintf "[%s] = avg: %.6f | min: %.6f | max: %.6f | V: %.3f [%d]\n%!"
      name avg min max (var *. 1000000.) o in
  output_string chan s;
  flush chan

let once (f: unit -> unit) =
  let before = Unix.gettimeofday () in
  f ();
  let after = Unix.gettimeofday () in
  after -. before

let multi ?times:(times=100000) (f: unit -> unit): float * float * float * float * int * float array =
  let ring = Ring.make times 0. in
  for i = 0 to times
  do
    try
      let x = once f in
      Ring.push ring x
    with exn ->
      Printf.printf "bench exception: %s\n%!" (Printexc.to_string exn)
  done;

  let arr = Ring.get ring in

  let avg = avg_fct arr
  and min = min_fct arr
  and max = max_fct arr
  and var = variance_fct arr in

  avg, min, max, var, 0, arr

let multithread ?times:(times=100000) testname fcts =
  let len = Array.length fcts in
  let start = ref false in
  let stopped = ref 0 in

  let avgs = Array.make len 0.
  and maxs = Array.make len 0.
  and mins = Array.make len 0.
  and vars = Array.make len 0.
  and alls = Array.make len 0.
  and others = Array.make len 0 in

  ignore alls;

  let start_on i f =
    while not !start
    do
      Thread.yield ()
    done;
    let (avg, min, max, var, r, all) = f () in
    avgs.(i) <- avg;
    mins.(i) <- min;
    maxs.(i) <- max;
    vars.(i) <- var;
    others.(i) <- r;

    incr stopped
  in

  Array.iteri (fun i f ->
      ignore (Thread.create (fun () -> start_on i f) ())) fcts;
  start := true;

  while !stopped < len do Thread.yield () done;

  for i = 0 to len - 1
  do
    output stdout testname (avgs.(i), mins.(i), maxs.(i), vars.(i), others.(i), alls.(i))
  done;
  ()

let multifork ?times:(times=100000) testname fcts =
  let len = Array.length fcts in
  let pids = Array.make len 0 in

  let file = "/tmp/start" in
  Unix.close (Unix.openfile file [ Unix.O_CREAT ] 0o777);

  Array.iteri (fun i f ->
      let pid = Unix.fork () in
      if pid = 0 then (
        try
          while true do
            ignore(Unix.stat file);
            Thread.delay 0.00001
          done
        with _ -> ();
          output stdout testname (f ()) ;
          (exit 0)
      ) else
        pids.(i) <- pid
    ) fcts;
  (* start signal *)
  (try Unix.unlink file with _ -> ());

  let childdead = ref 0 in
  while !childdead < len
  do
    match Unix.waitpid [] (-1) with
    | pid, Unix.WEXITED ret -> incr childdead
    | _                     -> Printf.printf "hum\n%!"
  done;
  (*Printf.printf "out !\n%!";*)
