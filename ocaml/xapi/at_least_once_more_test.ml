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
(* Test the 'at_least_once_more' function by constantly changing the input arguments to a simple arithmetic
   operation and expecting that, when the input changes stop, the output of the function is eventually correct. *)

open Threadext

type locked_int = {
  mutable x: int64;
  m: Mutex.t;
}
let make init = {
  x = init;
  m = Mutex.create ()
}
let get (x: locked_int) = Mutex.execute x.m (fun () -> x.x)
let set (x: locked_int) newval = Mutex.execute x.m (fun () -> x.x <- newval)

let inputs = [ make 0L; make 0L; make 0L ]
let total = make 0L

let keep_changing_inputs = ref true
let keep_changing_inputs_m = Mutex.create ()

let ( +* ) = Int64.add
let num_invocations = make 0L
let update_total () =
  set num_invocations (get num_invocations +* 1L);
  let inputs' = List.map get inputs in
  set total (List.fold_left Int64.add 0L inputs')

let need_to_recompute_total = At_least_once_more.make "recompute total" update_total

let background_thread_changing_input input =
  while (Mutex.execute keep_changing_inputs_m (fun () -> !keep_changing_inputs)) do
    set input (get input +* 1L);
    At_least_once_more.again need_to_recompute_total;
  done

let _ =
  (* Start background threads *)
  let threads = List.map (Thread.create background_thread_changing_input) inputs in
  (* Wait for a while *)
  Thread.delay 30.;
  (* Signal threads to stop *)
  Mutex.execute keep_changing_inputs_m (fun () -> keep_changing_inputs := false);
  List.iter Thread.join threads;
  (* Wait for the total to settle *)
  let total_should_be = List.fold_left Int64.add 0L (List.map get inputs) in
  let now = Unix.gettimeofday () in
  while Unix.gettimeofday () -. now < 5. && (get total) <> total_should_be do Thread.delay 1. done;
  let total_is = get total in
  Printf.printf "total_should_be = %Ld; total_is = %Ld\n" total_should_be total_is;
  let num_invocations = get num_invocations in
  Printf.printf "total was recomputed %Ld times (%.2f%% of potential total)\n" num_invocations (Int64.to_float num_invocations /. (Int64.to_float total_should_be) *. 100.);
  exit (if total_should_be = total_is then 0 else 1)
