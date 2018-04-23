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
(** Time activities, monitor the mean and standard deviation. Try to help understand how
    long key operations take under load. *)
open Stdext

module Normal_population = struct
  (** Stats on a normally-distributed population *)
  type t = { sigma_x: float;
             sigma_xx: float;
             n: int }

  let empty = { sigma_x = 0.; sigma_xx = 0.; n = 0 }

  let sample (p: t) (x: float) : t =
    { sigma_x = p.sigma_x +. x;
      sigma_xx = p.sigma_xx +. x *. x;
      n = p.n + 1 }

  exception Unknown

  let mean (p: t) : float = p.sigma_x /. (float_of_int p.n)
  let sd (p: t) : float =
    if p.n = 0
    then raise Unknown
    else
      let n = float_of_int p.n in
      sqrt (n *. p.sigma_xx -. p.sigma_x *. p.sigma_x) /. n
end

(* We keep a finite global table of timing statistics, each named by a string.
   The total number of names should be kept finite to avoid space leaks; the
   number should never be proportional to the number of VMs, VIFs etc!

   Since these are used for timing data, which is better approximated by a
   lognormal distribution than a normal one, we take care to apply the
   lognormal transformations here.
*)

module D=Debug.Make(struct let name="stats" end)
open D
open Threadext
open Pervasiveext

let timings : (string, Normal_population.t) Hashtbl.t = Hashtbl.create 10
let timings_m = Mutex.create ()

let mean (p: Normal_population.t) =
  let sigma = Normal_population.sd p in
  let mu = Normal_population.mean p in
  exp (mu +. sigma *. sigma /. 2.)

let sd (p: Normal_population.t) =
  let sigma = Normal_population.sd p in
  let mu = Normal_population.mean p in
  let v = (exp(sigma *. sigma) -. 1.) *. (exp (2. *. mu +. sigma *. sigma)) in
  sqrt v

let string_of (p: Normal_population.t) =
  Printf.sprintf "%f [sd = %f]" (mean p) (sd p)

(** [sample thing t] records new time [t] for population named [thing] *)
let sample (name: string) (x: float) : unit =
  (* Use the lognormal distribution: *)
  let x' = log x in
  Mutex.execute timings_m
    (fun () ->
       let p =
         if Hashtbl.mem timings name
         then Hashtbl.find timings name
         else Normal_population.empty in
       let p' = Normal_population.sample p x' in
       Hashtbl.replace timings name p';
       (*       debug "Population %s time = %f mean = %s" name x (string_of p'); *)
    )
(*
  (* Check to see if the value is > 3 standard deviations from the mean *)
  if abs_float (x -. (mean p)) > (sd p *. 3.)
  then debug "Population %s time more than 3 standard deviations from the mean (time = %f; mean = %s)" name x (string_of p)
*)

(** Helper function to time a specific thing *)
let time_this (name: string) f =
  let start_time = Unix.gettimeofday () in
  finally f
    (fun () ->
       try
         let end_time = Unix.gettimeofday () in
         sample name (end_time -. start_time)
       with e ->
         warn "Ignoring exception %s while timing: %s" (Printexc.to_string e) name
    )

let summarise () =
  Mutex.execute timings_m
    (fun () ->
       Hashtbl.fold (fun k v acc -> (k, string_of v) :: acc) timings []
    )
