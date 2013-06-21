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

open Xenops_utils

module D=Debug.Make(struct let name="stats" end)
open D

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

(*****************************)
(* Database stats            *)

type dbcallty = Read | Write | Create | Drop

let dbstats_m = Mutex.create ()
let dbstats_read_dbcalls : (string,int) Hashtbl.t = Hashtbl.create 100
let dbstats_write_dbcalls : (string,int) Hashtbl.t = Hashtbl.create 100
let dbstats_create_dbcalls : (string,int) Hashtbl.t = Hashtbl.create 100
let dbstats_drop_dbcalls : (string,int) Hashtbl.t = Hashtbl.create 100
let dbstats_task : (string,(string * dbcallty) list) Hashtbl.t = Hashtbl.create 100
(* let dbstats_taskthreads : (string, int list) Hashtbl.t = Hashtbl.create 100*)
let dbstats_threads : (int, (string * dbcallty) list) Hashtbl.t = Hashtbl.create 100

let log_stats = ref false

let log_db_call task_opt dbcall ty = 
  if not !log_stats then () else
    Mutex.execute dbstats_m (fun () ->
      let hashtbl = match ty with 
	| Read -> dbstats_read_dbcalls 
	| Write -> dbstats_write_dbcalls
	| Create -> dbstats_create_dbcalls
	| Drop -> dbstats_drop_dbcalls
      in
      Hashtbl.replace hashtbl dbcall (1 + (try Hashtbl.find hashtbl dbcall with _ -> 0));
      let threadid = Thread.id (Thread.self ()) in
      Hashtbl.replace dbstats_threads threadid ((dbcall,ty)::(try Hashtbl.find dbstats_threads threadid with _ -> []));
      match task_opt with 
	|	Some task ->
		  Hashtbl.replace dbstats_task task ((dbcall,ty)::(try Hashtbl.find dbstats_task task with _ -> []))
	| None -> ()

	    
    )
      
let summarise_db_calls () =
  let string_of_ty = function | Read -> "read" | Write -> "write" | Create -> "create" | Drop -> "drop" in
  let summarise_table hashtbl =
    let counts = Hashtbl.fold (fun k v acc -> (v,k)::acc) hashtbl [] in
    let sorted = List.sort (fun (a,_) (b,_) -> compare b a) counts in
    let total = List.fold_left (fun acc (a,_) -> acc + a) 0 counts in
    (Printf.sprintf "Total: %d" total) :: (List.map (fun (count,str) -> Printf.sprintf "%s: %d" str count) sorted)
  in

  Mutex.execute dbstats_m (fun () ->
    (summarise_table dbstats_write_dbcalls,
    summarise_table dbstats_read_dbcalls,
    summarise_table dbstats_create_dbcalls,
    summarise_table dbstats_drop_dbcalls,
    Hashtbl.fold (fun k v acc -> (k,List.map (fun (dbcall,ty) -> (string_of_ty ty,dbcall)) (List.rev v))::acc) dbstats_task [],
    List.sort (fun (a,_) (b,_) -> compare a b) (Hashtbl.fold (fun k v acc -> (k,List.map (fun (dbcall,ty) -> (string_of_ty ty,dbcall)) (List.rev v))::acc) dbstats_threads [])))



    
      


