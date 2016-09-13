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
(* Unit tests and performance tests for the binpacking module *)

open Binpack

let time f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let time = Unix.gettimeofday () -. start in
  Printf.printf "result: %Ld time taken: %.2f\n" result time

(* Return a table of hosts or VMs *)
let make_thing base extra n = List.map (fun x -> x, Int64.add base (Random.int64 extra)) (mkints n)

let choose_one list = List.nth list (Random.int (List.length list))

let make_config host_num host_base host_extra vm_num vm_base vm_extra num_failures =
  let hosts = make_thing host_base host_extra host_num in
  let vms = make_thing vm_base vm_extra vm_num in
  let placement = List.map (fun (vm, _) -> vm, fst (choose_one hosts)) vms in
  let config = { hosts = hosts; vms = vms; placement = placement; total_hosts = host_num; num_failures = num_failures } in
  check_configuration config;
  config

(* Return true if the plan looks good ie
   1. no host is overcommitted (free memory after new VMs are subtracted >= 0)
   2. every VM running on the dead hosts is mentioned in the plan *)
(* Return true if the hosts have enough free memory to run the VMs in the plan *)
let check_plan config dead_hosts plan =
  let memory_remaining = account config.hosts config.vms plan in
  (*    List.iter (fun mem -> Printf.printf "%Ld\n" mem) free; *)
  (* No host should be overcommitted: *)
  if List.fold_left (||) false (List.map (fun x -> x < 0L) (List.map snd memory_remaining)) then failwith "bad plan";
  (* All failed VMs should be restarted: *)
  let failed_vms = get_failed_vms config dead_hosts in
  if List.length failed_vms > (List.length plan) then failwith "bad plan"

(* Convince ourselves that a plan is always possible (call if plan_always_possible returns true)
   by searching for a counterexample.
   Returns true -- definitely OK (exhaustive search failed to find any bad plans)
   Returns false -- maybe OK (too many for exhaustive search, didn't find any bad plans)
   Throws (Failure "bad plan") -- definitely bad
*)
let prove_plan_is_possible_via_counterexample_search (h: (int, int) Binpack.heuristic) config =
  (* If a small number of combinations then try each one. Otherwise try a bunch at random *)
  let limit = 10000L in
  let num_hosts = List.length config.hosts in
  let total_combinations = binomial num_hosts config.num_failures in
  let combinations_to_try, exhaustive =
    if total_combinations < limit
    then choose (List.map fst config.hosts) config.num_failures, true
    else List.map (fun _ ->
        let num_failures = Random.int config.num_failures in
        (* choose 'num_failures' elements at random *)
        let alive, dead = List.fold_left
            (fun (remaining, sofar) _ ->
               if List.length sofar = num_failures
               then (remaining, sofar)
               else begin
                 let host = choose_one remaining in
                 List.filter (fun x -> x <> host) remaining, host :: sofar
               end)
            (List.map fst config.hosts, []) (mkints num_failures) in
        dead) (mkints (Int64.to_int limit)), false in
  Printf.printf "Trying %d (out of %Ld) combinations %s\n" (List.length combinations_to_try) total_combinations (if exhaustive then "(EXHAUSTIVE)" else "");
  List.iter
    (fun dead_hosts ->
       let failed_vms = get_failed_vms config dead_hosts in
       let config = { config with hosts = List.filter (fun (x, _) -> not(List.mem x dead_hosts)) config.hosts } in
(*
       Printf.printf "Config = %s\n" (string_of_configuration string_of_int string_of_int config);
       Printf.printf "  Dead hosts = [ %s ]; failed VMs = [ %s ]\n" (String.concat ";" (List.map string_of_int dead_hosts)) (String.concat ";" (List.map string_of_int failed_vms));
*)
       let plan = h.get_specific_plan config failed_vms in
(*
       Printf.printf "  Plan = [ %s ]\n" (String.concat "; " (List.map (fun (a, b) -> Printf.sprintf "%d -> %d" a b) plan));
*)
       check_plan config dead_hosts plan) combinations_to_try;
  (* If search was exhaustive then we are sure. Otherwise it's "maybe" *)
  exhaustive

(* Negative tests -- make sure the planner fails in obviously impossible situations *)
let try_impossible_cases () =
  Printf.printf "Trying impossible cases\n";

  (* Make sure an obviously bad plan is detected by the 'check_plan' fn *)
  Printf.printf "Making sure an obviously bad plan is detected by the 'check_plan' fn: ";
  let hosts      = [ 0, 1L; 1, 1L ]   (* two hosts, 1 unit free each *)
  and vms        = [ 0, 1L; 1, 1L ]   (* two VMs, 1 unit required each *)
  and placement  = [ 0, 1; 1, 1 ]     (* both running on host 1 *)
  and dead_hosts = [ 1 ]              (* host 1 fails *)
  and bad_plan   = [ 0, 0; 0, 0 ] in  (* plan is to restart both VMs on host 0 *)
  begin
    let config = { hosts = hosts; vms = vms; placement = placement; total_hosts = List.length hosts; num_failures = 1 } in
    try
      check_plan config dead_hosts bad_plan;
      failwith "bad plan was not detected"
    with (Failure "bad plan") -> ()
  end;
  Printf.printf "OK\n";

  (* Hosts all have 500L + a few Mb; 1 400L VM per host; >n/2 failures *)
  Printf.printf "Checking a more complicated configuration for which no failover plan should exist\n";
  let hosts = make_thing 500L 1L 8 in
  let vms = make_thing 400L 1L 8 in
  let placement = List.combine (List.map fst vms) (List.map fst hosts) in
  let config = { hosts = hosts; vms = vms; placement = placement; total_hosts = List.length hosts; num_failures = 5 } in
  List.iter (fun h ->
      Printf.printf "Trying heuristic: %s\n" h.name;
      Printf.printf "* checking plan_always_possible = false\n";
      if h.plan_always_possible config then failwith "plan_always_possible shouldn't return true";
      try
        Printf.printf "* checking 'check_plan_always_possible' agrees\n";
        if prove_plan_is_possible_via_counterexample_search h config
        then failwith "prove_plan_is_possible_via_counterexample_search performed exhaustive search and found no counterexample"
        else Printf.printf "WARNING: failed to find a counterexample; not sure if plan is ok or not\n"
      with Failure "bad plan" -> Printf.printf "Found a counterexample: no plan is possible\n") all_heuristics

(* Positive test -- make sure the planner succeeds in easy cases *)
let try_possible_cases () =
  Printf.printf "Trying possible cases\n";
  let c = make_config 10 500L 1000L 5 256L 1L 3 in
  let h = choose_heuristic c in
  Printf.printf "Trying heuristic: %s\n" h.name;
  let always = h.plan_always_possible c in
  if not always then failwith "heuristic reported plan not always possible";
  if prove_plan_is_possible_via_counterexample_search h c
  then Printf.printf "Proved that plan is always possible\n"
  else Printf.printf "Failed to prove that plan is always possible -- might be ok still\n"

let int_of_heuristic h = if h.name = approximate_bin_pack.name then 0 else 1

let check_planning_performance filename n' r' i =
  let file = open_out filename in
  (*    Printf.printf "Checking performance of planner\n"; *)
  let successes = Array.make (n' * r') 0 in
  let max_time = Array.make (n' * r') 0. in
  let heuristic = Array.make (n' * r') 0 in

  let get array n r = array.(r' * (n-1) + (r-1)) in
  let set array n r value = array.(r' * (n-1) + (r-1)) <- value in
  for attempts = 1 to i do
    for n = 1 to n' do
      for r = 1 to r' do
        if r < n then begin
          let c = make_config n 8000L 4000L (16 * n) 500L 250L r in
          let h = choose_heuristic c in
          let start = Unix.gettimeofday () in
          let always = h.plan_always_possible c in
          (* If it should always be possible then look for a proof. Don't fail if we can't find one; only fail if we find
             	     a counterexample showing it doesn't work *)
          if always then ignore(prove_plan_is_possible_via_counterexample_search h c);
          let time = Unix.gettimeofday () -. start in
          if always then set successes n r (get successes n r + 1);
          set max_time n r (max (get max_time n r) time);
          (* Assumes heuristic choice is a function of n and r only *)
          set heuristic n r (int_of_heuristic h);

          Printf.fprintf stderr "%d %d %d %d %.2f\n" n r (get heuristic n r) (get successes n r) (get max_time n r); flush stderr;

        end
      done
    done
  done;
  for n = 1 to n' do
    for r = 1 to r' do
      Printf.fprintf file "%d %d %d %d %.2f\n" n r (get heuristic n r) (get successes n r) (get max_time n r)
    done
  done;
  close_out file

let _ =
  let graph = ref "" in
  let graph_n = ref 64 and graph_r = ref 64 and graph_i = ref 1 in

  Arg.parse [ "-graph", Arg.Set_string graph, "Run performance tests and write graph output to file specified";
              "-graph_n", Arg.Set_int graph_n, "Set the maximum N value for the performance tests (eg total hosts)";
              "-graph_r", Arg.Set_int graph_r, "Set the maximum R value for the performance tests (eg host failures to simulate)";
              "-graph_i", Arg.Set_int graph_i, "Set the number of iterations to run the performance tests over" ]
    (fun x -> Printf.fprintf stderr "Skipping unknown argument: %s" x)
    "Run unit and optional performance tests on the binpacker";


  try_impossible_cases ();
  try_possible_cases ();
  if !graph <> "" then check_planning_performance !graph !graph_n !graph_r !graph_i

