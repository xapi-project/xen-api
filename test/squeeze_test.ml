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
(**
   	Simulation environment and set of unit tests for the domain memory balancer.
*)

open Squeeze

(**
   	Computes the memory_actual delta for a VM assuming the balloon driver
   	responds at a given speed. Warning: make sure the balloon_rate * time_passed
   	is > 0 when rounded to an integer.
*)
let compute_memory_actual_delta domain rate time =
  let max_change = Int64.of_float (time *. (Int64.to_float rate)) in
  (* compute the direction of travel of the balloon *)
  let vector = if domain.memory_actual_kib > domain.target_kib then -1L else 1L in
  let distance = (domain.target_kib -* domain.memory_actual_kib) ** vector in
  (* We stop when we get to 'inaccuracy_kib' *)
  let distance' = max 0L (distance -* domain.inaccuracy_kib) in
  (* don't exceed the target *)
  let distance'' = min distance' max_change in
  distance'' ** vector

class virtual vm initial_domain = object (self)
  val mutable domain = initial_domain
  val mutable time_of_last_update = 0.

  (** Return the current domain state *)
  method get_domain = domain

  (** Helper function to change the domain's balloon target *)
  method set_target new_target_kib =
    if new_target_kib <> domain.target_kib
    then debug "domid %d: target change was %Ld now %Ld (max %Ld)"
        domain.domid domain.target_kib new_target_kib domain.memory_max_kib;
    if new_target_kib > domain.memory_max_kib
    then failwith (Printf.sprintf "Target set above max_mem domid %d; max_mem = %Ld; target = %Ld" domain.domid domain.memory_max_kib new_target_kib);
    domain <- { domain with target_kib = new_target_kib }

  (** Helper function to set the domain's maxmem *)
  method set_maxmem new_max_kib =
    if domain.target_kib > new_max_kib
    then failwith (Printf.sprintf "mem_max set below target domid %d; max_mem = %Ld; target = %Ld" domain.domid new_max_kib domain.target_kib);
    domain <- { domain with memory_max_kib = new_max_kib }

  (**
     		Given a number of time units since the last call to 'update',
     		compute the expected change in memory_actual. Note that this
     		might be unfulfilled if the host is low on memory.
     	*)
  method virtual compute_memory_actual_delta : float -> int64

  (**
     		Called by the simulator to update memory_actual. It also returns
     		memory_actual so the host free memory total can be updated.
     	*)
  method update host_free_mem time =
    let time_passed = time -. time_of_last_update in
    (* We can release as much memory as we like but *)
    (* we can't allocate more than is available     *)
    let delta = self#compute_memory_actual_delta time_passed in
    (* By construction it should never be possible for a domain to wish to allocate
       	     more memory than exists. *)
    if delta > host_free_mem
    then failwith (Printf.sprintf "Attempted to allocate more than host_free_mem domid = %d; delta = %Ld; free = %Ld" domain.domid delta host_free_mem);
    domain <- { domain with memory_actual_kib = domain.memory_actual_kib +* delta };
    time_of_last_update <- time;
    delta (* if -ve this means host memory increased *)
end

(** Represents a VM whose balloon driver responds at a certain speed *)
class idealised_vm initial_domain balloon_rate_kib_per_unit_time = object
  inherit vm initial_domain

  method compute_memory_actual_delta time_passed =
    compute_memory_actual_delta
      domain balloon_rate_kib_per_unit_time time_passed
end

(**
   	Represents a VM whose balloon driver responds at a certain speed but which
   	has a minimum limit
*)
class idealised_vm_with_limit
    initial_domain balloon_rate_kib_per_unit_time minimum_memory = object
  inherit vm initial_domain

  method compute_memory_actual_delta time_passed =
    let delta = compute_memory_actual_delta
        domain balloon_rate_kib_per_unit_time time_passed in
    let proposed_new_memory_actual = domain.memory_actual_kib +* delta in
    (* If the proposed_new_memory_actual is bigger than our memory actual *)
    (* then this is always ok.                                            *)
    (* If the proposed value is smaller but greater than the minumum then *)
    (* this is always ok too.                                             *)
    (* If the proposed value is smaller and less than the minimum then    *)
    (* this is clipped.                                                   *)
    if proposed_new_memory_actual > domain.memory_actual_kib
    || proposed_new_memory_actual > minimum_memory
    then delta
    else minimum_memory -* domain.memory_actual_kib
    (* this takes us to the minimum *)
end

(** Represents a VM which fails to allocate above a certain threshold *)
class idealised_vm_with_upper_limit initial_domain rate limit = object
  inherit vm initial_domain

  method compute_memory_actual_delta time_passed =
    let delta = compute_memory_actual_delta domain rate time_passed in
    let proposed_new_memory_actual = domain.memory_actual_kib +* delta in
    if proposed_new_memory_actual < limit
    then delta
    else (limit -* domain.memory_actual_kib)
end



(** Represents a VM whose balloon driver has completely failed *)
class stuck_vm initial_domain = object
  inherit vm initial_domain

  method compute_memory_actual_delta _ = 0L
end

(**
   	Represents a VM whose balloon driver moves at a constant rate
   	but gets stuck for 'interval' seconds every 'interval' seconds
*)
class intermittently_stuck_vm
    initial_domain balloon_rate_kib_per_unit_time interval = object
  inherit vm initial_domain

  method compute_memory_actual_delta time_passed =
    (* Every interval we switch from stuck to non-stuck. Assume for *)
    (* simplicity that time_passed < 1 and that our timesteps are   *)
    (* suitably small that we disregard transients.                 *)
    let notstuck t = int_of_float (t /. interval) mod 2 = 0 in
    let useful_time =
      if notstuck time_of_last_update
      && (notstuck (time_of_last_update +. time_passed))
      then time_passed
      else 0.
    in
    debug "useful_time = %.2f (current actual = %Ld; target = %Ld)"
      useful_time domain.memory_actual_kib domain.target_kib;
    compute_memory_actual_delta
      domain balloon_rate_kib_per_unit_time useful_time
end

type scenario = {
  name: string;
  description: string;
  should_succeed: bool;
  scenario_domains: vm list;
  host_free_mem_kib: int64;
  required_mem_kib: int64;
  fistpoints: Squeeze.fistpoint list;
}


let scenario_a = {
  name = "a";
  description = "a small domain with a hidden limit and a large domain which \
                 		exhibits 'sticky' behaviour";
  should_succeed = true;
  scenario_domains = [
    new idealised_vm_with_limit
      (domain_make 0 true 1000L 1500L 2000L 1500L 1500L 4L) 100L 1250L;
    new intermittently_stuck_vm
      (domain_make 1 true 2500L 3500L 4500L 3500L 3500L 4L) 500L 0.25;
  ];
  host_free_mem_kib = 0L;
  required_mem_kib = 1000L;
  fistpoints = [ ];
}

let scenario_b = {
  name = "b";
  description = "two domains exhibiting 'sticky' behaviour with different \
                 		periods: one > than the assumed stuck interval and the other <";
  should_succeed = true;
  scenario_domains = [
    new intermittently_stuck_vm
      (domain_make 1 true 500L 3500L 4500L 3500L 3500L 4L) 100L 3.;
    new intermittently_stuck_vm
      (domain_make 0 true 500L 1500L 2500L 1500L 1500L 4L) 100L 1.5;
  ];
  host_free_mem_kib = 0L;
  required_mem_kib = 1000L;
  fistpoints = [ ];
}

let scenario_c = {
  name = "c";
  description = "dynamic_mins are too high to allow enough memory to be \
                 		freed";
  should_succeed = false;
  scenario_domains = [
    new idealised_vm (domain_make 0 true 1000L 1500L 2000L 1500L 1500L 0L) 100L;
    new idealised_vm (domain_make 1 true 2000L 2500L 3000L 2500L 2500L 0L) 100L;
  ];
  host_free_mem_kib = 0L;
  required_mem_kib = 1500L;
  fistpoints = [ ];
}

let scenario_d = {
  name = "d";
  description = "looks ok but one VM is permanently stuck above its \
                 		dynamic_min";
  should_succeed = false;
  scenario_domains = [
    new idealised_vm
      (domain_make 0 true 1000L 1500L 2000L 1500L 1500L 0L) 100L;
    new idealised_vm_with_limit
      (domain_make 1 true 2000L 2500L 3000L 2500L 2500L 0L) 100L 2250L;
  ];
  host_free_mem_kib = 0L;
  required_mem_kib = 1000L;
  fistpoints = [ ];
}

let scenario_e = {
  name = "e";
  description = "one domain needs to free but is stuck, other domain needs to allocate \
                 		but can't because there is not enough free memory. We need to give up on the stuck \
                 		domain first and then tell the other domain to free rather than allocate. We must not \
                 		conclude the second domain is stuck because it can't allocate.";
  should_succeed = true;
  scenario_domains = [
    (* The stuck domain is using more than it should be if the memory was freed and everything balanced *)
    new stuck_vm
      (domain_make 0 true (*min*)5000L (*target*)7000L (*max*)7000L (*actual*)7000L 7000L 0L);
    (* The working domain is using less than it should be if the memory was freed and everything balanced *)
    new idealised_vm
      (domain_make 1 true (*min*)5000L (*target*)6000L (*max*)11000L (*actual*)6000L 6000L 0L) 100L;

  ];
  host_free_mem_kib = 0L;
  required_mem_kib = 1000L;
  (* The system has 3000L units of surplus memory. Ideally we'd give 1000L to the system and then split
     	   the remaining 2000L units proportionally amongst the domains: 500L to 0 and 1500L to 1. If both
     	   domains were ideal then we could set 0's target to 5500L (down) and 1's target to 6500L (up)
     	   However since the stuck domain is stuck this strategy will fail. In this case we want the idealised
     	   VM to release the 1000L units of memory. However the likely failure mode is that it will have been
     	   asked to increase its allocation and been unable to do so because all host memory is exhausted.
     	   It will then also have been marked as stuck and the operation will fail. *)
  fistpoints = [ ];
}

let scenario_f = {
  scenario_e with
  name = "f";
  should_succeed = false;
  fistpoints = [ Squeeze.DisableTwoPhaseTargetSets ];
  (* Since one domain is trying to allocate and the other free (but is stuck), the allocating
     domain will try to allocate more memory than is free on the host IF we disable our two-phase
     setting of the domain targets. In real life, xen allocates memory from the top down, keeping
     memory < 4GiB until the end. This is important because some small structures can only be placed
     in memory < 4GiB. If we give this memory to a guest then the balloon driver may well only release
     memory > 4GiB resulting in a system with memory free but memory allocation failures on domain create. *)
}

let scenario_g = {
  scenario_a with
  name = "g";
  should_succeed = false;
  fistpoints = [ Squeeze.DisableInaccuracyCompensation ];
  (* The two domains are programmed to have an inaccuracy of 4KiB. We will conclude that the
     domains are both stuck if we don't take this into account. *)
}

let scenario_h = {
  name = "h";
  description = "a small domain with a hidden upper limit and a perfectly working domain";
  should_succeed = true;
  scenario_domains = [
    new idealised_vm_with_upper_limit
      (domain_make 0 true 1000L 1500L 2000L 1500L 1500L 4L) 100L 1500L;
    new idealised_vm
      (domain_make 1 true 1000L 1500L 2000L 1500L 1500L 4L) 100L; (* this one can take up the slack *)
  ];
  host_free_mem_kib = 1000L;
  required_mem_kib = 0L;
  fistpoints = [ ];
}

(* scenario_a with < 24L after the balancing will fail *)

let all_scenarios = [
  scenario_a;
  scenario_b;
  scenario_c;
  scenario_d;
  scenario_e;
  scenario_f;
  scenario_g;
  scenario_h;
]

(*
(** Fails if either memory_actual or target lie outside our dynamic range *)
let assert_within_dynamic_range host =
	List.iter
		(fun domain ->
			 let lt domid x x' y y' =
				if x < y
				then failwith (Printf.sprintf "domid %d %s (%Ld) < %s (%Ld)"
					domid x' x y' y)
			in
			lt domain.domid domain.memory_actual_kib "memory_actual"
				domain.dynamic_min_kib "dynamic_min";
			lt domain.domid domain.target_kib "target"
				domain.dynamic_min_kib "dynamic_min";
			lt domain.domid domain.dynamic_max_kib "dynamic_max"
				domain.memory_actual_kib "memory_actual";
			lt domain.domid domain.dynamic_max_kib "dynamic_max"
				domain.target_kib "target"
		)
		host.domains
*)

let verify_memory_is_guaranteed_free host kib =
  (* Each domain could set its memory_actual to this much and still be considered ok *)
  let extreme domain = domain.target_kib +* domain.inaccuracy_kib in
  let increase domain = extreme domain -* domain.memory_actual_kib in
  let total = List.fold_left ( +* ) 0L (List.map increase host.domains) in
  if host.free_mem_kib -* total < kib
  then failwith (Printf.sprintf "Memory not guaranteed free: free_mem = %Ld; total guests could take = %Ld; required free = %Ld" host.free_mem_kib total kib)

let files_created_by_scenario scenario =
  [
    Printf.sprintf "%s.dat" scenario.name;
    Printf.sprintf "%s.out" scenario.name;
    Printf.sprintf "%s.gp" scenario.name;
  ]

(** Run a full simulation of the given scenario *)
let simulate scenario =
  let host_free_mem_kib = ref scenario.host_free_mem_kib in
  let all_domains = ref scenario.scenario_domains in

  let domid_to_domain = List.map (fun x -> x#get_domain.domid, x) !all_domains in
  (* Update all the recorded balloon targets *)
  let execute_action (action: action) =
    let domain = List.assoc action.action_domid domid_to_domain in
    if action.new_target_kib > domain#get_domain.memory_max_kib then begin
      domain#set_maxmem action.new_target_kib;
      domain#set_target action.new_target_kib;
    end else begin
      domain#set_target action.new_target_kib;
      domain#set_maxmem action.new_target_kib;
    end
  in
  let setmaxmem domid kib =
    debug "setmaxmem domid = %d; kib = %Ld" domid kib;
    execute_action { Squeeze.action_domid = domid; new_target_kib = kib }
  in
  (* Allow the simulated balloon drivers to change memory_actual_kib *)
  (* and update host_free_memory accordingly.                        *)
  let update_balloons time =
    List.iter
      (fun d ->
         let delta = d#update !host_free_mem_kib time in
         host_free_mem_kib :=
           !host_free_mem_kib -* delta
      )
      !all_domains
  in

  let dat_filename = Printf.sprintf "%s.dat" scenario.name in
  let out_filename = Printf.sprintf "%s.out" scenario.name in

  let dat_oc = open_out dat_filename in
  let out_oc = open_out out_filename in
  debug_oc := out_oc;

  let cols = [ Gnuplot.Memory_actual; Gnuplot.Target ] in
  Gnuplot.write_header dat_oc cols;
  let i = ref 0 in

  let gettimeofday () = float_of_int !i /. 10. in

  let make_host () =
    Squeeze.make_host
      ~free_mem_kib:!host_free_mem_kib
      ~domains:(List.map (fun d -> d#get_domain) !all_domains) in

  let wait _ =
    incr i;
    let t = gettimeofday () in
    update_balloons t;
    Gnuplot.write_row dat_oc (make_host ()) cols t in

  let io = {
    verbose = true;
    Squeeze.gettimeofday = gettimeofday;
    make_host = (fun () -> Printf.sprintf "F%Ld" !host_free_mem_kib, make_host ());
    domain_setmaxmem = setmaxmem;
    wait = wait;
    execute_action = execute_action;
    target_host_free_mem_kib = scenario.required_mem_kib;
    free_memory_tolerance_kib = 0L;
  } in

  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
       (* Phase 1: attempt to free memory *)
       debug "%s: attempting to free %Ld KiB" scenario.name scenario.required_mem_kib;
       Squeeze.change_host_free_memory ~fistpoints:scenario.fistpoints io scenario.required_mem_kib (fun x -> x >= scenario.required_mem_kib);
       debug "%s: %Ld KiB of memory has been freed" scenario.name scenario.required_mem_kib;

       (* Check that even if all domains ballooned up to target + accuracy, this much memory would still be free *)
       verify_memory_is_guaranteed_free (make_host ()) scenario.required_mem_kib;

       (* Phase 2: use some of this memory and return the rest to remaining VMs *)
       host_free_mem_kib := !host_free_mem_kib -* 500L;

       (* Phase 3: give free memory back to VMs *)
       Squeeze.change_host_free_memory ~fistpoints:scenario.fistpoints io 0L (fun x -> x <= 32L);
       debug "%s: After rebalancing only %Ld KiB of memory is used" scenario.name !host_free_mem_kib;

       verify_memory_is_guaranteed_free (make_host ()) 0L;
    )
    (fun () ->
       close_out dat_oc;
       close_out out_oc;
       debug_oc := stderr;
       Gnuplot.write_gp scenario.name (make_host()) cols)

let failed_scenarios = ref []
let scenario_error_table = ref []

let run_test scenario =
  try
    simulate scenario;
    List.iter Xapi_stdext_unix.Unixext.unlink_safe (files_created_by_scenario scenario);
    if not scenario.should_succeed then begin
      failed_scenarios := scenario :: !failed_scenarios;
      scenario_error_table :=
        (scenario, "simulation was expected tp fail but succeeded")
        :: !scenario_error_table
    end
  with e ->
    if scenario.should_succeed then begin
      failed_scenarios := scenario :: !failed_scenarios;
      scenario_error_table :=
        (scenario,
         Printf.sprintf
           "simulation was expected to succeed but failed: %s"
           (Printexc.to_string e)
        )
        :: !scenario_error_table
    end else begin
      List.iter Xapi_stdext_unix.Unixext.unlink_safe (files_created_by_scenario scenario);
    end

let go () =
  List.iter run_test all_scenarios;
  debug "%d tests executed; %d unexpected results"
    (List.length all_scenarios) (List.length !failed_scenarios);
  List.iter
    (fun (scenario, error) -> debug "%s: %s" scenario.name error)
    !scenario_error_table
