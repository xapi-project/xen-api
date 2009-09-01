(**
	Simulation environment and set of unit tests for the domain memory balancer.
*)

open Squeeze

(**
	Computes the memory_actual delta for a VM assuming the balloon driver
	responds at a given speed. Warning: make sure the balloon_rate * time_passed
	is > 0 when rounded to an integer.
*)
let compute_memory_actual_delta
		domain balloon_rate_kib_per_unit_time time_passed =
	let kib_changed = Int64.of_float (time_passed *. (
		Int64.to_float balloon_rate_kib_per_unit_time)) in
	(* compute the direction of travel of the balloon *)
	let vector =
		if domain.memory_actual_kib > domain.target_kib then -1L else 1L in
	let abs_distance_from_target =
		(domain.target_kib -* domain.memory_actual_kib) ** vector in
	(* don't exceed the target *)
	let abs_distance_to_move = min abs_distance_from_target kib_changed in
	abs_distance_to_move ** vector

class virtual vm initial_domain = object (self)
	val mutable domain = initial_domain
	val mutable time_of_last_update = 0.

	(** Return the current domain state *)
	method get_domain = domain

	(** Helper function to change the domain's balloon target *)
	method set_target new_target_kib =
		if new_target_kib <> domain.target_kib
		then debug "domid %d: target change was %Ld now %Ld"
			domain.domid domain.target_kib new_target_kib;
		domain <- { domain with target_kib = new_target_kib }

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
		let memory_actual_delta = min host_free_mem
			(self#compute_memory_actual_delta time_passed) in
		domain <- {domain with
			memory_actual_kib = domain.memory_actual_kib +* memory_actual_delta
		};
		time_of_last_update <- time;
		memory_actual_delta (* if -ve this means host memory increased *)
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
	host_emergency_pool_kib: int64;
	required_mem_kib: int64;
}


let scenario_a = {
	name = "a";
	description = "a small domain with a hidden limit and a large domain which \
		exhibits 'sticky' behaviour"; 
	should_succeed = true;
	scenario_domains = [
		new idealised_vm_with_limit
			(domain_make 0 true 1000L 1500L 2000L 1500L) 100L 1250L;
		new intermittently_stuck_vm
			(domain_make 1 true 2500L 3500L 4500L 3500L) 500L 0.25;
	];
	host_free_mem_kib = 0L;
	host_emergency_pool_kib = 0L;
	required_mem_kib = 1000L
}

let scenario_b = {
	name = "b";
	description = "two domains exhibiting 'sticky' behaviour with different \
		periods: one > than the assumed stuck interval and the other <";
	should_succeed = true;
	scenario_domains = [
		new intermittently_stuck_vm
			(domain_make 1 true 500L 3500L 4500L 3500L) 100L 3.;
		new intermittently_stuck_vm
			(domain_make 0 true 500L 1500L 2500L 1500L) 100L 1.5;
	];
	host_free_mem_kib = 0L;
	host_emergency_pool_kib = 0L;
	required_mem_kib = 1000L }

let scenario_c = {
	name = "c";
	description = "dynamic_mins are too high to allow enough memory to be \
		freed";
	should_succeed = false;
	scenario_domains = [
		new idealised_vm (domain_make 0 true 1000L 1500L 2000L 1500L) 100L;
		new idealised_vm (domain_make 1 true 2000L 2500L 3000L 2500L) 100L;
	];
	host_free_mem_kib = 0L;
	host_emergency_pool_kib = 0L;
	required_mem_kib = 1500L 
}

let scenario_d = {
	name = "d";
	description = "looks ok but one VM is permanently stuck above its \
		dynamic_min";
	should_succeed = false;
	scenario_domains = [
		new idealised_vm
			(domain_make 0 true 1000L 1500L 2000L 1500L) 100L;
		new idealised_vm_with_limit
			(domain_make 1 true 2000L 2500L 3000L 2500L) 100L 2250L;
	];
	host_free_mem_kib = 0L;
	host_emergency_pool_kib = 0L;
	required_mem_kib = 1000L
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
			(domain_make 0 true (*min*)5000L (*target*)7000L (*max*)7000L (*actual*)7000L);
		(* The working domain is using less than it should be if the memory was freed and everything balanced *)
		new idealised_vm
			(domain_make 1 true (*min*)5000L (*target*)6000L (*max*)11000L (*actual*)6000L) 100L;

	];
	host_free_mem_kib = 0L;
	host_emergency_pool_kib = 0L;
	required_mem_kib = 1000L;
	(* The system has 3000L units of surplus memory. Ideally we'd give 1000L to the system and then split
	   the remaining 2000L units proportionally amongst the domains: 500L to 0 and 1500L to 1. If both 
	   domains were ideal then we could set 0's target to 5500L (down) and 1's target to 6500L (up)
	   However since the stuck domain is stuck this strategy will fail. In this case we want the idealised
	   VM to release the 1000L units of memory. However the likely failure mode is that it will have been
	   asked to increase its allocation and been unable to do so because all host memory is exhausted. 
	   It will then also have been marked as stuck and the operation will fail. *)
}
  
let all_scenarios = [
	scenario_a;
	scenario_b;
	scenario_c;
	scenario_d;
	scenario_e;
]
	
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

(** Run a full simulation of the given scenario *)
let simulate scenario = 
	let host_free_mem_kib = ref scenario.host_free_mem_kib in
	let emergency_pool_kib = scenario.host_emergency_pool_kib in
	let all_domains = ref scenario.scenario_domains in

	let domid_to_domain =
		List.map (fun x -> x#get_domain.domid, x) !all_domains
	in
	let make_host () = {
		free_mem_kib = !host_free_mem_kib;
		emergency_pool_kib = emergency_pool_kib;
		domains = List.map (fun d -> d#get_domain) !all_domains
	} in
	(* Update all the recorded balloon targets *)
	let update_target (action: action) =
		let domain = List.assoc action.action_domid domid_to_domain in
		domain#set_target action.new_target_kib
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

	let oc = open_out (Printf.sprintf "%s.dat" scenario.name) in
	let cols = [ Gnuplot.Memory_actual; Gnuplot.Target ] in
	let acc = ref (Proportional.make ()) in
	let i = ref 0 in

	(* Change the free memory on the host *)
	let change_memory success_condition host_target_kib = 

		let finished = ref false in
		while not (!finished) do
			let host = make_host () in
			if not (!finished) then begin
				let t = float_of_int !i /. 10. in
				let acc', _, _, result = Proportional.change_host_free_memory success_condition !acc host host_target_kib t in
				acc := acc';
				begin match result with
				| Success ->
					debug "%Ld KiB of memory has been freed"
						scenario.required_mem_kib;
					finished := true
				| Failed domains ->
					failwith (Printf.sprintf "Failed to free %Ld KiB of memory \
						(only %Ld KiB free) [stuck domains: %s]"
						scenario.required_mem_kib !host_free_mem_kib
						(String.concat ", "
							(List.map (fun x -> string_of_int x.domid) domains)
						)
					);
				| AdjustTargets actions ->
					(* Set all the balloon targets *)
					List.iter update_target actions
				end;
				(* Allow the balloon drivers to do some work *)
				update_balloons t;

				let host = make_host () in
				assert_within_dynamic_range host;
				Gnuplot.write_row oc host cols t;
				incr i

			end;
		done in

	(* Phase 1: attempt to free memory *)
	debug "%s: attempting to free %Ld KiB" scenario.name scenario.required_mem_kib;
	change_memory (fun x -> x >= scenario.required_mem_kib) scenario.required_mem_kib;
	debug "%s: %Ld KiB of memory has been freed" scenario.name scenario.required_mem_kib;

	(* Phase 2: use some of this memory and return the rest to remaining VMs *)
	host_free_mem_kib := !host_free_mem_kib -* 500L;

	(* Phase 3: give free memory back to VMs *)
	change_memory (fun x -> x = 0L) 0L;
	debug "%s: After rebalancing only %Ld KiB of memory is used" scenario.name !host_free_mem_kib;

	if !host_free_mem_kib > 10L 
	then failwith (
		Printf.sprintf
			"After the phase 2 balancer ran the host had %Ld KiB of memory left"
			!host_free_mem_kib
	);

	close_out oc;
	Gnuplot.write_gp scenario.name (make_host()) cols

let failed_scenarios = ref []
let scenario_error_table = ref []

let run_test scenario = 
	try
		simulate scenario;
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
		end

let _ = 
	List.iter run_test all_scenarios;
	debug "%d tests executed; %d unexpected results"
		(List.length all_scenarios) (List.length !failed_scenarios);
	List.iter
		(fun (scenario, error) -> debug "%s: %s" scenario.name error)
		!scenario_error_table
