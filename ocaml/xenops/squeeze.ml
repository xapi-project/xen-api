(**
	Implement a simple 'proportional memory' policy where VMs are squeezed
	equally to free memory. There is no direct attempt to respond to distress or
	memory pressure information from within the guest. Guests which are stuck
	(e.g. broken balloon driver or who cannot free any more memory) are observed
	and worked around.
*)
(*
	Remember:
	Be tolerant of domains appearing or disappearing.
	What if target <> memory_actual because of domU accounting errors.
	Can we calibrate on boot if the offset is const?
*)

(* Make debug printing work both when linked into a daemon and from the commandline *)
let start = Unix.gettimeofday ()

module D = Debug.Debugger(struct let name = "xenops" end)

let debug fmt =
  Printf.kprintf 
    (fun x -> 
       Printf.fprintf stdout "[%.2f] %s\n" (Unix.gettimeofday() -. start) x; 
       flush stdout;
       D.debug "%s" x
    ) fmt

(** Per-domain data *)
type domain = {
	domid: int;
	(** true if the domain has ballooning capability i.e. is not paused etc. *)
	can_balloon: bool;
	(** admin-imposed lower-limit on the balloon target *)
	dynamic_min_kib: int64;
	(** current balloon target requested by the system *)
	target_kib: int64;
	(** admin-imposed upper-limit on the balloon target *)
	dynamic_max_kib: int64;
	(** view from dom0 of how much memory this guest is using *)
	memory_actual_kib: int64;
}

let domain_make
	domid can_balloon dynamic_min_kib target_kib dynamic_max_kib memory_actual_kib =
	{
		domid = domid;
		can_balloon = can_balloon;
		dynamic_min_kib = dynamic_min_kib;
		target_kib = target_kib;
		dynamic_max_kib = dynamic_max_kib;
		memory_actual_kib = memory_actual_kib
	}

let domain_to_string_pairs (x: domain) = 
	let i64 = Int64.to_string and i = string_of_int in
	[
		"domid", i x.domid;
		"can_balloon", string_of_bool x.can_balloon;
		"dynamic_min_kib", i64 x.dynamic_min_kib;
		"target_kib", i64 x.target_kib;
		"dynamic_max_kib", i64 x.dynamic_max_kib;
		"memory_actual_kib", i64 x.memory_actual_kib;
	]

(** Per-Host data *)
type host = {
	(** VMs running on this host *)
	domains: domain list;
	(** total free memory on this host *)
	free_mem_kib: int64;
	(** size of the emergency pool; memory which cannot be used *)
	emergency_pool_kib: int64;
}

let string_pairs_to_string (x: (string * string) list) =
	String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x)

let domain_to_string d = string_pairs_to_string (domain_to_string_pairs d)

let host_to_string_pairs (x: host) =
	let domains = (String.concat "; " (List.map domain_to_string x.domains)) in
	[
		"domains", "[" ^ domains ^ "]";
		"free_mem_kib", Int64.to_string x.free_mem_kib;
	]

(** The ballooning algorithm returns a list of actions to perform *)
type action = {
	(** domid of domain to operate on *)
	action_domid: int;
	(** new balloon target to set *)
	new_target_kib: int64;
}

let action_to_string_pairs (x: action) = [
	"domid", string_of_int x.action_domid;
	"new_target_kib", Int64.to_string x.new_target_kib;
]

let ( -* ) = Int64.sub
let ( +* ) = Int64.add
let ( ** ) = Int64.mul

let set_difference a b = List.filter (fun x -> not (List.mem x b)) a

(** Fails if either memory_actual or target lie outside our dynamic range *)
let assert_within_dynamic_range host = List.iter
	(fun domain ->
		let lt domid x x' y y' =
			if x < y
			then failwith
				(Printf.sprintf "domid %d %s (%Ld) < %s (%Ld)" domid x' x y' y)
		in
		(*
		lt domain.domid domain.memory_actual_kib "memory_actual"
			domain.dynamic_min_kib "dynamic_min";
		*)
		lt domain.domid domain.target_kib "target"
			domain.dynamic_min_kib "dynamic_min";
		lt domain.domid domain.dynamic_max_kib "dynamic_max"
			domain.memory_actual_kib "memory_actual";
		lt domain.domid domain.dynamic_max_kib "dynamic_max"
			domain.target_kib "target"
	)
	host.domains

(** The value returned from the 'free_memory' function *)
type result = 
	| Success
		(** enough memory is now available *)
	| Failed of domain list
		(** we have run out of options: all domains are either
		fully-ballooned or are stuck (stuck domids returned) *)
	| AdjustTargets of action list
		(** we want to change the targets of some domains *)

(** Work around the fact that the target may not be hit precisely *)
let has_hit_target memory_actual_kib target_kib =
  Int64.div memory_actual_kib 4L = (Int64.div target_kib 4L)

(** Generic code to guesstimate if a balloon driver is stuck *)
module Stuckness_monitor = struct

	(*
		We keep some state to help us spot stuck / dead / unco-operative
		balloon drivers. If a driver has been requested to release some memory
		but nothing has changed after some threshold time, we mark it as stuck
		and exclude it from our calculations. The effect is to ask the remaining
		functioning balloon drivers to balloon down faster.
	*)
	let assume_balloon_driver_stuck_after = 2. (* seconds *)

	type t = { memory_actual_updates: (int, int64 * float) Hashtbl.t;
		   has_hit_targets: (int, bool) Hashtbl.t }

	(** Make a monitoring object *)
	let make () : t = { memory_actual_updates = Hashtbl.create 10; has_hit_targets = Hashtbl.create 10 }

	(** Update our internal state given a snapshot of the outside world *)
	let update (x: t) (state: host) (now: float) =
		List.iter
			(fun domain ->
			   let hit_target = has_hit_target domain.memory_actual_kib domain.target_kib in
			   if not hit_target && Hashtbl.mem x.has_hit_targets domain.domid then begin
			     debug "domid %d is nolonger on its target; target = %Ld; memory_actual = %Ld" domain.domid domain.target_kib domain.memory_actual_kib;
			     Hashtbl.remove x.has_hit_targets domain.domid
			   end;

			   let have_useful_update = 
			     (* either I have no information at all *)
			     if not (Hashtbl.mem x.memory_actual_updates domain.domid) then begin
			       debug "domid %d is untracked so I will consider memory_actual = %Ld to be a change"
				 domain.domid domain.memory_actual_kib;
			       true
			     end else if domain.memory_actual_kib <> fst (Hashtbl.find x.memory_actual_updates domain.domid) then begin
			       (* or the information I have is out of date *)
			       debug "domid %d has changed memory_actual from %Ld to %Ld" domain.domid (fst (Hashtbl.find x.memory_actual_updates domain.domid)) domain.memory_actual_kib;
			       true
			     end else if hit_target then begin
			       (* we assume that if the target has been hit then the domain is still active *)
			       if not (Hashtbl.mem x.has_hit_targets domain.domid) then begin
				 debug "domid %d has hit its target; target = %Ld; memory_actual = %Ld" domain.domid domain.target_kib domain.memory_actual_kib;
				 Hashtbl.replace x.has_hit_targets domain.domid true
			       end;
			       true
			     end else if domain.memory_actual_kib < domain.target_kib && state.free_mem_kib <= state.emergency_pool_kib then begin
			       (* I've been told to allocate more memory but I can't since there 
				  is no memory free... probably because of someone else getting stuck *)
			       true
			     end else false in
			     if have_useful_update 
			     then Hashtbl.replace x.memory_actual_updates domain.domid (domain.memory_actual_kib, now)
			)
			state.domains;
		(* Clear out dead domains just in case someone keeps *)
		(* one of these things around for a long time.       *)
		let live_domids = List.map (fun domain -> domain.domid) state.domains in
		let to_delete = Hashtbl.fold
			(fun domid _ acc ->
				if List.mem domid live_domids then acc else domid :: acc
			)
			x.memory_actual_updates []
		in
		List.iter (Hashtbl.remove x.memory_actual_updates) to_delete;
		List.iter (Hashtbl.remove x.has_hit_targets) to_delete

	(** Return true if we think a particular driver is still making useful progress.
	    If it is not making progress it may have either hit its target or it may have failed. *)
	let domid_is_active (x: t) domid (now: float) = 
	  if not (Hashtbl.mem x.memory_actual_updates domid)
	  then false (* it must have been destroyed *)
	  else 
	    let _, time = Hashtbl.find x.memory_actual_updates domid in
	    now -. time <= assume_balloon_driver_stuck_after

		 
(* XXX: debugging *)

end

(**
	Represents an algorithm which attempts to (i) free memory; and (ii) balance
	memory between VMs on a host by setting balloon targets.
*)
module Proportional = struct

	(** State maintained between invocations of the algorithm function *)
	type t = {
		stuckness: Stuckness_monitor.t;
		non_active_domains: domain list;
	}
	let make () = { stuckness = Stuckness_monitor.make (); non_active_domains = [] }

	(**
		Given a list of domains, return the total amount of memory which could
		be freed (in theory) if each were ballooned down to their dynamic_min;
		and the total amount of memory which could be allocated (in theory)
		if each were ballooned up to their dynamic_max.
	*)
	let compute_host_memory_delta_range domains =
		(* If all non-stuck domains balloon down to their *)
		(* dynamic_min, how much memory would be freed?   *)
		let freeable_memory_kib = List.map
			(fun domain ->
				domain.memory_actual_kib -* domain.dynamic_min_kib)
			domains in
		let could_be_freed = List.fold_left ( +* ) 0L freeable_memory_kib in
		(* If all non-stuck domains balloon up to their   *)
		(* dynamic_max, how much memory would be used?    *)
		let allocatable_memory_kib = List.map
			(fun domain ->
				domain.dynamic_max_kib -* domain.memory_actual_kib)
			domains in
		let could_be_allocated = List.fold_left (+*) 0L allocatable_memory_kib in
		could_be_freed, could_be_allocated

	(** Constrains [value] within the range [minimum] ... [maximum]. *)
	let constrain minimum maximum value =
		assert (minimum < maximum);
		if value < minimum then minimum else
		if value > maximum then maximum else 
		if classify_float value = FP_nan then minimum else (* arbitrary *)
		value

	(**
		Given an amount of theoretically surplus memory (= host free memory +
		that which would be freed by ballooning down to dynamic_min) produce a
		set of balloon-target-set actions to divide it up amongst the given VMs
		so that (target - min) / (max - min) is the same for all VMs.
	*)
	let allocate_memory_in_proportion surplus_memory_kib domains =
		(* We assign surplus memory to domains in proportion to  *)
		(* their dynamic ranges [min_i...max_i].                 *)
		(* First we find the greatest \gamma such that:          *)
		(*     \sigma i \in VM. \gamma.(max_i - min_i) ≤ surplus *)
		(*     where 0 ≤ \gamma ≤ 1                              *)
		(* Then we assign each guest a target t_i according to:  *)
		(*     \forall i \in VM. t_i := \gamma.(max_i - min_i)   *)
		let dynamic_ranges = List.map
			(fun domain -> domain.dynamic_max_kib -* domain.dynamic_min_kib)
			domains in
		let sum_of_dynamic_ranges = List.fold_left ( +* ) 0L dynamic_ranges in
		(* Note that we can't allocate more then 'sum_of_dynamic_ranges' without asking VMs to
		   increase *above* they're dynamic_max. We can compute the total amount of memory
		   we expect to allocate: *)
		let unallocatable_memory_kib = max 0L (surplus_memory_kib -* sum_of_dynamic_ranges) in
		let total_to_allocate_kib = surplus_memory_kib -* unallocatable_memory_kib in
		if total_to_allocate_kib < 0L
		then failwith (Printf.sprintf "domains = %s; surplus = %Ld; sum_dynamic = %Ld; total_to_allocate = %Ld" (String.concat ";" (List.map domain_to_string domains)) surplus_memory_kib sum_of_dynamic_ranges total_to_allocate_kib);
		assert (total_to_allocate_kib <= sum_of_dynamic_ranges);
		assert (total_to_allocate_kib >= 0L);

		(* The following divide operation can produce values from one *)
		(* of the following ranges:                                   *)
		(*     a. between 0 and 1 (when host memory is over-utilised) *)
		(*     b. greater than 1 (when host memory is under-utilised) *)
		(*     c. \nan (if \forall i \in VM. min_i = max_i       *)
		(* Constrain the result so that it satisfies 0 ≤ \gamma ≤ 1.  *) 
		let gamma_unconstrained = Int64.to_float total_to_allocate_kib /.
			Int64.to_float sum_of_dynamic_ranges in
		let gamma = constrain 0.0 1.0 gamma_unconstrained in

		debug "total surplus memory = %Ld KiB; total we can allocate = %Ld KiB; gamma = %.2f" surplus_memory_kib total_to_allocate_kib gamma;

		(* Calculate how much memory should be allocated to each domain (note we round down) *)
		let per_domain_allocations = List.map (fun range -> Int64.of_float (Int64.to_float range *. gamma)) dynamic_ranges in

		(* Since we rounded down some of the total may now be unallocated. *)
		let total_allocated = List.fold_left ( +* ) 0L per_domain_allocations in
		let under_allocation = total_to_allocate_kib -* total_allocated in

		if under_allocation < 0L
		then failwith (Printf.sprintf "domains = %s; surplus = %Ld; to_allocate = %Ld; gamma = %.2f; total_alloc = %Ld"
		  (let domain_to_string d = string_pairs_to_string (domain_to_string_pairs d) in
		   (String.concat "; " (List.map domain_to_string domains)))
		  surplus_memory_kib total_to_allocate_kib gamma total_allocated);

		assert (under_allocation >= 0L); (* because we rounded down *)
		let per_domain_potential_increase = List.map (fun (range, allocation) -> range -* allocation)
		  (List.combine dynamic_ranges per_domain_allocations) in
		(* Walk through the list of potential increases, increasing until we've allocated everything *)
		let remaining, adjustments_reversed = 
		  List.fold_left (fun (remaining, extra) possible -> 
				    let allocation = min remaining possible in
				    remaining -* allocation, allocation :: extra) 
		     (under_allocation, []) per_domain_potential_increase in
		assert (remaining = 0L);
		let adjustments = List.rev adjustments_reversed in
		let per_domain_allocations = List.map (fun (a, b) -> a +* b) (List.combine per_domain_allocations adjustments) in

		let new_targets = List.map (fun (domain, allocation) -> domain, (domain.dynamic_min_kib +* allocation))
		  (List.combine domains per_domain_allocations) in
		(* Filter out attempts to set the target to a different value *)
		let useful_new_targets = List.filter (fun (domain, target) -> domain.target_kib <> target) new_targets in
		List.map (fun (domain, target) -> { action_domid = domain.domid; new_target_kib = target }) useful_new_targets

	(**
		Takes a view of the host state and amount of free memory desired and
		returns a list of ballooning actions which may help achieve the goal.
	*)
	let change_host_free_memory success_condition (x: t) (host: host) host_target_kib (now: float) =
		(* 1. Compute which domains are still considered active *)
		Stuckness_monitor.update x.stuckness host now;
		let active_domains = 
		  List.filter (fun domain ->
				 domain.can_balloon
				 && (Stuckness_monitor.domid_is_active x.stuckness domain.domid now))
			host.domains in
		let non_active_domains = set_difference host.domains active_domains in
		let declared_inactive = set_difference non_active_domains x.non_active_domains in
		let declared_active = set_difference x.non_active_domains non_active_domains in
		
		List.iter
			(fun domain ->
				debug "domid %d has been declared inactive" domain.domid
			) declared_inactive;
		List.iter
			(fun domain ->
				debug "domid %d has been declared active" domain.domid
			) declared_active;

		let x = { x with non_active_domains = non_active_domains } in

		(* 2. Compute how we would adjust the domain memory targets *)

		(* Assuming *all non-stuck* domains successfully balloon down to their *)
		(* dynamic_min or up to their dynamic_max:                             *)
		let total_freeable_memory_kib, total_allocatable_memory_kib =
			compute_host_memory_delta_range active_domains in
		let minimum_possible_free_memory_kib = host.free_mem_kib -* total_allocatable_memory_kib in
		let maximum_possible_free_memory_kib = host.free_mem_kib +* total_freeable_memory_kib in

		(* 'allocate_memory_in_proportion' is used to give back some memory after we pretend
		   that every domain has been ballooned down to dynamic_min. Obviously the amount of 
		   memory given back can't be negative (negative implies 'target_too_big' *)
		let give_back_kib = max 0L (maximum_possible_free_memory_kib -* host_target_kib) in
		let adjustments = allocate_memory_in_proportion give_back_kib active_domains in

		(* Have all the non-stuck domains reached their current targets? *)
		let targets_reached = List.map (fun domain -> has_hit_target domain.memory_actual_kib domain.target_kib) active_domains in
		let all_targets_reached = List.fold_left (&&) true targets_reached in

		(* Note the asymmetry between:
		   1. increasing free memory: if we think we can't free enough then we give up
		   2. reducing free memory: we wait until as much as possible is allocated *)
		
		let success = success_condition host.free_mem_kib in

		let target_too_big = maximum_possible_free_memory_kib < host_target_kib in
		let cant_allocate_any_more = host.free_mem_kib > host_target_kib && total_allocatable_memory_kib = 0L in

		debug "host free memory: %Ld <= %Ld <= %Ld (goal = %Ld)%s; all domain targets %sreached%s" 
		  minimum_possible_free_memory_kib host.free_mem_kib maximum_possible_free_memory_kib host_target_kib
		  (if success then " OK"
		   else if target_too_big then " cannot free enough"
		   else if cant_allocate_any_more then " cannot allocate enough" else "")
		  (if all_targets_reached then "" else "not ")
		  (if adjustments = [] then "" else "; however about to adjust targets");

		if (success && all_targets_reached && (adjustments = [])) || cant_allocate_any_more then x, declared_active, declared_inactive, Success
		else 
		  if target_too_big
		  then x, declared_active, declared_inactive, Failed non_active_domains
		  else x, declared_active, declared_inactive, AdjustTargets adjustments

	let free_memory x h host_target_kib n = change_host_free_memory (fun x -> x >= host_target_kib) x h host_target_kib n 

	let balance x h n = change_host_free_memory (fun x -> x = 0L) x h 0L n
(*
	(**
		Takes a view of the host state and issues actions which, if executed,
		would share the host memory amongst the VMs.
	*)
	let balance (host: host) =
		(*
			We ignore stuck domains. We pretend that all domains balloon down to
			their dynamic_min and compute how much memory would be freed. We add
			this to the host's free memory and then divide it up amongst the
			domains proportionally.

			If some domains are stuck they'll be left with target < actual,
			while non-stuck domains will have target > memory_actual. Hopefully
			if some domains become unstuck in future, then memory will be
			automatically re-allocated to the non-stuck domains.

			If some domains are destroyed in future then this function will need
			to be recalled to re-divide the memory.
		*)

		(* Here we assume *all* domains become unstuck and  *)
		(* successfully balloon down to their dynamic_min:  *)
		(* Σ v ∈ V (v.memory-actual - v.memory-dynamic-min) *)
		let total_freeable_memory_kib, _ =
			compute_host_memory_delta_range host.domains in
		let surplus_memory_kib =
			host.free_mem_kib +* total_freeable_memory_kib in
		allocate_memory_in_proportion surplus_memory_kib host.domains
*)
end

module Gnuplot = struct
	type colspec = Dynamic_min | Memory_actual | Dynamic_max | Target

	let write_row oc host cols time =
		Printf.fprintf oc "%.2f %Ld " time host.free_mem_kib;
		List.iter
			(fun domain ->
				if List.mem Dynamic_min cols
				then Printf.fprintf oc " %Ld " domain.dynamic_min_kib;
				if List.mem Dynamic_max cols
				then Printf.fprintf oc " %Ld " domain.dynamic_max_kib;
				if List.mem Memory_actual cols
				then Printf.fprintf oc " %Ld " domain.memory_actual_kib;
				if List.mem Target cols
				then Printf.fprintf oc " %Ld " domain.target_kib
			)
			host.domains;
		Printf.fprintf oc "\n"

	let write_gp stem host cols = 
	let oc = open_out (Printf.sprintf "%s.gp" stem) in
	Printf.fprintf oc "set xlabel 'time/seconds'\n";
	Printf.fprintf oc "set ylabel 'memory/KiB'\n";
	Printf.fprintf oc "plot \"%s.dat\" using 1:2 title \"free host memory\" \
		with lines " stem;
	let col = ref 3 in
	List.iter
		(fun domain ->
			if List.mem Dynamic_min cols
			then (Printf.fprintf oc ", \"%s.dat\" using 1:%d title \"domid %d \
				dynamic_min\" with points " stem !col domain.domid; incr col);
			if List.mem Dynamic_max cols
			then (Printf.fprintf oc ", \"%s.dat\" using 1:%d title \"domid %d \
				dynamic_max\" with points " stem !col domain.domid; incr col);
			if List.mem Memory_actual cols
			then (Printf.fprintf oc ", \"%s.dat\" using 1:%d title \"domid %d \
				memory_actual\" with lines " stem !col domain.domid; incr col);
			if List.mem Target cols
			then (Printf.fprintf oc ", \"%s.dat\" using 1:%d title \"domid %d \
				target\" with lines " stem !col domain.domid; incr col);
		)
		host.domains;
	close_out oc
end
