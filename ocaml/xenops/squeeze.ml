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

(** Per-domain data *)
type domain = {
	domid: int;
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
	domid dynamic_min_kib target_kib dynamic_max_kib memory_actual_kib =
	{
		domid = domid;
		dynamic_min_kib = dynamic_min_kib;
		target_kib = target_kib;
		dynamic_max_kib = dynamic_max_kib;
		memory_actual_kib = memory_actual_kib
	}

let domain_to_string_pairs (x: domain) = 
	let i64 = Int64.to_string and i = string_of_int in
	[
		"domid", i x.domid;
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
}

let string_pairs_to_string (x: (string * string) list) =
	String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) x)

let host_to_string_pairs (x: host) =
	let domain_to_string d =
		string_pairs_to_string (domain_to_string_pairs d) in
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

let start = Unix.gettimeofday ()

let debug fmt =
	Printf.kprintf (fun x -> Printf.fprintf stdout "[%.2f] %s\n"
	(Unix.gettimeofday() -. start) x; flush stdout) fmt

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

	type t = (int, int64 * float) Hashtbl.t

	(** Make a monitoring object *)
	let make () = Hashtbl.create 10 

	(** Update our internal state given a snapshot of the outside world *)
	let update (x: t) (state: host) (now: float) =
		let have_useful_update domain = false
			(* either I have no information at all *)
			|| not (Hashtbl.mem x domain.domid)
			(* or the information I have is out of date *)
			|| (domain.memory_actual_kib <> (fst (Hashtbl.find x domain.domid)))
			(* or I've hit my target already (so I assume this *)
			(* is an update to avoid being declared stuck      *)
			|| (domain.memory_actual_kib = domain.target_kib)
		in
		List.iter
			(fun domain ->
				if have_useful_update domain then begin
					debug "domid %d has changed memory_actual = %Ld"
						domain.domid domain.memory_actual_kib;
					Hashtbl.replace x domain.domid
						(domain.memory_actual_kib, now)
				end
			)
			state.domains;
		(* Clear out dead domains just in case someone keeps *)
		(* one of these things around for a long time.       *)
		let live_domids = List.map (fun domain -> domain.domid) state.domains in
		let to_delete = Hashtbl.fold
			(fun domid _ acc ->
				if List.mem domid live_domids then acc else domid :: acc
			)
			x []
		in
		List.iter (Hashtbl.remove x) to_delete

	(** Return true if we think a particular driver is stuck. *)
	let domid_is_stuck (x: t) domid (now: float) = true
		&& (Hashtbl.mem x domid)
		&& (
			let _, time = Hashtbl.find x domid in
			now -. time > assume_balloon_driver_stuck_after
		)

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
		stuck_domains: domain list;
	}
	let make () = { stuckness = Stuckness_monitor.make (); stuck_domains = [] }

	(**
		Given a list of domains, return the total amount of memory which could
		be freed (in theory) if each were ballooned down to their dynamic_min.
	*)
	let compute_total_freeable_memory domains =
		(* If all non-stuck domains balloon down to their *)
		(* dynamic_min, how much memory would be freed?   *)
		let freeable_memory_kib = List.map
			(fun domain ->
				domain.memory_actual_kib -* domain.dynamic_min_kib)
			domains in
		List.fold_left ( +* ) 0L freeable_memory_kib

	(** Constrains [value] within the range [minimum] ... [maximum]. *)
	let constrain minimum maximum value =
		assert (minimum < maximum);
		if value < minimum then minimum else
		if value > maximum then maximum else value

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
		(* The following divide operation can produce values from one *)
		(* of the following ranges:                                   *)
		(*     a. between 0 and 1 (when host memory is over-utilised) *)
		(*     b. greater than 1 (when host memory is under-utilised) *)
		(*     c. \infinity (if \forall i \in VM. min_i = max_i       *)
		(* Constrain the result so that it satisfies 0 ≤ \gamma ≤ 1.  *) 
		let gamma_unconstrained = Int64.to_float surplus_memory_kib /.
			Int64.to_float sum_of_dynamic_ranges in
		let gamma = constrain 0.0 1.0 gamma_unconstrained in
		List.concat
			(List.map 
				(fun domain ->
					let new_target_kib = domain.dynamic_min_kib +*
						Int64.of_float (Int64.to_float (
						domain.dynamic_max_kib -* domain.dynamic_min_kib) *.
						gamma)
					in
					(* Don't generate no-ops i.e. attempts *)
					(* to set the target to the same value *)
					if new_target_kib <> domain.target_kib
					then [{
						action_domid = domain.domid;
						new_target_kib = new_target_kib;
					}]
					else []
				)
				domains
			)

	(**
		Takes a view of the host state and amount of free memory desired and
		returns a list of ballooning actions which may help achieve the goal.
	*)
	let free_memory (x: t) (host: host) free_memory_needed_kib (now: float) =
		Stuckness_monitor.update x.stuckness host now;
		let non_stuck_domains = List.filter
			(fun domain ->
					not (Stuckness_monitor.domid_is_stuck
						x.stuckness domain.domid now
					)
			)
			host.domains
		in
		let stuck_domains = set_difference host.domains non_stuck_domains in
		List.iter
			(fun domain ->
				debug "domid %d has been declared stuck" domain.domid
			)
			(set_difference stuck_domains x.stuck_domains);
		List.iter
			(fun domain ->
				debug "domid %d has been declared unstuck" domain.domid
			)
			(set_difference x.stuck_domains stuck_domains);

		let x = { x with stuck_domains = stuck_domains } in

		(* Continue while there is more memory to free and *)
		(* apparently working balloon drivers to free it.  *)
		if host.free_mem_kib >= free_memory_needed_kib then begin
			debug "There is enough free memory already (%Ld >= %Ld)"
				host.free_mem_kib free_memory_needed_kib;
			x, Success
		end else begin

		(* Assuming *all non-stuck* domains successfully *)
		(* balloon down to their dynamic_min:            *)
		let total_freeable_memory_kib =
			compute_total_freeable_memory non_stuck_domains in
		(* If this happens, what would the resulting surplus be? *)
		let surplus_memory_kib = host.free_mem_kib +* total_freeable_memory_kib
			-* free_memory_needed_kib in

		(* If surplus_memory < 0L then there is nothing we think we can *)
		(* do with the non_stuck_domains which will be good enough; in  *)
		(* this case we adopt a 'do no harm' principle and do nothing.  *)
		if surplus_memory_kib < 0L then begin
			x, Failed stuck_domains
		end else begin
			let actions = allocate_memory_in_proportion surplus_memory_kib
				non_stuck_domains in
			x, AdjustTargets actions
		end
	end

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
		let total_freeable_memory_kib =
			compute_total_freeable_memory host.domains in
		let surplus_memory_kib =
			host.free_mem_kib +* total_freeable_memory_kib in
		allocate_memory_in_proportion surplus_memory_kib host.domains

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
