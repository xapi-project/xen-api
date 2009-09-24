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

let debug_oc = ref stdout

let debug fmt =
  Printf.kprintf 
    (fun x -> 
       Printf.fprintf !debug_oc "[%.2f] %s\n" (Unix.gettimeofday() -. start) x; 
       flush !debug_oc;
       D.debug "%s" x
    ) fmt

let error fmt =
  Printf.kprintf 
    (fun x -> 
       Printf.fprintf !debug_oc "[%.2f] %s\n" (Unix.gettimeofday() -. start) x; 
       flush !debug_oc;
       D.error "%s" x
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
	(** xen maxmem *)
	memory_max_kib: int64;
	(** amount by which the target may differ from memory_actual and be declared a 'hit' *)
	inaccuracy_kib: int64;
}

let domain_make
	domid can_balloon dynamic_min_kib target_kib dynamic_max_kib memory_actual_kib memory_max_kib inaccuracy_kib =
	{
		domid = domid;
		can_balloon = can_balloon;
		dynamic_min_kib = dynamic_min_kib;
		target_kib = target_kib;
		dynamic_max_kib = dynamic_max_kib;
		memory_actual_kib = memory_actual_kib;
		memory_max_kib = memory_max_kib;
		inaccuracy_kib = inaccuracy_kib;
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
		"memory_max_kib", i64 x.memory_max_kib;
		"inaccuracy_kib", i64 x.inaccuracy_kib;
	]

module IntMap = Map.Make(struct type t = int let compare = compare end)

(** Per-Host data *)
type host = {
	(** VMs running on this host *)
	domains: domain list;
	domid_to_domain: domain IntMap.t;
	(** total free memory on this host *)
	free_mem_kib: int64;
}

let make_host ~domains ~free_mem_kib = 
  { 
    domains = domains;
    domid_to_domain = List.fold_left (fun map domain -> IntMap.add domain.domid domain map) IntMap.empty domains;
    free_mem_kib = free_mem_kib;
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
let sum = List.fold_left ( +* ) 0L 

(** The value returned from the 'free_memory' function *)
type result = 
	| Success
		(** enough memory is now available *)
	| Failed of int list
		(** we have run out of options: all domains are either
		fully-ballooned or are stuck (stuck domids returned) *)
	| AdjustTargets of action list
		(** we want to change the targets of some domains *)

type direction = Up | Down

let string_of_direction = function
  | Some Up -> "^" | Some Down -> "v" | None -> "x"

let direction_of_actual inaccuracy_kib memory_actual_kib target_kib = 
  let delta_kib = memory_actual_kib -* target_kib in
  let abs x = if x < 0L then 0L -* x else x in
  if abs delta_kib <= inaccuracy_kib then None
  else (if target_kib > memory_actual_kib then Some Down else Some Up)

let direction_of_int64 a b = if a = b then None else (if a > b then Some Down else Some Up)

(** Work around the fact that the target may not be hit precisely *)
let has_hit_target inaccuracy_kib memory_actual_kib target_kib = 
  direction_of_actual inaccuracy_kib memory_actual_kib target_kib = None

let short_string_of_domain domain = 
  Printf.sprintf "%d T%Ld A%Ld M%Ld %s%s" domain.domid
    domain.target_kib domain.memory_actual_kib domain.memory_max_kib
    (if domain.can_balloon then "B" else "?")
    (string_of_direction (direction_of_actual domain.inaccuracy_kib domain.memory_actual_kib domain.target_kib))

(** Generic code to guesstimate if a balloon driver is stuck *)
module Stuckness_monitor = struct

	(*
		We keep some state to help us spot stuck / dead / unco-operative
		balloon drivers. If a driver has been requested to release some memory
		but nothing has changed after some threshold time, we mark it as stuck
		and exclude it from our calculations. The effect is to ask the remaining
		functioning balloon drivers to balloon down faster.
	*)
	let assume_balloon_driver_stuck_after = 5. (* seconds *)

	type t = { memory_actual_updates: (int, int64 * float) Hashtbl.t;
		   has_hit_targets: (int, bool) Hashtbl.t }

	(** Make a monitoring object *)
	let make () : t = { memory_actual_updates = Hashtbl.create 10; has_hit_targets = Hashtbl.create 10 }

	(** Update our internal state given a snapshot of the outside world *)
	let update (x: t) (state: host) (now: float) =
		List.iter
			(fun domain ->
			   let hit_target = has_hit_target domain.inaccuracy_kib domain.memory_actual_kib domain.target_kib in
			   if not hit_target && Hashtbl.mem x.has_hit_targets domain.domid then begin
			     debug "domid %d is nolonger on its target; target = %Ld; memory_actual = %Ld" domain.domid domain.target_kib domain.memory_actual_kib;
			     Hashtbl.remove x.has_hit_targets domain.domid
			   end;

			   let have_useful_update = 
			     (* either I have no information at all *)
			     if not (Hashtbl.mem x.memory_actual_updates domain.domid) then begin
			       true
			     end else if domain.memory_actual_kib <> fst (Hashtbl.find x.memory_actual_updates domain.domid) then begin
			       (* or the information I have is out of date *)
			       true
			     end else if hit_target then begin
			       (* we assume that if the target has been hit then the domain is still active *)
			       if not (Hashtbl.mem x.has_hit_targets domain.domid) then begin
				 if domain.domid <> 0 (* dom0 is boring and sits on its target *)
				 then debug "domid %d has hit its target; target = %Ld; memory_actual = %Ld" domain.domid domain.target_kib domain.memory_actual_kib;
				 Hashtbl.replace x.has_hit_targets domain.domid true
			       end;
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
end

type fistpoint = 
  | DisableTwoPhaseTargetSets (** this prevents free memory going to 0 and allowing all low memory to be allocated *)
  | DisableInaccuracyCompensation (** don't factor balloon driver inaccuracy into calculations *)

(** The minimum amount we will free by setting target = dynamic_min *)
let min_freeable ?(fistpoints=[]) domain = 
  if List.mem DisableInaccuracyCompensation fistpoints
  then max 0L (domain.memory_actual_kib -* domain.dynamic_min_kib)
  else max 0L (domain.memory_actual_kib -* domain.dynamic_min_kib -* 2L ** domain.inaccuracy_kib)
    (** The minimum amount we will allocate by setting target = dynamic_max *)
let min_allocatable domain = max 0L (domain.dynamic_max_kib -* domain.memory_actual_kib -* 2L ** domain.inaccuracy_kib)
  (** The range between dynamic_min and dynamic_max i.e. the total amount we may vary the balloon target
      NOT the total amount the memory_actual may vary. *)
let range domain = max 0L (domain.dynamic_max_kib -* domain.dynamic_min_kib)
  
module type POLICY = sig
  val compute_target_adjustments: ?fistpoints:(fistpoint list) -> host -> int64 -> (domain * int64) list
end

(**
	Represents an algorithm which attempts to (i) free memory; and (ii) balance
	memory between VMs on a host by setting balloon targets.
*)
module Proportional = struct

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
	  (* We allocate surplus memory in proportion to each domain's dynamic_range: *)
	  let allocate gamma domain = Int64.of_float (gamma *. (Int64.to_float (range domain))) in
	  (* gamma = the proportion where 0 <= gamma <= 1 *)
	  let gamma = constrain 0. 1. (Int64.to_float surplus_memory_kib /. (Int64.to_float (sum (List.map range domains)))) in
	  debug "total surplus memory = %Ld KiB; gamma = %.2f" surplus_memory_kib gamma;

	  List.map (fun domain -> domain, domain.dynamic_min_kib +* (allocate gamma domain)) domains


	(* Given a set of domains and a host free memory target, return balloon target adjustments *)
	let compute_target_adjustments ?fistpoints (host: host) host_target_kib =
	  (* If all domains balloon down to dynamic_min: *)
	  let maximum_free_mem_kib = host.free_mem_kib +* (sum (List.map (min_freeable ?fistpoints) host.domains)) in
	  let surplus_memory_kib = max 0L (maximum_free_mem_kib -* host_target_kib) in
	  allocate_memory_in_proportion surplus_memory_kib host.domains
end

module Policy = (Proportional: POLICY)

module Squeezer = struct

	(** State maintained between invocations of the algorithm function *)
	type t = {
		stuckness: Stuckness_monitor.t;
		non_active_domids: int list; (* domids are unique and constant *)
	}
	let make () = { stuckness = Stuckness_monitor.make (); non_active_domids = [] }

	(**
		Takes a view of the host state and amount of free memory desired and
		returns a list of ballooning actions which may help achieve the goal.
	*)
	let one_iteration ?(fistpoints=[]) success_condition (x: t) (host: host) host_target_kib (now: float) =
		(* 1. Compute which domains are still considered active *)
		Stuckness_monitor.update x.stuckness host now;
		let active_domains = 
		  List.filter (fun domain ->
				 domain.can_balloon
				 && (Stuckness_monitor.domid_is_active x.stuckness domain.domid now))
			host.domains in
		let non_active_domids = List.map (fun d -> d.domid) (set_difference host.domains active_domains) in
		
		let declared_inactive_domids = set_difference non_active_domids x.non_active_domids in
		let declared_active_domids = set_difference x.non_active_domids non_active_domids in

		List.iter
			(fun d ->
				debug "domid %d has been declared inactive" d
			) declared_inactive_domids;
		List.iter
			(fun d ->
				debug "domid %d has been declared active" d
			) declared_active_domids;

		let x = { x with non_active_domids = non_active_domids } in

		(* 2. Compute how we would adjust the domain memory targets *)
		let targets = Policy.compute_target_adjustments ~fistpoints { host with domains = active_domains } host_target_kib in

		let maximum_possible_free_memory_kib = host.free_mem_kib +* (sum (List.map (min_freeable ~fistpoints) active_domains)) in
		debug "maximum_possible_free_memory_kib = %Ld" maximum_possible_free_memory_kib;
		(* Xen heap workaround: *)
		let is_freeing_memory (domain, new_target_kib) = not (has_hit_target domain.inaccuracy_kib domain.memory_actual_kib new_target_kib) && new_target_kib < domain.memory_actual_kib in
		let freeing, allocating = List.partition is_freeing_memory targets in
		let allocation_phase = freeing = [] in
		let targets = 
		  if List.mem DisableTwoPhaseTargetSets fistpoints
		  then targets
		  else (if allocation_phase then allocating else freeing) in

		(* Have all the non-stuck domains reached their current targets? *)
		let all p xs = List.fold_left (&&) true (List.map p xs) in
		let hit_target domain = has_hit_target domain.inaccuracy_kib domain.memory_actual_kib domain.target_kib in
		let all_targets_reached = all hit_target active_domains in
		let min_target domain = domain.target_kib = domain.dynamic_min_kib in
		let max_target domain = domain.target_kib = domain.dynamic_max_kib in

		let cant_allocate_any_more = all max_target active_domains in
		let cant_free_any_more = all min_target active_domains in

		(* Note the asymmetry between:
		   1. increasing free memory: if we think we can't free enough then we give up
		   2. reducing free memory: we wait until as much as possible is allocated *)
		
		let success = success_condition host.free_mem_kib in

		let target_too_big = maximum_possible_free_memory_kib < host_target_kib in

		let no_target_changes = List.filter (fun (domain, target) -> domain.target_kib <> target) targets = [] in
		debug "free_mem = %Ld KiB; target = %Ld KiB; %s; all targets%s reached%s; %s"
		  host.free_mem_kib host_target_kib 
		  (if success then " OK"
		   else if target_too_big then " cannot free enough"
		   else if cant_allocate_any_more then " cannot allocate enough" else "")
		  (if all_targets_reached then "" else " not")
		  (if no_target_changes then "" else "; however about to adjust targets")
 		  (if allocation_phase then "allocation phase" else "freeing phase");

		(* If we have to blame a domain for being stuck, we don't blame it if it can't balloon. *)
		let non_active_domains = List.concat (List.map (fun d -> try [ IntMap.find d host.domid_to_domain ] with Not_found -> []) non_active_domids) in
		let non_active_can_balloon_domains = List.filter (fun d -> d.can_balloon) non_active_domains in
		let non_active_can_balloon_domids = List.map (fun d -> d.domid) non_active_can_balloon_domains in

		(* 1. In all cases we wait for all targets to be reached and to be stable.
		   2. If targets are reached and stable we evaluate our success condition and succeed/fail accordingly. *)
		let action = 
		  if not all_targets_reached || not no_target_changes
		  then AdjustTargets (List.map (fun (d, t) -> { action_domid = d.domid; new_target_kib = t}) targets)
		  else
		    if success then Success
		    else Failed non_active_can_balloon_domids in

		x, declared_active_domids, declared_inactive_domids, action

end

module Gnuplot = struct
	type colspec = Dynamic_min | Memory_actual | Dynamic_max | Target
	let write_header oc cols = 
	  let all = List.concat [ if List.mem Dynamic_min cols then [ "dynamic_min" ] else [];
				  if List.mem Dynamic_max cols then [ "dynamic_max" ] else [];
				  if List.mem Memory_actual cols then [ "memory_actual" ] else [];
				  if List.mem Target cols then [ "target" ] else [] ] in
	  Printf.fprintf oc "# for each domain: %s\n" (String.concat " " all)

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

type io = {
  make_host: unit -> string * host;
  domain_setmaxmem: int -> int64 -> unit;
  execute_action: action -> unit;
  wait: float -> unit;
  gettimeofday: unit -> float;

  target_host_free_mem_kib: int64;
  free_memory_tolerance_kib: int64;
}

exception Cannot_free_this_much_memory of int64 * int64 (** even if we balloon everyone down we can't free this much *)
exception Domains_refused_to_cooperate of int list (** these VMs didn't release memory and we failed *)

let change_host_free_memory ?fistpoints io required_mem_kib success_condition = 
  (* XXX: debugging *)
  debug "change_host_free_memory required_mem = %Ld KiB" required_mem_kib;

  let acc = ref (Squeezer.make ()) in
  let finished = ref false in
  while not (!finished) do
    let t = io.gettimeofday () in
    let host_debug_string, host = io.make_host () in
    let acc', declared_active_domids, declared_inactive_domids, result =
      Squeezer.one_iteration ?fistpoints success_condition !acc host required_mem_kib t in
    acc := acc';
    
    (* Set the max_mem of a domain as follows:

       If the VM has never been run && is paused -> use initial-reservation
       If the VM is active                       -> use target
       If the VM is inactive                     -> use min(target, actual)
       
       So active VMs may move up or down towards their target and either get there 
       (while we actively monitor them) or are declared inactive.
       Inactive VMs are allowed to free memory while we aren't looking but 
       they are not to allocate more.
       
       Note that the concept of having 'never been run' is hidden from us by the
       'make_host' function above. The data we receive here will show either an
       inactive (paused) domain with target=actual=initial_reservation or an
       active (unpaused) domain. So the we need only deal with 'active' vs 'inactive'.
    *)
    
    (* Compile a list of new targets *)
    let new_targets = match result with
      | AdjustTargets actions ->
	  List.map (fun a -> a.action_domid, a.new_target_kib) actions
      | _ -> [] in
    let new_target_direction domain = 
      let new_target = if List.mem_assoc domain.domid new_targets then Some(List.assoc domain.domid new_targets) else None in
      match new_target with
      | None -> string_of_direction None
      | Some t -> string_of_direction (direction_of_int64 domain.target_kib t) in
    let debug_string = String.concat "; " (host_debug_string :: (List.map (fun domain -> short_string_of_domain domain ^ (new_target_direction domain)) host.domains)) in
    debug "%s" debug_string;
    
    (* Deal with inactive and 'never been run' domains *)
    List.iter (fun domid -> 
		 try
		   let domain = IntMap.find domid host.domid_to_domain in
		   let mem_max_kib = min domain.target_kib domain.memory_actual_kib in
		   debug "Setting inactive domain %d mem_max = %Ld" domid mem_max_kib;
		   io.domain_setmaxmem domid mem_max_kib
		 with Not_found ->
		   debug "WARNING: inactive domain %d not in map" domid
	      ) declared_inactive_domids;
    (* Next deal with the active domains (which may have new targets) *)
    List.iter (fun domid ->
		 try
		   let domain = IntMap.find domid host.domid_to_domain in
		   let mem_max_kib = 
		     if List.mem_assoc domid new_targets 
		     then List.assoc domid new_targets 
		     else domain.target_kib in
		   debug "Setting active domain %d mem_max = %Ld" domid mem_max_kib;
		   io.domain_setmaxmem domid mem_max_kib
		 with Not_found ->
		   debug "WARNING: active domain %d not in map" domid
	      ) declared_active_domids;
    
    begin match result with
    | Success ->
	debug "Success: Host free memory = %Ld KiB" required_mem_kib;
	finished := true
    | Failed [] ->
	debug "Failed to free %Ld KiB of memory: operation impossible within current dynamic_min limits of balloonable domains" required_mem_kib;
	raise (Cannot_free_this_much_memory (required_mem_kib, host.free_mem_kib));
    | Failed domids ->
	let s = String.concat ", " (List.map string_of_int domids) in
	debug "Failed to free %Ld KiB of memory: the following domains have failed to meet their targets: [ %s ]"
	  required_mem_kib s;
	raise (Domains_refused_to_cooperate domids)
    | AdjustTargets actions ->
	(* Set all the balloon targets *)
	List.iter io.execute_action actions;
	io.wait 1.
    end
  done

let free_memory fistpoints io required_mem_kib = change_host_free_memory ?fistpoints io (required_mem_kib +* io.target_host_free_mem_kib) (fun x -> x >= (required_mem_kib +* io.target_host_free_mem_kib))

let free_memory_range ?fistpoints io min_kib max_kib =
  (* First compute the 'ideal' amount of free memory based on the proportional allocation policy *)
  let domain = { domid = -1;
		 can_balloon = true;
		 dynamic_min_kib = min_kib; dynamic_max_kib = max_kib;
		 target_kib = min_kib;
		 memory_actual_kib = 0L;
		 memory_max_kib = 0L;
		 inaccuracy_kib = 4L;
	       } in
  let host = snd(io.make_host ())in
  let host' = { host with domains = domain :: host.domains } in
  let adjustments = Policy.compute_target_adjustments host' io.target_host_free_mem_kib in
  let target = 
    if List.mem_assoc domain adjustments
    then List.assoc domain adjustments
    else min_kib in
  debug "free_memory_range ideal target = %Ld" target;

  change_host_free_memory ?fistpoints io (target +* io.target_host_free_mem_kib) (fun x -> x >= (min_kib +* io.target_host_free_mem_kib));
  let host = snd(io.make_host ()) in
  let usable_free_mem_kib = host.free_mem_kib -* io.target_host_free_mem_kib in
  if usable_free_mem_kib < min_kib then begin
    debug "WARNING usable_free_mem_kib (%Ld) < min_kib (%Ld) (difference = %Ld KiB)" usable_free_mem_kib min_kib (min_kib -* usable_free_mem_kib);
  end;
  max min_kib (min usable_free_mem_kib max_kib)

let is_balanced ?fistpoints io x = Int64.sub x io.target_host_free_mem_kib < io.free_memory_tolerance_kib

let balance_memory ?fistpoints io = 
  try
    change_host_free_memory ?fistpoints io io.target_host_free_mem_kib (is_balanced io);
  with e -> debug "balance memory caught: %s" (Printexc.to_string e)

(** Return true if the host memory is currently unbalanced and needs rebalancing *)
let is_host_memory_unbalanced ?fistpoints io = 
  let debug_string, host = io.make_host () in
  let domains = List.map (fun d -> d.domid, d) host.domains in

  let t = io.gettimeofday () in
  let _, _, _, result = Squeezer.one_iteration ?fistpoints
    (is_balanced io) (Squeezer.make ()) host 
    io.target_host_free_mem_kib (io.gettimeofday ()) in
  
  let is_new_target a = 
    let existing = (List.assoc a.action_domid domains).target_kib in
    a.new_target_kib <> existing in

  match result with
  | AdjustTargets ts ->
      if List.fold_left (||) false (List.map is_new_target ts) then begin
	debug "Memory is not currently balanced";
	debug "%s" debug_string;
	true
      end else false
  | _ -> false
  
