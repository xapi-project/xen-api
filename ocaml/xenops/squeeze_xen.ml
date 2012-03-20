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
	Interface between the abstract domain memory balancing code and Xen.
*)
(*
	Aims are:
	1. make this code robust to domains being created and destroyed around it.
	2. not depend on any other info beyond domain_getinfolist and xenstore.
*)
open Pervasiveext
open Xenstore
open Fun

module M = Debug.Debugger(struct let name = "memory" end)
let debug = Squeeze.debug
let error = Squeeze.error 

let _initial_reservation = "/memory/initial-reservation"  (* immutable *)
let _target              = "/memory/target"               (* immutable *)
let _feature_balloon     = "/control/feature-balloon"     (* immutable *)
let _data_updated        = "/data/updated"                (* mutable: written by guest agent *)
let _memory_offset       = "/memory/memory-offset"        (* immutable *)
let _uncooperative       = "/memory/uncooperative"        (* mutable: written by us *)

let _dynamic_min         = "/memory/dynamic-min"          (* mutable: written by domain manager *)
let _dynamic_max         = "/memory/dynamic-max"          (* mutable: written by domain manager *)

let ( ** ) = Int64.mul
let ( +* ) = Int64.add
let ( -* ) = Int64.sub
let mib = 1024L

let set_difference a b = List.filter (fun x -> not (List.mem x b)) a

let low_mem_emergency_pool = 1L ** mib (** Same as xen commandline *)

(** Return the extra amount we always add onto maxmem *) 
let xen_max_offset_kib hvm =
  let maxmem_mib = if hvm then Memory.HVM.xen_max_offset_mib else Memory.Linux.xen_max_offset_mib in
  Memory.kib_of_mib maxmem_mib

(** Cache of per-domain info, to avoid repeated per-domain queries *)
module Domain = struct
  type per_domain = { path: string;
					  hvm: bool;
					  mutable maxmem: int64;
					  keys: (string, string option) Hashtbl.t }
  type t = (int, per_domain) Hashtbl.t
	  
  let cache = Hashtbl.create 10
  let get_per_domain (xc, xs) domid = 
	if Hashtbl.mem cache domid
	then Hashtbl.find cache domid
	else 
	  let di = Xenctrl.domain_getinfo xc domid in
      let maxmem = Xenctrl.pages_to_kib (Int64.of_nativeint di.Xenctrl.max_memory_pages) in
	  let d = { path = xs.Xs.getdomainpath domid;
				hvm = di.Xenctrl.hvm_guest;
				maxmem = maxmem;
				keys = Hashtbl.create 10 } in
	  Hashtbl.replace cache domid d;
	  d
  let get_hvm cnx domid = (get_per_domain cnx domid).hvm
  let get_maxmem cnx domid = (get_per_domain cnx domid).maxmem
  let set_maxmem_noexn (xc, xs) domid m = 
	let per_domain = get_per_domain (xc, xs) domid in
	if per_domain.maxmem <> m then begin
		debug "Xenctrl.domain_setmaxmem domid=%d max=%Ld (was=%Ld)" domid m per_domain.maxmem;
	  try
		Xenctrl.domain_setmaxmem xc domid m;
		per_domain.maxmem <- m
	  with e ->
		  error "Xenctrl.domain_setmaxmem domid=%d max=%Ld: (was=%Ld) %s" domid m per_domain.maxmem (Printexc.to_string e)
	end

  (** Read a particular domain's key, using the cache *)
  let read (xc, xs) domid key = 
	let per_domain = get_per_domain (xc, xs) domid in
	let x = 
	  if Hashtbl.mem per_domain.keys key
	  then Hashtbl.find per_domain.keys key
	  else begin
		let x = (try Some (xs.Xs.read (per_domain.path ^ key))
				 with Xenbus.Xb.Noent -> None) in
		Hashtbl.replace per_domain.keys key x;
		x
	  end in
	match x with
	| Some y -> y
	| None ->
		  raise Xenbus.Xb.Noent

  (** Read a particular domain's key from xenstore, for when we believe the cache is out of date *)
  let read_nocache (xc, xs) domid key = 
	let per_domain = get_per_domain (xc, xs) domid in
	if Hashtbl.mem per_domain.keys key then Hashtbl.remove per_domain.keys key;
	read (xc, xs) domid key
  (** Write a new (key, value) pair into a domain's directory in xenstore. Don't write anything
	  if the domain's directory doesn't exist. Don't throw exceptions. *)

  let write_noexn (xc, xs) domid key value = 
	let per_domain = get_per_domain (xc, xs) domid in
	if not(Hashtbl.mem per_domain.keys key) || Hashtbl.find per_domain.keys key <> (Some value) then begin
	  try
		Xs.transaction xs
			(fun t ->
				 (* Fail if the directory has been deleted *)
				 ignore (xs.Xs.read per_domain.path);
				 t.Xst.write (per_domain.path ^ key) value
			);
		  Hashtbl.replace per_domain.keys key (Some value);
	  with e ->
		  (* Log but don't throw an exception *)
		  error "xenstore-write %d %s = %s failed: %s" domid key value (Printexc.to_string e)
	end
  (** Returns true if the key exists, false otherwise *)
  let exists (xc, xs) domid key = try ignore(read (xc, xs) domid key); true with Xenbus.Xb.Noent -> false
  (** Delete the key. Don't throw exceptions. *)
  let rm_noexn (xc, xs) domid key = 
	let per_domain = get_per_domain (xc, xs) domid in
	Hashtbl.replace per_domain.keys key None;
	try
	  xs.Xs.rm (per_domain.path ^ key)
	with e ->
		error "xenstore-rm %d %s: %s" domid key (Printexc.to_string e)

  (** {High-level functions} *)

  (** Set a domain's memory target. Don't throw an exception if the domain has been destroyed. *)
  let set_target_noexn cnx domid target_kib = 
	write_noexn cnx domid _target (Int64.to_string target_kib)

  (** Get a domain's memory target. Throws Xenbus.Xb.Noent if the domain has been destroyed *)
  let get_target cnx domid = 
	Int64.of_string (read cnx domid _target)

  (** Mark a domain as uncooperative. Don't throw an exception if the domain has been destroyed. *)
  let set_uncooperative_noexn cnx domid x =
	if x
	then write_noexn cnx domid _uncooperative ""
	else rm_noexn cnx domid _uncooperative

  (** Query a domain's uncooperative status. Throws Xenbus.Xb.Noent if the domain has been destroyed *)
  let get_uncooperative_noexn cnx domid = exists cnx domid _uncooperative

  (** Set a domain's memory-offset. Don't throw an exception if the domain has been destroyed *)
  let set_memory_offset_noexn cnx domid offset_kib =
	write_noexn cnx domid _memory_offset (Int64.to_string offset_kib)

  (** Query a domain's memory-offset. Throws Xenbus.Xb.Noent if the domain has been destroyed *)
  let get_memory_offset cnx domid =
	Int64.of_string (read cnx domid _memory_offset)

  (** Set a domain's maxmem. Don't throw an exception if the domain has been destroyed *)
  let set_maxmem_noexn cnx domid target_kib = 
	let maxmem_kib = xen_max_offset_kib (get_hvm cnx domid) +* target_kib in
	set_maxmem_noexn cnx domid maxmem_kib

  let get_xenstore_key k (xc, xs) domid = 
	(* Cache the presence of this key when it appears but always read through when it hasn't yet *)
	let per_domain = get_per_domain (xc, xs) domid in
	if Hashtbl.mem per_domain.keys k && Hashtbl.find per_domain.keys k <> None
	then true
	else (try ignore (read_nocache (xc, xs) domid k); true with Xenbus.Xb.Noent -> false)

  (** Return true if feature_balloon has been advertised *)
  let get_feature_balloon = get_xenstore_key _feature_balloon

  (** Return true if the guest agent has been advertised *)
  let get_guest_agent = get_xenstore_key _data_updated

  let get_guest_agent (xc, xs) domid =
	(* Cache the presence of this key when it appears but always read through when it hasn't yet *)
	let per_domain = get_per_domain (xc, xs) domid in
	if Hashtbl.mem per_domain.keys _data_updated && Hashtbl.find per_domain.keys _data_updated <> None
	then true
	else (try ignore (read_nocache (xc, xs) domid _data_updated); true with Xenbus.Xb.Noent -> false)	  

  (** Query a domain's initial reservation. Throws Xenbus.Xb.Noent if the domain has been destroyed *)
  let get_initial_reservation cnx domid = 
	Int64.of_string (read cnx domid _initial_reservation)

  (** Query a domain's dynamic_min. Throws Xenbus.Xb.Noent if the domain has been destroyed *)
  let get_dynamic_min cnx domid = 
	Int64.of_string (read_nocache cnx domid _dynamic_min)

  (** Query a domain's dynamic_max. Throws Xenbus.Xb.Noent if the domain has been destroyed *)
  let get_dynamic_max cnx domid = 
	Int64.of_string (read_nocache cnx domid _dynamic_max)

  (** Expire old domain information from the cache *)
  let gc live_domids = 
	let cached_domids = Hashtbl.fold (fun d _ ds -> d::ds) cache [] in
	let to_remove = set_difference cached_domids live_domids in
	List.iter (Hashtbl.remove cache) to_remove
end



(** Record when the domain was last co-operative *)
let when_domain_was_last_cooperative : (int, float) Hashtbl.t = Hashtbl.create 10

let update_cooperative_table host = 
  let now = Unix.gettimeofday () in
  let alive_domids = List.map (fun d -> d.Squeeze.domid) host.Squeeze.domains in
  let known_domids = Hashtbl.fold (fun k _ acc -> k :: acc) when_domain_was_last_cooperative [] in
  let gone_domids = set_difference known_domids alive_domids in
  List.iter (Hashtbl.remove when_domain_was_last_cooperative) gone_domids;
  let arrived_domids = set_difference alive_domids known_domids in
  (* Assume domains are initially co-operative *)
  List.iter (fun x -> Hashtbl.replace when_domain_was_last_cooperative x now) arrived_domids;
  (* Main business: mark any domain which is on or above target OR which cannot balloon as co-operative *)
  List.iter (fun d -> 
	       if not d.Squeeze.can_balloon || (Squeeze.has_hit_target d.Squeeze.inaccuracy_kib d.Squeeze.memory_actual_kib d.Squeeze.target_kib)
	       then Hashtbl.replace when_domain_was_last_cooperative d.Squeeze.domid now
	    ) host.Squeeze.domains

(** Update all the flags in xenstore *)
let update_cooperative_flags cnx = 
  let now = Unix.gettimeofday () in
  Hashtbl.iter (fun domid last_time ->
		  let old_value = Domain.get_uncooperative_noexn cnx domid in
		  let new_value = now -. last_time > 20. in
		  if old_value <> new_value then Domain.set_uncooperative_noexn cnx domid new_value
	       )
    when_domain_was_last_cooperative
		  

(** Best-effort creation of a 'host' structure and a simple debug line showing its derivation *)
let make_host ~verbose ~xc ~xs =
	(* Wait for any scrubbing so that we don't end up with no immediately usable pages --
	   this might cause something else to fail (eg domain builder?) *)
	while Int64.div ((Xenctrl.physinfo xc).Xenctrl.scrub_pages |> Int64.of_nativeint) 1024L <> 0L do
		ignore(Unix.select [] [] [] 0.25)
	done;

	(* Some VMs are considered by us (but not by xen) to have an "initial-reservation". For VMs which have never 
	   run (eg which are still being built or restored) we take the difference between memory_actual_kib and the
	   reservation and subtract this manually from the host's free memory. Note that we don't get an atomic snapshot
	   of system state so there is a natural race between the hypercalls. Hopefully the memory is being consumed
	   fairly slowly and so the error is small. *)
  
	(* Additionally we have the concept of a 'reservation' separate from a domain which allows us to postpone
	   domain creates until such time as there is lots of memory available. This minimises the chance that the
	   remaining free memory will be too fragmented to actually use (some xen structures require contiguous frames) *)
  
	let reserved_kib = ref 0L in

	(* We cannot query simultaneously the host memory info and the domain memory info. Furthermore
	   the call to domain_getinfolist is not atomic but comprised of many hypercall invocations. *)

	let domain_infolist = Xenctrl.domain_getinfolist xc 0 in
	(*
		For the host free memory we sum the free pages and the pages needing
		scrubbing: we don't want to adjust targets simply because the scrubber
		is slow.
	*)
	let physinfo = Xenctrl.physinfo xc in
	let free_pages_kib = Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.free_pages)
	and scrub_pages_kib = Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.scrub_pages) 
	and total_pages_kib = Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.total_pages) in
	let free_mem_kib = free_pages_kib +* scrub_pages_kib in

	let cnx = xc, xs in
	let domains = List.concat
		(List.map
			(fun di ->
				try
					let memory_actual_kib = Xenctrl.pages_to_kib (Int64.of_nativeint di.Xenctrl.total_memory_pages) in
					let memory_shadow_kib =
						if di.Xenctrl.hvm_guest then
							try
								Memory.kib_of_mib (Int64.of_int (Xenctrl.shadow_allocation_get xc di.Xenctrl.domid))
							with _ -> 0L
						else 0L in
					(* dom0 is special for some reason *)
					let memory_max_kib = if di.Xenctrl.domid = 0 then 0L else Xenctrl.pages_to_kib (Int64.of_nativeint di.Xenctrl.max_memory_pages) in
					(* Misc other stuff appears in max_memory_pages *)
					let memory_max_kib = max 0L (memory_max_kib -* (xen_max_offset_kib di.Xenctrl.hvm_guest)) in
					let can_balloon = Domain.get_feature_balloon cnx di.Xenctrl.domid in
					let has_guest_agent = Domain.get_guest_agent cnx di.Xenctrl.domid in
					let has_booted = can_balloon || has_guest_agent in
					(* Once the domain tells us it has booted, we assume it's not currently ballooning and
					   record the offset between memory_actual and target. We assume this is constant over the 
					   lifetime of the domain. *)
					let offset_kib : int64 = 
					  if not has_booted then 0L
					  else begin
					    try
					      Domain.get_memory_offset cnx di.Xenctrl.domid
					    with Xenbus.Xb.Noent ->
							(* Our memory_actual_kib value was sampled before reading xenstore which means there is a slight race.
							   The race is probably only noticable in the hypercall simulator. However we can fix it by resampling
							   memory_actual *after* noticing the feature-balloon flag. *)
							let target_kib = Domain.get_target cnx di.Xenctrl.domid in
							let memory_actual_kib' = Xenctrl.pages_to_kib (Int64.of_nativeint (Xenctrl.domain_getinfo xc di.Xenctrl.domid).Xenctrl.total_memory_pages) in
							let offset_kib = memory_actual_kib' -* target_kib in
							debug "domid %d just %s; calibrating memory-offset = %Ld KiB" di.Xenctrl.domid
								(match can_balloon, has_guest_agent with
									| true, false -> "advertised a balloon driver"
									| true, true -> "started a guest agent and advertised a balloon driver"
									| false, true -> "started a guest agent (but has no balloon driver)"
									| false, false -> "N/A" (*impossible: see if has_booted above *)
							) offset_kib;
							Domain.set_memory_offset_noexn cnx di.Xenctrl.domid offset_kib;
							offset_kib
					  end in
					let memory_actual_kib = memory_actual_kib -* offset_kib in

					let domain = 
					  { Squeeze.
						domid = di.Xenctrl.domid;
						can_balloon = can_balloon;
						dynamic_min_kib = 0L;
						dynamic_max_kib = 0L;
						target_kib = 0L;
						memory_actual_kib = 0L;
						memory_max_kib = memory_max_kib;
						inaccuracy_kib = 4L;
					  } in
					
					(* If the domain has never run (detected by being paused, not shutdown and clocked up no CPU time)
					   then we'll need to consider the domain's "initial-reservation". Note that the other fields
					   won't necessarily have been created yet. *)

					(* If the domain has yet to boot properly then we assume it is using at least its
					   "initial-reservation". *)
					if not has_booted then begin
						let initial_reservation_kib = Domain.get_initial_reservation cnx di.Xenctrl.domid in
						let unaccounted_kib = max 0L
							(initial_reservation_kib -* memory_actual_kib -* memory_shadow_kib) in
						reserved_kib := Int64.add !reserved_kib unaccounted_kib;
						[ { domain with Squeeze.
							dynamic_min_kib   = memory_max_kib;
							dynamic_max_kib   = memory_max_kib;
							target_kib        = memory_max_kib;
							memory_actual_kib = memory_max_kib;
						} ]
					end else begin

						let target_kib = Domain.get_target cnx di.Xenctrl.domid in
						(* min and max are written separately; if we notice they *)
						(* are missing set them both to the target for now.      *)
						let min_kib, max_kib =
						try
						  Domain.get_dynamic_min cnx di.Xenctrl.domid,
						  Domain.get_dynamic_max cnx di.Xenctrl.domid
						with _ ->
							target_kib, target_kib
						in
						 [ { domain with Squeeze.
						      dynamic_min_kib = min_kib;
						      dynamic_max_kib = max_kib;
						      target_kib = target_kib;
						      memory_actual_kib = memory_actual_kib
						  } ]
					end
				with
				| Xenbus.Xb.Noent ->
				    (* useful debug message is printed by the Domain.read* functions *)
				    []
				|  e ->
					if verbose 
					then debug "Skipping domid %d: %s"
						di.Xenctrl.domid (Printexc.to_string e);
					[]
			)
			domain_infolist
		) in

	(* Sum up the 'reservations' which exist separately from domains *)
	let non_domain_reservations = Squeezed_state.total_reservations xs Squeezed_rpc._service in
	if verbose && non_domain_reservations <> 0L
	then debug "Total non-domain reservations = %Ld" non_domain_reservations;
	reserved_kib := Int64.add !reserved_kib non_domain_reservations;

	let host = Squeeze.make_host ~domains ~free_mem_kib:(Int64.sub free_mem_kib !reserved_kib) in
	Domain.gc (List.map (fun di -> di.Xenctrl.domid) domain_infolist);

	(* Externally-visible side-effects. It's a bit ugly to include these here: *)
	update_cooperative_table host;
	update_cooperative_flags cnx;

	(* It's always safe to _decrease_ a domain's maxmem towards target. This catches the case
	   where a toolstack creates a domain with maxmem = static_max and target < static_max (eg
	   CA-36316) *)
	let updates = Squeeze.IntMap.fold (fun domid domain updates ->
										   if domain.Squeeze.target_kib < (Domain.get_maxmem (xc, xs) domid)
										   then Squeeze.IntMap.add domid domain.Squeeze.target_kib updates
										   else updates) host.Squeeze.domid_to_domain Squeeze.IntMap.empty in
	Squeeze.IntMap.iter (Domain.set_maxmem_noexn (xc, xs)) updates;

	Printf.sprintf "F%Ld S%Ld R%Ld T%Ld" free_pages_kib scrub_pages_kib !reserved_kib total_pages_kib, host

(** Best-effort update of a domain's memory target. *)
let execute_action ~xc ~xs action =
	try
		let domid = action.Squeeze.action_domid in
		let target_kib = action.Squeeze.new_target_kib in
		if target_kib < 0L
		then failwith (Printf.sprintf "Proposed target is negative (domid %d): %Ld" domid target_kib);
		let cnx = (xc, xs) in
		let memory_max_kib = Domain.get_maxmem cnx domid in
		(* We only set the target of a domain if it has exposed feature-balloon: otherwise
		   we can screw up the memory-offset calculations for partially-built domains. *)
		let can_balloon = Domain.get_feature_balloon cnx domid in
		if target_kib > memory_max_kib then begin
		  Domain.set_maxmem_noexn cnx domid target_kib;
		  if can_balloon
		  then Domain.set_target_noexn cnx domid target_kib
		  else debug "Not setting target for domid: %d since no feature-balloon. Setting maxmem to %Ld" domid target_kib;
		end else begin
		  if can_balloon
		  then Domain.set_target_noexn cnx domid target_kib
		  else debug "Not setting target for domid: %d since no feature-balloon. Setting maxmem to %Ld" domid target_kib;
		  Domain.set_maxmem_noexn cnx domid target_kib;
		end
	with e ->
		debug "Failed to reset balloon target (domid: %d) (target: %Ld): %s"
			action.Squeeze.action_domid action.Squeeze.new_target_kib
			(Printexc.to_string e)

let extra_mem_to_keep = 8L ** mib (** Domain.creates take "ordinary" memory as well as "special" memory *)

let target_host_free_mem_kib = low_mem_emergency_pool +* extra_mem_to_keep

let free_memory_tolerance_kib = 512L (** No need to be too exact *)

let io ~xc ~xs ~verbose = {
	verbose = verbose;
  Squeeze.gettimeofday = Unix.gettimeofday;
  make_host = (fun () -> make_host ~verbose ~xc ~xs);
  domain_setmaxmem = (fun domid kib -> execute_action ~xc ~xs { Squeeze.action_domid = domid; new_target_kib = kib });
  wait = (fun delay -> ignore(Unix.select [] [] [] delay));
  execute_action = (fun action -> execute_action ~xc ~xs action);
  target_host_free_mem_kib = target_host_free_mem_kib;
  free_memory_tolerance_kib = free_memory_tolerance_kib;
}

let change_host_free_memory ~xc ~xs required_mem_kib success_condition = 
  Squeeze.change_host_free_memory (io ~verbose:true ~xc ~xs) required_mem_kib success_condition

let free_memory ~xc ~xs required_mem_kib = 
  let io = io ~verbose:true ~xc ~xs in
  Squeeze.change_host_free_memory io (required_mem_kib +* io.Squeeze.target_host_free_mem_kib) (fun x -> x >= (required_mem_kib +* io.Squeeze.target_host_free_mem_kib))

let free_memory_range ~xc ~xs min_kib max_kib =
  let io = io ~verbose:true ~xc ~xs in
  Squeeze.free_memory_range io min_kib max_kib

let is_balanced x = Int64.sub x target_host_free_mem_kib < free_memory_tolerance_kib

let balance_memory ~xc ~xs = 
  Squeeze.balance_memory (io ~verbose:true ~xc ~xs)

(** Return true if the host memory is currently unbalanced and needs rebalancing *)
let is_host_memory_unbalanced ~xc ~xs = 
  Squeeze.is_host_memory_unbalanced (io ~verbose:false ~xc ~xs)
  
