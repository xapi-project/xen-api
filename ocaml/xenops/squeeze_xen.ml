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
open Threadext
open Listext
open Stringext
open Xenstore
open Xenops_helpers
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

  let m = Mutex.create ()
  (* get_per_domain can return None if the domain is deleted by
	 someone else while we are processing some other event handlers *)
  let get_per_domain (xc, xs) domid = 
	  if Hashtbl.mem cache domid
	  then Some (Hashtbl.find cache domid)
	  else
		  try
			  let open Xenctrl.Domain_info in
			  let di = Xenctrl.domain_getinfo xc domid in
			  let maxmem = Xenctrl.pages_to_kib (Int64.of_nativeint di.max_memory_pages) in
			  let d = { path = xs.Xs.getdomainpath domid;
						hvm = di.hvm_guest;
						maxmem = maxmem;
						keys = Hashtbl.create 10 } in
			  Hashtbl.replace cache domid d;
			  Some d
		  with Xenctrl.Error _ ->
			  Hashtbl.remove cache domid;
			  None

  module MemoryActions = struct
	let interesting_paths = [
	        [ "memory";  "initial-reservation" ];
	        [ "memory";  "target" ];
	        [ "control"; "feature-balloon" ];
	        [ "data";    "updated" ];
	        [ "memory";  "memory-offset" ];
	        [ "memory";  "uncooperative" ];
	        [ "memory";  "dynamic-min" ];
	        [ "memory";  "dynamic-max" ];
	]

	let interesting_paths_for_domain domid uuid = 
		List.map (fun p -> Printf.sprintf "/local/domain/%d/%s" domid (String.concat "/" p)) interesting_paths

	let unmanaged_domain _ _ = false

	let found_running_domain _ _ = ()

	let domain_disappeared xc xs domid =
		Mutex.execute m
			(fun () ->
				Hashtbl.remove cache domid
			)

	let domain_appeared xc xs domid =
		Mutex.execute m
			(fun () ->
				try ignore(get_per_domain (xc, xs) domid) with _ -> ()
			)

	let watch_fired xc xs path domains watches =
		match List.filter (fun x -> x <> "") (String.split '/' path) with
			| "local" :: "domain" :: domid :: rest when List.mem rest interesting_paths ->
				let value = try Some (xs.Xs.read path) with _ -> None in
				let domid = int_of_string domid in
				Mutex.execute m
					(fun () ->
						match get_per_domain (xc, xs) domid with
						| None -> ()
						| Some per_domain ->
							let key = "/" ^ (String.concat "/" rest) in
							debug "watch %s <- %s" key (Opt.default "None" value);
							Hashtbl.replace per_domain.keys key value
					)
			| _  -> debug "Ignoring unexpected watch: %s" path
  end

  module Watcher = Xenstore_watch.WatchXenstore(MemoryActions)

  let start_watch_xenstore_thread () = ignore (Watcher.create_watcher_thread ())

  let get_hvm cnx domid =
	  Mutex.execute m (fun () ->
		  Opt.default false (Opt.map (fun d -> d.hvm) (get_per_domain cnx domid))
	  )

  let get_maxmem cnx domid =
	  Mutex.execute m (fun () ->
		  Opt.default 0L (Opt.map (fun d -> d.maxmem) (get_per_domain cnx domid))
	  )

  let set_maxmem_noexn (xc, xs) domid mem = 
	  Mutex.execute m
		  (fun () ->
			  match get_per_domain (xc, xs) domid with
			  | None -> ()
			  | Some per_domain ->
				  if per_domain.maxmem <> mem then begin
					  debug "Xenctrl.domain_setmaxmem domid=%d max=%Ld (was=%Ld)" domid mem per_domain.maxmem;
					  try
						  Xenctrl.domain_setmaxmem xc domid mem;
						  per_domain.maxmem <- mem
					  with e ->
						  error "Xenctrl.domain_setmaxmem domid=%d max=%Ld: (was=%Ld) %s"
							  domid mem per_domain.maxmem (Printexc.to_string e)
				  end
		  )

  (** Read a particular domain's key, using the cache *)
  let read (xc, xs) domid key = 
	  let x = Mutex.execute m
		  (fun () ->
			  match get_per_domain (xc, xs) domid with
			  | None -> None
			  | Some per_domain ->
				  if Hashtbl.mem per_domain.keys key
				  then Hashtbl.find per_domain.keys key
				  else begin
					  let x =
						  try Some (xs.Xs.read (per_domain.path ^ key))
						  with Xenbus.Xb.Noent -> None in
					  Hashtbl.replace per_domain.keys key x;
					  x
				  end) in
	match x with
	| Some y -> y
	| None ->
		  raise Xenbus.Xb.Noent

  (** Write a new (key, value) pair into a domain's directory in xenstore. Don't write anything
	  if the domain's directory doesn't exist. Don't throw exceptions. *)
  let write_noexn (xc, xs) domid key value = 
	match get_per_domain (xc, xs) domid with
	| None -> ()
	| Some per_domain ->
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
	match get_per_domain (xc, xs) domid with
	| None -> ()
	| Some per_domain ->
		Hashtbl.replace per_domain.keys key None;
		try xs.Xs.rm (per_domain.path ^ key)
		with e -> error "xenstore-rm %d %s: %s" domid key (Printexc.to_string e)

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

  (** Return true if feature_balloon has been advertised *)
  let get_feature_balloon cnx domid = try ignore(read cnx domid _feature_balloon); true with Xenbus.Xb.Noent -> false

  (** Return true if the guest agent has been advertised *)
  let get_guest_agent cnx domid = try ignore(read cnx domid _data_updated); true with Xenbus.Xb.Noent -> false

  (** Query a domain's initial reservation. Throws Xenbus.Xb.Noent if the domain has been destroyed *)
  let get_initial_reservation cnx domid = 
	Int64.of_string (read cnx domid _initial_reservation)

  (** Query a domain's dynamic_min. Throws Xenbus.Xb.Noent if the domain has been destroyed *)
  let get_dynamic_min cnx domid = 
	Int64.of_string (read cnx domid _dynamic_min)

  (** Query a domain's dynamic_max. Throws Xenbus.Xb.Noent if the domain has been destroyed *)
  let get_dynamic_max cnx domid = 
	Int64.of_string (read cnx domid _dynamic_max)
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
	while Int64.div ((Xenctrl.physinfo xc).Xenctrl.Phys_info.scrub_pages |> Int64.of_nativeint) 1024L <> 0L do
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

	(* We are excluding dom0, so it will never be ballooned down. *)
	let open Xenctrl.Domain_info in
	let domain_infolist = List.filter (fun di -> di.domid > 0) (Xenctrl.domain_getinfolist xc 0) in
	(*
		For the host free memory we sum the free pages and the pages needing
		scrubbing: we don't want to adjust targets simply because the scrubber
		is slow.
	*)
	let physinfo = Xenctrl.physinfo xc in
	let free_pages_kib = Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.Phys_info.free_pages)
	and scrub_pages_kib = Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.Phys_info.scrub_pages)
	and total_pages_kib = Xenctrl.pages_to_kib (Int64.of_nativeint physinfo.Xenctrl.Phys_info.total_pages) in
	let free_mem_kib = free_pages_kib +* scrub_pages_kib in

	let cnx = xc, xs in
	let domains = List.concat
		(List.map
			(fun di ->
				try
					let memory_actual_kib = Xenctrl.pages_to_kib (Int64.of_nativeint di.total_memory_pages) in
					let memory_shadow_kib =
						if di.hvm_guest then
							try
								Memory.kib_of_mib (Int64.of_int (Xenctrl.shadow_allocation_get xc di.domid))
							with _ -> 0L
						else 0L in
					(* dom0 is special for some reason *)
					let memory_max_kib = if di.domid = 0 then 0L else Xenctrl.pages_to_kib (Int64.of_nativeint di.max_memory_pages) in
					(* Misc other stuff appears in max_memory_pages *)
					let memory_max_kib = max 0L (memory_max_kib -* (xen_max_offset_kib di.hvm_guest)) in
					let can_balloon = Domain.get_feature_balloon cnx di.domid in
					let has_guest_agent = Domain.get_guest_agent cnx di.domid in
					let has_booted = can_balloon || has_guest_agent in
					(* Once the domain tells us it has booted, we assume it's not currently ballooning and
					   record the offset between memory_actual and target. We assume this is constant over the 
					   lifetime of the domain. *)
					let offset_kib : int64 = 
					  if not has_booted then 0L
					  else begin
					    try
					      Domain.get_memory_offset cnx di.domid
					    with Xenbus.Xb.Noent ->
							(* Our memory_actual_kib value was sampled before reading xenstore which means there is a slight race.
							   The race is probably only noticable in the hypercall simulator. However we can fix it by resampling
							   memory_actual *after* noticing the feature-balloon flag. *)
							let target_kib = Domain.get_target cnx di.domid in
							let memory_actual_kib' = Xenctrl.pages_to_kib (Int64.of_nativeint (Xenctrl.domain_getinfo xc di.domid).total_memory_pages) in
							let offset_kib = memory_actual_kib' -* target_kib in
							debug "domid %d just %s; calibrating memory-offset = %Ld KiB" di.domid
								(match can_balloon, has_guest_agent with
									| true, false -> "advertised a balloon driver"
									| true, true -> "started a guest agent and advertised a balloon driver"
									| false, true -> "started a guest agent (but has no balloon driver)"
									| false, false -> "N/A" (*impossible: see if has_booted above *)
							) offset_kib;
							Domain.set_memory_offset_noexn cnx di.domid offset_kib;
							offset_kib
					  end in
					let memory_actual_kib = memory_actual_kib -* offset_kib in

					let domain = 
					  { Squeeze.
						domid = di.domid;
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
						let initial_reservation_kib = Domain.get_initial_reservation cnx di.domid in
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

						let target_kib = Domain.get_target cnx di.domid in
						(* min and max are written separately; if we notice they *)
						(* are missing set them both to the target for now.      *)
						let min_kib, max_kib =
						try
						  Domain.get_dynamic_min cnx di.domid,
						  Domain.get_dynamic_max cnx di.domid
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
						di.domid (Printexc.to_string e);
					[]
			)
			domain_infolist
		) in

	(* Sum up the 'reservations' which exist separately from domains *)
	let non_domain_reservations = Squeezed_state.total_reservations xs Squeezed_state._service in
	if verbose && non_domain_reservations <> 0L
	then debug "Total non-domain reservations = %Ld" non_domain_reservations;
	reserved_kib := Int64.add !reserved_kib non_domain_reservations;

	let host = Squeeze.make_host ~domains ~free_mem_kib:(Int64.sub free_mem_kib !reserved_kib) in

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
  
