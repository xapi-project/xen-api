(**
	Interface between the abstract domain memory balancing code and Xen.
*)
(*
	Aims are:
	1. make this code robust to domains being created and destroyed around it.
	2. not depend on any other info beyond domain_getinfolist and xenstore.
*)
open Pervasiveext

module M = Debug.Debugger(struct let name = "memory" end)
let debug = Squeeze.debug
let error = Squeeze.error 

(** We define a domain which is paused, not shutdown and has not clocked up any CPU cycles
    as 'never_been_run' *)
let never_been_run di = di.Xc.paused && not di.Xc.shutdown && di.Xc.cpu_time = 0L

let initial_reservation_path dom_path = dom_path ^ "/memory/initial-reservation"
let target_path              dom_path = dom_path ^ "/memory/target"
let dynamic_min_path         dom_path = dom_path ^ "/memory/dynamic-min"
let dynamic_max_path         dom_path = dom_path ^ "/memory/dynamic-max"
let feature_balloon_path     dom_path = dom_path ^ "/control/feature-balloon"
let memory_offset_path       dom_path = dom_path ^ "/memory/memory-offset"
let uncooperative_path       dom_path = dom_path ^ "/memory/uncooperative"

let ( ** ) = Int64.mul
let ( +* ) = Int64.add
let ( -* ) = Int64.sub
let mib = 1024L

let set_difference a b = List.filter (fun x -> not (List.mem x b)) a

let low_mem_emergency_pool = 1L ** mib (** Same as xen commandline *)

let exists xs path = try ignore(xs.Xs.read path); true with Xb.Noent -> false

let xs_read xs path = try xs.Xs.read path with Xb.Noent as e -> begin debug "xenstore-read %s returned ENOENT" path; raise e end

(** Return the extra amount we always add onto maxmem *) 
let xen_max_offset_kib di =
  let maxmem_mib = if di.Xc.hvm_guest then Memory.HVM.xen_max_offset_mib else Memory.Linux.xen_max_offset_mib in
  Memory.kib_of_mib maxmem_mib

let domain_setmaxmem_noexn xc domid target_kib = 
  let di = Xc.domain_getinfo xc domid in
  let maxmem_kib = xen_max_offset_kib di +* target_kib in
  try
    let existing_maxmem_kib = Xc.pages_to_kib (Int64.of_nativeint di.Xc.max_memory_pages) in
    if existing_maxmem_kib <> maxmem_kib then begin
      debug "Xc.domain_setmaxmem domid=%d target=%Ld max=%Ld" domid target_kib maxmem_kib;      
      Xc.domain_setmaxmem xc domid maxmem_kib
    end
  with e ->
    (* someone probably just destroyed the domain *)
    error "Xc.domain_setmaxmem domid=%d max=%Ld: %s" domid maxmem_kib (Printexc.to_string e)
      
let set_target_noexn xs dom_path target_kib = 
  let path = target_path dom_path in
  try
    Xs.transaction xs
      (fun t ->
	 (* make sure no-one deletes the tree *)
	 let existing_target_kib = Int64.of_string (t.Xst.read path) in
	 if existing_target_kib <> target_kib then begin
	   debug "xenstore-write %s = %Ld" path target_kib;
	   t.Xst.write path (Int64.to_string target_kib)
	 end
      )
  with e ->
    (* someone probably just destroyed the domain *)
    error "xenstore-write %s = %Ld: %s" path target_kib (Printexc.to_string e)

let set_uncooperative_noexn xs dom_path x =
  try
    if x 
    then 
      Xs.transaction xs
	(fun t ->
	   (* make sure no-one deletes the tree *)
	   ignore (t.Xst.read dom_path);
	   t.Xst.write (uncooperative_path dom_path) ""
	)
    else xs.Xs.rm (uncooperative_path dom_path)
  with e ->
    error "set_uncooperative %s %b: %s" dom_path x (Printexc.to_string e)

let get_uncooperative_noexn xs dom_path = exists xs (uncooperative_path dom_path)

let set_memory_offset_noexn xs dom_path offset_kib =
  try
    Xs.transaction xs
      (fun t ->
	 (* Make sure the domain still exists *)
	 ignore (xs.Xs.read dom_path);
	 t.Xst.write (memory_offset_path dom_path) (Int64.to_string offset_kib)
      )
  with e ->
    error "set_memory_offset_noexn %s %Ld: %s" dom_path offset_kib (Printexc.to_string e)


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
let update_cooperative_flags xs = 
  let now = Unix.gettimeofday () in
  Hashtbl.iter (fun domid last_time ->
		  let dom_path = xs.Xs.getdomainpath domid in
		  let old_value = get_uncooperative_noexn xs dom_path in
		  let new_value = now -. last_time > 20. in
		  if old_value <> new_value then set_uncooperative_noexn xs dom_path new_value
	       )
    when_domain_was_last_cooperative
		  

(** Best-effort creation of a 'host' structure and a simple debug line showing its derivation *)
let make_host ~xc ~xs =
	(* Wait for any scrubbing so that we don't end up with no immediately usable pages --
	   this might cause something else to fail (eg domain builder?) *)
	while Memory.get_scrub_memory_kib ~xc <> 0L do Unix.select [] [] [] 0.25 done;

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

	let domain_infolist = Xc.domain_getinfolist xc 0 in
	(*
		For the host free memory we sum the free pages and the pages needing
		scrubbing: we don't want to adjust targets simply because the scrubber
		is slow.
	*)
	let physinfo = Xc.physinfo xc in
	let free_pages_kib = Xc.pages_to_kib (Int64.of_nativeint physinfo.Xc.free_pages)
	and scrub_pages_kib = Xc.pages_to_kib (Int64.of_nativeint physinfo.Xc.scrub_pages) 
	and total_pages_kib = Xc.pages_to_kib (Int64.of_nativeint physinfo.Xc.total_pages) in
	let free_mem_kib = free_pages_kib +* scrub_pages_kib in

	let domains = List.concat
		(List.map
			(fun di ->
				try
					let path = xs.Xs.getdomainpath di.Xc.domid in
					let memory_actual_kib = Xc.pages_to_kib (Int64.of_nativeint di.Xc.total_memory_pages) in
					(* dom0 is special for some reason *)
					let memory_max_kib = if di.Xc.domid = 0 then 0L else Xc.pages_to_kib (Int64.of_nativeint di.Xc.max_memory_pages) in
					(* Misc other stuff appears in max_memory_pages *)
					let memory_max_kib = max 0L (memory_max_kib -* (xen_max_offset_kib di)) in
					let can_balloon = exists xs (feature_balloon_path path) in
					(* Once the domain tells us it can balloon, we assume it's not currently ballooning and
					   record the offset between memory_actual and target. We assume this is constant over the 
					   lifetime of the domain. *)
					let offset_kib = 
					  if not can_balloon then 0L
					  else begin
					    try
					      Int64.of_string (xs_read xs (memory_offset_path path))
					    with Xb.Noent ->
					      let target_kib = Int64.of_string (xs_read xs (target_path path)) in
					      let offset_kib = memory_actual_kib -* target_kib in
					      debug "domid %d just exposed feature-balloon; calibrating total_pages offset = %Ld KiB" di.Xc.domid offset_kib;
					      set_memory_offset_noexn xs path offset_kib;
					      offset_kib
					  end in
					let memory_actual_kib = memory_actual_kib -* offset_kib in

					let domain = 
					  { Squeeze.
						domid = di.Xc.domid;
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

					if never_been_run di then begin
						let initial_reservation_kib = Int64.of_string (xs_read xs (initial_reservation_path path)) in
						(* memory_actual_kib is memory which xen has accounted to this domain. We bump this up to
						   the "initial-reservation" and compute how much memory to subtract from the host's free
						   memory *)
						let unaccounted_kib = max 0L (Int64.sub initial_reservation_kib memory_actual_kib) in
						reserved_kib := Int64.add !reserved_kib unaccounted_kib;

						[ { domain with Squeeze.
						      dynamic_min_kib = initial_reservation_kib;
						      dynamic_max_kib = initial_reservation_kib;
						      target_kib = initial_reservation_kib;
						      memory_actual_kib = max memory_actual_kib initial_reservation_kib;
						  } ]
					end else begin

						let target_kib = Int64.of_string (xs_read xs (target_path path)) in
						(* min and max are written separately; if we notice they *)
						(* are missing set them both to the target for now.      *)
						let min_kib, max_kib =
						try
							Int64.of_string (xs_read xs (dynamic_min_path path)),
							Int64.of_string (xs_read xs (dynamic_max_path path))
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
				| Xb.Noent ->
				    (* useful debug message is printed by the xs_read function *)
				    []
				|  e ->
					debug "Skipping domid %d: %s"
						di.Xc.domid (Printexc.to_string e);
					[]
			)
			domain_infolist
		) in

	(* Sum up the 'reservations' which exist separately from domains *)
	let non_domain_reservations = Squeezed_state.total_reservations xs Squeezed_rpc._service in
	debug "Total non-domain reservations = %Ld" non_domain_reservations;
	reserved_kib := Int64.add !reserved_kib non_domain_reservations;

	let host = Squeeze.make_host ~domains ~free_mem_kib:(Int64.sub free_mem_kib !reserved_kib) in

	update_cooperative_table host;
	update_cooperative_flags xs;

	Printf.sprintf "F%Ld S%Ld R%Ld T%Ld" free_pages_kib scrub_pages_kib !reserved_kib total_pages_kib, host

(** Best-effort update of a domain's memory target. *)
let execute_action ~xc ~xs action =
	try
		let domid = action.Squeeze.action_domid in
		let path = xs.Xs.getdomainpath domid in
		let target_kib = action.Squeeze.new_target_kib in
		if target_kib < 0L
		then failwith "Proposed target is negative (domid %d): %Ld" domid target_kib;

		domain_setmaxmem_noexn xc domid target_kib;
		set_target_noexn xs path target_kib
	with e ->
		debug "Failed to reset balloon target (domid: %d) (target: %Ld): %s"
			action.Squeeze.action_domid action.Squeeze.new_target_kib
			(Printexc.to_string e)

let extra_mem_to_keep = 8L ** mib (** Domain.creates take "ordinary" memory as well as "special" memory *)

let target_host_free_mem_kib = low_mem_emergency_pool +* extra_mem_to_keep

let free_memory_tolerance_kib = 512L (** No need to be too exact *)

let io ~xc ~xs = {
  Squeeze.gettimeofday = Unix.gettimeofday;
  make_host = (fun () -> make_host ~xc ~xs);
  domain_setmaxmem = (fun domid kib -> domain_setmaxmem_noexn xc domid kib);
  wait = (fun delay -> ignore(Unix.select [] [] [] delay));
  execute_action = (fun action -> execute_action ~xc ~xs action);
  target_host_free_mem_kib = target_host_free_mem_kib;
  free_memory_tolerance_kib = free_memory_tolerance_kib;
}

let change_host_free_memory ~xc ~xs required_mem_kib success_condition = 
  Squeeze.change_host_free_memory (io ~xc ~xs) required_mem_kib success_condition

let free_memory ~xc ~xs required_mem_kib = 
  let io = io ~xc ~xs in
  Squeeze.change_host_free_memory io (required_mem_kib +* io.Squeeze.target_host_free_mem_kib) (fun x -> x >= (required_mem_kib +* io.Squeeze.target_host_free_mem_kib))

let free_memory_range ~xc ~xs min_kib max_kib =
  let io = io ~xc ~xs in
  Squeeze.free_memory_range io min_kib max_kib

let is_balanced x = Int64.sub x target_host_free_mem_kib < free_memory_tolerance_kib

let balance_memory ~xc ~xs = 
  Squeeze.balance_memory (io ~xc ~xs)

(** Return true if the host memory is currently unbalanced and needs rebalancing *)
let is_host_memory_unbalanced ~xc ~xs = 
  Squeeze.is_host_memory_unbalanced (io ~xc ~xs)
  
