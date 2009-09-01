(**
	Interface between the abstract domain memory balancing code and Xen.
*)
(*
	Aims are:
	1. make this code robust to domains being created and destroyed around it.
	2. not depend on any other info beyond domain_getinfolist and xenstore.
*)
open Pervasiveext

let debug = Squeeze.debug

(** We define a domain which is paused, not shutdown and has not clocked up any CPU cycles
    as 'never_been_run' *)
let never_been_run di = di.Xc.paused && not di.Xc.shutdown && di.Xc.cpu_time = 0L

let initial_reservation_path dom_path = dom_path ^ "/memory/initial-reservation"
let target_path              dom_path = dom_path ^ "/memory/target"
let dynamic_min_path         dom_path = dom_path ^ "/memory/dynamic-min"
let dynamic_max_path         dom_path = dom_path ^ "/memory/dynamic-max"

let xs_read xs path = try xs.Xs.read path with Xb.Noent as e -> begin debug "xenstore-read %s returned ENOENT" path; raise e end

exception Cannot_free_this_much_memory of int64 (** even if we balloon everyone down we can't free this much *)
exception Domains_refused_to_cooperate of int list (** these VMs didn't release memory and we failed *)

(** Best-effort creation of a 'host' structure *)
let make_host ~xc ~xs =
	(* Some VMs are considered by us (but not by xen) to have an "initial-reservation". For VMs which have never 
	   run (eg which are still being built or restored) we take the difference between memory_actual_kib and the
	   reservation and subtract this manually from the host's free memory. Note that we don't get an atomic snapshot
	   of system state so there is a natural race between the hypercalls. Hopefully the memory is being consumed
	   fairly slowly and so the error is small. *)

	let reserved_kib = ref 0L in

	let domain_infolist = Xc.domain_getinfolist xc 0 in
	(*
		For the host free memory we sum the free pages and the pages needing
		scrubbing: we don't want to adjust targets simply because the scrubber
		is slow.
	*)
	let physinfo = Xc.physinfo xc in
	let free_mem_kib = Xc.pages_to_kib (Int64.add
		(Int64.of_nativeint physinfo.Xc.free_pages)
		(Int64.of_nativeint physinfo.Xc.scrub_pages))
	in

	let domains = List.concat
		(List.map
			(fun di ->
				try
					let path = xs.Xs.getdomainpath di.Xc.domid in
					let memory_actual_kib = Xc.pages_to_kib (Int64.of_nativeint di.Xc.total_memory_pages) in
					let domain = 
					  { Squeeze.
						domid = di.Xc.domid;
						can_balloon = not di.Xc.paused;
						dynamic_min_kib = 0L;
						dynamic_max_kib = 0L;
						target_kib = 0L;
						memory_actual_kib = 0L;
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
				with e ->
					debug "Skipping domid %d: %s"
						di.Xc.domid (Printexc.to_string e);
					[]
			)
			domain_infolist
		) in


	{Squeeze.
		domains = domains;
		free_mem_kib = Int64.sub free_mem_kib !reserved_kib;
	}

(** Best-effort update of a domain's memory target. *)
let execute_action ~xc ~xs action =
	try
		let domid = action.Squeeze.action_domid in
		let path = xs.Xs.getdomainpath domid in
		let target_kib = action.Squeeze.new_target_kib in
		Xs.transaction xs
			(fun t ->
				(* make sure no-one deletes the tree *)
				ignore (t.Xst.read path);
				if target_kib < 0L
				then failwith "Proposed target is negative (domid %d): %Ld"
					domid target_kib;
				Xc.domain_setmaxmem xc domid target_kib;
				t.Xst.write (target_path path) (Int64.to_string target_kib)
			)
	with e ->
		debug "Failed to reset balloon target (domid: %d) (target: %Ld): %s"
			action.Squeeze.action_domid action.Squeeze.new_target_kib
			(Printexc.to_string e)

(**
	If this returns successfully the required amount of memory should be free
	(modulo scrubbing).
*)
let change_host_free_memory ~xc ~xs required_mem_kib success_condition = 
	(* XXX: debugging *)
	debug "change_host_free_memory required_mem = %Ld KiB" required_mem_kib;
	let cols = [ Squeeze.Gnuplot.Memory_actual; Squeeze.Gnuplot.Target ] in
	let oc = open_out "/tmp/free_memory.dat" in
	finally
	(fun () ->

		(* Initially set all domain's maxmem = target. Note this portentially lets domains
		   allocate memory which we intend for other purposes. *)
		let host = make_host ~xc ~xs in
		List.iter (fun d -> Xc.domain_setmaxmem xc d.Squeeze.domid d.Squeeze.target_kib) host.Squeeze.domains;
		
		let acc = ref (Squeeze.Proportional.make ()) in
		let finished = ref false in
		while not (!finished) do
			let t = Unix.gettimeofday () in
			let host = make_host ~xc ~xs in
			Squeeze.Gnuplot.write_row oc host cols t;
			let acc', declared_active, declared_inactive, result =
				Squeeze.Proportional.change_host_free_memory success_condition !acc host required_mem_kib t in
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
			  | Squeeze.AdjustTargets actions ->
			      List.map (fun a -> a.Squeeze.action_domid, a.Squeeze.new_target_kib) actions
			  | _ -> [] in
			
			(* Deal with inactive and 'never been run' domains *)
			List.iter (fun domain -> 
				     let mem_max_kib = min domain.Squeeze.target_kib domain.Squeeze.memory_actual_kib in
				     debug "Setting inactive domain %d mem_max = %Ld" domain.Squeeze.domid mem_max_kib;
				     Xc.domain_setmaxmem xc domain.Squeeze.domid mem_max_kib
				  ) declared_inactive;
			(* Next deal with the active domains (which may have new targets) *)
			List.iter (fun domain ->
				     let domid = domain.Squeeze.domid in
				     let mem_max_kib = 
				       if List.mem_assoc domid new_targets 
				       then List.assoc domid new_targets 
				       else domain.Squeeze.target_kib in
				     debug "Setting active domain %d mem_max = %Ld" domain.Squeeze.domid mem_max_kib;
				     Xc.domain_setmaxmem xc domain.Squeeze.domid mem_max_kib				     
				  ) declared_active;

			begin match result with
				| Squeeze.Success ->
				    debug "Success: Host free memory = %Ld KiB" required_mem_kib;
				    finished := true
				| Squeeze.Failed [] ->
				    debug "Failed to free %Ld KiB of memory: operation impossible within current dynamic_min limits" required_mem_kib;
				    raise (Cannot_free_this_much_memory required_mem_kib);
				| Squeeze.Failed domains_to_blame ->
				    let domids = List.map (fun x -> x.Squeeze.domid) domains_to_blame in
				    let s = String.concat ", " (List.map string_of_int domids) in
				    debug "Failed to free %Ld KiB of memory: the following domains have failed to meet their targets: [ %s ]"
				      required_mem_kib s;
				    raise (Domains_refused_to_cooperate domids)
				| Squeeze.AdjustTargets actions ->
				    (* Set all the balloon targets *)
				    List.iter (fun action -> execute_action ~xc ~xs action) actions;
				    ignore(Unix.select [] [] [] 0.25);
			end
		done
	)
	(fun () ->
		close_out oc;
		Squeeze.Gnuplot.write_gp "/tmp/free_memory" (make_host ~xc ~xs) cols
	)

let free_memory ~xc ~xs required_mem_kib = change_host_free_memory ~xc ~xs required_mem_kib (fun x -> x >= required_mem_kib)




let target_host_free_mem_kib = 2048L (** Keep some memory around for new domain allocations (NB HVM takes more than PV) *)

let free_memory_tolerance_kib = 1024L (** No need to be too exact *)

let balance_memory ~xc ~xs = 
  try
    change_host_free_memory ~xc ~xs target_host_free_mem_kib 
      (fun x -> Int64.sub x target_host_free_mem_kib < free_memory_tolerance_kib)
  with e -> debug "balance memory caught: %s" (Printexc.to_string e)

