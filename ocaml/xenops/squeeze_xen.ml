(**
	Interface between the abstract domain memory balancing code and Xen.
*)
(*
	Aims are:
	1. make this code robust to domains being created and destroyed around it.
	2. not depend on any other info beyond domain_getinfolist and xenstore.
*)
open Pervasiveext

let start = Unix.gettimeofday ()
let debug fmt = Printf.kprintf
	(fun x ->
		Printf.fprintf stdout
			"[%.2f] %s\n" (Unix.gettimeofday() -. start) x;
		flush stdout
	)
	fmt

(** Best-effort creation of a 'host' structure *)
let make_host ~xc ~xs =
	let domains = List.concat
		(List.map
			(fun di ->
				try
					let path = xs.Xs.getdomainpath di.Xc.domid in
					let target_kib = Int64.of_string
						(xs.Xs.read (path ^ "/memory/target")) in
					(* min and max are written separately; if we notice they *)
					(* are missing set them both to the target for now.      *)
					let min_kib, max_kib =
						try
							Int64.of_string
								(xs.Xs.read (path ^ "/memory/dynamic-min")),
							Int64.of_string
								(xs.Xs.read (path ^ "/memory/dynamic-max"))
						with _ ->
							target_kib, target_kib
					in
					[{Squeeze.
						domid = di.Xc.domid;
						dynamic_min_kib = min_kib;
						dynamic_max_kib = max_kib;
						target_kib = target_kib;
						memory_actual_kib =
							Xc.pages_to_kib
								(Int64.of_nativeint di.Xc.total_memory_pages);
					}]
				with e ->
					debug "Skipping domid %d: %s"
						di.Xc.domid (Printexc.to_string e);
					[]
			)
			(Xc.domain_getinfolist xc 0)
		) in

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
	{Squeeze.
		domains = domains;
		free_mem_kib = free_mem_kib;
	}

(** Best-effort update of a domain's memory target. *)
let execute_action ~xs action =
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
				t.Xst.write
					(path ^ "/memory/target")
					(Int64.to_string target_kib)
			)
	with e ->
		debug "Failed to reset balloon target (domid: %d) (target: %Ld): %s"
			action.Squeeze.action_domid action.Squeeze.new_target_kib
			(Printexc.to_string e)

(**
	If this returns successfully the required amount of memory should be free
	(modulo scrubbing).
*)
let free_memory ~xc ~xs required_mem_kib = 
	(* XXX: debugging *)
	let cols = [ Squeeze.Gnuplot.Memory_actual; Squeeze.Gnuplot.Target ] in
	let oc = open_out "/tmp/free_memory.dat" in
	finally
	(fun () ->
		let acc = ref (Squeeze.Proportional.make ()) in
		let finished = ref false in
		while not (!finished) do
			let t = Unix.gettimeofday () in
			let host = make_host ~xc ~xs in
			Squeeze.Gnuplot.write_row oc host cols t;
			let acc', result =
				Squeeze.Proportional.free_memory !acc host required_mem_kib t in
			acc := acc';
			begin match result with
				| Squeeze.Success ->
					debug "%Ld KiB of memory has been freed" required_mem_kib;
					finished := true
				| Squeeze.Failed [] ->
					failwith (Printf.sprintf "Failed to free %Ld KiB of \
						memory: operation impossible within current \
						dynamic_min limits" required_mem_kib)
				| Squeeze.Failed domains_to_blame ->
					failwith (Printf.sprintf "Failed to free %Ld KiB of \
						memory: the following domains have failed to meet \"
						their targets: [ %s ]"
						required_mem_kib
						(String.concat ", "
							(List.map
								(fun x -> string_of_int x.Squeeze.domid)
								domains_to_blame
							)
						)
					);
				| Squeeze.AdjustTargets actions ->
					(* Set all the balloon targets *)
					List.iter (fun action -> execute_action ~xs action) actions;
					ignore(Unix.select [] [] [] 0.25);
			end
		done
	)
	(fun () ->
		close_out oc;
		Squeeze.Gnuplot.write_gp "/tmp/free_memory" (make_host ~xc ~xs) cols
	)

(** Reset all targets in an attempt to reduce the host free memory to zero. *)
let balance_memory ~xc ~xs = 
	let host = make_host ~xc ~xs in
	Squeeze.assert_within_dynamic_range host;
	debug "Before balancing: %s" (Squeeze.string_pairs_to_string
		(Squeeze.host_to_string_pairs host));
	let actions = Squeeze.Proportional.balance host in
	(* Set all balloon targets *)
	List.iter (fun action -> execute_action ~xs action) actions
