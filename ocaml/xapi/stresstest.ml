(*
 * Copyright (C) 2007 XenSource Ltd.
 * Author Vincent Hanquez <vincent@xensource.com>
 *
 * Cluster stress testing for rio
 *)

open Printf
open Pervasiveext
open Stringext

module XapiImpl = functor(Remote: API.API) -> struct
	let sid = ref (Ref.null)

	let host = ref ""
	let port = ref 443
	let username = ref "root"
	let password = ref "xenroot"
	let secure = ref true

	type vmState = Running | Halted | Suspended | Paused | Unknown

	let rpc xml =
		if !secure then
			Xmlrpcclient.do_secure_xml_rpc ~version:"1.1" ~host:!host ~port:!port ~path:"/" xml
		else
			Xmlrpcclient.do_xml_rpc ~version:"1.0" ~host:!host ~port:!port ~path:"/" xml

	let session_init () =
		sid := Remote.Session.login_with_password ~rpc ~uname:!username ~pwd:!password ~version:Xapi_globs.api_version_string;
		()

	(** Query data *)
	let get_sr_record (sr: API.ref_SR) =
		Remote.SR.get_record ~rpc ~session_id:!sid ~self:sr

	let get_vm_record (vm: API.ref_VM) =
		Remote.VM.get_record ~rpc ~session_id:!sid ~self:vm

	let get_vbd_record (vbd: API.ref_VBD) =
		Remote.VBD.get_record ~rpc ~session_id:!sid ~self:vbd

	let get_all_vms () =
		let vms = Remote.VM.get_all ~rpc ~session_id:!sid in
		List.filter (fun (vm, r) -> not r.API.vM_is_a_template)
			    (List.map (fun vm -> vm, get_vm_record vm) vms)

	let get_pbd_record (pbd: API.ref_PBD) =
		Remote.PBD.get_record ~rpc ~session_id:!sid ~self:pbd

	let get_all_pbds () =
		let pbds = Remote.PBD.get_all ~rpc ~session_id:!sid in
		List.map (fun pbd -> pbd, get_pbd_record pbd) pbds

	let get_host_record (host: API.ref_host) =
		Remote.Host.get_record ~rpc ~session_id:!sid ~self:host

	let get_all_hosts () =
		let hosts = Remote.Host.get_all ~rpc ~session_id:!sid in
		List.map (fun host -> host, get_host_record host) hosts

	let pbd_get_sr pbd =
		Remote.PBD.get_SR ~rpc ~session_id:!sid ~self:pbd

	let vm_get_by_uuid uuid : API.ref_VM =
		Remote.VM.get_by_uuid ~rpc ~session_id:!sid ~uuid

	let vm_get_uuid vm =
		Remote.VM.get_uuid ~rpc ~session_id:!sid ~self:vm

	let vm_get_state vm =
		match Remote.VM.get_power_state ~rpc ~session_id:!sid ~self:vm with
		| `Halted -> Halted
		| `Paused -> Paused
		| `Running -> Running
		| `Suspended -> Suspended
		| `Unknown -> Unknown

	let vm_is_running vm = (vm_get_state vm = Running)
	let vm_is_suspended vm = (vm_get_state vm = Suspended)

	let vm_get_resident_on vm =
		Remote.VM.get_resident_on ~rpc ~session_id:!sid ~self:vm

	let vm_get_vbds vm =
		let vbds = Remote.VM.get_VBDs ~rpc ~session_id:!sid ~self:vm in
		List.map (fun vbd -> vbd, get_vbd_record vbd) vbds

	let vbd_get_vdi vbd =
		Remote.VBD.get_VDI ~rpc ~session_id:!sid ~self:vbd

	let vdi_get_vbds vdi =
		Remote.VDI.get_VBDs ~rpc ~session_id:!sid ~self:vdi

	let sr_get_uuid sr =
		Remote.SR.get_uuid ~rpc ~session_id:!sid ~self:sr

	let get_my_srs host ty : API.ref_SR list =
		let pbds = get_all_pbds () in
		let pbds_ok = List.filter (fun (pbd, r) ->
			r.API.pBD_currently_attached) pbds in
		let pbds_host = List.filter (fun (pbd, r) ->
			let pbdhost = Remote.PBD.get_host ~rpc ~session_id:!sid ~self:pbd in
			pbdhost = host
		) pbds_ok in
		let srs = List.map (fun (pbd, _) ->
			let sr = Remote.PBD.get_SR ~rpc ~session_id:!sid ~self:pbd in
			sr, get_sr_record sr
		) pbds_host in
		List.map fst (List.filter (fun (sr, r) -> r.API.sR_type = ty) srs)

	(** VDI commands *)
	let vdi_destroy vdi =
		Remote.VDI.destroy ~rpc ~session_id:!sid ~self:vdi

	(** VM commands -- all async except destroy and prepare_provisioning *)
	let vm_destroy vm =
		Remote.VM.destroy ~rpc ~session_id:!sid ~self:vm
	let vm_start vm : API.ref_task =
		Remote.Async.VM.start ~rpc ~session_id:!sid ~start_paused:false ~force:false ~vm
	let vm_pause vm : API.ref_task =
		Remote.Async.VM.pause ~rpc ~session_id:!sid ~vm
	let vm_unpause vm : API.ref_task =
		Remote.Async.VM.unpause ~rpc ~session_id:!sid ~vm
	let vm_suspend vm : API.ref_task =
		Remote.Async.VM.suspend ~rpc ~session_id:!sid ~vm
	let vm_resume vm : API.ref_task =
		Remote.Async.VM.resume ~rpc ~session_id:!sid ~vm ~start_paused:false ~force:false
	let vm_clone vm new_name : API.ref_task =
		Remote.Async.VM.clone ~rpc ~session_id:!sid ~vm ~new_name
	let vm_provision vm : API.ref_task =
		Remote.Async.VM.provision ~rpc ~session_id:!sid ~vm
	let vm_shutdown vm : API.ref_task =
		Remote.Async.VM.clean_shutdown ~rpc ~session_id:!sid ~vm
	let vm_shutdown_force vm : API.ref_task =
		Remote.Async.VM.hard_shutdown ~rpc ~session_id:!sid ~vm
	let vm_migrate vm host : API.ref_task =
		Remote.Async.VM.pool_migrate ~rpc ~session_id:!sid ~vm ~host ~options:[]

	let vm_prepare_provisioning vm sruuid =
		let rewrite_xmlstring xmlstring =
			let rewrite_disk disk =
				match disk with
				| Xml.Element("disk", params, []) ->
					let f (x, y) = if x <> "sr" then (x, y) else ("sr", sruuid) in
					Xml.Element("disk", List.map f params, [])
				| x -> x
				in
			let xml = Xml.parse_string xmlstring in
			let newxml = match xml with
			| Xml.Element("provision", [], disks) ->
				Xml.Element("provision", [], List.map rewrite_disk disks)
			| x ->
				x in
			Xml.to_string newxml
			in
		(* get xml provisioning stuff *)
		let other_config = Remote.VM.get_other_config ~rpc ~session_id:!sid ~self:vm in
		if List.mem_assoc "disks" other_config then (
			let newdisks = rewrite_xmlstring (List.assoc "disks" other_config) in
			Remote.VM.remove_from_other_config ~rpc ~session_id:!sid ~self:vm ~key:"disks";
			Remote.VM.add_to_other_config ~rpc ~session_id:!sid ~self:vm ~key:"disks" ~value:newdisks;
		);
		Remote.VM.add_to_other_config ~rpc ~session_id:!sid ~self:vm ~key:"stresstestvm" ~value:"";
		()
	let vm_slaughter_memory vm dividor =
		(* set dynamic max/min *)
		let memory = Remote.VM.get_memory_dynamic_max ~rpc ~session_id:!sid ~self:vm in
		let memory = Int64.div memory dividor in
		Remote.VM.set_memory_dynamic_max ~rpc ~session_id:!sid ~self:vm ~value:memory;
		let memory = Remote.VM.get_memory_dynamic_min ~rpc ~session_id:!sid ~self:vm in
		let memory = Int64.div memory dividor in
		Remote.VM.set_memory_dynamic_min ~rpc ~session_id:!sid ~self:vm ~value:memory;
		(* set static max/min *)
		let memory = Remote.VM.get_memory_static_max ~rpc ~session_id:!sid ~self:vm in
		let memory = Int64.div memory dividor in
		Remote.VM.set_memory_static_max ~rpc ~session_id:!sid ~self:vm ~value:memory;
		let memory = Remote.VM.get_memory_static_min ~rpc ~session_id:!sid ~self:vm in
		let memory = Int64.div memory dividor in
		Remote.VM.set_memory_static_min ~rpc ~session_id:!sid ~self:vm ~value:memory

	(** task query status methods *)
	let task_is_finished (task : API.ref_task) =
		Remote.Task.get_progress ~rpc ~session_id:!sid ~self:task = 1.0
	let task_is_error (task : API.ref_task) =
		Remote.Task.get_status ~rpc ~session_id:!sid ~self:task = `failure
	let task_get_error (task : API.ref_task) =
		Remote.Task.get_error_info ~rpc ~session_id:!sid ~self:task
	let task_get_result (task: API.ref_task) =
		Remote.Task.get_result ~rpc ~session_id:!sid ~self:task
	let task_get_type (task: API.ref_task) =
		Remote.Task.get_type ~rpc ~session_id:!sid ~self:task
	let task_get_ref task result =
		let xml = Xml.parse_string result in
		match xml with
		| Xml.Element("value", [], [ Xml.PCData s ]) -> Ref.of_string s
		| _                                          -> failwith "unknown result"
end

module Xapi = XapiImpl(Client.Client)

type task_status = Error of string list | Success of string | Unknown

type stressvm = {
	vmref: API.ref_VM;
	name: string;
	mutable task: API.ref_task;
	mutable task_status: task_status;
	mutable host: API.ref_host;
	mutable sr: API.ref_SR * API.sR_t;
}

let logger = Log.openout Log.Info
let print fmt = Log.info logger fmt
let print_err fmt = Log.error logger fmt

let print_progressbar = ref false
let progressbar_lastchar = ref '-'
let progressbarchar () =
	let c = !progressbar_lastchar in
	let next = match c with '-' -> '\\' | '\\' -> '|' | '|' -> '/' | '/' -> '-' in
	progressbar_lastchar := next;
	c

let progressbar completed_task len_task =
	if !print_progressbar then (
		let completed = (float_of_int completed_task) /. (float_of_int len_task) in
		let x = completed *. 80. in
		let s = String.make 80 ' ' in
		String.fill s 0 (int_of_float (floor x)) '=';
		printf "\r[%s] %c %.0f%%%!" s (progressbarchar ())
		                              (floor (completed *. 100.));
		if completed = 1. then
			printf "\n%!";
	)

(** wait until all tasks are finished, or timeout. Return the completed tasks with their status *)
let wait_tasks tasks timeout =
	let tasks_done = ref []
	and tasks_left = ref tasks
	and timeleft = ref timeout
	and ltask = List.length tasks in

	let before = Unix.gettimeofday () in
	while !timeleft > 0. && !tasks_left <> []
	do
		let nf, f = List.fold_left (fun (nf, f) task ->
			if Xapi.task_is_finished task then (
				let err =
					if Xapi.task_is_error task then
						Error (Xapi.task_get_error task)
					else
						Success (Xapi.task_get_result task) in
				(nf, (task, err) :: f)
			) else
				(task :: nf, f)
		) ([], []) !tasks_left in

		tasks_left := nf;
		tasks_done := f @ !tasks_done;

		Thread.delay 0.5;
		timeleft := !timeleft -. 0.5;

		progressbar (List.length !tasks_done) (ltask)
	done;
	if !timeleft > 0. then
		print "tasks finished in %.1f seconds" ((Unix.gettimeofday ()) -. before)
	else
		print "tasks timedout (%.1f)" timeout;
	!tasks_done

let wait_tasks_for_vms vms timeout =
	let tasks_done = wait_tasks (List.map (fun vm -> vm.task) vms) timeout in
	List.iter (fun vm ->
		try
			let status = List.assoc vm.task tasks_done in
			vm.task_status <- status
		with _ ->
			vm.task_status <- Unknown
	) vms

(* we allow 4s per VM with a minimum time waiting of 10s *)
let wait_startup_enough vms =
	let len = List.length vms in
	let timeout = len * 4 in
	let timeout =
		if timeout < 10 then 10.
		else if timeout > 45 then 45.
		else float_of_int timeout
		in
	print "waiting VM full startup for %.0f seconds" timeout;
	Thread.delay timeout

let check_error_and_reset_and_filter vms =
	List.fold_left (fun acc vm ->
		vm.task <- Ref.null;
		match vm.task_status with
		| Success _ -> vm :: acc
		| Error e   -> print_err "%s" (String.concat " " e); acc
		| Unknown   -> acc
	) [] vms

let check_error_and_reset vms =
	ignore (check_error_and_reset_and_filter vms)

let operate_on vms opname f =
	List.iter (fun vm ->
		print "op %s on VM \"%s\"" opname vm.name;
		let task = f vm.vmref in
		vm.task <- task;
	) vms

let operate_on_vm_vector v =
	List.iter (fun (vm, name, f) ->
		print "%s on VM \"%s\"" name vm.name;
		let task = f vm.vmref in
		vm.task <- task;
	) v

let get_hosts_sr hosts ty =
	(* get all the pbds currently attached *)
	let pbds = Xapi.get_all_pbds () in
	let pbds_ok = List.filter (fun (pbd, r) -> r.API.pBD_currently_attached) pbds in

	(* get all pbds per host *)
	let hostpbds = List.map (fun (host, hr) ->
		host, hr, (List.filter (fun (pbd, pr) -> pr.API.pBD_host = host) pbds_ok)
	) hosts in

	(* get srs that are of the type specified *)
	List.map (fun (host, hr, pbds) ->
		let srs = List.map (fun (pbd, r) ->
			let sr = Xapi.pbd_get_sr pbd in
			sr, Xapi.get_sr_record sr
		) pbds in
		host, hr, (List.filter (fun (sr, r) -> r.API.sR_type = ty) srs)
	) hostpbds

let prepare hosts_sr template_ref vmperhost =
	(* clone in parallel *)
	let vms_cloning = ref [] in
	for i = 0 to vmperhost - 1
	do
		let host_index = ref 0 in
		let nvms = List.map (fun x ->
			let name = sprintf "stresstest-vm-%d-host-%d" i !host_index in
			incr host_index;
			print "cloning VM \"%s\" from template" name;
			(name, x, (Xapi.vm_clone template_ref name))
		) hosts_sr in
		vms_cloning := nvms @ !vms_cloning
	done;
	let vms_cloning = !vms_cloning in

	let tasks = wait_tasks (List.map (fun (_, _, tsk) -> tsk) vms_cloning) 1000. in
	let tasks_success = List.filter (fun (task, status) ->
		match status with
		| Error e -> print_err "%s" (String.concat " " e); false
		| Unknown -> print_err "unknown"; false
		| Success _ -> true) tasks in

	let vms = List.map (fun (task, status) ->
		let (vm_name, (host, hr, srs), task) = List.find (fun (vm_name, x, vmtask) -> task = vmtask) vms_cloning in
		let vmref = match status with Success s -> Xapi.task_get_ref task s in
		{
			vmref = vmref;
			name = vm_name;
			task = Ref.null;
			task_status = Unknown;
			host = host;
			sr = List.hd srs
		}
	) tasks_success in

	List.iter (fun vm ->
		let sr_uuid = (snd vm.sr).API.sR_uuid in
		print "provisioning VM \"%s\"" vm.name;
		Xapi.vm_prepare_provisioning vm.vmref sr_uuid;
		vm.task <- Xapi.vm_provision vm.vmref;
	) vms;

	(* Wait install to finish on all hosts *)
	wait_tasks_for_vms vms 3600.;
	let installed_vms = check_error_and_reset_and_filter vms in
	List.iter (fun vm -> Xapi.vm_slaughter_memory vm.vmref 4L) installed_vms;
	installed_vms

let reset_state vms =
	let tasks = List.fold_left (fun acc vm ->
		if Xapi.vm_is_running vm.vmref then
			(Xapi.vm_shutdown_force vm.vmref) :: acc
		else
			acc
	) [] vms in
	ignore (wait_tasks tasks 60.);
	()

let reset_state_fully vms tries =
	let lefttries = ref tries in
	while !lefttries > 0
	do
		let tasks = List.fold_left (fun acc vm ->
			match Xapi.vm_get_state vm.vmref with
			| Xapi.Running   -> (Xapi.vm_shutdown_force vm.vmref) :: acc
			| Xapi.Paused    -> (Xapi.vm_unpause vm.vmref) :: acc
			| Xapi.Suspended -> (Xapi.vm_resume vm.vmref) :: acc
			| Xapi.Halted
			| Xapi.Unknown   -> acc
		) [] vms in
		if List.length tasks > 0 then ( 
			wait_tasks tasks 60.;
			decr lefttries
		) else
			lefttries := 0
	done;
	()

let do_startstop vms =
	operate_on vms "start" Xapi.vm_start;
	wait_tasks_for_vms vms 600.;
	let started_vms = check_error_and_reset_and_filter vms in

	wait_startup_enough started_vms;

	operate_on started_vms "shutdown" Xapi.vm_shutdown;
	wait_tasks_for_vms started_vms 600.;
	check_error_and_reset started_vms;

	reset_state vms;
	()

let do_start_suspend_resume_shutdown_cycle vms attempts =
	operate_on vms "start" Xapi.vm_start;
	wait_tasks_for_vms vms 600.;
	let started_vms = check_error_and_reset_and_filter vms in

	wait_startup_enough started_vms;
	let running_vms = ref started_vms in
	for i = 0 to attempts - 1
	do
		operate_on !running_vms "suspend" Xapi.vm_suspend;
		wait_tasks_for_vms !running_vms 3600.;
		let suspended_vms = check_error_and_reset_and_filter !running_vms in

		Thread.delay 1.;

		operate_on suspended_vms "restore" Xapi.vm_resume;
		wait_tasks_for_vms suspended_vms 3600.;
		running_vms := check_error_and_reset_and_filter suspended_vms;

		Thread.delay 1.;
	done;

	operate_on !running_vms "shutdown" Xapi.vm_shutdown;
	wait_tasks_for_vms !running_vms 600.;
	check_error_and_reset !running_vms;

	reset_state vms;
	()

let do_local_migrate_cycle vms migperattempt =
	operate_on vms "start" Xapi.vm_start;
	wait_tasks_for_vms vms 600.;
	let started_vms = check_error_and_reset_and_filter vms in

	wait_startup_enough started_vms;

	let migrate_vms = ref started_vms in
	for i = 0 to migperattempt - 1
	do
		operate_on !migrate_vms "migrate localhost" (fun vm ->
			let host = Xapi.vm_get_resident_on vm in
			Xapi.vm_migrate vm host
		);
		wait_tasks_for_vms !migrate_vms 600.;
		migrate_vms := check_error_and_reset_and_filter !migrate_vms;
		Thread.delay 0.1;
	done;

	operate_on !migrate_vms "shutdown" Xapi.vm_shutdown;
	wait_tasks_for_vms !migrate_vms 600.;
	check_error_and_reset !migrate_vms;
	reset_state vms;
	()

type random_op =
	| Op_start | Op_suspend | Op_shutdown | Op_shutdown_force
	| Op_migrate_local | Op_resume | Op_pause | Op_unpause

let do_random_operation vms =
	let compute_transitions state =
		match state with
		| Xapi.Running   -> "running", [ Op_suspend; Op_shutdown; Op_migrate_local; Op_pause ]
		| Xapi.Paused    -> "paused", [ Op_unpause ]
		| Xapi.Suspended -> "suspended", [ Op_resume ]
		| Xapi.Halted    -> "halted", [ Op_start ]
		| Xapi.Unknown   -> "unknown", [ Op_start ]
		in
	let choose_one operations : random_op =
		let l = List.length operations in
		if l > 1 then List.nth operations (Random.int l) else List.hd operations
		in
	let taskedvms = List.map (fun vm ->
		let (previous, operations) = compute_transitions (Xapi.vm_get_state vm.vmref) in
		let op = choose_one operations in
		let (name, cmd) = match op with
		| Op_resume         -> "resume", Xapi.vm_resume
		| Op_pause          -> "pause", Xapi.vm_pause
		| Op_unpause        -> "unpause", Xapi.vm_unpause
		| Op_suspend        -> "suspend", Xapi.vm_suspend
		| Op_shutdown_force -> "shutdown_force", Xapi.vm_shutdown_force
		| Op_shutdown       -> "shutdown", Xapi.vm_shutdown
		| Op_start          -> "start", Xapi.vm_start
		| Op_migrate_local  -> "migrate-local", (fun vm ->
			let host = Xapi.vm_get_resident_on vm in
			Xapi.vm_migrate vm host)
			in
		vm, ("state " ^ previous ^ " -> " ^ name), cmd
	) vms in

	operate_on_vm_vector taskedvms;
	wait_tasks_for_vms vms 600.;
	check_error_and_reset vms;

	Thread.delay 10.;
	()

let do_cluster_migrate_cycle vms migperattempt =
	operate_on vms "start" Xapi.vm_start;
	wait_tasks_for_vms vms 600.;
	let started_vms = check_error_and_reset_and_filter vms in

	wait_startup_enough started_vms;

	let find_unique_host hosts =
		let r = ref [] in
		List.iter (fun host -> if not (List.mem host !r) then r := host :: !r) hosts;
		!r
		in
	let hosts = List.map (fun vm -> Xapi.vm_get_resident_on vm.vmref) started_vms in
	let uniqhosts = find_unique_host hosts in
	let host_tbl = Hashtbl.create (List.length uniqhosts) in
	List.iter (fun host -> Hashtbl.add host_tbl host (Queue.create ())) uniqhosts;
	List.iter (fun vm ->
		let host = Xapi.vm_get_resident_on vm.vmref in
		let q = Hashtbl.find host_tbl host in
		Queue.push vm.vmref q
	) started_vms;

	let move src dst =
		let vms_src = Hashtbl.find host_tbl src in
		let vms_dst = Hashtbl.find host_tbl dst in
		let vm = Queue.pop vms_src in
		Queue.push vm vms_dst;
		vm
		in

	let hostring = Ring.make (List.length uniqhosts) (Ref.null) in
	List.iter (fun h -> Ring.push hostring h) uniqhosts;

	(* take nb_hosts VM in random and migrate them to (random_hosts - home_host) *)
	for i = 0 to migperattempt - 1
	do
		let nbhost =  List.length uniqhosts in
		let mov_vms = ref [] in
		for i = 0 to nbhost - 1
		do
			let src = Ring.peek hostring (i mod hostring.Ring.size) in
			let dst = Ring.peek hostring ((i + 1) mod hostring.Ring.size) in
			try
				let vm = move src dst in
				mov_vms := (vm, dst) :: !mov_vms
			with _ ->
				()
		done;

		let migrating_vms : stressvm list = List.fold_left (fun acc (vm, host) ->
			try
				let vmstress = List.find (fun vmobj -> vmobj.vmref = vm) started_vms in
				vmstress.host <- host;
				vmstress :: acc
			with _ -> acc
		) [] !mov_vms in

		operate_on migrating_vms "migrate" (fun vmref ->
			let vmstress = List.find (fun vmobj -> vmobj.vmref = vmref) started_vms in
			Xapi.vm_migrate vmref vmstress.host
		);
		wait_tasks_for_vms migrating_vms 600.;
		check_error_and_reset_and_filter migrating_vms;

		Thread.delay 0.5;
	done;

	operate_on started_vms "shutdown" Xapi.vm_shutdown;
	wait_tasks_for_vms started_vms 600.;
	check_error_and_reset started_vms;

	Thread.delay 0.1;
	
	reset_state vms;
	()

let uninstall vms =
	begin try reset_state_fully vms 10 with _ -> () end;
	List.iter (fun vm ->
		print "uninstalling VM \"%s\"" vm.name;

		let vbds = Xapi.vm_get_vbds vm.vmref in
		let vbds = List.filter (fun (vbd, r) -> r.API.vBD_mode = `RW) vbds in
		let vdis_to_remove = List.fold_left (fun acc (vbd, r) ->
			try
				let vdi = Xapi.vbd_get_vdi vbd in
				if Xapi.vdi_get_vbds vdi = [ vbd ] then
					vdi :: acc
				else
					acc
			with _ ->
				acc
		) [] vbds in

		List.iter (fun vdi ->
			print "\tdeleting VDI %s" (Ref.string_of vdi);
			Xapi.vdi_destroy vdi
		) vdis_to_remove;

		print "\tdestroying VM \"%s\"" vm.name;
		Xapi.vm_destroy vm.vmref
	) vms;
	()

let do_cycle n f =
	let a = Array.make n 0. in
	for i = 0 to n - 1 do
		(* starting cycle *)
		let starttime = Unix.gettimeofday () in
		f ();
		let endtime = Unix.gettimeofday () in
		a.(i) <- endtime -. starttime;
		Thread.delay 0.1
	done;
	a

let _ =
	let srtype = ref "" and templates = ref [] and mode = ref "" and modearg = ref "" in
	let attempt = ref 1 and cyclestat = ref false in
	let append_template s = templates := s :: !templates in
	let args = [
	("--progress", Arg.Set print_progressbar, "print a progressbar when performing actions");
	("--host", Arg.Set_string Xapi.host, "set hostname");
	("--port", Arg.Set_int Xapi.port, "set port");
	("--user", Arg.Set_string Xapi.username, "set username");
	("--pw", Arg.Set_string Xapi.password, "set password");
	("--nossl", Arg.Clear Xapi.secure, "don't use ssl");
	("--type", Arg.Set_string srtype, "install on a specific sr type");
	("--cycle", Arg.Set_int attempt, "set the number of cycle to do stuff");
	("--cyclestat", Arg.Set cyclestat, "print the cycle statistics at the end of the test");
	("--template", Arg.String append_template, "install these templates to cluster");
	("--mode", Arg.Set_string mode, "stress test mode");
	("--modearg", Arg.Set_string modearg, "stresst test mode arguments") ] in
	Arg.parse args (fun x -> eprintf "unknown arg: %s" x) "Api Cluster Stress test";

	let srtype = if !srtype <> "" then !srtype else failwith "error: no SR type specified (use --type)" in
	let templates = if !templates <> [] then !templates else failwith "error: no template specified (use --template)" in
	let attempt = !attempt in
	let mode = !mode and modearg = !modearg in
	if !Xapi.host = "" then failwith "error: no host specified (use --host)";

	if !Xapi.secure then
		Stunnel.init_stunnel_path();

	Xapi.session_init ();

	let template_refs =
		List.map (fun template ->
			try Xapi.vm_get_by_uuid template
			with _ ->
				let s = sprintf "error: cannot find the template reference by uuid: %s" template in
				failwith s
		) templates
		in

	(* get all the hosts *)
	let hosts = Xapi.get_all_hosts () in
	let hosts_sr = get_hosts_sr hosts srtype in

	(* check if we have one SR per host *)
	List.iter (fun (host, hr, srs) ->
		if List.length srs = 1 then () else (
			let s = sprintf "error: host %s have %d SRs of the type specified"
			                (hr.API.host_uuid) (List.length srs) in
			failwith s
		)
	) hosts_sr;

	(* try to parse mode arguments if any which should be a comma separated list of "k=v" *)
	let args =
		if modearg <> "" then
			List.map (fun x ->
				match (String.split ~limit:2 '=' x) with
				| [ k; v ] -> (k, v)
				| _ -> failwith "wrong argument to modearg") (String.split ',' modearg)
		else
			[] in

	(* FIXME *)
	let template_ref = List.hd template_refs in
	let do_cycle =
		let do_cycle_print n f =
			let a = do_cycle n f in
			Array.iteri (fun i a -> print "cycle %.3d %f" i a) a
			in
		if !cyclestat then do_cycle_print else (fun n f -> ignore (do_cycle n f))
		in

	match mode with
	| "startstop" ->
		let vmperhost = try int_of_string (List.assoc "vm_per_host" args) with _ -> 1 in
		let vms = prepare hosts_sr template_ref vmperhost in

		finally (fun () -> do_cycle attempt (fun () -> do_startstop vms))
			(fun () -> uninstall vms)
	| "suspendrestore" ->
		let vmperhost = try int_of_string (List.assoc "vm_per_host" args) with _ -> 1 in
		let attempt = try int_of_string (List.assoc "attempt" args) with _ -> 1 in
		let vms = prepare hosts_sr template_ref vmperhost in

		finally (fun () -> do_cycle attempt (fun () -> do_start_suspend_resume_shutdown_cycle vms attempt))
			(fun () -> uninstall vms)
	| "localmigrate" ->
		let vmperhost = try int_of_string (List.assoc "vm_per_host" args) with _ -> 1 in
		let migattempt = try int_of_string (List.assoc "migattempt" args) with _ -> 1 in
		let vms = prepare hosts_sr template_ref vmperhost in

		finally (fun () -> do_cycle attempt (fun () -> do_local_migrate_cycle vms migattempt))
		        (fun () -> uninstall vms)
	| "clustermigrate" ->
		let vmperhost = try int_of_string (List.assoc "vm_per_host" args) with _ -> 1 in
		let migattempt = try int_of_string (List.assoc "migattempt" args) with _ -> 1 in
		let vms = prepare hosts_sr template_ref vmperhost in

		finally (fun () -> do_cycle attempt (fun () -> do_cluster_migrate_cycle vms migattempt))
		        (fun () -> uninstall vms)
	| "random" ->
		let vmperhost = try int_of_string (List.assoc "vm_per_host" args) with _ -> 1 in
		let vms = prepare hosts_sr template_ref vmperhost in

		finally (fun () -> do_cycle attempt (fun () -> do_random_operation vms))
		        (fun () -> uninstall vms)
	| "install" ->
		let vmperhost = try int_of_string (List.assoc "vm_per_host" args) with _ -> 1 in
		let vms = prepare hosts_sr template_ref vmperhost in
		List.iter (fun vm ->
			print "installed vm: %s" (Xapi.vm_get_uuid vm.vmref);
		) vms;
	| _ ->
		failwith (sprintf "unknown mode: \"%s\"" mode)
