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
(** Functions for implementing 'High Availability' (HA). File is divided into 3 sections:
	+ scripts and functions which form part of the HA subsystem interface
	+ internal API calls used for arming and disarming individual hosts
	+ external API calls (Pool.enable_ha, Pool.disable_ha) used for turning on/off HA pool-wide
	* @group High Availability (HA)
 *)

module D = Debug.Make(struct let name="xapi_ha" end)
open D

module Rrdd = Rrd_client.Client

open Listext
open Xstringext
open Threadext
open Pervasiveext
open Forkhelpers
open Client
open Threadext
open Db_filter_types
open Xha_scripts

(* Create a redo_log instance to use for HA. *)
let ha_redo_log = Redo_log.create ~name:"HA redo log" ~state_change_callback:None ~read_only:false

(*********************************************************************************************)
(* Interface with the low-level HA subsystem                                                 *)

(** Returns the current live set info *)
let query_liveset() =
	let txt = call_script ~log_successful_output:false ha_query_liveset [] in
	Xha_interface.LiveSetInformation.of_xml_string txt

(** Returns true if this node has statefile access *)
let i_have_statefile_access () =
	try
		let liveset = query_liveset () in
		let hosts = liveset.Xha_interface.LiveSetInformation.hosts in
		let me = Hashtbl.find hosts liveset.Xha_interface.LiveSetInformation.local_host_id in
		me.Xha_interface.LiveSetInformation.Host.state_file_access && not (me.Xha_interface.LiveSetInformation.Host.state_file_corrupted)
	with e ->
		info "Caught exception querying liveset; assuming we have no statefile access: %s" (ExnHelper.string_of_exn e);
		false

(** Returns true if this node is allowed to be the master *)
let propose_master () =
	try
		let result = call_script ha_propose_master [] in
		String.rtrim result = "TRUE"
	with Xha_error e ->
		error "ha_propose_master threw unexpected exception: %s" (Xha_errno.to_string e);
		false

(** Returns true if local failover decisions have not been disabled on this node *)
let local_failover_decisions_are_ok () =
	try not(bool_of_string (Localdb.get Constants.ha_disable_failover_decisions)) with _ -> true

(** Since the liveset info doesn't include the host IP address, we persist these ourselves *)
let write_uuid_to_ip_mapping ~__context =
	let table = List.map (fun (_, host) -> host.API.host_uuid, host.API.host_address) (Db.Host.get_all_records ~__context) in
	let v = String_marshall_helper.map (fun x -> x) (fun x -> x) table in
	Localdb.put Constants.ha_peers v

(** Since the liveset info doesn't include the host IP address, we persist these ourselves *)
let get_uuid_to_ip_mapping () =
	let v = Localdb.get Constants.ha_peers in
	String_unmarshall_helper.map (fun x -> x) (fun x -> x) v

(** Without using the Pool's database, returns the IP address of a particular host
	named by UUID. *)
let address_of_host_uuid uuid =
	let table = get_uuid_to_ip_mapping () in
	if not(List.mem_assoc uuid table) then begin
		error "Failed to find the IP address of host UUID %s" uuid;
		raise Not_found
	end else List.assoc uuid table

(** Without using the Pool's database, returns the UUID of a particular host named by
	heartbeat IP address. This is only necesary because the liveset info doesn't include
	the host IP address *)
let uuid_of_host_address address =
	let table = List.map (fun (k, v) -> v, k) (get_uuid_to_ip_mapping ()) in
	if not(List.mem_assoc address table) then begin
		error "Failed to find the UUID address of host with address %s" address;
		raise Not_found
	end else List.assoc address table

(** Called in two circumstances:
	1. When I started up I thought I was the master but my proposal was rejected by the
	heartbeat component.
	2. I was happily running as someone's slave but they left the liveset.
 *)
let on_master_failure () =
	(* The plan is: keep asking if I should be the master. If I'm rejected then query the
	   live set and see if someone else has been marked as master, if so become a slave of them. *)

	let become_master () =
		info "This node will become the master";
		Xapi_pool_transition.become_master ();
		info "Waiting for server restart";
		while true do Thread.delay 3600. done in
	let become_slave_of uuid =
		let address = address_of_host_uuid uuid in
		info "This node will become the slave of host %s (%s)" uuid address;
		Xapi_pool_transition.become_another_masters_slave address;
		(* XXX CA-16388: prevent blocking *)
		Thread.delay 15.;
		error "Failed to flush and exit properly; forcibly exiting";
		exit Xapi_globs.restart_return_code in

	let finished = ref false in
	while not !finished do
		(* When HA is disabled without the statefile we set a flag to indicate that this node
		   cannot transition to master automatically on boot. This is to prevent failures during
		   the 'disarm fencing' step which cause some nodes to not fence themselves when they should. *)
		if local_failover_decisions_are_ok () && propose_master () then begin
			info "ha_propose_master succeeded";
			become_master ();
			finished := true
		end else begin
			if local_failover_decisions_are_ok ()
			then info "ha_propose_master failed: looking for another master"
			else info "ha_can_not_be_master_on_next_boot set: I cannot be master; looking for another master";

			let liveset = query_liveset () in
			match Hashtbl.fold
				(fun uuid host acc ->
					if host.Xha_interface.LiveSetInformation.Host.master
						&& host.Xha_interface.LiveSetInformation.Host.liveness (* CP-25481: a dead host may still have the master lock *)
					then uuid :: acc else acc) liveset.Xha_interface.LiveSetInformation.hosts [] with
					| [] ->
						info "no other master exists yet; waiting 5 seconds and retrying";
						Thread.delay 5.
					| uuid :: [] ->
						become_slave_of (Uuid.string_of_uuid uuid)
					| xs ->
						(* should never happen *)
						error "multiple masters reported: [ %s ]; failing"
							(String.concat "; " (List.map Uuid.string_of_uuid xs));
						failwith "multiple masters"
		end
	done

module Timeouts = struct
	type t = {
		heart_beat_interval: int;
		state_file_interval: int;
		heart_beat_timeout: int;
		state_file_timeout: int;
		heart_beat_watchdog_timeout: int;
		state_file_watchdog_timeout: int;
		boot_join_timeout: int;
		enable_join_timeout: int;
		xapi_healthcheck_timeout: int;
		xapi_healthcheck_interval: int;
		xapi_restart_timeout: int;
		xapi_restart_attempts: int;
	}
	let derive (t: int) =
		(* xHA interface section 4.1.4.1.1 Formula of key timeouts *)
		(* t >= 10 *)
		if t < 10 then failwith "constraint violation: timeout >= 10";
		(* All other values are derived from this single parameter *)
		let heart_beat_interval = (t + 10) / 10 in
		let state_file_interval = heart_beat_interval in
		let heart_beat_timeout = t in
		let state_file_timeout = t in
		let heart_beat_watchdog_timeout = t in
		let state_file_watchdog_timeout = t + 15 in
		let boot_join_timeout = t + 60 in
		let enable_join_timeout = boot_join_timeout in

		{ heart_beat_interval = heart_beat_interval;
		state_file_interval = state_file_interval;
		heart_beat_timeout = heart_beat_timeout;
		state_file_timeout = state_file_timeout;
		heart_beat_watchdog_timeout = heart_beat_watchdog_timeout;
		state_file_watchdog_timeout = state_file_watchdog_timeout;
		boot_join_timeout = boot_join_timeout;
		enable_join_timeout = enable_join_timeout;

		xapi_healthcheck_interval = !Xapi_globs.ha_xapi_healthcheck_interval;
		xapi_healthcheck_timeout = !Xapi_globs.ha_xapi_healthcheck_timeout;
		xapi_restart_attempts = !Xapi_globs.ha_xapi_restart_attempts;
		xapi_restart_timeout = !Xapi_globs.ha_xapi_restart_timeout; (* 180s is max start delay and 60s max shutdown delay in the initscript *)
		}

	(** Returns the base timeout value from which the rest are derived *)
	let get_base_t ~__context =
		let pool = Helpers.get_pool ~__context in
		let other_config = Db.Pool.get_other_config ~__context ~self:pool in
		let configuration = Db.Pool.get_ha_configuration ~__context ~self:pool in
		(* xHA built-in default is 30s. We've bumped ours to 40s to make TC7710/CA-17639 happier *)
		(* We then bumped it again to 60s to work around multipath breakage in CA-27666 *)
		(* We then allowed it to be bumped persistently by users to work around multipath breakage in CA-28306 *)
		let t =
			if List.mem_assoc Xapi_globs.xha_timeout configuration
			then int_of_string (List.assoc Xapi_globs.xha_timeout configuration)
			else
				if List.mem_assoc Xapi_globs.default_ha_timeout other_config
				then int_of_string (List.assoc Xapi_globs.default_ha_timeout other_config)
				else int_of_float !Xapi_globs.ha_default_timeout_base in
		t
end

module Monitor = struct
	(** Control the background HA monitoring thread *)

	let request_shutdown = ref false
	let prevent_failover_actions_until = ref 0. (* protected by the request_shutdown_m too *)
	let block_delay_calls = ref false (* set to true when Pool.ha_prevent_restarts_for calls must wait *)
	let block_delay_calls_c = Condition.create () (* used to wake up all Pool.ha_prevent_restarts_for threads *)
	let m = Mutex.create ()


	(* We use this for interruptible sleeping *)
	let delay = Delay.make ()

	let thread = ref None
	let thread_m = Mutex.create ()

	let database_state_valid = ref false
	let database_state_valid_c = Condition.create ()

	(* Used to explicitly signal that we should replan *)
	let plan_out_of_date = ref true

	exception Already_started
	exception Not_started

	(** Background thread which monitors the membership set and takes action if HA is
		armed and something goes wrong *)
	let ha_monitor () : unit = Debug.with_thread_named "ha_monitor" (fun () ->
		debug "initialising HA background thread";
		(* NB we may be running this code on a slave in emergency mode *)

		Server_helpers.exec_with_new_task "HA monitor" (fun __context ->

			let statefiles = Xha_statefile.list_existing_statefiles () in

			debug "HA background thread starting";

			(* Grab the base timeout value so we can cook the reported latencies *)
			let base_t = int_of_string (Localdb.get Constants.ha_base_t) in
			let timeouts = Timeouts.derive base_t in

			(* Set up our per-host alert triggers *)

			let localhost_uuid = Helpers.get_localhost_uuid () in
			let boolean_warning msg body_option =
				let trigger =
					Xapi_alert.edge_trigger
						(fun old newvalue ->
							if newvalue then begin
								warn "%s" (fst msg);
								begin
									match body_option with
											None -> ()
										| (Some body) ->
											Xapi_alert.add ~msg ~cls:`Host ~obj_uuid:localhost_uuid ~body
								end
							end) in
				(* make sure we spot the case where the warning triggers immediately *)
				trigger false;
				trigger in
			(* Per-host warnings which are logged but do not generate alerts: *)
			let warning_statefile_lost = boolean_warning Api_messages.ha_statefile_lost None in
			let warning_heartbeat_approaching_timeout = boolean_warning Api_messages.ha_heartbeat_approaching_timeout None in
			let warning_statefile_approaching_timeout = boolean_warning Api_messages.ha_statefile_approaching_timeout None in
			let warning_xapi_healthcheck_approaching_timeout = boolean_warning Api_messages.ha_xapi_healthcheck_approaching_timeout None in
			(* Per-host warnings which are logged *and* generate alerts: *)
			let warning_network_bonding_error =
				boolean_warning Api_messages.ha_network_bonding_error
					(Some (Printf.sprintf "The network bond used for transmitting HA heartbeat messages on host '%s' has failed" localhost_uuid)) in

			(* Pool-wide warning which is logged by the master *and* generates an alert. Since this call is only ever made on the master we're
			   ok to make database calls to compute the message body without worrying about the db call blocking: *)
			let warning_all_live_nodes_lost_statefile =
				boolean_warning Api_messages.ha_statefile_lost
					(Some (Printf.sprintf "All live servers have lost access to the HA statefile")) in

			let last_liveset_uuids = ref [] in
			let last_plan_time = ref 0. in

			(* Called on all hosts to query the liveset and update statistics + messages *)
			(* WARNING: must not touch the database or perform blocking I/O              *)
			let query_liveset_on_all_hosts () =
				let liveset =
					try
						(* XXX: if we detect the liveset has been poisoned then we're in the middle
						   of a with-statefile disable and should stop our monitor thread *)
						query_liveset ()
					with
						| Xha_error Xha_errno.Mtc_exit_daemon_is_not_present as e ->
							info "Monitor thread caught MTC_EXIT_DAEMON_IS_NOT_PRESENT; deactivating HA failover thread";
							Mutex.execute m (fun () -> request_shutdown := true);
							raise e
						| e ->
							info "Caught exception querying liveset: %s; deactivating HA failover thread"
								(ExnHelper.string_of_exn e);
							Mutex.execute m (fun () -> request_shutdown := true);
							raise e in
				debug "Liveset: %s" (Xha_interface.LiveSetInformation.to_summary_string liveset);
				(* All hosts: Feed the current latency values into the per-host RRDs (if available) *)
				Opt.iter
					(fun local ->
						(* Assume all values are ms *)
						let statefile = float_of_int (local.Xha_interface.LiveSetInformation.RawStatus.statefile_latency) /. 1000. in
						let heartbeat_latency = float_of_int local.Xha_interface.LiveSetInformation.RawStatus.heartbeat_latency /. 1000. -. (float_of_int timeouts.Timeouts.heart_beat_interval) in
						let xapi_latency = float_of_int (local.Xha_interface.LiveSetInformation.RawStatus.xapi_healthcheck_latency) /. 1000. in
						let statefile_latencies = List.map (fun vdi -> let open Rrd.Statefile_latency in {id = vdi.Static_vdis.uuid; latency = Some statefile}) statefiles in
						log_and_ignore_exn (fun () -> Rrdd.HA.enable_and_update ~statefile_latencies ~heartbeat_latency ~xapi_latency)
					) liveset.Xha_interface.LiveSetInformation.raw_status_on_local_host;

				(* All hosts: create alerts from per-host warnings (if available) *)
				debug "Processing warnings";
				Opt.iter
					(fun warning ->
						warning_statefile_lost warning.Xha_interface.LiveSetInformation.Warning.statefile_lost;
						warning_heartbeat_approaching_timeout warning.Xha_interface.LiveSetInformation.Warning.heartbeat_approaching_timeout;
						warning_statefile_approaching_timeout warning.Xha_interface.LiveSetInformation.Warning.statefile_approaching_timeout;
						warning_xapi_healthcheck_approaching_timeout warning.Xha_interface.LiveSetInformation.Warning.xapi_healthcheck_approaching_timeout;
						warning_network_bonding_error warning.Xha_interface.LiveSetInformation.Warning.network_bonding_error;
					) liveset.Xha_interface.LiveSetInformation.warning_on_local_host;
				debug "Done with warnings";
				liveset in

			(* Slaves monitor the master                                    *)
			(* WARNING: must not touch the database or perform blocking I/O *)
			let process_liveset_on_slave liveset =
				let address = Pool_role.get_master_address () in
				let master_uuid = uuid_of_host_address address in
				let master_info = Hashtbl.find liveset.Xha_interface.LiveSetInformation.hosts (Uuid.uuid_of_string master_uuid) in
				if true
					&& master_info.Xha_interface.LiveSetInformation.Host.liveness
					&& master_info.Xha_interface.LiveSetInformation.Host.master
				then debug "The node we think is the master is still alive and marked as master; this is OK"
				else begin
					warn "We think node %s (%s) is the master but the liveset disagrees" master_uuid address;
					on_master_failure ()
				end in

			(* Return the host UUIDs of all nodes in the liveset *)
			let uuids_of_liveset liveset =
				Hashtbl.fold
					(fun uuid host acc ->
						if host.Xha_interface.LiveSetInformation.Host.liveness then uuid :: acc else acc)
					liveset.Xha_interface.LiveSetInformation.hosts [] in

			(* Master performs VM restart and keeps track of the recovery plan *)
			(* WARNING: database is ok but must not perform blocking I/O       *)
			let process_liveset_on_master liveset =
				let pool = Helpers.get_pool ~__context in
				let to_tolerate = Int64.to_int (Db.Pool.get_ha_host_failures_to_tolerate ~__context ~self:pool) in
				(* let planned_for = Int64.to_int (Db.Pool.get_ha_plan_exists_for ~__context ~self:pool) in *)

				(* First consider whether VM failover actions need to happen.
				   Convert the liveset into a list of Host references used by the VM failover code *)
				let liveset_uuids = List.sort compare (uuids_of_liveset liveset) in
				if !last_liveset_uuids <> liveset_uuids then begin
					warn "Liveset looks different; assuming we need to rerun the planner";
					plan_out_of_date := true;
					last_liveset_uuids := liveset_uuids
				end;

				let liveset_refs = List.map (fun uuid -> Db.Host.get_by_uuid ~__context ~uuid:(Uuid.string_of_uuid uuid)) liveset_uuids in
				if local_failover_decisions_are_ok () then begin
					try
						Xapi_ha_vm_failover.restart_auto_run_vms ~__context liveset_refs to_tolerate
					with e ->
						log_backtrace ();
						error "Caught unexpected exception when executing restart plan: %s" (ExnHelper.string_of_exn e)
				end;

				(* At this point the hosts not in the liveset have been declared dead *)

				(* Next update the Host_metrics.live value to spot hosts coming back *)
				let all_hosts = Db.Host.get_all ~__context in
				let livemap = List.map (fun host -> host, List.mem host liveset_refs) all_hosts in
				List.iter (fun (host, live) ->
					Helpers.log_exn_continue
						(Printf.sprintf "updating Host_metrics.live to %b for %s" live (Ref.string_of host))
						(fun () ->
							let metrics = Db.Host.get_metrics ~__context ~self:host in
							let current = Db.Host_metrics.get_live ~__context ~self:metrics in
							let shutting_down =
								Mutex.execute Xapi_globs.hosts_which_are_shutting_down_m
									(fun () -> List.mem host !Xapi_globs.hosts_which_are_shutting_down) in
							if current <> live then begin
								(* This can only be a false -> true transient as the 'restart_auto_run_vms' function
								   has already dealt with the true -> false case. *)
								(* => live must be true. No need to consider calling current script hooks *)
								if shutting_down
								then info "Not marking host %s as live because it is shutting down" (Ref.string_of host)
								else begin
									Db.Host_metrics.set_live ~__context ~self:metrics ~value:live;
									Xapi_host_helpers.update_allowed_operations ~__context ~self:host
								end
							end
						) ()
				) livemap;

				(* Next update the Host.ha_statefiles and Host.ha_network_peers fields. For the network
				   peers we use whichever view is more recent: network or statefile *)
				let statefiles = Db.Pool.get_ha_statefiles ~__context ~self:pool in
				let host_host_table = List.map
					(fun host ->
						host, Hashtbl.find liveset.Xha_interface.LiveSetInformation.hosts
							(Uuid.uuid_of_string (Db.Host.get_uuid ~__context ~self:host))) all_hosts in
				List.iter (fun (host, xha_host) ->
					(* NB there is only one statefile currently *)
					let current = Db.Host.get_ha_statefiles ~__context ~self:host <> [] in
					let newval = xha_host.Xha_interface.LiveSetInformation.Host.state_file_access in
					if current <> newval
					then Db.Host.set_ha_statefiles ~__context ~self:host
						~value:(if newval then statefiles else []);
				) host_host_table;
				(* If all live hosts have lost statefile then we are running thanks to Survival Rule 2: this should be flagged to the user,
				   who should fix their storage. Note that if some hosts can see the storage but others cannot, there is no point generating
				   an alert because those who cannot are about to fence. *)
				let relying_on_rule_2 xha_host =
					true
					&& xha_host.Xha_interface.LiveSetInformation.Host.liveness              (* it is still alive *)
					&& not xha_host.Xha_interface.LiveSetInformation.Host.state_file_access (* and yet has no statefile access *)
				in
				let all_live_nodes_lost_statefile = List.fold_left (&&) true (List.map (fun (_, xha_host) -> relying_on_rule_2 xha_host) host_host_table) in
				warning_all_live_nodes_lost_statefile all_live_nodes_lost_statefile;

				(* Now the Host.ha_network_peers *)
				let subset a b = List.fold_left (fun acc x -> acc && (List.mem x b)) true a in
				let set_equals a b =
					let a' = List.setify a and b' = List.setify b in
					subset a' b' && (subset b' a') in

				(* NB raw_status_on_local_host not available if in 'Starting' state *)
				begin match liveset.Xha_interface.LiveSetInformation.raw_status_on_local_host with
					| None ->
						debug "No raw_status_on_local_host to process"
					| Some local ->
						let host_raw_table = List.map
							(fun host ->
								host, Hashtbl.find local.Xha_interface.LiveSetInformation.RawStatus.host_raw_data
									(Uuid.uuid_of_string (Db.Host.get_uuid ~__context ~self:host))) all_hosts in
						List.iter (fun (host, raw) ->
							(* Use the list of network peers given by the host recent update: statefile or network *)
							let peers =
								if raw.Xha_interface.LiveSetInformation.HostRawData.time_since_last_update_on_statefile <
									raw.Xha_interface.LiveSetInformation.HostRawData.time_since_last_heartbeat
								then raw.Xha_interface.LiveSetInformation.HostRawData.heartbeat_active_list_on_statefile
								else raw.Xha_interface.LiveSetInformation.HostRawData.heartbeat_active_list_on_heartbeat in
							let peer_strings = List.map Uuid.string_of_uuid peers in
							debug "Network peers = [%s]" (String.concat ";" peer_strings);
							let existing_strings = Db.Host.get_ha_network_peers ~__context ~self:host in
							if not(set_equals peer_strings existing_strings)
							then Db.Host.set_ha_network_peers ~__context ~self:host ~value:peer_strings)
							host_raw_table;

				end;

				let now = Unix.gettimeofday () in
				let plan_too_old = now -. !last_plan_time > !Xapi_globs.ha_monitor_plan_interval in
				if plan_too_old || !plan_out_of_date then begin
					let changed = Xapi_ha_vm_failover.update_pool_status ~__context ~live_set:liveset_refs () in

					(* Extremely bad: something managed to break our careful plan *)
					if changed && not !plan_out_of_date then error "Overcommit protection failed to prevent a change which invalidated our failover plan";

					last_plan_time := now;
					plan_out_of_date := false;
				end in

			(* Wait for all hosts in the liveset to become enabled so we can start VMs on them. We wait for up to ha_monitor_startup_timeout. *)
			let wait_for_slaves_on_master () =
				(* CA-17849: need to make sure that, in the xapi startup case, all hosts have been set to dead and disabled initially *)
				info "Master HA startup waiting for explicit signal that the database state is valid";
				Mutex.execute thread_m
					(fun () ->
						while not(!database_state_valid) do
							Condition.wait database_state_valid_c thread_m
						done);

				info "Master HA startup waiting for up to %.2f for slaves in the liveset to report in and enable themselves" !Xapi_globs.ha_monitor_startup_timeout;
				let start = Unix.gettimeofday () in
				let finished = ref false in
				while Mutex.execute m (fun () -> not(!request_shutdown)) && not(!finished) do
					try
						ignore(Delay.wait delay !Xapi_globs.ha_monitor_interval);
						if Mutex.execute m (fun () -> not(!request_shutdown)) then begin
							let liveset = query_liveset_on_all_hosts () in
							let uuids = List.map Uuid.string_of_uuid (uuids_of_liveset liveset) in
							let enabled = List.map (fun uuid -> Db.Host.get_enabled ~__context ~self:(Db.Host.get_by_uuid ~__context ~uuid)) uuids in
							let enabled, disabled = List.partition (fun (_, x) -> x) (List.combine uuids enabled) in
							debug "Enabled hosts = [ %s ]; disabled hosts = [ %s ]" (String.concat "; " (List.map fst enabled)) (String.concat "; " (List.map fst disabled));

							if disabled = [] then begin
								info "Master HA startup: All live hosts are now enabled: [ %s ]" (String.concat "; " (List.map fst enabled));
								finished := true;
							end;

							if Unix.gettimeofday () -. start > !Xapi_globs.ha_monitor_startup_timeout && disabled <> [] then begin
								info "Master HA startup: Timed out waiting for all live slaves to enable themselves (have some hosts failed to attach storage?) Live but disabled hosts: [ %s ]"
									(String.concat "; " (List.map fst disabled));
								finished := true
							end;
						end;
					with e ->
						debug "Exception in HA monitor thread while waiting for slaves: %s" (ExnHelper.string_of_exn e);
						Thread.delay !Xapi_globs.ha_monitor_interval
				done in

			(* If we're the master we must wait for our live slaves to turn up before we consider restarting VMs etc *)
			if Pool_role.is_master () then wait_for_slaves_on_master ();

			(* Monitoring phase: we must assume the worst and not touch the database here *)
			while Mutex.execute m (fun () -> not(!request_shutdown)) do
				try
					ignore(Delay.wait delay !Xapi_globs.ha_monitor_interval);

					if Mutex.execute m (fun () -> not(!request_shutdown)) then begin
						let liveset = query_liveset_on_all_hosts () in
						if Pool_role.is_slave () then process_liveset_on_slave liveset;
						if Pool_role.is_master () then begin
							(* CA-23998: allow MTC to block master failover actions (ie VM restart) for a certain period of time
							   while their Level 2 VMs are being restarted. *)
							finally
								(fun () ->
									let until = Mutex.execute m
										(fun () ->
											(* Callers of 'delay' will have to wait until we have finished processing this loop iteration *)
											block_delay_calls := true;
											!prevent_failover_actions_until) in

									(* FIST *)
									while Xapi_fist.simulate_blocking_planner () do Thread.delay 1. done;

									let now = Unix.gettimeofday () in
									if now < until
									then debug "Blocking VM restart thread for at least another %.0f seconds" (until -. now)
									else process_liveset_on_master liveset)
								(fun () ->
									(* Safe to unblock callers of 'delay' now *)
									Mutex.execute m
										(fun () ->
											(* Callers of 'delay' can now safely request a delay knowing that the liveset won't be processed
											   until after the delay period. *)
											block_delay_calls := false;
											Condition.broadcast block_delay_calls_c) )
						end
					end
				with e ->
					debug "Exception in HA monitor thread: %s" (ExnHelper.string_of_exn e);
					Thread.delay !Xapi_globs.ha_monitor_interval
			done;

			debug "Re-enabling old Host_metrics.live heartbeat";
			Mutex.execute Db_gc.use_host_heartbeat_for_liveness_m
				(fun () -> Db_gc.use_host_heartbeat_for_liveness := true);

			debug "Stopping reading per-host HA stats";
			log_and_ignore_exn Rrdd.HA.disable;

			debug "HA background thread told to stop")
		) ()

	let prevent_restarts_for seconds =
		(* Wait until the thread stops processing and is about to sleep / is already sleeping *)
		Mutex.execute m
			(fun () ->
				while !block_delay_calls = true do Condition.wait block_delay_calls_c m done;
				debug "Blocking VM restart actions for another %Ld seconds" seconds;
				prevent_failover_actions_until := Unix.gettimeofday () +. (Int64.to_float seconds);
				(* If we get a value of 0 then we immediately trigger a VM restart *)
				if seconds < 1L then Delay.signal delay
			)

	let start () =
		debug "Monitor.start()";
		debug "Disabling old heartbeat; live flag will be driven directly from xHA liveset";
		(* NB in the xapi startup case this will prevent the db_gc.single_pass from setting any live flags *)
		Mutex.execute Db_gc.use_host_heartbeat_for_liveness_m
			(fun () -> Db_gc.use_host_heartbeat_for_liveness := false);

		Mutex.execute thread_m
			(fun () ->
				match !thread with
					| Some _ -> raise Already_started
					| None ->
						(* This will cause the started thread to block until signal_database_state_valid is called *)
						request_shutdown := false;
						thread := Some (Thread.create ha_monitor ()))

	let signal_database_state_valid () =
		Mutex.execute thread_m
			(fun () ->
				debug "Signalling HA monitor thread that it is ok to look at the database now";
				database_state_valid := true;
				Condition.signal database_state_valid_c)

	let stop () =
		debug "Monitor.stop()";
		Mutex.execute thread_m
			(fun () ->
				match !thread with
					| None ->
						warn "Failed to stop HA monitor thread because it wasn't running. Perhaps it was stopped more than once?"
					| Some t ->
						Mutex.execute m (fun () -> request_shutdown := true; Delay.signal delay);
						Thread.join t;
						thread := None)


end

(** Called by MTC in Orlando Update 1 to temporarily block the VM restart thread. *)
let ha_prevent_restarts_for __context seconds =
	(* Even if HA is not enabled, this should still go ahead (rather than doing
	 * a successful no-op) in case HA is about to be enabled within the specified
	 * number of seconds. Raising an error here caused CA-189075. *)
	Monitor.prevent_restarts_for seconds


(* ----------------------------- *)
(* Interaction with the redo log *)

(* This function is called when HA is enabled during run-time: flush the DB to
 * the redo-log and make future DB changes get written as deltas. *)
let redo_log_ha_enabled_during_runtime __context =
	debug "Enabling HA, so also enabling writing to redo-log";
	let pool = Helpers.get_pool ~__context in
	if Db.Pool.get_redo_log_enabled ~__context ~self:pool then begin
		info "A redo log is already in use; switch to the dedicated HA VDI.";
		Redo_log.switch ha_redo_log Xapi_globs.ha_metadata_vdi_reason
	end else begin
		info "Switching on HA redo log.";
		Redo_log.enable ha_redo_log Xapi_globs.ha_metadata_vdi_reason
			(* upon the first attempt to write a delta, it will realise that a DB flush
			 * is necessary as the I/O process will not be running *)
	end


(* This function is called when HA is disabled during run-time: stop the
 * I/O process and make future DB changes not go to the redo-log. *)
let redo_log_ha_disabled_during_runtime __context =
	debug "Disabling HA, so also disabling writing to redo-log";
	let pool = Helpers.get_pool ~__context in
	if Db.Pool.get_redo_log_enabled ~__context ~self:pool then begin
		(* switch to the other VDI *)
		info "A general redo-log is in available, independent from HA; using it now";
		Redo_log.switch ha_redo_log Xapi_globs.gen_metadata_vdi_reason
	end
	else begin
		Redo_log_usage.stop_using_redo_log ha_redo_log;
		Redo_log.disable ha_redo_log
	end

(* This function is called when HA is found to be enabled at startup, before
 * the DB backend is initialised. Read the latest DB from the block-device, and
 * make future DB changes get written as deltas. *)
let redo_log_ha_enabled_at_startup () =
	(* If we are still the master, extract any HA metadata database so we can consider population from it *)
	if Pool_role.is_master () then begin
		debug "HA is enabled, so enabling writing to redo-log";
		Redo_log.enable ha_redo_log Xapi_globs.ha_metadata_vdi_reason; (* enable the use of the redo log *)
		debug "This node is a master; attempting to extract a database from a metadata VDI";
		let db_ref = Db_backend.make () in 
		Redo_log_usage.read_from_redo_log ha_redo_log Xapi_globs.ha_metadata_db db_ref (* best effort only: does not raise any exceptions *)
	end

(* ----------------------------- *)


(** Called when xapi restarts: server may be in emergency mode at this point. We need
	to inspect the local configuration and if HA is supposed to be armed we need to
	set everything up.
	Note that
	the master shouldn't be able to activate HA while we are offline since that would cause
	us to come up with a broken configuration (the enable-HA stage has the critical task of
	synchronising the HA configuration on all the hosts). So really we only want to notice
	if the Pool has had HA disabled while we were offline. *)
let on_server_restart () =
	let armed = bool_of_string (Localdb.get Constants.ha_armed) in

	if armed then begin
		debug "HA is supposed to be armed";
		(* Make sure daemons are up *)

		let finished = ref false in
		(* Do not proceed any further until the situation is resolved.
		   XXX we might need some kind of user-override *)
		while not (!finished) do

			(* If someone has called Host.emergency_ha_disable in the background then we notice the change here *)
			if not (try bool_of_string (Localdb.get Constants.ha_armed) with _ -> false) then begin
				warn "ha_start_daemon aborted because someone has called Host.emergency_ha_disable";
				failwith "Host.emergency_ha_disable"; (* failure causes HA startup to abort *)
			end;

			try
				if Xapi_fist.ha_cannot_access_statefile () then raise (Xha_error Xha_errno.Mtc_exit_can_not_access_statefile);
				if Xapi_fist.ha_daemon_startup_failed () then failwith "simulating xha daemon startup failure";

				(* CA-21406: Try again to reattach the statefile VDI *)
				Static_vdis.reattempt_on_boot_attach ();

				let (_ : string) = call_script ha_start_daemon [] in
				finished := true;
			with
				| Xha_error Xha_errno.Mtc_exit_daemon_is_present ->
					warn "ha_start_daemon failed with MTC_EXIT_DAEMON_IS_PRESENT: continuing with startup";
					finished := true;
				| Xha_error Xha_errno.Mtc_exit_invalid_pool_state as e ->
					warn "ha_start_daemon failed with MTC_EXIT_INVALID_POOL_STATE: disabling HA on this host";
					Localdb.put Constants.ha_armed "false";
					raise e
				| Xha_error Xha_errno.Mtc_exit_can_not_access_statefile as e ->
					warn "ha_start_daemon failed with MTC_EXIT_CAN_NOT_ACCESS_STATEFILE: will contact existing master and check if HA is still enabled";

					(* check the Pool.ha_enabled on the master... assuming we can find the master. If we can't we stay here forever. *)
					let master_can_confirm_ha_is_disabled () =
						try
							let address = Pool_role.get_master_address () in
							Helpers.call_emergency_mode_functions address
								(fun rpc session_id ->
									let pool = List.hd (Client.Pool.get_all rpc session_id) in
									not (Client.Pool.get_ha_enabled rpc session_id pool)
								)
						with _ ->
							(* there's no-one for us to ask about whether HA is enabled or not *)
							false in
					if master_can_confirm_ha_is_disabled () then begin
						info "Existing master confirmed that HA is disabled pool-wide: disabling HA on this host";
						Localdb.put Constants.ha_armed "false";
						raise e
					end;
					info "Assuming HA is still enabled on the Pool and that our storage system has failed: will retry in 10s";
					Xapi_globs.slave_emergency_mode := true;
					Xapi_globs.emergency_mode_error := Api_errors.Server_error(Api_errors.ha_host_cannot_access_statefile, []);
					Helpers.touch_file !Xapi_globs.ready_file;
					Thread.delay 10.;
				| Xha_error errno ->
					error "ha_start_daemon failed with unexpected error %s: will retry in 10s" (Xha_errno.to_string errno);
					Xapi_globs.slave_emergency_mode := true;
					Xapi_globs.emergency_mode_error := Api_errors.Server_error(Api_errors.ha_heartbeat_daemon_startup_failed, []);
					Helpers.touch_file !Xapi_globs.ready_file;
					Thread.delay 10.;
				| e ->
					error "ha_start_daemon failed with unexpected exception: %s -- retrying in 10s" (ExnHelper.string_of_exn e);
					Xapi_globs.slave_emergency_mode := true;
					Xapi_globs.emergency_mode_error := Api_errors.Server_error(Api_errors.ha_heartbeat_daemon_startup_failed, []);
					Helpers.touch_file !Xapi_globs.ready_file;
					Thread.delay 10.;
		done;

		if Pool_role.is_master () then begin
			if not (local_failover_decisions_are_ok ()) then begin
				warn "ha.disable_failover_decisions flag set: not proposing myself as master";
				on_master_failure ()
			end else begin
				if propose_master ()
				then info "ha_propose_master succeeded; continuing"
				else on_master_failure ()
			end
		end;

		(* Start up the redo-log if appropriate. *)
		redo_log_ha_enabled_at_startup ();

		debug "About to start the monitor";
		Monitor.start ();
		(* We signal the monitor that the database state is valid (wrt liveness + disabledness of hosts) later *)
	end

(** Called in the master xapi startup when the database is ready. We set all hosts (including this one) to
	disabled then signal the monitor thread to look. It can then wait for slaves to turn up
	before trying to restart VMs. *)
let on_database_engine_ready () =
	info "Setting all hosts to dead and disabled. Hosts must re-enable themselves explicitly";
	Server_helpers.exec_with_new_task "Setting all hosts to dead and disabled"
		(fun __context ->
			List.iter (fun self ->
				let uuid = Db.Host.get_uuid ~__context ~self in
				let hostname = Db.Host.get_hostname ~__context ~self in
				info "Host.enabled: on_database_engine_ready: setting host %s (%s) to disabled" uuid hostname;
				Db.Host.set_enabled ~__context ~self ~value:false
			)
				(Db.Host.get_all ~__context)
		);
	Monitor.signal_database_state_valid ()

(*********************************************************************************************)
(* Internal API calls to configure individual hosts                                          *)

(** Internal API call to prevent this node making an unsafe failover decision.
	This call is idempotent. *)
let ha_disable_failover_decisions __context localhost =
	debug "Disabling failover decisions";
	(* FIST *)
	if Xapi_fist.disable_ha_disable_failover () then begin
		error "FIST: ha_disable_failover_decisions";
		failwith "FIST: ha_disable_failover_decisions"
	end;
	Localdb.put Constants.ha_disable_failover_decisions "true"

(** Internal API call to disarm localhost.
	If the daemon is missing then we return success. Either fencing was previously disabled and the
	daemon has shutdown OR the daemon has died and this node will fence shortly...
 *)
let ha_disarm_fencing __context localhost =
	try
		let (_ : string) = call_script ha_disarm_fencing [] in ()
	with Xha_error Xha_errno.Mtc_exit_daemon_is_not_present ->
		info "ha_disarm_fencing: daemon has exited so returning success"

let ha_set_excluded __context localhost =
	let (_ : string) = call_script ha_set_excluded [] in ()

(** Internal API call to stop the HA daemon.
	This call is idempotent. *)
let ha_stop_daemon __context localhost =
	Monitor.stop ();
	let (_ : string) = call_script ha_stop_daemon [] in ()

(** Emergency-mode API call to disarm localhost *)
let emergency_ha_disable __context =
	if Localdb.get Constants.ha_armed = "false"
	then raise (Api_errors.Server_error(Api_errors.ha_not_enabled, []));

	warn "Host.emergency_ha_disable: Disabling the HA subsystem on the local host only.";
	Localdb.put Constants.ha_armed "false";

	begin
		try
			ha_disarm_fencing __context ();
		with Xha_error e ->
			error "Host.emergency_ha_disable: ha_disarm_fencing failed with %s; continuing" (Xha_errno.to_string e)
	end;
	begin
		try
			ha_stop_daemon __context ();
		with Xha_error e ->
			error "Host.emergency_ha_disable: ha_stop_daemon failed with %s; continuing" (Xha_errno.to_string e)
	end;
	(* Might not be able to access the database to detach statefiles; however this isn't critical *)
	()

(** Internal API call to release any HA resources after the system has
	been shutdown.  This call is idempotent. Modified for CA-48539 to
	call vdi.deactivate before vdi.detach. *)
let ha_release_resources __context localhost =
	Monitor.stop ();

	(* Why aren't we calling Xha_statefile.detach_existing_statefiles?
	   Does Db.Pool.get_ha_statefiles return a different set of
	   statefiles than Xha_statefile.list_existing_statefiles? *)

	(* Deactivate and detach all statefile VDIs in the entire pool *)
	let statefile_vdis = Db.Pool.get_ha_statefiles ~__context ~self:(Helpers.get_pool ~__context)
	and deactivate_and_detach_vdi vdi_str =
		let uuid = Db.VDI.get_uuid ~__context ~self:(Ref.of_string vdi_str) in
		Helpers.log_exn_continue
			(Printf.sprintf "detaching statefile VDI uuid: %s" uuid)
			(fun () ->
				Static_vdis.permanent_vdi_deactivate_by_uuid ~__context ~uuid ;
				Static_vdis.permanent_vdi_detach_by_uuid ~__context ~uuid) ()
	in List.iter deactivate_and_detach_vdi statefile_vdis ;

	(* Deactivate and detach any metadata VDIs *)
	Helpers.log_exn_continue
		(Printf.sprintf "deactivating and detaching metadata VDIs")
		(fun () -> Xha_metadata_vdi.deactivate_and_detach_existing ~__context) ();

	(* At this point a restart won't enable the HA subsystem *)
	Localdb.put Constants.ha_armed "false"

(** Internal API call which blocks until this node's xHA daemon spots the invalid statefile
	and exits cleanly. If the daemon survives but the statefile access is lost then this function
	will return an exception and the no-statefile shutdown can be attempted.
 *)
let ha_wait_for_shutdown_via_statefile __context localhost =
	try
		while true do
			let liveset = query_liveset () in

			let hosts = liveset.Xha_interface.LiveSetInformation.hosts in
			let me = Hashtbl.find hosts liveset.Xha_interface.LiveSetInformation.local_host_id in
			(* If we have no statefile access or if it looks corrupted, fail the operation *)
			if false
				|| not me.Xha_interface.LiveSetInformation.Host.state_file_access
				|| me.Xha_interface.LiveSetInformation.Host.state_file_corrupted
			then raise (Api_errors.Server_error(Api_errors.ha_lost_statefile, []));

			(* check to see if this node still has statefile access *)
			Thread.delay 5.
		done
	with
		| Xha_error Xha_errno.Mtc_exit_daemon_is_not_present ->
			info "ha_wait_for_shutdown_via_statefile: daemon has exited so returning success"


(** Attach the statefile VDIs and return the resulting list of paths in dom0 *)
let attach_statefiles ~__context statevdis =
	(* First GC any existing statefiles: these are not needed any more *)
	info "Detaching any existing statefiles: these are not needed any more";
	Xha_statefile.detach_existing_statefiles ~__context;

	let paths = ref [] in
	begin
		let cur_vdi_str = ref "" in
		try
			List.iter
				(fun vdi ->
					cur_vdi_str := Ref.string_of vdi;
					info "Attempting to permanently attach statefile VDI: %s" (Ref.string_of vdi);
					paths := Static_vdis.permanent_vdi_attach ~__context ~vdi ~reason:Xha_statefile.reason:: !paths) statevdis
		with e ->
			error "Caught exception attaching statefile: %s" (ExnHelper.string_of_exn e);
			List.iter
				(fun vdi ->
					Helpers.log_exn_continue
						(Printf.sprintf "detaching statefile: %s" (Ref.string_of vdi))
						(fun () -> Static_vdis.permanent_vdi_detach ~__context ~vdi) ()
				) statevdis;
			raise  (Api_errors.Server_error(Api_errors.vdi_not_available, [!cur_vdi_str]))
	end;
	!paths

(** Attach the metadata VDI and return the resulting path in dom0 *)
let attach_metadata_vdi ~__context vdi =
	info "Detaching any existing metadata volume: these are not needed anymore";
	Xha_metadata_vdi.detach_existing ~__context;
	Static_vdis.permanent_vdi_attach ~__context ~vdi ~reason:Xapi_globs.ha_metadata_vdi_reason

(** Write the local configfile *)
let write_config_file ~__context statevdi_paths generation =
	let local_heart_beat_interface = Xapi_inventory.lookup Xapi_inventory._management_interface in
	(* Need to find the name of the physical interface, so xHA can monitor the bonding status (if appropriate).
	   Note that this interface isn't used for sending packets so VLANs don't matter: the physical NIC or bond device is all we need. *)
	let localhost = Helpers.get_localhost ~__context in
	let mgmt_pifs = List.filter (fun self -> Db.PIF.get_management ~__context ~self) (Db.Host.get_PIFs ~__context ~self:localhost) in
	if mgmt_pifs = [] then failwith (Printf.sprintf "Cannot enable HA on host %s: there is no management interface for heartbeating" (Db.Host.get_hostname ~__context ~self:localhost));
	let mgmt_pif = List.hd mgmt_pifs in (* there should be only one in Orlando *)
	let local_heart_beat_physical_interface = Db.PIF.get_device ~__context ~self:mgmt_pif in

	let local_state_file = List.hd statevdi_paths in
	info "Using statefile: %s" local_state_file;

	let base_t = Timeouts.get_base_t ~__context in
	let timeouts = Timeouts.derive base_t in

	(* Rewrite HA configuration files *)
	let config = Xha_interface.DaemonConfiguration.create
		~state_file_interval:timeouts.Timeouts.state_file_interval
		~heart_beat_interval:timeouts.Timeouts.heart_beat_interval
		~state_file_timeout:timeouts.Timeouts.state_file_timeout
		~heart_beat_timeout:timeouts.Timeouts.heart_beat_timeout
		~state_file_watchdog_timeout:timeouts.Timeouts.state_file_watchdog_timeout
		~heart_beat_watchdog_timeout:timeouts.Timeouts.heart_beat_watchdog_timeout
		~boot_join_timeout:timeouts.Timeouts.boot_join_timeout
		~enable_join_timeout:timeouts.Timeouts.enable_join_timeout
		~xapi_healthcheck_interval:timeouts.Timeouts.xapi_healthcheck_interval
		~xapi_healthcheck_timeout:timeouts.Timeouts.xapi_healthcheck_timeout
		~xapi_restart_attempts:timeouts.Timeouts.xapi_restart_attempts
		~xapi_restart_timeout:timeouts.Timeouts.xapi_restart_timeout
		~common_udp_port:Xapi_globs.xha_udp_port
		~common_generation_uuid:(Uuid.uuid_of_string generation)
		~local_heart_beat_interface
		~local_heart_beat_physical_interface
		~local_state_file
		~__context
		() in

	Unixext.write_string_to_file Xha_interface.DaemonConfiguration.filename
		(Xha_interface.DaemonConfiguration.to_xml_string config);
	debug "%s file written" Xha_interface.DaemonConfiguration.filename

(** Internal API call to preconfigure localhost *)
let preconfigure_host __context localhost statevdis metadata_vdi generation =
	info "Host.preconfigure_ha host = %s; statevdis = [ %s ]; generation = %s"
		(Ref.string_of localhost) (String.concat "; " (List.map Ref.string_of statevdis)) generation;

	(* FIST *)
	if Xapi_fist.reconfigure_host () then begin
		error "FIST: fist_reconfigure_host";
		failwith "FIST: fist_reconfigure_host"
	end;

	(* Write name of cluster stack to the local DB. This determines which HA scripts we use. *)
	let pool = Helpers.get_pool ~__context in
	let cluster_stack = Db.Pool.get_ha_cluster_stack ~__context ~self:pool in
	(try
		let dir = Filename.concat !Xapi_globs.cluster_stack_root cluster_stack in
		Unix.access dir [Unix.F_OK]
	with _ ->
		failwith ("cluster stack " ^ cluster_stack ^ " not installed"));
	Localdb.put Constants.ha_cluster_stack cluster_stack;

	Db.Host.set_ha_statefiles ~__context ~self:localhost ~value:(List.map Ref.string_of statevdis);

	(* The master has already attached the statefile VDIs and written the
	   configuration file. *)
	if not(Pool_role.is_master ()) then begin
		let statefiles = attach_statefiles ~__context statevdis in
		write_config_file ~__context statefiles generation;

		(* It's unnecessary to remember the path since this can be queried dynamically *)
		ignore(attach_metadata_vdi ~__context metadata_vdi);
	end;

	write_uuid_to_ip_mapping ~__context;

	let base_t = Timeouts.get_base_t ~__context in
	Localdb.put Constants.ha_base_t (string_of_int base_t)

let join_liveset __context host =
	info "Host.ha_join_liveset host = %s" (Ref.string_of host);
	let (_ : string) = call_script ha_start_daemon [] in
	Localdb.put Constants.ha_disable_failover_decisions "false";
	Localdb.put Constants.ha_armed "true";
	info "Local flag ha_armed <- true";

	(* If this host is the current master then it must assert its authority as master;
	   otherwise another host's heartbeat thread might conclude that the master has gone
	   and propose itself. This would lead the xHA notion of master to immediately diverge
	   from the XenAPI notion. *)
	if Pool_role.is_master () then begin
		if not (propose_master ())
		then failwith "failed to propose the current master as master";
		info "ha_propose_master succeeded; continuing";
	end else begin
		(* If this host is a slave then we must wait to confirm that the master manages to
		   assert itself, otherwise our monitoring thread might attempt a hostile takeover *)
		let master_address = Pool_role.get_master_address () in
		let master_uuid = Uuid.uuid_of_string (uuid_of_host_address master_address) in
		let master_found = ref false in
		while not !master_found do
			(* It takes a non-trivial amount of time for the master to assert itself: we might
			   as well wait here rather than enumerating all the if/then/else branches where we
			   should wait. *)
			Thread.delay 5.;
			let liveset = query_liveset () in
			debug "Liveset: %s" (Xha_interface.LiveSetInformation.to_summary_string liveset);
			if liveset.Xha_interface.LiveSetInformation.status = Xha_interface.LiveSetInformation.Status.Online then begin
				(* 'master' is the node we believe should become the xHA-level master initially *)
				let master = Hashtbl.find liveset.Xha_interface.LiveSetInformation.hosts master_uuid in
				if master.Xha_interface.LiveSetInformation.Host.master then begin
					info "existing master has successfully asserted itself";
					master_found := true (* loop will terminate *)
				end else begin
					if false
						|| not master.Xha_interface.LiveSetInformation.Host.liveness
						|| master.Xha_interface.LiveSetInformation.Host.state_file_corrupted
						|| not master.Xha_interface.LiveSetInformation.Host.state_file_access
						|| master.Xha_interface.LiveSetInformation.Host.excluded then begin
							error "Existing master has failed during HA enable process";
							failwith "Existing master failed during HA enable process"
						end else debug "existing master has not yet asserted itself: looking again in 5s";
				end
			end else debug "liveset is not yet online: looking again in 5s";
		done
	end;

	debug "About to start the monitor";
	Monitor.start ();
	Monitor.signal_database_state_valid ()

(* The last proposal received *)
let proposed_master : string option ref = ref None
	(* The time the proposal was received. XXX need to be quite careful with timeouts to handle
	   the case where the proposed new master dies in the middle of the protocol. Once we believe
	   he has fenced himself then we can abort the transaction. *)
let proposed_master_time = ref 0.

let proposed_master_m = Mutex.create ()

(* This should be called under proposed_master_m *)
let rec propose_new_master_internal ~__context ~address ~manual =
	(* Handy function to throw the right API error *)
	let issue_abort reason =
		raise (Api_errors.Server_error (Api_errors.ha_abort_new_master, [ reason ]))
	in
	match !proposed_master with
		| Some x when address = x ->
			proposed_master_time := Unix.gettimeofday ()
		| Some x -> begin
			(* XXX: check if we're past the fencing time *)
			let now = Unix.gettimeofday () in
			let diff = now -. !proposed_master_time in
			let ten_minutes = 10. *. 60. in (* TO TEST: change to 60 secs *)

			if diff > ten_minutes
			then begin
				proposed_master := None;
				propose_new_master_internal ~__context ~address ~manual
			end else
				issue_abort (Printf.sprintf "Already agreed to commit host address '%s' at %s ('%f' secs ago)"
					x (Date.to_string (Date.of_float !proposed_master_time)) diff)
		end
		| None ->
			(* XXX no more automatic transititions *)

			proposed_master := Some address;
			proposed_master_time := Unix.gettimeofday ()

(* First phase of a two-phase commit of a new master *)
let propose_new_master ~__context ~address ~manual =
	Mutex.execute proposed_master_m
		(fun () -> propose_new_master_internal ~__context ~address ~manual)

(* Second phase of a two-phase commit of a new master *)
let commit_new_master ~__context ~address =
	begin match !proposed_master with
		| Some x when x <> address ->
			let msg = Printf.sprintf "Received commit_new_master(%s) but previously received proposal for %s" address x in
			error "%s" msg;
			raise (Api_errors.Server_error(Api_errors.ha_abort_new_master, [ msg ]))
		| None ->
			let msg = Printf.sprintf "Received commit_new_master(%s) but never received a proposal" address in
			error "%s" msg;
			raise (Api_errors.Server_error(Api_errors.ha_abort_new_master, [ msg ]))
		| Some _ -> debug "Setting new master address to: %s" address;
	end;

	Mutex.execute proposed_master_m
		(fun () ->
			(* NB we might not be in emergency mode yet, so not identical to
			   Xapi_pool.emergency_reset_master *)
			if Helpers.this_is_my_address ~__context address
			then Xapi_pool_transition.become_master ()
			else Xapi_pool_transition.become_another_masters_slave address)

let abort_new_master ~__context ~address =
	Mutex.execute proposed_master_m
		(fun () ->
			if !proposed_master = None
			then error "Received abort_new_master %s but we never saw the original proposal" address;
			proposed_master := None)


(*********************************************************************************************)
(* External API calls ( Pool.* )                                                             *)

(* Called either from the API call Pool.ha_disable or from Pool.ha_enable in a failure case.
   This function does its best to turn everything off such that if a failure occurs then the
   remaining nodes cannot survive (either they see the poisoned statefile or they don't see
   a full heartbeat partition) *)
let disable_internal __context =
	debug "Disabling HA on the Pool";

	let pool = Helpers.get_pool ~__context in

	(* Find the HA metadata and statefile VDIs for later *)
	let statefile_vdis = List.map Ref.of_string (Db.Pool.get_ha_statefiles ~__context ~self:pool) in
	let metadata_vdis = List.map (fun x -> Db.VDI.get_by_uuid ~__context ~uuid:x.Static_vdis.uuid) (Xha_metadata_vdi.list_existing ()) in

	redo_log_ha_disabled_during_runtime __context;

	(* Steps from 8.6 Disabling HA
	   If the master has access to the state file (how do we determine this)?
	   * ha_set_pool_state(invalid)
	   If the master hasn't access to the state file but all hosts are available via heartbeat
	   * set the flag "can not be master and no VM failover decision on next boot"
	   * ha_disarm_fencing()
	   * ha_stop_daemon()
	   Otherwise we'll be fenced *)

	let hosts = Db.Host.get_all ~__context in

	(** Attempt the HA disable via the statefile, returning true if successful and false
		otherwise. If false then we'll retry with the no-statefile procedure *)
	let attempt_disable_through_statefile () =
		info "I have statefile access -- setting pool state to invalid";

		(* FIST *)
		if Xapi_fist.disable_ha_via_statefile () then begin
			error "FIST: attempt_disable_through_statefile";
			failwith "FIST: attempt_disable_through_statefile"
		end;

		begin
			try
				let (_: string) = call_script ha_set_pool_state [ "invalid" ] in ()
			with e ->
				error "Caught exception %s while setting pool state to invalid-- retrying with no-statefile procedure" (ExnHelper.string_of_exn e);
				raise e
		end;

		(* Normally all hosts would now see the invalid pool state and gracefully disarm fencing
		   and stop their HA daemons. If the statefile disappears for *all* hosts then the hosts
		   could remain alive by Survival Rule 2, if none of them (including us!) saw the invalid state.

		   [XXX: can the HA daemon on this node fail to notice the invalid state?]

		   We can prevent this by explicitly stopping our HA daemon now -- this will cause remaining
		   nodes to self-fence if the statefile disappears. *)
		Helpers.log_exn_continue
			"stopping HA daemon on the master after setting pool state to invalid"
			(fun () -> ha_stop_daemon __context (Helpers.get_localhost ~__context)) ();

		(* No node may become the master automatically without the statefile so we can safely change
		   the Pool state to disabled *)
		Db.Pool.set_ha_plan_exists_for ~__context ~self:pool ~value:0L;
		Db.Pool.set_ha_enabled ~__context ~self:pool ~value:false;
		info "Pool.ha_enabled <- false";

		(* The rest of the shutdown is necessarily best-effort: errors are logged but the operation
		   will succeed anyway. Noone will perform any failover actions. *)

		Helpers.call_api_functions ~__context
			(fun rpc session_id ->
				(* Wait for each host to shutdown via the statefile *)
				let errors = thread_iter_all_exns
					(fun host ->
						debug "Waiting for host '%s' ('%s') to see invalid statefile" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
						Client.Host.ha_wait_for_shutdown_via_statefile rpc session_id host
					) hosts in
				(* Print all the errors that happened *)
				List.iter
					(fun (host, e) ->
						error "Host '%s' ('%s') failed to diable HA via statefile; if node has statefile access it will disarm; if not it will self-fence (%s)" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host) (ExnHelper.string_of_exn e)
					) errors
			) in

	(** Attempt the HA disable without the statefile. *)
	let attempt_disable_without_statefile () =

		(* This is the no-statefile procedure: *)
		Helpers.call_api_functions ~__context
			(fun rpc session_id ->
				(* By disabling failover decisions (through ha_disable_failover_decisions) we prevent a
				   failure leaving some hosts with their fencing disabled, causing potential split-brain
				   and VM corruption.
				   We cannot continue unless all hosts have completed this operation.
				   Transient failures (due to temporary network blips) may cause this operation to fail
				   in which case the user will have to retry. Permanent network outtages will cause all
				   nodes to self-fence. *)
				let errors = thread_iter_all_exns
					(fun host ->
						debug "Disabling all failover decisions on host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
						Client.Host.ha_disable_failover_decisions rpc session_id host) hosts in
				List.iter
					(fun (host, e) ->
						error "Host '%s' ('%s') failed to diable failover decisions: %s" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host) (ExnHelper.string_of_exn e)
					) errors;
				if errors <> [] then raise (snd (List.hd errors));

				(* From this point no host will attempt to become the new master so no split-brain.

				   This also means that we own the pool database and can safely set ha_enabled to false,
				   knowing that, although each slave has a backup database where ha_enabled is still true,
				   they won't be able to rollback our change because they cannot become the master.

				   NB even if we fail to disarm fencing on individuals then the worst that will happen
				   is that they will fail and fence themselves. When they come back they will either
				   resynchronise HA state with us and disarm themselves, or if we've failed the situation
				   is equivalent to a master failure without HA. *)
				Db.Pool.set_ha_enabled ~__context ~self:pool ~value:false;
				info "Pool.ha_enabled <- false";

				let errors = thread_iter_all_exns
					(fun host ->
						debug "Disarming fencing on host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
						Client.Host.ha_disarm_fencing rpc session_id host
					) hosts in
				List.iter
					(fun (host, e) ->
						error "Failed to disarm fencing on host '%s' ('%s'); this means the host may well be about to fence itself even though HA is officially disabled on the Pool (%s)" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host) (ExnHelper.string_of_exn e)
					) errors;

				let errors = thread_iter_all_exns
					(fun host ->
						debug "Stopping HA daemon on host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
						Client.Host.ha_stop_daemon rpc session_id host
					) hosts in
				List.iter
					(fun (host, e) ->
						error "Failed to stop daemon on host '%s' ('%s') even though HA is officially disabled on the Pool (%s)" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host) (ExnHelper.string_of_exn e)
					) errors
			) in

	try

		let do_one_attempt () =
			(* Have a go at disabling HA. If we're sure we've done it then return true. If we suffer a partial
			   failure (which may leave some nodes with their failover actions disabled) then return false. *)
			let exn_to_bool f = try f (); true with _ -> false in
			(* Check if the statefile exists and try that first. If it succeeds then we're done. If it fails or wasn't attempted
			   then we need to try the without-statefile procedure: *)
			(if i_have_statefile_access ()
			then exn_to_bool attempt_disable_through_statefile
			else false)
			|| (exn_to_bool attempt_disable_without_statefile) in

		(* CA-16296: if we temporarily lose access to the statefile and attempt the non-statefile procedure
		   we will fail if some nodes cannot be contacted to have their failover decision flag set. If the
		   statefile comes back then the pool can become stable again but with some nodes crippled by the
		   failover decision flag. If this partial failure happens we keep trying forever to disable HA. *)
		while not(do_one_attempt ()) do
			error "Suffered a partial failure during HA disable procedure. Will try again in 30s";
			Thread.delay 30.
		done;

		(* Assuming all is well then we can release resources on all hosts *)
		Helpers.call_api_functions ~__context
			(fun rpc session_id ->
				let errors = thread_iter_all_exns
					(fun host ->
						debug "Releasing resources on host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
						Client.Host.ha_release_resources rpc session_id host
					) hosts in
				List.iter
					(fun (host, e) ->
						error "Failed to release HA resources on host '%s' ('%s') (%s)" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host) (ExnHelper.string_of_exn e)
					) errors
			);
		(* Update the allowed operations on the statefile VDIs for tidiness *)
		List.iter (fun vdi -> Xapi_vdi.update_allowed_operations ~__context ~self:vdi) (metadata_vdis @ statefile_vdis);

	with exn ->
		error "Caught exception while disabling HA: %s" (ExnHelper.string_of_exn exn);
		error "Pool.ha_enabled = %b [but some hosts may be out of sync]" (Db.Pool.get_ha_enabled ~__context ~self:pool);
		raise exn

let disable __context =
	let pool = Helpers.get_pool ~__context in
	if not(Db.Pool.get_ha_enabled ~__context ~self:pool)
	then raise (Api_errors.Server_error(Api_errors.ha_not_enabled, []));
	disable_internal __context

open Db_cache_types (* for the Manifest. Database. functions below *)

let enable __context heartbeat_srs configuration =
	debug "Enabling HA on the Pool.";
	let pool = Helpers.get_pool ~__context in
	if Db.Pool.get_ha_enabled ~__context ~self:pool
	then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []));

	Pool_features.assert_enabled ~__context ~f:Features.HA;

	(* Check that all of our 'disallow_unplug' PIFs are currently attached *)
	let unplugged_ununpluggable_pifs = Db.PIF.get_refs_where ~__context ~expr:(And (
		Eq (Field "disallow_unplug", Literal "true"),
		Eq (Field "currently_attached", Literal "false")
	)) in
	if List.length unplugged_ununpluggable_pifs > 0 then
		raise (Api_errors.Server_error(Api_errors.required_pif_is_unplugged,
		(List.map (fun pif -> Ref.string_of pif) unplugged_ununpluggable_pifs)));

	(* Check also that any PIFs with IP information set are currently attached - it's a non-fatal
	   error if they are, but we'll warn with a message *)
	let pifs_with_ip_config = Db.PIF.get_records_where ~__context ~expr:(
		Not (Eq (Field "ip_configuration_mode", Literal "None"))
	) in
	let not_bond_slaves = List.filter (fun (_,pifr) -> not (Db.is_valid_ref __context pifr.API.pIF_bond_slave_of)) pifs_with_ip_config in
	let without_disallow_unplug = List.filter (fun (_,pifr) -> not (pifr.API.pIF_disallow_unplug || pifr.API.pIF_management)) not_bond_slaves in
	if List.length without_disallow_unplug > 0 then begin
		let pifinfo = List.map (fun (pif,pifr) -> (Db.Host.get_name_label ~__context ~self:pifr.API.pIF_host, pif, pifr)) without_disallow_unplug in
		let bodylines =
			["A possible network anomaly was found. The following hosts possibly have storage PIFs that are not dedicated:"] @
				(List.map (fun (hostname,pif,pifr) -> Printf.sprintf "%s: %s (uuid: %s)" hostname pifr.API.pIF_device pifr.API.pIF_uuid) pifinfo)
		in
		warn "Warning: A possible network anomaly was found. The following hosts possibly have storage PIFs that can be unplugged: %s"
			(String.concat ", " bodylines);
		let (name, priority) = Api_messages.ip_configured_pif_can_unplug in
		ignore(Xapi_message.create ~__context ~name ~priority ~cls:`Pool ~obj_uuid:(Db.Pool.get_uuid ~__context ~self:(Helpers.get_pool ~__context))
			~body:(String.concat "\n" bodylines))
	end;

	(* Fail if any host is offline. Otherwise we end up with an Xha_errno(bootjoin_timeout) *)
	List.iter (fun host ->
		let alive =
			try
				let hm = Db.Host.get_metrics ~__context ~self:host in
				Db.Host_metrics.get_live ~__context ~self:hm
			with _ -> false in
		if not alive then raise (Api_errors.Server_error(Api_errors.host_offline, [ Ref.string_of host ]))
	) (Db.Host.get_all ~__context);

	let pool = Helpers.get_pool ~__context in

	let cluster_stack = Cluster_stack_constraints.choose_cluster_stack ~__context in
	Db.Pool.set_ha_cluster_stack ~__context ~self:pool ~value:cluster_stack;
	Localdb.put Constants.ha_cluster_stack cluster_stack;

	(* Steps from 8.7 Enabling HA in Marathon spec:
	 * 1. Bring up state file VDI(s)
	 * 2. Clear the flag "can not be master and no VM failover decision on next boot"
	 * 3. XAPI stops its internal heartbeats with other hosts in the pool
	 * 4. ha_set_pool_state(init) *)

	let statefile_vdis = ref [] in
	let database_vdis = ref [] in
	try
		(* 1a. Create state file VDIs *)
		let possible_srs = if heartbeat_srs = [] then Xha_statefile.list_srs_which_can_host_statefile ~__context ~cluster_stack else heartbeat_srs in
		if List.length possible_srs = 0
		then raise (Api_errors.Server_error(Api_errors.cannot_create_state_file, []));

		(* For the moment we'll create a state file in one compatible SR since the xHA component only handles one *)
		let srs = [ List.hd possible_srs ] in
		List.iter
			(fun sr ->
				let vdi = Xha_statefile.find_or_create ~__context ~sr ~cluster_stack in
				statefile_vdis := vdi :: !statefile_vdis;
			) srs;
		(* For storing the database, assume there is only one SR *)
		let database_vdi = Xha_metadata_vdi.find_or_create ~__context ~sr:(List.hd srs) in
		database_vdis := database_vdi :: !database_vdis;

		(* Record the statefile UUIDs in the Pool.ha_statefile set *)
		Db.Pool.set_ha_statefiles ~__context ~self:pool ~value:(List.map Ref.string_of !statefile_vdis);

		(* Record the extra configuration in the Pool.ha_configuration map *)
		Db.Pool.set_ha_configuration ~__context ~self:pool ~value:configuration;

		(* Update the Pool's planning configuration (ha_overcommitted, ha_plan_exists_for) *)
		(* Start by assuming there is no ha_plan_for: this can be revised upwards later *)
		Db.Pool.set_ha_plan_exists_for ~__context ~self:pool ~value:0L;
		let (_: bool) = Xapi_ha_vm_failover.update_pool_status ~__context () in

		let generation = Uuid.string_of_uuid (Uuid.make_uuid ()) in

		let hosts = Db.Host.get_all ~__context in

		(* This code always runs on the master *)
		let statefiles = attach_statefiles ~__context (!statefile_vdis) in
		write_config_file ~__context statefiles generation;
		let (_: string) = call_script ha_set_pool_state [ "init" ] in

		(* It's unnecessary to remember the path since this can be queried dynamically *)
		ignore(attach_metadata_vdi ~__context database_vdi);

		(* To make a progress bar we keep track of the total API calls *)
		let task = Context.get_task_id __context in
		let total_calls = List.length hosts * 3 in
		let count_call =
			let task_m = Mutex.create () in
			let call_count = ref 0 in
			fun () ->
				Mutex.execute task_m
					(fun () ->
						incr call_count;
						Db.Task.set_progress ~__context ~self:task ~value:(float_of_int !call_count /. (float_of_int total_calls))
					) in

		Helpers.call_api_functions ~__context
			(fun rpc session_id ->
				(* ... *)
				(* Tell each host to attach its statefile, write config files etc. Do not start the xHA daemon. *)
				List.iter
					(fun host ->
						try
							debug "Preconfiguring HA on host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
							Client.Host.preconfigure_ha rpc session_id host !statefile_vdis database_vdi generation;
							count_call ()
						with e ->
							error "Caught exception while calling Host.preconfigure_ha: '%s' ('%s') %s" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host) (ExnHelper.string_of_exn e);
							(* Perform a disable since the pool HA state isn't consistent *)
							error "Attempting to disable HA pool-wide";
							Helpers.log_exn_continue "Disabling HA after a failure during the configuration stage" disable_internal __context;
							raise e
					) hosts;

				let errors = thread_iter_all_exns
					(fun host ->
						debug "host '%s' ('%s') will attempt to join the liveset" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
						Client.Host.ha_join_liveset rpc session_id host;
						count_call ()
					) hosts in
				List.iter
					(fun (host, e) ->
						error "Caught exception while calling Host.ha_join_liveset: '%s' ('%s') %s" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host) (ExnHelper.string_of_exn e)
					) errors;
				if errors <> [] then begin
					(* Perform a disable since the pool HA state isn't consistent *)
					error "Attempting to disable HA pool-wide";
					Helpers.log_exn_continue "Disabling HA after a failure joining all hosts to the liveset" disable_internal __context;
					raise (snd (List.hd errors))
				end;

				(* We have to set the HA enabled flag before forcing a database resynchronisation *)
				Db.Pool.set_ha_enabled ~__context ~self:pool ~value:true;
				debug "HA enabled";

				(* Enable writing to the redo-log *)
				redo_log_ha_enabled_during_runtime __context;

				(* ... *)
				(* Make sure everyone's got a fresh database *)
				let generation = Db_lock.with_lock (fun () -> Manifest.generation (Database.manifest (Db_ref.get_database (Db_backend.make ())))) in
				let errors = thread_iter_all_exns
					(fun host ->
						debug "Synchronising database with host '%s' ('%s')" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host);
						Client.Host.request_backup rpc session_id host generation true;
						count_call ()
					) hosts in
				List.iter
					(fun (host, e) ->
						error "Caught exception while calling Host.request_backup: '%s' ('%s') %s" (Db.Host.get_name_label ~__context ~self:host) (Ref.string_of host) (ExnHelper.string_of_exn e)
					) errors;
				if errors <> [] then begin
					(* Perform a disable since the pool HA state isn't consistent *)
					error "Attempting to disable HA pool-wide";
					Helpers.log_exn_continue "Disabling HA after a failure during enable" disable_internal __context;
					raise (snd (List.hd errors))
				end;

				(* Update the allowed_operations on the HA volumes to prevent people thinking they can mess with them *)
				List.iter (fun vdi -> Xapi_vdi.update_allowed_operations ~__context ~self:vdi) (!database_vdis @ !statefile_vdis);
			); (* end of api call *)

	with exn ->
		debug "Caught exception while enabling HA: %s" (ExnHelper.string_of_exn exn);
		(* We don't destroy the statefile VDIs, preferring to leave these around for the next
		   time enable is called. Hopefully any confused host which reads the statefile will
		   notice the invalid state and disable its HA *)
		raise exn


(* Called before shutting down or rebooting a host *)
let before_clean_shutdown_or_reboot ~__context ~host =
	let pool = Helpers.get_pool ~__context in
	if Db.Pool.get_ha_enabled ~__context ~self:pool then begin

		(* The XenServer HA interface spec recommends that we set this node to excluded
		   only after we disarm fencing and stop the HA daemon, since otherwise we'll self-fence
		   on the next watchdog timeout, which is too soon for a clean shutdown.

		   One problem is that ha_set_excluded will fail if this node does not have statefile access,
		   which would leave us running with no fencing. The suggested solution is to first check
		   if we have statefile access and abort (to avoid killing the whole pool in the case
		   where everyone has lost the statefile). If we do have statefile access initially but
		   then we lose it and ha_set_excluded fails, manually fence ourselves. *)

		(* Safe early abort if we don't have statefile access *)
		let liveset = query_liveset () in
		let me = Hashtbl.find liveset.Xha_interface.LiveSetInformation.hosts
			liveset.Xha_interface.LiveSetInformation.local_host_id in
		if false
			|| not(me.Xha_interface.LiveSetInformation.Host.state_file_access)
			|| me.Xha_interface.LiveSetInformation.Host.state_file_corrupted
		then raise (Api_errors.Server_error(Api_errors.ha_lost_statefile, []));

		(* From this point we will fence ourselves if any unexpected error occurs *)
		try
			begin
				try ha_disarm_fencing __context host
				with Xha_error Xha_errno.Mtc_exit_daemon_is_not_present ->
					info "Ignoring MTC_EXIT_DAEMON_IS_NOT_PRESENT error while disarming fencing"
			end;
			begin
				try ha_stop_daemon __context host
				with Xha_error Xha_errno.Mtc_exit_daemon_is_not_present ->
					info "Ignoring MTC_EXIT_DAEMON_IS_NOT_PRESENT error while stopping xHA daemon"
			end;

			ha_set_excluded __context host;
			info "This node has been marked as excluded. Proceeding to shutdown";
		with e ->
			(* UNLIKELY to happen but we do our best to kill ourselves and do not return *)
			error "Error past the commit-point while cleanly shutting down host: %s" (ExnHelper.string_of_exn e);
			error "Host will self-fence via its own watchdog for safety";
			(* NB we don't use Xenctrl directly because in the SDK VM this is all fake... *)
			ignore(Forkhelpers.execute_command_get_output !Xapi_globs.fence [ "yesreally" ]);
			Thread.delay 60.;
			error "Watchdog has not triggered after 60 seconds";
			(* Attempt to issue a reboot and kill the control stack *)
			Xapi_fuse.light_fuse_and_reboot ();
			info "Waiting for reboot";
			let start = Unix.gettimeofday () in
			while true do
				Thread.delay 300.;
				info "Still waiting to reboot after %.2f seconds" (Unix.gettimeofday () -. start)
			done
	end
