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
(* The ways we can run out of space are:

   1. VDI.create of a volume that is exactly the same size as the free space (the VHD meta data would push it over the edge)
   2. VDI.resize to just over the space allowed
   3. Coalesce failing to create a relink journal
   4. Coalesce failing to inflate the grandparent VHD
   5. VDI.attach of a snapshot VDI failing to inflate

For each way by which we may run out of space:

   1. Create a scenario where the failure can happen by filling the SR with VDIs
   2. Cause the failure
   3. Free up some space
   4. Check the SR is operable by performing a VDI.create and attach to dom0
   5. Verify no stale journals exist in the SR  *)

open Globs
open Client
open Utils
open Listext
open Lvhdrt_exceptions

let test_name = "TC-8707"
let test_descr = "Space Check"

(* different kinds of VDIs which will populate the SR during the test *)
type kind = 
	| Raw 
	| VHD 
	| Tree of int * int  (* number of inner nodes x ratio of their total size compare to the free space *)

type vdi_linear = {
	name_label: string; (* the name-label of the VDIs *)
	ratio: int;         (* the percentage of current free space which has to be filled in total *)
	number: int;        (* the number of VDIs which have to be created *)
	kind: kind }

let fillup_VDIs = {
	name_label = "LVHDRT_fillup";
	ratio = 75;
	number = 10;
	kind = VHD }

let tmp_VDIs = {
	name_label = "LVHDRT_tmp";
	ratio = 50;
	number = 5;
	kind = VHD }

let vdi_create_test = {
	name_label = "LVHDRT_VDI.create";
	ratio = 100;
	number = 1;
	kind = VHD }

let raw_vdi_create_test = {
	name_label = "LVHDRT_VDI_raw.create";
	ratio = 100;
	number = 1;
	kind = Raw }

let vdi_resize_test = {
	name_label = "LVHDRD_VDI.create";
	ratio = 50;
	number = 1;
	kind = VHD }

let coalesce_test = {
	name_label = "LVHDRT_coalesce_test";
	ratio = 70;
	number = 3;
	kind = Tree (2,10) } 

let vdi_create_for_snapshot = {
	name_label = "LVHDRT_VDI.for_snapshot";
	ratio = 30;
	number = 1;
	kind = VHD }

(* parameters of all of the function call in this file *)
type params = {
	rpc: Rpc.call -> Rpc.response;
	session: API.ref_session;
	sr: API.ref_SR }

let sm_config_of_kind = function
	| Raw -> ["type","raw"]
	| VHD | Tree _ -> []

let make_vdi params name_label size kind = 
	Client.VDI.create ~rpc:params.rpc ~session_id:params.session ~name_label
		~name_description:"" ~sR:params.sr ~virtual_size:size ~_type:`ephemeral (*!*)
		~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[]
		~sm_config:(sm_config_of_kind kind) ~tags:[]

let get_free_space params =
	Utils.get_free_space params.rpc params.session params.sr

let wait_for_fist params fist =
	Utils.wait_for_fist params.rpc params.session params.sr ~delay:120.0 fist

let wait_for_vdi_deletion params ?(delay=90.) vdi =
	Utils.wait_for_vdi_deletion params.rpc params.session params.sr ~delay vdi

let get_actual_free_space params =
	let master = get_master params.rpc params.session in
	let sruuid = Client.SR.get_uuid params.rpc params.session params.sr in
	let actual_size_str = Client.Host.call_plugin params.rpc params.session master Globs.helper_plugin "get_sr_vfree" [ "sruuid", sruuid ] in
	try
		Int64.of_string actual_size_str
	with Failure _ ->
		failwith "Error: could not determine free space from 'vgs' via plugin"

let wait_for_physical_utilisation params expected_free_space =
	let delay = 10 in
	let max_delay = 60 * 5 in
	let rec aux n =
		(* Find out the actual physical utilisation by invoking "vgs" via a plugin *)
		let free_space = get_actual_free_space params in
		debug "# Current (physical) free space: %s; Expected free space: %s" (size_of free_space) (size_of expected_free_space);
		
	 	if n > max_delay then
			failwith "timeout: didn't reach expected free space";

		if expected_free_space > free_space then begin
			Thread.delay (float delay);
			aux (n+delay) 
		end in
	aux 0

let wait_for_SR_utilisation_update params expected_free_space =
	let delay = 10 in
	let max_delay = 60 * 5 in
	let rec aux n =
		let free_space = get_free_space params in
		debug "# Current free space: %s; Expected free space: %s" (size_of free_space) (size_of expected_free_space);
		
		if n > max_delay then
			failwith "timeout: SR.physical-utilisation is not correctly updated";

		if expected_free_space > free_space then begin
			Thread.delay (float delay);
			aux (n+delay) 
		end in
	aux 0

let with_diff_free_space params f =
	let old_free_space = get_free_space params in
	let x = f () in
	let new_free_space = get_free_space params in
	if old_free_space > new_free_space then
		debug "# Current free space: %s Changes: -%s" (size_of new_free_space) (size_of (Int64.sub old_free_space new_free_space))
	else
		debug "# Current free space: %s Changes: +%s" (size_of new_free_space) (size_of (Int64.sub new_free_space old_free_space));
	x

let (%%) x r =
	Int64.div (Int64.mul x (Int64.of_int r)) 100L

(* Create 'n' VDIs named 'name', of size 'size/n' *)
let create_VDIs params vdi_kind =
	with_diff_free_space params
		(fun () -> 
			let size = get_free_space params in
			let vdi_size = Int64.div (size %% vdi_kind.ratio) (Int64.of_int vdi_kind.number) in
		
			match vdi_kind.kind with
			| Raw | VHD ->
					let s = Printf.sprintf "Creating %i VDIs of size %s with name-label='%s': " vdi_kind.number (size_of vdi_size) vdi_kind.name_label in
					for i = 1 to vdi_kind.number do 
						debug "%s%i/%i" s i vdi_kind.number;
						ignore (make_vdi params vdi_kind.name_label vdi_size vdi_kind.kind);
					done;
					debug "%sdone" s
			| Tree (inner_nodes, pattern_ratio) ->
					let pattern_size = Int64.div (size %% pattern_ratio) (Int64.of_int inner_nodes) in
					(* Pattern 3 ensures data is written in each 2 MB. Hence the VDI will be fully inflated since the block size is 2 MB. *)
					ignore (Utils.create_vdi_tree params.rpc params.session params.sr vdi_kind.name_label vdi_size ~pattern_type:3 pattern_size))

(* Destroy all the VDIs named 'name' *)
let destroy_VDIs params vdi_kind =
	with_diff_free_space params
		(fun () ->
			let vdis = Client.VDI.get_by_name_label params.rpc params.session vdi_kind.name_label in

			if vdis = [] then
				raise (Test_error (Printf.sprintf "VDI.destroy: unknown VDI named %s!" vdi_kind.name_label));

			(* compute the expected SR utilisation after the VDI deletion *)
			let free_space = get_free_space params in
			let sizes = List.fold_left 
				(fun accu vdi -> Int64.add accu (Client.VDI.get_physical_utilisation params.rpc params.session vdi))
				0L vdis
			in
			let expected_free_space = Int64.add free_space sizes in
			debug "free_space: %s; sizes: %s; expected_free_space: %s" (size_of free_space) (size_of sizes) (size_of expected_free_space);

			let n = List.length vdis in
			let s = Printf.sprintf "Removing %i VDIs with name-label='%s': " n vdi_kind.name_label in
			
			(* delete the VDIs *)
			List.iteri 
				(fun i vdi -> 
					debug "%s%i/%i" s (i+1) n;
					Client.VDI.destroy params.rpc params.session vdi)
				vdis;
			debug "%sdone" s;

			(* wait for the SR really deletes the VDIs *)
			if vdis <> [] then
				wait_for_SR_utilisation_update params expected_free_space)

let snapshot_VDI params vdi_kind =
	with_diff_free_space params
		(fun () ->
			let vdis = Client.VDI.get_by_name_label params.rpc params.session vdi_kind.name_label in
			let num_found = List.length vdis in

			if num_found <> 1 then
				raise (Test_error (Printf.sprintf "VDI.snapshot: we found %d VDIs named %s; expecting 1" num_found vdi_kind.name_label));
			let vdi = List.hd vdis in

			let new_vdi = Client.VDI.snapshot ~rpc:params.rpc ~session_id:params.session ~vdi:vdi ~driver_params:[] in
			debug "Snapshotted VDI with name-label='%s'" vdi_kind.name_label;
			new_vdi)

(* Check if we can create a small VDI (1Mo) and attach it to dom0. *)
let check_SR_is_operable params =
	try
		Utils.with_new_vdi params.rpc params.session params.sr meg "LVHDRT_check_SR_is_operable" ""
			(fun vdi -> Utils.with_attached_vdi params.rpc params.session vdi 
				(fun _ vdi -> debug "-> The SR is still operable (OK)"))
	with _ ->
		failwith (Printf.sprintf "FAILURE: The SR is no more operable (%s)" test_name)

let verify_no_stale_journals_exist params =
	let master = get_master params.rpc params.session in
	let sruuid = Client.SR.get_uuid params.rpc params.session params.sr in
	(* The plugin returns a comma-separated list of volumes except MGT and VHD and LV volumes *)
	let remaining_journals = Client.Host.call_plugin params.rpc params.session master Globs.helper_plugin "get_journals" [ "sruuid", sruuid ] in
	if remaining_journals = "" then debug "-> No stale journals exist (OK)"
	else failwith (Printf.sprintf "FAILURE: Stale journals still exist: [%s]" remaining_journals)

let invoke_sr_scan params =
	Client.SR.scan params.rpc params.session params.sr

let to_remove (vdi_ref, vdi_record) = 
	List.mem_assoc "to_remove" vdi_record.API.vDI_other_config

(* Return a list of the VDIs which are parents of the VDIs in the given list *)
let parents_of params vdis =
	List.fold_left
		(fun accu (vdi, vdi_rec) ->
			let sm_config = vdi_rec.API.vDI_sm_config in
			if List.mem_assoc Utils.vhd_parent sm_config then begin
				let uuid = List.assoc Utils.vhd_parent sm_config in
				let vdi_ref = Client.VDI.get_by_uuid params.rpc params.session uuid in
				let vdi_rec = Client.VDI.get_record params.rpc params.session vdi_ref in
				(vdi_ref, vdi_rec) :: accu
			end else accu
		)
		[] vdis

(* Return a list of the VDIs which are children of the VDIs in the given list *)
let children_of params vdis =
	List.filter
		(fun (vdi, vdi_rec) ->
			let sm_config = vdi_rec.API.vDI_sm_config in
			if List.mem_assoc Utils.vhd_parent sm_config then begin
				let uuid = List.assoc Utils.vhd_parent sm_config in
				let vdi_ref = Client.VDI.get_by_uuid params.rpc params.session uuid in
				List.mem vdi_ref (List.map fst vdis)
			end else false)
		(get_my_vdis params.rpc params.session params.sr)

let dump_forest params =
	let vdis = Client.SR.get_VDIs params.rpc params.session params.sr in
	let f = Utils.construct_forest params.rpc params.session vdis in
	Utils.dump_forest f

module Scenario =
struct

	let with_space_error error f =
		try 
			f ();
			failwith error
		with Api_errors.Server_error _ as x ->
			debug "Expected error: %s" (Printexc.to_string x)
				
	let vdi_create params =
		debug "TEST: VDI.create of a volume that is exactly of size of the SR free space...";
		with_space_error "Creating a VDI which fill exactly the remaining SR size should fail"
			(fun () -> 
				create_VDIs params vdi_create_test;
				destroy_VDIs params vdi_create_test)
		
	let void params = ()

	let vdi_resize params = 
		debug "TEST: VDI.resize to just over the space allowed...";
		with_space_error "Resizing a VDI to fill exactly the remaining SR size should fail"
			(fun () -> 
				(* first, create a VDI *)
				create_VDIs params vdi_resize_test;
				match  Client.VDI.get_by_name_label params.rpc params.session vdi_resize_test.name_label with
				| [vdi] -> 
					let vdi_size = Client.VDI.get_physical_utilisation params.rpc params.session vdi in
					let new_size = Int64.add vdi_size (get_free_space params) in
					(* then, resize it to fill the SR *)
					Client.VDI.resize params.rpc params.session vdi new_size;
					   
					(* The last operation should fail, so this one should never be executed *)
					destroy_VDIs params vdi_resize_test
				| _ -> raise (Test_error "Scenario.vdi_resize: unexpected error"))

	let vdi_resize_cleanup params =
		destroy_VDIs params vdi_resize_test

	let exercise_coalesce_daemon params fist space_overhead_at_fistpoint =
		(* First, create a tree of VDIs *)
		create_VDIs params coalesce_test;
		let master = get_master params.rpc params.session in

		(* Pause the coalesce operation when it tries to relink *)
		let vdis_that_will_be_deleted = with_fistpoint params.rpc params.session master fist
			(fun () ->
				let free_space = get_free_space params in
				 
				(* Remove a VDI of the tree to kick-off the coalesce daemon *)
				let vdis_to_remove =
					List.filter to_remove (get_my_vdis params.rpc params.session params.sr) in

                                let parents_of_vdis_to_remove = parents_of params vdis_to_remove in

				let other_children_of_parents =
					List.filter
						(fun p -> not (List.mem p vdis_to_remove))
						(children_of params parents_of_vdis_to_remove) in

				debug "The following forest shows the initial state:";
				dump_forest params;

				let sizes = List.fold_left 
					(fun accu (vdi, vdi_record) -> 
						debug "Removing a VDI of size %s with name-label='%s' uuid=%s to trigger coalesce daemon" (size_of vdi_record.API.vDI_physical_utilisation) vdi_record.API.vDI_name_label vdi_record.API.vDI_uuid;
						Client.VDI.destroy params.rpc params.session vdi;
						Int64.add accu vdi_record.API.vDI_physical_utilisation)
					0L vdis_to_remove in
				 
				debug "Parents: %s" (vdis_to_uuids parents_of_vdis_to_remove);
				debug "VDIs which will be deleted on coalesce: %s" (vdis_to_uuids other_children_of_parents);
				 
				(* wait for FIST point to fire *)
				debug "Waiting for the coalesce daemon to hit the fist point...";
				let fist_fired = wait_for_fist params fist in
				if not fist_fired then failwith "Fist point did not fire; maybe the coalescing daemon didn't execute?";

				debug "Fist point has fired!";
				(* We've now got one minute to fiddle around before the coalescing daemon wakes up... *)

				(* Wait until we've got the amount of free space we expect *)
				let expected_free_space = Int64.sub (Int64.add sizes free_space) space_overhead_at_fistpoint in
				debug "Expected free space is %Ld + %Ld - %Ld = %Ld" sizes free_space space_overhead_at_fistpoint expected_free_space;
				wait_for_SR_utilisation_update params expected_free_space;

				(* Return the nodes which we expect to be deleted on coalesce *)
				other_children_of_parents
			)
			() in

		(* at this point, the coalesce sould be blocked just before relinking: we create a raw VDI which fill the free space. This will cause the relinking to fail when the coalesce daemon resumes. *)
		let t = Thread.create (fun () ->
			debug "Waiting 5 seconds to ensure event-wait loop has started...";
			Thread.delay 5.;
			debug "Filling up remaining space with a raw VDI...";
			(* Note that the LVHDRT_before_create_relink_journal fist point does not hold the SR lock, so this operation can proceed as normal. *)
			create_VDIs params raw_vdi_create_test;
		) () in

		(* Now check whether any VDIs get deleted. Since the coalesce phase has completed, any VDI deletions must be a result of the relinking phase completing. *)
		debug "Now seeing if any VDIs get deleted (expecting none)...";
		let deleted = wait_for_vdi_deletion params vdis_that_will_be_deleted in
		if deleted then failwith "Relinking succeeded when it shouldn't have done";

		Thread.join t;
		debug "Timer expired and we didn't lose the VDIs, so relinking failed, as expected";

		debug "The following forest should still contain two \"base copy\" VDIs:";
		dump_forest params;

		(* Now remove the "blocker" VDI and try a coalesce again via an SR.scan and check that relinking succeeds. *)
		let t = Thread.create (fun () ->
			debug "Waiting 5 seconds to ensure event-wait loop has started...";
			Thread.delay 5.;

			(* Note that, because we do this deletion while waiting for VDI deletion events, the deletion of this VDI will be observed *)
			debug "Removing the VDI which filled up all the remaining space...";
			destroy_VDIs params raw_vdi_create_test;

			(* In case a scan hasn't automatically happened and we still haven't seen the deletion, force a scan now *)
			debug "Invoking an SR.scan...";
			invoke_sr_scan params;
		) () in
		debug "Now checking that we do get a VDI deletion (expecting %s to go)..." (vdis_to_uuids vdis_that_will_be_deleted);
		(* Note: delay of 90 seconds was not always long enough. *)
		let deleted = wait_for_vdi_deletion params ~delay:180. vdis_that_will_be_deleted in
		if not deleted then failwith "Relinking didn't succeed when it should have done";
		  
		Thread.join t;
		debug "A VDI was deleted. This suggests that relinking succeeded, as expected";

		debug "The following forest shows the state after a successful coalesce:";
		dump_forest params;
		()
	
	let exercise_coalesce_daemon_cleanup params =
		destroy_VDIs params coalesce_test

	let coalesce_relink params =
		debug "TEST: Make the coalescing fail when creating a relink journal...";
		exercise_coalesce_daemon params Fists.before_create_relink_journal 0L
		
	let coalesce_relink_cleanup params =
		exercise_coalesce_daemon_cleanup params

	let coalesce_inflate params = 
		debug "TEST: Make the coalesce fail to inflate the grandparent VHD...";
                (* The 4 MB space overhead is because, at the time the fist point fires, the coalesce journal will have been created *)
		exercise_coalesce_daemon params Fists.before_inflate_grandparent Globs.four_megs
		
	let coalesce_inflate_cleanup params =
		exercise_coalesce_daemon_cleanup params

	let vdi_attach_of_snapshot params = 
		debug "TEST: Make VDI.attach of a snapshot VDI fail to inflate...";
		create_VDIs params vdi_create_for_snapshot;
		debug "Snapshotting the VDI...";
		let snapshot_vdi = snapshot_VDI params vdi_create_for_snapshot in

		debug "Fill up remaining space on the disk...";
		create_VDIs params raw_vdi_create_test;

		debug "Attempt to attach the snapshot VDI, causing it to inflate, which should fail";
		(* Expect an SM_BACKEND_ERROR_180 *)
		with_space_error "Attaching the snapshot when the disk is full should fail"
			(fun () -> Utils.with_attached_vdi params.rpc params.session snapshot_vdi (fun device vbd -> ()));

		debug "Destroying the snapshot VDI";
		Client.VDI.destroy params.rpc params.session snapshot_vdi

       let vdi_attach_of_snapshot_cleanup params =
	       destroy_VDIs params vdi_create_for_snapshot;
	       destroy_VDIs params raw_vdi_create_test

	(* list of (operation x cleanup) functions *)
	let all = [
		vdi_create, void; 
		vdi_resize, vdi_resize_cleanup;
		coalesce_relink, coalesce_relink_cleanup;
                coalesce_inflate, coalesce_inflate_cleanup;
		vdi_attach_of_snapshot, vdi_attach_of_snapshot_cleanup;
	]
end

let check_operation params (operation,cleanup) =
	Pervasiveext.finally
		(fun () -> 
			create_VDIs params tmp_VDIs; 
			operation params)
		(fun () -> 
			cleanup params;
			destroy_VDIs params tmp_VDIs);
	
	check_SR_is_operable params;
	verify_no_stale_journals_exist params
	
let run rpc session =
	let sr = Utils.find_lvhd_sr rpc session in
	let params = { rpc=rpc; session=session; sr=sr } in

	Pervasiveext.finally
		(fun () ->
			create_VDIs params fillup_VDIs;
			List.iter (check_operation params) Scenario.all)
		(fun () ->
			destroy_VDIs params fillup_VDIs)
