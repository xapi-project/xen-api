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
(* Test case 8775: concurrent attach/detach of a snapshot VDI *)

open Client
open Threadext
open Lvhdrt_exceptions
open Utils

(* Test description:

1. Setup a pool of at least 2 hosts with a shared LVHD storage;
2. create a VDI, snapshot it;
3. on each host, do a attach/detach loop of the snapshot VDI.

attach should inflate the snapshot VDI, detach should deflate it. This test should check that nothing bad happens ... *)

let test_duration = 20. *. 60. (* 20 minutes *)

let create_VDI rpc session_id sr =
	Client.VDI.create ~rpc ~session_id ~name_label:"LVHDRT_init"
		~name_description:"" ~sR:sr ~virtual_size:Globs.thirtytwo_megs ~_type:`ephemeral (*!*)
		~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[]

let snapshot_VDI rpc session_id vdi =
	Client.VDI.snapshot ~rpc ~session_id ~vdi ~driver_params:[]

let run rpc session =

	let dom0s = get_all_control_domains rpc session in
	let dom0s = List.map (fun (dom0, dom0_record) -> (dom0, dom0_record.API.vM_uuid)) dom0s in 

	if List.length dom0s < 2 then
		raise (Test_error "This test should run in a pool of at least two hosts");
	
	let manager = manage_concurrent_tests () in
	let sr = find_shared_lvhd_sr rpc session in
	let vdi = create_VDI rpc session sr in
	let snapshot = snapshot_VDI rpc session vdi in

	Pervasiveext.finally (fun () ->
		let snapshot_uuid = Client.VDI.get_uuid rpc session snapshot in
		
		let start_time = Unix.time () in
		
		let rec loop (dom0, dom0_uuid) =
			if manager.continue () then begin
				with_attached_vdi rpc session ~dom0 ~mode:`RO snapshot (fun _ _ ->  
					debug "VDI %s is attached to VM %s" snapshot_uuid dom0_uuid;
					Thread.delay 0.1);
				debug "VDI %s is detached from VM %s" snapshot_uuid dom0_uuid;
				if (Unix.time ()) -. start_time < test_duration then
					loop (dom0, dom0_uuid)
				else begin
					debug "The test have been successfully running since %d minutes." (int_of_float test_duration);
					manager.success ()
				end 
			end in

		let protected_loop (dom0, dom0_uuid) = 
			try loop (dom0, dom0_uuid)
			with e -> 
				let host = Client.VM.get_resident_on rpc session dom0 in
				let host_name = Client.Host.get_name_label rpc session host in
				debug "Unexpected error on %s: %s" host_name (Printexc.to_string e);
				manager.failure () in

		let ts = List.map (fun d -> Thread.create (fun () -> protected_loop d) ()) dom0s in
		
		(* Wait for the threads to finish or fail *)
		List.iter Thread.join ts;
		
		if manager.is_success () then
			debug "Test succeeded"
		else
			failwith "Confusion while attaching/detaching a snapshot VDI...")
		(fun () ->
			 Client.VDI.destroy rpc session vdi;
			 Client.VDI.destroy rpc session snapshot)
