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
(* Description:
   1.a Setup a pool of hosts (SDKs would probably work for this) with a dummySR and a VM installed on it
   1.b Touch "/tmp/fist_LVHDRT_xapiSM_serialization_tests" on the SR master host
   2. In parallel: VM.start/VBD.plug/VBD.unplug/VM.shutdown_force *)
	
(* WARNING: this test need to be run with a fault injection point activated inside the DummySR plugin
   (or nothing will be checked).
   So, it is necessary to check that "/tmp/fist_LVHDRT_xapiSM_serialization_tests" exists on the SR master. *)

open Client
open Utils
open Lvhdrt_exceptions

let test_name = "TC_8682"
let test_descr = "Test of concurrent 100 VM.start/shutdown loop + VBD.plug/unplug loop"
let number_of_loop = 20

let really_run rpc session () =
	(* First, we look for a dummySR *)
	let srs = Client.SR.get_all_records_where rpc session "field \"type\"=\"dummy\"" in
	let srs, _ = List.split srs in
	if srs = [] then
		raise (Test_error "No dummy SR found");

	(* Then, find a VM with a VDI on one of these SRs *)
	let vdis = List.flatten (List.map (fun sr -> Client.SR.get_VDIs rpc session sr) srs) in
	if vdis = [] then
		raise (Test_error "No VDI found on any dummy SR");
	let vbds = List.flatten (List.map (fun vdi -> Client.VDI.get_VBDs rpc session vdi) vdis) in
	if vbds = [] then
		raise (Test_error "No VM found on any dummy SR");
	let vm_vbds = List.map (fun vbd -> Client.VBD.get_VM rpc session vbd, vbd) vbds in

	let vm, vbd = List.hd vm_vbds in
	(* Finally, put the VM in an appropriate power state *)
	if Client.VM.get_power_state rpc session vm <> `Halted then
		Client.VM.hard_shutdown rpc session vm;

	let manager = manage_concurrent_tests () in

	(* catch a backend failure in the dummy SR *)
	let with_dummySR_failure f =
		try f ()
		with 
			| Api_errors.Server_error("SR_BACKEND_FAILURE_1", _) ->
				Printf.printf "Received error. Failure is inevitable.\n%!";
				manager.failure ();
			| _ -> ()
	in

	(* start/force_shutdown loop for the VM *)
	let rec start_loop n =
		Printf.printf "Start/shutdown loop: %d iterations remaining\n%!" n;
		if n <> 0 && manager.continue () then begin
			with_dummySR_failure 
				(fun () -> 
					debug "%i/%i: Starting VM ..." (number_of_loop - n + 1) number_of_loop; 
					Client.VM.start rpc session vm false false;
					Thread.delay 10.;
					debug "%i/%i: Shutdowning VM ..." (number_of_loop - n + 1) number_of_loop; 
					Client.VM.hard_shutdown rpc session vm;
					Thread.delay 10.);
			start_loop (n-1)
		end else if n = 0 then
			manager.success ()
	in

	(* plug/unplug loop for the VBD *)
	let rec plug_loop () =
		with_dummySR_failure (fun () -> 
			Client.VBD.plug rpc session vbd; 
			debug "plugging VBD";
			Thread.delay 0.1);
		with_dummySR_failure (fun () ->
			Client.VBD.unplug rpc session vbd;
			debug "unplugging VBD";
			Thread.delay 0.1);
		if manager.continue ()
		then plug_loop ()
	in

	(* then, we run the test *) 
	let tvm = Thread.create start_loop number_of_loop in
	let tvbd = Thread.create plug_loop () in
	Thread.join tvm;
	Thread.join tvbd;
	if manager.is_success () then
		debug "SUCCESS: %s (%s)\n" test_name test_descr
	else 
		failwith (Printf.sprintf "FAILURE: %s (%s)" test_name test_descr)

let run rpc session =
  let master = Utils.get_master rpc session in
  Utils.with_fistpoint rpc session master "LVHDRT_xapiSM_serialization_tests" (really_run rpc session) ()
