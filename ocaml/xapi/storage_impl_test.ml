(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
 * @group Storage
 *)

open Pervasiveext
open Threadext
open Storage_interface

let total_errors = ref 0
let total_errors_m = Mutex.create ()

let debug = Storage_impl.debug
let error = Storage_impl.error
let info = Storage_impl.info

let inc_errors () =
	Mutex.execute total_errors_m
		(fun () ->
			incr total_errors;
		)

exception Api_error of string * (string list)

module Debug_print_impl = struct
	type context = Smint.request
	let query _ _ = assert false
	module DP = struct
		let create context ~task ~id = assert false
		let destroy context ~task ~dp = assert false
		let diagnostics context () = assert false
		let attach_info context ~task ~sr ~vdi ~dp = assert false
	end
	module VDI = struct
		let m = Mutex.create ()
		let attached = Hashtbl.create 10
		let activated = Hashtbl.create 10
        let created = Hashtbl.create 10
		let key_of sr vdi = Printf.sprintf "%s/%s" sr vdi

		let stat context ~task ~sr ~vdi () = assert false

        let create context ~task ~sr ~vdi_info ~params =
            let vdi = "newvdi" in
            let info =
                if List.mem_assoc "toosmall" params
                then { vdi_info with virtual_size = Int64.sub vdi_info.virtual_size 1L }
                else vdi_info in
            Mutex.execute m
                (fun () ->
                    let key = key_of sr vdi in
                    Hashtbl.replace created key info
                );
            info

		let snapshot context ~task ~sr ~vdi ~vdi_info ~params =
			create context ~task ~sr ~vdi_info ~params
		let clone = snapshot

        let destroy context ~task ~sr ~vdi =
            Mutex.execute m
                (fun () ->
                    let key = key_of sr vdi in
                    if not(Hashtbl.mem created key)
                    then raise (Backend_error("ENOENT", [ sr; vdi ]))
                    else if Hashtbl.mem activated key
                    then raise (Backend_error("Still activated", [ sr; vdi]))
                    else if Hashtbl.mem attached key
                    then raise (Backend_error("Still attached", [ sr; vdi]))
                    else begin
                        Hashtbl.remove created key
                    end
                )

		let attach context ~task ~dp ~sr ~vdi ~read_write =
			info "VDI.attach dp:%s sr:%s vdi:%s read_write:%b" dp sr vdi read_write;
			if dp = "error"
			then raise (Api_error("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]));
			if dp = "error2"
			then raise (Backend_error ("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]))
			else begin
				Mutex.execute m
					(fun () ->
						let key = key_of sr vdi in
						if Hashtbl.mem attached key then begin
							inc_errors ();
						error "VDI.attach dp:%s sr:%s vdi:%s : double attach" dp sr vdi;
						failwith "double attach"
						end else Hashtbl.replace attached key ());
				{ params="XXX"; xenstore_data=[] }
			end
		let activate context ~task ~dp ~sr ~vdi =
			Mutex.execute m
				(fun () ->
					let key = key_of sr vdi in
					if Hashtbl.mem activated key then begin
						inc_errors ();
						error "VDI.detach dp:%s sr:%s vdi:%s : double activate" dp sr vdi;
						failwith "double activate"
					end else Hashtbl.replace activated key ());
			info "VDI.activate dp:%s sr:%s vdi:%s" dp sr vdi

		let working = ref false

		let detach context ~task ~dp ~sr ~vdi =
			if vdi = "error" && not(!working)
			then raise (Api_error("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]));
			if vdi = "error2" && not(!working)
			then raise (Backend_error ("SR_BACKEND_FAILURE_test", [ "this"; "is"; "an"; "example" ]))
			else begin
				Mutex.execute m
					(fun () ->
						let key = key_of sr vdi in
						if not (Hashtbl.mem attached key) then begin
							inc_errors ();
							error "VDI.detach dp:%s sr:%s vdi:%s : double detach" dp sr vdi;
							failwith "double detach"
						end else Hashtbl.remove attached key);
				info "VDI.detach dp:%s sr:%s vdi:%s" dp sr vdi
			end
		let deactivate context ~task ~dp ~sr ~vdi =
			Mutex.execute m
				(fun () ->
					let key = key_of sr vdi in
					if not (Hashtbl.mem activated key) then begin
						inc_errors ();
						error "VDI.deactivate dp:%s sr:%s vdi:%s : double deactivate" dp sr vdi;
						failwith "double deactivate"
					end else Hashtbl.remove activated key);
			info "VDI.deactivate dp:%s sr:%s vdi:%s" dp sr vdi

		let copy context ~task ~sr ~vdi ~url ~dest = assert false
        let get_url context ~task ~sr ~vdi = assert false
		let compose context ~task ~sr ~vdi1 ~vdi2 = assert false
		let set_content_id context ~task ~sr ~vdi ~content_id = assert false
		let get_by_name context ~task ~sr ~name = assert false
		let similar_content context ~task ~sr ~vdi = assert false
		

	end

	let get_by_name context ~task ~name = assert false

	module Mirror = struct
		let start context ~task ~sr ~vdi ~dp ~url ~dest = assert false
		let stop context ~task ~sr ~vdi = assert false
		let list context ~task ~sr = assert false
		let copy_snapshot context ~task ~sr ~vdi ~dp ~url ~dest = assert false
		let receive_start context ~task ~sr ~vdi_info ~similar = assert false
		let receive_finalize context ~task ~sr ~vdi = assert false
		let receive_cancel context ~task ~sr ~vdi = assert false
	end
		
	module SR = struct
		let list context ~task = assert false
		let scan context ~task ~sr = assert false
		let attach context ~task ~sr ~device_config =
			info "SR.attach sr:%s" sr
		let fail_if_anything_leaked () = 
			Mutex.execute VDI.m
				(fun () ->
					Hashtbl.iter
						(fun k _ ->
							error "leaked attach: %s" k;
							inc_errors ();
						) VDI.attached;
					Hashtbl.iter
						(fun k _ ->
							error "leaked activate: %s" k;
							inc_errors ();
						) VDI.activated
				)
		let detach context ~task ~sr =
			info "SR.detach sr:%s" sr;
			fail_if_anything_leaked ()
		let reset context ~task ~sr = assert false
		let destroy context ~task ~sr =
			info "SR.destroy sr:%s" sr;
			fail_if_anything_leaked ()

	end

end


module Server=Server(Storage_impl.Wrapper(Debug_print_impl))

let path = "/tmp/storage"

let rpc_unix call = 
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~transport:(Unix path) ~http:(xmlrpc ~version:"1.0" "/") call
let rpc_inprocess call = Server.process (Some "") call

let use_inprocess_rpc = ref true

let rpc call = if !use_inprocess_rpc then rpc_inprocess call else rpc_unix call

let task = "task"

module Client=Client(struct let rpc=rpc end)

let datapath_of_id id = Client.DP.create ~task ~id

let expect expected f x =
	if not(f x) then begin
		error "error: expected %s" expected;
		inc_errors ();
	end

let backend_error f = 
	try ignore(f ()); false with
		| (Backend_error(code, params)) when code = "SR_BACKEND_FAILURE_test" -> true
		| e -> 
			debug "backend_error: Expecting SR_BACKEND_FAILURE_test, got '%s'" (Printexc.to_string e);
			false
			
let too_small_backend_error f = 
	try ignore(f ()); false with 
		| (Backend_error(code, params)) when code = "SR_BACKEND_FAILURE" && (List.hd params = "Disk too small") -> true
		| _ -> false

let internal_error f = 
	try ignore(f ()); false with 
		| (Internal_error "Storage_impl_test.Api_error(\"SR_BACKEND_FAILURE_test\", _)") -> true
		| _ -> false

let dp_is dp state s =
		if not (List.mem_assoc dp s.dps)
		then state = Vdi_automaton.Detached
		else
			let state' = List.assoc dp s.dps in
			let result = state = state' in
			if not result then begin
				debug "dp_is: returning false: actual state=%s passed state=%s"
					(Vdi_automaton.string_of_state state')
					(Vdi_automaton.string_of_state state)
			end;
			result

let test_vdis sr : unit =
	let num_users = 10 in
	let num_vdis = 10 in
	let iterations = 10 in
	let one id sr vdi () =
		let dp = datapath_of_id id in
		for i = 0 to iterations - 1 do
			expect "_" (function _ -> true)
				(Client.VDI.attach ~task ~dp ~sr ~vdi ~read_write:false);
			expect "Attached(RO) 1" (dp_is dp (Vdi_automaton.Attached Vdi_automaton.RO))
				(Client.VDI.stat ~task ~sr ~vdi ());
			expect "() 2" (fun x -> x = ())
				(Client.VDI.detach ~task ~dp ~sr ~vdi);
			expect "Detached 3" (dp_is dp Vdi_automaton.Detached)
				(Client.VDI.stat ~task ~sr ~vdi ());
			expect "()" (fun x -> x = ())
				(Client.VDI.detach ~task ~dp ~sr ~vdi);
			expect "Params _ 4" (function _ -> true)
				(Client.VDI.attach ~task ~dp ~sr ~vdi ~read_write:false);
			expect "Attached(RO) 5" (dp_is dp (Vdi_automaton.Attached Vdi_automaton.RO))
				(Client.VDI.stat ~task ~sr ~vdi ());
			expect "() 6" (fun x -> x = ())
				(Client.VDI.activate ~task ~dp ~sr ~vdi);
			expect "Activated(RO) 7" (dp_is dp (Vdi_automaton.Activated Vdi_automaton.RO))
				(Client.VDI.stat ~task ~sr ~vdi ());
			expect "() 8" (fun x -> x = ())
				(Client.VDI.deactivate ~task ~dp ~sr ~vdi);
			expect "Attached(RO) 9" (dp_is dp (Vdi_automaton.Attached Vdi_automaton.RO))
				(Client.VDI.stat ~task ~sr ~vdi ());
			expect "() 10" (fun x -> x = ())
				(Client.VDI.detach ~task ~dp ~sr ~vdi);
			expect "Detached 11" (dp_is dp Vdi_automaton.Detached)
				(Client.VDI.stat ~task ~sr ~vdi ());
		done in
	let vdis = Range.to_list (Range.make 0 num_vdis) in
	let users = Range.to_list (Range.make 0 num_users) in
	let bodies =
		List.concat (
			List.map (fun user ->
				List.map (fun vdi -> one (Printf.sprintf "vdi:%d/user:%d" vdi user) sr (string_of_int vdi)) vdis
			) users
		) in
	info "Starting %d threads%!" (List.length bodies);
	let threads = List.map (fun f -> Thread.create f ()) bodies in
	info "Joining %d threads%!" (List.length bodies);
	List.iter Thread.join threads

let leak dp sr activate vdi =
	info "Leaking some attaches and activates";
	expect "Params _" (function _ -> true)
		(Client.VDI.attach ~task ~dp ~sr ~vdi ~read_write:true);
	if activate
	then expect "()" (function () -> true)
		(Client.VDI.activate ~task ~dp ~sr ~vdi)

let test_sr sr =
	let dp = datapath_of_id "pbd" in
	expect "()" (fun x -> x = ())
		(Client.SR.attach ~task ~sr ~device_config:[]);
	test_vdis sr;
	leak dp sr false "leaked";
	leak dp sr true "leaked2";
	info "About to SR.detach";
	expect "()" (fun x -> x = ())
		(Client.SR.detach ~task ~sr);
	expect "()" (fun x -> x = ())
		(Client.SR.attach ~task ~sr ~device_config:[]);
	leak dp sr false "leaked";
	leak dp sr true "leaked2";
	info "About to logout";
	expect "()" (fun x -> x = ())
		(Client.DP.destroy ~task ~dp ~allow_leak:false);
	info "About to SR.detach";
	expect "()" (function () -> true)
		(Client.SR.detach ~task ~sr);
	(* About to check the error handling *)
	let dp = datapath_of_id "error" in
	expect "()" (fun x -> x = ())
		(Client.SR.attach ~task ~sr ~device_config:[]);
	debug "This VDI.attach should fail:";
	expect "internal_error" internal_error
		(fun () -> Client.VDI.attach ~task ~dp ~sr ~vdi:"leaked" ~read_write:true);
	let dp = datapath_of_id "error2" in
	debug "This VDI.attach should fail:";
	expect "backend_error" backend_error
		(fun () -> Client.VDI.attach ~task ~dp ~sr ~vdi:"leaked" ~read_write:true);
	debug "Detaching and cleaning up";
	expect "()" (fun x -> x = ())
		(Client.SR.detach ~task ~sr)

(* Check the VDI.stat function works *)
let test_stat sr vdi = 
	expect "()" (fun x -> x = ())
		(Client.SR.attach ~task ~sr ~device_config:[]);
	let dp1 = datapath_of_id "dp1" (* will be r/w *)
	and dp2 = datapath_of_id "dp2" (* will be r/o *) in
	expect "Params _" (function _ -> true)
		(Client.VDI.attach ~task ~dp:dp1 ~sr ~vdi ~read_write:true);
	(* dp1: Attached(RW) dp2: Detached superstate: Attached(RW) *)
	expect "Attached(RW)" (dp_is dp1 (Vdi_automaton.Attached Vdi_automaton.RW))
		(Client.VDI.stat ~task ~sr ~vdi ());
	expect "Attached(RW)" (function x -> x.superstate = Vdi_automaton.Attached Vdi_automaton.RW)
		(Client.VDI.stat ~task ~sr ~vdi ());
	expect "Params _" (function _ -> true)
		(Client.VDI.attach ~task ~dp:dp2 ~sr ~vdi ~read_write:false);
	(* dp1: Attached(RW) dp2: Attached(RO) superstate: Attached(RW) *)
	expect "Attached(RO)" (dp_is dp2 (Vdi_automaton.Attached Vdi_automaton.RO))
		(Client.VDI.stat ~task ~sr ~vdi ());
	expect "Attached(RW)" (function x -> x.superstate = Vdi_automaton.Attached Vdi_automaton.RW)
		(Client.VDI.stat ~task ~sr ~vdi ());
	expect "Illegal transition" (fun () ->
		try 
			Client.VDI.detach ~task ~dp:dp1 ~sr ~vdi;
			false
		with
			| Illegal_transition(Vdi_automaton.Attached(Vdi_automaton.RW), Vdi_automaton.Attached(Vdi_automaton.RO)) -> true
			| e -> false) ();
	expect "()" (fun () -> true)
		(Client.VDI.detach ~task ~dp:dp2 ~sr ~vdi);
	(* dp1: Attached(RW) dp2: Detached superstate: Attached(RW) *)
	expect "Detached" (dp_is dp2 Vdi_automaton.Detached)
		(Client.VDI.stat ~task ~sr ~vdi ());
	expect "Attached(RW)" (function x -> x.superstate = Vdi_automaton.Attached Vdi_automaton.RW)
		(Client.VDI.stat ~task ~sr ~vdi ());
	expect "()" (fun x -> x = ())
		(Client.VDI.detach ~task ~dp:dp1 ~sr ~vdi);
	(* dp1: Detached dp1: Detached superstate: Detached *)
	expect "Detached" (function x -> x.superstate = Vdi_automaton.Detached)
		(Client.VDI.stat ~task ~sr ~vdi ());
	expect "()" (fun x -> x = ())
		(Client.SR.detach ~task ~sr)

(* Manual cleanup with VDI.detach (fails) and then SR.detach (succeeds) *)
let test_sr_detach_cleanup_errors_1 sr vdi =
	Debug_print_impl.VDI.working := false;
	expect "()" (fun x -> x = ())
		(Client.SR.attach ~task ~sr ~device_config:[]);
	let dp = datapath_of_id "datapath" in
	leak dp sr true vdi;
	expect "()" (fun x -> x = ())
		(Client.VDI.deactivate ~task ~dp ~sr ~vdi);
	if vdi = "error2"
	then expect "backend_error in test_sr_detach_cleanup" backend_error
		(fun () -> Client.VDI.detach ~task ~dp ~sr ~vdi)
	else expect "internal_error in test sr detach cleanup" internal_error
		(fun () -> Client.VDI.detach ~task ~dp ~sr ~vdi);
	debug "Detaching and cleaning up";
	Debug_print_impl.VDI.working := true;
	(* Should succeed because the VDI.attach state will have been forgotten: FH2 *)
	expect "()" (fun x -> x = ())
		(Client.SR.detach ~task ~sr)

let test_sr_detach_cleanup_errors_2 sr vdi =
	Debug_print_impl.VDI.working := false;
	expect "()" (fun x -> x = ())
		(Client.SR.attach ~task ~sr ~device_config:[]);
	let dp = datapath_of_id "datapath" in
	leak dp sr true vdi;
	if vdi = "error2"
	then expect "backend_error" backend_error
		(fun () -> Client.DP.destroy ~task ~dp ~allow_leak:false)
	else expect "internal_error" internal_error
		(fun () -> Client.DP.destroy ~task ~dp ~allow_leak:false);
	Debug_print_impl.VDI.working := true;
	debug "Attempting to attach RO (having failed a detach of a RW detach)";
	expect "Params _" (function _ -> true)
		(Client.VDI.attach ~task ~dp ~sr ~vdi ~read_write:false);
	debug "Detaching and cleaning up";
	expect "()" (fun x -> x = ())
		(Client.SR.detach ~task ~sr)

let create_vdi_test sr =
    let dp = datapath_of_id "datapath" in
    expect "()" (fun x -> x = ())
        (Client.SR.attach ~task ~sr ~device_config:[]);
	let vdi_info = {
		vdi = "";
		sr = "";
		content_id = "";
		name_label = "name_label";
		name_description = "name_description";
		virtual_size = 10L;
		ty = "user";
        is_a_snapshot = false;
        snapshot_time = "";
        snapshot_of = "";
        read_only = false;
        physical_utilisation = 10L;
		metadata_of_pool = "";
	} in
    expect "too_small_backend_error" too_small_backend_error
        (fun () -> Client.VDI.create ~task ~sr ~vdi_info ~params:["toosmall", ""]);
    let vdi = Client.VDI.create ~task ~sr ~vdi_info ~params:[] in
    expect "attach_info" (fun _ -> true) 
		(Client.VDI.attach ~task ~dp ~sr ~vdi:vdi.vdi ~read_write:false);
    debug "Detaching and cleaning up";
    expect "()" (fun x -> x = ())
        (Client.SR.detach ~task ~sr)

let _ =
	Storage_impl.print_debug := true;
	Storage_impl.host_state_path := "/tmp/storage.db";
	Vdi_automaton.test ();
	Unixext.unlink_safe !Storage_impl.host_state_path;
	Storage_impl.Local_domain_socket.start path Server.process;
	info "Listening on %s" Storage_impl.Local_domain_socket.path;
	test_sr "sr";

	test_sr_detach_cleanup_errors_1 "sr" "error2";
	test_sr_detach_cleanup_errors_1 "sr" "error";

	test_sr_detach_cleanup_errors_2 "sr" "error2";
	test_sr_detach_cleanup_errors_2 "sr" "error";

	test_stat "sr" "vdi";

    create_vdi_test "sr";

	if !total_errors = 0 then begin
		info "OK";
		exit 0;
	end else begin
		info "%d errors detected" !total_errors;
		exit 1
	end

