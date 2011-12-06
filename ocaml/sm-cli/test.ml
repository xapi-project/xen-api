(*
 * Copyright (C) Citrix Systems Inc.
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

open OUnit

open Listext
open Stringext

open Xmlrpc_client

let default_path = Filename.concat Fhs.vardir "storage"
let transport = ref (Unix default_path)

let rpc call =
	XMLRPC_protocol.rpc ~transport:!transport
		~http:(xmlrpc ~version:"1.0" "/") call

open Storage_interface
module Client = Storage_interface.Client(struct let rpc = rpc end)

let task = "sm-test"

let mib = Int64.mul 1024L 1024L

let usage_and_exit () =
	Printf.fprintf stderr "Usage:\n";
	Printf.fprintf stderr "  %s <SR>" Sys.argv.(0);
	exit 1

let find_vdi_in_scan sr vdi =
	match Client.SR.scan ~task ~sr with
		| Success (Vdis results) ->
			begin
				try
					Some (List.find (fun x -> x.vdi = vdi) results)
				with Not_found ->
					None
			end
		| x ->
			failwith (Printf.sprintf "Unexpected result from SR.scan: %s\n" (string_of_result x))

let test_query sr _ = let (_: query_result) = Client.query () in ()

let missing_vdi = "missing"

let test_scan_missing_vdi sr _ =
	match find_vdi_in_scan sr missing_vdi with
		| Some vdi -> failwith (Printf.sprintf "SR.scan found a VDI that was supposed to be missing: %s" (string_of_vdi_info vdi))
		| None -> ()

let test_destroy_missing_vdi sr _ =
	begin match Client.VDI.destroy ~task ~sr ~vdi:missing_vdi with
		| Failure Vdi_does_not_exist -> ()
		| x -> failwith (Printf.sprintf "Unexpected result from VDI.destroy: %s\n" (string_of_result x))
	end

let vdi_info_assert_equal vdi_info vdi_info' =
	assert_equal ~msg:"name_label" ~printer:(fun x -> x) vdi_info.name_label vdi_info'.name_label;
	assert_equal ~msg:"name_description" ~printer:(fun x -> x) vdi_info.name_description vdi_info'.name_description;
	assert_equal ~msg:"ty" ~printer:(fun x -> x) (String.lowercase vdi_info.ty) (String.lowercase vdi_info'.ty);
	assert_equal ~msg:"metadata_of_pool" ~printer:(fun x -> x) vdi_info.metadata_of_pool vdi_info'.metadata_of_pool;
	assert_equal ~msg:"is_a_snapshot" ~printer:string_of_bool vdi_info.is_a_snapshot vdi_info'.is_a_snapshot;
	assert_equal ~msg:"snapshot_time" ~printer:(fun x -> x) vdi_info.snapshot_time vdi_info'.snapshot_time;
	assert_equal ~msg:"snapshot_of" ~printer:(fun x -> x) vdi_info.snapshot_of vdi_info'.snapshot_of;
	assert_equal ~msg:"read_only" ~printer:string_of_bool vdi_info.read_only vdi_info'.read_only

let example_vdi_info =
	let name_label = "test_name_label" in
	let name_description = "test_name_description" in
	let ty = "ephemeral" in
	let metadata_of_pool = "mop" in
	let is_a_snapshot = true in
	let snapshot_time = "19700101T00:00:00Z" in
	let snapshot_of = "sof" in
	let read_only = false in
	let virtual_size = Int64.mul 8L mib in
	let physical_utilisation = 0L in
	{
		vdi = "";
		name_label = name_label;
		name_description = name_description;
		ty = ty;
		metadata_of_pool = metadata_of_pool;
		is_a_snapshot = is_a_snapshot;
		snapshot_time = snapshot_time;
		snapshot_of = snapshot_of;
		read_only = read_only;
		virtual_size = virtual_size;
		physical_utilisation = physical_utilisation;
	}

let test_create_destroy sr _ =
	let vdi_info = example_vdi_info in
	let vdi_info' = begin match Client.VDI.create ~task ~sr ~vdi_info ~params:[] with
		| Success (Vdi vdi_info') ->
			vdi_info_assert_equal vdi_info vdi_info';
			vdi_info'
			(* sizes will be rounded up *)
		| x -> failwith (Printf.sprintf "Unexpected result: %s\n" (string_of_result x))
	end in
	begin match find_vdi_in_scan sr vdi_info'.vdi with
		| None -> failwith (Printf.sprintf "SR.scan failed to find vdi: %s" (string_of_vdi_info vdi_info'))
		| Some vdi_info'' -> vdi_info_assert_equal vdi_info' vdi_info''
	end;
	begin match Client.VDI.destroy ~task ~sr ~vdi:vdi_info'.vdi with
		| Success Unit -> ()
		| x -> failwith (Printf.sprintf "Unexpected result: %s\n" (string_of_result x))
	end;
	begin match find_vdi_in_scan sr vdi_info'.vdi with
		| Some vdi_info''' -> failwith (Printf.sprintf "SR.scan found a VDI that was just deleted: %s" (string_of_vdi_info vdi_info'''))
		| None -> ()
	end

let test_attach_activate sr _ =
	let vdi_info = match Client.VDI.create ~task ~sr ~vdi_info:example_vdi_info ~params:[] with
		| Success (Vdi x) -> x
		| x -> failwith (Printf.sprintf "Unexpected result: %s\n" (string_of_result x))	in
	let dp = "test_attach_activate" in
	let (_: string) = match Client.VDI.attach ~task ~sr ~dp ~vdi:vdi_info.vdi ~read_write:true with
		| Success (Params x) -> x
		| x -> failwith (Printf.sprintf "Unexpected result: %s\n" (string_of_result x))	in
	begin match Client.VDI.activate ~task ~sr ~dp ~vdi:vdi_info.vdi with
		| Success Unit -> ()
		| x -> failwith (Printf.sprintf "Unexpected result: %s\n" (string_of_result x))
	end;
	begin match Client.VDI.destroy ~task ~sr ~vdi:vdi_info.vdi with
		| Success Unit -> ()
		| x -> failwith (Printf.sprintf "Unexpected result: %s\n" (string_of_result x))
	end		

let _ =
	let verbose = ref false in
	let sr = ref "" in
	let unix_path = ref "" in
	let host_port = ref "" in

	Arg.parse [
		"-sr", Arg.Set_string sr, "Specify SR";
		"-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
		"-tcp", Arg.Set_string host_port, "Connect via TCP to a host:port";
		"-unix", Arg.Set_string unix_path, Printf.sprintf "Connect via a Unix domain socket (default %s)" default_path
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Test via storage backend";

	if !sr = "" then failwith "Please supply -sr argument";

	if !host_port <> "" && (!unix_path <> "") then failwith "Please supply either -tcp OR -unix";

	if !host_port <> "" then begin
		match String.split ~limit:2 ':' !host_port with
			| [ host; port ] ->
				transport := TCP(host, int_of_string port)
			| _ -> failwith "Failed to parse host:port"
	end;
	if !unix_path <> "" then transport := Unix(!unix_path);

	let suite = "Storage test" >::: 
		[
			"test_query" >:: (test_query !sr);
			"test_scan_missing_vdi" >:: (test_scan_missing_vdi !sr);
			"test_destroy_missing_vdi" >:: (test_destroy_missing_vdi !sr);
			"test_create_destroy" >:: (test_create_destroy !sr);
			"test_attach_activate" >:: (test_attach_activate !sr);
		] in

	run_test_tt ~verbose:!verbose suite

