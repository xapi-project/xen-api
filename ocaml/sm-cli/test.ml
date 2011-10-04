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

let socket = ref "/var/xapi/storage"

let rpc call =
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~transport:(Unix !socket) 
		~http:(xmlrpc ~version:"1.0" "/") call

open Storage_interface

let task = "sm-test"

let usage_and_exit () =
	Printf.fprintf stderr "Usage:\n";
	Printf.fprintf stderr "  %s <SR>" Sys.argv.(0);
	exit 1

let test_destroy_missing_vdi sr _ =
	let vdi = "missing" in
	begin match Client.VDI.destroy rpc ~task ~sr ~vdi with
		| Failure Vdi_does_not_exist -> ()
		| x -> Printf.fprintf stderr "Unexpected result: %s\n" (string_of_result x)
	end

let test_create_destroy sr _ =
	let name_label = "test_name_label" in
	let name_description = "test_name_description" in
	let ty = "ephemeral" in
	let metadata_of_pool = "mop" in
	let is_a_snapshot = true in
	let snapshot_time = 0. in
	let snapshot_of = "sof" in
	let read_only = false in
	let virtual_size = 123L in
	let physical_utilisation = 0L in
	let vdi_info = {
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
	} in
	let vdi = begin match Client.VDI.create rpc ~task ~sr ~vdi_info ~params:[] with
		| Success (Vdi vdi_info') ->
			let fail msg =
				Printf.fprintf stderr "VDI.create field mismatch: %s\n" msg in
			if vdi_info.name_label <> vdi_info'.name_label
			then fail (Printf.sprintf "name_label: %s <> %s" vdi_info.name_label vdi_info'.name_label);
			if vdi_info.name_description <> vdi_info'.name_description
			then fail (Printf.sprintf "name_description: %s <> %s" vdi_info.name_description vdi_info'.name_description);
			if vdi_info.ty <> vdi_info'.ty
			then fail (Printf.sprintf "ty: %s <> %s" vdi_info.ty vdi_info'.ty);
			if vdi_info.metadata_of_pool <> vdi_info'.metadata_of_pool
			then fail (Printf.sprintf "metadata_of_pool: %s <> %s" vdi_info.metadata_of_pool vdi_info'.metadata_of_pool);
			if vdi_info.is_a_snapshot <> vdi_info'.is_a_snapshot
			then fail (Printf.sprintf "is_a_snapshot: %b <> %b" vdi_info.is_a_snapshot vdi_info'.is_a_snapshot);
			if vdi_info.snapshot_time <> vdi_info'.snapshot_time
			then fail (Printf.sprintf "snapshot_time: %.0f <> %.0f" vdi_info.snapshot_time vdi_info'.snapshot_time);
			if vdi_info.snapshot_of <> vdi_info'.snapshot_of
			then fail (Printf.sprintf "snapshot_of: %s <> %s" vdi_info.snapshot_of vdi_info'.snapshot_of);
			if vdi_info.read_only <> vdi_info'.read_only
			then fail (Printf.sprintf "read_only: %b <> %b" vdi_info.read_only vdi_info'.read_only);
			assert_equal vdi_info.name_label vdi_info'.name_label;
			assert_equal vdi_info.name_description vdi_info'.name_description;
			assert_equal vdi_info.ty vdi_info'.ty;
			assert_equal vdi_info.metadata_of_pool vdi_info'.metadata_of_pool;
			assert_equal vdi_info.is_a_snapshot vdi_info'.is_a_snapshot;
			assert_equal vdi_info.snapshot_time vdi_info'.snapshot_time;
			assert_equal vdi_info.snapshot_of vdi_info'.snapshot_of;
			assert_equal vdi_info.read_only vdi_info'.read_only;
			vdi_info'.vdi
			(* sizes will be rounded up *)
		| x -> failwith (Printf.sprintf "Unexpected result: %s\n" (string_of_result x))
	end in
	begin match Client.VDI.destroy rpc ~task ~sr ~vdi with
		| Success Unit -> ()
		| x -> failwith (Printf.sprintf "Unexpected result: %s\n" (string_of_result x))
	end

let _ =
	let verbose = ref false in
	let sr = ref "" in
	Arg.parse [
		"-sr", Arg.Set_string sr, "Specify SR";
		"-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Test via storage backend";

	if !sr = "" then failwith "Please supply -sr argument";

	let suite = "Storage test" >::: 
		[ "test_destroy_missing_vdi" >:: (test_destroy_missing_vdi !sr);
		"test_create_destroy" >:: (test_create_destroy !sr) ] in

	run_test_tt ~verbose:!verbose suite

