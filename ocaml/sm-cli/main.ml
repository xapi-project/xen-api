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
(**
 * @group Storage
 *)

open Listext
open Stringext

let socket = ref "/var/xapi/storage"

let rpc call =
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~transport:(Unix !socket) 
		~http:(xmlrpc ~version:"1.0" "/") call

open Storage_interface

let task = "sm-cli"

let usage_and_exit () =
	Printf.fprintf stderr "Usage:\n";
	Printf.fprintf stderr "  %s sr-list\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s sr-scan <SR>\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s vdi-create <SR> key_1=val_1 ... key_n=val_n\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s vdi-destroy <SR> <VDI>\n" Sys.argv.(0);
	exit 1

let _ =
	if Array.length Sys.argv < 2 then usage_and_exit ();
	match Sys.argv.(1) with
		| "sr-list" ->
			let srs = Client.SR.list rpc ~task in
			List.iter
				(fun sr ->
					Printf.printf "%s\n" sr
				) srs
		| "sr-scan" ->
			if Array.length Sys.argv < 3 then usage_and_exit ();
			let sr = Sys.argv.(2) in
			begin match Client.SR.scan rpc ~task ~sr with
				| Success (Vdis vs) ->
					List.iter
						(fun v ->
							Printf.printf "%s\n" (string_of_vdi_info v)
						) vs
				| x ->
					Printf.fprintf stderr "Unexpected result: %s\n" (string_of_result x)
			end
		| "vdi-create" ->
			if Array.length Sys.argv < 3 then usage_and_exit ();
			let sr = Sys.argv.(2) in
			let kvpairs = List.filter_map
				(fun x -> match String.split ~limit:2 '=' x with
					| [k; v] -> Some (k, v)
					| _ -> None)
				(Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3))) in
			let find key = if List.mem_assoc key kvpairs then Some (List.assoc key kvpairs) else None in
			let vdi_info = {
				vdi = "";
				name_label = Opt.default "default name_label" (find "name_label");
				name_description = Opt.default "default name_description" (find "name_description");
				ty = Opt.default "user" (find "ty");
				metadata_of_pool = Opt.default "" (find "metadata_of_pool");
				is_a_snapshot = Opt.default false (Opt.map bool_of_string (find "is_a_snapshot"));
				snapshot_time = Opt.default "19700101T00:00:00Z" (find "snapshot_time");
				snapshot_of = Opt.default "" (find "snapshot_of");
				read_only = Opt.default false (Opt.map bool_of_string (find "read_only"));
				virtual_size = Opt.default 1L (Opt.map Int64.of_string (find "virtual_size"));
				physical_utilisation = 0L
			} in
			let params = List.filter_map
				(fun (k, v) ->
					let prefix = "params:" in
					let l = String.length prefix in
					if String.startswith prefix k
					then Some (String.sub k l (String.length k - l), v)
					else None) kvpairs in

			begin match Client.VDI.create rpc ~task ~sr ~vdi_info ~params with
				| Success (Vdi v) ->
					Printf.printf "%s\n" (string_of_vdi_info v)
				| x ->
					Printf.fprintf stderr "Unexpected result: %s\n" (string_of_result x)
			end
		| "vdi-destroy" ->
			if Array.length Sys.argv < 4 then usage_and_exit ();
			let sr = Sys.argv.(2) in
			let vdi = Sys.argv.(3) in
			begin match Client.VDI.destroy rpc ~task ~sr ~vdi with
				| Success Unit -> ()
				| x ->
					Printf.fprintf stderr "Unexpected result: %s\n" (string_of_result x)
			end
		| x ->
			Printf.fprintf stderr "Unknown command: %s\n" x;
			usage_and_exit ()
