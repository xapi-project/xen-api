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
open Fun
open Stringext
open Xmlrpc_client

let url = ref (Http.Url.File ({ Http.Url.path = Filename.concat Fhs.vardir "storage" }, "/"))

module RPC = struct
let rpc call =
	XMLRPC_protocol.rpc ~transport:(transport_of_url !url)
		~http:(xmlrpc ~version:"1.0" ?auth:(Http.Url.auth_of !url) (Http.Url.uri_of !url)) call
end

open Storage_interface
module Client = Client(RPC)

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
	Stunnel.init_stunnel_path ();
	(* Look for url=foo *)
	let args = Array.to_list Sys.argv in
	begin
		match List.filter (String.startswith "url=") args with
			| x :: _ ->
				url := Http.Url.of_string (String.sub x 4 (String.length x - 4))
			| _ -> ()
	end;
	let args = List.filter (not ++ (String.startswith "url=")) args |> List.tl in
	match args with
		| [ "sr-list" ] ->
			let srs = Client.SR.list ~task in
			List.iter
				(fun sr ->
					Printf.printf "%s\n" sr
				) srs
		| [ "sr-scan"; sr ] ->
			if Array.length Sys.argv < 3 then usage_and_exit ();
			begin match Client.SR.scan ~task ~sr with
				| Success (Vdis vs) ->
					List.iter
						(fun v ->
							Printf.printf "%s\n" (string_of_vdi_info v)
						) vs
				| x ->
					Printf.fprintf stderr "Unexpected result: %s\n" (string_of_result x)
			end
		| "vdi-create" :: sr :: args ->
			if Array.length Sys.argv < 3 then usage_and_exit ();
			let kvpairs = List.filter_map
				(fun x -> match String.split ~limit:2 '=' x with
					| [k; v] -> Some (k, v)
					| _ -> None) args in
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

			begin match Client.VDI.create ~task ~sr ~vdi_info ~params with
				| Success (Vdi v) ->
					Printf.printf "%s\n" (string_of_vdi_info v)
				| x ->
					Printf.fprintf stderr "Unexpected result: %s\n" (string_of_result x)
			end
		| [ "vdi-destroy"; sr; vdi ] ->
			begin match Client.VDI.destroy ~task ~sr ~vdi with
				| Success Unit -> ()
				| x ->
					Printf.fprintf stderr "Unexpected result: %s\n" (string_of_result x)
			end
		| _ ->
			usage_and_exit ()
