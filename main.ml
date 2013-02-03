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

open Storage_interface
open Storage_client

let ( |> ) a b = b a
let ( ++ ) a b x = a (b x)

let verbose = ref false

let dbg = "sm-cli"

let usage_and_exit () =
	Printf.fprintf stderr "Usage:\n";
	Printf.fprintf stderr "  %s query\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s sr-create <SR> key_1=val_1 ... key_n=val_n\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s sr-attach <SR> key_1=val_1 ... key_n=val_n\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s sr-detach <SR>\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s sr-list\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s sr-scan <SR>\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s vdi-create <SR> key_1=val_1 ... key_n=val_n\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s vdi-destroy <SR> <VDI>\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s vdi-attach <DP> <SR> <VDI> <RW>\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s vdi-detach <DP> <SR> <VDI>\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s vdi-activate <DP> <SR> <VDI>\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s vdi-deactivate <DP> <SR> <VDI>\n" Sys.argv.(0);
	Printf.fprintf stderr "  %s GET <uri>\n" Sys.argv.(0);
	exit 1

let kvpairs = List.fold_left
	(fun acc x -> match Re_str.bounded_split_delim (Re_str.regexp_string "=") x 2 with
	| [k; v] -> (k, v) :: acc
	| _ -> acc) []

let startswith prefix x =
	let x' = String.length x and prefix' = String.length prefix in
	x' >= prefix' && (String.sub x 0 prefix') = prefix

module Opt = struct
	let default d = function None -> d | Some x -> x
	let map f = function None -> None | Some x -> Some (f x)
end

let _ =
	if Array.length Sys.argv < 2 then usage_and_exit ();
	(* Look for url=foo *)
	let args = Array.to_list Sys.argv in
	begin
		match List.filter (startswith "socket=") args with
			| x :: _ ->
				set_sockets_dir (String.sub x 7 (String.length x - 7));
			| _ -> ()
	end;
	let args = List.filter (not ++ (startswith "socket=")) args |> List.tl in
	verbose := List.mem "-v" args;
	let args = List.filter (not ++ ((=) "-v")) args in
	match args with
		| [ "query" ] ->
			let q = Client.Query.query ~dbg in
			Printf.printf "%s\n" (q |> rpc_of_query_result |> Jsonrpc.to_string)
		| "sr-create" :: sr :: device_config ->
			let device_config = kvpairs device_config in
			Client.SR.create ~dbg ~sr ~device_config ~physical_size:0L
		| "sr-attach" :: sr :: device_config ->
			let device_config = kvpairs device_config in
			Client.SR.attach ~dbg ~sr ~device_config
		| "sr-detach" :: [ sr ] ->
			Client.SR.detach ~dbg ~sr
		| [ "sr-list" ] ->
			let srs = Client.SR.list ~dbg in
			List.iter
				(fun sr ->
					Printf.printf "%s\n" sr
				) srs
		| [ "sr-scan"; sr ] ->
			if Array.length Sys.argv < 3 then usage_and_exit ();
			let vs = Client.SR.scan ~dbg ~sr in
			List.iter 
				(fun v -> 
					Printf.printf "%s\n" (string_of_vdi_info v)
				) vs
		| "vdi-create" :: sr :: args ->
			if Array.length Sys.argv < 3 then usage_and_exit ();
			let kvpairs = kvpairs args in
			let find key = if List.mem_assoc key kvpairs then Some (List.assoc key kvpairs) else None in
			let params = List.fold_left
				(fun acc (k, v) ->
					let prefix = "params:" in
					let l = String.length prefix in
					if startswith prefix k
					then (String.sub k l (String.length k - l), v) :: acc
					else acc) [] kvpairs in
			let vdi_info = {
				vdi = "";
				content_id = ""; (* PR-1255 *)
				name_label = Opt.default "default name_label" (find "name_label");
				name_description = Opt.default "default name_description" (find "name_description");
				ty = Opt.default "user" (find "ty");
				metadata_of_pool = Opt.default "" (find "metadata_of_pool");
				is_a_snapshot = Opt.default false (Opt.map bool_of_string (find "is_a_snapshot"));
				snapshot_time = Opt.default "19700101T00:00:00Z" (find "snapshot_time");
				snapshot_of = Opt.default "" (find "snapshot_of");
				read_only = Opt.default false (Opt.map bool_of_string (find "read_only"));
				virtual_size = Opt.default 1L (Opt.map Int64.of_string (find "virtual_size"));
				physical_utilisation = 0L;
				persistent = true;
				sm_config = params;
			} in

			let v = Client.VDI.create ~dbg ~sr ~vdi_info in
			Printf.printf "%s\n" (string_of_vdi_info v)
		| [ "vdi-destroy"; sr; vdi ] ->
			Client.VDI.destroy ~dbg ~sr ~vdi
		| [ "vdi-attach"; dp; sr; vdi; rw ] ->
			let x = Client.VDI.attach ~dbg ~dp ~sr ~vdi ~read_write:(String.lowercase rw = "rw") in
			Printf.printf "%s\n" (x |> rpc_of_attach_info |> Jsonrpc.to_string)
		| [ "vdi-detach"; dp; sr; vdi ] ->
			Client.VDI.detach ~dbg ~dp ~sr ~vdi
		| [ "vdi-activate"; dp; sr; vdi ] ->
			Client.VDI.activate ~dbg ~dp ~sr ~vdi
		| [ "vdi-deactivate"; dp; sr; vdi ] ->
			Client.VDI.deactivate ~dbg ~dp ~sr ~vdi
		| [ "vdi-get-by-name"; sr; name ] ->
			let v = Client.VDI.get_by_name ~dbg ~sr ~name in
			Printf.printf "%s\n" (string_of_vdi_info v)
		| [ "vdi-get-by-name"; name ] ->
			let sr, v = Client.get_by_name ~dbg ~name in
			Printf.printf "sr=%s; %s\n" sr (string_of_vdi_info v)
		| [ "vdi-set-content-id"; sr; vdi; content_id ] ->
			Client.VDI.set_content_id ~dbg ~sr ~vdi~content_id
		| [ "vdi-similar-content"; sr; vdi ] ->
			let vs = Client.VDI.similar_content ~dbg ~sr ~vdi in
			List.iter
				(fun v ->
					Printf.printf "%s\n" (string_of_vdi_info v)
				) vs
		| [ "vdi-compose"; sr; vdi1; vdi2 ] ->
			Client.VDI.compose ~dbg ~sr ~vdi1 ~vdi2
		| [ "mirror-copy-into"; sr; vdi; url; dest; dest_vdi ] ->
			let v = Client.DATA.copy_into ~dbg ~sr ~vdi ~url ~dest ~dest_vdi in
			Printf.printf "Created task %s\n" v
		| [ "vdi-get-url"; sr; vdi ] ->
			let x = Client.VDI.get_url ~dbg ~sr ~vdi in
			Printf.printf "%s\n" x
		| [ "mirror-start"; sr; vdi; dp; url; dest ] ->
			let task = Client.DATA.MIRROR.start ~dbg ~sr ~vdi ~dp ~url ~dest in
			Printf.printf "Task id: %s\n" task
		| [ "mirror-stop"; id ] ->
			Client.DATA.MIRROR.stop ~dbg ~id
		| [ "mirror-list"; ] ->
			let list = Client.DATA.MIRROR.list ~dbg in
			let open Storage_interface.Mirror in
			List.iter (fun (id,status) ->
				Printf.printf "id: %s\nsrc_vdi: %s\ndest_vdi: %s\nstatus: %s\nfailed: %b\n" 
					id
					status.source_vdi
					status.dest_vdi
					(String.concat "," (List.map (fun s -> match s with | Receiving -> "Receiving" | Sending -> "Sending") status.state))
					status.failed) list
		| [ "task-list" ] ->
			let tasks = Client.TASK.list ~dbg in
			let open Storage_interface.Task in

			List.iter (fun t ->
				Printf.printf "%-8s %-12f %-30s %s\n" t.Task.id t.Task.ctime t.Task.debug_info (t.Task.state |> Task.rpc_of_state |> Jsonrpc.to_string);
				List.iter
					(fun (name, state) ->
						Printf.printf "  |_ %-30s %s\n" name (state |> Task.rpc_of_state |> Jsonrpc.to_string)
					) t.Task.subtasks
					) tasks
		| ["task-cancel"; task ] ->
			Client.TASK.cancel ~dbg ~task
		| _ ->
			usage_and_exit ()
