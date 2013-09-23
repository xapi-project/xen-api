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

let old_main () =
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

let project_url = "http://github.com/djs55/sm-cli"

open Cmdliner

module Common = struct
	type t = {
        verbose: bool;
        debug: bool;
        socket: string;
	} with rpc

	let make verbose debug socket queue =
		begin match queue with
		| None -> ();
		| Some name ->
			Storage_interface.queue_name := name;
			Xcp_client.use_switch := true
		end;
		{ verbose; debug; socket }

	let to_string x = Jsonrpc.to_string (rpc_of_t x)
end

let _common_options = "COMMON OPTIONS"

(* Options common to all commands *)
let common_options_t = 
  let docs = _common_options in 
  let debug = 
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in 
    Arg.(last & vflag_all [false] [verbose]) in 
  let socket = 
    let doc = Printf.sprintf "Specify path to the server Unix domain socket." in
    Arg.(value & opt file !Storage_interface.default_path & info ["socket"] ~docs ~doc) in
  let queue =
    let doc = Printf.sprintf "Specify queue name in message switch." in
    Arg.(value & opt (some string) None & info ["queue"] ~docs ~doc) in
  Term.(pure Common.make $ debug $ verb $ socket $ queue)


(* Help sections common to all commands *)
let help = [ 
 `S _common_options; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Commands *)

let wrap common_opts f =
	Storage_interface.default_path := common_opts.Common.socket;
	try
		f ();
		`Ok ()
	with
	| Unix.Unix_error((Unix.ECONNREFUSED|Unix.ENOENT), "connect", _) as e->
		Printf.fprintf stderr "Failed to connect to %s: %s\n%!" common_opts.Common.socket (Printexc.to_string e);
		Printf.fprintf stderr "Check whether the storage service is listening and try again.\n%!";
		`Error(false, "could not connect to service")
	| Unix.Unix_error(Unix.EACCES, "connect", _) as e ->
		Printf.fprintf stderr "Failed to connect to %s: %s\n%!" common_opts.Common.socket (Printexc.to_string e);
		Printf.fprintf stderr "Ensure this program is being run as root and try again.\n%!";
		`Error(false, "permission denied")
	| Storage_interface.Backend_error(code, params) ->
		Printf.fprintf stderr "Error from storage backend:\n";
		Printf.fprintf stderr "%s: [ %s ]\n" code (String.concat "; " params);
		exit 1

let query common_opts =
  wrap common_opts (fun () ->
    let q = Client.Query.query ~dbg in
    Printf.printf "%s\n" (q |> rpc_of_query_result |> Jsonrpc.to_string)
  )

let filename_suffix = "-filename"
let filename_suffix_regex = Re_str.regexp_string filename_suffix

let string_of_file filename =
  let ic = open_in filename in
  let output = Buffer.create 1024 in
  try
    while true do
      let block = String.make 4096 '\000' in
      let n = input ic block 0 (String.length block) in
      if n = 0 then raise End_of_file;
      Buffer.add_substring output block 0 n
    done;
    "" (* never happens *)
  with End_of_file ->
    close_in ic;
    Buffer.contents output

let mirror_list common_opts =
  wrap common_opts (fun () -> 
      let list = Client.DATA.MIRROR.list ~dbg in
      let open Storage_interface.Mirror in
      List.iter (fun (id,status) ->
          Printf.printf "id: %s\nsrc_vdi: %s\ndest_vdi: %s\nstatus: %s\nfailed: %b\n" 
	    id status.source_vdi status.dest_vdi
            (String.concat "," (List.map (fun s -> match s with | Receiving -> "Receiving" | Sending -> "Sending") status.state))
            status.failed) list)

let sr_attach common_opts sr device_config = match sr with
  | None -> `Error(true, "must supply SR")
  | Some sr ->
    (* Read the advertised device_config from the driver *)
    let q = Client.Query.query ~dbg in
    let expected_device_config_keys = List.map fst q.configuration in
    (* The first 'device_config' will actually be the sr *)
    let device_config = List.tl device_config in
    let device_config = List.map (fun x -> match Re_str.bounded_split (Re_str.regexp_string "=") x 2 with
    | [ k; v ] when List.mem k expected_device_config_keys -> k, v
    | [ k; v ] -> begin match Re_str.bounded_split_delim filename_suffix_regex k 2 with
      | [ k'; "" ] ->
        (* We will send the contents of the file [v] as the value and [k'] as the key *)
        if not(Sys.file_exists v) then begin
          Printf.fprintf stderr "File does not exist: %s\n%!" v;
          exit 1
        end;
        if not (List.mem k' expected_device_config_keys) then begin
          Printf.fprintf stderr "unexpected device_config key: %s (expected: %s)\n" k (String.concat ", " expected_device_config_keys);
          exit 1
        end;
        k', string_of_file v
      | _ ->
        Printf.fprintf stderr "unexpected device_config key: %s (expected: %s)\n" k (String.concat ", " expected_device_config_keys);
        exit 1
      end
    | _ ->
      Printf.fprintf stderr "device_config arguments need to be of the form key=value (got '%s')\n" x;
      exit 1
    ) device_config in
    wrap common_opts (fun () ->
      Client.SR.attach ~dbg ~sr ~device_config
    )

let sr_detach common_opts sr = match sr with
  | None -> `Error(true, "must supply SR")
  | Some sr ->
    wrap common_opts (fun () ->
      Client.SR.detach ~dbg ~sr
    )

let sr_stat common_opts sr = match sr with
  | None -> `Error(true, "must supply SR")
  | Some sr ->
    wrap common_opts (fun () ->
      let sr_info = Client.SR.stat ~dbg ~sr in
      Printf.fprintf stdout "Total space on substrate:      %Ld\n" sr_info.total_space;
      Printf.fprintf stdout "Free space on substrate:       %Ld\n" sr_info.free_space
    )

let sr_scan common_opts sr = match sr with
  | None -> `Error(true, "must supply SR")
  | Some sr ->
    wrap common_opts (fun () ->
      let vdis = Client.SR.scan ~dbg ~sr in
      List.iter (fun vdi ->
        Printf.fprintf stdout "%s: %s\n" vdi.vdi (Jsonrpc.to_string (rpc_of_vdi_info vdi))
      ) vdis
    )

let parse_size x =
  let kib = 1024L in
  let mib = Int64.mul kib kib in
  let gib = Int64.mul mib kib in
  let tib = Int64.mul gib kib in
  let endswith suffix x =
    let suffix' = String.length suffix in
    let x' = String.length x in
    x' >= suffix' && (String.sub x (x' - suffix') suffix' = suffix) in
  let remove suffix x =
    let suffix' = String.length suffix in
    let x' = String.length x in
    String.sub x 0 (x' - suffix') in
  try
    if endswith "KiB" x then Int64.(mul kib (of_string (remove "KiB" x)))
    else if endswith "MiB" x then Int64.(mul mib (of_string (remove "MiB" x)))
    else if endswith "GiB" x then Int64.(mul gib (of_string (remove "GiB" x)))
    else if endswith "TiB" x then Int64.(mul tib (of_string (remove "TiB" x)))
    else Int64.of_string x
  with _ ->
    failwith (Printf.sprintf "Cannot parse size: %s" x)

let vdi_create common_opts sr name descr virtual_size format = match sr with
  | None -> `Error(true, "must supply SR")
  | Some sr ->
    wrap common_opts (fun () ->
      let vdi_info = {
        vdi = "";
        content_id = "";
        name_label = name;
        name_description = descr;
        ty = "user";
        metadata_of_pool = "";
        is_a_snapshot = false;
        snapshot_time = "";
        snapshot_of = "";
        read_only = false;
        virtual_size = parse_size virtual_size;
        physical_utilisation = 0L;
        sm_config = (match format with None -> [] | Some x -> ["type", x]);
        persistent = true;
      } in
      let vdi_info = Client.VDI.create ~dbg ~sr ~vdi_info in
      Printf.printf "%s\n" vdi_info.vdi
    )

let on_vdi f common_opts sr vdi = match sr, vdi with
  | None, _ -> `Error(true, "must supply SR")
  | _, None -> `Error(true, "must supply VDI")
  | Some sr, Some vdi ->
    wrap common_opts (fun () -> f sr vdi)

let mirror_start common_opts sr vdi dp url dest =
  on_vdi (fun sr vdi ->
      let get_opt x err =
        match x with Some y -> y | None -> failwith err in
      let dp = get_opt dp "Need a local data path" in
      let url = get_opt url "Need a URL" in
      let dest = get_opt dest "Need a destination SR" in
      let task = Client.DATA.MIRROR.start ~dbg ~sr ~vdi ~dp ~url ~dest in
      Printf.printf "Task id: %s\n" task) common_opts sr vdi

let mirror_stop common_opts id =
  wrap common_opts (fun () -> 
      match id with Some id -> Client.DATA.MIRROR.stop ~dbg ~id 
                  | None -> failwith "Need an ID")

let vdi_clone common_opts sr vdi name descr = on_vdi
  (fun sr vdi ->
    wrap common_opts (fun () ->
      let vdi_info = Client.VDI.stat ~dbg ~sr ~vdi in
      let vdi_info = { vdi_info with
        name_label = (match name with None -> vdi_info.name_label | Some x -> x);
        name_description = (match descr with None -> vdi_info.name_description | Some x -> x);
      } in
      let vdi_info = Client.VDI.clone ~dbg ~sr ~vdi_info in
      Printf.printf "%s\n" vdi_info.vdi
    )
  ) common_opts sr vdi

let vdi_resize common_opts sr vdi new_size = on_vdi
  (fun sr vdi ->
    match new_size with
    | None -> `Error(true, "must supply a new size")
    | Some new_size ->
      let new_size = parse_size new_size in
      wrap common_opts (fun () ->
        let new_size = Client.VDI.resize ~dbg ~sr ~vdi ~new_size in
        Printf.printf "%Ld\n" new_size
      )
  ) common_opts sr vdi

let vdi_destroy common_opts sr vdi =
  on_vdi (fun sr vdi ->
    Client.VDI.destroy ~dbg ~sr ~vdi
  ) common_opts sr vdi

let vdi_attach common_opts sr vdi =
  on_vdi (fun sr vdi ->
    let info = Client.VDI.attach ~dbg ~dp:dbg ~sr ~vdi ~read_write:true in
    Printf.printf "%s\n" (Jsonrpc.to_string (rpc_of_attach_info info))
  ) common_opts sr vdi

let vdi_detach common_opts sr vdi =
  on_vdi (fun sr vdi ->
    Client.VDI.detach ~dbg ~dp:dbg ~sr ~vdi
  ) common_opts sr vdi

let vdi_activate common_opts sr vdi =
  on_vdi (fun sr vdi ->
    Client.VDI.activate ~dbg ~dp:dbg ~sr ~vdi
  ) common_opts sr vdi

let vdi_deactivate common_opts sr vdi =
  on_vdi (fun sr vdi ->
    Client.VDI.deactivate ~dbg ~dp:dbg ~sr ~vdi
  ) common_opts sr vdi

let query_cmd =
  let doc = "query the capabilities of a storage service" in
  let man = [
    `S "DESCRIPTION";
    `P "Queries the capabilities, vendor and version information from a storage service.";
  ] @ help in
  Term.(ret(pure query $ common_options_t)),
  Term.info "query" ~sdocs:_common_options ~doc ~man

let sr_arg =
  let doc = "unique identifier for this storage repository (typically a uuid)" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"SR" ~doc)

let vdi_arg =
  let doc = "unique identifier for this VDI within this storage repository" in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"VDI" ~doc)

let mirror_list_cmd =
  let doc = "List the active VDI mirror operations" in
  let man = [
    `S "DESCRIPTION";
    `P "Lists any receiving or sending mirror operations that are currently active";
  ] @ help in
  Term.(ret(pure mirror_list $ common_options_t)),
  Term.info "mirror-list" ~sdocs:_common_options ~doc ~man

let mirror_start_cmd =
  let doc = "Start mirroring a VDI to a different SR" in
  let man = [
    `S "DESCRIPTION";
    `P "Start a mirror operation that will initialise and then actively mirror the contents of a VDI to a different (possibly remote) SR.";
    `P "The local datapath needs to be attached already"] @ help in
  let dp = 
    let doc = "Local data path attaching the VDI" in
    Arg.(value & pos 2 (some string) None & info [] ~docv:"DP" ~doc) in
  let url =
    let doc = "URL of the (possibly remote) storage service" in
    Arg.(value & pos 3 (some string) None & info [] ~docv:"URL" ~doc) in
  let dest =
    let doc = "Destination SR" in
    Arg.(value & pos 4 (some string) None & info [] ~docv:"REMOTESR" ~doc) in
  Term.(ret(pure mirror_start $ common_options_t $ sr_arg $ vdi_arg $ dp $ url $ dest)),
  Term.info "mirror-start" ~sdocs:_common_options ~doc ~man

let mirror_stop_cmd =
  let doc = "Stop a currently-active mirror" in
  let man = [
    `S "DESCRIPTION";
    `P "Stop a currently-active mirror";] in
  let id = 
    let doc = "ID of the mirror" in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"ID" ~doc) in
  Term.(ret(pure mirror_stop $ common_options_t $ id)),
  Term.info "mirror-stop" ~sdocs:_common_options ~doc ~man

let sr_attach_cmd =
  let doc = "storage repository configuration in the form of key=value pairs" in
  let device_config = Arg.(value & (pos_all string []) & info [] ~docv:"DEVICE-CONFIG" ~doc) in
  let doc = "connect to a storage repository" in
  let man = [
    `S "DESCRIPTION";
    `P "Prepare a storage repository so that it can be accessed from this host. Once a storage repository has been attached, it is possible to query metadata, create/destroy/attach/detach virtual disks.";
    `P "Each storage repository requires a set of configuration key/value pairs. Use the \"query\" sub-command to list the required configuration parameters.";
    `S "On configuration syntax:";
    `P "Simple parameters may be written directly on the commandline as:";
    `P "key=value";
    `P "If a particular value is stored in a file (e.g. as XML) then you may write:";
    `P " key-filename=<filename containing the value>.";
  ] @ help in
  Term.(ret(pure sr_attach $ common_options_t $ sr_arg $ device_config)),
  Term.info "sr-attach" ~sdocs:_common_options ~doc ~man

let sr_detach_cmd =
  let doc = "disconnect from a storage repository" in
  let man = [
    `S "DESCRIPTION";
    `P "Disconnects from a connected storage repository, and frees any associated resources (e.g. iSCSI sessions, other control connections etc).";
  ] @ help in
  Term.(ret(pure sr_detach $ common_options_t $ sr_arg)),
  Term.info "sr-detach" ~sdocs:_common_options ~doc ~man

let sr_stat_cmd =
  let doc = "query global SR statistics" in
  let man = [
    `S "DESCRIPTION";
    `P "Query the global SR statistics including: (i) total virtual space allocated to VDIs; (ii) total amount of space on the storage substrate; and (iii) amount of storage instantaneously free.";
  ] @ help in
  Term.(ret(pure sr_stat $ common_options_t $ sr_arg)),
  Term.info "sr-stat" ~sdocs:_common_options ~doc ~man

let sr_scan_cmd =
  let doc = "list all the virtual disks in a storage repository" in
  let man = [
    `S "DESCRIPTION";
    `P "Lists all virtual disks and all their associated metadata within a given storage repository.";
  ] @ help in
  Term.(ret(pure sr_scan $ common_options_t $ sr_arg)),
  Term.info "sr-scan" ~sdocs:_common_options ~doc ~man

let vdi_create_cmd =
  let name_arg =
    let doc = "short name for the virtual disk" in
    Arg.(value & opt string "name" & info ["name"] ~docv:"NAME" ~doc) in
  let descr_arg =
    let doc = "longer description for the virtual disk" in
    Arg.(value & opt string "" & info ["description"] ~docv:"DESCRIPTION" ~doc) in
  let virtual_size_arg =
    let doc = "size of the disk" in
    Arg.(value & opt string "0" & info ["size"] ~docv:"SIZE" ~doc) in
  let format_arg =
    let doc = "Request a specific format for the disk on the backend storage substrate, e.g. 'vhd' or 'raw'. Note that not all storage implementations support all formats. Every storage implementation will use its preferred format if no override is supplied." in
    Arg.(value & opt (some string) None & info ["format"] ~docv:"FORMAT" ~doc) in

  let doc = "create a new virtual disk in a storage repository" in
  let man = [
    `S "DESCRIPTION";
    `P "Create an empty virtual disk in a storage repository.";
  ] @ help in
  Term.(ret(pure vdi_create $ common_options_t $ sr_arg $ name_arg $ descr_arg $ virtual_size_arg $ format_arg)),
  Term.info "vdi-create" ~sdocs:_common_options ~doc ~man

let vdi_clone_cmd =
  let name_arg =
    let doc = "short name for the virtual disk" in
    Arg.(value & opt (some string) None & info ["name"] ~docv:"NAME" ~doc) in
  let descr_arg =
    let doc = "longer description for the virtual disk" in
    Arg.(value & opt (some string) None & info ["description"] ~docv:"DESCRIPTION" ~doc) in
  let doc = "clone a virtual disk." in
  let man = [
    `S "DESCRIPTION";
    `P "Clones an existing virtual disk. This operation produces a new virtual disk whose content is initially the same as the original virtual disk."
  ] @ help in
  Term.(ret(pure vdi_clone $ common_options_t $ sr_arg $ vdi_arg $ name_arg $ descr_arg)),
  Term.info "vdi-clone" ~sdocs:_common_options ~doc ~man

let vdi_resize_cmd =
  let new_size_arg =
    let doc = "new virtual_size for the disk" in
    Arg.(value & pos 2 (some string) None & info [] ~docv:"SIZE" ~doc) in
  let doc = "resize a virtual disk." in
  let man = [
    `S "DESCRIPTION";
    `P "Changes the virtual_size of a given disk. The VM may or may not notice the size change. The disk may or may not physically expand on the physical storage substrate."
  ] @ help in
  Term.(ret(pure vdi_resize $ common_options_t $ sr_arg $ vdi_arg $ new_size_arg)),
  Term.info "vdi-resize" ~sdocs:_common_options ~doc ~man

let vdi_destroy_cmd =
  let doc = "destroy an existing virtual disk in a storage repository." in
  let man = [
    `S "DESCRIPTION";
    `P "Destroy an existing virtual disk in a storage repository.";
  ] @ help in
  Term.(ret(pure vdi_destroy $ common_options_t $ sr_arg $ vdi_arg)),
  Term.info "vdi-destroy" ~sdocs:_common_options ~doc ~man

let vdi_attach_cmd =
  let doc = "attach a virtual disk." in
  let man = [
    `S "DESCRIPTION";
    `P "Attach a virtual disk. This will allocate resources and prepare the disk to be used by a Virtual Machine. The disk won't be readable or writable until a call to vdi-activate.";
  ] @ help in
  Term.(ret(pure vdi_attach $ common_options_t $ sr_arg $ vdi_arg)),
  Term.info "vdi-attach" ~sdocs:_common_options ~doc ~man

let vdi_detach_cmd =
  let doc = "detach a virtual disk." in
  let man = [
    `S "DESCRIPTION";
    `P "Detach a virtual disk. This will deallocate any resources consumed by the disk.";
  ] @ help in
  Term.(ret(pure vdi_detach $ common_options_t $ sr_arg $ vdi_arg)),
  Term.info "vdi-detach" ~sdocs:_common_options ~doc ~man

let vdi_activate_cmd =
  let doc = "activate a virtual disk." in
  let man = [
    `S "DESCRIPTION";
    `P "Activate a virtual disk. This makes it possible for a Virtual Machine to read or write the disk.";
  ] @ help in
  Term.(ret(pure vdi_activate $ common_options_t $ sr_arg $ vdi_arg)),
  Term.info "vdi-activate" ~sdocs:_common_options ~doc ~man

let vdi_deactivate_cmd =
  let doc = "deactivate a virtual disk." in
  let man = [
    `S "DESCRIPTION";
    `P "Deactivate a virtual disk. It will nolonger be possible for a Virtual Machine to read or write the disk. When this command completes, all outstanding I/O will have been flushed.";
  ] @ help in
  Term.(ret(pure vdi_deactivate $ common_options_t $ sr_arg $ vdi_arg)),
  Term.info "vdi-deactivate" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "interact with an XCP storage management service" in 
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "sm-cli" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [query_cmd; sr_attach_cmd; sr_detach_cmd; sr_stat_cmd; sr_scan_cmd;
            vdi_create_cmd; vdi_destroy_cmd; vdi_attach_cmd; vdi_detach_cmd;
            vdi_activate_cmd; vdi_deactivate_cmd; vdi_clone_cmd; vdi_resize_cmd; 
            mirror_list_cmd; mirror_start_cmd; mirror_stop_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
