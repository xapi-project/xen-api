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
open Xapi_stdext_monadic
module StringSet = Set.Make(String)

(* Server configuration. We have built-in (hopefully) sensible defaults,
   together with command-line arguments and a configuration file. They
   are applied in order: (latest takes precedence)
      defaults < arguments < config file
*)
let default_service_name = Filename.basename Sys.argv.(0)
let config_file = ref (Printf.sprintf "/etc/%s.conf" default_service_name)
let config_dir = ref (Printf.sprintf "/etc/%s.conf.d" default_service_name)
let pidfile = ref (Printf.sprintf "/var/run/%s.pid" default_service_name)
let extra_search_path = ref []
let log_destination = ref "syslog:daemon"
let log_level = ref Syslog.Debug
let daemon = ref false
let have_daemonized () = Unix.getppid () = 1

let common_prefix = "org.xen.xapi."

let (|>) x f = f x

let finally f g =
	try
 		let result = f () in
		g ();
		result
	with e ->
		g ();
		raise e

type opt = string * Arg.spec * (unit -> string) * string

module D = Debug.Make(struct let name = default_service_name end)
open D

module Config_file = struct
	open Arg

	let apply v = function
	| Unit f -> f ()
	| Bool f -> f (bool_of_string v)
	| Set b -> b := (bool_of_string v)
	| Clear b -> b := not (bool_of_string v)
	| String f -> f v
	| Set_string s -> s := v
	| Int f -> f (int_of_string v)
	| Set_int i -> i := (int_of_string v)
	| Float f -> f (float_of_string v)
	| Set_float f -> f := (float_of_string v)
	| _ -> failwith "Unsupported type in config file"

	(* Trim trailing whitespace from a line *)
	let trim_trailing_ws line =
		let re_ws = Re.compile (Re_emacs.re "[ \t]+$") in
		try
			let ofs = fst (Re.get_all_ofs (Re.exec re_ws line)).(0) in
			String.sub line 0 ofs
		with Not_found ->
			line

	let trim_comment line =
		try
			let i = String.index line '#' in
			String.sub line 0 i
		with Not_found -> line

	let get_kv line =
		let re = Re.compile (Re_emacs.re "\\([^=\\ \t]+\\)[\\ \t]*=[\\ \t]*\\(.*\\)") in
		let get (x,y) = String.sub line x (y-x) in
		try
			match Re.get_all_ofs (Re.exec re line) with
			| [| _; key_ofs; v_ofs |] ->
				(* First in array is always the full extent of all matches *)
				Some (get key_ofs, get v_ofs)
			| _ ->
				None
		with _ ->
			None

	let strip_quotes (k,v) =
		if String.length v < 2
		then (k,v)
		else
			let first = v.[0] and last = v.[String.length v - 1] in
			if first = last && (first = '"' || first = '\'')
			then (k,String.sub v 1 (String.length v - 2))
			else (k,v)

	let parse_line line =
		(* Strip comments *)
		let stripped = line |> trim_comment |> trim_trailing_ws in
		let lift f x = Some (f x) in
		let (>>=) m f = match m with Some x -> f x | None -> None in
		get_kv stripped >>= lift strip_quotes

	let process_line data spec =
		let spec = List.map (fun (a, b, _, _) -> a, b) spec in
		match parse_line data with
		| Some (key,v) ->
			if List.mem_assoc key spec then apply v (List.assoc key spec)
		| None -> ()

	let parse filename spec =
		(* Remove the unnecessary doc parameter *)
		let ic = open_in filename in
		finally
		(fun () ->
			try
				while true do
					let line = input_line ic in
					process_line line spec
				done
			with End_of_file -> ()
		) (fun () -> close_in ic)

	let dump spec =
		List.iter (fun (name, _, printer, description) ->
			debug "%s = %s (%s)" name (printer ()) description
		) spec

end

let rec split_c c str =
  try
    let i = String.index str c in
    String.sub str 0 i :: (split_c c (String.sub str (i+1) (String.length str - i - 1)))
  with Not_found -> [str]

let setify =
  let rec loop acc = function
    | [] -> acc
    | x :: xs -> (if List.mem x acc then loop acc else loop (x :: acc)) xs in
  loop []

let common_options = [
	"use-switch", Arg.Bool (fun b -> Xcp_client.use_switch := b), (fun () -> string_of_bool !Xcp_client.use_switch), "true if the message switch is to be enabled";
	"switch-path", Arg.Set_string Xcp_client.switch_path, (fun () -> !Xcp_client.switch_path), "Unix domain socket path on localhost where the message switch is listening";
	"search-path", Arg.String (fun s -> extra_search_path := (split_c ':' s) @ !extra_search_path), (fun () -> String.concat ":" !extra_search_path), "Search path for resources";
	"pidfile", Arg.Set_string pidfile, (fun () -> !pidfile), "Filename to write process PID";
	"log", Arg.Set_string log_destination, (fun () -> !log_destination), "Where to write log messages";
	"daemon", Arg.Bool (fun x -> daemon := x), (fun () -> string_of_bool !daemon), "True if we are to daemonise";
	"disable-logging-for", Arg.String
		(fun x -> debug "Parsing [%s]" x;
			try
				let modules = List.filter (fun x -> x <> "") (split_c ' ' x) in
				List.iter Debug.disable modules
			with e ->
				error "Processing disabled-logging-for = %s: %s" x (Printexc.to_string e)
		), (fun () -> String.concat " " (setify (List.map fst (Debug.disabled_modules ())))), "A space-separated list of debug modules to suppress logging from";

	"loglevel", Arg.String 
		(fun x ->
			debug "Parsing [%s]" x;
			try
				log_level := Syslog.level_of_string x;
				Debug.set_level !log_level
			with e ->
				error "Processing loglevel = %s: %s" x (Printexc.to_string e)), 
		(fun () -> Syslog.string_of_level !log_level), "Log level";

	"inventory", Arg.Set_string Inventory.inventory_filename, (fun () -> !Inventory.inventory_filename), "Location of the inventory file";
	"config", Arg.Set_string config_file, (fun () -> !config_file), "Location of configuration file";
	"config-dir", Arg.Set_string config_dir, (fun () -> !config_dir), "Location of directory containing configuration file fragments";
]

let loglevel () = !log_level

module Term = Cmdliner.Term

let rec list = function
  | [] -> Term.pure []
  | x :: xs -> Term.app (Term.app (Term.pure (fun x y -> x :: y)) x) (list xs)

let command_of ?(name = Sys.argv.(0)) ?(version = "unknown") ?(doc = "Please describe this command.") xs =
  let term_of_option (key, arg, get_fn, doc) =
    let default = get_fn () in
    match arg with
    | Arg.Unit f ->
      let t = Cmdliner.Arg.(value & flag & info [ key ] ~doc) in
      let make = function true -> f () | false -> () in
      Term.(pure make $ t)
    | Arg.Bool f ->
      let t = Cmdliner.Arg.(value & opt bool (bool_of_string default) & info [ key ] ~doc) in
      Term.(pure f $ t)
    | Arg.Set b ->
      let t = Cmdliner.Arg.(value & opt bool (bool_of_string default) & info [ key ] ~doc) in
      let make v = b := v in
      Term.(pure make $ t)
    | Arg.Clear b ->
      let t = Cmdliner.Arg.(value & opt bool (bool_of_string default) & info [ key ] ~doc) in
      let make v = b := not v in
      Term.(pure make $ t)
    | Arg.String f ->
      let t = Cmdliner.Arg.(value & opt string default & info [ key ] ~doc) in
      Term.(pure f $ t)
    | Arg.Set_string s ->
      let t = Cmdliner.Arg.(value & opt string default & info [ key ] ~doc) in
      let make v = s := v in
      Term.(pure make $ t)
    | Arg.Int f ->
      let t = Cmdliner.Arg.(value & opt int (int_of_string default) & info [ key ] ~doc) in
      Term.(pure f $ t)
    | Arg.Set_int s ->
      let t = Cmdliner.Arg.(value & opt int (int_of_string default) & info [ key ] ~doc) in
      let make v = s := v in
      Term.(pure make $ t)
    | Arg.Float f ->
      let t = Cmdliner.Arg.(value & opt float (float_of_string default) & info [ key ] ~doc) in
      Term.(pure f $ t)
    | Arg.Set_float s ->
      let t = Cmdliner.Arg.(value & opt float (float_of_string default) & info [ key ] ~doc) in
      let make v = s := v in
      Term.(pure make $ t)
    | _ ->
      let t = Cmdliner.Arg.(value & opt string default & info [ key ] ~doc) in
      let make v = Config_file.apply v arg in
      Term.(pure make $ t) in
  let terms = List.map term_of_option xs in

  let _common_options = "COMMON OPTIONS" in
  let man = [
    `S "DESCRIPTION";
    `P doc;
    `S _common_options;
    `P "These options are common to all services.";
    `S "BUGS";
    `P "Check bug reports at http://github.com/xapi-project/xcp-idl";
  ] in
  Term.(ret(pure (fun (_: unit list) -> `Ok ()) $ (list terms))),
  Term.info name ~version ~sdocs:_common_options ~man

let arg_spec = List.map (fun (a, b, _, c) -> "-" ^ a, b, c)

type res = {
	name: string;
	description: string;
	essential: bool;
	path: string ref;
	perms: Unix.access_permission list
}

let default_resources = [
]

let canonicalise x =
	if not(Filename.is_relative x)
	then x
	else begin
		(* Search the PATH and XCP_PATH for the executable *)
		let paths = split_c ':' (Sys.getenv "PATH") in
		let first_hit = List.fold_left (fun found path -> match found with
			| Some _hit -> found
			| None ->
				let possibility = Filename.concat path x in
				if Sys.file_exists possibility
				then Some possibility
				else None
		) None (paths @ !extra_search_path) in
		match first_hit with
		| None ->
			warn "Failed to find %s on $PATH ( = %s) or search_path option ( = %s)" x (Sys.getenv "PATH") (String.concat ":" !extra_search_path);
      x
		| Some hit ->
			info "Found '%s' at '%s'" x hit;
			hit
	end

let to_opt = List.map (fun f -> f.name, Arg.String (fun x -> f.path := canonicalise x), (fun () -> !(f.path)), f.description)

let read_config_file x =
	if Sys.file_exists !config_file then begin
		(* Will raise exception if config is mis-formatted. It's up to the
		   caller to inspect and handle the failure.
		*)
		Config_file.parse !config_file x;
	end;
	(try Sys.readdir !config_dir with _ -> [||])
	|> Array.to_list
	|> List.stable_sort compare
	|> List.iter
		(fun fragment ->
			let path = Filename.concat !config_dir fragment in
			Config_file.parse path x
		)

let startswith prefix x =
        let prefix' = String.length prefix and x' = String.length x in
        prefix' <= x' && (String.sub x 0 prefix' = prefix)

let configure_common ~options ~resources arg_parse_fn =
	let resources = default_resources @ resources in
	let config_spec = common_options @ options @ (to_opt resources) in

  (* It's very confusing if there are duplicate key names *)
  let keys = List.map (fun (k, _, _, _) -> k) config_spec in
  let rec check_for_duplicates seen_already = function
  | [] -> ()
  | x :: xs ->
    if List.mem x seen_already then begin
      warn "Duplicate configuration keys in Xcp_service.configure: %s in [ %s ]"
        x (String.concat "; " keys)
    end;
    check_for_duplicates (x :: seen_already) xs in
  check_for_duplicates [] keys;

  arg_parse_fn config_spec;
	read_config_file config_spec;
	List.iter (fun r -> r.path := canonicalise !(r.path)) resources;
	Config_file.dump config_spec;
	(* Check the required binaries are all available *)
	List.iter
		(fun f ->
			try
				if f.essential
				then Unix.access !(f.path) f.perms
			with _ ->
                                let args = List.filter (fun x -> not(startswith ("--" ^ f.name) x)) (Array.to_list Sys.argv) in
                                let lines = [
                                        "Cannot access " ^ !(f.path);
                                        Printf.sprintf "Please either add to %s" !config_file;
                                        Printf.sprintf "  %s=<%s>" f.name f.description;
                                        "or add a command-line argument";
                                        Printf.sprintf "  %s --%s=<%s>" (String.concat " " args) f.name f.description;
                                ] in
                                List.iter (fun x -> error "%s" x) lines;
                                failwith (String.concat "\n" lines)
		) resources;

	Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let configure ?(options=[]) ?(resources=[]) () =
  try
    configure_common ~options ~resources
      (fun config_spec ->
        Arg.parse (Arg.align (arg_spec config_spec))
          (fun _ -> failwith "Invalid argument")
          (Printf.sprintf "Usage: %s [-config filename]" Sys.argv.(0))
      )
  with Failure _ ->
    exit 1

type ('a, 'b) error = [
  | `Ok of 'a
  | `Error of 'b
]

let configure2 ~name ~version ~doc ?(options=[]) ?(resources=[]) () =
  try
    Xcp_coverage.init name;
    configure_common ~options ~resources
      (fun config_spec ->
        match Term.eval (command_of ~name ~version ~doc config_spec) with
        | `Ok () -> ()
        | `Error _ -> failwith "Failed to parse command-line arguments"
        | _ -> exit 0 (* --help *)
      );
    `Ok ()
  with Failure m ->
    `Error m

let http_handler call_of_string string_of_response process s =
	let ic = Unix.in_channel_of_descr s in
	let oc = Unix.out_channel_of_descr s in
	let module Request = Cohttp.Request.Make(Cohttp_posix_io.Buffered_IO) in
	let module Response = Cohttp.Response.Make(Cohttp_posix_io.Buffered_IO) in
	match Request.read ic with
	| `Eof ->
		debug "Failed to read HTTP request"
        | `Invalid x ->
		debug "Failed to read HTTP request. Got: '%s'" x
	| `Ok req ->
		begin match Cohttp.Request.meth req, Uri.path (Cohttp.Request.uri req) with
		| `POST, _ ->
			let headers = Cohttp.Request.headers req in
			begin match Cohttp.Header.get headers "content-length" with
			| None ->
				debug "Failed to read content-length"
			| Some content_length ->
				let content_length = int_of_string content_length in
				let request_txt = String.make content_length '\000' in
				really_input ic request_txt 0 content_length;
				let rpc_call = call_of_string request_txt in
				debug "%s" (Rpc.string_of_call rpc_call);
				let rpc_response = process rpc_call in
				debug "   %s" (Rpc.string_of_response rpc_response);
				let response_txt = string_of_response rpc_response in
				let content_length = String.length response_txt in
				let headers = Cohttp.Header.of_list [
					"user-agent", default_service_name;
					"content-length", string_of_int content_length;
				] in
				let response = Cohttp.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers ~encoding:(Cohttp.Transfer.Fixed (Int64.of_int content_length)) () in
				Response.write (fun t -> Response.write_body t response_txt) response oc
			end
		| _, _ ->
			let content_length = 0 in
			let headers = Cohttp.Header.of_list [
				"user-agent", default_service_name;
				"content-length", string_of_int content_length;
			] in
			let response = Cohttp.Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers ~encoding:(Cohttp.Transfer.Fixed (Int64.of_int content_length)) () in
			Response.write (fun _t -> ()) response oc
		end

let ign_int (t:int)         = ignore t

let default_raw_fn rpc_fn s =
	http_handler Xmlrpc.call_of_string Xmlrpc.string_of_response rpc_fn s

let mkdir_rec dir perm =
	let rec p_mkdir dir =
		let p_name = Filename.dirname dir in
		if p_name <> "/" && p_name <> "."
		then p_mkdir p_name;
		(try Unix.mkdir dir perm  with Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in
	p_mkdir dir

type server =
  | Socket of Unix.file_descr * (Unix.file_descr -> unit)
  | Queue of string * (Rpc.call -> Rpc.response)

(* Start accepting connections on sockets before we daemonize *)
let make_socket_server path fn =
	try
		(try Unix.unlink path with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
		mkdir_rec (Filename.dirname path) 0o0755;
		let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
		Unix.bind sock (Unix.ADDR_UNIX path);
		Unix.listen sock 5;
		info "Listening on %s" path;
		Socket (sock, fn)
	with e ->
		error "Failed to listen on Unix domain socket %s. Raw error was: %s" path (Printexc.to_string e);
		begin match e with
		| Unix.Unix_error(Unix.EACCES, _, _) ->
			error "Access was denied.";
			error "Possible fixes include:";
			error "1. Run this program as root (recommended)";
			error "2. Make the permissions in the filesystem more permissive (my effective uid is %d)" (Unix.geteuid ());
			error "3. Adjust the sockets-path directive in %s" !config_file;
			exit 1
		| _ -> ()
		end;
		raise e

let make_queue_server name fn =
	Queue(name, fn) (* TODO: connect to the message switch *)

let make ~path ~queue_name ?raw_fn ~rpc_fn () =
	if !Xcp_client.use_switch
	then make_queue_server queue_name rpc_fn
	else make_socket_server path (match raw_fn with
		| Some x -> x
		| None -> default_raw_fn rpc_fn
		)

let serve_forever = function
	| Socket(listening_sock, fn) ->
		while true do
			let this_connection, _ = Unix.accept listening_sock in
			let (_: Thread.t) = Thread.create
				(fun () ->
					finally
						(fun () -> fn this_connection)
						(fun () -> Unix.close this_connection)
				) () in
			()
		done
	| Queue(queue_name, fn) ->
		let process x = Jsonrpc.string_of_response (fn (Jsonrpc.call_of_string x)) in
		let _ = Protocol_unix.Server.listen ~process ~switch:!Xcp_client.switch_path ~queue:queue_name () in
		let rec forever () =
			Thread.delay 3600.;
			forever () in
		forever ()

let pidfile_write filename =
	let fd = Unix.openfile filename
		[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ]
		0o640 in
	finally
	(fun () ->
		let pid = Unix.getpid () in
		let buf = string_of_int pid ^ "\n" in
		let len = String.length buf in
		if Unix.write fd buf 0 len <> len
		then failwith "pidfile_write failed")
	(fun () -> Unix.close fd)


(* Cf Stevens et al, Advanced Programming in the UNIX Environment,
	 Section 13.3 *)
let daemonize ?start_fn () =
	if not (have_daemonized ())
	then
		ign_int (Unix.umask 0);
		match Unix.fork () with
	| 0 ->
		if Unix.setsid () == -1 then failwith "Unix.setsid failed";
		Sys.set_signal Sys.sighup Sys.Signal_ignore;
		(match Unix.fork () with
		| 0 ->
			Opt.iter (fun fn -> fn ()) start_fn;
			Unix.chdir "/";
			mkdir_rec (Filename.dirname !pidfile) 0o755;
			pidfile_write !pidfile;
			let nullfd = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
			Unix.dup2 nullfd Unix.stdin;
			Unix.dup2 nullfd Unix.stdout;
			Unix.dup2 nullfd Unix.stderr;
			Unix.close nullfd
		| _ -> exit 0)
	| _ -> exit 0

let maybe_daemonize ?start_fn () =
	if !daemon then
		daemonize ?start_fn ()
	else
		Opt.iter (fun fn -> fn ()) start_fn
