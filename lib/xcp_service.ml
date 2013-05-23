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

module StringSet = Set.Make(String)

module type BRAND = sig val name: string end
module Debug = struct
	type level = Debug | Warn | Info | Error
	type backend = Backend_syslog of string | Backend_stderr

	let stderr_ key level x =
		output_string stderr (Printf.sprintf "[%s|%s] %s" key (match level with
			| Debug -> "debug"
			| Warn -> "warn"
			| Info -> "info"
			| Error -> "error") x);
    output_string stderr "\n";
    flush stderr

	let syslog ?(facility=`LOG_LOCAL5) name =
		let t = Syslog.openlog ~facility name in
		fun key level x ->
			Syslog.syslog t (match level with
				| Debug -> `LOG_DEBUG
				| Warn -> `LOG_WARNING
				| Info -> `LOG_INFO
				| Error -> `LOG_ERR
			) (Printf.sprintf "[%s] %s" key x)

	let current_backend = ref (if have_daemonized ()
		then Backend_syslog default_service_name
		else Backend_stderr)

	let get_backend () = !current_backend
	let set_backend b  = current_backend := b

	let get_backend_fun () = match !current_backend with
		| Backend_stderr -> stderr_
		| Backend_syslog n -> syslog n

	let disabled_modules = ref StringSet.empty
	let disable m =
		disabled_modules := StringSet.add m !disabled_modules
	let enable m =
		disabled_modules := StringSet.remove m !disabled_modules

	let write key level x =
		if not (StringSet.mem key !disabled_modules)
		then get_backend_fun () key level x

	module Make = functor(Brand: BRAND) -> struct
		let debug fmt = Printf.ksprintf (write Brand.name Debug) fmt
		let error fmt = Printf.ksprintf (write Brand.name Error) fmt
		let info fmt = Printf.ksprintf (write Brand.name Info) fmt
		let warn fmt = Printf.ksprintf (write Brand.name Warn) fmt
	end
end

let finally f g =
	try
 		let result = f () in
		g ();
		result
	with e ->
		g ();
		raise e

type opt = string * Arg.spec * (unit -> string) * string

(* Server configuration. We have built-in (hopefully) sensible defaults,
   together with command-line arguments and a configuration file. They
   are applied in order: (latest takes precedence)
      defaults < arguments < config file
*)
let sockets_group = ref "xapi"
let default_service_name = Filename.basename Sys.argv.(0)
let config_file = ref (Printf.sprintf "/etc/%s.conf" default_service_name)
let pidfile = ref (Printf.sprintf "/var/run/%s.pid" default_service_name)
let log_destination = ref "syslog:daemon"
let daemon = ref false
let have_daemonized () = Unix.getppid () = 1

let common_prefix = "org/xen/xcp/"

module D = Debug.Make(struct let name = default_service_name end)
open D

module Config_file = struct
	open Arg

	let apply v = function
	| Unit f -> f ()
	| Bool f -> f (bool_of_string v)
	| String f -> f v
	| Set_string s -> s := v
	| Int f -> f (int_of_string v)
	| Set_int i -> i := (int_of_string v)
	| Float f -> f (float_of_string v)
	| Set_float f -> f := (float_of_string v)
	| _ -> failwith "Unsupported type in config file"

	let parse_line data spec =
		let spec = List.map (fun (a, b, _, _) -> a, b) spec in
		(* Strip comments *)
		match Re_str.(split_delim (regexp (quote "#"))) data with
		| [] -> ()
		| x :: _ ->
			begin match Re_str.bounded_split_delim (Re_str.regexp "[ \t]*=[ \t]*") x 2 with
			| key :: v :: [] ->
				(* For values we will accept "v" and 'v' *)
				let v =
					if String.length v < 2
					then v
					else
						let first = v.[0] and last = v.[String.length v - 1] in
						if first = last && (first = '"' || first = '\'')
						then String.sub v 1 (String.length v - 2)
						else v in
				if List.mem_assoc key spec then apply v (List.assoc key spec)
			| _ -> ()
			end

	let parse filename spec =
		(* Remove the unnecessary doc parameter *)
		let ic = open_in filename in
		finally
		(fun () ->
			try
				while true do
					let line = input_line ic in
					parse_line line spec
				done
			with End_of_file -> ()
		) (fun () -> close_in ic)

	let dump spec =
		List.iter (fun (name, _, printer, description) ->
			debug "%s = %s (%s)" name (printer ()) description
		) spec

end

let common_options = [
	"use-switch", Arg.Bool (fun b -> Xcp_client.use_switch := b), (fun () -> string_of_bool !Xcp_client.use_switch), "true if the message switch is to be enabled";
	"switch-port", Arg.Set_int Xcp_client.switch_port, (fun () -> string_of_int !Xcp_client.switch_port), "port on localhost where the message switch is listening";
	"sockets-group", Arg.Set_string sockets_group, (fun () -> !sockets_group), "Group to allow access to the control socket";
	"pidfile", Arg.Set_string pidfile, (fun () -> !pidfile), "Filename to write process PID";
	"log", Arg.Set_string log_destination, (fun () -> !log_destination), "Where to write log messages";
	"daemon", Arg.Bool (fun x -> daemon := x), (fun () -> string_of_bool !daemon), "True if we are to daemonise";
	"disable-logging-for", Arg.String
		(fun x ->
			try
				let modules = Re_str.split (Re_str.regexp "[ ]+") x in
				List.iter Debug.disable modules
			with e ->
				error "Processing disabled-logging-for = %s: %s" x (Printexc.to_string e)
		), (fun () -> String.concat " " (StringSet.elements !Debug.disabled_modules)), "A space-separated list of debug modules to suppress logging from";
	"config", Arg.Set_string config_file, (fun () -> !config_file), "Location of configuration file";
]

let arg_spec = List.map (fun (a, b, _, c) -> "-" ^ a, b, c)

type res = {
	name: string;
	description: string;
	essential: bool;
	path: string ref;
	perms: Unix.access_permission list
}

let canonicalise x = Filename.(if is_relative x then concat (Unix.getcwd ()) x else x)

let to_opt = List.map (fun f -> f.name, Arg.String (fun x -> f.path := canonicalise x), (fun () -> !(f.path)), f.description)

let read_config_file x =
	if Sys.file_exists !config_file then begin
		(* Will raise exception if config is mis-formatted. It's up to the
		   caller to inspect and handle the failure.
		*)
		Config_file.parse !config_file x;
	end

let configure ?(options=[]) ?(resources=[]) () =
	let config_spec = common_options @ options @ (to_opt resources) in
	let arg_spec = arg_spec config_spec in
	Arg.parse (Arg.align arg_spec)
		(fun _ -> failwith "Invalid argument")
		(Printf.sprintf "Usage: %s [-config filename]" Sys.argv.(0));
	read_config_file config_spec;
	Config_file.dump config_spec;
	(* Check the required binaries are all available *)
	List.iter
		(fun f ->
			try
				if f.essential
				then Unix.access !(f.path) f.perms
			with _ ->
				error "Cannot access %s: please set %s in %s" !(f.path) f.description !config_file;
				error "For example:";
				error "    # %s" f.description;
				error "    %s=/path/to/%s" f.name f.name;
				exit 1
		) resources;

	Sys.set_signal Sys.sigpipe Sys.Signal_ignore

type 'a handler =
	(string -> Rpc.call) ->
	(Rpc.response -> string) ->
	('a -> Rpc.call -> Rpc.response) ->
	Unix.file_descr ->
	'a->
	unit

(* Apply a binary message framing protocol where the first 16 bytes are an integer length
   stored as an ASCII string *)
let binary_handler call_of_string string_of_response process s context =
	let ic = Unix.in_channel_of_descr s in
	let oc = Unix.out_channel_of_descr s in
	(* Read a 16 byte length encoded as a string *)
	let len_buf = String.make 16 '\000' in
	really_input ic len_buf 0 (String.length len_buf);
	let len = int_of_string len_buf in
	let msg_buf = String.make len '\000' in
	really_input ic msg_buf 0 (String.length msg_buf);
	let (request: Rpc.call) = call_of_string msg_buf in
	let (result: Rpc.response) = process context request in
	let msg_buf = string_of_response result in
	let len_buf = Printf.sprintf "%016d" (String.length msg_buf) in
	output_string oc len_buf;
	output_string oc msg_buf;
	flush oc

let http_handler call_of_string string_of_response process s context =
	let ic = Unix.in_channel_of_descr s in
	let oc = Unix.out_channel_of_descr s in
	let module Request = Cohttp.Request.Make(Cohttp_posix_io.Buffered_IO) in
	let module Response = Cohttp.Response.Make(Cohttp_posix_io.Buffered_IO) in
	match Request.read ic with
	| None ->
		debug "Failed to read HTTP request"
	| Some req ->
		begin match Request.meth req, Uri.path (Request.uri req) with
		| `POST, _ ->
			begin match Request.header req "content-length" with
			| None ->
				debug "Failed to read content-length"
			| Some content_length ->
				let content_length = int_of_string content_length in
				let request_txt = String.make content_length '\000' in
				really_input ic request_txt 0 content_length;
				let rpc_call = call_of_string request_txt in
				debug "%s" (Rpc.string_of_call rpc_call);
				let rpc_response = process context rpc_call in
				debug "   %s" (Rpc.string_of_response rpc_response);
				let response_txt = string_of_response rpc_response in
				let content_length = String.length response_txt in
				let headers = Cohttp.Header.of_list [
					"user-agent", default_service_name;
					"content-length", string_of_int content_length;
				] in
				let response = Response.make ~version:`HTTP_1_1 ~status:`OK ~headers ~encoding:(Cohttp.Transfer.Fixed content_length) () in
				Response.write (fun t oc -> Response.write_body t oc response_txt) response oc
			end
		| _, _ ->
			let content_length = 0 in
			let headers = Cohttp.Header.of_list [
				"user-agent", default_service_name;
				"content-length", string_of_int content_length;
			] in
			let response = Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers ~encoding:(Cohttp.Transfer.Fixed content_length) () in
			Response.write (fun t oc -> ()) response oc
		end

let ign_thread (t:Thread.t) = ignore t
let ign_int (t:int)         = ignore t
let ign_string (t:string)   = ignore t

let accept_forever sock f =
	ign_thread (Thread.create (fun () ->
		while true do
			let this_connection, _ = Unix.accept sock in
			ign_thread (Thread.create (fun c -> finally (fun () -> f c)  (fun () -> Unix.close c)) this_connection)
		done
	) ())

let mkdir_rec dir perm =
	let rec p_mkdir dir =
		let p_name = Filename.dirname dir in
		if p_name <> "/" && p_name <> "." 
		then p_mkdir p_name;
		(try Unix.mkdir dir perm  with Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in
	p_mkdir dir

(* Start accepting connections on sockets before we daemonize *)
let listen path =
	(* Check the sockets-group exists *)
  let (_:Unix.group_entry) = try Unix.getgrnam !sockets_group with _ ->
		(error "Group %s doesn't exist." !sockets_group;
		 error "Either create the group, or select a different group by modifying the config file:";
		 error "# Group which can access the control socket";
		 error "sockets-group=<some group name>";
		 exit 1) in ();
	try
		(try Unix.unlink path with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
		mkdir_rec (Filename.dirname path) 0o0755;
		let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
		Unix.bind sock (Unix.ADDR_UNIX path);
		Unix.listen sock 5;
		info "Listening on %s" path;
		sock
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
let daemonize () =
	if not (have_daemonized ())
	then
		ign_int (Unix.umask 0);
		match Unix.fork () with
	| 0 ->
		if Unix.setsid () == -1 then failwith "Unix.setsid failed";
		Sys.set_signal Sys.sighup Sys.Signal_ignore;
		(match Unix.fork () with
		| 0 ->
			Unix.chdir "/";
			mkdir_rec (Filename.dirname !pidfile) 0o755;
			pidfile_write !pidfile;
			Unix.close Unix.stdin;
			Unix.close Unix.stdout;
			Unix.close Unix.stderr;
			let nullfd = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
			assert (nullfd = Unix.stdin);
			let (_:Unix.file_descr) = Unix.dup nullfd in ();
			let (_:Unix.file_descr) = Unix.dup nullfd in ();
			Debug.set_backend (Debug.Backend_syslog default_service_name)
	  | _ -> exit 0)
	| _ -> exit 0

let maybe_daemonize () = if !daemon then daemonize ()

let wait_forever () =
	while true do
		try
			Thread.delay 60.
		with e ->
			debug "Thread.delay caught: %s" (Printexc.to_string e)
	done

