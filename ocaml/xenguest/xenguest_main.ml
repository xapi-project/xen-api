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
(* Very simple commandline wrapper around useful but not threadsafe xenguest
   functions.  Serves as an example of the ocaml bindings. *)

let mode = ref None

open Printf

let finally fct clean_f =
	let result = try
		fct ();
	with
		exn ->
		  clean_f (); raise exn in
	clean_f ();
	result

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

let file_descr_of_int (x: int) : Unix.file_descr = Obj.magic x
let int_of_file_descr (x: Unix.file_descr) : int = Obj.magic x

let close_all_fds_except (fds: Unix.file_descr list) =
  let all_open = Sys.readdir (sprintf "/proc/%d/fd" (Unix.getpid ())) in
  let all_open = List.map int_of_string (Array.to_list all_open) in
  let all_open = List.map file_descr_of_int all_open in
  let to_close = set_difference all_open fds in
  List.iter (fun fd -> try Unix.close fd with _ -> ()) to_close

(* Code to log internal debug messages ***************************************)
let debug_fd = ref None
let openlog filename =
  debug_fd := Some (Unix.openfile filename [ Unix.O_CREAT; Unix.O_WRONLY ] 0o644)
let log_writer prefix (x: string) = match !debug_fd with
  | Some fd ->
      let x = prefix ^ x ^ "\n" in
      let n = Unix.write fd x 0 (String.length x) in
      if n <> String.length x then begin
	  (* Just in case this works: *)
	  Printf.fprintf stderr "Failed to write to debug log file; quitting";
	  exit 1
      end
  | None -> ()
let debug (fmt: ('a , unit, string, unit) format4) =
  Printf.kprintf (log_writer "xenguest helper: debug: ") fmt
let error (fmt: ('a , unit, string, unit) format4) =
  Printf.kprintf (log_writer "xenguest helper: error: ") fmt
let closelog () = match !debug_fd with
  | Some fd -> Unix.close fd
  | None -> ()

(* Types of messages we can transmit *****************************************)
type message =
    | Stdout of string (* captured stdout from libxenguest *)
    | Stderr of string (* captured stderr from libxenguest *)
    | Error of string  (* an actual error that we detected *)
    | Suspend          (* request the caller suspends the domain *)
    | Info of string   (* some info that we want to send back *)
    | Result of string (* the result of the operation *)

let string_of_message = function
  | Stdout x -> "stdout:" ^ (String.escaped x)
  | Stderr x -> "stderr:" ^ (String.escaped x)
  | Error x  -> "error:" ^ (String.escaped x)
  | Suspend  -> "suspend:"
  | Info x   -> "info:" ^ (String.escaped x)
  | Result x -> "result:" ^ (String.escaped x)

(* Parameter parsing code ****************************************************)

(** A mapping from param name -> string options *)
let params = Hashtbl.create 10
(** A mapping from param name -> docstring *)
let doc = Hashtbl.create 10

let add_param (param: string) (docstring: string) =
	Hashtbl.add params param None;
	Hashtbl.add doc param docstring
let get_args () =
	let set param v = Hashtbl.replace params param (Some v) in
	Hashtbl.fold (fun param docstring acc ->
		("-" ^ param, Arg.String (set param), docstring) :: acc) doc []
let require xs = List.iter (fun param ->
	if not(Hashtbl.mem params param)
	then begin
	    let msg = sprintf "Internal error; unexpected parameter %s" param in
	    error "%s" msg;
	    failwith msg
	end else match Hashtbl.find params param with
	| None ->
	    let msg = sprintf "This option requires parameters [ %s ]. You missed %s"
	      (String.concat ", " xs) param in
	    error "%s" msg;
	    failwith msg
	| Some v -> ()) xs

let get_param param = match Hashtbl.find params param with
	| None ->
	    let msg = sprintf "Internal error; unexpected parameter %s" param in
	    error "%s" msg;
	    failwith msg
	| Some v -> v
let has_param param = Hashtbl.find params param <> None

(* Code to talk to the controlling process ***********************************)

let controlinfd = ref (-1)
let controloutfd = ref (-1)

let control_write (x: message) =
  let x = string_of_message x in
  debug "control_write: %s" x;
  let outfd = file_descr_of_int !controloutfd in
  let oc = Unix.out_channel_of_descr outfd in
  output_string oc (x ^ "\n");
  flush oc

let control_read () : string =
  let infd = file_descr_of_int !controlinfd in
  let ic = Unix.in_channel_of_descr infd in
  let result = input_line ic in
  debug "control_read: %s" result;
  result

let fork_capture_stdout_stderr callback f x =
	let stdout_r, stdout_w = Unix.pipe ()
	and stderr_r, stderr_w = Unix.pipe ()
	and output_r, output_w = Unix.pipe () in

	let pid = Unix.fork () in
	if pid = 0 then begin
		try
			Unix.dup2 stdout_w Unix.stdout;
			Unix.dup2 stderr_w Unix.stderr;
			List.iter Unix.close [ stdout_r; stderr_r; output_r ];
			(* On success, prepend a well-known character. *)
			let result = try "!" ^ f x
				     with e -> Printf.sprintf "Subprocess failure: %s" (Printexc.to_string e) in
			let len=String.length result in
			if (Unix.write output_w result 0 len) <> len 
			then (failwith "Write returned short in fork_capture_stdout_stderr");
			exit 0
		with _ -> exit 0
	end;

	Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun i -> debug "Signal handler killing PID=%d" pid; Unix.kill pid Sys.sigterm));
	List.iter Unix.close [ stdout_w; stderr_w; output_w ];

	let finished = ref false in
	let buf = String.make 1024 ' ' in

	(* We want to make sure we drain stdout and stderr before quitting *)
	let active_fds = ref [ stdout_r; stderr_r; output_r ] in

	let result = ref "" in
	while not(!finished) do
		let r, _, _ =
			try Unix.select !active_fds [] [] (-1.)
			with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
	
		if List.mem stdout_r r then begin
			let n = Unix.read stdout_r buf 0 (String.length buf) in
			if n = 0 then begin
			    debug "Zero-length read on stdout; closing";
			    Unix.close stdout_r;
			    active_fds := List.filter (fun x -> x <> stdout_r) !active_fds
			end else callback (Stdout(String.sub buf 0 n))
		end;
		if List.mem stderr_r r then begin
			let n = Unix.read stderr_r buf 0 (String.length buf) in
			if n = 0 then begin
			    debug "Zero-length read on stderr; closing";
			    Unix.close stderr_r;
			    active_fds := List.filter (fun x -> x <> stderr_r) !active_fds
			end else callback (Stderr(String.sub buf 0 n))
		end;
		if List.mem output_r r then begin
			let n = Unix.read output_r buf 0 (String.length buf) in
			if n = 0 then begin
			    debug "Zero-length read on output; closing";
			    Unix.close output_r;
			    active_fds := List.filter (fun x -> x <> output_r) !active_fds
			end else result := String.sub buf 0 n
		end;
		finished := !active_fds = []
	done;
	begin match snd (Unix.waitpid [] pid) with
	| Unix.WEXITED 0    -> ()
	| Unix.WEXITED rc   -> failwith (sprintf "child failure, return code %d" rc)
	| Unix.WSIGNALED si -> failwith (sprintf "child killed by signal %s" (Unixext.string_of_signal si))
	| _                 -> failwith (sprintf "child stopped")
	end;
	(* Success/failure is encoded in the first character *)
	if String.length !result = 0 then
	  failwith "result empty string";
	if !result.[0] = '!'
	then result := String.sub !result 1 (String.length !result - 1)
	else failwith !result;
	!result


(* Helper functions ********************************************************)

(** Global callback function to be called from C bindings *)
let suspend_callback id : bool =
	if id = int_of_string (get_param "domid") then begin
		control_write Suspend;
		let line = control_read () in
		print_endline line;
		true
	end else false

let _ = Callback.register "suspend_callback" suspend_callback

(** real operations *)
let with_xenguest f =
	let xc = Xenguest.init () in
	finally (fun () -> f xc) (fun () -> Xenguest.close xc)

let linux_build_real domid mem_max_mib mem_start_mib image ramdisk cmdline features flags store_port console_port =
	with_xenguest (fun xc ->
		let store_mfn, console_mfn, proto =
			Xenguest.linux_build xc domid mem_max_mib mem_start_mib image
			                     ramdisk cmdline features flags store_port console_port in
		String.concat " " [ Nativeint.to_string store_mfn;
		                    Nativeint.to_string console_mfn; proto ]
	)

let hvm_build_real domid mem_max_mib mem_start_mib image store_port console_port=
	with_xenguest (fun xc ->
		let store_mfn, console_mfn =
			Xenguest.hvm_build xc domid mem_max_mib mem_start_mib image
			                   store_port console_port in
		String.concat " " [Nativeint.to_string store_mfn;
		                   Nativeint.to_string console_mfn]
	)

let domain_save_real fd domid x y flags hvm =
	with_xenguest (fun xc ->
		Xenguest.domain_save xc fd domid x y flags hvm;
		""
	)

let domain_restore_real fd domid store_port store_domid console_port console_domid hvm no_incr_generationid =
	with_xenguest (fun xc ->
		let store_mfn, console_mfn =
		Xenguest.domain_restore xc fd domid store_port store_domid
					console_port console_domid hvm no_incr_generationid in
		String.concat " "  [ Nativeint.to_string store_mfn;
				     Nativeint.to_string console_mfn ]
	)

(** fake operations *)
let linux_build_fake domid mem_max_mib mem_start_mib image ramdisk cmdline features flags store_port console_port = "10 10 x86-32"
let hvm_build_fake domid mem_max_mib mem_start_mib image store_port console_port = "2901 2901"
let domain_save_fake fd domid x y flags hvm = Unix.sleep 1; ignore (suspend_callback domid); ""
let domain_restore_fake fd domid store_port store_domid console_port console_domid hvm no_incr_generationid = "10 10"

(** operation vector *)
type ops = {
	linux_build: int -> int -> int -> string -> string option -> string -> string -> int -> int -> int -> string;
	hvm_build: int -> int -> int -> string -> int -> int -> string;
	domain_save: Unix.file_descr -> int -> int -> int -> Xenguest.suspend_flags list -> bool -> string;
	domain_restore: Unix.file_descr -> int -> int -> int -> int -> int -> bool -> bool -> string;
}

(* main *)
let _ =
	(* Union of all the options required by all modes: *)
	add_param "fd" "the file-descriptor on which to send the data";
	add_param "image" "kernel image to boot from";
	add_param "cmdline" "kernel commandline to use";
	add_param "ramdisk" "kernel ramdisk path to use";
	add_param "domid" "domain ID on which to operate";
	add_param "live" "perform a live suspend";
	add_param "debug" "suspend in debug mode";
	add_param "store_port" "";
	add_param "store_domid" "";
	add_param "console_port" "";
	add_param "console_domid" "";
	add_param "no_incr_generationid" "";
	add_param "features" "";
	add_param "flags" "";
	add_param "mem_max_mib" "maximum memory allocation / MiB";
	add_param "mem_start_mib" "initial memory allocation / MiB";
	add_param "fork" "true to fork a background thread to capture stdout and stderr";

	let fake = ref false in

	Arg.parse ([
	  "-mode", Arg.Symbol ([ "save"; "hvm_save"; "restore"; "hvm_restore"; "resume_slow"; "linux_build"; "hvm_build"; "test" ],
			       fun x -> mode := Some x),
	  "set the mode of operation";
	] @ (get_args ()) @ [
	  "-controlinfd", Arg.Set_int controlinfd,
	  "set the fd on which to receive the control commands (defaults to stdin)";
	  "-controloutfd", Arg.Set_int controloutfd,
	  "set the fd on which to send responses (defaults to stdout)";
	  "-debuglog", Arg.String openlog,
	  "Append debug logging direct to a file";
	  "-fake", Arg.Set fake,
	  "Use Fake calls";
	]) (fun x -> print_endline ("Ignoring argument: " ^ x))
	  "Helper program to interface with libxenguest";

	if !controlinfd = -1
	then controlinfd := int_of_file_descr Unix.stdin
	else Unix.set_close_on_exec (file_descr_of_int !controlinfd);

	if !controloutfd = -1
	then controloutfd := int_of_file_descr Unix.stdout
	else Unix.set_close_on_exec (file_descr_of_int !controloutfd);

	let fds_to_keep =
	  List.map file_descr_of_int [  !controlinfd; !controloutfd ] @
	    [ Unix.stdout; Unix.stderr ] @
	    (if has_param "fd" then [ file_descr_of_int (int_of_string (get_param "fd")) ] else []) @
	    (match !debug_fd with Some x -> [ x ] | None -> []) in

	(* Prevent accidentally inheriting someone elses fd *)
	close_all_fds_except fds_to_keep;

	debug "Arguments parsed successfully [ %s ]." (String.concat "; " (Array.to_list Sys.argv));

	let capture_stdout_stderr = has_param "fork" && (get_param "fork" = "true") in
	if capture_stdout_stderr
	then debug "Will fork to capture stdout and stderr from libxenguest"
	else debug "Will not fork; stdout and stderr will not be redirected";

	let with_logging f = if capture_stdout_stderr
	  then fork_capture_stdout_stderr control_write f ()
	  else f () in

	let real_ops = {
		linux_build = linux_build_real;
		hvm_build = hvm_build_real;
		domain_save = domain_save_real;
		domain_restore = domain_restore_real;
	} in
	let fake_ops = {
		linux_build = linux_build_fake;
		hvm_build = hvm_build_fake;
		domain_save = domain_save_fake;
		domain_restore = domain_restore_fake;
	} in

	let ops = if !fake then fake_ops else real_ops in

	begin
	  try
	    let result = match !mode with
	      | None ->
		  error "Must have a -mode commandline option";
		  failwith "Must have a -mode commandline option";
	      | Some "hvm_save"
	      | Some "save" ->
		  debug "save mode selected";
		  require [ "domid"; "fd" ];
		  let hvm = if !mode = (Some "hvm_save") then true else false in
		  let fd = file_descr_of_int (int_of_string (get_param "fd"))
		  and domid = int_of_string (get_param "domid")
		  and flags = List.concat [ if has_param "live" then [ Xenguest.Live ] else [];
					    if has_param "debug" then [ Xenguest.Debug ] else [] ] in

		  with_logging (fun () -> ops.domain_save fd domid 0 0 flags hvm)
	      | Some "hvm_restore"
	      | Some "restore" ->
		  debug "restore mode selected";
		  let hvm = if !mode = (Some "hvm_restore") then true else false in
		  require [ "domid"; "fd"; "store_port"; "store_domid"; "console_port"; "console_domid" ];
		  let fd = file_descr_of_int (int_of_string (get_param "fd"))
		  and domid = int_of_string (get_param "domid")
		  and store_port = int_of_string (get_param "store_port")
		  and store_domid = int_of_string (get_param "store_domid")
		  and console_port = int_of_string (get_param "console_port")
		  and console_domid = int_of_string (get_param "console_domid")
		  and no_incr_generationid = bool_of_string (get_param "no_incr_generationid") in

		  with_logging (fun () -> ops.domain_restore fd domid store_port store_domid console_port console_domid hvm no_incr_generationid)
	      | Some "linux_build" ->
		  debug "linux_build mode selected";
		  require [ "domid"; "mem_max_mib"; "mem_start_mib"; "image"; "ramdisk"; "cmdline"; "features"; "flags";
			    "store_port"; "console_port" ];
		  let domid = int_of_string (get_param "domid")
		  and mem_max_mib = int_of_string (get_param "mem_max_mib")
		  and mem_start_mib = int_of_string (get_param "mem_start_mib")
		  and image = get_param "image"
		  and ramdisk = get_param "ramdisk"
		  and cmdline = get_param "cmdline"
		  and features = get_param "features"
		  and flags = int_of_string (get_param "flags")
		  and store_port = int_of_string (get_param "store_port")
		  and console_port = int_of_string (get_param "console_port") in

		  with_logging (fun () -> ops.linux_build domid mem_max_mib mem_start_mib image
		                          (if ramdisk = "" then None else Some ramdisk)
		                          cmdline features flags store_port console_port)
	      | Some "hvm_build" ->
		  debug "hvm_build mode selected";
		      require [ "domid"; "mem_max_mib"; "mem_start_mib"; "image";
		                "store_port"; "console_port" ];
		  let domid = int_of_string (get_param "domid")
		  and mem_max_mib = int_of_string (get_param "mem_max_mib")
		  and mem_start_mib = int_of_string (get_param "mem_start_mib")
		  and image = get_param "image"
		  and store_port = int_of_string (get_param "store_port")
	    and console_port = int_of_string (get_param "console_port") in

		  with_logging (fun () -> ops.hvm_build domid mem_max_mib mem_start_mib image
			  store_port console_port)
	      | Some "test" ->
		  debug "test mode selected";
		  with_logging (fun () -> ignore(Unix.system "/tmp/test"); "result")

	      | Some "resume_slow" ->
		  debug "resume slow selected";
		  require [ "domid" ];
		  let domid = int_of_string (get_param "domid") in
		  with_logging (fun () -> with_xenguest (fun xc ->
		    Xenguest.domain_resume_slow xc domid;
		    ""))
	      | Some x ->
		  let msg = sprintf "Unrecognised mode: %s" x in
		  error "%s" msg;
		  failwith msg
 	    in
	    control_write (Result result);
	with
	| Failure x as e ->
		let prefix = "Subprocess failure: Failure(\"" in
		if String.sub x 0 (String.length prefix) = prefix then
			begin
				let rest = String.sub x (String.length prefix)
					(String.length x - (String.length prefix)) in
				try
					let lbr = String.index rest '['
					and rbr = String.index rest ']' in
					let code = String.sub rest 0 (lbr - 2) in
					let errno = String.sub rest (lbr + 1) (rbr - lbr - 1) in
					let rest = String.sub rest (rbr + 1)
						(String.length rest - rbr - 2) in
					control_write (Error (sprintf "%s %s %s" code errno rest))
				with _ ->
					control_write (Error rest)
			end
		else
			control_write (Error (sprintf "caught exception: %s"
				(Printexc.to_string e)))
	| e ->
		control_write (Error (sprintf "caught exception: %s"
			(Printexc.to_string e)))
	end;
	closelog ()

