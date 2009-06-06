(*
 * Copyright (c) 2006 XenSource Inc.
 * Author Vincent Hanquez <vincent@xensource.com>
 *
 * All rights reserved.
 *)

type config =
{
	domain_init: bool;
	activate_access_log: bool;
	daemonize: bool;
	reraise_top_level: bool;
	config_file: string option;
	pidfile: string option; (* old xenstored compatibility *)
	tracefile: string option; (* old xenstored compatibility *)
	restart: bool;
}

let do_argv =
	let pidfile = ref "" and tracefile = ref "" (* old xenstored compatibility *)
	and domain_init = ref true
	and activate_access_log = ref true
	and daemonize = ref true
	and reraise_top_level = ref false
	and config_file = ref ""
	and restart = ref false in

	let speclist =
		[ ("--no-domain-init", Arg.Unit (fun () -> domain_init := false),
		   "to state that xenstored should not initialise dom0");
		  ("--config-file", Arg.Set_string config_file,
		   "set an alternative location for the configuration file");
		  ("--no-fork", Arg.Unit (fun () -> daemonize := false),
		   "to request that the daemon does not fork");
		  ("--reraise-top-level", Arg.Unit (fun () -> reraise_top_level := true),
		   "reraise exceptions caught at the top level");
		  ("--no-access-log", Arg.Unit (fun () -> activate_access_log := false),
		  "do not create a xenstore-access.log file");
		  ("--pid-file", Arg.Set_string pidfile, ""); (* for compatibility *)
		  ("-T", Arg.Set_string tracefile, ""); (* for compatibility *)
		  ("--restart", Arg.Set restart, "Read database on starting");
		   ] in
	let usage_msg = "usage : xenstored [--config-file <filename>] [--no-domain-init] [--help] [--no-fork] [--reraise-top-level] [--restart]" in
	Arg.parse speclist (fun s -> ()) usage_msg;
	{
		domain_init = !domain_init;
		activate_access_log = !activate_access_log;
		daemonize = !daemonize;
		reraise_top_level = !reraise_top_level;
		config_file = if !config_file <> "" then Some !config_file else None;
		pidfile = if !pidfile <> "" then Some !pidfile else None;
		tracefile = if !tracefile <> "" then Some !tracefile else None;
		restart = !restart
	}
