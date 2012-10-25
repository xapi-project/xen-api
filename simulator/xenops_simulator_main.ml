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
open Xenops_utils

let name = "xenopsd"

let major_version = 0
let minor_version = 9

(* Server configuration. We have built-in (hopefully) sensible defaults,
   together with command-line arguments and a configuration file. They
   are applied in order: (latest takes precedence)
      defaults < arguments < config file
*)
let config_file = ref (Printf.sprintf "/etc/%s.conf" name)
let pidfile = ref (Printf.sprintf "/var/run/%s.pid" name)
let log_destination = ref "syslog:daemon"
let persist = ref true
let daemon = ref false
let worker_pool_size = ref 4

open Xenopsd
let _ = 
	debug "xenopsd version %d.%d starting" major_version minor_version;

	Arg.parse (Arg.align [
	       "-daemon", Arg.Set daemon, "Create a daemon";
	       "-pidfile", Arg.Set_string pidfile, Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
		   "-config", Arg.Set_string config_file, Printf.sprintf "Read configuration from the specified config file (default \"%s\")" !config_file;
	     ])
    (fun _ -> failwith "Invalid argument")
    (Printf.sprintf "Usage: %s [-daemon] [-pidfile filename]" name);

	Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

	(* Accept connections before we have daemonized *)
	let sockets = prepare_sockets path in

(*
	if !daemon
	then Unixext.daemonize ();
*)

  Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
  (* Unixext.pidfile_write !pidfile; *) (* XXX *)

  Xenops_utils.set_fs_backend
	  (Some (if !persist
	  then (module Xenops_utils.FileFS: Xenops_utils.FS)
	  else (module Xenops_utils.MemFS: Xenops_utils.FS)));

  Xenops_server.register_objects();
  Xenops_server.set_backend (Some (module Xenops_server_simulator: Xenops_server_plugin.S));

  Debug.with_thread_associated "main" start sockets;
  Xenops_task.Updates.Scheduler.start ();
  Xenops_server.WorkerPool.start !worker_pool_size;
  while true do
	  try
		  Thread.delay 60.
	  with e ->
		  debug "Thread.delay caught: %s" (Printexc.to_string e)
  done


