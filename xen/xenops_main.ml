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

module D = Debug.Make(struct let name = "xenops_main" end)
open D

let name = "xenopsd"

let major_version = 0
let minor_version = 9

open Xenopsd
let _ = 
	debug "xenopsd version %d.%d starting" major_version minor_version;

	Arg.parse (Arg.align arg_spec)
		(fun _ -> failwith "Invalid argument")
		(Printf.sprintf "Usage: %s [-config filename]" name);

	read_config_file ();
	dump_config_file ();

	if !daemon then begin
		Debug.output := Debug.syslog "xenopd_simulator" ();
		Unixext.daemonize();
	end;

	Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

	(* Accept connections before we have daemonized *)
	let sockets = prepare_sockets () in

(*
	if !daemon
	then Unixext.daemonize ();
*)

  Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
  (* Unixext.pidfile_write !pidfile; *) (* XXX *)

  Xenops_utils.set_fs_backend (Some (module Xenops_utils.FileFS: Xenops_utils.FS));

  Xenops_server.register_objects();
  Xenops_server.set_backend (Some (module Xenops_server_xen: Xenops_server_plugin.S));

  Debug.with_thread_associated "main" start sockets;
  Scheduler.start ();
  Xenops_server.WorkerPool.start !worker_pool_size;
  while true do
	  try
		  Thread.delay 60.
	  with e ->
		  debug "Thread.delay caught: %s" (Printexc.to_string e)
  done


