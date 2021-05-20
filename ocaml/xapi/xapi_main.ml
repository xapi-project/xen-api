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
 * @group Main Loop and Start-up
*)

open Xapi
module Unixext = Xapi_stdext_unix.Unixext

let _ =
  Debug.set_facility Syslog.Local5 ;
  Sys.enable_runtime_warnings true ;
  init_args () ;
  (* need to read args to find out whether to daemonize or not *)
  Xcp_service.maybe_daemonize () ;
  (* Disable logging for the module requested in the config *)
  List.iter
    (fun m ->
      D.debug "Disabling logging for: %s" m ;
      Debug.disable m
      )
    !Xapi_globs.disable_logging_for ;
  Unixext.pidfile_write "/var/run/xapi.pid" ;
  (* chdir to /var/lib/xcp/debug so that's where xapi coredumps go
     (in the unlikely event that there are any ;) *)
  Unixext.mkdir_rec (Filename.concat "/var/lib/xcp" "debug") 0o700 ;
  Unix.chdir (Filename.concat "/var/lib/xcp" "debug") ;
  watchdog server_init
