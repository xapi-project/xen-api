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

module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = __MODULE__ end)

let () =
  Debug.set_facility Syslog.Local5 ;
  Sys.enable_runtime_warnings true ;
  Xapi.init_args () ;
  (* Disable logging for the module requested in the config *)
  List.iter
    (fun m ->
      D.debug "Disabling logging for: %s" m ;
      Debug.disable m
    )
    !Xapi_globs.disable_logging_for ;
  Unixext.pidfile_write "/var/run/xapi.pid" ;
  ignore
    Xapi_stdext_unix.Unixext.Daemon.(
      systemd_notify (State.MainPID (Unix.getpid ()))
    ) ;
  (* chdir to /var/lib/xcp/debug so that's where xapi coredumps go
     (in the unlikely event that there are any ;) *)
  Unixext.mkdir_rec (Filename.concat "/var/lib/xcp" "debug") 0o700 ;
  Unix.chdir (Filename.concat "/var/lib/xcp" "debug") ;
  Xapi.(watchdog server_init)
