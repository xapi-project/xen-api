(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Xapi_stdext_unix

let signal_name signum =
  let signals =
    let t = Hashtbl.create 30 in
    let map =
      [ ("SIGABRT", Sys.sigabrt)
      ; ("SIGALRM", Sys.sigalrm)
      ; ("SIGFPE", Sys.sigfpe)
      ; ("SIGHUP", Sys.sighup)
      ; ("SIGILL", Sys.sigill)
      ; ("SIGINT", Sys.sigint)
      ; ("SIGKILL", Sys.sigkill)
      ; ("SIGPIPE", Sys.sigpipe)
      ; ("SIGQUIT", Sys.sigquit)
      ; ("SIGSEGV", Sys.sigsegv)
      ; ("SIGTERM", Sys.sigterm)
      ; ("SIGUSR1", Sys.sigusr1)
      ; ("SIGUSR2", Sys.sigusr2)
      ; ("SIGCHLD", Sys.sigchld)
      ; ("SIGCONT", Sys.sigcont)
      ; ("SIGSTOP", Sys.sigstop)
      ; ("SIGTSTP", Sys.sigtstp)
      ; ("SIGTTIN", Sys.sigttin)
      ; ("SIGTTOU", Sys.sigttou)
      ; ("SIGVTALRM", Sys.sigvtalrm)
      ; ("SIGPROF", Sys.sigprof)
      ; ("SIGBUS", Sys.sigbus)
      ; ("SIGPOLL", Sys.sigpoll)
      ; ("SIGSYS", Sys.sigsys)
      ; ("SIGTRAP", Sys.sigtrap)
      ; ("SIGURG", Sys.sigurg)
      ; ("SIGXCPU", Sys.sigxcpu)
      ; ("SIGXFSZ", Sys.sigxfsz) ]
    in
    List.iter (fun (str, key) -> Hashtbl.add t key str) map ;
    t in
    try Hashtbl.find signals signum with Not_found ->
      Printf.sprintf "unknown signal (%d)" signum

module Utils = Utils
module Reporter = struct
  include Reporter
  include Reporter_local
  include Reporter_interdomain
  
  type target =
    | Local of int
    | Interdomain of (int * int)

  let start (module D : Debug.DEBUG) ~uid ~neg_shift ~target ~protocol ~dss_f =
    match target with
    | Local page_count ->
      start_local (module D)
        ~reporter:None
        ~uid
        ~neg_shift
        ~page_count
        ~protocol
        ~dss_f
    | Interdomain (backend_domid, page_count) ->
      start_interdomain (module D)
        ~reporter:None
        ~uid
        ~backend_domid
        ~page_count
        ~protocol
        ~dss_f

  let start_async (module D : Debug.DEBUG) ~uid ~neg_shift ~target ~protocol ~dss_f =
    let reporter = make () in
    let (_ : Thread.t) =
      Thread.create (fun () ->
          match target with
          | Local page_count ->
            start_local (module D)
              ~reporter:(Some reporter)
              ~uid
              ~neg_shift
              ~page_count
              ~protocol
              ~dss_f
          | Interdomain (backend_domid, page_count) ->
            start_interdomain (module D)
              ~reporter:(Some reporter)
              ~uid
              ~backend_domid
              ~page_count
              ~protocol
              ~dss_f)
        ()
    in
    reporter
end

module Process = functor (N : (sig val name : string end)) -> struct
  module D = Debug.Make(struct let name=N.name end)

  let on_sigterm signum =
    D.info "Received signal %s: deregistering plugin %s..."
      (signal_name signum) N.name;
    D.info "Raising exception Killed in %s" __LOC__;
    Reporter.killed := true;
    raise Reporter.Killed

  let initialise () =
    (* CA-92551, CA-97938: Use syslog's local0 facility *)
    Debug.set_facility Syslog.Local0;

    D.info "installing signal handler for SIGTERM in %s" __LOC__;
    Sys.set_signal Sys.sigterm (Sys.Signal_handle on_sigterm);

    let pidfile = ref "" in
    let daemonize = ref false in
    Arg.parse (Arg.align [
        "-daemon", Arg.Set daemonize, "Create a daemon";
        "-pidfile", Arg.Set_string pidfile,
        Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
      ])
      (fun _ -> failwith "Invalid argument")
      (Printf.sprintf "Usage: %s [-daemon] [-pidfile filename]" N.name);

    if !daemonize then (
      D.info "Daemonizing ..";
      Unixext.daemonize ()
    ) else (
      D.info "Not daemonizing ..";
      Sys.catch_break true;
      Debug.log_to_stdout ()
    );

    if !pidfile <> "" then
      (D.debug "Storing process id into specified file ..";
       Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
       Unixext.pidfile_write !pidfile)

  let main_loop ~neg_shift ~target ~protocol ~dss_f =
    Reporter.start
      (module D)
      ~uid:N.name
      ~neg_shift
      ~target
      ~protocol
      ~dss_f
end
