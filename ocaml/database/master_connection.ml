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
(* Manages persistent connection from slave->master over which the db is accessed.

   The state of this connection is used by the slave to determine whether it can see its
   master or not. After the slave has not been able to see the master for a while, xapi
   restarts in emergency mode.
*)

open Safe_resources

type db_record = (string * string) list * (string * string list) list

module D = Debug.Make (struct let name = "master_connection" end)

open D

let my_connection : Stunnel.t option ref = ref None

let delay = Scheduler.PipeDelay.make ()

exception Uninitialised

let is_slave : (unit -> bool) ref =
  ref (fun () ->
      error "is_slave called without having been set. This is a fatal error." ;
      raise Uninitialised
  )

let get_master_address =
  ref (fun () ->
      error
        "get_master_address called without having been set. This is a fatal \
         error" ;
      raise Uninitialised
  )

let master_rpc_path = ref "<invalid>"

exception Cannot_connect_to_master

(* kill the stunnel underlying the connection.. When the master dies
   then read/writes to the connection block for ages waiting for the
   TCP timeout. By killing the stunnel, we can get these calls to
   unblock. (We do not clean up after killing stunnel, it is still the
   duty of the person who initiated the stunnel connection to call
   Stunnel.disconnect which closes the relevant fds and does wait_pid on
   the (now dead!) stunnel process.
*)
let force_connection_reset () =
  (* Cleanup cached stunnel connections to the master, so that future API
     	   calls won't be blocked. *)
  if !is_slave () then (
    let host = !get_master_address () in
    let port = !Db_globs.https_port in
    (* We don't currently have a method to enumerate all the stunnel links
       		   to an address in the cache. The easiest way is, for each valid config
       		   combination, we pop out (remove) its links until Not_found is raised.
       		   Here, we have two such combinations, i.e. verify_cert=true/false, as
       		   host and port are fixed values. *)
    let rec purge_stunnels verify_cert =
      match
        Stunnel_cache.with_remove ~host ~port verify_cert @@ fun st ->
        try Stunnel.disconnect ~wait:false ~force:true st with _ -> ()
      with
      | None ->
          () (* not found in cache: stop *)
      | Some () ->
          purge_stunnels verify_cert
    in
    purge_stunnels None ;
    purge_stunnels (Some Stunnel.pool) ;
    purge_stunnels (Some Stunnel.appliance) ;
    info
      "force_connection_reset: all cached connections to the master have been \
       purged"
  ) ;
  match !my_connection with
  | None ->
      ()
  | Some st_proc ->
      info "stunnel reset pid=%d fd=%d"
        (Stunnel.getpid st_proc.Stunnel.pid)
        (Xapi_stdext_unix.Unixext.int_of_file_descr
           Unixfd.(!(st_proc.Stunnel.fd))
        ) ;
      Unix.kill (Stunnel.getpid st_proc.Stunnel.pid) Sys.sigterm

(* whenever a call is made that involves read/write to the master connection, a timestamp is
   written into this global: *)
let last_master_connection_call : float option ref = ref None

(* the master_connection_watchdog uses this time to determine whether the master connection
   should be reset *)

(* Set and unset the timestamp global. (No locking required since we are operating under
   mutual exclusion provided by the database lock here anyway) *)
let with_timestamp f =
  last_master_connection_call := Some (Unix.gettimeofday ()) ;
  Xapi_stdext_pervasives.Pervasiveext.finally f (fun () ->
      last_master_connection_call := None
  )

(* call force_connection_reset if we detect that a master-connection is blocked for too long.
   One common way this can happen is if we end up blocked waiting for a TCP timeout when the
   master goes away unexpectedly... *)
let watchdog_start_mutex = Mutex.create ()

let my_watchdog : Thread.t option ref = ref None

let start_master_connection_watchdog () =
  Xapi_stdext_threads.Threadext.Mutex.execute watchdog_start_mutex (fun () ->
      match !my_watchdog with
      | None ->
          my_watchdog :=
            Some
              (Thread.create
                 (fun () ->
                   while true do
                     try
                       ( match !last_master_connection_call with
                       | None ->
                           ()
                       | Some t ->
                           let now = Unix.gettimeofday () in
                           let since_last_call = now -. t in
                           if
                             since_last_call
                             > !Db_globs.master_connection_reset_timeout
                           then (
                             debug
                               "Master connection timeout: forcibly resetting \
                                master connection" ;
                             force_connection_reset ()
                           )
                       ) ;
                       Thread.delay 10.
                     with _ -> ()
                   done
                 )
                 ()
              )
      | Some _ ->
          ()
  )

module StunnelDebug = Debug.Make (struct let name = "stunnel" end)

exception Goto_handler

(** Called when the connection to the master is (re-)established. This will be called once
    on slave start and then every time after the master restarts and we reconnect. *)
let on_database_connection_established = ref (fun () -> ())

let open_secure_connection () =
  let host = !get_master_address () in
  let port = !Db_globs.https_port in
  let verify_cert = Stunnel_client.pool () in
  Stunnel.with_connect ~use_fork_exec_helper:true ~extended_diagnosis:true
    ~write_to_log:(fun x -> debug "stunnel: %s\n" x)
    ~verify_cert host port
  @@ fun st_proc ->
  let fd_closed =
    Xapi_stdext_unix.Unixext.wait_timed_read
      Unixfd.(!(st_proc.Stunnel.fd))
      Mtime.Span.(5 * s)
  in
  let proc_quit =
    try
      Unix.kill (Stunnel.getpid st_proc.Stunnel.pid) 0 ;
      false
    with _ -> true
  in
  if (not fd_closed) && not proc_quit then (
    info "stunnel connected pid=%d fd=%d"
      (Stunnel.getpid st_proc.Stunnel.pid)
      (Xapi_stdext_unix.Unixext.int_of_file_descr Unixfd.(!(st_proc.Stunnel.fd))) ;
    my_connection := Some (Stunnel.move_out_exn st_proc) ;
    !on_database_connection_established ()
  ) else (
    info "stunnel disconnected fd_closed=%s proc_quit=%s"
      (string_of_bool fd_closed) (string_of_bool proc_quit) ;
    let () = try Stunnel.disconnect st_proc with _ -> () in
    raise Goto_handler
  )

(* Do a db xml_rpc request, catching exception and trying to reopen the connection if it
   fails *)
let connection_timeout = ref !Db_globs.master_connection_default_timeout

(* if this is true then xapi will restart if retries exceeded [and enter emergency mode if still
   can't reconnect after reboot]. if this is false then xapi will just throw exception if retries
   are exceeded *)
let restart_on_connection_timeout = ref true

exception Content_length_required

let do_db_xml_rpc_persistent_with_reopen ~host:_ ~path (req : string) :
    Db_interface.response =
  let time_call_started = Unix.gettimeofday () in
  let write_ok = ref false in
  let result = ref "" in
  let surpress_no_timeout_logs = ref false in
  let backoff_delay = ref 2.0 in
  (* initial delay = 2s *)
  let update_backoff_delay () =
    backoff_delay := !backoff_delay *. 2.0 ;
    if !backoff_delay < 2.0 then
      backoff_delay := 2.0
    else if !backoff_delay > 256.0 then
      backoff_delay := 256.0
  in
  while not !write_ok do
    try
      let req_string = req in
      let length = String.length req_string in
      if length > Db_globs.http_limit_max_rpc_size then
        raise Http.Client_requested_size_over_limit ;
      (* The pool_secret is added here and checked by the Xapi_http.add_handler RBAC code. *)
      let open Xmlrpc_client in
      let request =
        xmlrpc ~version:"1.1" ~frame:true ~keep_alive:true
          ~length:(Int64.of_int length) ~body:req path
        |> Db_secret_string.with_cookie !Db_globs.pool_secret
      in
      match !my_connection with
      | None ->
          raise Goto_handler
      | Some stunnel_proc ->
          let fd = stunnel_proc.Stunnel.fd in
          with_timestamp (fun () ->
              with_http request
                (fun (response, _) ->
                  (* XML responses must have a content-length because we cannot use the Xml.parse_in
                     in_channel function: the input channel will buffer an arbitrary amount of stuff
                     and we'll be out of sync with the next request. *)
                  let res =
                    match response.Http.Response.content_length with
                    | None ->
                        raise Content_length_required
                    | Some l ->
                        Xapi_stdext_unix.Unixext.really_read_string
                          Unixfd.(!fd)
                          (Int64.to_int l)
                  in
                  write_ok := true ;
                  result := res
                  (* yippeee! return and exit from while loop *)
                )
                Unixfd.(!fd)
          )
    with
    | Http.Client_requested_size_over_limit ->
        error "Content length larger than known limit (%d)."
          Db_globs.http_limit_max_rpc_size ;
        debug "Re-raising exception to caller." ;
        raise Http.Client_requested_size_over_limit
    (* TODO: This http exception handler caused CA-36936 and can probably be removed now that there's backoff delay in the generic handler _ below *)
    | Http_client.Http_error (http_code, err_msg) ->
        error
          "Received HTTP error %s (%s) from master. This suggests our master \
           address is wrong. Sleeping for %.0fs and then executing restart_fn."
          http_code err_msg
          !Db_globs.permanent_master_failure_retry_interval ;
        Thread.delay !Db_globs.permanent_master_failure_retry_interval ;
        !Db_globs.restart_fn ()
    | e ->
        error "Caught %s" (Printexc.to_string e) ;
        (* RPC failed - there's no way we can recover from this so try reopening connection every 2s + backoff delay *)
        ( match !my_connection with
        | None ->
            ()
        | Some st_proc -> (
            my_connection := None ;
            (* don't want to try closing multiple times *)
            try Stunnel.disconnect st_proc with _ -> ()
          )
        ) ;
        let time_sofar = Unix.gettimeofday () -. time_call_started in
        if !connection_timeout < 0. then (
          if not !surpress_no_timeout_logs then (
            debug
              "Connection to master died. I will continue to retry \
               indefinitely (supressing future logging of this message)." ;
            error
              "Connection to master died. I will continue to retry \
               indefinitely (supressing future logging of this message)."
          ) ;
          surpress_no_timeout_logs := true
        ) else
          debug
            "Connection to master died: time taken so far in this call '%f'; \
             will %s"
            time_sofar
            ( if !connection_timeout < 0. then
                "never timeout"
              else
                Printf.sprintf "timeout after '%f'" !connection_timeout
            ) ;
        if time_sofar > !connection_timeout && !connection_timeout >= 0. then
          if !restart_on_connection_timeout then (
            debug
              "Exceeded timeout for retrying master connection: restarting xapi" ;
            !Db_globs.restart_fn ()
          ) else (
            debug
              "Exceeded timeout for retrying master connection: raising \
               Cannot_connect_to_master" ;
            raise Cannot_connect_to_master
          ) ;
        debug "Sleeping %f seconds before retrying master connection..."
          !backoff_delay ;
        let timed_out = Scheduler.PipeDelay.wait delay !backoff_delay in
        if not timed_out then
          debug "%s: Sleep interrupted, retrying master connection now"
            __FUNCTION__ ;
        update_backoff_delay () ;
        D.log_and_ignore_exn open_secure_connection
  done ;
  !result

let execute_remote_fn string =
  let host = !get_master_address () in
  Db_lock.with_lock (fun () ->
      (* Ensure that this function is always called under mutual exclusion (provided by the recursive db lock) *)
      do_db_xml_rpc_persistent_with_reopen ~host ~path:!master_rpc_path string
  )
