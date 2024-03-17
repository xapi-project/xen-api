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
(* Copyright (C) 2007 XenSource Inc *)

module D = Debug.Make (struct let name = "stunnel" end)

open Printf
open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_unix
open Safe_resources

exception Stunnel_binary_missing

exception Stunnel_error of string

exception Stunnel_verify_error of string

let crl_path = "/etc/stunnel/crls"

let cached_stunnel_path = ref None

let stunnel_logger = ref ignore

let timeoutidle = ref None

let init_stunnel_path () =
  try cached_stunnel_path := Some (Unix.getenv "XE_STUNNEL")
  with Not_found ->
    let choices =
      [
        "/opt/xensource/libexec/stunnel/stunnel"
      ; "/usr/sbin/stunnel4"
      ; "/usr/sbin/stunnel"
      ; "/usr/bin/stunnel4"
      ; "/usr/bin/stunnel"
      ]
    in
    let rec choose l =
      match l with
      | [] ->
          raise Stunnel_binary_missing
      | p :: ps -> (
        try Unix.access p [Unix.X_OK] ; p with _ -> choose ps
      )
    in
    let path = choose choices in
    cached_stunnel_path := Some path

let stunnel_path () =
  if Option.is_none !cached_stunnel_path then
    init_stunnel_path () ;
  Option.get !cached_stunnel_path

module Unsafe = struct
  (** These functions are not safe in a multithreaded program *)

  (* Low-level (unsafe) function which forks, runs a 'pre_exec' function and
     	 then executes some other binary. It makes sure to catch any exception thrown by
     	 exec* so that we don't end up with two ocaml processes. *)
  let fork_and_exec ?(pre_exec = fun () -> ()) ?env (cmdline : string list) =
    let args = Array.of_list cmdline in
    let argv0 = List.hd cmdline in
    let pid = Unix.fork () in
    if pid = 0 then
      try
        pre_exec () ;
        (* CA-18955: xapi now runs with priority -3. We then set his sons priority to 0. *)
        ignore_int (Unix.nice (-Unix.nice 0)) ;
        ignore_int (Unix.setsid ()) ;
        match env with
        | None ->
            Unix.execv argv0 args
        | Some env ->
            Unix.execve argv0 args env
      with _ -> exit 1
    else
      pid

  (* File descriptor operations to be performed after a fork.
   * These are all safe in the presence of threads *)
  type fd_operation = Dup2 of Unix.file_descr * Unix.file_descr

  let do_fd_operation = function Dup2 (a, b) -> Unix.dup2 a b
end

type pid =
  | StdFork of int  (** we forked and exec'ed. This is the pid *)
  | FEFork of Forkhelpers.pidty  (** the forkhelpers module did it for us. *)
  | Nopid

let getpid ty =
  match ty with
  | StdFork pid ->
      pid
  | FEFork pid ->
      Forkhelpers.getpid pid
  | Nopid ->
      failwith "No pid!"

type verify = VerifyPeer | CheckHost

type verification_config = {
    sni: string option
  ; verify: verify
  ; cert_bundle_path: string
}

type t = {
    mutable pid: pid
  ; fd: Unixfd.t
  ; host: string
  ; port: int
  ; connected_time: float
  ; unique_id: int option
  ; mutable logfile: string
  ; verified: verification_config option
}

let appliance =
  {
    sni= None
  ; verify= CheckHost
  ; cert_bundle_path= "/etc/stunnel/xapi-stunnel-ca-bundle.pem"
  }

let pool =
  {
    sni= Some "pool"
  ; verify= VerifyPeer
  ; cert_bundle_path= "/etc/stunnel/xapi-pool-ca-bundle.pem"
  }

let debug_conf_of_bool verbose : string =
  if verbose then "debug=authpriv.7" else "debug=authpriv.5"

let debug_conf_of_env () : string =
  (try Unix.getenv "debug_stunnel" with _ -> "") |> String.lowercase_ascii
  |> fun x -> List.mem x ["yes"; "true"; "1"] |> debug_conf_of_bool

let config_file ?(accept = None) config host port =
  ( match config with
  | None ->
      D.debug "client cert verification %s:%d: None" host port
  | Some {sni= Some x; cert_bundle_path; _} ->
      D.debug "client cert verification %s:%d: SNI=%s path=%s" host port x
        cert_bundle_path
  | Some {sni= None; cert_bundle_path; _} ->
      D.debug "client cert verification %s:%d: path=%s" host port
        cert_bundle_path
  ) ;
  let is_fips =
    Inventory.inventory_filename := "/etc/xensource-inventory" ;
    try bool_of_string (Inventory.lookup ~default:"false" "CC_PREPARATIONS")
    with _ -> false
  in
  String.concat "\n"
  @@ List.concat
       [
         [
           "client=yes"
         ; "foreground=yes"
         ; "socket = r:TCP_NODELAY=1"
         ; "socket = r:SO_KEEPALIVE=1"
         ; "socket = a:SO_KEEPALIVE=1"
         ; ( match !timeoutidle with
           | None ->
               ""
           | Some x ->
               Printf.sprintf "TIMEOUTidle = %d" x
           )
         ]
       ; (if is_fips then ["fips=yes"] else ["fips=no"])
       ; [debug_conf_of_env ()]
       ; ( match accept with
         | Some (h, p) ->
             [
               "[client-proxy]"
             ; Printf.sprintf "accept=%s:%s" h (string_of_int p)
             ]
         | None ->
             []
         )
       ; [Printf.sprintf "connect=%s:%d" host port]
       ; [
           "sslVersion = TLSv1.2"
         ; "ciphers = " ^ Constants.good_ciphersuites
         ; "curve = secp384r1"
         ]
       ; ( match config with
         | None ->
             []
         | Some {sni; verify; cert_bundle_path} ->
             [
               ""
             ; "# use SNI to request a specific cert. CAfile contains"
             ; "# public certs of all hosts in the pool and must contain"
             ; "# the cert of the server we connect to"
             ; (match sni with None -> "" | Some s -> sprintf "sni = %s" s)
             ; ( match verify with
               | VerifyPeer ->
                   ""
               | CheckHost ->
                   sprintf "checkHost=%s" host
               )
             ; "verifyPeer=yes"
             ; sprintf "CAfile=%s" cert_bundle_path
             ; ( match Sys.readdir crl_path with
               | [||] ->
                   ""
               | _ ->
                   sprintf "CRLpath=%s" crl_path
               | exception _ ->
                   ""
               )
             ]
         )
       ; [""]
       ]

let ignore_exn f x = try f x with _ -> ()

let disconnect_with_pid ?(wait = true) ?(force = false) pid =
  let do_disc waiter pid =
    let res =
      try waiter ()
      with Unix.Unix_error (Unix.ECHILD, _, _) -> (pid, Unix.WEXITED 0)
    in
    match res with
    | 0, _ when force -> (
      try Unix.kill pid Sys.sigkill
      with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
    )
    | _ ->
        ()
  in
  match pid with
  | FEFork fpid ->
      let pid_int = Forkhelpers.getpid fpid in
      do_disc
        (fun () ->
          ( if wait then
              Forkhelpers.waitpid
            else
              Forkhelpers.waitpid_nohang
          )
            fpid
        )
        pid_int
  | StdFork pid ->
      do_disc
        (fun () ->
          ( if wait then
              Unix.waitpid []
            else
              Unix.waitpid [Unix.WNOHANG]
          )
            pid
        )
        pid
  | Nopid ->
      ()

let disconnect ?(wait = true) ?(force = false) x =
  ignore_exn Unixfd.safe_close x.fd ;
  disconnect_with_pid ~wait ~force x.pid ;
  (* make disconnect idempotent, need to do it here,
     due to the recursive call *)
  x.pid <- Nopid

(* With some probability, stunnel fails during its startup code before it reads
   the config data from us. Therefore we get a SIGPIPE writing the config data.
   Assuming SIGPIPE has been ignored, catch the failing write and throw this
   exception instead *)
exception Stunnel_initialisation_failed

(* Internal function which may throw Stunnel_initialisation_failed *)
let attempt_one_connect ?(use_fork_exec_helper = true)
    ?(write_to_log = fun _ -> ()) ?(extended_diagnosis = false) data_channel
    verify_cert host port =
  Unixfd.with_pipe () ~loc:__LOC__ @@ fun config_out config_in ->
  let config_out_uuid = Uuidx.(to_string (make ())) in
  let config_out_fd =
    string_of_int (Unixext.int_of_file_descr Unixfd.(!config_out))
  in
  let configs = [(config_out_uuid, Unixfd.(!config_out))] in
  let args =
    ["-fd"; (if use_fork_exec_helper then config_out_uuid else config_out_fd)]
  in
  let start sock_of_stunnel config =
    Forkhelpers.with_logfile_fd "stunnel" ~delete:(not extended_diagnosis)
      (fun logfd ->
        let path = stunnel_path () in
        let fds_needed, fdops, sock =
          match sock_of_stunnel with
          | Some s ->
              ( [Unixfd.(!config_out); Unix.stdin; Unix.stdout; Unix.stderr]
              , [
                  Unsafe.Dup2 (Unixfd.(!s), Unix.stdin)
                ; Unsafe.Dup2 (Unixfd.(!s), Unix.stdout)
                ; Unsafe.Dup2 (logfd, Unix.stderr)
                ]
              , Some Unixfd.(!s)
              )
          | None ->
              ([], [], None)
        in
        let pid =
          if use_fork_exec_helper || Option.is_none sock_of_stunnel then
            FEFork
              (Forkhelpers.safe_close_and_exec sock sock (Some logfd) configs
                 path args
              )
          else
            StdFork
              (Unsafe.fork_and_exec
                 ~pre_exec:(fun _ ->
                   List.iter Unsafe.do_fd_operation fdops ;
                   Unixext.close_all_fds_except fds_needed
                 )
                 (path :: args)
              )
        in
        Unixfd.safe_close config_out ;
        (* The sock_of_stunnel has been passed to stunnel process. Close it in XAPI *)
        Option.iter (fun s -> Unixfd.safe_close s) sock_of_stunnel ;
        (* Catch the occasional initialisation failure of stunnel: *)
        try
          let len = String.length config in
          let n =
            Unix.write Unixfd.(!config_in) (Bytes.of_string config) 0 len
          in
          if n < len then (
            disconnect_with_pid ~wait:false ~force:true pid ;
            raise Stunnel_initialisation_failed
          ) ;
          Unixfd.safe_close config_in ;
          pid
        with Unix.Unix_error (err, fn, arg) ->
          write_to_log
            (Printf.sprintf
               "Caught Unix.Unix_error(%s, %s, %s); raising \
                Stunnel_initialisation_failed"
               (Unix.error_message err) fn arg
            ) ;
          disconnect_with_pid ~wait:false ~force:true pid ;
          raise Stunnel_initialisation_failed
    )
  in
  let result =
    match data_channel with
    | `Local_host_port (h, p) ->
        (* The stunnel will listen on a local host and port *)
        let config = config_file ~accept:(Some (h, p)) verify_cert host port in
        start None config
    | `Unix_socket s ->
        (* The stunnel will listen on a UNIX socket *)
        let config = config_file verify_cert host port in
        start (Some s) config
  in
  (* Tidy up any remaining unclosed fds *)
  match result with
  | Forkhelpers.Success (log, pid) ->
      if extended_diagnosis then write_to_log "stunnel start" ;
      (pid, log)
  | Forkhelpers.Failure (log, exn) ->
      write_to_log ("stunnel abort: Log from stunnel: [" ^ log ^ "]") ;
      raise exn

(** To cope with a slightly unreliable stunnel, attempt to retry to make
    the connection a number of times. *)
let rec retry f = function
  | 0 ->
      raise Stunnel_initialisation_failed
  | n -> (
    try f ()
    with Stunnel_initialisation_failed ->
      (* Leave a few seconds between each attempt *)
      ignore (Unix.select [] [] [] 3.) ;
      retry f (n - 1)
  )

(** Establish a fresh stunnel to a (host, port)
    @param extended_diagnosis If true, the stunnel log file will not be
    deleted.  Instead, it is the caller's responsibility to delete it.  This
    allows the caller to use diagnose_failure below if stunnel fails.  *)
let with_connect ?unique_id ?use_fork_exec_helper ?write_to_log ~verify_cert
    ?(extended_diagnosis = false) host port f =
  let _ =
    match write_to_log with
    | Some logger ->
        stunnel_logger := logger
    | None ->
        ()
  in
  retry
    (fun () ->
      Unixfd.with_socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 ~loc:__LOC__
      @@ fun sock_of_stunnel sock_of_xapi ->
      let pid, logfile =
        attempt_one_connect ?use_fork_exec_helper ?write_to_log
          ~extended_diagnosis (`Unix_socket sock_of_stunnel) verify_cert host
          port
      in
      D.debug "Started a client (pid:%s): -> %s:%s"
        (string_of_int (getpid pid))
        host (string_of_int port) ;
      let t =
        {
          pid
        ; fd= sock_of_xapi
        ; host
        ; port
        ; connected_time= Unix.gettimeofday ()
        ; unique_id
        ; logfile
        ; verified= verify_cert
        }
      in
      f t
    )
    5

let with_client_proxy ~verify_cert ~remote_host ~remote_port ~local_host
    ~local_port f =
  ( try
      D.debug "Clean up running stunnel client proxy if there is any ..." ;
      let out, _ =
        Forkhelpers.execute_command_get_output "/usr/sbin/fuser"
          ["-4k"; string_of_int local_port ^ "/tcp"]
      in
      D.debug "Killed running stunnel client proxy:%s" out
    with
    | Forkhelpers.Spawn_internal_error (stderr, stdout, process_status) -> (
      match process_status with
      | Unix.WEXITED 1 ->
          D.debug "No running stunnel client proxy"
      | _ ->
          D.warn
            "Cleaning up running stunnel client proxy returned unexpectedly: \
             stdout=(%s); stderr=(%s)"
            stdout stderr
    )
  ) ;

  retry
    (fun () ->
      let pid, _ =
        attempt_one_connect
          (`Local_host_port (local_host, local_port))
          verify_cert remote_host remote_port
      in
      D.debug "Started a client proxy (pid:%s): %s:%s -> %s:%s"
        (string_of_int (getpid pid))
        local_host (string_of_int local_port) remote_host
        (string_of_int remote_port) ;
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () -> f ())
        (fun () -> disconnect_with_pid ~wait:false ~force:true pid)
    )
    5

let check_verify_error line =
  let sub_after i s =
    let len = String.length s in
    String.sub s i (len - i)
  in
  let split_1 c s =
    match Astring.String.cut ~sep:c s with Some (x, _) -> x | None -> s
  in
  if Astring.String.is_infix ~affix:"VERIFY ERROR: " line then
    match Astring.String.find_sub ~sub:"error=" line with
    | Some e ->
        raise
          (Stunnel_verify_error
             (split_1 "," (sub_after (e + String.length "error=") line))
          )
    | None ->
        raise (Stunnel_verify_error "")
  else
    ()

let check_error s line =
  if Astring.String.is_infix ~affix:line s then
    raise (Stunnel_error s)

let diagnose_failure st_proc =
  let check_line line =
    !stunnel_logger line ;
    check_verify_error line ;
    check_error "Connection refused" line ;
    check_error "No host resolved" line ;
    check_error "No route to host" line ;
    check_error "Invalid argument" line
  in
  Unixext.readfile_line check_line st_proc.logfile

(* If we reach here the whole stunnel log should have been gone through
   (possibly printed/logged somewhere. No necessity to raise an exception,
   since when this function being called, there is usually some exception
   already existing in the caller's context, and it's not necessary always a
   stunnel error.
*)

let test host port =
  let counter = ref 0 in
  while true do
    with_connect ~write_to_log:print_endline host ~verify_cert:None port
      disconnect ;
    incr counter ;
    if !counter mod 100 = 0 then (
      Printf.printf "Ran stunnel %d times\n" !counter ;
      flush stdout
    )
  done

let move_out_exn t = {t with fd= Safe.move_exn t.fd}

let with_moved_exn t f =
  Safe.within (Safe.move_exn t.fd) @@ fun fd -> f {t with fd}

let safe_release t = disconnect ~wait:false ~force:true t
