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

module D=Debug.Make(struct let name="stunnel" end)
open D
let trim = String.trim

open Printf
open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_unix

exception Stunnel_binary_missing
exception Stunnel_error of string
exception Stunnel_verify_error of string

let certificate_path = "/etc/stunnel/certs"
let crl_path = "/etc/stunnel/crls"
let verify_certificates_ctrl = "/var/xapi/verify_certificates"

let use_new_stunnel = ref false
let new_stunnel_path = "/usr/sbin/stunnelng"

let cached_stunnel_path = ref None
let stunnel_logger = ref ignore

let timeoutidle = ref None

(* Use good settings by default *)
let legacy_protocol_and_ciphersuites_allowed = ref false

let is_legacy_protocol_and_ciphersuites_allowed () =
  !legacy_protocol_and_ciphersuites_allowed

let good_ciphersuites = ref (Some "!EXPORT:RSA+AES128-SHA256")
let legacy_ciphersuites = ref "RSA+AES256-SHA:RSA+AES128-SHA:RSA+RC4-SHA:RSA+DES-CBC3-SHA"

let init_stunnel_path () =
  try cached_stunnel_path := Some (Unix.getenv "XE_STUNNEL")
  with Not_found ->
    if !use_new_stunnel then
      cached_stunnel_path := Some new_stunnel_path
    else (
      let choices = [
        "/opt/xensource/libexec/stunnel/stunnel";
        "/usr/sbin/stunnel4";
        "/usr/sbin/stunnel";
        "/usr/bin/stunnel4";
        "/usr/bin/stunnel";
      ] in
      let rec choose l =
        match l with
        | [] -> raise Stunnel_binary_missing
        | p::ps ->
          try Unix.access p [Unix.X_OK]; p
          with _ -> choose ps
      in
      let path = choose choices in
      cached_stunnel_path := Some path
    )

let stunnel_path () =
  let open Xapi_stdext_monadic in
  if Opt.is_none !cached_stunnel_path then
    init_stunnel_path ();
  Opt.unbox !cached_stunnel_path

module Unsafe = struct
  (** These functions are not safe in a multithreaded program *)

  (* Low-level (unsafe) function which forks, runs a 'pre_exec' function and
     	 then executes some other binary. It makes sure to catch any exception thrown by
     	 exec* so that we don't end up with two ocaml processes. *)
  let fork_and_exec ?(pre_exec=fun () -> ()) ?env (cmdline: string list) = 
    let args = Array.of_list cmdline in
    let argv0 = List.hd cmdline in
    let pid = Unix.fork () in
    if pid = 0 then begin
      try
        pre_exec ();
        (* CA-18955: xapi now runs with priority -3. We then set his sons priority to 0. *) 
        ignore_int (Unix.nice (-(Unix.nice 0)));
        ignore_int (Unix.setsid ());
        match env with
        | None -> Unix.execv argv0 args
        | Some env -> Unix.execve argv0 args env
      with _ -> exit 1
    end else pid

  (* File descriptor operations to be performed after a fork.
   * These are all safe in the presence of threads *)
  type fd_operation =
    | Dup2 of Unix.file_descr * Unix.file_descr
    | Close of Unix.file_descr

  let do_fd_operation = function
    | Dup2(a, b) -> Unix.dup2 a b
    | Close a -> Unix.close a
end

type pid =
  | StdFork of int (** we forked and exec'ed. This is the pid *)
  | FEFork of Forkhelpers.pidty (** the forkhelpers module did it for us. *)
  | Nopid

(* let string_of_pid = function
  | StdFork x -> Printf.sprintf "(StdFork %d)" x
  | FEFork x -> Forkhelpers.string_of_pidty x
  | Nopid -> "None" *)

let getpid ty =
  match ty with
  | StdFork pid -> pid
  | FEFork pid -> Forkhelpers.getpid pid
  | Nopid -> failwith "No pid!"

type t = { mutable pid: pid; fd: Unix.file_descr; host: string; port: int; 
           connected_time: float;
           unique_id: int option;
           mutable logfile: string;
           verified: bool;
           legacy: bool;
         }

let config_file verify_cert extended_diagnosis host port legacy =

  let good_ciphers () = match !good_ciphersuites with
    | None -> raise (Stunnel_error "good_ciphersuites is unset in the OCaml Stunnel module.")
    | Some s -> s
  in

  let lines = [
    "client=yes"; "foreground=yes"; "socket = r:TCP_NODELAY=1"; "socket = r:SO_KEEPALIVE=1"; "socket = a:SO_KEEPALIVE=1";
    (match !timeoutidle with None -> "" | Some x -> Printf.sprintf "TIMEOUTidle = %d" x);
    Printf.sprintf "connect=%s:%d" host port;
    "fips = no"; (* stunnel fips-mode stops us using sslVersion other than TLSv1 which means 1.0 only. *)
  ] @	(if extended_diagnosis then
         ["debug=4"]
       else
         []
      ) @ (
      if verify_cert then
        ["verify=2";
         sprintf "CApath=%s" certificate_path;
         sprintf "CRLpath=%s" crl_path]
      else
        []
    ) @ (
      if legacy then [
        "sslVersion = all";
        "options = NO_SSLv2";
        "options = NO_SSLv3";
        "ciphers = " ^ (good_ciphers ()) ^ (match !legacy_ciphersuites with "" -> "" | s -> (":" ^ s))
      ] else [
        "sslVersion = TLSv1.2";
        "ciphers = " ^ (good_ciphers ());
      ]
    )
  in
  String.concat "" (List.map (fun x -> x ^ "\n") lines)

let set_legacy_protocol_and_ciphersuites_allowed b =
  legacy_protocol_and_ciphersuites_allowed := b;
  info "legacy-config %B; example: %S" b
    (match !good_ciphersuites with
     | None -> "(Ciphersuites are not configured.)"
     | _ -> config_file false false "dummyhost" 443 b)

let set_good_ciphersuites s =
  info "set_good_ciphersuites received %S" s;
  if trim s <> "" then (
    good_ciphersuites := Some s
  ) else raise (Stunnel_error
                  ("Stunnel.set_good_ciphersuites received a blank or empty string. Leaving it unchanged as " ^
                   (match !good_ciphersuites with None -> "None" | Some s -> ("Some " ^ (String.escaped s)))
                  )
               )

let set_legacy_ciphersuites s =
  legacy_ciphersuites := s;
  info "set_legacy_ciphersuites: %S" s

let ignore_exn f x = try f x with _ -> ()

let rec disconnect ?(wait = true) ?(force = false) x =
  List.iter (ignore_exn Unix.close) [ x.fd ];

  let do_disc waiter pid =
    let res =
      try waiter ()
      with Unix.Unix_error (Unix.ECHILD, _, _) -> pid, Unix.WEXITED 0 in
    match res with
    | 0, _ when force ->
      (try Unix.kill pid Sys.sigkill 
       with Unix.Unix_error (Unix.ESRCH, _, _) ->());
      disconnect ~wait:wait ~force:force x
    | _ -> ()
  in
  let verbose = x.legacy && not (!legacy_protocol_and_ciphersuites_allowed) in
  match x.pid with
  | FEFork fpid ->
    let pid_int = Forkhelpers.getpid fpid in
    if verbose then info "Disconnecting FEFork %d" pid_int;
    do_disc
      (fun () ->
         (if wait then Forkhelpers.waitpid 
          else Forkhelpers.waitpid_nohang) fpid)
      pid_int
  | StdFork pid ->
    if verbose then info "Disconnecting StdFork %d" pid;
    do_disc
      (fun () ->
         (if wait then Unix.waitpid []
          else Unix.waitpid [Unix.WNOHANG]) pid)
      pid
  | Nopid -> if verbose then info "Disconnecting Nopid"

(* With some probability, stunnel fails during its startup code before it reads
   the config data from us. Therefore we get a SIGPIPE writing the config data.
   Assuming SIGPIPE has been ignored, catch the failing write and throw this
   exception instead *)
exception Stunnel_initialisation_failed


(* Internal function which may throw Stunnel_initialisation_failed *)
let attempt_one_connect ?unique_id ?(use_fork_exec_helper = true)
    ?(write_to_log = fun _ -> ()) verify_cert extended_diagnosis host port =
  let fds_needed = ref [ Unix.stdin; Unix.stdout; Unix.stderr ] in
  let config_in, config_out, configs, args = 
    if !use_new_stunnel
    then begin
      assert (not verify_cert); (* !! Unimplemented *)
      let args = [ "-m"; "client"; "-s"; "-"; "-d"; 
                   Printf.sprintf "%s:%d" host port ] in
      None, None, [], (if extended_diagnosis then "-v" :: args else args)
    end else begin
      let config_out, config_in = Unix.pipe () in
      let config_out_uuid = Uuidm.to_string (Uuidm.create `V4) in
      let config_out_fd = 
        string_of_int (Unixext.int_of_file_descr config_out) in
      fds_needed := config_out :: !fds_needed;
      Some config_in, Some config_out, [(config_out_uuid, config_out)],
      ["-fd"; if use_fork_exec_helper then config_out_uuid else config_out_fd]
    end in
  let data_out,data_in = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (* Dereference just once to ensure we are consistent in t and config_file *)
  let legacy = !legacy_protocol_and_ciphersuites_allowed in
  let t = 
    { pid = Nopid; fd = data_out; host = host; port = port; 
      connected_time = Unix.gettimeofday (); unique_id = unique_id; 
      logfile = ""; verified = verify_cert;
      legacy = legacy } in
  let result = Forkhelpers.with_logfile_fd "stunnel"
      ~delete:(not extended_diagnosis)
      (fun logfd ->
         let path = stunnel_path() in
         let fdops = 
           [ Unsafe.Dup2(data_in, Unix.stdin);
             Unsafe.Dup2(data_in, Unix.stdout);
             Unsafe.Dup2(logfd, Unix.stderr) ] in
         t.pid <-
           if use_fork_exec_helper then begin
             FEFork(Forkhelpers.safe_close_and_exec 
                      (Some data_in) (Some data_in) (Some logfd) configs path args)
           end else
             StdFork(Unsafe.fork_and_exec 
                       ~pre_exec:(fun _ -> 
                           List.iter Unsafe.do_fd_operation fdops;
                           Unixext.close_all_fds_except !fds_needed) 
                       (path::args));
         (match config_out with Some fd -> Unix.close fd | _ -> ());
         Unix.close data_in;
         (* Make sure we close config_in eventually *)
         finally
           (fun () ->
              match config_in with
              | Some fd -> begin
                  let config = config_file verify_cert extended_diagnosis host port legacy in
                  (* Catch the occasional initialisation failure of stunnel: *)
                  try
                    let n = Unix.write fd config 0 (String.length config) in
                    if n < String.length config then raise Stunnel_initialisation_failed
                  with Unix.Unix_error(err, fn, arg) -> 
                    write_to_log (Printf.sprintf "Caught Unix.Unix_error(%s, %s, %s); raising Stunnel_initialisation_failed" (Unix.error_message err) fn arg);
                    raise Stunnel_initialisation_failed
                end 
              | _ -> ())
           (fun () -> match config_in with Some fd -> Unix.close fd | _ -> assert false)) in
  (* Tidy up any remaining unclosed fds *)
  match result with
  | Forkhelpers.Success(log, _) -> 
    if extended_diagnosis then begin
      write_to_log "stunnel start";
      t.logfile <- log
    end;
    t
  | Forkhelpers.Failure(log, exn) ->
    write_to_log ("stunnel abort: Log from stunnel: [" ^ log ^ "]");
    disconnect t;
    raise exn

(** To cope with a slightly unreliable stunnel, attempt to retry to make 
    the connection a number of times. *)
let rec retry f = function
  | 0 -> raise Stunnel_initialisation_failed
  | n -> 
    try f ()
    with Stunnel_initialisation_failed -> 
      (* Leave a few seconds between each attempt *)
      ignore(Unix.select [] [] [] 3.);
      retry f (n - 1)

let must_verify_cert verify_cert =
  match verify_cert with
  | Some x -> x
  | None -> Sys.file_exists verify_certificates_ctrl

(** Establish a fresh stunnel to a (host, port)
    @param extended_diagnosis If true, the stunnel log file will not be
    deleted.  Instead, it is the caller's responsibility to delete it.  This
    allows the caller to use diagnose_failure below if stunnel fails.  *)
let connect
    ?unique_id
    ?use_fork_exec_helper
    ?write_to_log
    ?verify_cert
    ?(extended_diagnosis=false)
    host
    port = 
  let _verify_cert = must_verify_cert verify_cert in
  let _ = match write_to_log with 
    | Some logger -> stunnel_logger := logger
    | None -> () in
  retry (fun () -> attempt_one_connect ?unique_id ?use_fork_exec_helper ?write_to_log _verify_cert extended_diagnosis host port) 5

let check_verify_error line =
  let sub_after i s =
    let len = String.length s in
    String.sub s i (len - i)
  in
  let split_1 c s =
    match Astring.String.cut ~sep:c s with
    | Some (x , _) -> x
    | None -> s
  in
  if Astring.String.is_infix ~affix:"VERIFY ERROR: " line then
    match Astring.String.find_sub ~sub:"error=" line with
    | Some e ->
      raise
        (Stunnel_verify_error
           (split_1 ","
              (sub_after (e + String.length "error=") line)))
    | None ->
      raise (Stunnel_verify_error "")
  else ()

let check_error s line =
  if Astring.String.is_infix ~affix:line s then
    raise (Stunnel_error s)

let diagnose_failure st_proc =
  let check_line line =
    !stunnel_logger line;
    check_verify_error line;
    check_error "Connection refused" line;
    check_error "No host resolved" line;
    check_error "No route to host" line;
    check_error "Invalid argument" line in
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
    let c = connect ~write_to_log:print_endline host port in
    disconnect c;
    incr counter;
    if !counter mod 100 = 0 then (Printf.printf "Ran stunnel %d times\n" !counter; flush stdout)
  done
