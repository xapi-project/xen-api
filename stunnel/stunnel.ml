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

open Printf
open Pervasiveext
open Stringext

exception Stunnel_binary_missing
exception Stunnel_error of string
exception Stunnel_verify_error of string

let certificate_path = "/etc/stunnel/certs"
let crl_path = "/etc/stunnel/crls"

let use_new_stunnel = ref false
let new_stunnel_path = "/usr/sbin/stunnelng"

let cached_stunnel_path = ref None

let init_stunnel_path () =
  try cached_stunnel_path := Some (Unix.getenv "XE_STUNNEL")
  with Not_found ->
    if !use_new_stunnel then
      cached_stunnel_path := Some new_stunnel_path
    else (
      let choices = ["/opt/xensource/libexec/stunnel/stunnel";
		     "/usr/sbin/stunnel4";
		     "/usr/sbin/stunnel";
		     "/usr/bin/stunnel4";
		     "/usr/bin/stunnel";
		    ] in
      let rec choose l =
        match l with
	    [] -> raise Stunnel_binary_missing
	  | (p::ps) ->
	      try Unix.access p [Unix.X_OK]; p
	      with _ -> choose ps in
      let path = choose choices in
      cached_stunnel_path := Some path
    )

let stunnel_path() =
  match !cached_stunnel_path with
    | Some p -> p 
    | None -> raise Stunnel_binary_missing

type t = { mutable pid: int; fd: Unix.file_descr; host: string; port: int; 
	   connected_time: float;
	   unique_id: int option;
	   mutable logfile: string;
	 }

let config_file verify_cert extended_diagnosis host port = 
  let lines = ["client=yes"; "foreground=yes"; "socket = r:TCP_NODELAY=1"; Printf.sprintf "connect=%s:%d" host port ] @
    (if extended_diagnosis then
       ["debug=4"]
     else
       []) @
    (if verify_cert then
       ["verify=2";
        sprintf "CApath=%s" certificate_path;
        sprintf "CRLpath=%s" crl_path]
     else
       [])
  in
    String.concat "" (List.map (fun x -> x ^ "\n") lines)

let ignore_exn f x = try f x with _ -> ()

let disconnect x = 
  List.iter (ignore_exn Unix.close) [ x.fd ];
  ignore_exn Forkhelpers.waitpid x.pid

(* With some probability, stunnel fails during its startup code before it reads
   the config data from us. Therefore we get a SIGPIPE writing the config data.
   Assuming SIGPIPE has been ignored, catch the failing write and throw this
   exception instead *)
exception Stunnel_initialisation_failed

let attempt_one_connect_new ?unique_id ?(use_external_fd_wrapper = true) ?(write_to_log = fun _ -> ()) verify_cert extended_diagnosis host port = 
  assert (not verify_cert); (* !!! Unimplemented *)
  assert (not extended_diagnosis); (* !!! Unimplemented *)
  let data_out,data_in = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let args = [ "-m"; "client"; "-s"; "-"; "-d"; Printf.sprintf "%s:%d" host port ] in
  let t = { pid = 0; fd = data_out; host = host; port = port; 
	    connected_time = Unix.gettimeofday (); unique_id = unique_id;
	    logfile = "" } in
  let to_close = ref [ data_in ] in
  let result = Forkhelpers.with_logfile_fd "stunnel" (fun logfd ->
    let fdops = [
      Forkhelpers.Dup2(data_in, Unix.stdin);
      Forkhelpers.Dup2(data_in, Unix.stdout);
      Forkhelpers.Dup2(logfd, Unix.stderr)
    ] in
    let fds_needed = [ Unix.stdin; Unix.stdout; Unix.stderr ] in
    t.pid <- (
      if use_external_fd_wrapper then
        Forkhelpers.safe_close_and_exec fdops fds_needed (stunnel_path ()) args
      else
        Forkhelpers.fork_and_exec ~pre_exec:(fun _ -> 
          List.iter Forkhelpers.do_fd_operation fdops;
          Unixext.close_all_fds_except fds_needed
	  ) ((stunnel_path ()) :: args)
    );
    List.iter Unix.close [ data_in ];
  ) in
  List.iter Unix.close !to_close; 
  match result with
  | Forkhelpers.Failure(log, exn) ->
      write_to_log ("failed: Log from stunnel: [" ^ log ^ "]");
      disconnect t;
      raise exn
  | Forkhelpers.Success(log, _) -> 
      write_to_log ("success: Log from stunnel: [" ^ log ^ "]");
      t

(* Internal function which may throw Stunnel_initialisation_failed *)
let attempt_one_connect ?unique_id ?(use_external_fd_wrapper = true) ?(write_to_log = fun _ -> ()) verify_cert extended_diagnosis host port =
  let data_out,data_in = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  and config_out, config_in = Unix.pipe ()
  in
  (* FDs we must close. NB stdin_in and stdout_out end up in our 't' record *)
  let to_close = ref [ data_in; config_out; config_in  ] in
  let close fd = 
    if List.mem fd !to_close 
    then (Unix.close fd; to_close := List.filter (fun x -> x <> fd) !to_close) in
  let t = { pid = 0; fd = data_out; host = host; port = port; 
	    connected_time = Unix.gettimeofday (); unique_id = unique_id;
	    logfile = "" } in
  let result = Forkhelpers.with_logfile_fd "stunnel"
    ~delete:(not extended_diagnosis)
    (fun logfd ->
       let path = stunnel_path() in
       let fdops = 
	 [ Forkhelpers.Dup2(data_in, Unix.stdin);
	   Forkhelpers.Dup2(data_in, Unix.stdout);
	   Forkhelpers.Dup2(logfd, Unix.stderr) ] in
       let fds_needed = [ Unix.stdin; Unix.stdout; Unix.stderr; config_out ] in
       let args = [ "-fd"; string_of_int (Unixext.int_of_file_descr config_out) ] in
       if use_external_fd_wrapper then begin
	 let cmdline = Printf.sprintf "Using commandline: %s\n" (String.concat " " (Forkhelpers.close_and_exec_cmdline fds_needed path args)) in
	 write_to_log cmdline;
       end;
       t.pid <-
	 (if use_external_fd_wrapper
	  (* Run thread-safe external wrapper *)
	  then Forkhelpers.safe_close_and_exec fdops fds_needed path args
	  (* or do it ourselves (safe if there are no threads) *)
	  else Forkhelpers.fork_and_exec ~pre_exec:
	      (fun _ -> 
		 List.iter Forkhelpers.do_fd_operation fdops;
		 Unixext.close_all_fds_except fds_needed) 
	      (path::args) );
       List.iter close [ data_in; config_out; ]; 
       (* Make sure we close config_in eventually *)
       finally
	 (fun () ->

	    let pidmsg = Printf.sprintf "stunnel has pid: %d\n" t.pid in
	    write_to_log pidmsg;

	    let config = config_file verify_cert extended_diagnosis host port in
	    (* Catch the occasional initialisation failure of stunnel: *)
	    try
	      let n = Unix.write config_in config 0 (String.length config) in
	      if n < String.length config then raise Stunnel_initialisation_failed
	    with Unix.Unix_error(err, fn, arg) -> 
	      write_to_log (Printf.sprintf "Caught Unix.Unix_error(%s, %s, %s); raising Stunnel_initialisation_failed" (Unix.error_message err) fn arg);
	      raise Stunnel_initialisation_failed)
	 (fun () -> close config_in)) in
    (* Tidy up any remaining unclosed fds *)
  List.iter Unix.close !to_close; 
  match result with
  | Forkhelpers.Success(log, _) -> 
      if extended_diagnosis then
        begin
          write_to_log "success";
          t.logfile <- log
        end
      else
        write_to_log ("success: Log from stunnel: [" ^ log ^ "]");
      t
  | Forkhelpers.Failure(log, exn) ->
      write_to_log ("failed: Log from stunnel: [" ^ log ^ "]");
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

(** Establish a fresh stunnel to a (host, port)
    @param extended_diagnosis If true, the stunnel log file will not be
    deleted.  Instead, it is the caller's responsibility to delete it.  This
    allows the caller to use diagnose_failure below if stunnel fails.  *)
let connect ?unique_id ?use_external_fd_wrapper ?write_to_log
    ?(verify_cert=false) ?(extended_diagnosis=false) host port = 
  let connect = if !use_new_stunnel then attempt_one_connect_new else attempt_one_connect in
  retry (fun () -> connect ?unique_id ?use_external_fd_wrapper ?write_to_log verify_cert extended_diagnosis host port) 5

let sub_after i s =
  let len = String.length s in
    String.sub s i (len - i)

let split_1 c s =
  match String.split ~limit:1 c s with
    | x :: _ -> x
    | [] -> s

let check_verify_error line =
  match String.find_all "VERIFY ERROR: " line with
      | p :: _ ->
          begin
            match String.find_all "error=" line with
              | e :: _ ->
                  raise
                    (Stunnel_verify_error
                       (split_1 ','
                          (sub_after (e + String.length "error=") line)))
              | [] ->
                  raise (Stunnel_verify_error "")
          end
      | [] ->
          ()
          
let check_error s line =
  if (String.has_substr line s) then
    raise (Stunnel_error s)
    
let diagnose_failure st_proc =
  let check_line line =
    Printf.eprintf "stunnel_failure: %s\n" line;
    check_verify_error line;
    check_error "Connection refused" line; 
    check_error "No host resolved" line;
    check_error "Invalid argument" line;
  in
    Unixext.readfile_line check_line st_proc.logfile;
    raise (Stunnel_error (Unixext.read_whole_file_to_string st_proc.logfile))

let test host port = 
  let counter = ref 0 in
  while true do
    let c = connect ~write_to_log:print_endline host port in
    disconnect c;
    incr counter;
    if !counter mod 100 = 0 then (Printf.printf "Ran stunnel %d times\n" !counter; flush stdout)
  done
