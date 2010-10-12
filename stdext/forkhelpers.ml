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
(* Functions to safely fork potentially long-running sub-processes without
   leaking file descriptors or accidentally deadlocking the parent process. *)

(* Functions should:
   1. Arrange to close all fds except the ones they actually want to keep open
   2. Not access any ocaml library or runtime function which might touch a lock
   (since that would cause deadlock) *)

(* XXX: this is a work in progress *)

let default_path = [ "/sbin"; "/usr/sbin"; "/bin"; "/usr/bin" ]

open Pervasiveext

type pidty = (Unix.file_descr * int) (* The forking executioner has been used, therefore we need to tell *it* to waitpid *)

let string_of_pidty (fd, pid) = Printf.sprintf "(FEFork (%d,%d))" (Unixext.int_of_file_descr fd) pid

exception Subprocess_failed of int
exception Subprocess_killed of int

let waitpid (sock, pid) =
	let status = Fecomms.read_raw_rpc sock in
	Unix.close sock;
	begin match status with
	  | Fe.Finished (Fe.WEXITED n) -> (pid,Unix.WEXITED n)
	  | Fe.Finished (Fe.WSIGNALED n) -> (pid,Unix.WSIGNALED n)
	  | Fe.Finished (Fe.WSTOPPED n) -> (pid,Unix.WSTOPPED n)
	end

let waitpid_nohang ((sock, _) as x) =
	(match Unix.select [sock] [] [] 0.0 with
	  | ([s],_,_) -> waitpid x
	  | _ -> (0,Unix.WEXITED 0))
	  
let dontwaitpid (sock, pid) =
	Unix.close sock


let waitpid_fail_if_bad_exit ty =
  let (_,status) = waitpid ty in
  match status with
    | (Unix.WEXITED 0) -> ()
    | (Unix.WEXITED n) -> raise (Subprocess_failed n)
    | (Unix.WSIGNALED n) -> raise (Subprocess_killed n)
    | (Unix.WSTOPPED n) -> raise (Subprocess_killed n)

let getpid (sock, pid) = pid

type 'a result = Success of string * 'a | Failure of string * exn

(** Creates a temporary file and opens it for logging. The fd is passed to the function
    'f'. The logfile is guaranteed to be closed afterwards, and unlinked if either the delete flag is set or the call fails. If the
    function 'f' throws an error then the log file contents are read in *)
let with_logfile_fd ?(delete = true) prefix f = 
  let logfile = Filename.temp_file prefix ".log" in
  let read_logfile () = 
    let contents = Unixext.string_of_file logfile in
    Unix.unlink logfile;
    contents in

  let log_fd = Unix.openfile logfile [ Unix.O_WRONLY; Unix.O_CREAT ] 0o0 in
  try
    let result = f log_fd in
    Unix.close log_fd;
    Success((if delete then read_logfile() else logfile), result)
  with e ->
    Unix.close log_fd;
    Failure(read_logfile(), e)


exception Spawn_internal_error of string * string * Unix.process_status

let id = ref 0 

(** Safe function which forks a command, closing all fds except a whitelist and
    having performed some fd operations in the child *)
let safe_close_and_exec ?env stdin stdout stderr (fds: (string * Unix.file_descr) list) 
    (cmd: string) (args: string list) = 

  let sock = Fecomms.open_unix_domain_sock_client "/var/xapi/forker/main" in
  let stdinuuid = Uuid.to_string (Uuid.make_uuid ()) in
  let stdoutuuid = Uuid.to_string (Uuid.make_uuid ()) in
  let stderruuid = Uuid.to_string (Uuid.make_uuid ()) in

  let fds_to_close = ref [] in

  let add_fd_to_close_list fd = fds_to_close := fd :: !fds_to_close in
  let remove_fd_from_close_list fd = fds_to_close := List.filter (fun fd' -> fd' <> fd) !fds_to_close in
  let close_fds () = List.iter (fun fd -> Unix.close fd) !fds_to_close in

  finally (fun () -> 

    let maybe_add_id_to_fd_map id_to_fd_map (uuid,fd,v) =
      match v with 
	| Some _ -> (uuid, fd)::id_to_fd_map
	| None -> id_to_fd_map
    in

    let predefined_fds = [
      (stdinuuid, Some 0, stdin);
      (stdoutuuid, Some 1, stdout);
      (stderruuid, Some 2, stderr)] 
    in

    (* We don't care what fd these end up as - they're named in the argument list for us, and the
       forking executioner will sort it out. *)
    let dest_named_fds = List.map (fun (uuid,_) -> (uuid,None)) fds in
    let id_to_fd_map = List.fold_left maybe_add_id_to_fd_map dest_named_fds predefined_fds in

    let env = match env with 
      |	Some e -> e
      | None -> [| "PATH=" ^ (String.concat ":" default_path) |]
    in
    Fecomms.write_raw_rpc sock (Fe.Setup {Fe.cmdargs=(cmd::args); env=(Array.to_list env); id_to_fd_map = id_to_fd_map});

    let response = Fecomms.read_raw_rpc sock in

    let s = match response with
      | Fe.Setup_response s -> s 
      | _ -> failwith "Failed to communicate with forking executioner"
    in

    let fd_sock = Fecomms.open_unix_domain_sock_client s.Fe.fd_sock_path in
    add_fd_to_close_list fd_sock;
    
    let send_named_fd uuid fd =
      Fecomms.send_named_fd fd_sock uuid fd;
    in

    List.iter (fun (uuid,_,srcfdo) ->
      match srcfdo with Some srcfd -> send_named_fd uuid srcfd | None -> ()) predefined_fds;
    List.iter (fun (uuid,srcfd) ->
      send_named_fd uuid srcfd) fds;
    Fecomms.write_raw_rpc sock Fe.Exec;
    match Fecomms.read_raw_rpc sock with Fe.Execed pid -> (sock, pid))
   
    close_fds


let execute_command_get_output ?env cmd args =
  match with_logfile_fd "execute_command_get_out" (fun out_fd ->
    with_logfile_fd "execute_command_get_err" (fun err_fd ->
      let (sock,pid) = safe_close_and_exec ?env None (Some out_fd) (Some err_fd) [] cmd args in
      match Fecomms.read_raw_rpc sock with
	| Fe.Finished x -> Unix.close sock; x
	| _ -> Unix.close sock; failwith "Communications error"	    
    )) with
    | Success(out,Success(err,(status))) -> 
	begin
	  match status with
	    | Fe.WEXITED 0 -> (out,err)
	    | Fe.WEXITED n -> raise (Spawn_internal_error(err,out,Unix.WEXITED n))
	    | Fe.WSTOPPED n -> raise (Spawn_internal_error(err,out,Unix.WSTOPPED n))
	    | Fe.WSIGNALED n -> raise (Spawn_internal_error(err,out,Unix.WSIGNALED n))
	end
    | Success(_,Failure(_,exn))
    | Failure(_, exn) ->
	raise exn

