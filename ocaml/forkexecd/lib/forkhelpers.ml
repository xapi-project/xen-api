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

let default_path = ["/sbin"; "/usr/sbin"; "/bin"; "/usr/bin"]

let default_path_env_pair = [|"PATH=" ^ String.concat ":" default_path|]

(* /var/ may not be writable when testing, use XDG_RUNTIME_DIR instead in that
   case. Avoid changing the directory unless the code is being tested. *)
let test_path =
  Option.bind (Sys.getenv_opt "FE_TEST") (fun _ ->
      Sys.getenv_opt "XDG_RUNTIME_DIR"
  )

let runtime_path = Option.value ~default:"/var" test_path

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

type pidty = Unix.file_descr * int

(* The forking executioner has been used, therefore we need to tell *it* to waitpid *)

let string_of_pidty (fd, pid) =
  Printf.sprintf "(FEFork (%d,%d))" (Fd_send_recv.int_of_fd fd) pid

exception Subprocess_failed of int

exception Subprocess_killed of int

exception Subprocess_timeout

let waitpid (sock, pid) =
  let status = Fecomms.read_raw_rpc sock in
  Unix.close sock ;
  match status with
  | Ok Fe.(Finished (WEXITED n)) ->
      (pid, Unix.WEXITED n)
  | Ok Fe.(Finished (WSIGNALED n)) ->
      (pid, Unix.WSIGNALED n)
  | Ok Fe.(Finished (WSTOPPED n)) ->
      (pid, Unix.WSTOPPED n)
  | Ok status ->
      let msg =
        Printf.sprintf "%s: unexpected status received (%s)" __FUNCTION__
          (Fe.ferpc_to_string status)
      in
      failwith msg
  | Error err ->
      let msg =
        Printf.sprintf "%s: error happened when trying to read the status. %s"
          __FUNCTION__ err
      in
      failwith msg

let waitpid_nohang ((sock, _) as x) =
  Unix.set_nonblock sock ;
  let r =
    try waitpid x
    with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
      (0, Unix.WEXITED 0)
  in
  Unix.clear_nonblock sock ; r

let dontwaitpid (sock, _pid) =
  ( try
      (* Try to tell the child fe that we're not going to wait for it. If the
         other end of the pipe has been closed then this doesn't matter, as this
         means the child has already exited. *)
      Fecomms.write_raw_rpc sock Fe.Dontwaitpid
    with Unix.Unix_error (Unix.EPIPE, _, _) -> ()
  ) ;
  Unix.close sock

let waitpid_fail_if_bad_exit ty =
  let _, status = waitpid ty in
  match status with
  | Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED n ->
      raise (Subprocess_failed n)
  | Unix.WSIGNALED n ->
      raise (Subprocess_killed n)
  | Unix.WSTOPPED n ->
      raise (Subprocess_killed n)

let getpid (_sock, pid) = pid

type 'a result = Success of string * 'a | Failure of string * exn

let temp_dir_server = runtime_path ^ "/run/nonpersistent/forkexecd/"

let temp_dir =
  try
    Unix.access temp_dir_server [Unix.W_OK; Unix.R_OK; Unix.X_OK] ;
    Some temp_dir_server
  with _ -> None

(** Creates a temporary file and opens it for logging. The fd is passed to the function
    'f'. The logfile is guaranteed to be closed afterwards, and unlinked if either the delete flag is set or the call fails. If the
    function 'f' throws an error then the log file contents are read in *)
let with_logfile_fd ?(delete = true) prefix f =
  let logfile = Filename.temp_file ?temp_dir prefix ".log" in
  let read_logfile () =
    let contents = Xapi_stdext_unix.Unixext.string_of_file logfile in
    Unix.unlink logfile ; contents
  in

  let log_fd = Unix.openfile logfile [Unix.O_WRONLY; Unix.O_CREAT] 0o0 in
  try
    let result = f log_fd in
    Unix.close log_fd ;
    Success ((if delete then read_logfile () else logfile), result)
  with e ->
    Unix.close log_fd ;
    Failure (read_logfile (), e)

exception Spawn_internal_error of string * string * Unix.process_status

type syslog_stdout_t =
  | NoSyslogging
  | Syslog_DefaultKey
  | Syslog_WithKey of string

(** Safe function which forks a command, closing all fds except a whitelist and
    having performed some fd operations in the child *)
let safe_close_and_exec ?env stdin stdout stderr
    (fds : (string * Unix.file_descr) list) ?(syslog_stdout = NoSyslogging)
    ?(redirect_stderr_to_stdout = false) (cmd : string) (args : string list) =
  let sock =
    Fecomms.open_unix_domain_sock_client (runtime_path ^ "/xapi/forker/main")
  in
  let stdinuuid = Uuidx.(to_string (make ())) in
  let stdoutuuid = Uuidx.(to_string (make ())) in
  let stderruuid = Uuidx.(to_string (make ())) in

  let fds_to_close = ref [] in

  let add_fd_to_close_list fd = fds_to_close := fd :: !fds_to_close in
  (* let remove_fd_from_close_list fd = fds_to_close := List.filter (fun fd' -> fd' <> fd) !fds_to_close in *)
  let close_fds () = List.iter (fun fd -> Unix.close fd) !fds_to_close in

  finally
    (fun () ->
      let maybe_add_id_to_fd_map id_to_fd_map (uuid, fd, v) =
        match v with
        | Some _ ->
            (uuid, fd) :: id_to_fd_map
        | None ->
            id_to_fd_map
      in

      let predefined_fds =
        [
          (stdinuuid, Some 0, stdin)
        ; (stdoutuuid, Some 1, stdout)
        ; (stderruuid, Some 2, stderr)
        ]
      in

      (* We don't care what fd these end up as - they're named in the argument list for us, and the
         forking executioner will sort it out. *)
      let dest_named_fds = List.map (fun (uuid, _) -> (uuid, None)) fds in
      let id_to_fd_map =
        List.fold_left maybe_add_id_to_fd_map dest_named_fds predefined_fds
      in

      let env = match env with Some e -> e | None -> default_path_env_pair in
      let syslog_stdout =
        match syslog_stdout with
        | NoSyslogging ->
            {Fe.enabled= false; Fe.key= None}
        | Syslog_DefaultKey ->
            {Fe.enabled= true; Fe.key= None}
        | Syslog_WithKey k ->
            {Fe.enabled= true; Fe.key= Some k}
      in
      Fecomms.write_raw_rpc sock
        (Fe.Setup
           {
             Fe.cmdargs= cmd :: args
           ; env= Array.to_list env
           ; id_to_fd_map
           ; syslog_stdout
           ; redirect_stderr_to_stdout
           }
        ) ;

      let response = Fecomms.read_raw_rpc sock in

      let s =
        match response with
        | Ok (Fe.Setup_response s) ->
            s
        | Ok status ->
            let msg =
              Printf.sprintf
                "%s: Received unexpected reply from forking executioner (%s)"
                __FUNCTION__
                (Fe.ferpc_to_string status)
            in
            failwith msg
        | Error err ->
            let msg =
              Printf.sprintf
                "%s: Received invalid reply from forking executioner (%s)"
                __FUNCTION__ err
            in
            failwith msg
      in

      let fd_sock = Fecomms.open_unix_domain_sock_client s.Fe.fd_sock_path in
      add_fd_to_close_list fd_sock ;

      let send_named_fd uuid fd = Fecomms.send_named_fd fd_sock uuid fd in

      List.iter
        (fun (uuid, _, srcfdo) ->
          match srcfdo with
          | Some srcfd ->
              send_named_fd uuid srcfd
          | None ->
              ()
        )
        predefined_fds ;
      List.iter (fun (uuid, srcfd) -> send_named_fd uuid srcfd) fds ;
      Fecomms.write_raw_rpc sock Fe.Exec ;
      match Fecomms.read_raw_rpc sock with
      | Ok (Fe.Execed pid) ->
          (sock, pid)
      | Ok status ->
          let msg =
            Printf.sprintf
              "%s: Received unexpected reply from forking executioner (%s)"
              __FUNCTION__
              (Fe.ferpc_to_string status)
          in
          failwith msg
      | Error err ->
          let msg =
            Printf.sprintf
              "%s: Received invalid reply from forking executioner (%s)"
              __FUNCTION__ err
          in
          failwith msg
    )
    close_fds

let execute_command_get_output_inner ?env ?stdin ?(syslog_stdout = NoSyslogging)
    ?(redirect_stderr_to_stdout = false) ?(timeout = -1.0) cmd args =
  let to_close = ref [] in
  let close fd =
    if List.mem fd !to_close then (
      Unix.close fd ;
      to_close := List.filter (fun x -> x <> fd) !to_close
    )
  in
  let stdinandpipes =
    Option.map
      (fun str ->
        let x, y = Unix.pipe () in
        to_close := x :: y :: !to_close ;
        (str, x, y)
      )
      stdin
  in
  finally
    (fun () ->
      match
        with_logfile_fd "execute_command_get_out" (fun out_fd ->
            with_logfile_fd "execute_command_get_err" (fun err_fd ->
                let sock, pid =
                  safe_close_and_exec ?env
                    (Option.map (fun (_, fd, _) -> fd) stdinandpipes)
                    (Some out_fd) (Some err_fd) [] ~syslog_stdout
                    ~redirect_stderr_to_stdout cmd args
                in
                Option.iter
                  (fun (str, _, wr) ->
                    Xapi_stdext_unix.Unixext.really_write_string wr str ;
                    close wr
                  )
                  stdinandpipes ;
                if timeout > 0. then
                  Unix.setsockopt_float sock Unix.SO_RCVTIMEO timeout ;
                try waitpid (sock, pid)
                with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
                  Unix.kill pid Sys.sigkill ;
                  ignore (waitpid (sock, pid)) ;
                  raise Subprocess_timeout
            )
        )
      with
      | Success (out, Success (err, (_pid, status))) -> (
        match status with
        | Unix.WEXITED 0 ->
            (out, err)
        | e ->
            raise (Spawn_internal_error (err, out, e))
      )
      | Success (_, Failure (_, exn)) | Failure (_, exn) ->
          raise exn
    )
    (fun () -> List.iter Unix.close !to_close)

let execute_command_get_output ?env ?(syslog_stdout = NoSyslogging)
    ?(redirect_stderr_to_stdout = false) ?timeout cmd args =
  execute_command_get_output_inner ?env ?stdin:None ?timeout ~syslog_stdout
    ~redirect_stderr_to_stdout cmd args

let execute_command_get_output_send_stdin ?env ?(syslog_stdout = NoSyslogging)
    ?(redirect_stderr_to_stdout = false) ?timeout cmd args stdin =
  execute_command_get_output_inner ?env ~stdin ~syslog_stdout
    ~redirect_stderr_to_stdout ?timeout cmd args
