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
open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_threads.Threadext
open Xapi_stdext_std.Xstringext
open Xapi_stdext_monadic
open Xapi_stdext_unix

module R = Debug.Make(struct let name = "redo_log" end)

(* --------------------------------------- *)
(* Functions relating to the redo log VDI. *)

let get_static_device reason =
  (* Specifically use Static_vdis_list rather than Static_vdis to avoid the
     	   cyclic dependency caused by reference to Server_helpers in Static_vdis *)
  let vdis = List.filter
      (fun x -> x.Static_vdis_list.reason = reason && x.Static_vdis_list.currently_attached)
      (Static_vdis_list.list ())
  in
  (* Return the path to the first attached VDI which matches the reason *)
  R.debug "Found %d VDIs matching [%s]" (List.length vdis) reason;
  match vdis with
  | [] -> None
  | hd :: _ -> hd.Static_vdis_list.path

(* Make sure we have plenty of room for the database *)
let minimum_vdi_size =
  let ( ** ) = Int64.mul in
  let mib = 1024L ** 1024L in
  256L ** mib

let redo_log_sm_config = [ "type", "raw" ]

(* ---------------------------------------------------- *)
(* Encapsulate the state of a single redo_log instance. *)

type redo_log = {
  name: string;
  marker: string;
  read_only: bool;
  enabled: bool ref;
  device: string option ref;
  currently_accessible: bool ref;
  state_change_callback: (bool -> unit) option;
  time_of_last_failure: float ref;
  backoff_delay: int ref;
  sock: Unix.file_descr option ref;
  pid: (Forkhelpers.pidty * string * string) option ref;
  dying_processes_mutex: Mutex.t;
  num_dying_processes: int ref;
}

module RedoLogSet = Set.Make(
  struct
    type t = redo_log
    let compare = fun log1 log2 -> compare log1.marker log2.marker
  end
  )

(* Keep a store of all redo_logs - this will make it easy to write to all the active ones. *)
let all_redo_logs = ref (RedoLogSet.empty)

(* ------------------------------------------------------------------------ *)
(* Functions relating to whether writing to the log is enabled or disabled. *)

let ready_to_write = ref true (* Controls whether DB writes are also copied to the redo log. *)

let is_enabled log = !(log.enabled)

let enable log vdi_reason =
  R.info "Enabling use of redo log";
  log.device := get_static_device vdi_reason;
  log.enabled := true

let enable_block log path =
  R.info "Enabling use of redo log";
  log.device := Some path;
  log.enabled := true

let disable log =
  R.info "Disabling use of redo log";
  log.device := None;
  log.enabled := false

(* ------------------------------------------------------------------------------------------------ *)
(* Functions relating to whether the latest attempt to read/write the redo-log succeeded or failed. *)

let redo_log_events = Event.new_channel ()

let cannot_connect_fn log =
  if !(log.currently_accessible) then begin
    R.debug "Signalling unable to access redo log";
    Event.sync (Event.send redo_log_events (log.name, false));
    Opt.iter (fun callback -> callback false) log.state_change_callback
  end;
  log.currently_accessible := false

let can_connect_fn log =
  if not !(log.currently_accessible) then begin
    R.debug "Signalling redo log is healthy";
    Event.sync (Event.send redo_log_events (log.name, true));
    Opt.iter (fun callback -> callback true) log.state_change_callback
  end;
  log.currently_accessible := true

(* ----------------------------------------------------------- *)
(* Functions relating to the serialisation of redo log entries *)

(* The type of a delta, describing an incremental change to the database. *)
type t =
  (* (tblname, newobjref, (k,v) list) *)
  | CreateRow of string * string * (string*string) list
  (* (tblname, objref) *)
  | DeleteRow of string * string
  (* (tblname, objref, fldname, newval) *)
  | WriteField of string * string * string * string

(* First 9 bytes of encoding of entries is an ASCII string indicating the kind of record, from {"CreateRow", "DeleteRow", "WriteFiel"} *)
(* Constituent strings are expressed as "<length><string>", where the length is specified in decimal using 8 ASCII digits *)

exception MalformedLogEntry of string

let kind_size = 9 (* number of bytes to indicate kind of record *)
let short_length_size = 8 (* number of bytes to encode a string's length *)

let redo_log_entry_to_string r =
  match r with
  | CreateRow(tbl, objref, kvs) ->
    Printf.sprintf "CreateRow%08d%s%08d%s%08d%s" (String.length tbl) tbl (String.length objref) objref (List.length kvs)
      (String.concat ""
         (List.map (fun (k,v) -> Printf.sprintf "%08d%s%08d%s" (String.length k) k (String.length v) v) kvs)
      )
  | DeleteRow(tbl, objref) ->
    Printf.sprintf "DeleteRow%08d%s%08d%s" (String.length tbl) tbl (String.length objref) objref
  | WriteField(tblname, objref, fldname, newval) ->
    Printf.sprintf "WriteFiel%08d%s%08d%s%08d%s%08d%s" (String.length tblname) tblname (String.length objref) objref (String.length fldname) fldname (String.length newval) newval

let parse_string str len pos =
  let str = String.sub str !pos len in
  pos := !pos + len;
  str

let parse_num str pos =
  let len_str = parse_string str short_length_size pos in
  let len = (try int_of_string len_str with _ -> 0) in
  len

let parse_length_and_string str pos =
  let len = parse_num str pos in
  parse_string str len pos

let string_to_redo_log_entry str =
  let pos = ref 0 in
  let kind = parse_string str kind_size pos in
  match kind with
  | "CreateRow" ->
    let tbl = parse_length_and_string str pos in
    let objref = parse_length_and_string str pos in
    let num_kvs = parse_num str pos in
    (* Parse key-value pairs *)
    let parse_kvs n =
      let rec aux acc = function
        | 0 -> acc
        | n ->
          let k = parse_length_and_string str pos in
          let v = parse_length_and_string str pos in
          aux ((k,v)::acc) (n-1)
      in aux [] n
    in
    let kvs = parse_kvs num_kvs in
    CreateRow (tbl, objref, kvs)

  | "DeleteRow" ->
    let tbl = parse_length_and_string str pos in
    let objref = parse_length_and_string str pos in
    DeleteRow (tbl, objref)

  | "WriteFiel" ->
    let tbl = parse_length_and_string str pos in
    let objref = parse_length_and_string str pos in
    let fldname = parse_length_and_string str pos in
    let newval = parse_length_and_string str pos in
    WriteField (tbl, objref, fldname, newval)

  | s -> (* unrecognised log entry *)
    raise (MalformedLogEntry s)


(* ---------------------------------------------------------------------- *)
(* Functions relating to communication with the block device I/O process. *)

exception RedoLogFailure of string
exception CommunicationsProblem of string

let generation_size = 16
let length_size = 16

let get_latest_response_time block_time =
  let now = Unix.gettimeofday() in
  now +. block_time

(* Returns the PID of the process *)
let start_io_process block_dev ctrlsockpath datasockpath =
  (* Execute the process *)
  let args = ["-device"; block_dev; "-ctrlsock"; ctrlsockpath; "-datasock"; datasockpath] in
  Forkhelpers.safe_close_and_exec None None None [] !Db_globs.redo_log_block_device_io args

let connect sockpath latest_response_time =
  let rec attempt () =
    let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    try
      Unix.connect s (Unix.ADDR_UNIX sockpath);
      R.debug "Connected to I/O process via socket %s" sockpath;
      s
    with Unix.Unix_error(a,b,c) ->
      (* It's probably the case that the process hasn't started yet. *)
      (* See if we can afford to wait and try again *)
      Unix.close s;
      let attempt_delay = !Db_globs.redo_log_connect_delay in
      let now = Unix.gettimeofday() in
      let remaining = latest_response_time -. now in
      if attempt_delay < remaining then begin
        (* Wait for a while then try again *)
        R.debug "Waiting to connect to I/O process via socket %s (error was %s: %s)..." sockpath b (Unix.error_message a);
        Thread.delay attempt_delay;
        attempt ()
      end else
        raise Unixext.Timeout
  in
  attempt ()

let read_separator sock latest_response_time =
  let sep = Unixext.time_limited_read sock 1 latest_response_time in
  if sep <> "|" then raise (CommunicationsProblem "expected separator") else ()

let read_generation_count sock latest_response_time =
  read_separator sock latest_response_time;
  let gen_count = Unixext.time_limited_read sock generation_size latest_response_time in
  Generation.of_string gen_count

let read_length sock latest_response_time =
  read_separator sock latest_response_time;
  let length_str = Unixext.time_limited_read sock length_size latest_response_time in
  let length = (try int_of_string length_str with _ -> 0) in
  length

let read_length_and_string sock latest_response_time =
  let length = read_length sock latest_response_time in
  read_separator sock latest_response_time;
  let str = Unixext.time_limited_read sock length latest_response_time in
  str

let read_database f gen_count sock latest_response_time datasockpath =
  R.debug "Reading database with generation count %s" (Generation.to_string gen_count);
  let expected_length = read_length sock latest_response_time in
  R.debug "Expecting to receive database of length %d" expected_length;

  (* Connect to the data socket *)
  let datasock = connect datasockpath latest_response_time in
  R.debug "Connected to data socket";

  finally
    (fun () ->
       (* Pass the gen_count and the socket's fd to f. f may raise Unixext.Timeout if it cannot complete before latest_response_time. *)
       f gen_count datasock expected_length latest_response_time;
    )
    (fun () ->
       (* Close the data socket *)
       R.debug "Closing the data socket";
       Unix.close datasock
    )

let read_delta f gen_count sock latest_response_time =
  R.debug "Reading delta with generation count %s" (Generation.to_string gen_count);
  let str = read_length_and_string sock latest_response_time in
  let entry = string_to_redo_log_entry str in
  f gen_count entry

let rec read_read_response sock fn_db fn_delta expected_gen_count latest_response_time datasockpath =
  let hdr = Unixext.time_limited_read sock 5 latest_response_time in
  if hdr <> "read|" then raise (CommunicationsProblem "expected 'read|'");

  let kind = Unixext.time_limited_read sock 5 latest_response_time in
  match kind with
  | "db___" ->
    let gen_count = read_generation_count sock latest_response_time in
    read_database fn_db gen_count sock latest_response_time datasockpath;
    read_read_response sock fn_db fn_delta (Generation.add_int gen_count 1) latest_response_time datasockpath
  | "delta" ->
    let gen_count = read_generation_count sock latest_response_time in
    (* Since Boston all deltas cause a bump in generation count, whether or not they are persisted to the redo_log. *)
    (* For safety, we should only read a delta if the generation count is higher than that of the last delta read. *)
    if gen_count < expected_gen_count then begin
      R.debug "Found record with generation count %Ld. Expected a record with generation count of at least %Ld so skipping this record." gen_count expected_gen_count;
      (* Now skip over all the remaining data that the process is trying to send, discarding it all *)
      read_delta (fun _ _ -> ()) gen_count sock latest_response_time;
      read_read_response sock fn_db fn_delta expected_gen_count latest_response_time datasockpath
    end else begin
      R.debug "Found record with generation count %Ld which is >= expected generation count %Ld" gen_count expected_gen_count;
      read_delta fn_delta gen_count sock latest_response_time;
      read_read_response sock fn_db fn_delta (Generation.add_int gen_count 1) latest_response_time datasockpath
    end
  | "end__" -> R.debug "Reached the end of the read response"; ()
  | "nack_" ->
    (* Read the error message *)
    let error = read_length_and_string sock latest_response_time in
    R.warn "Read error received: [%s]" error;
    if error = Block_device_io_errors.timeout_error_msg then raise Unixext.Timeout
    else raise (RedoLogFailure error)
  | e -> raise (CommunicationsProblem ("unrecognised read response prefix ["^e^"]"))

let action_empty sock datasockpath =
  R.debug "Performing empty";
  (* Compute desired response time *)
  let latest_response_time = get_latest_response_time !Db_globs.redo_log_max_block_time_empty in
  (* Empty *)
  let str = Bytes.of_string "empty_____" in
  Unixext.time_limited_write sock (Bytes.length str) str latest_response_time;
  (* Read response *)
  let response_length = 10 in
  let response = Unixext.time_limited_read sock response_length latest_response_time in
  match response with
  | "empty|ack_" -> ()
  | "empty|nack" ->
    (* Read the error message *)
    let error = read_length_and_string sock latest_response_time in
    R.warn "Emptying was unsuccessful: [%s]" error;
    if error = Block_device_io_errors.timeout_error_msg then raise Unixext.Timeout
    else raise (RedoLogFailure error)
  | e -> raise (CommunicationsProblem ("unrecognised empty response ["^e^"]"))

let action_read fn_db fn_delta sock datasockpath =
  R.debug "Performing read";
  (* Compute desired response time *)
  let latest_response_time = get_latest_response_time !Db_globs.redo_log_max_block_time_read in
  (* Write *)
  let str = Bytes.of_string "read______" in
  Unixext.time_limited_write sock (Bytes.length str) str latest_response_time;
  (* Read response *)
  read_read_response sock fn_db fn_delta Generation.null_generation latest_response_time datasockpath

let action_write_db marker generation_count write_fn sock datasockpath =
  R.debug "Performing writedb (generation %Ld)" generation_count;
  (* Compute desired response time *)
  let latest_response_time = get_latest_response_time !Db_globs.redo_log_max_block_time_writedb in
  (* Send write command down control channel *)
  let str =
    Printf.sprintf "writedb___|%s|%016Ld" marker generation_count
    |> Bytes.of_string
  in
  Unixext.time_limited_write sock (Bytes.length str) str latest_response_time;

  (*
   * Connect to the data socket. Note that this may delay a bit before being
   * able to connect, as we might need to wait for the I/O process to start
   * listening on the socket.
   *)
  let datasock = connect datasockpath latest_response_time in

  finally
    (fun () ->
       (* Send data straight down the data channel, then close it to send an EOF. *)
       (* Ideally, we would check whether this completes before the latest_response_time. Could implement this by performing the write in a separate thread. *)

       try
         write_fn datasock;
         R.debug "Finished writing database to data socket";
       with
       | Sys_error("Connection reset by peer") ->
         (* CA-41914: Note that if the block_device_io process internally
          * throws Timeout (or indeed any other exception), it will forcibly
          * close this connection, we'll see a Sys_error("Connection reset by
          * peer"). This can be safely suppressed because we'll hear all the
          * gory details in the response we read over the control socket. *)
         R.warn "I/O process forcibly closed the data socket while trying to write database to it. Await the response to see why it did that.";
       | e ->
         (* We'll re-raise other exceptions, though. *)
         R.error "Got an unexpected exception while trying to write database to the data socket: %s. Re-raising." (Printexc.to_string e);
         raise e
    )
    (fun () ->
       (* Ensure the data socket is closed even if exception is thrown from write_fn *)
       R.info "Closing data socket";
       Unix.close datasock;
    );

  (* Read response *)
  let response_length = 12 in
  R.debug "Reading response...";
  let response = Unixext.time_limited_read sock response_length latest_response_time in
  R.debug "Got response [%s]" response;
  match response with
  | "writedb|ack_" -> ()
  | "writedb|nack" ->
    (* Read the error message *)
    let error = read_length_and_string sock latest_response_time in
    R.warn "Write was unsuccessful: [%s]" error;
    if error = Block_device_io_errors.timeout_error_msg then raise Unixext.Timeout
    else raise (RedoLogFailure error)
  | e -> raise (CommunicationsProblem ("unrecognised writedb response ["^e^"]"))

let action_write_delta marker generation_count data flush_db_fn sock datasockpath =
  R.debug "Performing writedelta (generation %Ld)" generation_count;
  (* Compute desired response time *)
  let latest_response_time = get_latest_response_time !Db_globs.redo_log_max_block_time_writedelta in
  (* Write *)
  let str =
    Printf.sprintf "writedelta|%s|%016Ld|%016d|%s" marker generation_count (String.length data) data
    |> Bytes.of_string
  in
  Unixext.time_limited_write sock (Bytes.length str) str latest_response_time;
  (* Read response *)
  let response_length = 15 in
  let response = Unixext.time_limited_read sock response_length latest_response_time in
  match response with
  | "writedelta|ack_" -> R.debug "Write was successful"; ()
  | "writedelta|nack" ->
    (* Read the error message *)
    let error = read_length_and_string sock latest_response_time in
    R.warn "Write was unsuccessful: [%s]" error;
    if error = Block_device_io_errors.timeout_error_msg then
      raise Unixext.Timeout (* Propagate the timeout exception *)
    else if error = Block_device_io_errors.not_enough_space_error_msg then begin
      R.info "Not enough space on block device, so attempting to flush the DB...";
      flush_db_fn() (* There wasn't enough space to write the delta, so flush the current database instead, which will free up space. *)
    end else
      raise (RedoLogFailure error) (* Some other error *)
  | e -> R.warn "Received unexpected response"; raise (CommunicationsProblem ("unrecognised writedelta response ["^e^"]"))


(* ----------------------------------------------------------------------------------------------- *)
(* Functions relating to the exponential back-off of repeated attempts to reconnect after failure. *)

let initialise_backoff_delay log =
  log.backoff_delay := Db_globs.redo_log_initial_backoff_delay

let increase_backoff_delay log =
  if !(log.backoff_delay) = 0 then initialise_backoff_delay log
  else log.backoff_delay := !(log.backoff_delay) * Db_globs.redo_log_exponentiation_base;
  if !(log.backoff_delay) > Db_globs.redo_log_maximum_backoff_delay then
    log.backoff_delay := Db_globs.redo_log_maximum_backoff_delay;
  R.debug "Bumped backoff delay to %d seconds" !(log.backoff_delay)

let set_time_of_last_failure log =
  let now = Unix.gettimeofday() in
  log.time_of_last_failure := now;
  increase_backoff_delay log

let reset_time_of_last_failure log =
  log.time_of_last_failure := 0. ;
  initialise_backoff_delay log

let maybe_retry f log =
  let now = Unix.gettimeofday() in
  R.debug "Considering whether to attempt to flush the DB... (backoff %d secs, time since last failure %.1f secs)"
    !(log.backoff_delay) (now -. !(log.time_of_last_failure));
  if now -. !(log.time_of_last_failure) >= float_of_int !(log.backoff_delay) then begin
    R.debug "It's time for an attempt to reconnect and flush the DB.";
    f()
  end else
    R.debug "No; we'll wait a bit longer before trying again."


(* -------------------------------------------------------------------- *)
(* Functions relating to the lifecycle of the block device I/O process. *)

(* Close any existing socket and kill the corresponding process. *)
let shutdown log =
  if is_enabled log then begin
    R.debug "Shutting down connection to I/O process for '%s'" log.name;
    try
      begin
        match !(log.pid) with
        | None -> ()
        | Some (p, ctrlsockpath, datasockpath) ->
          (* If there's an existing socket, close it. *)
          begin
            match !(log.sock) with
            | None -> ()
            | Some s ->
              ignore_exn (fun () -> Unix.close s);
              (* Now we can forget about the communication channel to the process *)
              log.sock := None;
          end;

          (* Terminate the child process *)
          let ipid = Forkhelpers.getpid p in
          R.info "Killing I/O process with pid %d" ipid;
          Unix.kill ipid Sys.sigkill;
          (* Wait for the process to die. This is done in a separate thread in case it does not respond to the signal immediately. *)
          ignore (Thread.create (fun () ->
              R.debug "Waiting for I/O process with pid %d to die..." ipid;
              Mutex.execute log.dying_processes_mutex
                (fun () -> log.num_dying_processes := !(log.num_dying_processes) + 1);
              ignore(Forkhelpers.waitpid p);
              R.debug "Finished waiting for process with pid %d" ipid;
              Mutex.execute log.dying_processes_mutex
                (fun () -> log.num_dying_processes := !(log.num_dying_processes) - 1)
            ) ());
          (* Forget about that process *)
          log.pid := None;

          (* Attempt to remove the sockets *)
          List.iter (fun sockpath ->
              R.debug "Removing socket %s" sockpath;
              Unixext.unlink_safe sockpath
            ) [ctrlsockpath; datasockpath]
      end;
    with e -> R.error "Caught %s: while shutting down connection to I/O process" (Printexc.to_string e); () (* ignore any errors *)
  end

let broken log =
  set_time_of_last_failure log;
  shutdown log;
  cannot_connect_fn log

let healthy log =
  reset_time_of_last_failure log;
  can_connect_fn log

exception TooManyProcesses

let startup log =
  if is_enabled log then
    try
      begin
        match !(log.pid) with
        | Some _ -> () (* We're already started *)
        | None ->
          begin
            (* Don't start if there are already some processes hanging around *)
            Mutex.execute log.dying_processes_mutex
              (fun () -> if !(log.num_dying_processes) >= Db_globs.redo_log_max_dying_processes then raise TooManyProcesses);

            match !(log.device) with
            | None ->
              R.info "Could not find block device"
            | Some device ->
              begin
                R.info "Using block device at %s" device;

                (* Check that the block device exists *)
                Unix.access device [Unix.F_OK; Unix.R_OK];
                (* will throw Unix.Unix_error if not readable *)

                (* Start the I/O process *)
                let ctrlsockpath, datasockpath =
                  let f suffix = Filename.temp_file Db_globs.redo_log_comms_socket_stem suffix in
                  f "ctrl", f "data" in
                R.info "Starting I/O process with block device [%s], control socket [%s] and data socket [%s]" device ctrlsockpath datasockpath;
                let p = start_io_process device ctrlsockpath datasockpath in

                log.pid := Some (p, ctrlsockpath, datasockpath);
                R.info "Block device I/O process has PID [%d]" (Forkhelpers.getpid p)
              end
          end
      end;
      match !(log.pid) with
      | Some (_, ctrlsockpath, _) ->
        begin
          match !(log.sock) with
          | Some _ -> () (* We're already connected *)
          | None ->
            let latest_connect_time = get_latest_response_time !Db_globs.redo_log_max_startup_time in

            (* Now connect to the process via the socket *)
            let s = connect ctrlsockpath latest_connect_time in
            finally
              (fun () ->
                 try
                   begin
                     (* Check that we connected okay by reading the startup message *)
                     let response_length = 12 in
                     let response = Unixext.time_limited_read s response_length latest_connect_time in
                     match response with
                     | "connect|ack_" ->
                       R.info "Connect was successful";
                       (* Save the socket. This defers the responsibility for closing it to shutdown(). *)
                       log.sock := Some s
                     | "connect|nack" ->
                       (* Read the error message *)
                       let error = read_length_and_string s latest_connect_time in
                       R.warn "Connect was unsuccessful: [%s]" error;
                       broken log;
                     | e ->
                       R.warn "Received unexpected connect response: [%s]" e;
                       broken log
                   end
                 with Unixext.Timeout -> R.warn "Timed out waiting to connect"; broken log
              )
              (fun () ->
                 (* If the socket s has been opened, but sock hasn't been set then close it here. *)
                 match !(log.sock) with
                 | Some _ -> ()
                 | None -> ignore_exn (fun () -> Unix.close s)
              )
        end
      | None -> () (* don't attempt to connect *)
    with TooManyProcesses ->
      R.info "Too many dying I/O processes. Not starting another one.";
      cannot_connect_fn log

let switch log vdi_reason =
  shutdown log;
  log.device := get_static_device vdi_reason;
  startup log

(* Given a socket, execute a function and catch exceptions. *)
let perform_action f desc sock log =
  try
    match !(log.pid) with
    | None -> ()
    | Some (_, _, datasockpath) ->
      R.debug "About to perform action %s" desc;
      Stats.time_this ("redo_log: " ^ desc) (fun () ->
        f sock datasockpath);
      R.debug "Action '%s' completed successfully" desc;
      healthy log (* no exceptions: we can be confident that the redo log is working healthily *)
  with
  | Unixext.Timeout ->
    (* Timeout: try to close the connection to the redo log. it will be re-opened when we next attempt another access *)
    R.warn "Could not %s: Timeout." desc;
    broken log
  | Unix.Unix_error(a,b,c) ->
    (* problem with process I/O *)
    R.warn "Could not %s: Unix error on %s: %s" desc b (Unix.error_message a);
    broken log
  | RedoLogFailure e ->
    (* error received from block_device_io *)
    R.warn "Could not %s: received error %s" desc e;
    broken log
  | CommunicationsProblem str ->
    (* unexpected response received from block_device_io *)
    R.warn "Could not %s: communications problem: %s" desc str;
    broken log
  | e ->
    (* other exception *)
    R.warn "Could not %s: unexpected exception %s" desc (Printexc.to_string e);
    broken log

(* Attempt to connect to the block device I/O process. If successful, execute the function. *)
let connect_and_do f log =
  if !(log.pid) = None then startup log; (* try to connect if not already connected *)
  match !(log.sock) with
  | None -> () (* do nothing *)
  | Some sock -> f sock log (* execute the function, passing the socket and the redo_log state *)

let connect_and_perform_action f desc log =
  connect_and_do (perform_action f desc) log

(* ------------------------------------------------------------------- *)
(* Functions for handling creation and deletion of redo log instances. *)

let redo_log_creation_mutex = Mutex.create ()

let create ~name ~state_change_callback ~read_only =
  let instance = {
    name = name;
    marker = Uuid.to_string (Uuid.make_uuid ());
    read_only = read_only;
    enabled = ref false;
    device = ref None;
    currently_accessible = ref true;
    state_change_callback = state_change_callback;
    time_of_last_failure = ref 0.;
    backoff_delay = ref Db_globs.redo_log_initial_backoff_delay;
    sock = ref None;
    pid = ref None;
    dying_processes_mutex = Mutex.create ();
    num_dying_processes = ref 0;
  } in
  Mutex.execute redo_log_creation_mutex
    (fun () -> all_redo_logs := RedoLogSet.add instance !all_redo_logs);
  instance

let delete log =
  shutdown log;
  disable log;
  Mutex.execute redo_log_creation_mutex
    (fun () -> all_redo_logs := RedoLogSet.remove log !all_redo_logs)

(* -------------------------------------------------------- *)
(* Helper functions for interacting with multiple redo_logs *)
let with_active_redo_logs f =
  Mutex.execute redo_log_creation_mutex
    (fun () ->
       let active_redo_logs =
         RedoLogSet.filter (fun log -> (is_enabled log) && not(log.read_only)) !(all_redo_logs)
       in
       RedoLogSet.iter f active_redo_logs)

(* --------------------------------------------------------------- *)
(* Functions which interact with the redo log on the block device. *)

let write_db generation_count write_fn log =
  if is_enabled log then
    let f () = connect_and_perform_action (action_write_db log.marker generation_count write_fn) "write database to redo log" log in

    if !(log.sock) = None then begin
      (* We're not currently connected. See if it's time to attempt to reconnect *)
      R.debug "We're not currently connected to the block device I/O process.";
      maybe_retry f log
    end else begin
      (* it looks like everything's healthy *)
      R.debug "We believe that we are currently connected to a healthy block device. Attempting to write DB...";
      f ()
    end

let write_delta generation_count t flush_db_fn log =
  if (is_enabled log) && !ready_to_write then begin
    (* If we're not currently connected, then try to re-connect (by calling flush_db_fn) at increasing time intervals. *)
    match !(log.sock) with
    | None ->
      (* Instead of writing a delta, try to write the whole DB *)
      R.debug "write_delta: Not currently connected, so trying to re-connect and flush DB instead of writing the delta";
      Stats.time_this "redo_log: flush_db" flush_db_fn
    | Some sock ->
      (* It looks like we're probably connected, so try to write the delta *)
      let str = redo_log_entry_to_string t in
      perform_action (action_write_delta log.marker generation_count str flush_db_fn) "write delta to redo log" sock log
  end

let apply fn_db fn_delta log =
  if is_enabled log then begin
    (* Turn off writing to the database while we are applying deltas. *)
    ready_to_write := false;
    finally
      (fun () -> connect_and_perform_action (action_read fn_db fn_delta) "read from redo log" log)
      (fun () -> ready_to_write := true)
  end

let empty log =
  if is_enabled log then
    connect_and_perform_action (action_empty) "invalidate the redo log" log

(** ------------------------------------------------ *)
(** Functions which operate on all active redo_logs. *)

(* Flush the database to the given redo_log instance. *)
let flush_db_to_redo_log db log =
  R.info "Flushing database to redo_log [%s]" log.name;
  let write_db_to_fd = (fun out_fd -> Db_xml.To.fd out_fd db) in
  write_db (Db_cache_types.Manifest.generation (Db_cache_types.Database.manifest db)) write_db_to_fd log;
  !(log.currently_accessible)

(* Write the given database to all active redo_logs *)
let flush_db_to_all_active_redo_logs db =
  R.info "Flushing database to all active redo-logs";
  with_active_redo_logs (fun log ->
      ignore(flush_db_to_redo_log db log))

(* Write a delta to all active redo_logs *)
let database_callback event db =
  let to_write =
    match event with
    | Db_cache_types.RefreshRow (tblname, objref) ->
      None
    | Db_cache_types.WriteField (tblname, objref, fldname, oldval, newval) ->
      R.debug "WriteField(%s, %s, %s, %s, %s)" tblname objref fldname (Schema.Value.marshal oldval) (Schema.Value.marshal newval);
      if Schema.is_field_persistent (Db_cache_types.Database.schema db) tblname fldname
      then Some (WriteField(tblname, objref, fldname, Schema.Value.marshal newval))
      else None
    | Db_cache_types.PreDelete (tblname, objref) ->
      None
    | Db_cache_types.Delete (tblname, objref, _) ->
      if Schema.is_table_persistent (Db_cache_types.Database.schema db) tblname
      then Some (DeleteRow(tblname, objref))
      else None
    | Db_cache_types.Create (tblname, objref, kvs) ->
      if Schema.is_table_persistent (Db_cache_types.Database.schema db) tblname
      then Some (CreateRow(tblname, objref, (List.map (fun (k, v) -> k, Schema.Value.marshal v) kvs)))
      else None
  in

  Opt.iter (fun entry ->
      with_active_redo_logs (fun log ->
          write_delta (Db_cache_types.Manifest.generation (Db_cache_types.Database.manifest db)) entry
            (fun () ->
               (* the function which will be invoked if a database write is required instead of a delta *)
               ignore(flush_db_to_redo_log db log))
            log
        )
    ) to_write
