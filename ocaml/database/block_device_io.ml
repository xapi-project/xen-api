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
(*
 * Code to store a database and deltas in a block device and retrieve it again later.
 *
 * This module can be compiled and executed in a stand-alone fashion from the command-line.
 * The filename of the block device and the filename of a file to use as a Unix domain socket are provided as command-line parameters.
 * The process must have read- and write-permission to the block device.
 *)

(*
 * Command-line parameters
 * -----------------------
 *
 * -device		[mandatory] Gives the path to the block device.
 * -ctrlsock <file>	Requests that a file is used as a Unix domain socket to listen on for read/write commands.
 * -datasock <file>	Requests that a file is used as a Unix domain socket to use to transfer database contents.
 * -empty       	Requests that the block device is emptied and re-initialised.
 * -dump		Requests that the contents of the block device are written to STDOUT.
 *
 * The -device option is mandatory. Options -ctrlsock and -datasock must be specified together; these are mutually exclusive with either -empty or -dump.
 *)

(*
 * On-disk structure
 * -----------------
 *
 * The database is double-buffered, so that in case of corruption or write errors, there will always be an intact version of the data preserved.
 * There is a "validity byte" indicating which buffer is currently being written to.
 * Each buffer starts with a database record followed by zero or more deltas.
 *
 * A database record consists of:
 *
 * bytes | description
 * ------+-----------------------------------
 *    36 | marker
 *    16 | length of database (decimal ASCII)
 * <len> | database (binary data)
 *    36 | marker
 *
 * The two markers in a valid database record are equal.
 *
 * A delta record consists of:
 *
 * bytes | description
 * ------+-----------------------------------
 *    16 | length of delta (decimal ASCII)
 * <len> | delta (binary data)
 *    36 | marker
 *
 * The marker for each delta will be equal to the marker for the database they correspond to.
 *)

(*
 * Communications protocol
 * -----------------------
 *
 * The control socket is used for another process to send commands and receive responses.
 * The response time for command is guaranteed to be no greater than a particular maximum delay (not accounting for network delay between the processes).
 * If the command could not complete in the available time, the response indicates that it failed.
 *
 * The four commands are:
 *
 * 1. command:   "writedb___|<marker>|<length>|<data>"
 *    response:  "writedb|ack_"  for successful write; or
 *               "writedb|nack"  otherwise
 *
 * 2. command:   "writedelta|<marker>|<length>|<data>"
 *    response:  "writedelta|ack_"  for successful write; or
 *               "writedelta|nack"  otherwise
 *
 * 3. command:   "read______"
 *    response:  "read|db___|<length>|<data>"  then zero or more  "read|delta|<length>|<data>"  and finally  "read|end__"
 *
 * 4. command:   "empty_____"
 *    response:  "empty|ack_"  for successful invalidation; or
 *               "empty|nack"  otherwise
 *)

open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_unix

let name = "block_device_io"
module R = Debug.Make(struct let name = name end)

let magic = "redo-log--------" (* 16 bytes *)

(* Lengths of things on the socket comms protocol and in the redo-log *)
let magic_size = 16
let generation_size = 16
let marker_size = String.length (Uuid.to_string (Uuid.make_uuid ()))
let size_size = 16 (* decimal digits *)
let command_size = 10 (* "write_delta" or "write_db___" or "read______" or "empty_____" *)

(* Layout of block device *)
let pos_validity_byte = 17
let pos_first_half = 18
let pos_second_half = pos_first_half + Db_globs.redo_log_length_of_half

type half = Neither | First | Second

(* Cache of pointer into each half. If set, this is the position to which the next delta will be written. These values are absolute positions relative to the start of the block device. *)
let pointer_first_half : int option ref = ref None
let pointer_second_half : int option ref = ref None

exception InvalidBlockDevice
exception NonMatchingMarkers of string*string
exception EndOfDeltas
exception NotEnoughSpace

(* Make informational output go to the syslog *)
let initialise_logging () =
  Debug.set_facility Syslog.Local5;
  Debug.disable ~level:Syslog.Debug name

(* --------------------------------------------- *)
(* Functions to deal with layout of block device *)

let start_of_half half =
  match half with
  | Neither -> raise InvalidBlockDevice (* start_of_half should never be called on "Neither" *)
  | First -> pos_first_half
  | Second -> pos_second_half

let half_to_pointer half =
  let ptr = match half with
    | Neither -> raise InvalidBlockDevice (* half_to_pointer should never be called on "Neither" *)
    | First -> pointer_first_half
    | Second -> pointer_second_half in
  ptr

let half_to_string half =
  match half with
  | Neither -> "0"
  | First -> "1"
  | Second -> "2"

let set_pointer half pos =
  let ptr = half_to_pointer half in
  ptr := Some pos

let get_pointer half =
  let ptr = half_to_pointer half in
  !ptr



(* ----------------------------------------------------------- *)
(* Helper functions to read and write to/from the block device *)

(* Lay out a blank double-buffered redo log on the given block device. *)
(* May raise Unixext.Timeout exception *)
let initialise_redo_log block_dev_fd target_response_time =
  ignore_int (Unixext.seek_to block_dev_fd 0);
  Unixext.time_limited_write block_dev_fd magic_size (Bytes.unsafe_of_string magic) target_response_time;
  Unixext.time_limited_write block_dev_fd 2 (Bytes.unsafe_of_string "\0000") target_response_time (* write the NUL and set the initial validity byte to 0 *)

(* Check that the given filename refers to a valid redo-log block device. Returns a read/write file descriptor. *)
(* May raise exceptions Unixext.Timeout or Unix.Unix_error *)
let open_block_device block_dev target_response_time =
  (* Check that the block device exists and is writeable *)
  R.debug "Checking block device...";
  begin
    try
      Unix.access block_dev [Unix.F_OK; Unix.W_OK; Unix.R_OK]
    with _ as e -> R.error "Block device %s does not exist or is unwriteable" block_dev; raise e
  end;
  R.debug "Opening block device for read/write";
  (* O_DSYNC ensures that calls to Unix.write do not return until the data has been flushed to disk. *)
  let block_dev_fd = Unix.openfile block_dev [Unix.O_RDWR; Unix.O_DSYNC; Unix.O_NONBLOCK] 0o755 in
  R.debug "Block device opened";
  (* Check that it's a redo-log block device *)
  let magic' = (try Unixext.time_limited_read block_dev_fd 16 target_response_time with End_of_file -> "") in
  if magic' <> magic then begin
    R.warn "Magic string not matched. Initialising redo log...";
    initialise_redo_log block_dev_fd target_response_time
  end;
  block_dev_fd

(* Within the given block device, seek to the position of the validity byte. *)
let seek_to_validity_byte block_dev_fd =
  ignore_int (Unixext.seek_to block_dev_fd pos_validity_byte)

(* Read the validity byte from the given block device. *)
let read_validity_byte block_dev_fd target_response_time =
  seek_to_validity_byte block_dev_fd;
  let validity = Unixext.time_limited_read block_dev_fd 1 target_response_time in
  validity

(* Set the validity byte in the given block device to indicate the given half is active. *)
let set_validity_byte block_dev_fd half target_response_time =
  seek_to_validity_byte block_dev_fd;
  let validity = half_to_string half in
  Unixext.time_limited_write block_dev_fd 1 (Bytes.unsafe_of_string validity) target_response_time

(* Seeks to, and returns, the position just after the last db or delta record in the given half. *)
let seek_past_last_record block_dev_fd half target_response_time =
  (* (We could use read_database and read_deltas instead of this implementation) *)
  (* Seek to the start of the half *)
  let cursor = ref (Unixext.seek_to block_dev_fd (start_of_half half)) in
  let stop = ref false in
  while not !stop do
    (* Seek past the first marker *)
    cursor := Unixext.seek_rel block_dev_fd marker_size;
    (* Read the next 16 bytes: the length of this record *)
    let len_str = Unixext.time_limited_read block_dev_fd size_size target_response_time in
    let len = (try int_of_string len_str with _ -> 0) in
    if len = 0 then begin
      (* The "length" was zero or it contained a non-ASCII decimal integer. Rewind back past it. *)
      cursor := Unixext.seek_rel block_dev_fd (-size_size);
      stop := true
    end
    else
      (* It was a valid length; seek past the record and the following generation count *)
      cursor := Unixext.seek_rel block_dev_fd (len + generation_size)
  done;
  !cursor

(* Return the length, the db and the marker. Assumes the cursor is pointing to the start of a db record. *)
(* Checks that the marker at the start of the record matches the marker at the end and raises NonMatchingMarkers if not. *)
(* val read_database : Unix.file_descr -> float -> (int * (Unix.file_descr -> unit) * Generation.t * string) *)
let read_database block_dev_fd target_response_time =
  let read len = Unixext.time_limited_read block_dev_fd len target_response_time in
  let marker_start = read marker_size in
  let len_str = read size_size in
  let len = (try int_of_string len_str with _ -> 0) in
  (* See if there is a valid db there *)
  if len = 0 then raise InvalidBlockDevice;

  (* Create a function which will read the database and pass it to a given fd *)
  let cur_pos = Unixext.current_cursor_pos block_dev_fd in
  let db_fn f =
    let prev_pos = Unixext.current_cursor_pos block_dev_fd in
    (* Seek to the position of the database *)
    ignore_int (Unixext.seek_to block_dev_fd cur_pos);
    (* Read 'len' bytes from the block device and send them to the function we were given *)
    ignore_int (Unixext.read_data_in_chunks f ~max_bytes:len block_dev_fd);
    (* Seek back to where we were before *)
    ignore_int (Unixext.seek_to block_dev_fd prev_pos)
  in

  (* For now, skip over where the database is *)
  ignore_int (Unixext.seek_rel block_dev_fd len);

  (* Read the generation count and marker *)
  let generation_count = Int64.of_string (read generation_size) in
  let marker_end = read marker_size in
  if marker_start <> marker_end then raise (NonMatchingMarkers(marker_start, marker_end))
  else (len, db_fn, generation_count, marker_start)

(* Return the length, the db and the marker. Assumes the cursor is pointing to the start of a delta record. *)
(* If there is no record here, this function raises EndOfDeltas. *)
let read_delta block_dev_fd target_response_time =
  let read len = Unixext.time_limited_read block_dev_fd len target_response_time in
  let len_str = read size_size in
  let len = (try int_of_string len_str with _ -> 0) in
  (* See if it's a valid delta *)
  if len = 0 then raise EndOfDeltas;
  (* Otherwise, it seems valid so read it *)
  let delta = read len in
  let generation_count = Int64.of_string (read generation_size) in
  let marker = read marker_size in
  (len, delta, generation_count, marker)




(* -------------------------------------------- *)
(* Functions to help with socket communications *)

(* Returns a server socket listening on the specified socket file *)
let listen_on sock =
  let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (* Remove any existing socket file *)
  begin try
      Unix.unlink sock
    with Unix.Unix_error _ -> ()
  end;
  Unix.bind s (Unix.ADDR_UNIX sock);
  Unix.listen s 1; (* 1 = maximum number of pending requests *)
  s

let accept_conn s latest_response_time =
  let now = Unix.gettimeofday() in
  let timeout = latest_response_time -. now in
  (* Await an incoming connection... *)
  let ready_to_read, _, _ = Unix.select [s] [] [] timeout in
  R.debug "Finished selecting";
  if List.mem s ready_to_read then
    (* We've received a connection. Accept it and return the socket. *)
    fst (Unix.accept s)
  else
    (* We must have timed out *)
    raise Unixext.Timeout

(* Listen on a given socket. Accept a single connection and transfer all the data from it to dest_fd, or raise Timeout if target_response_time happens first. *)
(* Raises NotEnoughSpace if the next write would exceed the available_space. *)
let transfer_data_from_sock_to_fd sock dest_fd available_space target_response_time =
  (* Open the data channel *)
  let s = listen_on sock in
  try
    (* May raise a Timeout exception: CA-106403 *)
    let data_client = accept_conn s target_response_time in
    R.info "Accepted connection on data socket";
    ignore_exn (fun () -> Unix.close s);

    (* Read all the data from the data channel, writing it straight into the block device, keeping track of accumulated length *)
    let total_length = ref 0 in
    R.debug "Reading from data socket, writing to the block device...";
    let bytes_read = finally
        (fun () ->
           (* Read data from the client until EOF. Returns the length read. *)
           Unixext.read_data_in_chunks (fun chunk len ->
               (* Check that there's enough space *)
               if available_space - !total_length < len then raise NotEnoughSpace;
               (* Otherwise write it *)
               Unixext.time_limited_write dest_fd len (Bytes.unsafe_of_string chunk) target_response_time;
               total_length := !total_length + len
             ) ~block_size:65536 data_client
        )
        (fun () ->
           (* Close the connection *)
           (* CA-42914: If there was an exception, note that we are forcibly closing the connection when possibly the client (xapi) is still trying to write data. This will cause it to see a 'connection reset by peer' error. *)
           R.info "Closing connection on data socket";
           try
             Unix.shutdown data_client Unix.SHUTDOWN_ALL;
             Unix.close data_client
           with e ->
             R.warn "Exception %s while closing socket" (Printexc.to_string e);
        ) in
    R.debug "Finished reading from data socket";
    bytes_read
  with Unixext.Timeout -> (* Raised by accept_conn *)
    ignore_exn (fun () -> Unix.close s);
    raise Unixext.Timeout

let transfer_database_to_sock sock db_fn target_response_time =
  (* Open the data channel *)
  let s = listen_on sock in
  let data_client = accept_conn s target_response_time in
  R.debug "Accepted connection on data socket";
  ignore_exn (fun () -> Unix.close s);

  finally
    (fun () ->
       (* Read the data and send it down the socket *)
       db_fn (fun chunk len -> Unixext.time_limited_write data_client len (Bytes.of_string chunk) target_response_time)
    )
    (fun () ->
       (* Close the socket *)
       Unix.close data_client
    )

(* --------------------------------------------------- *)
(* Functions to read and write from the client process *)

exception ExpectedSeparator

(* Write a string to the given file descriptor. *)
let send_response client str =
  Unixext.really_write_string client str;
  let len = String.length str in
  if len > 19 then R.debug "Sent long response of length %d beginning [%s...] to client" len (String.sub str 0 19)
  else R.debug "Sent response [%s] to client" str

(* Write a string containing a text string message. *)
let send_failure client prefix error =
  let len = String.length error in
  let str = Printf.sprintf "%s|%016d|%s" prefix len error in
  Unixext.really_write_string client str;
  R.debug "Sent failure message for command [%s] saying [%s] to client" prefix error

(* Read a byte from the given file descriptor. If it is not '|' then raises ExpectedSeparator. *)
let read_separator client =
  let str = Bytes.of_string "\000" in
  Unixext.really_read client str 0 1;
  if str <> (Bytes.of_string "|") then raise ExpectedSeparator

(* Read and return data of a specified length from a given file descriptor. *)
let read_data client length =
  let str = Bytes.make length '\000' in
  Unixext.really_read client str 0 length;
  str

(* Read a marker from the given file descriptor and return it. *)
let read_marker client = read_data client marker_size

(* Read a generation count from the given file descriptor and return it. *)
let read_generation_count client = read_data client generation_size

(* Read an encoded length from the given file descriptor and return it (as an ASCII decimal string). *)
let read_length client = read_data client size_size


(* --------------------------------------------------------------------- *)
(* Functions to perform the actions which may be requested by the client *)

(* Perform a "writedb" operation. *)
let action_writedb block_dev_fd client datasock target_response_time =
  let failure_mesg = "writedb|nack" in
  let success_mesg = "writedb|ack_" in

  R.debug "Received writedb command";

  (* Read marker and generation count from client *)
  read_separator client;
  let marker = read_marker client in
  read_separator client;
  let generation_count = read_generation_count client in

  R.debug "Read marker [%s] and gen_count [%s] from control socket" (Bytes.to_string marker) (Bytes.to_string generation_count);

  try
    (* Read the validity byte *)
    let validity = read_validity_byte block_dev_fd target_response_time in
    R.debug "Validity byte is [%s]" validity;

    (* Decide which half of the double-buffered file to use. If the first half is currently valid, use the second half; and vice versa. *)
    let half_to_use = match validity with
      | "1" -> Second
      | "2" -> First
      | _ -> First (* if neither half is valid, use the first half *) in

    (* Seek to the start of the chosen half *)
    ignore_int (Unixext.seek_to block_dev_fd (start_of_half half_to_use));

    (* Check that we've got enough space for two markers, a length and a generation count. This is the smallest possible size for a db record. *)
    let min_space_needed = marker_size*2 + size_size + generation_size in
    let available_space = Db_globs.redo_log_length_of_half in
    R.debug "Min space needed is %d and we've got %d available" min_space_needed available_space;
    if min_space_needed > available_space then raise NotEnoughSpace;

    (* Write the marker *)
    Unixext.time_limited_write block_dev_fd marker_size marker target_response_time;
    R.debug "Written the marker [%s]" (Bytes.to_string marker);

    (* Save the current cursor position *)
    let pos_to_write_length = Unixext.current_cursor_pos block_dev_fd in
    R.debug "Cursor position to which the length will be written is %d" pos_to_write_length;

    (* Seek forwards to the position to write the data *)
    ignore_int (Unixext.seek_rel block_dev_fd size_size);

    (* Read the data from the data channel and write this directly into block_dev_fd *)
    let remaining_space = Db_globs.redo_log_length_of_half - marker_size - size_size in
    R.debug "Transferring data directly from socket...";
    let total_length = transfer_data_from_sock_to_fd datasock block_dev_fd remaining_space target_response_time in (* may raise NotEnoughSpace or Timeout *)
    R.debug "Transfer complete. Total length was %d bytes" total_length;

    (* Check that there's space for the generation count and marker *)
    let remaining_space = remaining_space - total_length in
    let min_space_needed = generation_size + marker_size in
    R.debug "We now have %d bytes remaining and need %d bytes of space" remaining_space min_space_needed;
    if min_space_needed > remaining_space then raise NotEnoughSpace;

    (* Write the generation count and marker *)
    Unixext.time_limited_write block_dev_fd generation_size generation_count target_response_time;
    Unixext.time_limited_write block_dev_fd marker_size marker target_response_time;
    R.debug "Written generation count and marker";

    (* Save the current cursor position *)
    let pos_after_dbrecord = Unixext.current_cursor_pos block_dev_fd in
    R.debug "Cursor position after marker is %d" pos_after_dbrecord;

    (* If there's space, write some ASCII NULs over the next few bytes so that we trample on any data which may already exist on the block device *)
    let remaining_space = remaining_space - min_space_needed in
    let trample_size = if size_size > remaining_space then remaining_space else size_size in
    Unixext.time_limited_write block_dev_fd trample_size (Bytes.make trample_size '\000') target_response_time;

    (* Seek backwards in the block device to where the length is supposed to go and write it *)
    ignore_int (Unixext.seek_to block_dev_fd pos_to_write_length);
    let total_length_str = Printf.sprintf "%016d" total_length in
    Unixext.time_limited_write block_dev_fd size_size (Bytes.of_string total_length_str) target_response_time;
    R.debug "Gone backwards and written the length %d at position %d" total_length pos_to_write_length;

    (* Set the internal pointer for this half to the position after the db, generation count and marker *)
    set_pointer half_to_use pos_after_dbrecord;

    (* Set the validity byte to indicate that this new half is now valid *)
    set_validity_byte block_dev_fd half_to_use target_response_time;

    (* Respond to the client indicating success *)
    send_response client success_mesg
  with
  | Unixext.Timeout ->
    R.warn "Received timeout during database write. Sending failure message.";
    send_failure client failure_mesg Block_device_io_errors.timeout_error_msg
  | NotEnoughSpace ->
    R.warn "Not enough space on block device. Sending failure message.";
    send_failure client failure_mesg Block_device_io_errors.not_enough_space_error_msg
  | e ->
    let excstr = Printexc.to_string e in
    R.error "Received other exception during database write: %s" excstr;
    send_failure client failure_mesg excstr

(* Perform a "writedelta" operation. *)
let action_writedelta block_dev_fd client datasock target_response_time =
  let failure_mesg = "writedelta|nack" in
  let success_mesg = "writedelta|ack_" in

  (* Read marker, generation count, length and delta from client *)
  read_separator client;
  let marker = read_marker client in
  read_separator client;
  let generation_count = read_generation_count client in
  read_separator client;
  let length_str = read_length client in
  let length = int_of_string (Bytes.unsafe_to_string length_str) in
  read_separator client;
  let data = read_data client length in

  R.debug "writedelta command read params from client: generation count [%s] length [%d]" (Bytes.to_string generation_count) length;

  try
    (* Read the validity byte *)
    let validity = read_validity_byte block_dev_fd target_response_time in

    (* Decide which half of the double-buffered file to use *)
    let half_to_use = match validity with
      | "1" -> First
      | "2" -> Second
      | _ -> raise InvalidBlockDevice (* the log cannot accept deltas *) in

    (* Seek to the position to which to write *)
    let ptr = get_pointer half_to_use in
    let pos = begin
      match ptr with
      | None -> seek_past_last_record block_dev_fd half_to_use target_response_time (* we don't know where it is, so find the position now *)
      | Some p -> Unixext.seek_to block_dev_fd p (* seek straight to the cached position *)
    end in

    (* Construct the delta string *)
    let str = Bytes.concat Bytes.empty [length_str; data; generation_count; marker] in
    let str_len = Bytes.length str in

    (* See if there's enough space for the delta *)
    let available_space = Db_globs.redo_log_length_of_half - (pos - start_of_half half_to_use) in
    if str_len > available_space then raise NotEnoughSpace;

    (* If there's space, write some ASCII NULs over the next few bytes so that we trample on any data which may already exist on the block device *)
    let available_space = available_space - str_len in
    let trample_size = if size_size > available_space then available_space else size_size in
    let str = Bytes.concat Bytes.empty [str; Bytes.make trample_size '\000'] in

    (* Write the delta *)
    Unixext.time_limited_write block_dev_fd (Bytes.length str) str target_response_time;

    (* Set the internal pointer for this half to the position after this point *)
    set_pointer half_to_use (str_len + pos);

    (* Respond to the client indicating success *)
    send_response client success_mesg
  with
  | Unixext.Timeout ->
    R.warn "Received timeout during delta write. Sending failure message.";
    send_failure client failure_mesg Block_device_io_errors.timeout_error_msg
  | InvalidBlockDevice ->
    R.error "Block device is invalid (in action_writedelta). Sending failure message.";
    send_failure client failure_mesg Block_device_io_errors.not_initialised_error_msg
  | NotEnoughSpace ->
    R.warn "Not enough space on block device. Sending failure message.";
    send_failure client failure_mesg Block_device_io_errors.not_enough_space_error_msg
  | e ->
    let excstr = Printexc.to_string e in
    R.error "Received other exception during delta write: %s" excstr;
    send_failure client failure_mesg excstr

(* Perform an "empty" operation, which invalidates the redo log. *)
let action_empty block_dev_fd client datasock target_response_time =
  let success_mesg = "empty|ack_" in
  let failure_mesg = "empty|nack" in

  R.info "Received command to initialise block device";

  try
    (* Set the validity byte to indicate that neither half is valid *)
    R.debug "Resetting validity byte...";
    set_validity_byte block_dev_fd Neither target_response_time;

    (* Respond to the client indicating success *)
    R.debug "Sending ack to client...";
    send_response client success_mesg
  with
  | Unixext.Timeout ->
    R.warn "Received timeout during log initialisation. Sending failure message.";
    send_failure client failure_mesg Block_device_io_errors.timeout_error_msg
  | e ->
    let excstr = Printexc.to_string e in
    R.error "Received other exception during initialisation: %s" excstr;
    send_failure client failure_mesg excstr

(* Perform a "read" operation. *)
let action_read block_dev_fd client datasock target_response_time =
  let end_mesg     = "read|end__" in
  let db_mesg      = "read|db___" in
  let delta_mesg   = "read|delta" in
  let failure_mesg = "read|nack_" in

  R.debug "Received read command";

  try
    (* Read the validity byte *)
    let validity = read_validity_byte block_dev_fd target_response_time in

    (* Decide which half of the double-buffered file to use *)
    let half_to_use = match validity with
      | "1" -> First
      | "2" -> Second
      | _ -> raise InvalidBlockDevice in (* the log is empty *)

    (* Seek to the start of the chosen half *)
    ignore_int (Unixext.seek_to block_dev_fd (start_of_half half_to_use));

    (* Attempt to read a database record *)
    let length, db_fn, generation_count, marker = read_database block_dev_fd target_response_time in

    (* Send the generation count and length of the database to the client *)
    send_response client (Printf.sprintf "%s|%016Ld|%016d" db_mesg generation_count length);

    (* Open the data channel; send the contents of the database down the data channel; close the data channel *)
    transfer_database_to_sock datasock db_fn target_response_time;

    (* Attempt to read the deltas *)
    try
      while true do
        let length, delta, generation_count, marker' = read_delta block_dev_fd target_response_time in
        if marker <> marker' then raise (NonMatchingMarkers(marker, marker'))
        else
          (* Send the delta to the client *)
          send_response client (Printf.sprintf "%s|%016Ld|%016d|%s" delta_mesg generation_count length delta);
      done
    with EndOfDeltas -> send_response client end_mesg (* finish with the end message *)
  with
  | Unixext.Timeout ->
    R.warn "Received timeout during read. Sending failure message.";
    send_failure client failure_mesg Block_device_io_errors.timeout_error_msg
  | InvalidBlockDevice ->
    R.error "Block device is uninitialised. Hence there's nothing to read.";
    send_response client end_mesg (* send the end message *)
  | e ->
    let excstr = Printexc.to_string e in
    R.error "Received other exception during read: %s" excstr;
    send_failure client failure_mesg excstr


(* ---------------------- *)
(* Command-line interface *)

let block_dev = ref ""
let ctrlsock = ref ""
let datasock = ref ""
let dump = ref false
let empty = ref false

let _ =
  (* Initialise debug logging *)
  initialise_logging();

  (* Parse command-line arguments *)
  Arg.parse [ "-device", Arg.Set_string block_dev, "Names the block device";
              "-ctrlsock", Arg.Set_string ctrlsock, "Listen on specified socket as the control channel";
              "-datasock", Arg.Set_string datasock, "Listen on specified socket as the data channel";
              "-dump",   Arg.Set dump, "Dump the contents of the block device to STDOUT";
              "-empty",  Arg.Set empty, "Re-initialise the block device";
            ]
    (fun x -> R.warn "Ignoring argument: %s" x)
    "Provides facilities to interact with a redo logs on a block device";

  (* Check for mandatory parameter *)
  if !block_dev = "" then failwith "Missing -device";

  (* Check for mutually dependent and mutually exclusive parameters *)
  let count = ref 0 in
  if !ctrlsock <> "" then count := !count+1;
  if !datasock <> "" then count := !count+1;
  if !count = 1 then failwith "Option -ctrlsock must be supplied with option -datasock";
  if !count > 0 then count := 1;
  if !dump then count := !count+1;
  if !empty then count := !count+1;
  if !count > 1 then failwith "Options -socket, -dump and -empty are mutually exclusive";
  if !count = 0 then failwith "One of options -socket, -dump or -empty is required";

  if !dump then begin
    (* Open the block device *)
    let block_dev_fd = open_block_device !block_dev (Unix.gettimeofday() +. !Db_globs.redo_log_max_startup_time) in
    R.info "Opened block device.";

    let target_response_time = Unix.gettimeofday() +. 3600. in

    try
      (* Read the validity byte *)
      let validity = read_validity_byte block_dev_fd target_response_time in
      Printf.printf "*** Validity byte: [%s]\n" validity;

      let halves = [First; Second] in
      List.iter (fun half ->
          Printf.printf "*** [Half %s] Entering half.\n" (half_to_string half);

          (* Seek to the start of the chosen half *)
          ignore_int (Unixext.seek_to block_dev_fd (start_of_half half));

          begin
            try
              (* Attempt to read a database record *)
              let length, db_fn, generation_count, marker = read_database block_dev_fd target_response_time in
              Printf.printf "*** [Half %s] Database with generation count [%Ld] and length %d:\n" (half_to_string half) generation_count length;
              db_fn (fun chunk len -> print_string chunk);
              Printf.printf "\n";
              Printf.printf "*** [Half %s] Marker [%s]\n" (half_to_string half) marker;

              (* Attempt to read the deltas *)
              while true do
                let length, delta, generation_count, marker' = read_delta block_dev_fd target_response_time in
                if marker <> marker' then raise (NonMatchingMarkers(marker, marker'))
                else
                  (* Send the delta to the client *)
                  Printf.printf "*** [Half %s] Delta with generation count [%Ld] and length %d:\n" (half_to_string half) generation_count length;
                Printf.printf "%s\n" delta;
                Printf.printf "*** [Half %s] Marker [%s]\n" (half_to_string half) marker'
              done
            with
            | EndOfDeltas -> Printf.printf "*** [Half %s] No more deltas.\n" (half_to_string half)
            | InvalidBlockDevice -> Printf.printf "*** [Half %s] Error: no database found\n%!" (half_to_string half)
            | NonMatchingMarkers(a,b) -> Printf.printf "*** [Half %s] Error: non-matching marker found: expected [%s], got [%s]\n%!" (half_to_string half) a b
          end
        ) halves;
      Printf.printf "*** End.\n"
    with
    | InvalidBlockDevice ->
      R.error "Block device is uninitialised.";
    | Unix.Unix_error(a,b,c) ->
      R.error "Received Unix error [%s] [%s] [%s]" (Unix.error_message a) b c
    | e ->
      R.error "Received other exception: [%s]" (Printexc.to_string e)
  end; (* if !dump *)

  if !empty then begin
    (* Open the block device *)
    let block_dev_fd = open_block_device !block_dev (Unix.gettimeofday() +. !Db_globs.redo_log_max_startup_time) in
    R.info "Opened block device.";

    let target_response_time = Unix.gettimeofday() +. 3600. in
    initialise_redo_log block_dev_fd target_response_time;
    Printf.printf "Block device initialised.\n"
  end; (* if !empty *)

  if !ctrlsock <> "" && !datasock <> "" then begin
    let connect_success_mesg = "connect|ack_" in
    let connect_failure_mesg = "connect|nack" in

    let s = listen_on !ctrlsock in

    (* Main loop: accept a new client, communicate with it until it stops sending commands, repeat. *)
    while true do
      let start_of_startup = Unix.gettimeofday() in
      let target_startup_response_time = start_of_startup +. !Db_globs.redo_log_max_startup_time in

      R.debug "Awaiting incoming connections on %s..." !ctrlsock;
      let client = accept_conn s target_startup_response_time in
      R.debug "Accepted a connection";

      try
        (* Open the block device *)
        let block_dev_fd = open_block_device !block_dev target_startup_response_time in
        R.info "Opened block device '%s'" !block_dev;

        finally
          (fun () ->
             (* If no exception was thrown, respond to the client saying that all was okay *)
             send_response client connect_success_mesg;

             (* Now read and act upon a sequence of commands, until we receive EOF *)
             let stop = ref false in
             while not !stop do
               R.debug "Reading from client...";
               try
                 let buf = Bytes.make command_size '\000' in
                 Unixext.really_read client buf 0 command_size;
                 let str = Bytes.to_string buf in
                 (* Note: none of the action functions throw any exceptions; they report errors directly to the client. *)
                 let (action_fn, block_time) = match str with
                   | "writedelta" -> action_writedelta, !Db_globs.redo_log_max_block_time_writedelta
                   | "writedb___" -> action_writedb,    !Db_globs.redo_log_max_block_time_writedb
                   | "read______" -> action_read,       !Db_globs.redo_log_max_block_time_read
                   | "empty_____" -> action_empty,      !Db_globs.redo_log_max_block_time_empty
                   | _ -> (fun _ _ _ _ -> send_failure client (str^"|nack") ("Unknown command "^str)), 0.
                 in
                 (* "Start the clock!" -- set the latest time by which we need to have responded to the client. *)
                 let target_response_time = Unix.gettimeofday() +. block_time in
                 action_fn block_dev_fd client !datasock target_response_time
               with (* this must be an exception in Unixext.really_read because action_fn doesn't throw exceptions *)
               | End_of_file ->
                 R.info "The client sent EOF";
                 stop := true
               | e ->
                 R.info "Unexpected error when trying to read from client: %s. Closing connection." (Printexc.to_string e);
                 stop := true
             done;
             R.debug "Stopping.";
             ignore_exn (fun () -> Unix.close client)
          )
          (fun () ->
             (* Ensure that the block device FD is always closed *)
             R.info "Closing block device '%s'" !block_dev;
             ignore_exn (fun () -> Unix.close block_dev_fd)
          )
      with (* problems opening block device *)
      | Unix.Unix_error(a,b,c) ->
        R.error "Unix error when opening block device: %s (%s) [%s]" b (Unix.error_message a) c;
        ignore_exn (fun () -> send_failure client connect_failure_mesg (Printf.sprintf "Unix error on %s (%s) [%s]" b (Unix.error_message a) c));
        ignore_exn (fun () -> Unix.close client)
      | e ->
        R.error "Unexpected exception when opening block device: %s" (Printexc.to_string e);
        ignore_exn (fun () -> send_failure client connect_failure_mesg (Printexc.to_string e));
        ignore_exn (fun () -> Unix.close client)
    done
  end (* if !ctrlsock <> "" *)
