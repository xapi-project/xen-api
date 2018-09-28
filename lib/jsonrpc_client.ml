(*
 * Copyright (C) Citrix Systems Inc.
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

(* JSON-RPC Client *)

module D = Debug.Make(struct let name = "jsonrpc_client" end)
open D

exception Timeout
exception Read_error

let json_rpc_max_len = ref 65536 (* Arbitrary maximum length of RPC response *)
let json_rpc_read_timeout = ref 60000000000L (* timeout value in ns when reading RPC response *)
let json_rpc_write_timeout = ref 60000000000L (* timeout value in ns when writing RPC request *)

let to_s  s = (Int64.to_float s) *. 1e-9

(* Read the entire contents of the fd, of unknown length *)
let timeout_read fd timeout =
  let buf = Buffer.create !json_rpc_max_len in
  let read_start = Mtime_clock.counter () in
  let get_total_used_time () = Mtime.Span.to_uint64_ns (Mtime_clock.count read_start) in
  let rec inner max_time max_bytes =
    let (ready_to_read, _, _) = try Unix.select [fd] [] [] (to_s max_time) with
      (* in case the unix.select call fails in situation like interrupt *)
      | Unix.Unix_error(Unix.EINTR,_,_) -> [], [], []
    in
    (* This is not accurate the calculate time just for the select part. However, we
       		 * think the read time will be minor comparing to the scale of tens of seconds.
       		 * the current style will be much concise in code. *)
    let remain_time = 
      let used_time = get_total_used_time () in
      Int64.sub timeout used_time
    in
    if remain_time < 0L then 
      begin
        debug "Timeout after read %d" (Buffer.length buf);
        raise Timeout
      end;
    if List.mem fd ready_to_read then
      begin
        let bytes = Bytes.make 4096 '\000' in
        match Unix.read fd bytes 0 4096 with
        | 0 -> Buffer.contents buf (* EOF *)
        | n ->
          if n > max_bytes then
            begin
              debug "exceeding maximum read limit %d, clear buffer" !json_rpc_max_len;
              Buffer.clear buf;
              raise Read_error
            end
          else
            begin
              Buffer.add_subbytes buf bytes 0 n;
              inner remain_time (max_bytes - n)
            end
        | exception Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR),_,_) ->
          inner remain_time max_bytes
      end
    else inner remain_time max_bytes
  in
  inner timeout !json_rpc_max_len

(* Write as many bytes to a file descriptor as possible from data before a given clock time. *)
(* Raises Timeout exception if the number of bytes written is less than the specified length. *)
(* Writes into the file descriptor at the current cursor position. *)
let timeout_write filedesc total_length data response_time =
  let write_start = Mtime_clock.counter () in
  let get_total_used_time () = Mtime.Span.to_uint64_ns (Mtime_clock.count write_start) in
  let rec inner_write offset max_time =
    let (_, ready_to_write, _) = try Unix.select [] [filedesc] [] (to_s max_time) with
      (* in case the unix.select call fails in situation like interrupt *)
      | Unix.Unix_error(Unix.EINTR,_,_) -> [], [], []
    in
    let remain_time = 
      let used_time = get_total_used_time () in
      Int64.sub response_time used_time
    in
    if remain_time < 0L then
      begin
        debug "Timeout to write %d at offset %d" total_length offset;
        raise Timeout
      end;
    if List.mem filedesc ready_to_write then 
      begin
        let length = total_length - offset in
        let bytes_written = 
          (try Unix.single_write filedesc data offset length with 
           | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR),_,_) -> 0)
        in
        let new_offset = offset + bytes_written in
        if length = bytes_written then ()
        else inner_write new_offset remain_time
      end
    else inner_write offset remain_time
  in
  inner_write 0 response_time

let with_rpc ?(version=Jsonrpc.V2) ~path ~call () =
  let uri = Uri.of_string (Printf.sprintf "file://%s" path) in
  Open_uri.with_open_uri uri (fun s ->
      Unix.set_nonblock s;
      let req = Bytes.of_string (Jsonrpc.string_of_call ~version call) in
      timeout_write s (Bytes.length req) req !json_rpc_write_timeout;
      let res = timeout_read s !json_rpc_read_timeout in
      debug "Response: %s" res;
      Jsonrpc.response_of_string ~strict:false res)
