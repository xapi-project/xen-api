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


module D = Debug.Make(struct let name = "xenops_migrate" end)
open D

exception Remote_failed of string

(** Functions to synchronise between the sender and receiver via binary messages of the form:
    00 00 -- success
    11 22 <0x1122 bytes of data> -- failure, with error message
    Used rather than the API for signalling between sender and receiver to avoid having to
    go through the master and interact with locking. *)
module Handshake = struct
  type result =
    | Success
    | Error of string

  let string_of_result = function
    | Success -> "Success"
    | Error x -> "Error: " ^ x

  let rec really_read fd buf ofs len =
    let n = Unix.read fd buf ofs len in
    if n = 0 then raise End_of_file;
    if n < len then really_read fd buf (ofs + n) (len - n)

  (** Receive a 'result' from the remote *)
  let recv ?verbose:(verbose=false) (s: Unix.file_descr) : result =
    let buf = Bytes.make 2 '\000' in
    if verbose then debug "Handshake.recv: about to read result code from remote.";
    (try
       really_read s buf 0 (Bytes.length buf)
     with _ ->
       raise (Remote_failed "unmarshalling result code from remote"));
    if verbose then debug "Handshake.recv: finished reading result code from remote.";
    let len = int_of_char (Bytes.get buf 0) lsl 8 lor (int_of_char @@ Bytes.get buf 1) in
    if len = 0
    then Success
    else begin
      let msg = Bytes.make len '\000' in
      if verbose then debug "Handshake.recv: about to read error message from remote.";
      (try really_read s msg 0 len
       with _ ->
         raise (Remote_failed "unmarshalling error message from remote"));
      if verbose then debug "Handshake.recv: finished reading error message from remote.";
      Error (Bytes.unsafe_to_string msg)
    end

  (** Expects to receive a success code from the server, throws an exception otherwise *)
  let recv_success ?verbose (s: Unix.file_descr) : unit = match recv ?verbose s with
    | Success -> ()
    | Error x -> raise (Remote_failed ("error from remote: " ^ x))

  (** Transmit a 'result' to the remote *)
  let send ?verbose:(verbose=false) (s: Unix.file_descr) (r: result) =
    let len = match r with
      | Success -> 0
      | Error msg -> String.length msg in
    let buf = Bytes.make (2 + len) '\000' in
    Bytes.set buf 0 @@ char_of_int ((len lsr 8) land 0xff);
    Bytes.set buf 1 @@ char_of_int ((len lsr 0) land 0xff);
    (match r with
     | Success -> ()
     | Error msg -> String.blit msg 0 buf 2 len);
    if verbose then debug "Handshake.send: about to write result to remote.";
    if Unix.write s buf 0 (len + 2) <> len + 2
    then raise (Remote_failed "writing result to remote");
    if verbose then debug "Handshake.send: finished writing result to remote.";
end

module Forwarded_http_request = struct
  (** Subset of the structure sent by xapi *)
  type t = {
    uri: string;
    query: (string*string) list;
    cookie: (string * string) list;
    body: string option;
  } [@@deriving rpc]
end

