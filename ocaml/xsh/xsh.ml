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

type endpoint = { fdin: Unix.file_descr; fdout: Unix.file_descr; mutable buffer: bytes; mutable buffer_len: int }

let make_endpoint fdin fdout = {
  fdin = fdin;
  fdout = fdout;
  buffer = Bytes.make 4096 '\000';
  buffer_len = 0
}

let proxy (ain: Unix.file_descr) (aout: Unix.file_descr) (bin: Unix.file_descr) (bout: Unix.file_descr) =
  let a' = make_endpoint ain aout and b' = make_endpoint bin bout in
  Unix.set_nonblock ain;
  Unix.set_nonblock aout;
  Unix.set_nonblock bin;
  Unix.set_nonblock bout;

  let can_read x =
    x.buffer_len < (Bytes.length x.buffer - 1) in
  let can_write x =
    x.buffer_len > 0 in
  let write_from x y =
    let written = Unix.single_write y.fdout x.buffer 0 x.buffer_len in
    Bytes.blit x.buffer written x.buffer 0 (x.buffer_len - written);
    x.buffer_len <- x.buffer_len - written in
  let read_into x =
    let read = Unix.read x.fdin x.buffer x.buffer_len (Bytes.length x.buffer - x.buffer_len) in
    if read = 0 then raise End_of_file;
    x.buffer_len <- x.buffer_len + read in

  try
    while true do
      let r = (if can_read a' then [ ain ] else []) @ (if can_read b' then [ bin ] else []) in
      let w = (if can_write a' then [ bout ] else []) @ (if can_write b' then [ aout ] else []) in

      let r, w, _ = Unix.select r w [] (-1.0) in
      (* Do the writing before the reading *)
      List.iter (fun fd -> if aout = fd then write_from b' a' else write_from a' b') w;
      List.iter (fun fd -> if ain = fd then read_into a' else read_into b') r
    done
  with _ ->
    (try Unix.clear_nonblock ain with _ -> ());
    (try Unix.clear_nonblock bin with _ -> ());
    (try Unix.clear_nonblock aout with _ -> ());
    (try Unix.clear_nonblock bout with _ -> ());
    (try Unix.close ain with _ -> ());
    (try Unix.close bin with _ -> ());
    (try Unix.close aout with _ -> ());
    (try Unix.close bout with _ -> ())

let open_tcp_ssl server =
  let port = 443 in
  (* We don't bother closing fds since this requires our close_and_exec wrapper *)
  let x = Stunnel.connect ~use_fork_exec_helper:false
      ~write_to_log:(fun _ -> ()) server port in
  x.Stunnel.fd

let _ =
  let host = Sys.argv.(1) in
  let cmd = Sys.argv.(2) in
  Stunnel.set_legacy_protocol_and_ciphersuites_allowed
    (try bool_of_string (Sys.getenv "XSH_SSL_LEGACY") with _ -> failwith "ssl_legacy not specified");
  Stunnel.set_good_ciphersuites
    (try Sys.getenv "XSH_GOOD_CIPHERSUITES" with _ -> failwith "Good ciphersuites not specified");
  Stunnel.set_legacy_ciphersuites
    (try Sys.getenv "XSH_LEGACY_CIPHERSUITES" with _ -> "");
  let session = try Sys.getenv "XSH_SESSION" with _ -> failwith "Session not provided" in
  let args = List.map (fun arg -> "&arg="^arg) (List.tl (List.tl (List.tl (Array.to_list Sys.argv)))) in
  let req = Printf.sprintf "CONNECT /remotecmd?session_id=%s&cmd=%s%s http/1.0\r\n\r\n" session cmd (String.concat "" args) in
  let fd = open_tcp_ssl host in
  Unix.write_substring fd req 0 (String.length req) |> ignore;
  proxy Unix.stdin Unix.stdout fd (Unix.dup fd)
