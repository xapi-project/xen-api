(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Qmp

type t = Unix.file_descr * in_channel * out_channel

let connect path =
  Printf.fprintf stderr "Connecting to %s\n%!" path;
  let sockaddr = Unix.ADDR_UNIX path in
  let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect s sockaddr;
  s, Unix.in_channel_of_descr s, Unix.out_channel_of_descr s

let to_fd (fd, _, _) = fd

let read (_, ic, _) =
  let line = input_line ic in
  Printf.fprintf stderr "READ [%s]\n%!" line;
  message_of_string line

let write (_, _, oc) m =
  let msg = string_of_message m in
  output_string oc msg;
  output_string oc "\r\n";
  flush oc

let close (s, _, _) = Unix.close s

let negotiate c =
  match read c with
  | Greeting { major; minor; micro; package } ->
    Printf.fprintf stderr "Connected to qemu %d.%d.%d (%s)\n%!" major minor micro package;
    write c (Command(None, Qmp_capabilities));
    begin match read c with
    | Success(None, Unit) ->
      Printf.fprintf stderr "Capability negotiation complete\n%!";
      ()
    | x -> failwith (Printf.sprintf "Unexpected message: %s\n%!" (string_of_message x))
    end
  | x -> failwith (Printf.sprintf "Unexpected message: %s\n%!" (string_of_message x))
