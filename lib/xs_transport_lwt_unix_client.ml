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

(** A byte-level transport over the xenstore Unix domain socket *)

open Lwt

(* Individual connections *)
type channel = Lwt_unix.file_descr

let create () =
  ( match Xs_transport.choose_xenstore_path () with
    | None ->
      Printf.fprintf stderr "Failed to find xenstore socket. I tried the following:\n";
      List.iter (fun x -> Printf.fprintf stderr "  %s\n" x) (Xs_transport.get_xenstore_paths ());
      Printf.fprintf stderr "\nOn linux you might not have xenfs mounted:\n";
      Printf.fprintf stderr "   sudo mount -t xenfs xenfs /proc/xen\n";
      Printf.fprintf stderr "Or perhaps you just need to set the XENSTORED_PATH environment variable.\n";
      fail Xs_transport.Could_not_find_xenstore
    | Some x -> return x ) >>= fun path ->
  Lwt_unix.stat path >>= fun stats ->
  match stats.Lwt_unix.st_kind with
  | Lwt_unix.S_SOCK ->
    let sockaddr = Lwt_unix.ADDR_UNIX(path) in
    let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.connect fd sockaddr >>= fun () ->
    return fd
  | _ ->
    let fd = Unix.openfile path [ Lwt_unix.O_RDWR ] 0o0 in
    (* It looks like a file but behaves like a pipe: *)
    return (Lwt_unix.of_unix_file_descr ~blocking:false fd)

let destroy = Lwt_unix.close
let read = Lwt_unix.read

let write fd bufs ofs len =
  Lwt_unix.write fd bufs ofs len >>= fun n ->
  if n <> len
  then fail End_of_file
  else return ()

type 'a t = 'a Lwt.t
let return = Lwt.return
let ( >>= ) = Lwt.bind

type backend = [`unix | `xen]
let backend = `unix
