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

(* Individual connections *)
type channel = Unix.file_descr
let create () =
  let path = match Xs_transport.choose_xenstore_path () with
    | None ->
      Printf.fprintf stderr "Failed to find xenstore socket. I tried the following:\n";
      List.iter (fun x -> Printf.fprintf stderr "  %s\n" x) (Xs_transport.get_xenstore_paths ());
      Printf.fprintf stderr "\nOn linux you might not have xenfs mounted:\n";
      Printf.fprintf stderr "   sudo mount -t xenfs xenfs /proc/xen\n";
      Printf.fprintf stderr "Or perhaps you just need to set the XENSTORED_PATH environment variable.\n";
      raise Xs_transport.Could_not_find_xenstore
    | Some path -> path
  in
  let stats = Unix.stat path in
  match stats.Unix.st_kind with
  | Unix.S_SOCK -> begin
      let sockaddr = Unix.ADDR_UNIX(path) in
      let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      try
        Unix.connect fd sockaddr;
        fd
      with e ->
        Unix.close fd;
        raise e
    end
  | _ ->
    Unix.openfile path [Lwt_unix.O_RDWR] 0o0

let destroy fd = Unix.close fd
let read fd = Unix.read fd
let write fd bufs ofs len =
  let n = Unix.write fd bufs ofs len in
  if n <> len then begin
    raise End_of_file
  end

type 'a t = 'a
let return x = x
let ( >>= ) x f = f x

type backend = [`unix | `xen]
let backend = `unix
