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

open Iteratees

type 'a t = 'a Iteratee(Lwt).t =
  | IE_done of 'a
  | IE_cont of err option * (stream -> ('a t * stream) Lwt.t)

let (>>=) = Lwt.bind

let really_write fd str =
  let len = String.length str in
  let rec inner written =
    Lwt_unix.write fd (Bytes.unsafe_of_string str) written (len-written)
    >>= (fun n ->
        if n < (len-written) then inner (written+n) else Lwt.return ())
  in inner 0

let lwt_fd_enumerator fd =
  let blocksize = 1024 in
  let buf = Bytes.create blocksize in
  let get_str n =
    if n=0
    then (Eof None)
    else (Chunk (Bytes.sub_string buf 0 n))
  in
  let rec go = function
    | IE_cont (None,x) ->
      Lwt_unix.read fd buf 0 blocksize >>= fun n ->
      x (get_str n)                    >>= fun x ->
      Lwt.return (fst x)               >>= fun x ->
      go x
    | x -> Lwt.return x
  in go

let lwt_enumerator file iter =
  Lwt_unix.openfile file [Lwt_unix.O_RDONLY] 0o777 >>= fun fd ->
  lwt_fd_enumerator fd iter

exception Host_not_found of string

let with_fd fd ~callback =
  Lwt.finalize
    (fun () -> callback fd)
    (* The Lwt.catch below prevents errors on double close of the fd. *)
    (fun () -> Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit))

let with_open_connection_fd host port ~callback =
  Lwt_unix.gethostbyname host >>= fun he ->
  if (Array.length he.Lwt_unix.h_addr_list) = 0
  then Lwt.fail (Host_not_found host)
  else
    let ip = he.Unix.h_addr_list.(0) in
    let addr = Unix.ADDR_INET(ip, port) in
    let s = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    with_fd s ~callback:(fun fd -> 
        Lwt_unix.setsockopt fd Lwt_unix.SO_KEEPALIVE true;
        Lwt_unix.connect fd addr >>= fun () ->
        callback fd)
