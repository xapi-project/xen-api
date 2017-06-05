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

let really_write fd str =
  let len = String.length str in
  let (>>=) = Lwt.bind in
  let rec inner written =
    Lwt_unix.write fd str written (len-written) >>= (fun n ->
        if n < (len-written) then inner (written+n) else Lwt.return ())
  in inner 0

let lwt_fd_enumerator fd =
  let (>>=) = Lwt.bind in
  let blocksize = 1024 in
  let str = Bytes.create blocksize in
  let get_str n =
    if n=0 
    then (Eof None) 
    else (Chunk (String.sub str 0 n))
  in
  let rec go = function
    | IE_cont (None,x) -> 
      Lwt_unix.read fd str 0 blocksize >>= fun n ->
      x (get_str n)                    >>= fun x -> 
      Lwt.return (fst x)               >>= fun x ->
      go x
    | x -> Lwt.return x 
  in go 

let lwt_enumerator file iter =
  let (>>=) = Lwt.bind in
  Lwt_unix.openfile file [Lwt_unix.O_RDONLY] 0o777 >>= fun fd -> 
  lwt_fd_enumerator fd iter

exception Host_not_found of string

let open_connection_fd host port =
  let (>>=) = Lwt.bind in
  let s = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  if Array.length he.Lwt_unix.h_addr_list = 0
  then (Lwt_unix.close s >>= fun () -> Lwt.fail (Host_not_found host))
  else let ip = he.Unix.h_addr_list.(0) in
    let addr = Unix.ADDR_INET(ip, port) in
    Lwt_unix.connect s addr >>= fun () ->
    Lwt.return s

