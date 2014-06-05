(*
 * Copyright (C) 2011-2013 Citrix Inc
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
 
type 'a t = 'a Lwt.t

let (>>=) = Lwt.(>>=)
let return = Lwt.return
let fail = Lwt.fail

open Lwt

type fd = {
  fd: Lwt_unix.file_descr;
  mutable offset: int64;
}

let of_fd fd =
  let offset = 0L in
  { fd; offset }

let read fd buf =
  lwt () = IO.complete "read" (Some fd.offset) Lwt_bytes.read fd.fd buf in
  fd.offset <- Int64.(add fd.offset (of_int (Cstruct.len buf)));
  return ()

let skip_to fd n =
  let buf = Io_page.(to_cstruct (get 1)) in
  let rec loop remaining =
    if remaining = 0L
    then return ()
    else
      let this = Int64.(to_int (min remaining (of_int (Cstruct.len buf)))) in
      let frag = Cstruct.sub buf 0 this in
      lwt () = IO.complete "read" (Some fd.offset) Lwt_bytes.read fd.fd frag in
      fd.offset <- Int64.(add fd.offset (of_int this));
      loop Int64.(sub remaining (of_int this)) in  
  loop Int64.(sub n fd.offset)
