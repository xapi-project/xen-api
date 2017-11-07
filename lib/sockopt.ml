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

external _set_sock_keepalives : Unix.file_descr -> int -> int -> int -> unit = "stub_sockopt_set_sock_keepalives"
let set_sock_keepalives ?(count=5) ?(idle=30) ?(interval=2) fd =
  let open Unix in
  if (fstat fd).st_kind = S_SOCK then begin
    setsockopt fd SO_KEEPALIVE true;
    _set_sock_keepalives fd count idle interval
  end
