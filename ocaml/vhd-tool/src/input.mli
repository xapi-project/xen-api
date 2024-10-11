(* Copyright (C) Cloud Software Group Inc.
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

type 'a t = 'a Lwt.t

type fd = {fd: Lwt_unix.file_descr; mutable offset: int64}

include Vhd_format.S.INPUT with type 'a t := 'a t with type fd := fd

val of_fd : Lwt_unix.file_descr -> fd
