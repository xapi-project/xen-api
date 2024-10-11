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

type t = {
    really_read: Cstruct.t -> unit Lwt.t
  ; really_write: Cstruct.t -> unit Lwt.t
  ; offset: int64 ref
  ; skip: int64 -> unit Lwt.t
  ; copy_from: Lwt_unix.file_descr -> int64 -> int64 Lwt.t
  ; close: unit -> unit Lwt.t
}

exception Impossible_to_seek

val of_raw_fd : Lwt_unix.file_descr -> t Lwt.t

val of_seekable_fd : Lwt_unix.file_descr -> t Lwt.t

type verification_config = {
    sni: string option
  ; verify: Ssl.verify_mode
  ; cert_bundle_path: string
}

val of_ssl_fd :
  Lwt_unix.file_descr -> string option -> verification_config option -> t Lwt.t
