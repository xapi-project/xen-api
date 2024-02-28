(* Copyright (C) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

(** [t] t is the minimal type to recognise elements in a cache. This does not
    contain the contents of the elements being contained, only the metadata *)
type t = Uuidm.t * Mtime.t * Types.Tpm.key

val setup :
     Types.Service.t
  -> (t -> (string, exn) Lwt_result.t)
  -> (t -> string -> (unit, exn) Lwt_result.t)
  -> ( (   ((t -> string Lwt.t) * (t -> string -> unit Lwt.t) -> 'a Lwt.t)
        -> 'a Lwt.t
       )
     * (unit -> unit Lwt.t)
     )
     Lwt.t
(** [setup service read_callback push_callback] Returns a local disk buffer for
    [service] which will use [push_callback] to push the elements to their
    final destination and [read_callback] to read elements if they are not in
    the buffer. *)
