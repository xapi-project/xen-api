(*
 * Copyright (C) Cloud Software Group
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

(** Force only during [events_notify], under the writer's database lock.
    [None] means the record could not be read; drop the event. *)
type snapshot = unit -> Rpc.t option

val events_register :
  (?snapshot:snapshot -> string -> string -> string -> unit) -> unit

val events_notify : ?snapshot:snapshot -> string -> string -> string -> unit
