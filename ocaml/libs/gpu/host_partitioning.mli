(*
 * Copyright (C) 2026 Cloud Software Group
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

(** Why GPU partitioning is not wanted on a host. *)
type reason = No_partitionable_gpu | Pool_disabled

val token : reason -> string
(** The reason as the host's status shows it to clients *)

val desired :
     partitionable_gpu:(unit -> bool)
  -> pool_enabled:(unit -> bool)
  -> (unit, reason) result
(** [Ok ()] if the host should partition its GPUs, or else the reason of the
    first check that fails. The checks run in this order, most specific to the
    host first, and none runs after one fails. *)
