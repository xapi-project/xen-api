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

(** Why GPU partitioning is not wanted on a host. The constructor names, in
    lower case, are the values of the XenAPI field
    Host.gpu_partitioning_reason: renaming a constructor changes the XenAPI. *)
type reason = No_partitionable_gpu | Forced_off | Pool_disabled

val token : reason -> string
(** The value clients see in Host.gpu_partitioning_reason: the constructor's
    name in lower case. *)

val all_reasons : reason list
(** Every reason, in the order of the type. *)

(** Whether the host's own setting forces GPU partitioning off, whatever the
    pool's setting. *)
type forced = Not_forced | Forced

val forced_token : forced -> string
(** The value the host's force-off file holds for the setting. The file is
    kept across upgrades, so a token must never change. *)

val forced_of_file :
  unexpected:(string -> unit) -> (string option, string) result -> forced
(** The setting from a read of the force-off file, [Ok None] when there is no
    file. A file that holds exactly a value of {!forced_token} gives that
    setting. Anything else, an unreadable file included, forces GPU
    partitioning off: [unexpected] is told why. *)

val desired :
     partitionable_gpu:(unit -> bool)
  -> forced:(unit -> forced)
  -> pool_enabled:(unit -> bool)
  -> (unit, reason) result
(** [Ok ()] if the host should partition its GPUs, or else the reason of the
    first check that fails. The checks run in this order, most specific to the
    host first, and none runs after one fails. *)
