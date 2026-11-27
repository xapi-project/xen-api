(*
 * Copyright (c) Cloud Software Group, Inc.
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

(** Claims affect host ranking only. Database reservations determine capacity. *)

type t

type claim = {host: string; guest: Vm_placement.Guest_snapshot.t}

val create : unit -> t

val coordinator : t

val claims : t -> (API.ref_VM * claim) list

val claim :
     t
  -> vm:API.ref_VM
  -> ((API.ref_VM * claim) list -> claim option)
  -> claim option
(** [choose] runs under the claims mutex; it must not wait for other locks. *)

val release : t -> vm:API.ref_VM -> unit

val count : t -> int
