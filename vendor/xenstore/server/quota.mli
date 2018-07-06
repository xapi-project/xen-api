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

exception Limit_reached
(** The maximum number of entries has been created *)

exception Data_too_big
(** The value to be written is too big *)

exception Transaction_opened
(** Too many transactions have been started *)

val maxent: int ref
(** The current global default maximum number of entries per domain *)

val maxsize: int ref
(** The current global default maximum value size *)

val maxwatch: int ref
(** The current global default maximum number of watches *)

val maxtransaction: int ref
(** The current global default maximum number of open transactions *)

val maxwatchevent: int ref
(** The current global maximum number of outstanding watch events *)

type overrides

val maxent_overrides: overrides
(** Per-domain overrides for the maxent limit *)

val maxwatch_overrides: overrides
(** Per-domain overrides for the maxwatch limit *)

val maxtransaction_overrides: overrides
(** Per-domain overrides for the maxtransaction limit *)

val maxwatchevent_overrides: overrides
(** Per-domain overrides for the maxwatchevent limit *)

val set_override: overrides -> int -> int option -> unit
(** [set_override kind t domid x] sets the [kind] override for [domid] to [x] *)

val get_override: overrides -> int -> int option
(** [get_override kind t domid] returns any current [kind] override for [domid] *)

val list_overrides: overrides -> (int * int) list
(** [list_overrides kind] returns the current [kind] (domid, override) pairs *)

val maxwatch_of_domain: int -> int
(** [maxwatch_of_domain domid] returns the max number of watches for [domid] *)

val maxtransaction_of_domain: int -> int
(** [maxtransaction_of_domain domid] returns the max number of transactions for [domid] *)

val maxwatchevent_of_domain: int -> int
(** [maxwatchevent_of_domain domid] returns the max number of outstanding watch
	events for [domid] *)

type t
(** Represents the current per-domain number of entries *)

val create: unit -> t
(** Create an empty [t] *)

val copy: t -> t
(** Return a duplicate of [t] *)

val union: t -> t -> unit
(** [union a b] adds all entries from [b] to [a] *)

val merge: t -> t -> t -> unit
(** [union a b] adds all entries from [b] to [a] *)

val check: t -> int -> int -> unit
(** [check t domid size]
	throws Data_too_big if [size] is too large
	throws Limit_reached if [domid] has reached its entry limit *)

val get: t -> int -> int
(** [get domid] returns the number of entries associated with [domid] *)

val list: t -> (int * int) list
(** [list t] returns all (domid, entry count) pairs *)

val incr: t -> int -> unit
(** [incr t domid] adds an entry to [domid] *)

val decr: t -> int -> unit
(** [decr t domid] removes an entry from [domid] *)

