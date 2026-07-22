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

(** A least-recently-used (LRU) cache mapping keys of type ['k] to
    values of type ['v]. The cache has a fixed capacity; when full, the
    least recently used entries are evicted to make room for new ones.
    All operations are thread-safe. *)
type ('k, 'v) t

val create : int -> ('k, 'v) t
(** [create capacity] creates an empty cache with the given capacity.
    Raises [Invalid_argument] if [capacity <= 0]. *)

val size : ('k, 'v) t -> int
(** [size t] is the current number of entries in the cache. *)

val cap : ('k, 'v) t -> int
(** [cap t] is the maximum capacity of the cache. *)

val to_list : ('k, 'v) t -> ('k * 'v) list
(** [to_list t] returns all entries in LRU order: the least recently
    used entry is at the head of the list. *)

val lookup : ('k, 'v) t -> 'k -> 'v option
(** [lookup t key] returns the value bound to [key] if present, and
    marks the entry as most recently used. *)

val remove : ('k, 'v) t -> 'k -> unit
(** [remove t key] removes the entry for [key]; does nothing if absent. *)

val add : ('k, 'v) t -> 'k -> 'v -> bool
(** [add t key value] inserts [(key, value)] if [key] is not already
    present. Returns [true] if the cache now exceeds its capacity,
    signalling the caller to call [trim]. *)

val drop_while : ('k, 'v) t -> evict:('k * 'v -> bool -> bool) -> unit
(** [drop_while t ~evict] drops entries in least-recently-used order
    while [evict (k, v) over_capacity] is true. The boolean argument
    indicates whether the cache currently exceeds its capacity. *)

val trim : ('k, 'v) t -> unit
(** [trim t] evicts least-recently-used entries until the cache no
    longer exceeds its capacity. *)

val add_trim : ('k, 'v) t -> 'k -> 'v -> unit
(** [add_trim t key value] is equivalent to [add] followed by [trim]. *)

val filter : ('k, 'v) t -> f:('k -> 'v -> bool) -> ('k, 'v) t
(** [filter t ~f] returns a fresh cache containing only the entries for
    which [f k v] returns true, preserving LRU order. The source cache
    is not modified. *)
