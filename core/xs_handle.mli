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

type 'a t
(** A 'handle' is a sub-connection used for a particular purpose.
    The handle is a convenient place to store sub-connection state *)

val make: 'a -> 'a t

val get_tid: 'a t -> int32
(** return the transaction id (typically for debug printing) *)

val get_client: 'a t -> 'a
(** return the client instance wrapped in a handle *)

val no_transaction: 'a -> 'a t
(** Handle used for 'immediate' non-transactional read/writes *)

val transaction: 'a -> int32 -> 'a t
(** Handle used for transactional read/writes *)

val watching: 'a -> 'a t
(** Handle used to store watch-related information *)

val accessed_path: 'a t -> string -> 'a t
(** Get the list of recorded path accesses *)

module StringSet : module type of Set.Make(struct type t = string let compare = compare end)

val get_accessed_paths: 'a t -> StringSet.t
(** Get the list of paths we have accessed *)

val watch: 'a t -> string -> 'a t
(** Declare that we are watching a path *)

val unwatch: 'a t -> string -> 'a t
(** Declare that we are nolonger watching a path *)

val get_watched_paths: 'a t -> StringSet.t
(** Get the list of paths we're currently watching *)

