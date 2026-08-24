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

(** Key types for table entries and lookups.
    Entries are stored under a [pattern_key] (which may contain wildcards),
    while lookups identify a caller with a concrete [t]. *)
module Key : sig
  (** A pattern for a single field. [Full s] matches exactly [s].
      [Prefix p] matches any string starting with [p]; [Prefix ""] is a
      full wildcard. *)
  type match_pattern = Full of string | Prefix of string

  (** A concrete caller identifier used for lookups. *)
  type t = {user_agent: string; client_ip: string}

  (** A pattern stored in the table. Each field is matched independently. *)
  type pattern_key = {
      user_agent_pattern: match_pattern
    ; client_ip_pattern: match_pattern
  }

  val matches_key : pattern:pattern_key -> target:t -> bool
  (** [matches_key ~pattern ~target] returns true if [pattern] matches
      [target]. Both fields must match independently. *)

  val compare : pattern_key -> pattern_key -> int
  (** Total order on patterns: fewer wildcards first, then lexicographic
      by patterns. *)

  val is_all_wildcard : pattern_key -> bool
  (** [is_all_wildcard k] returns true if every field of [k] is a full
      wildcard ([Prefix ""]). Such patterns are rejected by [insert]. *)
end

(** List of entries mapping patterns to values.
    Lookups use wildcard matching with priority: exact > prefix > full wildcard. *)
type 'a t

val create : unit -> 'a t
(** [create ()] creates a new empty table. *)

val insert : 'a t -> pattern:Key.pattern_key -> 'a -> bool
(** [insert t ~pattern data] adds an entry for the given pattern.
    Returns [false] if an entry already exists for that exact pattern, or
    if [pattern] has both fields as full wildcards (all-wildcard patterns
    are rejected). *)

val mem : 'a t -> caller_id:Key.t -> bool
(** [mem t ~caller_id] returns whether [caller_id] matches any entry
    in the table using wildcard matching. *)

val delete : 'a t -> pattern:Key.pattern_key -> unit
(** [delete t ~pattern] removes the entry for the exact pattern. *)

val get : 'a t -> caller_id:Key.t -> 'a list
(** [get t ~caller_id] returns the values for all entries whose pattern
    matches [caller_id], ordered from most specific to least specific match
    (exact > prefix > full wildcard). Returns the empty list if no entry
    matches. *)

val get_exact : 'a t -> pattern:Key.pattern_key -> 'a option
(** [get_exact t ~pattern] returns the value for the entry whose pattern
    is exactly equal to [pattern], or [None]. Does not use wildcard
    matching. *)

val to_list : 'a t -> (Key.pattern_key * 'a) list
(** [to_list t] returns a snapshot of all entries in [t], most specific
    first. *)
