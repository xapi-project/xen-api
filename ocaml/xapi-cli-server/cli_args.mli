(*
 * Copyright (C) 2026 Vates.
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

(** [Cli_args] holds the [key=value] parameters parsed from an [xe] command
    line. It is a thin abstraction over an association list; the point of the
    abstraction is to give a single place through which every parameter access
    goes, so that later we can track which parameters a command actually
    consumed and report the ones that were silently ignored. *)

type 'a t

val from_pairs : (string * string) list -> string t
(** Build the structure from the parsed command-line [(key, value)] pairs. *)

val view : string -> 'a t -> 'a t
(** [view prefix t] is [t] restricted to the entries whose key is [prefix]
    followed by a separator and at least one more character (the
    [prefix:key=value] map/set syntax, and the legacy [prefix-key=value] form).
    Every accessor then sees those keys with the [prefix] and separator removed,
    e.g. [get "auto-scan" (view "other-config" t)] reads [other-config:auto-scan].
    The underlying entries are shared with [t]. *)

val reserved : string list
(** Parameter names reserved by the CLI framework; these cannot be used as
    command names. *)

val is_reserved : string -> bool

val get : string -> 'a t -> 'a
(** [get key t] is the value bound to [key]. Raises [Not_found] if absent. *)

val get_opt : string -> 'a t -> 'a option

val get_default : string -> 'a t -> 'a -> 'a
(** [get_default key t default] is the value bound to [key], or [default]. *)

val assoc_default_ci : string -> string t -> string
(** [assoc_default_ci key t] is the value bound to [key] matched
    case-insensitively, or [""] if absent. Marks the matched entry as read. *)

val get_all : string -> 'a t -> 'a list
(** Every value bound to [key], in command-line order (a key may legitimately
    appear more than once). *)

val exists : string -> 'a t -> bool

val consume : 'a t -> 'a t
(** Mark every entry visible through [t] as read, and return [t]. For call sites
    that take a whole (view's) contents wholesale -- e.g. a set parameter read
    with {!keys}; a map parameter goes through {!map_contents} instead. *)

val map_contents : 'a t -> (string * 'a) list
(** The visible [(key, value)] pairs of [t], all marked as read. For forwarding a
    whole map parameter -- or a filtered parameter list -- to the API:
    [Cli_maps.other_config params |> map_contents]. Marks as it reads, so the
    entries cannot be left unconsumed and then wrongly reported as ignored --
    which is why it, rather than a raw pair list, is what the interface exposes. *)

val log_args : censor:(string -> bool) -> string t -> string
(** [t] rendered as a ["key=value key=value"] line for a log, with the value of
    every [key] for which [censor key] is [true] replaced by ["(omitted)"]. Does
    not mark entries -- diagnostic output, not a command reading its arguments. *)

val mark_used : string -> 'a t -> unit
(** Mark the entry for [key], if any, as read, without reading its value. *)

val unused : 'a t -> string list
(** The keys of the entries no command has read through {!get} & co. Used to
    report the parameters a command silently ignored. *)

val add : string -> 'a -> 'a t -> 'a t
(** [add key value t] prepends [(key, value)], shadowing any existing binding
    for [key] in subsequent {!get} / {!get_opt} lookups. *)

val filter : (string -> bool) -> 'a t -> 'a t
(** Keep only the pairs whose key satisfies the predicate. *)

val filter_out : (string -> bool) -> 'a t -> 'a t
(** Drop the pairs whose key satisfies the predicate. *)

val remove : string -> 'a t -> 'a t
(** Remove the first pair bound to [key] (mirrors [List.remove_assoc]). *)

val rename_key : from_:string -> to_:string -> 'a t -> unit
(** Rename every entry keyed [from_] to [to_], in place. The entry records are
    shared with the views of [t] and with the structure it was derived from, so
    the rename -- and any subsequent {!mark_used} -- is visible from all of
    them. *)

val keys : 'a t -> string list
