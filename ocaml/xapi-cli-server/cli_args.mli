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

val to_pairs : 'a t -> (string * 'a) list
(** The [(key, value)] pairs, in command-line order. *)

val reserved : string list
(** Parameter names reserved by the CLI framework; these cannot be used as
    command names. *)

val is_reserved : string -> bool

val get : string -> 'a t -> 'a
(** [get key t] is the value bound to [key]. Raises [Not_found] if absent. *)

val get_opt : string -> 'a t -> 'a option

val get_default : string -> 'a t -> 'a -> 'a
(** [get_default key t default] is the value bound to [key], or [default]. *)

val get_all : string -> 'a t -> 'a list
(** Every value bound to [key], in command-line order (a key may legitimately
    appear more than once). *)

val exists : string -> 'a t -> bool

val add : string -> 'a -> 'a t -> 'a t
(** [add key value t] prepends [(key, value)], shadowing any existing binding
    for [key] in subsequent {!get} / {!get_opt} lookups. *)

val filter : (string -> bool) -> 'a t -> 'a t
(** Keep only the pairs whose key satisfies the predicate. *)

val filter_out : (string -> bool) -> 'a t -> 'a t
(** Drop the pairs whose key satisfies the predicate. *)

val remove : string -> 'a t -> 'a t
(** Remove the first pair bound to [key] (mirrors [List.remove_assoc]). *)

val keys : 'a t -> string list
