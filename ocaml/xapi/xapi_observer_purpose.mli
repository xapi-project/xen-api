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

(** Observer purposes: a second axis for labelling traces, saying what a
    trace is {e for}. It is orthogonal to {!Xapi_observer_components}, which
    says where a trace came from.

    A purpose is a label, not a destination. Enabling a purpose on an
    observer does not change where any span is exported; a consumer that
    wants only, say, audit spans filters on the label itself.

    Consumer contract: match on the purpose values you care about and
    ignore any you do not recognise, so that adding a value later is inert to
    existing consumers. *)

(** The closed set of purposes. *)
type t = Performance | Functional | Audit | Boundary [@@deriving ord]

val all : t list
(** Every purpose. *)

val to_string : t -> string
(** The string stored in [Observer.purpose]. *)

val of_string : string -> t option
(** [None] for a string that is not a known purpose. *)

val assert_valid_purpose : string list -> unit
(** Raises [Api_errors.Server_error (invalid_value, ["purpose"; p])] for
    the first element [p] that is not a known purpose. *)

val is_enabled : observers:(bool * string list) list -> purpose:t -> bool
(** [is_enabled ~observers ~purpose] is [true] iff some observer in
    [observers] is enabled and is enabled for [purpose].

    Each element of [observers] is the [(enabled, purposes)] pair of one
    Observer record, already fetched by the caller. An empty purposes list
    means all purposes, mirroring [Observer.components]. Unknown strings in
    a purposes list are ignored.

    This function is pure: it takes no context and makes no database calls,
    so it is safe on a per-span path. Fetching the records, and paying for
    that, is the caller's job: see {!observers_of_records}. *)

val observers_of_records :
  ('a * API.observer_t) list -> (bool * string list) list
(** Projects Observer records, as returned by
    [Db.Observer.get_all_records], into the input of {!is_enabled}. Pure. *)
