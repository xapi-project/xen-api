(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(** date-time with support for keeping timezone for ISO 8601 conversion *)
type t

(** Conversions *)

val of_ptime : Ptime.t -> t
(** Convert ptime to time in UTC *)

val to_ptime : t -> Ptime.t
(** Convert date/time to a ptime value: the number of seconds since 00:00:00
    UTC, 1 Jan 1970. Assumes the underlying {!t} is in UTC *)

val of_unix_time : float -> t
(** Convert calendar time [x] (as returned by e.g. Unix.time), to time in UTC *)

val to_unix_time : t -> float
(** Convert date/time to a unix timestamp: the number of seconds since
    00:00:00 UTC, 1 Jan 1970. Assumes the underlying {!t} is in UTC *)

val to_rfc822 : t -> string
(** Convert date/time to email-formatted (RFC 822) string. *)

val to_rfc3339 : t -> string
(** Convert date/time to an RFC-3339-formatted string. It also complies with
    the ISO 8601 format *)

val of_iso8601 : string -> t
(** Convert ISO 8601 formatted string to a date/time value. Does not accept a
    timezone annotated datetime - i.e. string must be UTC, and end with a Z *)

val epoch : t
(** 00:00:00 UTC, 1 Jan 1970, in UTC *)

val now : unit -> t
(** Count the number of seconds passed since 00:00:00 UTC, 1 Jan 1970, in UTC *)

val _localtime_string : Ptime.tz_offset_s option -> Ptime.t -> string
(** exposed for testing *)

val localtime : unit -> t
(** Count the number of seconds passed since 00:00:00 UTC, 1 Jan 1970, in local
    time *)

(** Comparisons *)

val eq : t -> t -> bool
(** [eq a b] returns whether [a] and [b] are equal *)

val compare : t -> t -> int
(** [compare a b] returns -1 if [a] is earlier than [b], 1 if [a] is later than
    [b] or the ordering of the timezone printer *)

val is_earlier : than:t -> t -> bool
(** [is_earlier ~than a] returns whether the timestamp [a] happens before
    [than] *)

val is_later : than:t -> t -> bool
(** [is_later ~than a] returns whether the timestamp [a] happens after [than]
    *)

val diff : t -> t -> Ptime.Span.t
(** [diff a b] returns the span of time corresponding to [a - b] *)

(** Deprecated bindings, these will be removed in a future release: *)

val rfc822_to_string : t -> string
(** Same as {!to_rfc822} *)

val rfc822_of_float : float -> t
(** Same as {!of_unix_time} *)

val of_float : float -> t
(** Same as {!of_unix_time} *)

val to_float : t -> float
(** Same as {!to_unix_time} *)

val to_string : t -> string
(** Same as {!to_rfc3339} *)

val of_string : string -> t
(** Same as {!of_iso8601} *)

val never : t
(** Same as {!epoch} *)

(** Deprecated alias for {!t} *)
type iso8601 = t

(** Deprecated alias for {!t} *)
type rfc822 = t
