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

(** Nanosecond-precision POSIX timestamps, allows datetimes with unspecified
    timezones. These are needed to produce and accept ISO 8601 datetimes without
    timezones, but because the timezone is not known they do not share a common
    point of time with any other timestamps they cannot be converted to unix
    time, or be compared with other timestamps. All other timestamps have a
    timezone attached to them, which will be used to serialize them to a
    datetime string. This timezone is determined when creating a value and
    cannot be changed. For timestamps created from datetime strings, the
    timezone is maintained. For all other values UTC is used. *)
type t

(** Conversions *)

val of_ptime : Ptime.t -> t
(** Converts ptime to date *)

val to_ptime : t -> Ptime.t
(** Convert date/time to a ptime value: the number of seconds since 00:00:00
    UTC, 1 Jan 1970. When {!t} lacks a timezone, UTC is assumed *)

val of_unix_time : float -> t
(** Convert calendar time [x] (as returned by e.g. Unix.time), to time in UTC *)

val to_unix_time : t -> float
(** Convert date/time to a unix timestamp: the number of seconds since
    00:00:00 UTC, 1 Jan 1970. When {!t} lacks a timezone, UTC is assumed *)

val to_rfc822 : t -> string
(** Convert date/time to email-formatted (RFC 822) string. *)

val to_rfc3339 : t -> string
(** Convert date/time to an RFC-3339-formatted string. It also complies with
    the ISO 8601 format *)

val of_iso8601 : string -> t
(** Convert ISO 8601 formatted string to a date/time value. Timezone can be
    missing from the string, but that means some conversions will assume UTC,
    which might be incorrect *)

val epoch : t
(** 00:00:00 UTC, 1 Jan 1970 *)

val now : unit -> t
(** Count the number of seconds passed since 00:00:00 UTC, 1 Jan 1970 *)

val _localtime_string : Ptime.tz_offset_s option -> Ptime.t -> string
(** exposed for testing *)

val localtime : unit -> t
(** Local date time, the timezone is stripped. Do not use this call in new
    code. *)

(** Comparisons *)

val equal : t -> t -> bool
(** [equal a b] returns whether [a] and [b] are equal. Timestamps that are not
    on UTC will only be equal to the values in their same memory position. *)

val compare : t -> t -> int
(** [compare a b] returns -1 if [a] is earlier than [b], 1 if [a] is later than
    [b] or which timeone is sooner. When [a] or [b] lack a timezone, UTC is
    assumed *)

val is_earlier : than:t -> t -> bool
(** [is_earlier ~than a] returns whether the timestamp [a] happens before
    [than]. When [than] or [b] lack a timezone, UTC is assumed. *)

val is_later : than:t -> t -> bool
(** [is_later ~than a] returns whether the timestamp [a] happens after [than].
    When [than] or [b] lack a timezone, UTC is assumed. *)

val diff : t -> t -> Ptime.Span.t
(** [diff a b] returns the span of time corresponding to [a - b]. When [than]
    or [b] lack a timezone, UTC is assumed. *)
