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
(** Additional types and functions for dates *)

(** {2 ISO 8601 Dates} *)

(** An ISO-8601 date/time type. *)
type iso8601

val of_ptime : Ptime.t -> iso8601
(** Convert ptime to time in UTC *)

val to_ptime : iso8601 -> Ptime.t
(** Convert date/time to a ptime value: the number of seconds since 00:00:00
    UTC, 1 Jan 1970. Assumes the underlying iso8601 is in UTC *)

val of_unix_time : float -> iso8601
(** Convert calendar time [x] (as returned by e.g. Unix.time), to time in UTC. *)

val to_unix_time : iso8601 -> float
(** Convert date/time to a unix timestamp: the number of seconds since 
    00:00:00 UTC, 1 Jan 1970. Assumes the underlying iso8601 is in UTC *)

val to_rfc3339 : iso8601 -> string
(** Convert date/time to an RFC-3339-formatted string. It also complies with
    the ISO 8601 format.*)

val of_iso8601 : string -> iso8601
(** Convert ISO 8601 formatted string to a date/time value. Does not accept a
    timezone annotated datetime - i.e. string must be UTC, and end with a Z *)

val of_float : float -> iso8601
(** Same as [of_unix_time] *)

val to_float : iso8601 -> float
(** Same as [to_unix_time] *)

val to_string : iso8601 -> string
(** Convert date/time to an ISO 8601 formatted string. *)

val of_string : string -> iso8601
(** Convert ISO 8601 formatted string to a date/time value.
  * Does not accept a timezone annotated datetime - i.e. string must be UTC, and end with a Z *)

val assert_utc : iso8601 -> unit
  [@@deprecated
    "assertions performed inside constructors, so this fn does nothing"]
(** Raises an Invalid_argument exception if the given date is not a UTC date.
 *  A UTC date is an ISO 8601 strings that ends with the character 'Z'. *)

val epoch : iso8601
(** 00:00:00 UTC, 1 Jan 1970, in UTC *)

val never : iso8601
(** Same as [epoch] *)

val now : unit -> iso8601
(** Count the number of seconds passed since 00:00:00 UTC, 1 Jan 1970, in UTC *)

val _localtime_string : Ptime.tz_offset_s option -> Ptime.t -> string
(** exposed for testing *)

val localtime : unit -> iso8601

(** {2 RFC 822 Dates} *)

(** An RFC 822 date/time type. *)
type rfc822

val rfc822_of_float : float -> rfc822
(** Convert calendar time [x] (as returned by e.g. Unix.time), to RFC 822. *)

val rfc822_to_string : rfc822 -> string
(** Convert RFC 822 date/time to a formatted string. *)

val eq : iso8601 -> iso8601 -> bool
