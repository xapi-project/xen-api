(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** A library for manipulation of MAC address representations.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** Raised when parsing of MAC address syntax fails. *)
exception Parse_error of string * string [@@deriving sexp]

(** Type of the hardware address (MAC) of an ethernet interface. *)
type t [@@deriving sexp]

val compare : t -> t -> int

(** Functions converting MAC addresses to bytes/string and vice
    versa. *)

(** [of_bytes_exn buf] is the hardware address extracted from
    [buf]. Raises [Parse_error] if [buf] has not length 6. *)
val of_bytes_exn : string -> t

(** Same as above but returns an option type instead of raising an
    exception. *)
val of_bytes : string -> t option

(** [of_string_exn mac_string] is the hardware address represented by
    [mac_string]. Raises [Parse_error] if [mac_string] is not a
    valid representation of a MAC address. *)
val of_string_exn : string -> t

(** Same as above but returns an option type instead of raising an
    exception. *)
val of_string : string -> t option

(** [to_bytes mac_addr] is a string of size 6 encoding [mac_addr]. *)
val to_bytes : t -> string

(** [to_string ?(sep=':') mac_addr] is the [sep]-separated string representation
    of [mac_addr], i.e. xx:xx:xx:xx:xx:xx. *)
val to_string : ?sep:char -> t -> string

(** [broadcast] is ff:ff:ff:ff:ff:ff. *)
val broadcast : t

(** [make_local bytegen] creates a unicast, locally administered MAC
    address given a function mapping octet offset to octet value. *)
val make_local : (int -> int) -> t

(** [get_oui macaddr] is the integer organization identifier for [macaddr]. *)
val get_oui : t -> int

(** [is_local macaddr] is the predicate on the locally administered bit
    of [macaddr]. *)
val is_local : t -> bool

(** [is_unicast macaddr] the is the predicate on the unicast bit of
    [macaddr]. *)
val is_unicast : t -> bool

include Map.OrderedType with type t := t
