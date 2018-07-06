(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
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
 *
 *)

(** A library for manipulation of IP address representations.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** Raised when parsing of IP address syntax fails. *)
exception Parse_error of string * string [@@deriving sexp]

(** Type of ordered address scope classifications *)
type scope =
| Point
| Interface
| Link
| Admin
| Site
| Organization
| Global
[@@deriving sexp]

(** A collection of functions for IPv4 addresses. *)
module V4 : sig
  (** Type of the internet protocol v4 address of a host *)
  type t [@@deriving sexp]
  val compare : t -> t -> int

  (** Converts the low bytes of four int values into an abstract {! V4.t }. *)
  val make : int -> int -> int -> int -> t

  (** {3 Text string conversion} *)

  (** [of_string_exn ipv4_string] is the address represented
      by [ipv4_string]. Raises [Parse_error] if [ipv4_string] is not a
      valid representation of an IPv4 address. *)
  val of_string_exn : string -> t

  (** Same as [of_string_exn] but returns an option type instead of raising
      an exception. *)
  val of_string : string -> t option

  (** Same as [of_string_exn] but takes as an extra argument the offset
      into the string for reading. *)
  val of_string_raw : string -> int ref -> t

  (** [to_string ipv4] is the dotted decimal string representation
      of [ipv4], i.e. XXX.XX.X.XXX. *)
  val to_string : t -> string

  (** [to_buffer buf ipv4] writes the string representation of [ipv4] into the
      buffer [buf]. *)
  val to_buffer : Buffer.t -> t -> unit

  (** [pp_hum f ipv4] outputs a human-readable representation of [ipv4] to
      the formatter [f]. *)
  val pp_hum : Format.formatter -> t -> unit

  (** {3 Bytestring conversion} *)

  (** [of_bytes_exn ipv4_octets] is the address represented
      by [ipv4_octets]. Raises [Parse_error] if [ipv4_octets] is not a
      valid representation of an IPv4 address. *)
  val of_bytes_exn : string -> t

  (** Same as [of_bytes_exn] but returns an option type instead of raising
      an exception. *)
  val of_bytes : string -> t option

  (** Same as [of_bytes_exn] but take an extra paramenter, the offset into
      the bytes for reading. *)
  val of_bytes_raw : string -> int -> t

  (** [to_bytes ipv4] is a string of length 4 encoding [ipv4]. *)
  val to_bytes : t -> string

  (** [to_bytes_raw ipv4 bytes offset] writes the 4 byte encoding of [ipv4]
      into [bytes] at offset [offset]. *)
  val to_bytes_raw : t -> Bytes.t -> int -> unit

  (** {3 Int conversion} *)

  (** [of_int32 ipv4_packed] is the address represented by
      [ipv4_packed]. *)
  val of_int32 : int32 -> t

  (** [to_int32 ipv4] is the 32-bit packed encoding of [ipv4]. *)
  val to_int32 : t -> int32

  (** [of_int16 ipv4_packed] is the address represented by
      [ipv4_packed]. *)
  val of_int16 : (int * int) -> t

  (** [to_int16 ipv4] is the 16-bit packed encoding of [ipv4]. *)
  val to_int16 : t -> int * int

  (** {3 MAC conversion} *)

  (** [multicast_to_mac ipv4] is the MAC address corresponding to the
      multicast address [ipv4]. Described by
      {{:http://tools.ietf.org/html/rfc1112#section-6.2}RFC 1112}. *)
  val multicast_to_mac : t -> Macaddr.t

  (** {3 Host conversion} *)

  (** [to_domain_name ipv4] is the domain name label list for reverse
      lookups of [ipv4]. This includes the [.in-addr.arpa.] suffix. *)
  val to_domain_name : t -> string list

  (** {3 Common addresses} *)

  (** [any] is 0.0.0.0. *)
  val any : t

  (** [unspecified] is 0.0.0.0. *)
  val unspecified : t

  (** [broadcast] is 255.255.255.255. *)
  val broadcast : t

  (** [nodes] is 224.0.0.1. *)
  val nodes : t

  (** [routers] is 224.0.0.2. *)
  val routers : t

  (** [localhost] is 127.0.0.1. *)
  val localhost : t

  (** A module for manipulating IPv4 network prefixes. *)
  module Prefix : sig
    type addr = t [@@deriving sexp]

    (** Type of a internet protocol subnet *)
    type t [@@deriving sexp]

    val compare : t -> t -> int

    (** [mask n] is the pseudo-address of an [n] bit subnet mask. *)
    val mask : int -> addr

    (** [make n addr] is the [n] bit subnet prefix to which [addr] belongs. *)
    val make : int -> addr -> t

    (** [network_address prefix addr] is the address with prefix [prefix]
        and suffix from [addr].
        See <http://tools.ietf.org/html/rfc4291#section-2.3>. *)
    val network_address : t -> addr -> addr

    (** [of_string_exn cidr] is the subnet prefix represented by the CIDR
        string, [cidr]. Raises [Parse_error] if [cidr] is not a valid
        representation of a CIDR notation routing prefix. *)
    val of_string_exn : string -> t

    (** Same as [of_string_exn] but returns an option type instead of raising
        an exception. *)
    val of_string : string -> t option

    (** Same as [of_string_exn] but takes as an extra argument the offset
        into the string for reading. *)
    val of_string_raw : string -> int ref -> t

    (** [to_string prefix] is the CIDR notation string representation
        of [prefix], i.e. XXX.XX.X.XXX/XX. *)
    val to_string : t -> string

    (** [pp_hum f prefix] outputs a human-readable representation of [prefix]
        to the formatter [f]. *)
    val pp_hum : Format.formatter -> t -> unit

    (** [of_address_string_exn cidr_addr] is the address and prefix
        represented by [cidr_addr]. Raises [Parse_error] if [cidr_addr] is not
        a valid representation of a CIDR-scoped address. *)
    val of_address_string_exn : string -> t * addr

    (** Same as [of_address_string_exn] but returns an option type instead of
        raising an exception. *)
    val of_address_string : string -> (t * addr) option

    (** [to_address_string prefix addr] is the network address
        constructed from [prefix] and [addr]. *)
    val to_address_string : t -> addr -> string

    (** [to_buffer buf prefix] writes the string representation
        of [prefix] into the buffer [buf]. *)
    val to_buffer : Buffer.t -> t -> unit

    (** [to_address_buffer buf prefix addr] writes string representation of the
        network address representing [addr] in [prefix] to the buffer [buf]. *)
    val to_address_buffer : Buffer.t -> t -> addr -> unit

    (** [of_netmask netmask addr] is the subnet prefix of [addr] with netmask
        [netmask]. *)
    val of_netmask : addr -> addr -> t

    (** [mem ip subnet] checks whether [ip] is found within [subnet]. *)
    val mem : addr -> t -> bool

    (** [subset ~subnet ~network] checks whether [subnet] is contained
        within [network]. *)
    val subset : subnet:t -> network:t -> bool

    (** [of_addr ip] create a subnet composed of only one address, [ip].
        It is the same as [make 32 ip]. *)
    val of_addr : addr -> t

    (** The default route, all addresses in IPv4-space, 0.0.0.0/0. *)
    val global    : t

    (** The host loopback network, 127.0.0.0/8. *)
    val loopback  : t

    (** The local-link network, 169.254.0.0/16. *)
    val link      : t

    (** The relative addressing network, 0.0.0.0/8. *)
    val relative  : t

    (** The multicast network, 224.0.0.0/4. *)
    val multicast : t

    (** The private subnet with 10 as first octet, 10.0.0.0/8. *)
    val private_10  : t

    (** The private subnet with 172 as first octet, 172.16.0.0/12. *)
    val private_172 : t

    (** The private subnet with 192 as first octet, 192.168.0.0/16. *)
    val private_192 : t

    (** The privately addressable networks: [loopback], [link],
        [private_10], [private_172], [private_192]. *)
    val private_blocks : t list

    (** [broadcast subnet] is the broadcast address for [subnet]. *)
    val broadcast : t -> addr

    (** [network subnet] is the address for [subnet]. *)
    val network : t -> addr

    (** [netmask subnet] is the netmask for [subnet]. *)
    val netmask : t -> addr

    (** [bits subnet] is the bit size of the [subnet] prefix. *)
    val bits : t -> int

    include Map.OrderedType with type t := t
  end

  (** [scope ipv4] is the classification of [ipv4] by the {! scope }
      hierarchy. *)
  val scope : t -> scope

  (** [is_global ipv4] is a predicate indicating whether [ipv4] globally
      addresses a node. *)
  val is_global : t -> bool

  (** [is_multicast ipv4] is a predicate indicating whether [ipv4] is a
      multicast address. *)
  val is_multicast : t -> bool

  (** [is_private ipv4] is a predicate indicating whether [ipv4] privately
      addresses a node. *)
  val is_private : t -> bool

  include Map.OrderedType with type t := t
end


(** A collection of functions for IPv6 addresses. *)
module V6 : sig
  (** Type of the internet protocol v6 address of a host *)
  type t [@@deriving sexp]
  val compare : t -> t -> int

  (** Converts the low bytes of eight int values into an abstract
      {! V6.t }. *)
  val make : int -> int -> int -> int -> int -> int -> int -> int -> t

  (** {3 Text string conversion} *)

  (** [of_string_exn ipv6_string] is the address represented
      by [ipv6_string]. Raises [Parse_error] if [ipv6_string] is not a
      valid representation of an IPv6 address. *)
  val of_string_exn : string -> t

  (** Same as [of_string_exn] but returns an option type instead of raising
      an exception. *)
  val of_string : string -> t option

  (** Same as [of_string_exn] but takes as an extra argument the offset into
      the string for reading. *)
  val of_string_raw : string -> int ref -> t

  (** [to_string ipv6] is the string representation of [ipv6],
      i.e. XXX:XX:X::XXX:XX. *)
  val to_string : ?v4:bool -> t -> string

  (** [to_buffer buf ipv6] writes the string representation of [ipv6] into the
      buffer [buf]. *)
  val to_buffer : ?v4:bool -> Buffer.t -> t -> unit

  (** [pp_hum f ipv6] outputs a human-readable representation of [ipv6] to
      the formatter [f]. *)
  val pp_hum : Format.formatter -> t -> unit

  (** {3 Bytestring conversion} *)

  (** [of_bytes_exn ipv6_octets] is the address represented
      by [ipv6_octets]. Raises [Parse_error] if [ipv6_octets] is not a
      valid representation of an IPv6 address. *)
  val of_bytes_exn : string -> t

  (** Same as [of_bytes_exn] but returns an option type instead of raising
      an exception. *)
  val of_bytes : string -> t option

  (** Same as [of_bytes_exn] but takes an extra paramenter, the offset into
      the bytes for reading. *)
  val of_bytes_raw : string -> int -> t

  (** [to_bytes ipv6] is a string of length 16 encoding [ipv6]. *)
  val to_bytes : t -> string

  (** [to_bytes_raw ipv6 bytes offset] writes the 16 bytes encoding of [ipv6]
      into [bytes] at offset [offset]. *)
  val to_bytes_raw : t -> Bytes.t -> int -> unit

  (** {3 Int conversion} *)

  (** [of_int64 (ho, lo)] is the IPv6 address represented by two int64. *)
  val of_int64 : int64 * int64 -> t
  (** [to_int64 ipv6] is the 128-bit packed encoding of [ipv6]. *)
  val to_int64 : t -> int64 * int64

  (** [of_int32 (a, b, c, d)] is the IPv6 address represented by four int32. *)
  val of_int32 : int32 * int32 * int32 * int32 -> t
  (** [to_int32 ipv6] is the 128-bit packed encoding of [ipv6]. *)
  val to_int32 : t -> int32 * int32 * int32 * int32

  (** [of_int16 (a, b, c, d, e, f, g, h)] is the IPv6 address represented by
      eight 16-bit int. *)
  val of_int16 : int * int * int * int * int * int * int * int -> t
  (** [to_int16 ipv6] is the 128-bit packed encoding of [ipv6]. *)
  val to_int16 : t -> int * int * int * int * int * int * int * int

  (** {3 MAC conversion} *)

  (** [multicast_to_mac ipv6] is the MAC address corresponding to the
      multicast address [ipv6]. Described by
      {{:https://tools.ietf.org/html/rfc2464#section-7}RFC 2464}. *)
  val multicast_to_mac : t -> Macaddr.t

  (** {3 Host conversion} *)

  (** [to_domain_name ipv6] is the domain name label list for reverse
      lookups of [ipv6]. This includes the [.ip6.arpa.] suffix. *)
  val to_domain_name : t -> string list

  (** {3 Common addresses} *)

  (** [unspecified] is ::. *)
  val unspecified : t

  (** [localhost] is ::1. *)
  val localhost : t

  (** [interface_nodes] is ff01::01. *)
  val interface_nodes : t

  (** [link_nodes] is ff02::01. *)
  val link_nodes : t

  (** [interface_routers] is ff01::02. *)
  val interface_routers : t

  (** [link_routers] is ff02::02. *)
  val link_routers : t

  (** [site_routers] is ff05::02. *)
  val site_routers : t

  (** A module for manipulating IPv6 network prefixes. *)
  module Prefix : sig
    type addr = t [@@deriving sexp]

    (** Type of a internet protocol subnet *)
    type t [@@deriving sexp]

    val compare : t -> t -> int

    (** [mask n] is the pseudo-address of an [n] bit subnet mask. *)
    val mask : int -> addr

    (** [make n addr] is the [n] bit subnet prefix to which [addr] belongs. *)
    val make : int -> addr -> t

    (** [network_address prefix addr] is the address with prefix [prefix]
        and suffix from [addr].
        See <http://tools.ietf.org/html/rfc4291#section-2.3>. *)
    val network_address : t -> addr -> addr

    (** [of_string_exn cidr] is the subnet prefix represented by the CIDR
        string, [cidr]. Raises [Parse_error] if [cidr] is not a valid
        representation of a CIDR notation routing prefix. *)
    val of_string_exn : string -> t

    (** Same as [of_string_exn] but returns an option type instead of raising
        an exception. *)
    val of_string : string -> t option

    (** Same as [of_string_exn] but takes as an extra argument the offset
        into the string for reading. *)
    val of_string_raw : string -> int ref -> t

    (** [to_string prefix] is the CIDR notation string representation
        of [prefix], i.e. XXX:XX:X::XXX/XX. *)
    val to_string : t -> string

    (** [pp_hum f prefix] outputs a human-readable representation of [prefix]
        to the formatter [f]. *)
    val pp_hum : Format.formatter -> t -> unit

    (** [of_address_string_exn cidr_addr] is the address and prefix
        represented by [cidr_addr]. Raises [Parse_error] if [cidr_addr] is not
        a valid representation of a CIDR-scoped address. *)
    val of_address_string_exn : string -> t * addr

    (** Same as [of_address_string_exn] but returns an option type instead of
        raising an exception. *)
    val of_address_string : string -> (t * addr) option

    (** [to_address_string prefix addr] is the network address
        constructed from [prefix] and [addr]. *)
    val to_address_string : t -> addr -> string

    (** [to_buffer buf prefix] writes the string representation
        of [prefix] to the buffer [buf]. *)
    val to_buffer : Buffer.t -> t -> unit

    (** [to_address_buffer buf prefix addr] writes string representation of the
        network address representing [addr] in [prefix] to the buffer [buf]. *)
    val to_address_buffer : Buffer.t -> t -> addr -> unit

    (** [of_netmask netmask addr] is the subnet prefix of [addr] with netmask
        [netmask]. *)
    val of_netmask : addr -> addr -> t

    (** [mem ip subnet] checks whether [ip] is found within [subnet]. *)
    val mem : addr -> t -> bool

    (** [subset ~subnet ~network] checks whether [subnet] is contained
        within [network]. *)
    val subset : subnet:t -> network:t -> bool

    (** [of_addr ip] create a subnet composed of only one address, [ip].
        It is the same as [make 128 ip]. *)
    val of_addr : addr -> t

    (** Global Unicast 001, 2000::/3. *)
    val global_unicast_001 : t

    (** The Unique Local Unicast (ULA), fc00::/7. *)
    val unique_local       : t

    (** Link-Local Unicast, fe80::/64. *)
    val link               : t

    (** The multicast network, ff00::/8. *)
    val multicast          : t

    (** IPv4-mapped addresses, ::ffff:0:0/96. *)
    val ipv4_mapped        : t

    (** Global Unicast addresses that don't use Modified EUI64 interface
        identifiers, ::/3. *)
    val noneui64_interface : t

    (** Solicited-Node multicast addresses *)
    val solicited_node : t

    (** [network subnet] is the address for [subnet]. *)
    val network : t -> addr

    (** [netmask subnet] is the netmask for [subnet]. *)
    val netmask : t -> addr

    (** [bits subnet] is the bit size of the [subnet] prefix. *)
    val bits : t -> int

    include Map.OrderedType with type t := t
  end

  (** [scope ipv6] is the classification of [ipv6] by the {! scope }
      hierarchy. *)
  val scope : t -> scope

  (** [link_address_of_mac mac] is the link-local address for an
      Ethernet interface derived by the IEEE MAC -> EUI-64 map with
      the Universal/Local bit complemented for IPv6.
      @see <https://tools.ietf.org/html/rfc2464#section-4> RFC 2464
  *)
  val link_address_of_mac : Macaddr.t -> t

  (** [is_global ipv6] is a predicate indicating whether [ipv6] globally
      addresses a node. *)
  val is_global : t -> bool

  (** [is_multicast ipv6] is a predicate indicating whether [ipv6] is a
      multicast address. *)
  val is_multicast : t -> bool

  (** [is_private ipv6] is a predicate indicating whether [ipv6] privately
      addresses a node. *)
  val is_private : t -> bool

  include Map.OrderedType with type t := t
end

(** Type of either an IPv4 value or an IPv6 value *)
type ('v4,'v6) v4v6 = V4 of 'v4 | V6 of 'v6 [@@deriving sexp]

(** Type of any IP address *)
type t = (V4.t,V6.t) v4v6 [@@deriving sexp]

val compare : t -> t -> int

(** [to_string addr] is the text string representation of [addr]. *)
val to_string : t -> string

(** [to_buffer buf addr] writes the text string representation of [addr] into
    [buf]. *)
val to_buffer : Buffer.t -> t -> unit

(** [pp_hum f ip] outputs a human-readable representation of [ip] to the
    formatter [f]. *)
val pp_hum : Format.formatter -> t -> unit

(** [of_string_exn s] parses [s] as an IPv4 or IPv6 address.
    Raises [Parse_error] if [s] is not a valid string representation of an IP
    address. *)
val of_string_exn : string -> t

(** Same as [of_string_exn] but returns an option type instead of raising an
    exception. *)
val of_string : string -> t option

(** Same as [of_string_exn] but takes as an extra argument the offset into
    the string for reading. *)
val of_string_raw : string -> int ref -> t

(** [v4_of_v6 ipv6] is the IPv4 representation of the IPv6 address [ipv6].
    If [ipv6] is not an IPv4-mapped address, None is returned. *)
val v4_of_v6 : V6.t -> V4.t option

(** [to_v4 addr] is the IPv4 representation of [addr]. *)
val to_v4 : t -> V4.t option

(** [v6_of_v4 ipv4] is the IPv6 representation of the IPv4 address [ipv4]. *)
val v6_of_v4 : V4.t -> V6.t

(** [to_v6 addr] is the IPv6 representation of [addr]. *)
val to_v6 : t -> V6.t

(** [scope addr] is the classification of [addr] by the {! scope }
    hierarchy. *)
val scope : t -> scope

(** [is_global addr] is a predicate indicating whether [addr] globally
    addresses a node. *)
val is_global : t -> bool

(** [is_multicast addr] is a predicate indicating whether [addr] is a
    multicast address. *)
val is_multicast : t -> bool

(** [is_private addr] is a predicate indicating whether [addr] privately
    addresses a node. *)
val is_private : t -> bool

(** [multicast_to_mac addr] is the MAC address corresponding to the
    multicast address [addr]. See {!V4.multicast_to_mac} and
    {!V6.multicast_to_mac}.*)
val multicast_to_mac : t -> Macaddr.t

(** [to_domain_name addr] is the domain name label list for reverse
    lookups of [addr]. This includes the [.arpa.] suffix. *)
val to_domain_name : t -> string list

module Prefix : sig
  type addr = t [@@deriving sexp]

  (** Type of a internet protocol subnet *)
  type t = (V4.Prefix.t, V6.Prefix.t) v4v6 [@@deriving sexp]

  val compare : t -> t -> int

  (** [to_string subnet] is the text string representation of [subnet]. *)
  val to_string : t -> string

  (** [to_buffer buf subnet] writes the text string representation of [subnet]
      into [buf]. *)
  val to_buffer : Buffer.t -> t -> unit

  (** [pp_hum f subnet] outputs a human-readable representation of [subnet]
      to the formatter [f]. *)
  val pp_hum : Format.formatter -> t -> unit

  (** [of_string_exn cidr] is the subnet prefix represented by the CIDR
      string, [cidr]. Raises [Parse_error] if [cidr] is not a valid
      representation of a CIDR notation routing prefix. *)
  val of_string_exn : string -> t

  (** Same as [of_string_exn] but returns an option type instead of raising
      an exception. *)
  val of_string     : string -> t option

  (** Same as [of_string_exn] but takes as an extra argument the offset
      into the string for reading. *)
  val of_string_raw : string -> int ref -> t

  (** [v4_of_v6 ipv6] is the IPv4 representation of the IPv6 subnet [ipv6].
      If [ipv6] is not an IPv4-mapped subnet, None is returned. *)
  val v4_of_v6 : V6.Prefix.t -> V4.Prefix.t option

  (** [to_v4 subnet] is the IPv4 representation of [subnet]. *)
  val to_v4 : t -> V4.Prefix.t option

  (** [v6_of_v4 ipv4] is the IPv6 representation of the IPv4 subnet [ipv4]. *)
  val v6_of_v4 : V4.Prefix.t -> V6.Prefix.t

  (** [to_v6 subnet] is the IPv6 representation of [subnet]. *)
  val to_v6 : t -> V6.Prefix.t

  (** [mem ip subnet] checks whether [ip] is found within [subnet]. *)
  val mem : addr -> t -> bool

  (** [subset ~subnet ~network] checks whether [subnet] is contained
      within [network]. *)
  val subset : subnet:t -> network:t -> bool

  (** [of_addr ip] create a subnet composed of only one address, [ip].*)
  val of_addr : addr -> t

  (** [network subnet] is the address for [subnet]. *)
  val network : t -> addr

  (** [netmask subnet] is the netmask for [subnet]. *)
  val netmask : t -> addr

  include Map.OrderedType with type t := t
end

include Map.OrderedType with type t := t
