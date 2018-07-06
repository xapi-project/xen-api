(*---------------------------------------------------------------------------
   Copyright (c) 2008 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Universally unique identifiers (UUIDs).

    [Uuidm] implements 128 bits universally unique identifiers version
    3, 5 (name based with MD5, SHA-1 hashing) and 4 (random based)
    according to {{:http://tools.ietf.org/html/rfc4122}RFC 4122}.

    {3 References}
    {ul
    {- P. Leach et al.
    {e {{:http://tools.ietf.org/html/rfc4122}A universally unique identifier
     (UUID) URN Namespace}}, 2005.}}

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 UUIDs} *)

type t
(** The type for UUIDs. *)

type version =
  [ `V3 of t * string (** Name based with MD5 hashing *)
  | `V4 (** Random based *)
  | `V5 of t * string (** Name based with SHA-1 hasing *) ]
(** The type for UUID versions and generation parameters. [`V3] and [`V5]
    specify a namespace and a name for the generation. [`V4] is random based
    with a private state seeded with [Random.State.make_self_init], use
    {!v4_gen} to specify your own seed. *)

val v : version -> t
(** [v version] is an UUID of the given [version]. *)

(**/**)
(* Deprecated *)
val create : version -> t
(**/**)

val v3 : t -> string -> t
(** [v3 ns n] is [create `V3 (ns, n)]. *)

val v5 : t -> string -> t
(** [v5 ns n] is [create `V5 (ns, n)]. *)

val v4_gen : Random.State.t -> (unit -> t)
(** [v4 seed] is a function that generates random version 4 UUIDs with
    the given [seed]. *)

(** {1 Constants} *)

val nil : t
(** [nil] is the nil UUID. *)

val ns_dns : t
(** [ns_dns] is the DNS namespace UUID. *)

val ns_url : t
(** [ns_url] is the URL namespace UUID. *)

val ns_oid : t
(** [ns_oid] is the ISO OID namespace UUID. *)

val ns_X500 : t
(** [ns_dn] is the X.500 DN namespace UUID. *)

(** {1 Comparing} *)

val compare : t -> t -> int
(** [compare u u'] totally orders [u] and [u']. *)

val equal : t -> t -> bool
(** [equal u u'] is [true] iff [u] and [u'] are equal. *)

(** {1 Conversion with UUID binary representation} *)

val of_bytes : ?pos:int -> string -> t option
(** [of_bytes pos s] is the UUID represented by the 16 bytes starting
    at [pos] (defaults to [0]) in [s].  Returns [None] if the string
    is not long enough. *)

val to_bytes : t -> string
(** [to_bytes u] is [u] as a 16 bytes long string. *)

(**/**)
val unsafe_to_bytes : t -> string
(**/**)

(** {1 Conversion with UUID US-ASCII representation} *)

val of_string : ?pos:int -> string -> t option
(** [of_string pos s] converts the substring of [s] starting at [pos]
    (defaults to [0]) of the form ["XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"]
    where X is a lower or upper case hexadecimal number to an
    UUID. Returns [None] if a parse error occured.  *)

val to_string : ?upper:bool -> t -> string
(** [to_string u] is [u] as a string of the form
    ["XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"] where X is
    a lower case hexadecimal number (or upper if [upper] is
    [true]). *)

(** {1 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf u] formats [u] on [ppf] like {!to_string} would do. It is
    unspecified whether upper or lower case hexadecimal numbers are used. *)

val pp_string : ?upper:bool -> Format.formatter -> t -> unit
(** [pp_string ?upper ppf u] formats [u] on [ppf] like {!to_string} would
    do. *)

(**/**)
(* Deprecated *)
val print : ?upper:bool -> Format.formatter -> t -> unit
(**/**)


(*---------------------------------------------------------------------------
   Copyright (c) 2008 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
