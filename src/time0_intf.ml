open! Import
open Std_internal

module type Basic = sig
  module Span : Span_intf.S

  type t

  module Replace_polymorphic_compare : Comparable.Polymorphic_compare with type t := t

  include Comparable.Polymorphic_compare with type t := t
  include Robustly_comparable            with type t := t

  val add  : t -> Span.t -> t
  val sub  : t -> Span.t -> t
  val diff : t -> t -> Span.t

  (** [next t] returns the next t (forwards in time) *)
  val next : t -> t

  (** [prev t] returns the previous t (backwards in time) *)
  val prev : t -> t

  val to_span_since_epoch : t -> Span.t
  val of_span_since_epoch : Span.t -> t
end

module type S = sig
  type underlying
  type t = private underlying [@@deriving bin_io, compare, hash, typerep]

  module Span  : Span_intf.S  with type underlying =  underlying
  module Ofday : Ofday_intf.S with type underlying := underlying and module Span := Span

  include Basic
    with type t := t
     and module Span := Span

  include Comparable.S_common
    with type t := t
     and module Replace_polymorphic_compare := Replace_polymorphic_compare

  (** Represents a timezone-relative time, rather than an absolute time.  This is
      equivalent to a [Date.t] and an [Ofday.t] with no time zone. A
      [Relative_to_unspecified_zone.t] does not correspond to a single, unambiguous point
      in time. Intended as a low-level back-end for high-level timezone-based functions;
      most clients should not use [Relative_to_unspecified_zone.t]. *)
  module Relative_to_unspecified_zone : sig
    type absolute = t
    type t = private underlying

    (** [to_span_since_epoch] and [of_span_since_epoch] don't precisely mean the UNIX
        epoch as a moment in time, but rather the timezone-relative date-ofday pair
        1970-01-01 00:00:00.

        Likewise [add] and [sub] and [diff] all have slightly subtle meanings, where the
        [Time.Span.t] values involved don't necessarily always translate exactly to an
        elapsed period of time. (You can add 2h to a timezone-relative time and get one
        that occurs only 1h later in real-time terms, or 3h later). *)
    include Basic with type t := t and module Span := Span

    (** Conversions between relative times and date/ofday. *)
    val of_date_ofday : Date0.t -> Ofday.t -> t
    val to_date_ofday : t -> Date0.t * Ofday.t
    val to_date       : t -> Date0.t
    val to_ofday      : t -> Ofday.t

    (** Conversions between absolute and relative time, based on the offset from UTC at
        the given time. Use the high-level [Time.Zone] wrappers of these conversions. *)
    val of_absolute : absolute -> offset_from_utc:Span.t -> t
    val to_absolute : t -> offset_from_utc:Span.t -> absolute
  end with type absolute := t

  val now : unit -> t
end
