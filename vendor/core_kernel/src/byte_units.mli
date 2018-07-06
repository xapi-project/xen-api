(** Conversions between units of measure that are based on bytes (like kilobytes,
    megabytes, gigabytes, and words).

    [t]'s are created with [create measure value], as in
    [Byte_units.create `Megabytes 100]. *)

open! Import

module Measure : sig
  type t = [ `Bytes | `Kilobytes | `Megabytes | `Gigabytes | `Words ]
  [@@deriving sexp, bin_io]
end

type t [@@deriving bin_io, sexp]

(** [create measure value] creates a [t] from [value] units of the given measure. *)
val create : Measure.t -> float -> t

include Comparable.S with type t := t
include Hashable  .S with type t := t
include Stringable.S with type t := t

(** [to_string_hum ?measure t] returns a string representation of [t].  If [measure] is
    not given then the largest measure (excluding [`Words]) is used that causes the
    translated value to exceed 1.

    For example [Byte_units.to_string_hum (Byte_units.create `Bytes 1000.)] gives [1000b],
    but [Byte_units.to_string_hum (Byte_units.create `Bytes 1500.)] gives [1.46484k]. *)
val to_string_hum : ?measure:Measure.t -> t -> string

val bytes     : t -> float
val kilobytes : t -> float
val megabytes : t -> float
val gigabytes : t -> float
val words     : t -> float

(** [scale t mul] scale the measure [t] by [mul] *)
val scale : t -> float -> t

module Infix : sig
  val ( - ) : t -> t -> t
  val ( + ) : t -> t -> t

  (** [( / ) t mul] scales [t] by [1/mul] *)
  val (/)   : t -> float -> t

  (** [( // ) t1 t2] returns the ratio of t1 to t2 *)
  val (//)  : t -> t -> float
end

module Stable : sig
  module V1 : Stable_module_types.S0_without_comparator with type t = t
end
