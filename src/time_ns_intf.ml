open! Import
open Std_internal

module type Span = sig
  (** [t] is immediate on 64bit boxes and so plays nicely with the GC write barrier.
      Unfortunately, [private Int63.t] is necessary for the compiler to optimize uses. *)
  type t = private Int63.t [@@deriving hash, typerep, bin_io]

  include Comparisons.S        with type t := t
  include Comparable.Validate  with type t := t
  include Comparable.With_zero with type t := t

  val nanosecond  : t
  val microsecond : t
  val millisecond : t
  val second      : t
  val minute      : t
  val hour        : t
  val day         : t

  val of_ns  : float -> t
  val of_us  : float -> t
  val of_ms  : float -> t
  val of_sec : float -> t
  val of_min : float -> t
  val of_hr  : float -> t
  val of_day : float -> t
  val to_ns  : t     -> float
  val to_us  : t     -> float
  val to_ms  : t     -> float
  val to_sec : t     -> float
  val to_min : t     -> float
  val to_hr  : t     -> float
  val to_day : t     -> float

  val of_sec_with_microsecond_precision : float -> t

  val of_int_us  : int -> t
  val of_int_ms  : int -> t
  val of_int_sec : int -> t
  val to_int_us  : t -> int
  val to_int_ms  : t -> int
  val to_int_sec : t -> int

  val zero : t

  (** The limits of [t] are chosen to allow conversion to and from [float] spans with
      microsecond precision.  This property supports {!Core.Timing_wheel_float} in
      particular.  See also {!Core.Time}. *)
  val min_value : t
  val max_value : t

  val ( + ) : t -> t -> t (** overflows silently                *)

  val ( - ) : t -> t -> t (** overflows silently                *)

  val abs   : t -> t      (** overflows silently on [min_value] *)

  val neg   : t -> t      (** overflows silently on [min_value] *)

  val scale       : t -> float        -> t
  val scale_int   : t -> int          -> t (** overflows silently *)

  val scale_int63 : t -> Int63.t -> t (** overflows silently *)

  val div    : t -> t     -> Int63.t
  val ( / )  : t -> float -> t
  val ( // ) : t -> t     -> float

  (** Overflows silently. *)
  val create
    :  ?sign : Sign.t
    -> ?day : int
    -> ?hr  : int
    -> ?min : int
    -> ?sec : int
    -> ?ms  : int
    -> ?us  : int
    -> ?ns  : int
    -> unit
    -> t

  (** Similar to {!Time.Span.Parts}. *)
  module Parts : Span_intf.Parts

  val to_parts : t -> Parts.t

  val to_unit_of_time : t -> Unit_of_time.t
  val of_unit_of_time : Unit_of_time.t -> t

  include Robustly_comparable with type t := t

  (** Fast, implemented as the identity function. *)
  val to_int63_ns : t -> Int63.t

  (** Fast, implemented as the identity function. *)
  val of_int63_ns : Int63.t -> t

  (** Will raise on 32-bit platforms.  Consider [to_int63_ns] instead. *)
  val to_int_ns : t   -> int
  val of_int_ns : int -> t

  (** The only condition [to_proportional_float] is supposed to satisfy is that for all
      [t1, t2 : t]: [to_proportional_float t1 /. to_proportional_float t2 = t1 // t2]. *)
  val to_proportional_float : t -> float

  val since_unix_epoch : unit -> t

  val random : ?state:Random.State.t -> unit -> t

  (** Note that we expose a sexp format that is not the one exposed in [Core]. *)
  module Alternate_sexp : sig
    type nonrec t = t [@@deriving sexp]
  end
end

(** Time represented as an [Int63.t] number of nanoseconds since the epoch.

    See {!Core.Time_ns} for important user documentation.

    Internally, arithmetic is not overflow-checked.  Instead, overflows are silently
    ignored, as for [int] arithmetic.  Conversions may (or may not) raise if prior
    arithmetic operations overflowed. *)
module type Time_ns = sig

  module Span : Span

  type t = private Int63.t [@@deriving hash, typerep, bin_io]

  include Comparisons.S with type t := t

  (** Note that we expose a sexp format that is not the one exposed in [Core]. *)
  module Alternate_sexp : sig
    type nonrec t = t [@@deriving sexp]
  end

  val epoch : t (** Unix epoch (1970-01-01 00:00:00 UTC) *)

  val min_value : t
  val max_value : t

  val now : unit -> t

  val add : t -> Span.t -> t (** overflows silently *)

  val sub : t -> Span.t -> t (** overflows silently *)

  val diff     : t -> t -> Span.t (** overflows silently *)

  val abs_diff : t -> t -> Span.t (** overflows silently *)

  val to_span_since_epoch : t -> Span.t
  val of_span_since_epoch : Span.t -> t

  val to_int63_ns_since_epoch : t -> Int63.t
  val of_int63_ns_since_epoch : Int63.t -> t

  (** Will raise on 32-bit platforms.  Consider [to_int63_ns_since_epoch] instead. *)
  val to_int_ns_since_epoch : t -> int
  val of_int_ns_since_epoch : int -> t


  (** [next_multiple ~base ~after ~interval] returns the smallest [time] of the form:

      {[
        time = base + k * interval
      ]}

      where [k >= 0] and [time > after].  It is an error if [interval <= 0].

      Supplying [~can_equal_after:true] allows the result to satisfy [time >= after].

      Overflows silently. *)
  val next_multiple
    :  ?can_equal_after:bool  (** default is [false] *)
    -> base:t
    -> after:t
    -> interval:Span.t
    -> unit
    -> t

  val random : ?state:Random.State.t -> unit -> t

  module Utc : sig
    (** [to_date_and_span_since_start_of_day] computes the date and intraday-offset of a
        time in UTC.  It may be slower than [Core.Time_ns.to_date_ofday], as this
        function does not cache partial results while the latter does. *)
    val to_date_and_span_since_start_of_day : t -> Date0.t * Span.t

    (** The inverse of [to_date_and_span_since_start_of_day]. *)
    val of_date_and_span_since_start_of_day : Date0.t -> Span.t -> t
  end

  module Stable : sig
    module Alternate_sexp : sig
      module V1 : Stable_without_comparator with type t = Alternate_sexp.t
    end
  end

end
