(** This module is useful for knowing that a set amount of time has passed
    since a particular moment in time. For example, to know when pasta is
    cooked al dente. They are meant to be used by polling them. *)
type t

type countdown = Remaining of Mtime.Span.t | Expired of Mtime.Span.t

val start : duration:Mtime.Span.t -> t
(** [start ~duration] starts a timer that expires after [duration] has elapsed.
    The elapsed time is counted in monotonic time, not in POSIX time. *)

val duration : t -> Mtime.Span.t
(** [duration timer] returns the amount of time after which the timer expires,
    from the moment it was started. *)

val has_expired : t -> bool
(** [has_expired timer] returns whether [timer] has reached its duration. *)

val elapsed : t -> Mtime.Span.t
(** [elapsed timer] returns the amount of time elapsed since [timer] was
    started. *)

val remaining : t -> countdown
(** [remaining timer] returns the amount of time left until [timer] expires or
    the amount of time since it expired. *)

val shorten_by : Mtime.Span.t -> t -> t
(** [shorten_by amount timer] creates a new timer with the duration of [timer]
    shortened by [amount]. The starting time doesn't change. The duration of a
    timer cannot go below 0. When a timer has a duration of 0, it's always
    considered expired. *)

val extend_by : Mtime.Span.t -> t -> t
(** [extend_by amount timer] creates a new timer with the duration of [timer]
    delayed by [amount]. The starting time doesn't change. *)

val pp : t Fmt.t
(** [pp] pretty-prints the timer. It uses the system clock to calculate
    the elapsed time every time the timer is printed. *)

(** Mtime.Span helpers *)

val span_is_shorter : Mtime.Span.t -> than:Mtime.Span.t -> bool
(** [is_shorter dur ~than] returns whether [dur] lasts less than [than]. *)

val span_is_longer : Mtime.Span.t -> than:Mtime.Span.t -> bool
(** [is_longer dur ~than] returns whether [dur] lasts more than [than]. *)

val span_to_s : Mtime.Span.t -> float
(** [span_to_s span] converts a time span into seconds, represented by a float.
    When the span is longer than ~55 years, rounding errors appear. Avoid
    whenever possible, this is unavoidable when using Thread.wait functions and
    related. *)

val s_to_span : float -> Mtime.Span.t option
(** [s_to_span seconds] converts a float representing seconds to a timespan.
    Returns None when [seconds] is negative, is not a number or larger than
    ~104 days. Avoid whenever possible, some RPC function already use this so
    it needs to be available. *)
