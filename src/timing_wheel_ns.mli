(** A timing wheel can be thought of as a set of alarms. *)

open! Import
open Timing_wheel_ns_intf

include Timing_wheel
  (*_ We would like to use [with module Time = Time_ns], but can't because that requires
    certain values to be present in [Time_ns] that aren't there. *)
  with type Time.t = Time_ns.t
  with type Time.Span.t = Time_ns.Span.t (** @inline *)
