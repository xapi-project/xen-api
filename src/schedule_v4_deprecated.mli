(** Extends {{!Core.Schedule_v4_deprecated}[Core.Schedule_v4_deprecated]}. *)

open! Core
open! Import

include module type of struct include Core.Schedule_v4_deprecated end (** @open *)

(** in [Transitions_and_tag_changes] equality for the tag type must be given *)
type ('tag, 'output) pipe_emit =
  | Transitions
    : ('tag, 'tag Event.transition) pipe_emit
  | Transitions_and_tag_changes
    :  ('tag -> 'tag -> bool)
    -> ('tag, [ 'tag Event.transition | 'tag Event.tag_change ]) pipe_emit

(** [to_pipe t ~start_time ~emit ?stop ()] produces a pipe containing the events from
    [to_endless_sequence ~start_time t ~emit], with [`No_change_until_at_least] filtered
    out and each event added only at or after its scheduled time. *)
val to_pipe
  : (zoned, 'tag) t
  -> start_time:Time.t
  -> emit:('tag, 'output) pipe_emit
  -> ?time_source:Time_source.t (** defaults to [Time_source.wall_clock ()] *)
  -> unit
  -> [ `Started_in_range of 'tag list * 'output Pipe.Reader.t
     | `Started_out_of_range of 'output Pipe.Reader.t ]

(** [next_event t ~event ~stop ~after ()] waits for the time of the next event matching
    [event] in [t] starting at time [after].  At that time, the resulting deferred is
    determined and filled with the time of the event.

    If [stop] becomes determined before the next event, the resulting deferred is never
    filled and the computation to find the next event stops.  If the caller intends to
    never use the returned deferred stop should be filled or the background computation
    will continue to keep the deferred alive until the event occurs.

    This function is a good choice for handling a single event during the run of a
    program, like scheduling shutdown.  If the intention is to follow along with all
    events in a schedule it is preferable to call [to_pipe] or [to_endless_sequence]
    (in the non-async module). *)
val next_event
  :  (zoned, 'tag) t
  -> event:[`Enter | `Leave]
  -> stop:unit Deferred.t
  -> ?time_source:Time_source.t (** defaults to [Time_source.wall_clock ()] *)
  -> ?after:Time.t              (** defaults to [Time_source.now time_source] *)
  -> unit
  -> Time.t Deferred.t

type 'a every_enter_callback = enter:Time.t -> leave:Time.t Deferred.t -> 'a

(** [every_enter_without_pushback t ~start ~stop ~continue_on_error ~start_in_range_is_enter f]
    calls [f] for each contiguous block of time in [t] starting at [start] and continuing
    until [stop] becomes determined.

    For each block of time with start time [enter] and end time [leave_time], [f] is
    called with [f ~enter ~leave], where [leave] is a deferred that becomes determined at
    [leave_time] with the value [leave_time].

    If [includes t start && start_in_range_is_enter], then [f] will be called as soon as
    possible after [start] with [enter = start].  Otherwise, [f] will not be called for
    any block of time that includes [start].

    If [continue_on_error = false] and [f] (or any async job started by [f]) raises an
    error, [f] will no longer be called, and all undetermined [leave] deferreds will
    remain unfulfilled.

    If [stop] is fulfilled then no further calls to [f] will be made and all undetermined
    [leave] deferreds will remain unfulfilled. *)
val every_enter_without_pushback
  :  (zoned, _) t
  -> ?time_source:Time_source.t     (** defaults to [Time_source.wall_clock ()] *)
  -> ?start:Time.t                  (** defaults to [Time_source.now time_source] *)
  -> ?stop:unit Deferred.t          (** defaults to [Deferred.never ()] *)
  -> ?continue_on_error:bool        (** defaults to [true] *)
  -> ?start_in_range_is_enter:bool  (** defaults to [true] *)
  -> unit every_enter_callback
  -> unit

(** Like [every_enter_without_pushback], except allows at most one call of [f] to be in
    flight at a time. If the schedule would cause [f] to be invoked again before the prior
    call has finished, then it invokes [on_pushback] instead (if provided). *)
val every_enter
  :  (zoned, _) t
  -> ?time_source:Time_source.t     (** defaults to [Time_source.wall_clock ()] *)
  -> ?start:Time.t                  (** defaults to [Time_source.now time_source] *)
  -> ?stop:unit Deferred.t          (** defaults to [Deferred.never ()] *)
  -> ?continue_on_error:bool        (** defaults to [true] *)
  -> ?start_in_range_is_enter:bool  (** defaults to [true] *)
  -> ?on_pushback:unit every_enter_callback (** defaults to doing nothing *)
  -> unit Deferred.t every_enter_callback
  -> unit

(** [every_tag_change t f] calls [f] for each contiguous block of time in [t] where the
    set of tags in effect remains stable (according to tag_equal).

    Moving from a range where the schedule is not in effect to one where it is in effect
    with no tags is considered a tag change.

    For each block of time tagged with [tags] and start time [enter] and end time
    [leave_time], [f] is called with [f ~tags ~enter ~leave], where [leave] is a deferred
    that becomes determined at [leave_time] with the value [leave_time].

    [stop], [continue_on_error], and [start_in_range_is_enter] act as documented in
    [every_enter]. *)
val every_tag_change_without_pushback
  :  (zoned, 'tag) t
  -> ?time_source:Time_source.t     (** defaults to [Time_source.wall_clock ()] *)
  -> ?start:Time.t                  (** defaults to [Time_source.now time_source] *)
  -> ?stop:unit Deferred.t          (** defaults to [Deferred.never ()] *)
  -> ?continue_on_error:bool        (** defaults to [true] *)
  -> ?start_in_range_is_enter:bool  (** defaults to [true] *)
  -> tag_equal:('tag -> 'tag -> bool)
  -> (tags:'tag list -> unit every_enter_callback)
  -> unit

(** Like [every_tag_change_without_pushback], but pushes back in the same manner as
    [every_enter]. *)
val every_tag_change
  :  (zoned, 'tag) t
  -> ?time_source:Time_source.t     (** defaults to [Time_source.wall_clock ()] *)
  -> ?start:Time.t                  (** defaults to [Time_source.now time_source] *)
  -> ?stop:unit Deferred.t          (** defaults to [Deferred.never ()] *)
  -> ?continue_on_error:bool        (** defaults to [true] *)
  -> ?start_in_range_is_enter:bool  (** defaults to [true] *)
  -> ?on_pushback:(tags:'tag list -> unit every_enter_callback) (** defaults to doing nothing *)
  -> tag_equal:('tag -> 'tag -> bool)
  -> (tags:'tag list -> unit Deferred.t every_enter_callback)
  -> unit

